%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_s2s).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(xmpp_router).

%% API functions
-export([start_link/0,
         filter/4,
         route/4,
         key/3,
         try_register/1,
         get_s2s_out_pids/1,
         remove_connection/2]).

%% Hooks callbacks
-export([node_cleanup/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ignore_xref([start_link/0]).

-include("mongoose.hrl").

%% Pair of hosts {FromServer, ToServer}.
%% FromServer is the local server.
%% ToServer is the remote server.
%% Used in a lot of API and backend functions.
-type fromto() :: {jid:lserver(), jid:lserver()}.

%% Pids for mongoose_s2s_out servers
-type s2s_pids() :: [pid()].

-record(state, {}).

-type stream_id() :: binary().
-type base16_secret() :: <<_:16, _:_*16>>. %% Hex encoded
-type s2s_dialback_key() :: <<_:16, _:_*16>>. %% Hex encoded

-export_type([fromto/0, s2s_pids/0, base16_secret/0, stream_id/0, s2s_dialback_key/0]).

%% API functions

%% Starts the server
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, noargs, [{hibernate_after, 0}]).

-spec filter(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    drop | xmpp_router:filter().
filter(From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.

-spec route(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    {done, mongoose_acc:t()}. % this is the 'last resort' router, it always returns 'done'.
route(From, To, Acc, Packet) ->
    do_route(From, To, Acc, Packet).

%% Called by mongoose_s2s_out process.
-spec try_register(fromto()) -> IsRegistered :: boolean().
try_register(FromTo) ->
    Pid = self(),
    IsRegistered = call_try_register(Pid, FromTo),
    case IsRegistered of
        false ->
            %% This usually happens when a mongoose_s2s_out connection is established
            %% during dialback procedure to check the key.
            %% We still are fine, we just would not use that s2s connection to route
            %% any stanzas to the remote server.
            %% Could be a sign of abuse or a bug though, so use logging here.
            ?LOG_INFO(#{what => s2s_register_failed, from_to => FromTo, pid => self()});
        _ ->
            ok
    end,
    IsRegistered.

-spec key(mongooseim:host_type(), fromto(), stream_id()) -> s2s_dialback_key().
key(HostType, FromTo, StreamID) ->
    {ok, Secret} = get_shared_secret(HostType),
    mongoose_s2s_dialback:make_key(FromTo, StreamID, Secret).

%% Hooks callbacks

-spec node_cleanup(map(), map(), map()) -> {ok, map()}.
node_cleanup(Acc, #{node := Node}, _) ->
    Res = call_node_cleanup(Node),
    {ok, maps:put(?MODULE, Res, Acc)}.

%% gen_server callbacks

init(noargs) ->
    internal_database_init(),
    set_shared_secret(),
    gen_hook:add_handlers(hooks()),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, {error, unexpected_call}, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

terminate(_Reason, _State) ->
    gen_hook:delete_handlers(hooks()),
    ok.

%%% Internal functions
-spec hooks() -> [gen_hook:hook_tuple()].
hooks() ->
    [{node_cleanup, global, fun ?MODULE:node_cleanup/3, #{}, 50}].

-spec do_route(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    {done, mongoose_acc:t()}.
do_route(From, To, Acc0, Packet) ->
    ?LOG_DEBUG(#{what => s2s_route, acc => Acc0}),
    case find_connection(From, To) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_DEBUG(#{what => s2s_found_connection,
                         text => <<"Send packet to s2s connection">>,
                         s2s_pid => Pid, acc => Acc0}),
            NewPacket = jlib:replace_from_to(From, To, Packet),
            NewStanzaParams = #{element => NewPacket, from_jid => From, to_jid => To},
            Acc1 = mongoose_acc:update_stanza(NewStanzaParams, Acc0),
            Acc2 = mongoose_hooks:s2s_send_packet(Acc1, From, To, NewPacket),
            mongoose_s2s_out:route(Pid, Acc2),
            {done, Acc2};
        {error, _Reason} ->
            case mongoose_acc:stanza_type(Acc0) of
                <<"error">> ->
                    {done, Acc0};
                <<"result">> ->
                    {done, Acc0};
                _ ->
                    ?LOG_DEBUG(#{what => s2s_connection_not_found, acc => Acc0}),
                    {Acc1, Err} = jlib:make_error_reply(
                            Acc0, Packet, mongoose_xmpp_errors:service_unavailable()),
                    Acc2 = ejabberd_router:route(To, From, Acc1, Err),
                    {done, Acc2}
            end
    end.

-spec find_connection(From :: jid:jid(), To :: jid:jid()) ->
        {ok, pid()} | {error, not_allowed}.
find_connection(From, To) ->
    FromTo = mongoose_s2s_lib:make_from_to(From, To),
    ?LOG_DEBUG(#{what => s2s_find_connection, from_to => FromTo}),
    OldCons = get_s2s_out_pids(FromTo),
    NewCons = ensure_enough_connections(FromTo, OldCons),
    case NewCons of
        [] ->
            {error, not_allowed};
        [_|_] ->
            {ok, mongoose_s2s_lib:choose_pid(From, NewCons)}
    end.

%% Opens more connections if needed and allowed.
%% Returns an updated list of connections.
-spec ensure_enough_connections(fromto(), s2s_pids()) -> s2s_pids().
ensure_enough_connections(FromTo, OldCons) ->
    NeededConnections =
        mongoose_s2s_lib:needed_extra_connections_number_if_allowed(FromTo, OldCons),
    %% Could be negative, if we have too many connections
    case NeededConnections > 0 of
        true ->
            open_new_connections(NeededConnections, FromTo),
            %% Query for s2s pids one more time
            get_s2s_out_pids(FromTo);
        false ->
            OldCons
    end.

-spec open_new_connections(N :: pos_integer(), FromTo :: fromto()) -> any().
open_new_connections(N, FromTo) ->
    [mongoose_s2s_out:start_connection(FromTo, new) || _N <- lists:seq(1, N)].

-spec set_shared_secret() -> ok.
set_shared_secret() ->
    [set_shared_secret(HostType) || HostType <- ?ALL_HOST_TYPES],
    ok.

%% Updates the secret across the cluster if needed
-spec set_shared_secret(mongooseim:host_type()) -> ok.
set_shared_secret(HostType) ->
    case mongoose_s2s_lib:check_shared_secret(HostType, get_shared_secret(HostType)) of
        {update, NewSecret} ->
            register_secret(HostType, NewSecret);
        ok ->
            ok
    end.

%% Backend logic functions

-spec internal_database_init() -> ok.
internal_database_init() ->
    Backend = mongoose_config:get_opt(s2s_backend),
    mongoose_s2s_backend:init(#{backend => Backend}).

%% Get mongoose_s2s_out pids
-spec get_s2s_out_pids(FromTo :: fromto()) -> s2s_pids().
get_s2s_out_pids(FromTo) ->
    mongoose_s2s_backend:get_s2s_out_pids(FromTo).

%% Returns true if the connection is registered
-spec call_try_register(Pid :: pid(), FromTo :: fromto()) -> IsRegistered :: boolean().
call_try_register(Pid, FromTo) ->
    mongoose_s2s_backend:try_register(Pid, FromTo).

-spec call_node_cleanup(Node :: node()) -> ok.
call_node_cleanup(Node) ->
    mongoose_s2s_backend:node_cleanup(Node).

-spec remove_connection(fromto(), pid()) -> ok.
remove_connection(FromTo, Pid) ->
    mongoose_s2s_backend:remove_connection(FromTo, Pid).

-spec get_shared_secret(mongooseim:host_type()) -> {ok, base16_secret()} | {error, not_found}.
get_shared_secret(HostType) ->
    mongoose_s2s_backend:get_shared_secret(HostType).

-spec register_secret(mongooseim:host_type(), base16_secret()) -> ok.
register_secret(HostType, Secret) ->
    mongoose_s2s_backend:register_secret(HostType, Secret).
