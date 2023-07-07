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

-xep([{xep, 185}, {version, "1.0"}]).

-behaviour(gen_server).
-behaviour(xmpp_router).

%% API
-export([start_link/0,
         filter/4,
         route/4,
         key/3,
         get_s2s_out_pids/1,
         try_register/1,
         remove_connection/2,
         allow_host/1,
         domain_utf8_to_ascii/1,
         timeout/0,
         lookup_certfile/1
        ]).

%% Hooks callbacks
-export([node_cleanup/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ejabberd API
-export([get_info_s2s_connections/1]).

-ignore_xref([get_info_s2s_connections/1, start_link/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").

-define(DEFAULT_MAX_S2S_CONNECTIONS, 1).
-define(DEFAULT_MAX_S2S_CONNECTIONS_PER_NODE, 1).

%% Pair of hosts {FromServer, ToServer}.
%% FromServer is the local server.
%% ToServer is the remote server.
%% Used in a lot of API and backend functions.
-type fromto() :: {jid:lserver(), jid:lserver()}.

%% Pids for ejabberd_s2s_out servers
-type s2s_pids() :: [pid()].

-record(state, {}).

-type base16_secret() :: binary().

-export_type([fromto/0, s2s_pids/0, base16_secret/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

filter(From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.

route(From, To, Acc, Packet) ->
    do_route(From, To, Acc, Packet).

-spec try_register(fromto()) -> boolean().
try_register(FromTo) ->
    ShouldWriteF = should_write_f(FromTo),
    Pid = self(),
    IsRegistered = call_try_register(Pid, ShouldWriteF, FromTo),
    case IsRegistered of
        false ->
            %% This usually happens when a ejabberd_s2s_out connection is established during dialback
            %% procedure to check the key.
            %% We still are fine, we just would not use that s2s connection to route
            %% any stanzas to the remote server.
            %% Could be a sign of abuse or a bug though, so use logging here.
            ?LOG_INFO(#{what => s2s_register_failed, from_to => FromTo, pid => self()});
        _ ->
            ok
    end,
    IsRegistered.

%%====================================================================
%% Hooks callbacks
%%====================================================================

-spec node_cleanup(map(), map(), map()) -> {ok, map()}.
node_cleanup(Acc, #{node := Node}, _) ->
    Res = call_node_cleanup(Node),
    {ok, maps:put(?MODULE, Res, Acc)}.

-spec key(mongooseim:host_type(), fromto(), binary()) -> binary().
key(HostType, {From, To}, StreamID) ->
    {ok, Secret} = get_shared_secret(HostType),
    SecretHashed = base16:encode(crypto:hash(sha256, Secret)),
    HMac = crypto:mac(hmac, sha256, SecretHashed, [From, " ", To, " ", StreamID]),
    base16:encode(HMac).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    internal_database_init(),
    set_shared_secret(),
    ejabberd_commands:register_commands(commands()),
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
    ejabberd_commands:unregister_commands(commands()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec hooks() -> [gen_hook:hook_tuple()].
hooks() ->
    [{node_cleanup, global, fun ?MODULE:node_cleanup/3, #{}, 50}].

-spec do_route(From :: jid:jid(),
               To :: jid:jid(),
               Acc :: mongoose_acc:t(),
               Packet :: exml:element()) ->
    {done, mongoose_acc:t()}. % this is the 'last resort' router, it always returns 'done'.
do_route(From, To, Acc, Packet) ->
    ?LOG_DEBUG(#{what => s2s_route, acc => Acc}),
    case find_connection(From, To) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_DEBUG(#{what => s2s_found_connection,
                         text => <<"Send packet to s2s connection">>,
                         s2s_pid => Pid, acc => Acc}),
            NewPacket = jlib:replace_from_to(From, To, Packet),
            Acc1 = mongoose_hooks:s2s_send_packet(Acc, From, To, Packet),
            send_element(Pid, Acc1, NewPacket),
            {done, Acc1};
        {error, _Reason} ->
            case mongoose_acc:stanza_type(Acc) of
                <<"error">> ->
                    {done, Acc};
                <<"result">> ->
                    {done, Acc};
                _ ->
                    ?LOG_DEBUG(#{what => s2s_connection_not_found, acc => Acc}),
                    {Acc1, Err} = jlib:make_error_reply(
                            Acc, Packet, mongoose_xmpp_errors:service_unavailable()),
                    Acc2 = ejabberd_router:route(To, From, Acc1, Err),
                    {done, Acc2}
            end
    end.

-spec make_from_to(From :: jid:jid(), To :: jid:jid()) ->  fromto().
make_from_to(#jid{lserver = FromServer}, #jid{lserver = ToServer}) ->
    {FromServer, ToServer}.

-spec find_connection(From :: jid:jid(), To :: jid:jid()) ->
        {ok, pid()} | {error, not_allowed}.
find_connection(From, To) ->
    FromTo = make_from_to(From, To),
    ?LOG_DEBUG(#{what => s2s_find_connection, from_to => FromTo}),
    OldCons = get_s2s_out_pids(FromTo),
    NewCons = ensure_enough_connections(FromTo, OldCons),
    case NewCons of
        [] ->
            {error, not_allowed};
        _ ->
            {ok, choose_pid(From, NewCons)}
    end.

%% Opens more connections if needed and allowed.
ensure_enough_connections(FromTo, OldCons) ->
    NeededConnections = needed_connections_number_if_allowed(FromTo, OldCons),
    case NeededConnections of
        0 ->
            OldCons;
        _ ->
            open_several_connections(NeededConnections, FromTo),
            %% Query for s2s pids one more time
            get_s2s_out_pids(FromTo)
    end.

%% Prefers the local connection (i.e. not on the remote node)
-spec choose_pid(From :: jid:jid(), Pids :: s2s_pids()) -> pid().
choose_pid(From, [_|_] = Pids) ->
    Pids1 = case filter_local_pids(Pids) of
                [] -> Pids;
                FilteredPids -> FilteredPids
            end,
    % Use sticky connections based on the JID of the sender
    % (without the resource to ensure that a muc room always uses the same connection)
    Pid = lists:nth(erlang:phash2(jid:to_bare(From), length(Pids1)) + 1, Pids1),
    ?LOG_DEBUG(#{what => s2s_choose_pid, from => From, s2s_pid => Pid}),
    Pid.

%% Returns only pids from the current node.
-spec filter_local_pids(s2s_pids()) -> s2s_pids().
filter_local_pids(Pids) ->
    Node = node(),
    [Pid || Pid <- Pids, node(Pid) == Node].

-spec open_several_connections(N :: pos_integer(), FromTo :: fromto()) -> ok.
open_several_connections(N, FromTo) ->
    ShouldWriteF = should_write_f(FromTo),
    [new_connection(FromTo, ShouldWriteF) || _N <- lists:seq(1, N)],
    ok.

-spec new_connection(FromTo :: fromto(), ShouldWriteF :: fun()) -> ok.
new_connection(FromTo, ShouldWriteF) ->
    {ok, Pid} = ejabberd_s2s_out:start(FromTo, new),
    case call_try_register(Pid, ShouldWriteF, FromTo) of
        true ->
            ?LOG_INFO(#{what => s2s_new_connection,
                        text => <<"New s2s connection started">>,
                        from_to => FromTo, s2s_pid => Pid}),
            ejabberd_s2s_out:start_connection(Pid);
        false ->
            ejabberd_s2s_out:stop_connection(Pid)
    end,
    ok.

-spec max_s2s_connections(fromto()) -> pos_integer().
max_s2s_connections(FromTo) ->
    match_integer_acl_rule(FromTo, max_s2s_connections,
                           ?DEFAULT_MAX_S2S_CONNECTIONS).

-spec max_s2s_connections_per_node(fromto()) -> pos_integer().
max_s2s_connections_per_node(FromTo) ->
    match_integer_acl_rule(FromTo, max_s2s_connections_per_node,
                           ?DEFAULT_MAX_S2S_CONNECTIONS_PER_NODE).

-spec match_integer_acl_rule(fromto(), atom(), integer()) -> term().
match_integer_acl_rule({FromServer, ToServer}, Rule, Default) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(FromServer),
    ToServerJid = jid:make(<<>>, ToServer, <<>>),
    case acl:match_rule(HostType, Rule, ToServerJid) of
        Int when is_integer(Int) -> Int;
        _ -> Default
    end.

needed_connections_number_if_allowed(FromTo, OldCons) ->
    case is_s2s_allowed_for_host(FromTo, OldCons) of
        true ->
            needed_extra_connections_number(FromTo, OldCons);
        false ->
            0
    end.

%% Checks:
%% - if the host is not a service
%% - and if the s2s host is not blacklisted or is in whitelist
-spec is_s2s_allowed_for_host(fromto(), _OldConnections :: s2s_pids()) -> boolean().
is_s2s_allowed_for_host(_FromTo, [_|_]) ->
    true; %% Has outgoing connections established, skip the check
is_s2s_allowed_for_host(FromTo, []) ->
    not is_service(FromTo) andalso allow_host(FromTo).

-spec needed_extra_connections_number(fromto(), s2s_pids()) -> non_neg_integer().
needed_extra_connections_number(FromTo, Connections) ->
    MaxConnections = max_s2s_connections(FromTo),
    MaxConnectionsPerNode = max_s2s_connections_per_node(FromTo),
    LocalPids = filter_local_pids(Connections),
    lists:min([MaxConnections - length(Connections),
               MaxConnectionsPerNode - length(LocalPids)]).

should_write_f(FromTo) ->
    fun(Connections) when is_list(Connections) ->
        needed_extra_connections_number(FromTo, Connections) > 0
    end.

%%--------------------------------------------------------------------
%% Description: Return true if the destination must be considered as a
%% service.
%% --------------------------------------------------------------------
-spec is_service(fromto()) -> boolean().
is_service({FromServer, ToServer} = _FromTo) ->
    case mongoose_config:lookup_opt({route_subdomains, FromServer}) of
        {ok, s2s} -> % bypass RFC 3920 10.3
            false;
        {error, not_found} ->
            Hosts = ?MYHOSTS,
            P = fun(ParentDomain) -> lists:member(ParentDomain, Hosts) end,
            lists:any(P, parent_domains(ToServer))
    end.

-spec parent_domains(binary()) -> [binary(), ...].
parent_domains(Domain) ->
    parent_domains(Domain, [Domain]).

-spec parent_domains(binary(), [binary(), ...]) -> [binary(), ...].
parent_domains(<<>>, Acc) ->
    lists:reverse(Acc);
parent_domains(<<$., Rest/binary>>, Acc) ->
    parent_domains(Rest, [Rest | Acc]);
parent_domains(<<_, Rest/binary>>, Acc) ->
    parent_domains(Rest, Acc).

-spec send_element(pid(), mongoose_acc:t(), exml:element()) ->
    {'send_element', mongoose_acc:t(), exml:element()}.
send_element(Pid, Acc, El) ->
    Pid ! {send_element, Acc, El}.

timeout() ->
    600000.
%%--------------------------------------------------------------------
%% Function: domain_utf8_to_ascii(Domain) -> binary() | false
%% Description: Converts a UTF-8 domain to ASCII (IDNA)
%% --------------------------------------------------------------------
-spec domain_utf8_to_ascii(binary() | string()) -> binary() | false.
domain_utf8_to_ascii(Domain) ->
    case catch idna:utf8_to_ascii(Domain) of
        {'EXIT', _} ->
            false;
        AsciiDomain ->
            list_to_binary(AsciiDomain)
    end.

%%%----------------------------------------------------------------------
%%% ejabberd commands

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    [
     #ejabberd_commands{name = incoming_s2s_number,
                       tags = [stats, s2s],
                       desc = "Number of incoming s2s connections on the node",
                       module = stats_api, function = incoming_s2s_number,
                       args = [],
                       result = {s2s_incoming, integer}},
     #ejabberd_commands{name = outgoing_s2s_number,
                       tags = [stats, s2s],
                       desc = "Number of outgoing s2s connections on the node",
                       module = stats_api, function = outgoing_s2s_number,
                       args = [],
                       result = {s2s_outgoing, integer}}
    ].

%% Check if host is in blacklist or white list
-spec allow_host(fromto()) -> boolean().
allow_host({FromServer, ToServer}) ->
    case mongoose_domain_api:get_host_type(FromServer) of
        {error, not_found} ->
            false;
        {ok, HostType} ->
            case mongoose_config:lookup_opt([{s2s, HostType}, host_policy, ToServer]) of
                {ok, allow} ->
                    true;
                {ok, deny} ->
                    false;
                {error, not_found} ->
                    mongoose_config:get_opt([{s2s, HostType}, default_policy]) =:= allow
                        andalso mongoose_hooks:s2s_allow_host(FromServer, ToServer) =:= allow
            end
    end.

%% @doc Get information about S2S connections of the specified type.
-spec get_info_s2s_connections('in' | 'out') -> [[{atom(), any()}, ...]].
get_info_s2s_connections(Type) ->
    ChildType = case Type of
                    in -> ejabberd_s2s_in_sup;
                    out -> ejabberd_s2s_out_sup
                end,
    Connections = supervisor:which_children(ChildType),
    get_s2s_info(Connections, Type).

-type connstate() :: 'restarting' | 'undefined' | pid().
-type conn() :: { any(), connstate(), 'supervisor' | 'worker', 'dynamic' | [_] }.
-spec get_s2s_info(Connections :: [conn()],
                  Type :: 'in' | 'out'
                  ) -> [[{any(), any()}, ...]]. % list of lists
get_s2s_info(Connections, Type)->
    complete_s2s_info(Connections, Type, []).

-spec complete_s2s_info(Connections :: [conn()],
                        Type :: 'in' | 'out',
                        Result :: [[{any(), any()}, ...]] % list of lists
                        ) -> [[{any(), any()}, ...]]. % list of lists
complete_s2s_info([], _, Result)->
    Result;
complete_s2s_info([Connection|T], Type, Result)->
    {_, PID, _, _} = Connection,
    State = get_s2s_state(PID),
    complete_s2s_info(T, Type, [State|Result]).

-spec get_s2s_state(connstate()) -> [{atom(), any()}, ...].
get_s2s_state(S2sPid) ->
    Infos = case gen_fsm_compat:sync_send_all_state_event(S2sPid, get_state_infos) of
                {state_infos, Is} -> [{status, open} | Is];
                {noproc, _} -> [{status, closed}]; %% Connection closed
                {badrpc, _} -> [{status, error}]
            end,
    [{s2s_pid, S2sPid} | Infos].

-spec set_shared_secret() -> ok.
set_shared_secret() ->
    [set_shared_secret(HostType) || HostType <- ?ALL_HOST_TYPES],
    ok.

set_shared_secret(HostType) ->
    %% register_secret is replicated across all nodes.
    %% So, when starting a node with updated secret in the config,
    %% we would replace stored secret on all nodes at once.
    %% There could be a small race condition when dialback key checks would get rejected,
    %% But there would not be conflicts when some nodes have one secret stored and others - another.
    case {get_shared_secret(HostType), get_shared_secret_from_config(HostType)} of
        {{error, not_found}, {ok, Secret}} ->
            %% Write the secret from the config into Mnesia/CETS for the first time
            register_secret(HostType, Secret);
        {{error, not_found}, {error, not_found}} ->
            %% Write a random secret into Mnesia/CETS for the first time
            register_secret(HostType, make_random_secret());
        {{ok, Secret}, {ok, Secret}} ->
            %% Config matches Mnesia/CETS
            skip_same;
        {{ok, _OldSecret}, {ok, NewSecret}} ->
            ?LOG_INFO(#{what => overwrite_secret_from_config}),
            register_secret(HostType, NewSecret);
        {{ok, _OldSecret}, {error, not_found}} ->
            %% Keep the secret already stored in Mnesia/CETS
            keep_existing
    end.

-spec get_shared_secret_from_config(mongooseim:host_type()) -> {ok, base16_secret()} | {error, not_found}.
get_shared_secret_from_config(HostType) ->
    mongoose_config:lookup_opt([{s2s, HostType}, shared]).

-spec make_random_secret() -> base16_secret().
make_random_secret() ->
    base16:encode(crypto:strong_rand_bytes(10)).

-spec lookup_certfile(mongooseim:host_type()) -> {ok, string()} | {error, not_found}.
lookup_certfile(HostType) ->
    case mongoose_config:lookup_opt({domain_certfile, HostType}) of
        {ok, CertFile} ->
            CertFile;
        {error, not_found} ->
            mongoose_config:lookup_opt([{s2s, HostType}, certfile])
    end.


%% Backend logic below:

internal_database_init() ->
    Backend = mongoose_config:get_opt(s2s_backend),
    mongoose_s2s_backend:init(#{backend => Backend}).

%% Get ejabberd_s2s_out pids
-spec get_s2s_out_pids(FromTo :: fromto()) -> s2s_pids().
get_s2s_out_pids(FromTo) ->
    mongoose_s2s_backend:get_s2s_out_pids(FromTo).

%% Returns true if the connection is registered
-spec call_try_register(Pid :: pid(), ShouldWriteF :: fun(), FromTo :: fromto()) -> boolean().
call_try_register(Pid, ShouldWriteF, FromTo) ->
    mongoose_s2s_backend:try_register(Pid, ShouldWriteF, FromTo).

call_node_cleanup(Node) ->
    mongoose_s2s_backend:node_cleanup(Node).

-spec remove_connection(fromto(), pid()) -> ok.
remove_connection(FromTo, Pid) ->
    mongoose_s2s_backend:remove_connection(FromTo, Pid).

-spec get_shared_secret(mongooseim:host_type()) -> {ok, base16_secret()} | {error, not_found}.
get_shared_secret(HostType) ->
    mongoose_s2s_backend:get_shared_secret(HostType).

-spec register_secret(mongooseim:host_type(), ejabberd_s2s:base16_secret()) -> ok.
register_secret(HostType, Secret) ->
    mongoose_s2s_backend:register_secret(HostType, Secret).
