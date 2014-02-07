%%%----------------------------------------------------------------------
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages
%%% See     : XEP-0160: Best Practices for Handling Offline Messages
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline).
-author('alexey@process-one.net').

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).

%% Hook handlers
-export([inspect_packet/3,
         resend_offline_messages/2,
         pop_offline_messages/3,
         get_sm_features/5,
         remove_expired_messages/1,
         remove_old_messages/2,
         remove_user/2]).

%% Internal exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_offline.hrl").

-define(PROCNAME, ejabberd_offline).

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).
-define(BACKEND, (mod_offline_backend:backend())).

-record(state, {host, access_max_user_messages}).

%% ------------------------------------------------------------------
%% Backend callbacks

-callback init(Host, Opts) -> ok when
    Host :: binary(),
    Opts :: list().

-callback remove_user(LUser, LServer) -> ok when
    LUser :: binary(),
    LServer :: binary().

%% gen_mod callbacks
%% ------------------------------------------------------------------

start(Host, Opts) ->
    AccessMaxOfflineMsgs = gen_mod:get_opt(access_max_user_messages, Opts,
                                           max_user_offline_messages),
    start_backend_module(Opts),
    ?BACKEND:init(Host, Opts),
    start_worker(Host, AccessMaxOfflineMsgs),
    ejabberd_hooks:add(offline_message_hook, Host,
		       ?MODULE, inspect_packet, 50),
    ejabberd_hooks:add(resend_offline_messages_hook, Host,
		       ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(disco_sm_features, Host,
		       ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_local_features, Host,
		       ?MODULE, get_sm_features, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, inspect_packet, 50),
    ejabberd_hooks:delete(resend_offline_messages_hook, Host,
			  ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_sm_features, 50),
    stop_worker(Host),
    ok.


%% Dynamic modules
%% ------------------------------------------------------------------

start_backend_module(Opts) ->
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    {Mod, Code} = dynamic_compile:from_string(mod_offline_backend(Backend)),
    code:load_binary(Mod, "mod_offline_backend.erl", Code).

-spec mod_offline_backend(atom()) -> string().
mod_offline_backend(Backend) when is_atom(Backend) ->
    lists:flatten(
      ["-module(mod_offline_backend).
        -export([backend/0]).
        -spec backend() -> atom().
        backend() ->
        mod_offline_",
                   atom_to_list(Backend),
                   ".\n"]).

%% Server side functions
%% ------------------------------------------------------------------

handle_offline_msg(#offline_msg{us=US} = Msg, AccessMaxOfflineMsgs) ->
    {LUser, LServer} = US,
    Msgs = receive_all(US, [Msg]),
    MaxOfflineMsgs = get_max_user_messages(
        AccessMaxOfflineMsgs, LUser, LServer),
    case ?BACKEND:write_messages(LUser, LServer, Msgs, MaxOfflineMsgs) of
        ok ->
            ok;
        {discarded, DiscardedMsgs} ->
            discard_warn_sender(DiscardedMsgs);
        {error, Reason} ->
            ?ERROR_MSG("~ts@~ts: write_messages failed with ~p.",
                [LUser, LServer, Reason]),
            discard_warn_sender(Msgs)
    end.

%% Function copied from ejabberd_sm.erl:
get_max_user_messages(AccessRule, LUser, Host) ->
    case acl:match_rule(Host, AccessRule, jlib:make_jid(LUser, Host, <<>>)) of
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_MESSAGES
    end.

receive_all(US, Msgs) ->
    receive
	#offline_msg{us=US} = Msg ->
	    receive_all(US, [Msg | Msgs])
    after 0 ->
	    Msgs
    end.

%% Supervision
%% ------------------------------------------------------------------

start_worker(Host, AccessMaxOfflineMsgs) ->
    Proc = srv_name(Host),
    ChildSpec =
    {Proc,
     {?MODULE, start_link, [Proc, Host, AccessMaxOfflineMsgs]},
     permanent,
     5000,
     worker,
     [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec),
    ok.

stop_worker(Host) ->
    Proc = srv_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Name, Host, AccessMaxOfflineMsgs) ->
    gen_server:start_link({local, Name}, ?MODULE, [Host, AccessMaxOfflineMsgs], []).

srv_name() ->
    mod_offline.

srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, AccessMaxOfflineMsgs]) ->
    {ok, #state{
            host = Host,
            access_max_user_messages = AccessMaxOfflineMsgs}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_, _, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(Msg, State) ->
    ?WARNING_MSG("Strange message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Msg=#offline_msg{},
            State=#state{access_max_user_messages = AccessMaxOfflineMsgs}) ->
    handle_offline_msg(Msg, AccessMaxOfflineMsgs),
    {noreply, State};
handle_info(Msg, State) ->
    ?WARNING_MSG("Strange message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Handlers
%% ------------------------------------------------------------------

get_sm_features(Acc, _From, _To, <<"">> = _Node, _Lang) ->
    add_feature(Acc, ?NS_FEATURE_MSGOFFLINE);
get_sm_features(_Acc, _From, _To, ?NS_FEATURE_MSGOFFLINE, _Lang) ->
    %% override all lesser features...
    {result, []};
get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

add_feature({result, Features}, Feature) ->
    {result, Features ++ [Feature]};
add_feature(_, Feature) ->
    {result, [Feature]}.

inspect_packet(From, To, Packet) ->
    case is_interesting_packet(Packet) of
        true ->
            case check_event_chatstates(From, To, Packet) of
                true ->
                    store_packet(From, To, Packet),
                    stop;
                false ->
                    ok
            end;
        false ->
            ok
    end.

store_packet(
        From,
        To = #jid{luser = LUser, lserver = LServer},
        Packet = #xmlel{children = Els}) ->
    TimeStamp = now(),
    Expire = find_x_expire(TimeStamp, Els),
    Pid = srv_name(LServer),
    Msg = #offline_msg{us = {LUser, LServer},
             timestamp = TimeStamp,
             expire = Expire,
             from = From,
             to = To,
             packet = Packet},
    Pid ! Msg,
    ok.

is_interesting_packet(Packet) ->
    Type = xml:get_tag_attr_s(<<"type">>, Packet),
    is_interesting_packet_type(Type).

is_interesting_packet_type(<<"error">>)     -> false;
is_interesting_packet_type(<<"groupchat">>) -> false;
is_interesting_packet_type(<<"headline">>)  -> false;
is_interesting_packet_type(_)               -> true.

%% Check if the packet has any content about XEP-0022 or XEP-0085
check_event_chatstates(From, To, Packet) ->
    #xmlel{children = Els} = Packet,
    case find_x_event_chatstates(Els, {false, false, false}) of
	%% There wasn't any x:event or chatstates subelements
	{false, false, _} ->
	    true;
	%% There a chatstates subelement and other stuff, but no x:event
	{false, CEl, true} when CEl /= false ->
	    true;
	%% There was only a subelement: a chatstates
	{false, CEl, false} when CEl /= false ->
	    %% Don't allow offline storage
	    false;
	%% There was an x:event element, and maybe also other stuff
	{El, _, _} when El /= false ->
	    case xml:get_subtag(El, <<"id">>) of
		false ->
		    case xml:get_subtag(El, <<"offline">>) of
			false ->
			    true;
			_ ->
                ejabberd_router:route(To, From, patch_offline_message(Packet)),
			    true
		    end;
		_ ->
		    false
	    end
    end.

patch_offline_message(Packet) ->
    ID = case xml:get_tag_attr_s(<<"id">>, Packet) of
         <<"">> ->
         #xmlel{name = <<"id">>};
         S ->
         #xmlel{name = <<"id">>,
                children = [#xmlcdata{content = S}]}
     end,
    Packet#xmlel{children = [x_elem(ID)]}.

x_elem(ID) ->
    #xmlel{
        name = <<"x">>,
        attrs = [{<<"xmlns">>, ?NS_EVENT}],
        children = [ID, #xmlel{name = <<"offline">>}]}.

%% Check if the packet has subelements about XEP-0022, XEP-0085 or other
find_x_event_chatstates([], Res) ->
    Res;
find_x_event_chatstates([#xmlcdata{} | Els], Res) ->
    find_x_event_chatstates(Els, Res);
find_x_event_chatstates([El | Els], {A, B, C}) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
	?NS_EVENT ->
	    find_x_event_chatstates(Els, {El, B, C});
	?NS_CHATSTATES ->
	    find_x_event_chatstates(Els, {A, El, C});
	_ ->
	    find_x_event_chatstates(Els, {A, B, true})
    end.

find_x_expire(_, []) ->
    never;
find_x_expire(TimeStamp, [#xmlcdata{} | Els]) ->
    find_x_expire(TimeStamp, Els);
find_x_expire(TimeStamp, [El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
	?NS_EXPIRE ->
	    Val = xml:get_tag_attr_s(<<"seconds">>, El),
	    case catch list_to_integer(Val) of
		{'EXIT', _} ->
		    never;
		Int when Int > 0 ->
		    {MegaSecs, Secs, MicroSecs} = TimeStamp,
		    S = MegaSecs * 1000000 + Secs + Int,
		    MegaSecs1 = S div 1000000,
		    Secs1 = S rem 1000000,
		    {MegaSecs1, Secs1, MicroSecs};
		_ ->
		    never
	    end;
	_ ->
	    find_x_expire(TimeStamp, Els)
    end.

pop_offline_messages(Ls, User, Server) ->
    Ls ++ pop_offline_messages(User, Server).

pop_offline_messages(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    case ?BACKEND:pop_messages(LUser, LServer) of
        {ok, Rs} ->
            lists:map(fun(R) ->
                Packet = resend_offline_message_packet(Server, R),
                compose_offline_message(R, Packet)
              end, Rs);
        {error, Reason} ->
            ?ERROR_MSG("~ts@~ts: pop_messages failed with ~p.", [LUser, LServer, Reason]),
            []
    end.

resend_offline_messages(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    case ?BACKEND:pop_messages(LUser, LServer) of
        {ok, Rs} ->
            lists:foreach(fun(R) ->
                  Packet = resend_offline_message_packet(Server, R),
                  route_offline_message(R, Packet)
              end, Rs);
        {error, _Reason} ->
            ok
    end.

route_offline_message(#offline_msg{from=From, to=To}, Packet) ->
    ejabberd_sm:route(From, To, Packet).

compose_offline_message(#offline_msg{from=From, to=To}, Packet) ->
    {route, From, To, Packet}.

resend_offline_message_packet(Server,
        #offline_msg{timestamp=TimeStamp, packet = Packet}) ->
    add_timestamp(TimeStamp, Server, Packet).

add_timestamp(undefined, _Server, Packet) ->
    Packet;
add_timestamp(TimeStamp, Server, Packet) ->
    Time = calendar:now_to_universal_time(TimeStamp),
    %% TODO: Delete the next element once XEP-0091 is Obsolete
    TimeStampLegacyXML = timestamp_legacy_xml(Server, Time),
    TimeStampXML = jlib:timestamp_to_xml(Time),
    xml:append_subtags(Packet, [TimeStampLegacyXML, TimeStampXML]).

timestamp_legacy_xml(Server, Time) ->
    FromJID = jlib:make_jid(<<>>, Server, <<>>),
    jlib:timestamp_to_xml(Time, utc, FromJID, <<"Offline Storage">>).

remove_expired_messages(Host) ->
    ?BACKEND:remove_expired_messages(Host).

remove_old_messages(Host, Days) ->
    ?BACKEND:remove_expired_messages(Host, Days).

remove_user(User, Server) ->
    ?BACKEND:remove_user(User, Server).

%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(
      fun(#offline_msg{from=From, to=To, packet=Packet}) ->
	      ErrText = <<"Your contact offline message queue is full. The message has been discarded.">>,
	      Lang = xml:get_tag_attr_s(<<"xml:lang">>, Packet),
	      Err = jlib:make_error_reply(
		      Packet, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)),
	      ejabberd_router:route(To, From, Err)
      end, Msgs).
