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
-xep([{xep, 160}, {version, "1.0"}]).
-xep([{xep, 23}, {version, "1.3"}]).
-xep([{xep, 22}, {version, "1.4"}]).
-xep([{xep, 85}, {version, "2.1"}]).
-behaviour(gen_mod).
-behaviour(gdpr).

%% gen_mod handlers
-export([start/2, stop/1]).

%% Hook handlers
-export([inspect_packet/4,
         pop_offline_messages/3,
         get_sm_features/5,
         remove_expired_messages/1,
         remove_old_messages/2,
         remove_user/2,
         remove_user/3,
         determine_amp_strategy/5,
         amp_failed_event/3]).

%% Internal exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% helpers to be used from backend moudules
-export([is_expired_message/2]).

%% GDPR related
-export([get_personal_data/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("amp.hrl").
-include("mod_offline.hrl").

-define(PROCNAME, ejabberd_offline).

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).

-type msg() :: #offline_msg{us :: {jid:luser(), jid:lserver()},
                          timestamp :: erlang:timestamp(),
                          expire :: erlang:timestamp() | never,
                          from ::jid:jid(),
                          to ::jid:jid(),
                          packet :: exml:element()}.

-export_type([msg/0]).

-record(state, {
    host :: jid:server(),
    access_max_user_messages,
    message_poppers = monitored_map:new() ::
        monitored_map:t({LUser :: binary(), LServer :: binary}, pid())
}).

%% ------------------------------------------------------------------
%% Backend callbacks

-callback init(Host, Opts) -> ok when
    Host :: binary(),
    Opts :: list().
-callback pop_messages(LUser, LServer) -> {ok, Result} | {error, Reason} when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Reason :: term(),
    Result :: list(#offline_msg{}).
-callback fetch_messages(LUser, LServer) -> {ok, Result} | {error, Reason} when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Reason :: term(),
    Result :: list(#offline_msg{}).
-callback write_messages(LUser, LServer, Msgs) ->
    ok | {error, Reason}  when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Msgs :: list(),
    Reason :: term().
-callback count_offline_messages(LUser, LServer, MaxToArchive) -> integer() when
      LUser :: jid:luser(),
      LServer :: jid:lserver(),
      MaxToArchive :: integer().
-callback remove_expired_messages(Host) -> {error, Reason} | {ok, Count} when
    Host :: jid:lserver(),
    Reason :: term(),
    Count :: integer().
-callback remove_old_messages(Host, Timestamp) -> {error, Reason} | {ok, Count} when
    Host :: jid:lserver(),
    Timestamp :: erlang:timestamp(),
    Reason :: term(),
    Count :: integer().
-callback remove_user(LUser, LServer) -> any() when
    LUser :: binary(),
    LServer :: binary().

%% gen_mod callbacks
%% ------------------------------------------------------------------

start(Host, Opts) ->
    AccessMaxOfflineMsgs = gen_mod:get_opt(access_max_user_messages, Opts,
                                           max_user_offline_messages),
    gen_mod:start_backend_module(?MODULE, Opts, [pop_messages, write_messages]),
    mod_offline_backend:init(Host, Opts),
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
    ejabberd_hooks:add(amp_determine_strategy, Host,
                       ?MODULE, determine_amp_strategy, 30),
    ejabberd_hooks:add(failed_to_store_message, Host,
                       ?MODULE, amp_failed_event, 30),
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
    ejabberd_hooks:delete(amp_determine_strategy, Host,
                          ?MODULE, determine_amp_strategy, 30),
    ejabberd_hooks:delete(failed_to_store_message, Host,
                          ?MODULE, amp_failed_event, 30),
    stop_worker(Host),
    ok.


%% Server side functions
%% ------------------------------------------------------------------

amp_failed_event(Acc, From, _Packet) ->
    mod_amp:check_packet(Acc, From, offline_failed).

handle_offline_msg(Acc, #offline_msg{us=US} = Msg, AccessMaxOfflineMsgs) ->
    {LUser, LServer} = US,
    Msgs = receive_all(US, [Msg]),
    MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs, LUser, LServer),
    Len = length(Msgs),
    case is_message_count_threshold_reached(MaxOfflineMsgs, LUser, LServer, Len) of
        false ->
            write_messages(Acc, LUser, LServer, Msgs);
        true ->
            discard_warn_sender(Acc, Msgs)
    end.

write_messages(Acc, LUser, LServer, Msgs) ->
    case mod_offline_backend:write_messages(LUser, LServer, Msgs) of
        ok ->
            lists:foreach(fun(#offline_msg{from = From, to = To, packet = Packet}) ->
                                  NAcc = mongoose_acc:new(#{ location => ?LOCATION,
                                                             lserver => LServer,
                                                             from_jid => From,
                                                             to_jid => To,
                                                             element => Packet }),
                                  mod_amp:check_packet(NAcc, From, archived)
                          end, Msgs);
        {error, Reason} ->
            ?ERROR_MSG("~ts@~ts: write_messages failed with ~p.",
                [LUser, LServer, Reason]),
            discard_warn_sender(Acc, Msgs)
    end.

-spec is_message_count_threshold_reached(integer(), jid:luser(),
                                         jid:lserver(), integer()) ->
    boolean().
is_message_count_threshold_reached(infinity, _LUser, _LServer, _Len) ->
    false;
is_message_count_threshold_reached(MaxOfflineMsgs, _LUser, _LServer, Len)
  when Len > MaxOfflineMsgs ->
    true;
is_message_count_threshold_reached(MaxOfflineMsgs, LUser, LServer, Len) ->
    %% Only count messages if needed.
    MaxArchivedMsg = MaxOfflineMsgs - Len,
    %% Maybe do not need to count all messages in archive
    MaxArchivedMsg < mod_offline_backend:count_offline_messages(LUser, LServer, MaxArchivedMsg + 1).



get_max_user_messages(AccessRule, LUser, Host) ->
    case acl:match_rule(Host, AccessRule, jid:make(LUser, Host, <<>>)) of
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
     permanent, 5000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

stop_worker(Host) ->
    Proc = srv_name(Host),
    ejabberd_sup:stop_child(Proc).

start_link(Name, Host, AccessMaxOfflineMsgs) ->
    gen_server:start_link({local, Name}, ?MODULE, [Host, AccessMaxOfflineMsgs], []).

srv_name() ->
    mod_offline.

srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).

determine_amp_strategy(Strategy = #amp_strategy{deliver = [none]},
                       _FromJID, ToJID, _Packet, initial_check) ->
    #jid{luser = LUser, lserver = LServer} = ToJID,
    ShouldBeStored = ejabberd_auth:is_user_exists(LUser, LServer),
    case ShouldBeStored of
        true -> Strategy#amp_strategy{deliver = [stored, none]};
        false -> Strategy
    end;
determine_amp_strategy(Strategy, _, _, _, _) ->
    Strategy.

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
handle_call({pop_offline_messages, LUser, LServer}, {Pid, _}, State) ->
    Result = mod_offline_backend:pop_messages(LUser, LServer),
    NewPoppers = monitored_map:put({LUser, LServer}, Pid, Pid, State#state.message_poppers),
    {reply, Result, State#state{message_poppers = NewPoppers}};
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
handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State) ->
    NewPoppers = monitored_map:handle_info(Msg, State#state.message_poppers),
    {noreply, State#state{message_poppers = NewPoppers}};
handle_info({Acc, Msg = #offline_msg{us = US}},
            State = #state{access_max_user_messages = AccessMaxOfflineMsgs}) ->
    handle_offline_msg(Acc, Msg, AccessMaxOfflineMsgs),
    case monitored_map:find(US, State#state.message_poppers) of
        {ok, Pid} ->
            Pid ! new_offline_messages;
        error -> ok
    end,
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

%% This function should be called only from hook
%% Calling it directly is dangerous and my store unwanted message
%% in the offline storage (f.e. messages of type error or groupchat)
%% #rh
inspect_packet(Acc, From, To, Packet) ->
    case check_event_chatstates(Acc, From, To, Packet) of
        true ->
            Acc1 = store_packet(Acc, From, To, Packet),
            {stop, Acc1};
        false ->
            Acc
    end.

store_packet(Acc, From, To = #jid{luser = LUser, lserver = LServer},
             Packet = #xmlel{children = Els}) ->
    TimeStamp =
    case exml_query:subelement(Packet, <<"delay">>) of
        undefined ->
            p1_time_compat:timestamp();
        #xmlel{name = <<"delay">>} = DelayEl ->
            case exml_query:attr(DelayEl, <<"stamp">>, <<>>) of
                <<"">> ->
                    p1_time_compat:timestamp();
                Stamp ->
                    jlib:datetime_binary_to_timestamp(Stamp)
            end
    end,

    Expire = find_x_expire(TimeStamp, Els),
    Pid = srv_name(LServer),
    Msg = #offline_msg{us = {LUser, LServer},
             timestamp = TimeStamp,
             expire = Expire,
             from = From,
             to = To,
             packet = jlib:remove_delay_tags(Packet)},
    Pid ! {Acc, Msg},
    mongoose_acc:set(offline, stored, true, Acc).

%% Check if the packet has any content about XEP-0022 or XEP-0085
check_event_chatstates(Acc, From, To, Packet) ->
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
            inspect_xevent(Acc, From, To, Packet, El)
    end.

inspect_xevent(Acc, From, To, Packet, XEvent) ->
    case exml_query:subelement(XEvent, <<"id">>) of
        undefined ->
            case exml_query:subelement(XEvent, <<"offline">>) of
                undefined ->
                    true;
                _ ->
                    ejabberd_router:route(To, From, Acc, patch_offline_message(Packet)),
                    true
            end;
        _ ->
            false
    end.

patch_offline_message(Packet) ->
    ID = case exml_query:attr(Packet, <<"id">>, <<>>) of
             <<"">> -> #xmlel{name = <<"id">>};
             S -> #xmlel{name = <<"id">>, children = [#xmlcdata{content = S}]}
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
    case exml_query:attr(El, <<"xmlns">>, <<>>) of
        ?NS_EVENT -> find_x_event_chatstates(Els, {El, B, C});
        ?NS_CHATSTATES -> find_x_event_chatstates(Els, {A, El, C});
        _ -> find_x_event_chatstates(Els, {A, B, true})
    end.

find_x_expire(_, []) ->
    never;
find_x_expire(TimeStamp, [#xmlcdata{} | Els]) ->
    find_x_expire(TimeStamp, Els);
find_x_expire(TimeStamp, [El | Els]) ->
    case exml_query:attr(El, <<"xmlns">>, <<>>) of
        ?NS_EXPIRE ->
            Val = exml_query:attr(El, <<"seconds">>, <<>>),
            case catch list_to_integer(binary_to_list(Val)) of
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

pop_offline_messages(Acc, User, Server) ->
    mongoose_acc:append(offline, messages, pop_offline_messages(User, Server), Acc).

pop_offline_messages(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case pop_messages(LUser, LServer) of
        {ok, Rs} ->
            lists:map(fun(R) ->
                Packet = resend_offline_message_packet(Server, R),
                compose_offline_message(R, Packet)
              end, Rs);
        {error, Reason} ->
            ?ERROR_MSG("~ts@~ts: pop_messages failed with ~p.", [LUser, LServer, Reason]),
            []
    end.

pop_messages(LUser, LServer) ->
    case gen_server:call(srv_name(LServer), {pop_offline_messages, LUser, LServer}) of
        {ok, RsAll} ->
            TimeStamp = os:timestamp(),
            Rs = skip_expired_messages(TimeStamp, lists:keysort(#offline_msg.timestamp, RsAll)),
            {ok, Rs};
        Other ->
            Other
    end.

get_personal_data(Username, Server) ->
    LUser = jid:nodeprep(Username),
    LServer = jid:nodeprep(Server),
    {ok, Messages} = mod_offline_backend:fetch_messages(LUser, LServer),
    [{offline, ["timestamp", "from", "to", "packet"], offline_messages_to_gdpr_format(Messages)}].

offline_messages_to_gdpr_format(MsgList) ->
    [offline_msg_to_gdpr_format(Msg) || Msg <- MsgList].

offline_msg_to_gdpr_format(#offline_msg{timestamp = Timestamp, from = From, to = To, packet = Packet}) ->
    NowUniversal = calendar:now_to_universal_time(Timestamp),
    {UTCTime, UTCDiff} = jlib:timestamp_to_iso(NowUniversal, utc),
    UTC = list_to_binary(UTCTime ++ UTCDiff),
    {UTC, jid:to_binary(From), jid:to_binary(jid:to_bare(To)), exml:to_binary(Packet)}.

skip_expired_messages(TimeStamp, Rs) ->
    [R || R <- Rs, not is_expired_message(TimeStamp, R)].

is_expired_message(_TimeStamp, #offline_msg{expire=never}) ->
    false;
is_expired_message(TimeStamp, #offline_msg{expire=ExpireTimeStamp}) ->
   ExpireTimeStamp < TimeStamp.

compose_offline_message(#offline_msg{from=From, to=To}, Packet) ->
    {route, From, To, Packet}.

resend_offline_message_packet(Server,
        #offline_msg{timestamp=TimeStamp, packet = Packet}) ->
    add_timestamp(TimeStamp, Server, Packet).

add_timestamp(undefined, _Server, Packet) ->
    Packet;
add_timestamp({_, _, Micro} = TimeStamp, Server, Packet) ->
    {D, {H, M, S}} = calendar:now_to_universal_time(TimeStamp),
    Time = {D, {H, M, S, Micro}},
    TimeStampXML = timestamp_xml(Server, Time),
    xml:append_subtags(Packet, [TimeStampXML]).

timestamp_xml(Server, Time) ->
    FromJID = jid:make(<<>>, Server, <<>>),
    jlib:timestamp_to_xml(Time, utc, FromJID, <<"Offline Storage">>).

remove_expired_messages(Host) ->
    mod_offline_backend:remove_expired_messages(Host).

remove_old_messages(Host, Days) ->
    Timestamp = fallback_timestamp(Days, os:timestamp()),
    mod_offline_backend:remove_old_messages(Host, Timestamp).

%% #rh
remove_user(Acc, User, Server) ->
    R = remove_user(User, Server),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, User, Server}),
    Acc.

remove_user(User, Server) ->
    mod_offline_backend:remove_user(User, Server).

%% Warn senders that their messages have been discarded:
discard_warn_sender(Acc, Msgs) ->
    lists:foreach(
      fun(#offline_msg{from=From, to=To, packet=Packet}) ->
              ErrText = <<"Your contact offline message queue is full."
                          " The message has been discarded.">>,
              Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
              amp_failed_event(Acc, From, Packet),
              {Acc1, Err} = jlib:make_error_reply(
                      Acc, Packet, mongoose_xmpp_errors:resource_constraint(Lang, ErrText)),
              ejabberd_router:route(To, From, Acc1, Err)
      end, Msgs).

fallback_timestamp(Days, {MegaSecs, Secs, _MicroSecs}) ->
    S = MegaSecs * 1000000 + Secs - 60 * 60 * 24 * Days,
    MegaSecs1 = S div 1000000,
    Secs1 = S rem 1000000,
    {MegaSecs1, Secs1, 0}.
