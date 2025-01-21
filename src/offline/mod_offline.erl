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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline).
-author('alexey@process-one.net').
-xep([{xep, 160}, {version, "1.0.1"}]).
-xep([{xep, 23}, {version, "1.3"}]).
-xep([{xep, 22}, {version, "1.4"}]).
-xep([{xep, 85}, {version, "2.1"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod handlers
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

%% Hook handlers
-export([inspect_packet/3,
         pop_offline_messages/3,
         remove_user/3,
         remove_domain/3,
         disco_features/3,
         determine_amp_strategy/3,
         amp_failed_event/3,
         get_personal_data/3]).

%% Admin API
-export([remove_expired_messages/2,
         remove_old_messages/3]).

%% Internal exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% helpers to be used from backend moudules
-export([is_expired_message/2]).

-export([config_metrics/1]).

-ignore_xref([
    behaviour_info/1, code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, init/1, start_link/3, terminate/2
]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("amp.hrl").
-include("mod_offline.hrl").
-include("mongoose_config_spec.hrl").

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).

-type msg() :: #offline_msg{us :: {jid:luser(), jid:lserver()},
                            timestamp :: integer(),
                            expire :: integer() | never,
                            from :: jid:jid(),
                            to :: jid:jid(),
                            packet :: exml:element()}.

-export_type([msg/0]).

-type poppers() :: monitored_map:t({jid:luser(), jid:lserver()}, pid()).

-record(state, {host_type :: mongooseim:host_type(),
                access_max_user_messages :: atom(),
                message_poppers = monitored_map:new() :: poppers()
               }).

-type state() :: #state{}.

%% Types used in backend callbacks
-type msg_count() :: non_neg_integer().
-type timestamp() :: integer().

-export_type([msg_count/0, timestamp/0]).

%% gen_mod callbacks
%% ------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{access_max_user_messages := AccessMaxOfflineMsgs} = Opts) ->
    mod_offline_backend:init(HostType, Opts),
    start_worker(HostType, AccessMaxOfflineMsgs),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(Host) ->
    stop_worker(Host),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"access_max_user_messages">> => #option{type = atom,
                                                            validate = access_rule},
                  <<"backend">> => #option{type = atom,
                                           validate = {module, mod_offline}},
                  <<"store_groupchat_messages">> => #option{type = boolean}
                 },
        defaults = #{<<"access_max_user_messages">> => max_user_offline_messages,
                     <<"store_groupchat_messages">> => false,
                     <<"backend">> => mnesia
                    }
    }.

supported_features() -> [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    DefaultHooks = [
        {offline_message, HostType, fun ?MODULE:inspect_packet/3, #{}, 50},
        {resend_offline_messages, HostType, fun ?MODULE:pop_offline_messages/3, #{}, 50},
        {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
        {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50},
        {anonymous_purge, HostType, fun ?MODULE:remove_user/3, #{}, 50},
        {disco_sm_features, HostType, fun ?MODULE:disco_features/3, #{}, 50},
        {disco_local_features, HostType, fun ?MODULE:disco_features/3, #{}, 50},
        {amp_determine_strategy, HostType, fun ?MODULE:determine_amp_strategy/3, #{}, 30},
        {failed_to_store_message, HostType, fun ?MODULE:amp_failed_event/3, #{}, 30},
        {get_personal_data, HostType, fun ?MODULE:get_personal_data/3, #{}, 50}
    ],
    case gen_mod:get_module_opt(HostType, ?MODULE, store_groupchat_messages) of
        true ->
            GroupChatHook = {offline_groupchat_message,
                             HostType, fun ?MODULE:inspect_packet/3, #{}, 50},
            [GroupChatHook | DefaultHooks];
        _ -> DefaultHooks
    end.

%% Server side functions
%% ------------------------------------------------------------------

-spec amp_failed_event(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
amp_failed_event(Acc, _Params, _Extra) ->
    {ok, mod_amp:check_packet(Acc, offline_failed)}.

handle_offline_msg(HostType, Acc, #offline_msg{us=US} = Msg, AccessMaxOfflineMsgs) ->
    {LUser, LServer} = US,
    Msgs = receive_all(US, [{Acc, Msg}]),
    MaxOfflineMsgs = get_max_user_messages(HostType, AccessMaxOfflineMsgs, LUser, LServer),
    Len = length(Msgs),
    case is_message_count_threshold_reached(HostType, MaxOfflineMsgs, LUser, LServer, Len) of
        false ->
            write_messages(HostType, LUser, LServer, Msgs);
        true ->
            discard_warn_sender(Msgs)
    end.

write_messages(HostType, LUser, LServer, Msgs) ->
    MsgsWithoutAcc = [Msg || {_Acc, Msg} <- Msgs],
    case mod_offline_backend:write_messages(HostType, LUser, LServer, MsgsWithoutAcc) of
        ok ->
            [mod_amp:check_packet(Acc, archived) || {Acc, _Msg} <- Msgs],
            ok;
        {error, Reason} ->
            ?LOG_ERROR(#{what => offline_write_failed,
                         text => <<"Failed to write offline messages">>,
                         reason => Reason,
                         user => LUser, server => LServer, msgs => Msgs}),
            discard_warn_sender(Msgs)
    end.

-spec is_message_count_threshold_reached(mongooseim:host_type(), integer() | infinity,
                                         jid:luser(), jid:lserver(), integer()) ->
          boolean().
is_message_count_threshold_reached(_HostType, infinity, _LUser, _LServer, _Len) ->
    false;
is_message_count_threshold_reached(_HostType, MaxOfflineMsgs, _LUser, _LServer, Len)
  when Len > MaxOfflineMsgs ->
    true;
is_message_count_threshold_reached(HostType, MaxOfflineMsgs, LUser, LServer, Len) ->
    %% Only count messages if needed.
    MaxArchivedMsg = MaxOfflineMsgs - Len,
    %% Maybe do not need to count all messages in archive
    MaxArchivedMsg < mod_offline_backend:count_offline_messages(HostType, LUser, LServer,
                                                                MaxArchivedMsg + 1).


get_max_user_messages(HostType, AccessRule, LUser, LServer) ->
    case acl:match_rule(HostType, LServer, AccessRule, jid:make_noprep(LUser, LServer, <<>>)) of
        Max when is_integer(Max) -> Max;
        infinity -> infinity;
        _ -> ?MAX_USER_MESSAGES
    end.

receive_all(US, Msgs) ->
    receive
        {_Acc, #offline_msg{us=US}} = Msg ->
            receive_all(US, [Msg | Msgs])
    after 0 ->
              Msgs
    end.

%% Supervision
%% ------------------------------------------------------------------

start_worker(HostType, AccessMaxOfflineMsgs) ->
    Proc = srv_name(HostType),
    ChildSpec =
    {Proc,
     {?MODULE, start_link, [Proc, HostType, AccessMaxOfflineMsgs]},
     permanent, 5000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

stop_worker(HostType) ->
    Proc = srv_name(HostType),
    ejabberd_sup:stop_child(Proc).

start_link(Name, HostType, AccessMaxOfflineMsgs) ->
    gen_server:start_link({local, Name}, ?MODULE, [HostType, AccessMaxOfflineMsgs], []).

srv_name() ->
    mod_offline.

srv_name(HostType) ->
    gen_mod:get_module_proc(HostType, srv_name()).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, state()}.
init([HostType, AccessMaxOfflineMsgs]) ->
    {ok, #state{host_type = HostType,
                access_max_user_messages = AccessMaxOfflineMsgs}}.

-spec handle_call(Request :: any(), {pid(), any()}, state()) -> {reply, Result, state()}
              when Result :: ok | {ok, [msg()]} | {error, any()}.
handle_call({pop_offline_messages, JID}, {Pid, _}, State = #state{host_type = HostType}) ->
    Result = mod_offline_backend:pop_messages(HostType, JID),
    NewPoppers = monitored_map:put(jid:to_lus(JID), Pid, Pid, State#state.message_poppers),
    {reply, Result, State#state{message_poppers = NewPoppers}};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State) ->
    NewPoppers = monitored_map:handle_info(Msg, State#state.message_poppers),
    {noreply, State#state{message_poppers = NewPoppers}};
handle_info({Acc, Msg = #offline_msg{us = US}},
            State = #state{host_type = HostType,
                           access_max_user_messages = AccessMaxOfflineMsgs}) ->
    handle_offline_msg(HostType, Acc, Msg, AccessMaxOfflineMsgs),
    case monitored_map:find(US, State#state.message_poppers) of
        {ok, Pid} ->
            Pid ! new_offline_messages;
        error -> ok
    end,
    {noreply, State};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Handlers
%% ------------------------------------------------------------------

%% This function should be called only from a hook
%% Calling it directly is dangerous and may store unwanted messages
%% in the offline storage (e.g. messages of type error)
-spec inspect_packet(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
inspect_packet(Acc, #{from := From, to := To, packet := Packet}, _Extra) ->
    case check_event_chatstates(Acc, From, To, Packet) of
        true ->
            Acc1 = store_packet(Acc, From, To, Packet),
            {stop, Acc1};
        false ->
            {ok, Acc}
    end.

-spec store_packet(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
store_packet(Acc, From, To = #jid{luser = LUser, lserver = LServer},
             Packet = #xmlel{children = Els}) ->
    TimeStamp = get_or_build_timestamp_from_packet(Packet),
    Expire = find_x_expire(TimeStamp, Els),
    HostType = mongoose_acc:host_type(Acc),
    Pid = srv_name(HostType),
    PermanentFields = mongoose_acc:get_permanent_fields(Acc),
    Msg = #offline_msg{us = {LUser, LServer},
                       timestamp = TimeStamp,
                       expire = Expire,
                       from = From,
                       to = To,
                       packet = jlib:remove_delay_tags(Packet),
                       permanent_fields = PermanentFields},
    Pid ! {Acc, Msg},
    mongoose_acc:set(offline, stored, true, Acc).

-spec get_or_build_timestamp_from_packet(exml:element()) -> integer().
get_or_build_timestamp_from_packet(Packet) ->
    case exml_query:path(Packet, [{element, <<"delay">>}, {attr, <<"stamp">>}]) of
        undefined ->
            erlang:system_time(microsecond);
        Stamp ->
            try
                calendar:rfc3339_to_system_time(binary_to_list(Stamp), [{unit, microsecond}])
            catch
                error:_Error -> erlang:system_time(microsecond)
            end
    end.

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
        attrs = #{<<"xmlns">> => ?NS_EVENT},
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
            try binary_to_integer(Val) of
                Int when Int > 0 ->
                    ExpireMicroSeconds = erlang:convert_time_unit(Int, second, microsecond),
                    TimeStamp + ExpireMicroSeconds;
                _ ->
                    never
            catch
                error:badarg -> never
            end;
        _ ->
            find_x_expire(TimeStamp, Els)
    end.

-spec pop_offline_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
pop_offline_messages(Acc, #{jid := JID}, _Extra) ->
    {ok, mongoose_acc:append(offline, messages, offline_messages(Acc, JID), Acc)}.

-spec offline_messages(mongoose_acc:t(), jid:jid()) ->
          [{route, jid:jid(), jid:jid(), mongoose_acc:t()}].
offline_messages(Acc, #jid{lserver = LServer} = JID) ->
    HostType = mongoose_acc:host_type(Acc),
    case pop_messages(HostType, JID) of
        {ok, Rs} ->
            lists:map(fun(R) ->
                Packet = resend_offline_message_packet(LServer, R),
                compose_offline_message(R, Packet, Acc)
              end, Rs);
        {error, Reason} ->
            ?LOG_WARNING(#{what => offline_pop_failed, reason => Reason, acc => Acc}),
            []
    end.

-spec pop_messages(mongooseim:host_type(), jid:jid()) -> {ok, [msg()]} | {error, any()}.
pop_messages(HostType, JID) ->
    case gen_server:call(srv_name(HostType), {pop_offline_messages, jid:to_bare(JID)}) of
        {ok, RsAll} ->
            TimeStamp = erlang:system_time(microsecond),
            Rs = skip_expired_messages(TimeStamp, lists:keysort(#offline_msg.timestamp, RsAll)),
            {ok, Rs};
        Other ->
            Other
    end.

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, #{host_type := HostType}) ->
    mod_offline_backend:remove_user(HostType, LUser, LServer),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_domain_api:remove_domain_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    mod_offline_backend:remove_domain(HostType, Domain),
    {ok, Acc}.

-spec disco_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_features(Acc = #{node := <<>>}, _Params, _Extra) ->
    {ok, mongoose_disco:add_features([?NS_FEATURE_MSGOFFLINE], Acc)};
disco_features(Acc = #{node := ?NS_FEATURE_MSGOFFLINE}, _Params, _Extra) ->
    %% override all lesser features...
    {ok, Acc#{result := []}};
disco_features(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec determine_amp_strategy(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_amp:amp_strategy(),
    Params :: map(),
    Extra :: gen_hook:extra().
determine_amp_strategy(Strategy = #amp_strategy{deliver = [none]}, #{to := ToJID, event := initial_check}, _Params) ->
    case ejabberd_auth:does_user_exist(ToJID) of
        true -> {ok, Strategy#amp_strategy{deliver = [stored, none]}};
        false -> {ok, Strategy}
    end;
determine_amp_strategy(Strategy, _Params, _Extra) ->
    {ok, Strategy}.

-spec get_personal_data(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc ::  gdpr:personal_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
get_personal_data(Acc, #{jid := JID}, #{host_type := HostType}) ->
    {ok, Messages} = mod_offline_backend:fetch_messages(HostType, JID),
    {ok, [{offline, ["timestamp", "from", "to", "packet"],
           offline_messages_to_gdpr_format(Messages)} | Acc]}.

offline_messages_to_gdpr_format(MsgList) ->
    [offline_msg_to_gdpr_format(Msg) || Msg <- MsgList].

offline_msg_to_gdpr_format(#offline_msg{timestamp = TimeStamp, from = From,
                                        to = To, packet = Packet}) ->
    SystemTime = erlang:convert_time_unit(TimeStamp, microsecond, second),
    UTCTime = calendar:system_time_to_rfc3339(SystemTime, [{offset, "Z"}]),
    UTC = list_to_binary(UTCTime),
    {UTC, jid:to_binary(From), jid:to_bare_binary(To), exml:to_binary(Packet)}.

skip_expired_messages(TimeStamp, Rs) ->
    [R || R <- Rs, not is_expired_message(TimeStamp, R)].

is_expired_message(_TimeStamp, #offline_msg{expire=never}) ->
    false;
is_expired_message(TimeStamp, #offline_msg{expire=ExpireTimeStamp}) ->
   ExpireTimeStamp < TimeStamp.

compose_offline_message(#offline_msg{from = From, to = To, permanent_fields = PermanentFields},
                        Packet, Acc0) ->
    Acc1 = mongoose_acc:set_permanent(PermanentFields, Acc0),
    Acc = mongoose_acc:update_stanza(#{element => Packet, from_jid => From, to_jid => To}, Acc1),
    {route, From, To, Acc}.

resend_offline_message_packet(LServer,
        #offline_msg{timestamp=TimeStamp, packet = Packet}) ->
    add_timestamp(TimeStamp, LServer, Packet).

add_timestamp(undefined, _LServer, Packet) ->
    Packet;
add_timestamp(TimeStamp, LServer, Packet) ->
    TimeStampXML = timestamp_xml(LServer, TimeStamp),
    jlib:append_subtags(Packet, [TimeStampXML]).

timestamp_xml(LServer, Time) ->
    FromJID = jid:make_noprep(<<>>, LServer, <<>>),
    TS = calendar:system_time_to_rfc3339(Time, [{offset, "Z"}, {unit, microsecond}]),
    jlib:timestamp_to_xml(TS, FromJID, <<"Offline Storage">>).

-spec remove_expired_messages(mongooseim:host_type(), jid:lserver()) -> {ok, msg_count()} | {error, any()}.
remove_expired_messages(HostType, LServer) ->
    Result = mod_offline_backend:remove_expired_messages(HostType, LServer),
    mongoose_lib:log_if_backend_error(Result, ?MODULE, ?LINE, [HostType]),
    Result.

-spec remove_old_messages(mongooseim:host_type(), jid:lserver(), non_neg_integer()) ->
          {ok, msg_count()} | {error, any()}.
remove_old_messages(HostType, LServer, HowManyDays) ->
    Timestamp = fallback_timestamp(HowManyDays, erlang:system_time(microsecond)),
    Result = mod_offline_backend:remove_old_messages(HostType, LServer, Timestamp),
    mongoose_lib:log_if_backend_error(Result, ?MODULE, ?LINE, [HostType, Timestamp]),
    Result.

%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(
      fun({Acc, #offline_msg{from=From, to=To, packet=Packet}}) ->
              ErrText = <<"Your contact offline message queue is full."
                          " The message has been discarded.">>,
              Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
              mod_amp:check_packet(Acc, offline_failed),
              {Acc1, Err} = jlib:make_error_reply(
                      Acc, Packet, mongoose_xmpp_errors:resource_constraint(Lang, ErrText)),
              ejabberd_router:route(To, From, Acc1, Err)
      end, Msgs).

fallback_timestamp(HowManyDays, TS_MicroSeconds) ->
    HowManySeconds = HowManyDays * 86400,
    HowManyMicroSeconds = erlang:convert_time_unit(HowManySeconds, second, microsecond),
    TS_MicroSeconds - HowManyMicroSeconds.

config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).
