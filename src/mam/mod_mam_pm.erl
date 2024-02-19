%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc XEP-0313: Message Archive Management
%%%
%%% The module uses several backend modules:
%%%
%%% <ul>
%%% <li>Preference manager ({@link mod_mam_muc_rdbms_prefs});</li>
%%% <li>Writer ({@link mod_mam_muc_rdbms_arch} or {@link mod_mam_muc_rdbms_async_pool_writer});</li>
%%% <li>Archive manager ({@link mod_mam_muc_rdbms_arch});</li>
%%% <li>User's ID generator ({@link mod_mam_muc_user}).</li>
%%% </ul>
%%%
%%% Preferences can be also stored in Mnesia ({@link mod_mam_mnesia_prefs}).
%%% This module handles simple archives.
%%%
%%% This module should be started for each host.
%%% Message archivation is not shaped here (use standard support for this).
%%% MAM's IQs are shaped inside {@link opuntia_srv}.
%%%
%%% Message identifiers (or UIDs in the spec) are generated based on:
%%%
%%% <ul>
%%% <li>date (using `timestamp()');</li>
%%% <li>node number (using {@link mongoose_node_num}).</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_pm).
-behavior(gen_mod).
-behaviour(mongoose_module_metrics).
%% ----------------------------------------------------------------------
%% Exports

%% Client API
-export([delete_archive/1,
         archive_size/2,
         archive_size_with_host_type/3,
         archive_id/2]).

%% gen_mod handlers
-export([start/2, stop/1, supported_features/0]).

%% hook handlers
-export([disco_local_features/3,
         disco_sm_features/3,
         user_send_message/3,
         filter_packet/3,
         remove_user/3,
         determine_amp_strategy/3,
         sm_filter_offline_message/3]).

%% ejabberd handlers
-export([process_mam_iq/5]).

%% gdpr callbacks
-export([get_personal_data/3]).

%%private
-export([archive_message_from_ct/1]).
-export([lookup_messages/2]).
-export([archive_id_int/2]).

-ignore_xref([archive_message_from_ct/1, archive_size/2,
              archive_size_with_host_type/3, delete_archive/1]).

-type host_type() :: mongooseim:host_type().

-include("mongoose.hrl").
-include("jlib.hrl").
-include("amp.hrl").

%% ----------------------------------------------------------------------
%% API

-spec delete_archive(jid:jid()) -> 'ok'.
delete_archive(ArcJID) ->
    ?LOG_DEBUG(#{what => mam_delete_archive, jid => ArcJID}),
    HostType = jid_to_host_type(ArcJID),
    ArcID = archive_id_int(HostType, ArcJID),
    remove_archive_hook(HostType, ArcID, ArcJID),
    ok.

-spec archive_size(jid:server(), jid:user()) -> integer().
archive_size(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make_bare(User, Server),
    HostType = jid_to_host_type(ArcJID),
    ArcID = archive_id_int(HostType, ArcJID),
    archive_size(HostType, ArcID, ArcJID).

-spec archive_size_with_host_type(host_type(), jid:server(), jid:user()) -> integer().
archive_size_with_host_type(HostType, Server, User) ->
    ArcJID = jid:make_bare(User, Server),
    ArcID = archive_id_int(HostType, ArcJID),
    archive_size(HostType, ArcID, ArcJID).

-spec archive_id(jid:server(), jid:user()) -> integer() | undefined.
archive_id(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make_bare(User, Server),
    HostType = jid_to_host_type(ArcJID),
    archive_id_int(HostType, ArcJID).

%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    ?LOG_INFO(#{what => mam_starting, host_type => HostType}),
    ensure_metrics(HostType),
    gen_hook:add_handlers(hooks(HostType)),
    add_iq_handlers(HostType, Opts),
    ok.

-spec stop(host_type()) -> any().
stop(HostType) ->
    ?LOG_INFO(#{what => mam_stopping, host_type => HostType}),
    gen_hook:delete_handlers(hooks(HostType)),
    remove_iq_handlers(HostType),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%% ----------------------------------------------------------------------
%% hooks and handlers

%% `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec process_mam_iq(Acc :: mongoose_acc:t(),
                     From :: jid:jid(), To :: jid:jid(), IQ :: jlib:iq(),
                     _Extra) -> {mongoose_acc:t(), jlib:iq() | ignore}.
process_mam_iq(Acc, From, To, IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    mod_mam_utils:maybe_log_deprecation(IQ),
    Action = mam_iq:action(IQ),
    case is_action_allowed(HostType, Action, From, To) of
        true  ->
            case mod_mam_utils:wait_shaper(HostType, To#jid.lserver, Action, From) of
                continue ->
                    handle_error_iq(HostType, Acc, To, Action,
                                    handle_mam_iq(Action, From, To, IQ, Acc));
                {error, max_delay_reached} ->
                    ?LOG_WARNING(#{what => mam_max_delay_reached,
                                   text => <<"Return max_delay_reached error IQ from MAM">>,
                                   action => Action, acc => Acc}),
                    mongoose_metrics:update(HostType, modMamDroppedIQ, 1),
                    {Acc, return_max_delay_reached_error_iq(IQ)}
            end;
        false ->
            mongoose_metrics:update(HostType, modMamDroppedIQ, 1),
            {Acc, return_action_not_allowed_error_iq(IQ)}
    end.

-spec disco_local_features(mongoose_disco:feature_acc(),
                           map(),
                           map()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{host_type := HostType, node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features(mod_mam_utils:features(?MODULE, HostType), Acc)};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

-spec disco_sm_features(mongoose_disco:feature_acc(),
                        map(), map()) -> {ok, mongoose_disco:feature_acc()}.
disco_sm_features(Acc = #{host_type := HostType, node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features(mod_mam_utils:features(?MODULE, HostType), Acc)};
disco_sm_features(Acc, _, _) ->
    {ok, Acc}.

%% @doc Handle an outgoing message.
%%
%% Note: for outgoing messages, the server MUST use the value of the 'to'
%%       attribute as the target JID.
-spec user_send_message(Acc, Args, Extra) -> {ok, Acc} when
       Acc :: mongoose_acc:t(),
       Args :: map(),
       Extra :: gen_hook:extra().
user_send_message(Acc, _, _) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    ?LOG_DEBUG(#{what => mam_user_send_message, acc => Acc}),
    {_, Acc2} = handle_package(outgoing, true, From, To, From, Packet, Acc),
    {ok, Acc2}.

%% @doc Handle an incoming message.
%%
%% Note: For incoming messages, the server MUST use the value of the
%%       'from' attribute as the target JID.
%%
%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
-spec filter_packet(FPacketAcc, Params, Extra) -> {ok, FPacketAcc} when
      FPacketAcc :: mongoose_hooks:filter_packet_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
filter_packet({From, To, Acc1, Packet}, _, _) ->
    ?LOG_DEBUG(#{what => mam_user_receive_packet, acc => Acc1}),
    HostType = mongoose_acc:host_type(Acc1),
    Type = mongoose_lib:get_message_type(Acc1),
    {AmpEvent, PacketAfterArchive, Acc3} =
        case mongoose_lib:does_local_user_exist(HostType, To, Type) of
            false ->
                {mam_failed, Packet, Acc1};
            true ->
                case process_incoming_packet(From, To, Packet, Acc1) of
                    {undefined, Acc2} ->
                        {mam_failed, Packet, Acc2};
                    {MessID, Acc2} ->
                        Packet2 = mod_mam_utils:maybe_add_arcid_elems(
                                     To, MessID, Packet,
                                     mod_mam_params:add_stanzaid_element(?MODULE, HostType)),
                        {archived, Packet2, Acc2}
                end
        end,
    Acc4 = mongoose_acc:update_stanza(#{ element => PacketAfterArchive,
                                         from_jid => From,
                                         to_jid => To }, Acc3),
    Acc5 = mod_amp:check_packet(Acc4, AmpEvent),
    {ok, {From, To, Acc5, mongoose_acc:element(Acc5)}}.

process_incoming_packet(From, To, Packet, Acc) ->
    handle_package(incoming, true, To, From, From, Packet, Acc).

%% hook handler
-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
remove_user(Acc, #{jid := JID}, _) ->
    delete_archive(JID),
    {ok, Acc}.

-spec determine_amp_strategy(StrategyAcc, Params, Extra) -> {ok, StrategyAcc} when
      StrategyAcc :: mod_amp:amp_strategy(),
      Params :: #{from := jid:jid(), to := jid:jid(), packet := exml:element(), event := mod_amp:amp_event()},
      Extra :: gen_hook:extra().
determine_amp_strategy(Strategy = #amp_strategy{deliver = Deliver},
                       #{from := FromJID, to := ToJID, packet := Packet, event := initial_check},
                       _) ->
    HostType = jid_to_host_type(ToJID),
    ShouldBeStored = is_archivable_message(HostType, incoming, Packet)
        andalso is_interesting(ToJID, FromJID)
        andalso ejabberd_auth:does_user_exist(ToJID),
    NewStrategy = case ShouldBeStored of
        true -> Strategy#amp_strategy{deliver = amp_deliver_strategy(Deliver)};
        false -> Strategy
    end,
    {ok, NewStrategy};
determine_amp_strategy(Strategy, _, _) ->
    {ok, Strategy}.

-spec sm_filter_offline_message(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: boolean(),
      Params :: #{packet := exml:element()},
      Extra :: gen_hook:extra().
sm_filter_offline_message(_Drop=false, #{packet := Packet}, _) ->
    %% If ...
    {ok, mod_mam_utils:is_mam_result_message(Packet)};
    %% ... than drop the message
sm_filter_offline_message(Other, _, _) ->
    {ok, Other}.

-spec get_personal_data(Acc, Params, Extra) -> {ok, Acc} when
       Acc :: gdpr:personal_data(),
       Params :: #{jid := jid:jid()},
       Extra :: #{host_type := mongooseim:host_type()}.
get_personal_data(Acc, #{jid := ArcJID}, #{host_type := HostType}) ->
    Schema = ["id", "from", "message"],
    Entries = mongoose_hooks:get_mam_pm_gdpr_data(HostType, ArcJID),
    {ok, [{mam_pm, Schema, Entries} | Acc]}.

%% ----------------------------------------------------------------------
%% Internal functions

-spec jid_to_host_type(jid:jid()) -> host_type().
jid_to_host_type(#jid{lserver=LServer}) ->
    lserver_to_host_type(LServer).

lserver_to_host_type(LServer) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            HostType;
        {error, not_found} ->
            error({get_domain_host_type_failed, LServer})
    end.

-spec acc_to_host_type(mongoose_acc:t()) -> host_type().
acc_to_host_type(Acc) ->
    case mongoose_acc:host_type(Acc) of
        undefined ->
            lserver_to_host_type(mongoose_acc:lserver(Acc));
        HostType ->
            HostType
    end.

-spec is_action_allowed(HostType :: host_type(),
                        Action :: mam_iq:action(), From :: jid:jid(),
                        To :: jid:jid()) -> boolean().
is_action_allowed(HostType, Action, From, To) ->
    case acl:match_rule(HostType, To#jid.lserver, Action, From, default) of
        allow   -> true;
        deny    -> false;
        default -> is_action_allowed_by_default(Action, From, To)
    end.

-spec is_action_allowed_by_default(Action :: mam_iq:action(), From :: jid:jid(),
                                   To :: jid:jid()) -> boolean().
is_action_allowed_by_default(_Action, From, To) ->
    jid:are_bare_equal(From, To).

-spec handle_mam_iq(mam_iq:action(), From :: jid:jid(), To :: jid:jid(),
                    IQ :: jlib:iq(), Acc :: mongoose_acc:t()) ->
    jlib:iq() | {error, term(), jlib:iq()}.
handle_mam_iq(Action, From, To, IQ, Acc) ->
    case Action of
        mam_get_prefs ->
            handle_get_prefs(To, IQ, Acc);
        mam_set_prefs ->
            handle_set_prefs(To, IQ, Acc);
        mam_set_message_form ->
            handle_set_message_form(From, To, IQ, Acc);
        mam_get_message_form ->
            handle_get_message_form(From, To, IQ, Acc);
        mam_get_metadata ->
            handle_get_metadata(From, IQ, Acc)
    end.

-spec handle_set_prefs(jid:jid(), jlib:iq(), mongoose_acc:t()) ->
                              jlib:iq() | {error, term(), jlib:iq()}.
handle_set_prefs(ArcJID=#jid{}, IQ=#iq{sub_el = PrefsEl}, Acc) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} = mod_mam_utils:parse_prefs(PrefsEl),
    ?LOG_DEBUG(#{what => mam_set_prefs, default_mode => DefaultMode,
                 always_jids => AlwaysJIDs, never_jids => NeverJIDs, iq => IQ}),
    HostType = acc_to_host_type(Acc),
    ArcID = archive_id_int(HostType, ArcJID),
    Res = set_prefs(HostType, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs),
    handle_set_prefs_result(Res, DefaultMode, AlwaysJIDs, NeverJIDs, IQ).

handle_set_prefs_result(ok, DefaultMode, AlwaysJIDs, NeverJIDs, IQ) ->
    Namespace = IQ#iq.xmlns,
    ResultPrefsEl = mod_mam_utils:result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_set_prefs_result({error, Reason},
                        _DefaultMode, _AlwaysJIDs, _NeverJIDs, IQ) ->
    return_error_iq(IQ, Reason).

-spec handle_get_prefs(jid:jid(), IQ :: jlib:iq(), Acc :: mongoose_acc:t()) ->
                              jlib:iq() | {error, term(), jlib:iq()}.
handle_get_prefs(ArcJID=#jid{}, IQ=#iq{}, Acc) ->
    HostType = acc_to_host_type(Acc),
    ArcID = archive_id_int(HostType, ArcJID),
    Res = get_prefs(HostType, ArcID, ArcJID, always),
    handle_get_prefs_result(Res, IQ).

handle_get_prefs_result({DefaultMode, AlwaysJIDs, NeverJIDs}, IQ) ->
    ?LOG_DEBUG(#{what => mam_get_prefs_result, default_mode => DefaultMode,
                 always_jids => AlwaysJIDs, never_jids => NeverJIDs, iq => IQ}),
    Namespace = IQ#iq.xmlns,
    ResultPrefsEl = mod_mam_utils:result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_get_prefs_result({error, Reason}, IQ) ->
    return_error_iq(IQ, Reason).

-spec handle_set_message_form(From :: jid:jid(), ArcJID :: jid:jid(),
                              IQ :: jlib:iq(), Acc :: mongoose_acc:t()) ->
    jlib:iq() | ignore | {error, term(), jlib:iq()}.
handle_set_message_form(#jid{} = From, #jid{} = ArcJID, #iq{} = IQ, Acc) ->
    HostType = acc_to_host_type(Acc),
    ArcID = archive_id_int(HostType, ArcJID),
    try iq_to_lookup_params(HostType, IQ) of
        Params0 ->
            do_handle_set_message_form(Params0, From, ArcID, ArcJID, IQ, HostType)
    catch _C:R:S ->
            report_issue({R, S}, mam_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, R)
    end.


-spec do_handle_set_message_form(Params :: mam_iq:lookup_params(),
                                 From :: jid:jid(),
                                 ArcId :: mod_mam:archive_id(),
                                 ArcJID :: jid:jid(),
                                 IQ :: jlib:iq(),
                                 HostType :: mongooseim:host_type()) ->
    jlib:iq() | ignore | {error, term(), jlib:iq()}.
do_handle_set_message_form(Params0, From, ArcID, ArcJID,
                           #iq{xmlns=MamNs, sub_el = QueryEl} = IQ,
                           HostType) ->
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID, From),
    case mod_mam_utils:lookup(HostType, Params, fun lookup_messages/2) of
        {error, Reason} ->
            report_issue(Reason, mam_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, #{total_count := TotalCount, offset := Offset, messages := MessageRows,
               is_complete := IsComplete}} ->
            %% Reverse order of messages if the client requested it
            MessageRows1 = mod_mam_utils:maybe_reverse_messages(Params0, MessageRows),
            %% Forward messages
            {FirstMessID, LastMessID} = forward_messages(HostType, From, ArcJID, MamNs,
                                                         QueryID, MessageRows1, true),
            %% Make fin iq
            IsStable = true,
            ResultSetEl = mod_mam_utils:result_set(FirstMessID, LastMessID, Offset, TotalCount),
            ExtFinMod = mod_mam_params:extra_fin_element_module(?MODULE, HostType),
            FinElem = mod_mam_utils:make_fin_element(HostType, Params, IQ#iq.xmlns,
                                                     IsComplete, IsStable,
                                                     ResultSetEl, ExtFinMod),
            IQ#iq{type = result, sub_el = [FinElem]}
    end.

iq_to_lookup_params(HostType, IQ) ->
    Max = mod_mam_params:max_result_limit(?MODULE, HostType),
    Def = mod_mam_params:default_result_limit(?MODULE, HostType),
    Ext = mod_mam_params:extra_params_module(?MODULE, HostType),
    Sim = mod_mam_params:enforce_simple_queries(?MODULE, HostType),
    mam_iq:form_to_lookup_params(IQ, Max, Def, Ext, Sim).

forward_messages(HostType, From, ArcJID, MamNs, QueryID, MessageRows, SetClientNs) ->
    %% Forward messages
    {FirstMessID, LastMessID} =
    case MessageRows of
        [] -> {undefined, undefined};
        [_|_] -> {message_row_to_ext_id(hd(MessageRows)),
                  message_row_to_ext_id(lists:last(MessageRows))}
    end,
    SendModule = mod_mam_params:send_message_mod(?MODULE, HostType),
    [send_message(SendModule, Row, ArcJID, From,
                  message_row_to_xml(MamNs, Row, QueryID, SetClientNs))
     || Row <- MessageRows],
    {FirstMessID, LastMessID}.

send_message(SendModule, Row, ArcJID, From, Packet) ->
    mam_send_message:call_send_message(SendModule, Row, ArcJID, From, Packet).

-spec handle_get_message_form(jid:jid(), jid:jid(), jlib:iq(), mongoose_acc:t()) ->
                                     jlib:iq().
handle_get_message_form(_From=#jid{}, _ArcJID=#jid{}, IQ=#iq{}, Acc) ->
    HostType = acc_to_host_type(Acc),
    return_message_form_iq(HostType, IQ).

-spec handle_get_metadata(jid:jid(), jlib:iq(), mongoose_acc:t()) ->
                                 jlib:iq() | {error, term(), jlib:iq()}.
handle_get_metadata(ArcJID=#jid{}, IQ=#iq{}, Acc) ->
    HostType = acc_to_host_type(Acc),
    ArcID = archive_id_int(HostType, ArcJID),
    case mod_mam_utils:lookup_first_and_last_messages(HostType, ArcID, ArcJID,
                                                      fun lookup_messages/2) of
        {error, Reason} ->
            report_issue(Reason, mam_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {FirstMsg, LastMsg} ->
            {FirstMsgID, FirstMsgTS} = mod_mam_utils:get_msg_id_and_timestamp(FirstMsg),
            {LastMsgID, LastMsgTS} = mod_mam_utils:get_msg_id_and_timestamp(LastMsg),
            MetadataElement =
                mod_mam_utils:make_metadata_element(FirstMsgID, FirstMsgTS, LastMsgID, LastMsgTS),
            IQ#iq{type = result, sub_el = [MetadataElement]};
        empty_archive ->
            MetadataElement = mod_mam_utils:make_metadata_element(),
            IQ#iq{type = result, sub_el = [MetadataElement]}
    end.

amp_deliver_strategy([none]) -> [stored, none];
amp_deliver_strategy([direct, none]) -> [direct, stored, none].

-spec handle_package(Dir :: incoming | outgoing, ReturnMessID :: boolean(),
                     LocJID :: jid:jid(), RemJID :: jid:jid(), SrcJID :: jid:jid(),
                     Packet :: exml:element(), Acc :: mongoose_acc:t()) ->
    {MaybeMessID :: binary() | undefined, Acc :: mongoose_acc:t()}.
handle_package(Dir, ReturnMessID,
               LocJID = #jid{}, RemJID = #jid{}, SrcJID = #jid{}, Packet, Acc) ->
    HostType = acc_to_host_type(Acc),
    MsgType = exml_query:attr(Packet, <<"type">>),
    case is_archivable_message(HostType, Dir, Packet)
         andalso should_archive_if_groupchat(HostType, MsgType)
         andalso should_archive_if_sent_to_yourself(LocJID, RemJID, Dir) of
        true ->
            ArcID = archive_id_int(HostType, LocJID),
            OriginID = mod_mam_utils:get_origin_id(Packet),
            case is_interesting(HostType, LocJID, RemJID, ArcID) of
                true ->
                    MessID = mod_mam_utils:get_or_generate_mam_id(Acc),
                    IsGroupChat = mod_mam_utils:is_groupchat(MsgType),
                    Params = #{message_id => MessID,
                               archive_id => ArcID,
                               local_jid => LocJID,
                               remote_jid => RemJID,
                               source_jid => SrcJID,
                               origin_id => OriginID,
                               direction => Dir,
                               packet => Packet,
                               is_groupchat => IsGroupChat},
                    Result = archive_message(HostType, Params),
                    ExtMessId = return_external_message_id_if_ok(ReturnMessID, Result, MessID),
                    {ExtMessId, return_acc_with_mam_id_if_configured(ExtMessId, HostType, Acc)};
                false ->
                    {undefined, Acc}
            end;
        false ->
            {undefined, Acc}
    end.

should_archive_if_groupchat(HostType, <<"groupchat">>) ->
    gen_mod:get_module_opt(HostType, ?MODULE, archive_groupchats);
should_archive_if_groupchat(_, _) ->
    true.

%% Only store messages sent to yourself in user_send_message.
should_archive_if_sent_to_yourself(LocJID, RemJID, incoming) ->
    not jid:are_bare_equal(LocJID, RemJID);
should_archive_if_sent_to_yourself(_LocJID, _RemJID, _Dir) ->
    true.

-spec return_external_message_id_if_ok(ReturnMessID :: boolean(),
                                       ArchivingResult :: ok | any(),
                                       MessID :: integer()) -> binary() | undefined.
return_external_message_id_if_ok(true, ok, MessID) ->
    mod_mam_utils:mess_id_to_external_binary(MessID);
return_external_message_id_if_ok(_, _, _MessID) ->
    undefined.

return_acc_with_mam_id_if_configured(undefined, _, Acc) ->
    Acc;
return_acc_with_mam_id_if_configured(ExtMessId, HostType, Acc) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, same_mam_id_for_peers) of
        false -> mongoose_acc:set(mam, mam_id, ExtMessId, Acc);
        true -> mongoose_acc:set_permanent(mam, mam_id, ExtMessId, Acc)
    end.

is_interesting(LocJID, RemJID) ->
    HostType = jid_to_host_type(LocJID),
    ArcID = archive_id_int(HostType, LocJID),
    is_interesting(HostType, LocJID, RemJID, ArcID).

is_interesting(HostType, LocJID, RemJID, ArcID) ->
    case get_behaviour(HostType, ArcID, LocJID, RemJID) of
        always -> true;
        never  -> false;
        roster -> mod_mam_utils:is_jid_in_user_roster(HostType, LocJID, RemJID)
    end.

%% ----------------------------------------------------------------------
%% Backend wrappers

-spec archive_id_int(host_type(), jid:jid()) ->
                            non_neg_integer() | undefined.
archive_id_int(HostType, ArcJID=#jid{}) ->
    mongoose_hooks:mam_archive_id(HostType, ArcJID).

-spec archive_size(host_type(), mod_mam:archive_id(), jid:jid()) -> integer().
archive_size(HostType, ArcID, ArcJID=#jid{}) ->
    mongoose_hooks:mam_archive_size(HostType, ArcID, ArcJID).

-spec get_behaviour(host_type(), mod_mam:archive_id(), LocJID :: jid:jid(),
                    RemJID :: jid:jid()) -> atom().
get_behaviour(HostType, ArcID,  LocJID=#jid{}, RemJID=#jid{}) ->
  mongoose_hooks:mam_get_behaviour(HostType, ArcID, LocJID, RemJID).

-spec set_prefs(host_type(), mod_mam:archive_id(), ArcJID :: jid:jid(),
                DefaultMode :: atom(), AlwaysJIDs :: [jid:literal_jid()],
                NeverJIDs :: [jid:literal_jid()]) -> any().
set_prefs(HostType, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    mongoose_hooks:mam_set_prefs(HostType, ArcID, ArcJID, DefaultMode,
                                 AlwaysJIDs, NeverJIDs).

%% @doc Load settings from the database.
-spec get_prefs(HostType :: host_type(), ArcID :: mod_mam:archive_id(),
                ArcJID :: jid:jid(), GlobalDefaultMode :: mod_mam:archive_behaviour()
               ) -> mod_mam:preference() | {error, Reason :: term()}.
get_prefs(HostType, ArcID, ArcJID, GlobalDefaultMode) ->
    mongoose_hooks:mam_get_prefs(HostType, GlobalDefaultMode, ArcID, ArcJID).

-spec remove_archive_hook(host_type(), mod_mam:archive_id(), jid:jid()) -> 'ok'.
remove_archive_hook(HostType, ArcID, ArcJID=#jid{}) ->
    mongoose_hooks:mam_remove_archive(HostType, ArcID, ArcJID),
    ok.

-spec lookup_messages(HostType :: host_type(), Params :: map()) ->
    {ok, mod_mam:lookup_result()}
    | {error, 'policy-violation'}
    | {error, Reason :: term()}.
lookup_messages(HostType, Params) ->
    Result = lookup_messages_without_policy_violation_check(HostType, Params),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    mod_mam_utils:check_result_for_policy_violation(Params, Result).

lookup_messages_without_policy_violation_check(
      HostType, #{search_text := SearchText} = Params) ->
    case SearchText /= undefined andalso
         not mod_mam_params:has_full_text_search(?MODULE, HostType) of
        true -> %% Use of disabled full text search
            {error, 'not-supported'};
        false ->
            StartT = erlang:monotonic_time(microsecond),
            R = case maps:get(message_ids, Params, undefined) of
                    undefined ->
                        mongoose_hooks:mam_lookup_messages(HostType,
                            Params#{message_id => undefined});
                    IDs ->
                        mod_mam_utils:lookup_specific_messages(HostType, Params, IDs,
                            fun mongoose_hooks:mam_lookup_messages/2)
                end,
            Diff = erlang:monotonic_time(microsecond) - StartT,
            mongoose_metrics:update(HostType, [backends, ?MODULE, lookup], Diff),
            R
    end.

archive_message_from_ct(Params = #{local_jid := JID}) ->
    HostType = jid_to_host_type(JID),
    archive_message(HostType, Params).

-spec archive_message(host_type(), mod_mam:archive_message_params()) ->
    ok | {error, timeout}.
archive_message(HostType, Params) ->
    StartT = erlang:monotonic_time(microsecond),
    R = mongoose_hooks:mam_archive_message(HostType, Params),
    Diff = erlang:monotonic_time(microsecond) - StartT,
    mongoose_metrics:update(HostType, [backends, ?MODULE, archive], Diff),
    R.

%% ----------------------------------------------------------------------
%% Helpers

-spec message_row_to_xml(binary(), mod_mam:message_row(), QueryId :: binary(), boolean()) ->
    exml:element().
message_row_to_xml(MamNs, #{id := MessID, jid := SrcJID, packet := Packet},
                   QueryID, SetClientNs)  ->
    {Microseconds, _NodeMessID} = mod_mam_utils:decode_compact_uuid(MessID),
    TS = calendar:system_time_to_rfc3339(Microseconds, [{offset, "Z"}, {unit, microsecond}]),
    BExtMessID = mod_mam_utils:mess_id_to_external_binary(MessID),
    Packet1 = mod_mam_utils:maybe_set_client_xmlns(SetClientNs, Packet),
    mod_mam_utils:wrap_message(MamNs, Packet1, QueryID, BExtMessID, TS, SrcJID).

-spec message_row_to_ext_id(mod_mam:message_row()) -> binary().
message_row_to_ext_id(#{id := MessID}) ->
    mod_mam_utils:mess_id_to_external_binary(MessID).

handle_error_iq(HostType, Acc, _To, _Action, {error, _Reason, IQ}) ->
    mongoose_metrics:update(HostType, modMamDroppedIQ, 1),
    {Acc, IQ};
handle_error_iq(_Host, Acc, _To, _Action, IQ) ->
    {Acc, IQ}.

-spec return_action_not_allowed_error_iq(jlib:iq()) -> jlib:iq().
return_action_not_allowed_error_iq(IQ) ->
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"not-allowed">>,
                                 <<"en">>, <<"The action is not allowed.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

-spec return_max_delay_reached_error_iq(jlib:iq()) -> jlib:iq().
return_max_delay_reached_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = mongoose_xmpp_errors:resource_constraint(
                 <<"en">>, <<"The action is cancelled because of flooding.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

-spec return_error_iq(jlib:iq(), Reason :: term()) -> {error, term(), jlib:iq()}.
return_error_iq(IQ, {Reason, {stacktrace, _Stacktrace}}) ->
    return_error_iq(IQ, Reason);
return_error_iq(IQ, timeout) ->
    E = mongoose_xmpp_errors:service_unavailable(<<"en">>, <<"Timeout">>),
    {error, timeout, IQ#iq{type = error, sub_el = [E]}};
return_error_iq(IQ, invalid_stanza_id) ->
    Text = mongoose_xmpp_errors:not_acceptable(<<"en">>, <<"Invalid stanza id provided">>),
    {error, invalid_stanza_id, IQ#iq{type = error, sub_el = [Text]}};
return_error_iq(IQ, item_not_found) ->
    {error, item_not_found, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:item_not_found()]}};
return_error_iq(IQ, not_implemented) ->
    {error, not_implemented, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:feature_not_implemented()]}};
return_error_iq(IQ, Reason) ->
    {error, Reason, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:internal_server_error()]}}.

return_message_form_iq(HostType, IQ) ->
    IQ#iq{type = result, sub_el = [mod_mam_utils:message_form(?MODULE, HostType, IQ#iq.xmlns)]}.

report_issue({Reason, {stacktrace, Stacktrace}}, Issue, ArcJID, IQ) ->
    report_issue(Reason, Stacktrace, Issue, ArcJID, IQ);
report_issue(Reason, Issue, ArcJID, IQ) ->
    report_issue(Reason, [], Issue, ArcJID, IQ).

report_issue(invalid_stanza_id, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(item_not_found, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(not_implemented, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(timeout, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(Reason, Stacktrace, Issue, #jid{lserver=LServer, luser=LUser}, IQ) ->
    ?LOG_ERROR(#{what => mam_error,
                 issue => Issue, server => LServer, user => LUser,
                 reason => Reason, iq => IQ, stacktrace => Stacktrace}).

-spec is_archivable_message(HostType :: host_type(),
                            Dir :: incoming | outgoing,
                            Packet :: exml:element()) -> boolean().
is_archivable_message(HostType, Dir, Packet) ->
    M = mod_mam_params:is_archivable_message_module(?MODULE, HostType),
    ArchiveChatMarkers = mod_mam_params:archive_chat_markers(?MODULE, HostType),
    erlang:apply(M, is_archivable_message, [?MODULE, Dir, Packet, ArchiveChatMarkers]).

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
        {disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 99},
        {disco_sm_features, HostType, fun ?MODULE:disco_sm_features/3, #{}, 99},
        {user_send_message, HostType, fun ?MODULE:user_send_message/3, #{}, 60},
        {filter_local_packet, HostType, fun ?MODULE:filter_packet/3, #{}, 60},
        {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
        {anonymous_purge_hook, HostType, fun ?MODULE:remove_user/3, #{}, 50},
        {amp_determine_strategy, HostType, fun ?MODULE:determine_amp_strategy/3, #{}, 20},
        {sm_filter_offline_message, HostType, fun ?MODULE:sm_filter_offline_message/3, #{}, 50},
        {get_personal_data, HostType, fun ?MODULE:get_personal_data/3, #{}, 50}
        | mongoose_metrics_mam_hooks:get_mam_hooks(HostType)
    ].

add_iq_handlers(HostType, Opts) ->
    Component = ejabberd_sm,
    %% `parallel' is the only one recommended here.
    ExecutionType = gen_mod:get_opt(iqdisc, Opts, parallel),
    IQHandlerFn = fun ?MODULE:process_mam_iq/5,
    Extra = #{},
    [gen_iq_handler:add_iq_handler_for_domain(HostType, Namespace,
                                              Component, IQHandlerFn,
                                              Extra, ExecutionType)
     || Namespace <- [?NS_MAM_04, ?NS_MAM_06]],
    ok.

remove_iq_handlers(HostType) ->
    Component = ejabberd_sm,
    [gen_iq_handler:remove_iq_handler_for_domain(HostType, Namespace, Component)
     || Namespace <- [?NS_MAM_04, ?NS_MAM_06]],
    ok.

ensure_metrics(HostType) ->
    mongoose_metrics:ensure_metric(HostType, [backends, ?MODULE, lookup], histogram),
    mongoose_metrics:ensure_metric(HostType, [modMamLookups, simple], spiral),
    mongoose_metrics:ensure_metric(HostType, [backends, ?MODULE, archive], histogram),
    lists:foreach(fun(Name) ->
                      mongoose_metrics:ensure_metric(HostType, Name, spiral)
                  end,
                  spirals()).

spirals() ->
    [modMamPrefsSets,
     modMamPrefsGets,
     modMamArchiveRemoved,
     modMamLookups,
     modMamForwarded,
     modMamArchived,
     modMamFlushed,
     modMamDropped,
     modMamDroppedIQ].
