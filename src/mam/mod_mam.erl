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
%%% Preferencies can be also stored in Mnesia ({@link mod_mam_mnesia_prefs}).
%%% This module handles simple archives.
%%%
%%% This module should be started for each host.
%%% Message archivation is not shaped here (use standard support for this).
%%% MAM's IQs are shaped inside {@link shaper_srv}.
%%%
%%% Message identifiers (or UIDs in the spec) are generated based on:
%%%
%%% <ul>
%%% <li>date (using `timestamp()');</li>
%%% <li>node number (using {@link ejabberd_node_id}).</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam).
-behavior(gen_mod).
-behaviour(mongoose_module_metrics).
-xep([{xep, 313}, {version, "0.4.1"}]).
-xep([{xep, 313}, {version, "0.5"}]).
-xep([{xep, 313}, {version, "0.6"}]).
%% ----------------------------------------------------------------------
%% Exports

%% Client API
-export([delete_archive/2,
         archive_size/2,
         archive_id/2]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([process_mam_iq/4,
         user_send_packet/4,
         remove_user/3,
         filter_packet/1,
         determine_amp_strategy/5,
         sm_filter_offline_message/4]).

%% gdpr callbacks
-export([get_personal_data/2]).

%%private
-export([archive_message/2]).
-export([lookup_messages/2]).
-export([archive_id_int/2]).

-export([config_metrics/1]).

%% ----------------------------------------------------------------------
%% Imports

%% UID
-import(mod_mam_utils,
        [generate_message_id/0,
         decode_compact_uuid/1]).

%% XML
-import(mod_mam_utils,
        [maybe_add_arcid_elems/4,
         wrap_message/6,
         result_set/4,
         result_query/2,
         result_prefs/4,
         make_fin_message/5,
         make_fin_element/4,
         parse_prefs/1,
         borders_decode/1,
         is_mam_result_message/1,
         features/2]).

%% Forms
-import(mod_mam_utils,
        [message_form/3]).

%% Other
-import(mod_mam_utils,
        [mess_id_to_external_binary/1,
         is_complete_result_page/4]).

%% ejabberd
-import(mod_mam_utils,
        [send_message/3,
         is_jid_in_user_roster/2]).


-include("mongoose.hrl").
-include("jlib.hrl").
-include("amp.hrl").
-include_lib("exml/include/exml.hrl").
-include("mod_mam.hrl").

%% ----------------------------------------------------------------------
%% Datetime types
%% Microseconds from 01.01.1970
-type unix_timestamp() :: non_neg_integer().

%% ----------------------------------------------------------------------
%% Other types
-type archive_behaviour()   :: roster | always | never.
-type message_id()          :: non_neg_integer().

-type archive_id()          :: non_neg_integer().

-type borders()             :: #mam_borders{}.

-type message_row() :: {message_id(), jid:jid(), exml:element()}.
-type lookup_result() :: {TotalCount :: non_neg_integer() | undefined,
                          Offset :: non_neg_integer() | undefined,
                          MessageRows :: [message_row()]}.

%% Internal types
-type iterator_fun() :: fun(() -> {'ok', {_, _}}).
-type rewriter_fun() :: fun((JID :: jid:literal_jid())
                            -> jid:literal_jid()).
-type restore_option() :: {rewrite_jids, rewriter_fun() | [{binary(), binary()}]}
                        | new_message_ids.

-type preference() :: {DefaultMode :: archive_behaviour(),
                       AlwaysJIDs  :: [jid:literal_jid()],
                       NeverJIDs   :: [jid:literal_jid()]}.

-type archive_message_params() :: #{message_id := mod_mam:message_id(),
                                    archive_id := mod_mam:archive_id(),
                                    local_jid := jid:jid(),
                                    remote_jid := jid:jid(),
                                    source_jid := jid:jid(),
                                    origin_id := binary() | none,
                                    direction := atom(),
                                    packet := exml:element(),
                                    %% Only in mod_mam_muc_rdbms_arch:retract_message/2
                                    sender_id => mod_mam:archive_id()}.

-export_type([rewriter_fun/0,
              borders/0,
              preference/0,
              archive_behaviour/0,
              iterator_fun/0,
              unix_timestamp/0,
              archive_id/0,
              lookup_result/0,
              message_row/0,
              message_id/0,
              restore_option/0,
              archive_message_params/0
             ]).

%% ----------------------------------------------------------------------
%% API

-spec get_personal_data(gdpr:personal_data(), jid:jid()) -> gdpr:personal_data().
get_personal_data(Acc, #jid{ lserver = LServer } = JID) ->
    Schema = ["id", "from", "message"],
    Entries = mongoose_hooks:get_mam_pm_gdpr_data(LServer, [], JID),
    [{mam_pm, Schema, Entries} | Acc].

-spec delete_archive(jid:server(), jid:user()) -> 'ok'.
delete_archive(Server, User)
  when is_binary(Server), is_binary(User) ->
    ?LOG_DEBUG(#{what => mam_delete_archive, user => User, server => Server}),
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    remove_archive_hook(Host, ArcID, ArcJID),
    ok.


-spec archive_size(jid:server(), jid:user()) -> integer().
archive_size(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    archive_size(Host, ArcID, ArcJID).


-spec archive_id(jid:server(), jid:user()) -> integer() | undefined.
archive_id(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    archive_id_int(Host, ArcJID).

%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?LOG_INFO(#{what => mam_starting}),

    %% `parallel' is the only one recommended here.
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    [mod_disco:register_feature(Host, Feature) || Feature <- features(?MODULE, Host)],
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_04,
                                  ?MODULE, process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_06,
                                  ?MODULE, process_mam_iq, IQDisc),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(amp_determine_strategy, Host, ?MODULE, determine_amp_strategy, 20),
    ejabberd_hooks:add(sm_filter_offline_message, Host, ?MODULE, sm_filter_offline_message, 50),
    ejabberd_hooks:add(get_personal_data, Host, ?MODULE, get_personal_data, 50),
    mongoose_metrics:ensure_metric(Host, [backends, ?MODULE, lookup], histogram),
    mongoose_metrics:ensure_metric(Host, [Host, modMamLookups, simple], spiral),
    mongoose_metrics:ensure_metric(Host, [backends, ?MODULE, archive], histogram),
    ok.


-spec stop(Host :: jid:server()) -> any().
stop(Host) ->
    ?LOG_INFO(#{what => mam_stopping}),
    ejabberd_hooks:delete(sm_filter_offline_message, Host, ?MODULE, sm_filter_offline_message, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(amp_determine_strategy, Host, ?MODULE, determine_amp_strategy, 20),
    ejabberd_hooks:delete(get_personal_data, Host, ?MODULE, get_personal_data, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_04),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_06),
    [mod_disco:unregister_feature(Host, Feature) || Feature <- features(?MODULE, Host)],
    ok.

%% ----------------------------------------------------------------------
%% hooks and handlers

%% `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec process_mam_iq(From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
                     IQ :: jlib:iq()) -> {mongoose_acc:t(), jlib:iq() | ignore}.
process_mam_iq(From=#jid{lserver=Host}, To, Acc, IQ) ->
    mod_mam_utils:maybe_log_deprecation(IQ),
    Action = mam_iq:action(IQ),
    case is_action_allowed(Action, From, To) of
        true  ->
            case mod_mam_utils:wait_shaper(Host, Action, From) of
                ok ->
                    handle_error_iq(Host, Acc, To, Action,
                                    handle_mam_iq(Action, From, To, IQ));
                {error, max_delay_reached} ->
                    ?LOG_WARNING(#{what => mam_max_delay_reached,
                                   text => <<"Return max_delay_reached error IQ from MAM">>,
                                   action => Action, acc => Acc}),
                    mongoose_metrics:update(Host, modMamDroppedIQ, 1),
                    {Acc, return_max_delay_reached_error_iq(IQ)}
            end;
        false ->
            mongoose_metrics:update(Host, modMamDroppedIQ, 1),
            {Acc, return_action_not_allowed_error_iq(IQ)}
    end.


%% @doc Handle an outgoing message.
%%
%% Note: for outgoing messages, the server MUST use the value of the 'to'
%%       attribute as the target JID.
-spec user_send_packet(Acc :: mongoose_acc:t(), From :: jid:jid(),
                       To :: jid:jid(),
                       Packet :: exml:element()) -> mongoose_acc:t().
user_send_packet(Acc, From, To, Packet) ->
    ?LOG_DEBUG(#{what => mam_user_send_packet, acc => Acc}),
    handle_package(outgoing, false, From, To, From, Packet),
    Acc.


%% @doc Handle an incoming message.
%%
%% Note: For incoming messages, the server MUST use the value of the
%%       'from' attribute as the target JID.
%%
%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
-type fpacket() :: {From :: jid:jid(),
                    To :: jid:jid(),
                    Acc :: mongoose_acc:t(),
                    Packet :: exml:element()}.
-spec filter_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_packet(drop) ->
    drop;
filter_packet({From, To = #jid{lserver = LServer}, Acc, Packet}) ->
    ?LOG_DEBUG(#{what => mam_user_receive_packet, acc => Acc}),
    {AmpEvent, PacketAfterArchive} =
        case ejabberd_users:does_user_exist(To) of
            false ->
                {mam_failed, Packet};
            true ->
                case process_incoming_packet(From, To, Packet) of
                    undefined -> {mam_failed, Packet};
                    MessID -> {archived, maybe_add_arcid_elems(
                                           To, MessID, Packet,
                                           mod_mam_params:add_stanzaid_element(?MODULE, LServer))}
                end
        end,
    Acc1 = mongoose_acc:update_stanza(#{ element => PacketAfterArchive,
                                         from_jid => From,
                                         to_jid => To }, Acc),
    Acc2 = mod_amp:check_packet(Acc1, AmpEvent),
    {From, To, Acc2, mongoose_acc:element(Acc2)}.

process_incoming_packet(From, To, Packet) ->
    handle_package(incoming, true, To, From, From, Packet).

%% hook handler
-spec remove_user(mongoose_acc:t(), jid:user(), jid:server()) -> mongoose_acc:t().
remove_user(Acc, User, Server) ->
    delete_archive(Server, User),
    Acc.

sm_filter_offline_message(_Drop=false, _From, _To, Packet) ->
    %% If ...
    is_mam_result_message(Packet);
    %% ... than drop the message
sm_filter_offline_message(Other, _From, _To, _Packet) ->
    Other.

%% ----------------------------------------------------------------------
%% Internal functions

-spec server_host(jid:jid()) -> jid:lserver().
server_host(#jid{lserver=LServer}) ->
    LServer.

-spec is_action_allowed(Action :: mam_iq:action(), From :: jid:jid(),
                        To :: jid:jid()) -> boolean().
is_action_allowed(Action, From, To=#jid{lserver=Host}) ->
    case acl:match_rule(Host, Action, From, default) of
        allow   -> true;
        deny    -> false;
        default -> is_action_allowed_by_default(Action, From, To)
    end.

-spec is_action_allowed_by_default(Action :: mam_iq:action(), From :: jid:jid(),
                                   To :: jid:jid()) -> boolean().
is_action_allowed_by_default(_Action, From, To) ->
    compare_bare_jids(From, To).


-spec compare_bare_jids(jid:simple_jid() | jid:jid(),
                        jid:simple_jid() | jid:jid()) -> boolean().
compare_bare_jids(JID1, JID2) ->
    jid:to_bare(JID1) =:= jid:to_bare(JID2).

-spec handle_mam_iq(mam_iq:action(), From :: jid:jid(), To :: jid:jid(),
                    IQ :: jlib:iq()) -> jlib:iq() | {error, term(), jlib:iq()}.
handle_mam_iq(Action, From, To, IQ) ->
    case Action of
        mam_get_prefs ->
            handle_get_prefs(To, IQ);
        mam_set_prefs ->
            handle_set_prefs(To, IQ);
        mam_set_message_form ->
            handle_set_message_form(From, To, IQ);
        mam_get_message_form ->
            handle_get_message_form(From, To, IQ)
    end.

-spec handle_set_prefs(jid:jid(), jlib:iq()) ->
                              jlib:iq() | {error, term(), jlib:iq()}.
handle_set_prefs(ArcJID=#jid{},
                 IQ=#iq{sub_el = PrefsEl}) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} = parse_prefs(PrefsEl),
    ?LOG_DEBUG(#{what => mam_set_prefs, default_mode => DefaultMode,
                 always_jids => AlwaysJIDs, never_jids => NeverJIDs, iq => IQ}),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    Res = set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs),
    handle_set_prefs_result(Res, DefaultMode, AlwaysJIDs, NeverJIDs, IQ).

handle_set_prefs_result(ok, DefaultMode, AlwaysJIDs, NeverJIDs, IQ) ->
    Namespace = IQ#iq.xmlns,
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_set_prefs_result({error, Reason},
                        _DefaultMode, _AlwaysJIDs, _NeverJIDs, IQ) ->
    return_error_iq(IQ, Reason).


-spec handle_get_prefs(jid:jid(), IQ :: jlib:iq()) ->
                              jlib:iq() | {error, term(), jlib:iq()}.
handle_get_prefs(ArcJID=#jid{}, IQ=#iq{}) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    Res = get_prefs(Host, ArcID, ArcJID, always),
    handle_get_prefs_result(Res, IQ).

handle_get_prefs_result({DefaultMode, AlwaysJIDs, NeverJIDs}, IQ) ->
    ?LOG_DEBUG(#{what => mam_get_prefs_result, default_mode => DefaultMode,
                 always_jids => AlwaysJIDs, never_jids => NeverJIDs, iq => IQ}),
    Namespace = IQ#iq.xmlns,
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_get_prefs_result({error, Reason}, IQ) ->
    return_error_iq(IQ, Reason).

-spec handle_set_message_form(From :: jid:jid(), ArcJID :: jid:jid(),
                              IQ :: jlib:iq()) ->
    jlib:iq() | ignore | {error, term(), jlib:iq()}.
handle_set_message_form(#jid{} = From, #jid{} = ArcJID,
                        #iq{xmlns=MamNs, sub_el = QueryEl} = IQ) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    Params0 = mam_iq:form_to_lookup_params(IQ, mod_mam_params:max_result_limit(?MODULE, Host),
                                           mod_mam_params:default_result_limit(?MODULE, Host),
                                           mod_mam_params:extra_params_module(?MODULE, Host)),
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID),
    case lookup_messages(Host, Params) of
        {error, Reason} ->
            report_issue(Reason, mam_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, {TotalCount, Offset, MessageRows}} ->
            %% Forward messages
            {FirstMessID, LastMessID} = forward_messages(From, ArcJID, MamNs,
                                                         QueryID, MessageRows, true),
            %% Make fin iq
            IsComplete = is_complete_result_page(TotalCount, Offset, MessageRows, Params),
            IsStable = true,
            ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
            FinElem = make_fin_element(IQ#iq.xmlns, IsComplete, IsStable, ResultSetEl),
            IQ#iq{type = result, sub_el = [FinElem]}
    end.

forward_messages(From, ArcJID, MamNs, QueryID, MessageRows, SetClientNs) ->
    %% Forward messages
    {FirstMessID, LastMessID} =
    case MessageRows of
        [] -> {undefined, undefined};
        [_|_] -> {message_row_to_ext_id(hd(MessageRows)),
                  message_row_to_ext_id(lists:last(MessageRows))}
    end,

    [send_message(ArcJID, From,
                  message_row_to_xml(MamNs, M, QueryID, SetClientNs))
     || M <- MessageRows],
    {FirstMessID, LastMessID}.

-spec handle_get_message_form(jid:jid(), jid:jid(), jlib:iq()) ->
                                     jlib:iq().
handle_get_message_form(_From=#jid{lserver = Host}, _ArcJID=#jid{}, IQ=#iq{}) ->
    return_message_form_iq(Host, IQ).


determine_amp_strategy(Strategy = #amp_strategy{deliver = Deliver},
                       FromJID, ToJID, Packet, initial_check) ->
    #jid{lserver = LServer} = ToJID,
    ShouldBeStored = is_archivable_message(LServer, incoming, Packet)
        andalso is_interesting(ToJID, FromJID)
        andalso ejabberd_auth:does_user_exist(ToJID),
    case ShouldBeStored of
        true -> Strategy#amp_strategy{deliver = amp_deliver_strategy(Deliver)};
        false -> Strategy
    end;
determine_amp_strategy(Strategy, _, _, _, _) ->
    Strategy.

amp_deliver_strategy([none]) -> [stored, none];
amp_deliver_strategy([direct, none]) -> [direct, stored, none].

-spec handle_package(Dir :: incoming | outgoing, ReturnMessID :: boolean(),
                     LocJID :: jid:jid(), RemJID :: jid:jid(), SrcJID :: jid:jid(),
                     Packet :: exml:element()) -> MaybeMessID :: binary() | undefined.
handle_package(Dir, ReturnMessID,
               LocJID = #jid{},
               RemJID = #jid{},
               SrcJID = #jid{}, Packet) ->
    Host = server_host(LocJID),
    case is_archivable_message(Host, Dir, Packet)
         andalso should_archive_if_groupchat(Host, exml_query:attr(Packet, <<"type">>)) of
        true ->
            ArcID = archive_id_int(Host, LocJID),
            OriginID = mod_mam_utils:get_origin_id(Packet),
            case is_interesting(Host, LocJID, RemJID, ArcID) of
                true ->
                    MessID = generate_message_id(),
                    Params = #{message_id => MessID,
                               archive_id => ArcID,
                               local_jid => LocJID,
                               remote_jid => RemJID,
                               source_jid => SrcJID,
                               origin_id => OriginID,
                               direction => Dir,
                               packet => Packet},
                    Result = archive_message(Host, Params),
                    return_external_message_id_if_ok(ReturnMessID, Result, MessID);
                false ->
                    undefined
            end;
        false ->
            undefined
    end.

should_archive_if_groupchat(Host, <<"groupchat">>) ->
    gen_mod:get_module_opt(Host, ?MODULE, archive_groupchats, true);
should_archive_if_groupchat(_, _) ->
    true.

-spec return_external_message_id_if_ok(ReturnMessID :: boolean(),
                                       ArchivingResult :: ok | any(),
                                       MessID :: integer()) -> binary() | undefined.
return_external_message_id_if_ok(true, ok, MessID) -> mess_id_to_external_binary(MessID);
return_external_message_id_if_ok(_, _, _MessID) -> undefined.

is_interesting(LocJID, RemJID) ->
    Host = server_host(LocJID),
    ArcID = archive_id_int(Host, LocJID),
    is_interesting(Host, LocJID, RemJID, ArcID).

is_interesting(Host, LocJID, RemJID, ArcID) ->
    case get_behaviour(Host, ArcID, LocJID, RemJID, always) of
        always -> true;
        never  -> false;
        roster -> is_jid_in_user_roster(LocJID, RemJID)
    end.

%% ----------------------------------------------------------------------
%% Backend wrappers

-spec archive_id_int(jid:server(), jid:jid()) ->
                            non_neg_integer() | undefined.
archive_id_int(Host, ArcJID=#jid{}) ->
    mongoose_hooks:mam_archive_id(Host, undefined, ArcJID).


-spec archive_size(jid:server(), archive_id(), jid:jid()) -> integer().
archive_size(Host, ArcID, ArcJID=#jid{}) ->
    mongoose_hooks:mam_archive_size(Host, 0, ArcID, ArcJID).


-spec get_behaviour(jid:server(), archive_id(), LocJID :: jid:jid(),
                    RemJID :: jid:jid(), Default :: 'always') -> atom().
get_behaviour(Host, ArcID,
              LocJID=#jid{},
              RemJID=#jid{}, DefaultBehaviour) ->
  mongoose_hooks:mam_get_behaviour(Host, DefaultBehaviour, ArcID, LocJID, RemJID).


-spec set_prefs(jid:server(), archive_id(), ArcJID :: jid:jid(),
                DefaultMode :: atom(), AlwaysJIDs :: [jid:literal_jid()],
                NeverJIDs :: [jid:literal_jid()]) -> any().
set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    mongoose_hooks:mam_set_prefs(Host, {error, not_implemented},
                                 ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs).


%% @doc Load settings from the database.
-spec get_prefs(Host :: jid:server(), ArcID :: archive_id(),
                ArcJID :: jid:jid(), GlobalDefaultMode :: archive_behaviour()
               ) -> preference() | {error, Reason :: term()}.
get_prefs(Host, ArcID, ArcJID, GlobalDefaultMode) ->
    mongoose_hooks:mam_get_prefs(Host, {GlobalDefaultMode, [], []}, ArcID, ArcJID).

-spec remove_archive_hook(jid:server(), archive_id(), jid:jid()) -> 'ok'.
remove_archive_hook(Host, ArcID, ArcJID=#jid{}) ->
    mongoose_hooks:mam_remove_archive(Host, ok, ArcID, ArcJID),
    ok.

-spec lookup_messages(Host :: jid:server(),
                      Params :: map()) ->
    {ok, mod_mam:lookup_result()}
    | {error, 'policy-violation'}
    | {error, Reason :: term()}.
lookup_messages(Host, Params) ->
    Result = lookup_messages_without_policy_violation_check(Host, Params),
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    mod_mam_utils:check_result_for_policy_violation(Params, Result).

lookup_messages_without_policy_violation_check(Host, #{search_text := SearchText} = Params) ->
    case SearchText /= undefined andalso not mod_mam_params:has_full_text_search(?MODULE, Host) of
        true -> %% Use of disabled full text search
            {error, 'not-supported'};
        false ->
            StartT = erlang:monotonic_time(microsecond),
            R = mongoose_hooks:mam_lookup_messages(Host, {ok, {0, 0, []}}, Params),
            Diff = erlang:monotonic_time(microsecond) - StartT,
            mongoose_metrics:update(Host, [backends, ?MODULE, lookup], Diff),
            R
    end.

-spec archive_message(jid:server(), mod_mam:archive_message_params()) -> ok | {error, timeout}.
archive_message(Host, Params) ->
    StartT = erlang:monotonic_time(microsecond),
    R = mongoose_hooks:mam_archive_message(Host, ok, Params),
    Diff = erlang:monotonic_time(microsecond) - StartT,
    mongoose_metrics:update(Host, [backends, ?MODULE, archive], Diff),
    R.

%% ----------------------------------------------------------------------
%% Helpers

-type messid_jid_packet() :: {MessId :: integer(),
                              SrcJID :: jid:jid(),
                              Packet :: exml:element()}.
-spec message_row_to_xml(binary(), messid_jid_packet(), QueryId :: binary(), boolean()) ->
    exml:element().
message_row_to_xml(MamNs, {MessID, SrcJID, Packet}, QueryID, SetClientNs)  ->
    {Microseconds, _NodeMessID} = decode_compact_uuid(MessID),
    TS = calendar:system_time_to_rfc3339(erlang:convert_time_unit(Microseconds, microsecond, second), [{offset, "Z"}]),
    BExtMessID = mess_id_to_external_binary(MessID),
    Packet1 = mod_mam_utils:maybe_set_client_xmlns(SetClientNs, Packet),
    wrap_message(MamNs, Packet1, QueryID, BExtMessID, TS, SrcJID).

-spec message_row_to_ext_id(messid_jid_packet()) -> binary().
message_row_to_ext_id({MessID, _, _}) ->
    mess_id_to_external_binary(MessID).

handle_error_iq(Host, Acc, _To, _Action, {error, _Reason, IQ}) ->
    mongoose_metrics:update(Host, modMamDroppedIQ, 1),
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
    {error, timeout, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:service_unavailable()]}};
return_error_iq(IQ, item_not_found) ->
    {error, item_not_found, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:item_not_found()]}};
return_error_iq(IQ, not_implemented) ->
    {error, not_implemented, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:feature_not_implemented()]}};
return_error_iq(IQ, Reason) ->
    {error, Reason, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:internal_server_error()]}}.

return_message_form_iq(Host, IQ) ->
    IQ#iq{type = result, sub_el = [message_form(?MODULE, Host, IQ#iq.xmlns)]}.

report_issue({Reason, {stacktrace, Stacktrace}}, Issue, ArcJID, IQ) ->
    report_issue(Reason, Stacktrace, Issue, ArcJID, IQ);
report_issue(Reason, Issue, ArcJID, IQ) ->
    report_issue(Reason, [], Issue, ArcJID, IQ).

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

-spec is_archivable_message(Host :: jid:lserver(), Dir :: incoming | outgoing,
                            Packet :: exml:element()) -> boolean().
is_archivable_message(Host, Dir, Packet) ->
    {M, F} = mod_mam_params:is_archivable_message_fun(?MODULE, Host),
    ArchiveChatMarkers = mod_mam_params:archive_chat_markers(?MODULE, Host),
    erlang:apply(M, F, [?MODULE, Dir, Packet, ArchiveChatMarkers]).

config_metrics(Host) ->
    OptsToReport = [{backend, rdbms}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
