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
%%% This module handles MUC archives.
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
-module(mod_mam_muc).
-behaviour(gen_mod).
%% ----------------------------------------------------------------------
%% Exports

%% Client API
-export([delete_archive/2,
         archive_size/2,
         archive_id/2]).

%% gen_mod handlers
-export([start/2, stop/1, supported_features/0, hooks/1, instrumentation/1]).

%% ejabberd room handlers
-export([disco_muc_features/3,
         filter_room_packet/3,
         forget_room/3]).

-export([room_process_mam_iq/5]).

%% gdpr callback
-export([get_personal_data/3]).

%% private
-export([archive_message_for_ct/1]).
-export([lookup_messages/2]).
-export([archive_id_int/2]).

-ignore_xref([archive_id/2, archive_message_for_ct/1, archive_size/2, delete_archive/2]).

-include_lib("mongoose.hrl").
-include_lib("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-callback is_complete_message(Module :: atom(), Dir :: atom(), Packet :: any()) ->
    boolean().

%% ----------------------------------------------------------------------
%% Other types
-type packet() :: any().
-type row_batch() :: {TotalCount :: non_neg_integer(),
                      Offset :: non_neg_integer(),
                      MessageRows :: [row()]}.
-type row() :: mod_mam:message_row().
-type host_type() :: mongooseim:host_type().
-type muc_action() :: atom().

-export_type([row/0, row_batch/0]).

%% ----------------------------------------------------------------------
%% API

-spec get_personal_data(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: gdpr:personal_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
get_personal_data(Acc, #{jid := ArcJID}, #{host_type := HostType}) ->
    Schema = ["id", "message"],
    Entries = mongoose_hooks:get_mam_muc_gdpr_data(HostType, ArcJID),
    {ok, [{mam_muc, Schema, Entries} | Acc]}.

-spec delete_archive(jid:server(), jid:user()) -> ok.
delete_archive(MucHost, RoomName) when is_binary(MucHost), is_binary(RoomName) ->
    ?LOG_DEBUG(#{what => mam_delete_room, room => RoomName, sub_host => MucHost}),
    ArcJID = jid:make_bare(RoomName, MucHost),
    HostType = mod_muc_light_utils:room_jid_to_host_type(ArcJID),
    ArcID = archive_id_int(HostType, ArcJID),
    remove_archive(HostType, ArcID, ArcJID),
    ok.

-spec archive_size(jid:server(), jid:user()) -> integer().
archive_size(MucHost, RoomName) when is_binary(MucHost), is_binary(RoomName) ->
    ArcJID = jid:make_bare(RoomName, MucHost),
    HostType = mod_muc_light_utils:room_jid_to_host_type(ArcJID),
    ArcID = archive_id_int(HostType, ArcJID),
    archive_size(HostType, ArcID, ArcJID).

-spec archive_id(jid:server(), jid:user()) -> integer().
archive_id(MucHost, RoomName) when is_binary(MucHost), is_binary(RoomName) ->
    ArcJID = jid:make_bare(RoomName, MucHost),
    HostType = mod_muc_light_utils:room_jid_to_host_type(ArcJID),
    archive_id_int(HostType, ArcJID).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for MUC archives

-spec start(host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    ?LOG_DEBUG(#{what => mam_muc_starting}),
    add_iq_handlers(HostType, Opts),
    ok.

-spec stop(host_type()) -> any().
stop(HostType) ->
    ?LOG_DEBUG(#{what => mam_muc_stopping}),
    remove_iq_handlers(HostType),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%% ----------------------------------------------------------------------
%% hooks and handlers for MUC

-spec disco_muc_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_muc_features(Acc = #{host_type := HostType, node := <<>>}, _Params, _Extra) ->
    {ok, mongoose_disco:add_features(mod_mam_utils:features(?MODULE, HostType), Acc)};
disco_muc_features(Acc, _Params, _Extra) ->
    {ok, Acc}.

%% @doc Handle public MUC-message.
-spec filter_room_packet(Packet, EventData, Extra) -> {ok, Packet} when
    Packet :: exml:element(),
    EventData :: mod_muc:room_event_data(),
    Extra :: gen_hook:extra().
filter_room_packet(Packet, EventData, #{host_type := HostType}) ->
    ?LOG_DEBUG(#{what => mam_room_packet, text => <<"Incoming room packet">>,
                 packet => Packet, event_data => EventData}),
    IsArchivable = is_archivable_message(HostType, incoming, Packet),
    case IsArchivable of
        true ->
            #{from_nick := FromNick, from_jid := FromJID, room_jid := RoomJID,
              role := Role, affiliation := Affiliation, timestamp := TS} = EventData,
            {ok, archive_room_packet(HostType, Packet, FromNick, FromJID,
                                     RoomJID, Role, Affiliation, TS)};
        false ->
            {ok, Packet}
    end.

%% @doc Archive without validation.
-spec archive_room_packet(HostType :: host_type(),
                          Packet :: packet(), FromNick :: jid:user(),
                          FromJID :: jid:jid(), RoomJID :: jid:jid(),
                          Role :: mod_muc:role(), Affiliation :: mod_muc:affiliation(),
                          TS :: integer()) -> packet().
archive_room_packet(HostType, Packet, FromNick, FromJID = #jid{},
                    RoomJID = #jid{}, Role, Affiliation, TS) ->
    ArcID = archive_id_int(HostType, RoomJID),
    %% Occupant JID <room@service/nick>
    SrcJID = jid:replace_resource(RoomJID, FromNick),
    IsMamMucEnabled = mod_mam_utils:is_mam_muc_enabled(RoomJID#jid.lserver, HostType),
    IsInteresting =
        case get_behaviour(HostType, ArcID, RoomJID, SrcJID) of
            always -> true;
            never -> false;
            roster -> true
        end,
    case IsInteresting andalso IsMamMucEnabled of
        true ->
            MessID = mod_mam_utils:generate_message_id(TS),
            Packet1 = mod_mam_utils:replace_x_user_element(FromJID, Role, Affiliation, Packet),
            OriginID = mod_mam_utils:get_origin_id(Packet),
            Params = #{message_id => MessID,
                       archive_id => ArcID,
                       local_jid => RoomJID,
                       remote_jid => FromJID,
                       source_jid => SrcJID,
                       origin_id => OriginID,
                       direction => incoming,
                       packet => Packet1},
            %% Packet to be broadcasted and packet to be archived are
            %% not 100% the same
            Result = archive_message(HostType, Params),
            case Result of
                ok ->
                    ExtID = mod_mam_utils:mess_id_to_external_binary(MessID),
                    ShouldAdd = mod_mam_params:add_stanzaid_element(?MODULE, HostType),
                    mod_mam_utils:maybe_add_arcid_elems(RoomJID, ExtID, Packet, ShouldAdd);
                {error, _} -> Packet
            end;
        false -> Packet
    end.

%% @doc `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec room_process_mam_iq(Acc :: mongoose_acc:t(),
                          From :: jid:jid(),
                          To :: jid:jid(),
                          IQ :: jlib:iq(),
                          Extra :: gen_hook:extra()) -> {mongoose_acc:t(), jlib:iq() | ignore}.
room_process_mam_iq(Acc, From, To, IQ, #{host_type := HostType}) ->
    mod_mam_utils:maybe_log_deprecation(IQ),
    Action = mam_iq:action(IQ),
    MucAction = action_to_muc_action(Action),
    case check_action_allowed(HostType, Acc, To#jid.lserver, Action, MucAction, From, To) of
        ok ->
            case mod_mam_utils:wait_shaper(HostType, To#jid.lserver, MucAction, From) of
                continue ->
                    handle_error_iq(Acc, HostType, To, Action,
                                    handle_mam_iq(HostType, Action, From, To, IQ));
                {error, max_delay_reached} ->
                    mongoose_instrument:execute(mod_mam_muc_dropped_iq,
                                                #{host_type => HostType}, #{acc => Acc, count => 1}),
                    {Acc, return_max_delay_reached_error_iq(IQ)}
            end;
        {error, Reason} ->
            ?LOG_WARNING(#{what => action_not_allowed,
                           action => Action, acc => Acc, reason => Reason,
                           can_access_room => can_access_room(HostType, Acc, From, To)}),
            {Acc, return_action_not_allowed_error_iq(Reason, IQ)}
    end.

-spec forget_room(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{muc_host := jid:server(), room := jid:luser()},
    Extra :: gen_hook:extra().
forget_room(Acc, #{muc_host := MucServer, room := RoomName}, _Extra) ->
    delete_archive(MucServer, RoomName),
    {ok, Acc}.

%% ----------------------------------------------------------------------
%% Internal functions

-spec check_action_allowed(host_type(), mongoose_acc:t(), jid:lserver(), mam_iq:action(), muc_action(),
                        jid:jid(), jid:jid()) -> ok | {error, binary()}.
check_action_allowed(HostType, Acc, Domain, Action, MucAction, From, To) ->
    case acl:match_rule(HostType, Domain, MucAction, From, default) of
        allow -> ok;
        deny -> {false, <<"Blocked by service policy.">>};
        default -> check_room_action_allowed_by_default(HostType, Acc, Action, From, To)
    end.

-spec action_to_muc_action(mam_iq:action()) -> atom().
action_to_muc_action(Action) ->
    list_to_atom("muc_" ++ atom_to_list(Action)).

-spec check_room_action_allowed_by_default(HostType :: host_type(),
                                           Acc :: mongoose_acc:t(),
                                           Action :: mam_iq:action(),
                                           From :: jid:jid(),
                                           To :: jid:jid()) -> ok | {error, binary()}.
check_room_action_allowed_by_default(HostType, Acc, Action, From, To) ->
    case mam_iq:action_type(Action) of
        set ->
            case is_room_owner(HostType, Acc, From, To) of
                true -> ok;
                false -> {error, <<"Not a room owner.">>}
            end;
        get ->
            case can_access_room(HostType, Acc, From, To) of
                true -> ok;
                false -> {error, <<"Not allowed to enter the room.">>}
            end
    end.

-spec is_room_owner(HostType :: host_type(),
                    Acc :: mongoose_acc:t(),
                    UserJid :: jid:jid(),
                    RoomJid :: jid:jid()) -> boolean().
is_room_owner(HostType, Acc, UserJid, RoomJid) ->
    mongoose_hooks:is_muc_room_owner(HostType, Acc, RoomJid, UserJid).

%% @doc Return true if user element should be removed from results
-spec is_user_identity_hidden(HostType :: host_type(),
                              UserJid :: jid:jid(),
                              RoomJid :: jid:jid()) -> boolean().
is_user_identity_hidden(HostType, UserJid, RoomJid) ->
    case mongoose_hooks:can_access_identity(HostType, RoomJid, UserJid) of
        CanAccess when is_boolean(CanAccess) -> not CanAccess
    end.

-spec can_access_room(HostType :: host_type(),
                      Acc :: mongoose_acc:t(),
                      UserJid :: jid:jid(),
                      RoomJid :: jid:jid()) -> boolean().
can_access_room(HostType, Acc, UserJid, RoomJid) ->
    mongoose_hooks:can_access_room(HostType, Acc, RoomJid, UserJid).

-spec handle_mam_iq(HostType :: host_type(), mam_iq:action(),
                    From :: jid:jid(), jid:jid(), jlib:iq()) ->
                           jlib:iq() | {error, any(), jlib:iq()} | ignore.
handle_mam_iq(HostType, Action, From, To, IQ) ->
    case Action of
        mam_get_prefs ->
            handle_get_prefs(HostType, To, IQ);
        mam_set_prefs ->
            handle_set_prefs(HostType, To, IQ);
        mam_set_message_form ->
            handle_set_message_form(HostType, From, To, IQ);
        mam_get_message_form ->
            handle_get_message_form(HostType, From, To, IQ);
        mam_get_metadata ->
            handle_get_metadata(HostType, From, To, IQ)
    end.

-spec handle_set_prefs(host_type(), jid:jid(), jlib:iq()) ->
                              jlib:iq() | {error, any(), jlib:iq()}.
handle_set_prefs(HostType, ArcJID = #jid{},
                 IQ = #iq{sub_el = PrefsEl}) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} = mod_mam_utils:parse_prefs(PrefsEl),
    ?LOG_DEBUG(#{what => mam_muc_set_prefs, archive_jid => ArcJID,
                 default_mode => DefaultMode,
                 always_jids => AlwaysJIDs, never_jids => NeverJIDs, iq => IQ}),
    ArcID = archive_id_int(HostType, ArcJID),
    Res = set_prefs(HostType, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs),
    handle_set_prefs_result(Res, DefaultMode, AlwaysJIDs, NeverJIDs, IQ).

handle_set_prefs_result(ok, DefaultMode, AlwaysJIDs, NeverJIDs, IQ) ->
    ResultPrefsEl = mod_mam_utils:result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, IQ#iq.xmlns),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_set_prefs_result({error, Reason},
                        _DefaultMode, _AlwaysJIDs, _NeverJIDs, IQ) ->
    return_error_iq(IQ, Reason).

-spec handle_get_prefs(host_type(), jid:jid(), jlib:iq()) ->
                              jlib:iq() | {error, any(), jlib:iq()}.
handle_get_prefs(HostType, ArcJID=#jid{}, IQ=#iq{}) ->
    ArcID = archive_id_int(HostType, ArcJID),
    Res = get_prefs(HostType, ArcID, ArcJID, always),
    handle_get_prefs_result(ArcJID, Res, IQ).

handle_get_prefs_result(ArcJID, {DefaultMode, AlwaysJIDs, NeverJIDs}, IQ) ->
    ?LOG_DEBUG(#{what => mam_muc_get_prefs_result, archive_jid => ArcJID,
                 default_mode => DefaultMode,
                 always_jids => AlwaysJIDs, never_jids => NeverJIDs, iq => IQ}),
    ResultPrefsEl = mod_mam_utils:result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, IQ#iq.xmlns),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_get_prefs_result(_ArcJID, {error, Reason}, IQ) ->
    return_error_iq(IQ, Reason).

-spec handle_set_message_form(HostType :: host_type(),
                              From :: jid:jid(), ArcJID :: jid:jid(),
                              IQ :: jlib:iq()) ->
                                  jlib:iq() | ignore | {error, term(), jlib:iq()}.
handle_set_message_form(HostType, #jid{} = From, #jid{} = ArcJID, IQ) ->
    ArcID = archive_id_int(HostType, ArcJID),
    ResLimit = mod_mam_params:max_result_limit(?MODULE, HostType),
    DefLimit = mod_mam_params:default_result_limit(?MODULE, HostType),
    ExtMod = mod_mam_params:extra_params_module(?MODULE, HostType),
    Sim = mod_mam_params:enforce_simple_queries(?MODULE, HostType),
    try mam_iq:form_to_lookup_params(IQ, ResLimit, DefLimit, ExtMod, Sim) of
        Params0 ->
            do_handle_set_message_form(HostType, From, ArcID, ArcJID, IQ, Params0)
    catch _C:R:S ->
            report_issue({R, S}, mam_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, R)
    end.


-spec do_handle_set_message_form(HostType :: mongooseim:host_type(),
                                 From :: jid:jid(),
                                 ArcId :: mod_mam:archive_id(),
                                 ArcJID :: jid:jid(),
                                 IQ :: jlib:iq(),
                                 Params :: mam_iq:lookup_params()) ->
    jlib:iq() | ignore | {error, term(), jlib:iq()}.
do_handle_set_message_form(HostType, From, ArcID, ArcJID, IQ, Params0) ->
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID, From),
    Result = mod_mam_utils:lookup(HostType, Params, fun lookup_messages/2),
    handle_lookup_result(Result, HostType, From, IQ, Params).

-spec handle_lookup_result({ok, mod_mam:lookup_result()} | {error, term()},
                           host_type(), jid:jid(), jlib:iq(), map()) ->
    jlib:iq() | ignore | {error, term(), jlib:iq()}.
handle_lookup_result(Result, HostType, From, IQ, #{owner_jid := ArcJID} = Params) ->
    case Result of
        {error, Reason} ->
            report_issue(Reason, mam_muc_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, Res} ->
            send_messages_and_iq_result(Res, HostType, From, IQ, Params)
    end.

send_messages_and_iq_result(#{total_count := TotalCount, offset := Offset,
                              messages := MessageRows, is_complete := IsComplete},
                            HostType, From,
                            #iq{xmlns = MamNs, sub_el = QueryEl} = IQ,
                            #{owner_jid := ArcJID} = Params) ->
    %% Reverse order of messages if the client requested it
    MessageRows1 = mod_mam_utils:maybe_reverse_messages(Params, MessageRows),
    %% Forward messages
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    {FirstMessID, LastMessID} = forward_messages(HostType, From, ArcJID, MamNs,
                                                 QueryID, MessageRows1, true),
    %% Make fin iq
    IsStable = true,
    ResultSetEl = mod_mam_utils:result_set(FirstMessID, LastMessID, Offset, TotalCount),
    ExtFinMod = mod_mam_params:extra_fin_element_module(?MODULE, HostType),
    FinElem = mod_mam_utils:make_fin_element(HostType, Params, IQ#iq.xmlns,
                                             IsComplete, IsStable,
                                             ResultSetEl, ExtFinMod),
    IQ#iq{type = result, sub_el = [FinElem]}.

forward_messages(HostType, From, ArcJID, MamNs, QueryID, MessageRows, SetClientNs) ->
    %% Forward messages
    {FirstMessID, LastMessID, HideUser} =
        case MessageRows of
            [] -> {undefined, undefined, undefined};
            [_ | _] -> {message_row_to_ext_id(hd(MessageRows)),
                        message_row_to_ext_id(lists:last(MessageRows)),
                        is_user_identity_hidden(HostType, From, ArcJID)}
        end,
    SendModule = mod_mam_params:send_message_mod(?MODULE, HostType),
    [send_message(SendModule, Row, ArcJID, From,
                  message_row_to_xml(MamNs, From, HideUser, SetClientNs, Row,
                                     QueryID))
     || Row <- MessageRows],
    {FirstMessID, LastMessID}.

send_message(SendModule, Row, ArcJID, From, Packet) ->
    mam_send_message:call_send_message(SendModule, Row, ArcJID, From, Packet).

-spec handle_get_message_form(host_type(), jid:jid(), jid:jid(), jlib:iq()) ->
                                     jlib:iq().
handle_get_message_form(HostType,
                        _From = #jid{}, _ArcJID = #jid{}, IQ = #iq{}) ->
    return_message_form_iq(HostType, IQ).

-spec handle_get_metadata(host_type(), jid:jid(), jid:jid(), jlib:iq()) ->
                                     jlib:iq() | {error, term(), jlib:iq()}.
handle_get_metadata(HostType, #jid{} = From, #jid{} = ArcJID, IQ) ->
    ArcID = archive_id_int(HostType, ArcJID),
    case mod_mam_utils:lookup_first_and_last_messages(HostType, ArcID, From,
                                                      ArcJID, fun lookup_messages/2) of
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

%% ----------------------------------------------------------------------
%% Backend wrappers

-spec archive_id_int(HostType :: host_type(), ArcJID :: jid:jid()) ->
    integer() | undefined.
archive_id_int(HostType, ArcJID = #jid{}) ->
    mongoose_hooks:mam_muc_archive_id(HostType, ArcJID).

-spec archive_size(HostType :: host_type(), ArcID :: mod_mam:archive_id(),
                   ArcJID ::jid:jid()) -> non_neg_integer().
archive_size(HostType, ArcID, ArcJID = #jid{}) ->
    mongoose_hooks:mam_muc_archive_size(HostType, ArcID, ArcJID).

-spec get_behaviour(HostType :: host_type(), ArcID :: mod_mam:archive_id(),
                    LocJID :: jid:jid(), RemJID :: jid:jid()) -> any().
get_behaviour(HostType, ArcID, LocJID = #jid{}, RemJID = #jid{}) ->
    mongoose_hooks:mam_muc_get_behaviour(HostType, ArcID, LocJID, RemJID).

-spec set_prefs(HostType :: host_type(), ArcID :: mod_mam:archive_id(),
                ArcJID :: jid:jid(), DefaultMode :: mod_mam:archive_behaviour(),
                AlwaysJIDs :: [jid:literal_jid()],
                NeverJIDs :: [jid:literal_jid()]) -> any().
set_prefs(HostType, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    Result = mongoose_hooks:mam_muc_set_prefs(HostType, ArcID, ArcJID, DefaultMode,
                                     AlwaysJIDs, NeverJIDs),
    mongoose_instrument:execute(mod_mam_muc_set_prefs, #{host_type => HostType},
                                #{jid => ArcJID, count => 1}),
    Result.

%% @doc Load settings from the database.
-spec get_prefs(HostType :: host_type(), ArcID :: mod_mam:archive_id(),
                ArcJID :: jid:jid(), GlobalDefaultMode :: mod_mam:archive_behaviour())
               -> mod_mam:preference() | {error, Reason :: term()}.
get_prefs(HostType, ArcID, ArcJID, GlobalDefaultMode) ->
    Result = mongoose_hooks:mam_muc_get_prefs(HostType, GlobalDefaultMode, ArcID, ArcJID),
    mongoose_instrument:execute(mod_mam_muc_get_prefs, #{host_type => HostType},
                                #{jid => ArcJID, count => 1}),
    Result.

-spec remove_archive(host_type(), mod_mam:archive_id() | undefined,
                     jid:jid()) -> ok.
remove_archive(HostType, ArcID, ArcJID = #jid{}) ->
    mongoose_hooks:mam_muc_remove_archive(HostType, ArcID, ArcJID),
    mongoose_instrument:execute(mod_mam_muc_remove_archive, #{host_type => HostType},
                                #{jid => ArcJID, count => 1}).

%% See description in mod_mam_pm.
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

lookup_messages_without_policy_violation_check(HostType,
                                               #{search_text := SearchText} = Params) ->
    case SearchText /= undefined andalso
         not mod_mam_params:has_full_text_search(?MODULE, HostType) of
        true -> %% Use of disabled full text search
            {error, 'not-supported'};
        false ->
            mongoose_instrument:span(mod_mam_muc_lookup, #{host_type => HostType},
                                     fun perform_lookup/2, [HostType, Params],
                                     fun(Time, Result) -> measure_lookup(Params, Time, Result) end)
    end.

perform_lookup(HostType, Params) ->
    case maps:get(message_ids, Params, undefined) of
        undefined ->
            mongoose_hooks:mam_muc_lookup_messages(HostType, Params#{message_id => undefined});
        IDs ->
            mod_mam_utils:lookup_specific_messages(HostType, Params, IDs,
                                                   fun mongoose_hooks:mam_muc_lookup_messages/2)
    end.

measure_lookup(Params, Time, {ok, {_TotalCount, _Offset, MessageRows}}) ->
    #{params => Params, count => 1, time => Time, size => length(MessageRows)};
measure_lookup(_Params, _Time, _OtherResult) ->
    #{}.

archive_message_for_ct(Params = #{local_jid := RoomJid}) ->
    HostType = mod_muc_light_utils:room_jid_to_host_type(RoomJid),
    archive_message(HostType, Params).

-spec archive_message(host_type(), mod_mam:archive_message_params()) -> ok | {error, timeout}.
archive_message(HostType, Params) ->
    mongoose_instrument:span(mod_mam_muc_archive_message, #{host_type => HostType},
                             fun mongoose_hooks:mam_muc_archive_message/2, [HostType, Params],
                             fun(Time, _Result) -> #{params => Params, time => Time, count => 1} end).

%% ----------------------------------------------------------------------
%% Helpers

-spec message_row_to_xml(binary(), jid:jid(), boolean(), boolean(), row(), binary() | undefined) ->
                                exml:element().
message_row_to_xml(MamNs, ReceiverJID, HideUser, SetClientNs,
                   #{id := MessID, jid := SrcJID, packet := Packet}, QueryID) ->
    {Microseconds, _NodeMessID} = mod_mam_utils:decode_compact_uuid(MessID),
    TS = calendar:system_time_to_rfc3339(Microseconds, [{offset, "Z"}, {unit, microsecond}]),
    BExtMessID = mod_mam_utils:mess_id_to_external_binary(MessID),
    Packet1 = maybe_delete_x_user_element(HideUser, ReceiverJID, Packet),
    Packet2 = mod_mam_utils:maybe_set_client_xmlns(SetClientNs, Packet1),
    Packet3 = replace_from_to_attributes(SrcJID, Packet2),
    mod_mam_utils:wrap_message(MamNs, Packet3, QueryID, BExtMessID, TS, SrcJID).

maybe_delete_x_user_element(true, ReceiverJID, Packet) ->
    PacketJID = mod_mam_utils:packet_to_x_user_jid(Packet),
    case jid:are_bare_equal(ReceiverJID, PacketJID) of
        false ->
            mod_mam_utils:delete_x_user_element(Packet);
        true -> %% expose identity for user's own messages
            Packet
    end;
maybe_delete_x_user_element(false, _ReceiverJID, Packet) ->
    Packet.

%% From XEP-0313:
%% When sending out the archives to a requesting client,
%% the forwarded stanza MUST NOT have a 'to' attribute, and
%% the 'from' MUST be the occupant JID of the sender of the archived message.
-spec replace_from_to_attributes(jid:jid(), exml:element()) -> exml:element().
replace_from_to_attributes(SrcJID, Packet) ->
    jlib:replace_from_to(SrcJID, undefined, Packet).

-spec message_row_to_ext_id(row()) -> binary().
message_row_to_ext_id(#{id := MessID}) ->
    mod_mam_utils:mess_id_to_external_binary(MessID).

-spec handle_error_iq(mongoose_acc:t(), host_type(), jid:jid(), atom(),
    {error, term(), jlib:iq()} | jlib:iq() | ignore) -> {mongoose_acc:t(), jlib:iq() | ignore}.
handle_error_iq(Acc, HostType, _To, _Action, {error, _Reason, IQ}) ->
    mongoose_instrument:execute(mod_mam_muc_dropped_iq, #{host_type => HostType},
                                #{acc => Acc, count => 1}),
    {Acc, IQ};
handle_error_iq(Acc, _HostType, _To, _Action, IQ) ->
    {Acc, IQ}.

return_error_iq(IQ, {Reason, {stacktrace, _Stacktrace}}) ->
    return_error_iq(IQ, Reason);
return_error_iq(IQ, timeout) ->
    {error, timeout, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:service_unavailable(<<"en">>, <<"Timeout in mod_mam_muc">>)]}};
return_error_iq(IQ, invalid_stanza_id) ->
    Text = mongoose_xmpp_errors:not_acceptable(<<"en">>, <<"Invalid stanza ID provided">>),
    {error, invalid_stanza_id, IQ#iq{type = error, sub_el = [Text]}};
return_error_iq(IQ, item_not_found) ->
    Text = mongoose_xmpp_errors:item_not_found(<<"en">>, <<"Message with specified ID is not found">>),
    {error, item_not_found, IQ#iq{type = error, sub_el = [Text]}};
return_error_iq(IQ, not_implemented) ->
    {error, not_implemented, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:feature_not_implemented(<<"en">>, <<"From mod_mam_muc">>)]}};
return_error_iq(IQ, missing_with_jid) ->
    Error =  mongoose_xmpp_errors:bad_request(<<"en">>,
                               <<"Limited set of queries allowed in the conversation mode.",
                                 "Missing with_jid filter">>),
    {error, bad_request, IQ#iq{type = error, sub_el = [Error]}};
return_error_iq(IQ, Reason) ->
    {error, Reason, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:internal_server_error()]}}.

-spec return_action_not_allowed_error_iq(Reason :: binary(), jlib:iq()) -> jlib:iq().
return_action_not_allowed_error_iq(Reason, IQ) ->
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"not-allowed">>,
                                 <<"en">>, <<"The action is not allowed. ", Reason/binary>>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

-spec return_max_delay_reached_error_iq(jlib:iq()) -> jlib:iq().
return_max_delay_reached_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = mongoose_xmpp_errors:resource_constraint(
                 <<"en">>, <<"The action is cancelled because of flooding.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

return_message_form_iq(HostType, IQ) ->
    Form = mod_mam_utils:message_form(?MODULE, HostType, IQ#iq.xmlns),
    IQ#iq{type = result, sub_el = [Form]}.

% the stacktrace is a big lie
report_issue({Reason, {stacktrace, Stacktrace}}, Issue, ArcJID, IQ) ->
    report_issue(Reason, Stacktrace, Issue, ArcJID, IQ);
report_issue(Reason, Issue, ArcJID, IQ) ->
    report_issue(Reason, [], Issue, ArcJID, IQ).

report_issue(invalid_stanza_id, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(item_not_found, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(missing_with_jid, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(not_implemented, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(timeout, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(Reason, Stacktrace, Issue, #jid{lserver = LServer, luser = LUser}, IQ) ->
    ?LOG_ERROR(#{what => mam_muc_error, issue => Issue, reason => Reason,
                 user => LUser, server => LServer, iq => IQ, stacktrace => Stacktrace}).

-spec is_archivable_message(HostType :: host_type(),
                            Dir :: incoming | outgoing,
                            Packet :: exml:element()) -> boolean().
is_archivable_message(HostType, Dir, Packet) ->
    M = mod_mam_params:is_archivable_message_module(?MODULE, HostType),
    ArchiveChatMarkers = mod_mam_params:archive_chat_markers(?MODULE, HostType),
    erlang:apply(M, is_archivable_message, [?MODULE, Dir, Packet, ArchiveChatMarkers]).

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{disco_muc_features, HostType, fun ?MODULE:disco_muc_features/3, #{}, 99},
     {filter_room_packet, HostType, fun ?MODULE:filter_room_packet/3, #{}, 60},
     {forget_room, HostType, fun ?MODULE:forget_room/3, #{}, 90},
     {get_personal_data, HostType, fun ?MODULE:get_personal_data/3, #{}, 50}].

add_iq_handlers(HostType, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel),
    MUCSubdomainPattern = gen_mod:get_module_opt(HostType, ?MODULE, host),

    gen_iq_handler:add_iq_handler_for_subdomain(HostType, MUCSubdomainPattern,
                                                ?NS_MAM_04, mod_muc_iq,
                                                fun ?MODULE:room_process_mam_iq/5,
                                                #{}, IQDisc),
    gen_iq_handler:add_iq_handler_for_subdomain(HostType, MUCSubdomainPattern,
                                                ?NS_MAM_06, mod_muc_iq,
                                                fun ?MODULE:room_process_mam_iq/5,
                                                #{}, IQDisc),
    ok.

remove_iq_handlers(HostType) ->
    MUCSubdomainPattern = gen_mod:get_module_opt(HostType, ?MODULE, host),
    gen_iq_handler:remove_iq_handler_for_subdomain(HostType, MUCSubdomainPattern,
                                                   ?NS_MAM_04, mod_muc_iq),
    gen_iq_handler:remove_iq_handler_for_subdomain(HostType, MUCSubdomainPattern,
                                                   ?NS_MAM_06, mod_muc_iq),
    ok.

-spec instrumentation(host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_mam_muc_archive_message, #{host_type => HostType},
      #{metrics => #{count => spiral, time => histogram}}},
     {mod_mam_muc_lookup, #{host_type => HostType},
      #{metrics => #{count => spiral, size => histogram, time => histogram}}},
     {mod_mam_muc_dropped_iq, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_mam_muc_dropped, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_mam_muc_remove_archive, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_mam_muc_get_prefs, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_mam_muc_set_prefs, #{host_type => HostType},
      #{metrics => #{count => spiral}}}].
