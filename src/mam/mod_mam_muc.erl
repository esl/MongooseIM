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
%%% This module handles MUC archives.
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
-module(mod_mam_muc).
-xep([{xep, 313}, {version, "0.4.1"}]).
-xep([{xep, 313}, {version, "0.5"}]).
-xep([{xep, 45}, {version, "1.25"}]).
%% ----------------------------------------------------------------------
%% Exports

%% Client API
-export([delete_archive/2,
         archive_size/2,
         archive_id/2]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd room handlers
-export([filter_room_packet/2,
         room_process_mam_iq/4,
         forget_room/2,
         forget_room/3]).

%% gdpr callback
-export([get_personal_data/2]).

%% private
-export([archive_message/8]).
-export([lookup_messages/2]).
-export([archive_id_int/2]).
-export([handle_set_message_form/3]).
-export([handle_lookup_result/4]).
%% ----------------------------------------------------------------------
%% Imports

-import(mod_mam_utils,
        [microseconds_to_now/1]).

%% UID
-import(mod_mam_utils,
        [generate_message_id/0,
         decode_compact_uuid/1]).

%% XML
-import(mod_mam_utils,
        [maybe_add_arcid_elems/4,
         replace_x_user_element/4,
         delete_x_user_element/1,
         packet_to_x_user_jid/1,
         wrap_message/6,
         result_set/4,
         result_query/2,
         result_prefs/4,
         make_fin_message/5,
         make_fin_element/4,
         parse_prefs/1,
         borders_decode/1]).

%% Forms
-import(mod_mam_utils,
        [message_form/3]).

%% Other
-import(mod_mam_utils,
        [mess_id_to_external_binary/1,
         is_complete_result_page/4]).

%% ejabberd
-import(mod_mam_utils,
        [send_message/3]).


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
-type row() :: {mod_mam:message_id(), jid:jid(), exml:element()}.

-export_type([row/0, row_batch/0]).

%% ----------------------------------------------------------------------
%% API

-spec get_personal_data(gdpr:personal_data(), jid:jid()) -> gdpr:personal_data().
get_personal_data(Acc, #jid{ lserver = LServer } = JID) ->
    Schema = ["id", "message"],
    Entries = mongoose_hooks:get_mam_muc_gdpr_data(LServer, [], JID),
    [{mam_muc, Schema, Entries} | Acc].

-spec delete_archive(jid:server(), jid:user()) -> 'ok'.
delete_archive(SubHost, RoomName) when is_binary(SubHost), is_binary(RoomName) ->
    ?DEBUG("Remove room ~p from ~p.", [RoomName, SubHost]),
    ArcJID = jid:make(RoomName, SubHost, <<>>),
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    ArcID = archive_id_int(Host, ArcJID),
    remove_archive(Host, ArcID, ArcJID),
    ok.


-spec archive_size(jid:server(), jid:user()) -> integer().

archive_size(SubHost, RoomName) when is_binary(SubHost), is_binary(RoomName) ->
    ArcJID = jid:make(RoomName, SubHost, <<>>),
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    ArcID = archive_id_int(Host, ArcJID),
    archive_size(Host, ArcID, ArcJID).


-spec archive_id(jid:server(), jid:user()) -> integer().
archive_id(SubHost, RoomName) when is_binary(SubHost), is_binary(RoomName) ->
    ArcJID = jid:make(RoomName, SubHost, <<>>),
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    archive_id_int(Host, ArcJID).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for MUC archives

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?DEBUG("mod_mam_muc starting", []),
    %% MUC host.
    MUCHost = gen_mod:get_opt_subhost(Host, Opts, mod_muc:default_host()),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    mod_disco:register_feature(MUCHost, ?NS_MAM_04),
    mod_disco:register_feature(MUCHost, ?NS_MAM_06),
    gen_iq_handler:add_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_04,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_06,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    ejabberd_hooks:add(filter_room_packet, MUCHost, ?MODULE,
                       filter_room_packet, 90),
    ejabberd_hooks:add(forget_room, MUCHost, ?MODULE, forget_room, 90),
    ejabberd_hooks:add(get_personal_data, Host, ?MODULE, get_personal_data, 50),
    ok.

-spec stop(Host :: jid:server()) -> any().

stop(Host) ->
    MUCHost = gen_mod:get_module_opt_subhost(Host, mod_mam_muc, mod_muc:default_host()),
    ?DEBUG("mod_mam stopping", []),
    ejabberd_hooks:delete(filter_room_packet, MUCHost, ?MODULE, filter_room_packet, 90),
    ejabberd_hooks:delete(forget_room, MUCHost, ?MODULE, forget_room, 90),
    ejabberd_hooks:delete(get_personal_data, Host, ?MODULE, get_personal_data, 50),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_04),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_06),
    mod_disco:unregister_feature(MUCHost, ?NS_MAM_04),
    mod_disco:unregister_feature(MUCHost, ?NS_MAM_06),
    ok.

%% ----------------------------------------------------------------------
%% hooks and handlers for MUC

%% @doc Handle public MUC-message.
-spec filter_room_packet(Packet :: packet(), EventData :: list()) -> packet().
filter_room_packet(Packet, EventData) ->
    {room_jid, #jid{lserver = LServer}} = lists:keyfind(room_jid, 1, EventData),
    ?DEBUG("Incoming room packet.", []),
    IsArchivable = is_archivable_message(LServer, incoming, Packet),
    case IsArchivable of
        true ->
            {_, FromNick} = lists:keyfind(from_nick, 1, EventData),
            {_, FromJID} = lists:keyfind(from_jid, 1, EventData),
            {_, RoomJID} = lists:keyfind(room_jid, 1, EventData),
            {_, Role} = lists:keyfind(role, 1, EventData),
            {_, Affiliation} = lists:keyfind(affiliation, 1, EventData),
            archive_room_packet(Packet, FromNick, FromJID, RoomJID, Role, Affiliation);
        false -> Packet
    end.


%% @doc Archive without validation.
-spec archive_room_packet(Packet :: packet(), FromNick :: jid:user(),
                          FromJID :: jid:jid(), RoomJID :: jid:jid(),
                          Role :: mod_muc:role(), Affiliation :: mod_muc:affiliation()) -> packet().
archive_room_packet(Packet, FromNick, FromJID=#jid{}, RoomJID=#jid{}, Role, Affiliation) ->
    {ok, Host} = mongoose_subhosts:get_host(RoomJID#jid.lserver),
    ArcID = archive_id_int(Host, RoomJID),
    %% Occupant JID <room@service/nick>
    SrcJID = jid:replace_resource(RoomJID, FromNick),
    IsInteresting =
        case get_behaviour(Host, ArcID, RoomJID, SrcJID, always) of
            always -> true;
            never -> false;
            roster -> true
        end,
    case IsInteresting of
        true ->
            MessID = generate_message_id(),
            Packet1 = replace_x_user_element(FromJID, Role, Affiliation, Packet),
            Result = archive_message(Host, MessID, ArcID,
                                     RoomJID, FromJID, SrcJID, incoming, Packet1),
            %% Packet2 goes to archive, Packet to other users
            case Result of
                ok ->
                    maybe_add_arcid_elems(RoomJID,
                                          mess_id_to_external_binary(MessID),
                                             Packet,
                                             mod_mam_params:add_stanzaid_element(?MODULE, Host));
                {error, _} -> Packet
            end;
        false -> Packet
    end.


%% @doc `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec room_process_mam_iq(From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
                          IQ :: jlib:iq()) -> {mongoose_acc:t(), jlib:iq() | ignore}.
room_process_mam_iq(From = #jid{lserver = Host}, To, Acc, IQ) ->
    mod_mam_utils:maybe_log_deprecation(IQ),
    Action = mam_iq:action(IQ),
    case is_action_allowed(Action, From, To) of
        true ->
            case mod_mam_utils:wait_shaper(Host, Action, From) of
                ok ->
                    handle_error_iq(Acc, Host, To, Action,
                                    handle_mam_iq(Action, From, To, IQ));
                {error, max_delay_reached} ->
                    mongoose_metrics:update(Host, modMucMamDroppedIQ, 1),
                    {Acc, return_max_delay_reached_error_iq(IQ)}
            end;
        false -> {Acc, return_action_not_allowed_error_iq(IQ)}
    end.


%% #rh
%% @doc This hook is called from `mod_muc:forget_room(Host, Name)'.
-spec forget_room(map(), jid:lserver(), binary()) -> map().
forget_room(Acc, LServer, RoomName) ->
    forget_room(LServer, RoomName),
    Acc.

%% @doc This hook is called from `mod_muc:forget_room(Host, Name)'.
-spec forget_room(jid:lserver(), binary()) -> 'ok'.
forget_room(LServer, RoomName) ->
    delete_archive(LServer, RoomName).

%% ----------------------------------------------------------------------
%% Internal functions

-spec is_action_allowed(atom(), jid:jid(), jid:jid()) -> boolean().
is_action_allowed(Action, From, To = #jid{lserver = Host}) ->
    case acl:match_rule(Host, Action, From, default) of
        allow -> true;
        deny -> false;
        default -> is_action_allowed_by_default(Action, From, To)
    end.

-spec is_action_allowed_by_default(Action :: mam_iq:action(), From :: jid:jid(),
                                   To :: jid:jid()) -> boolean().
is_action_allowed_by_default(Action, From, To) ->
    is_room_action_allowed_by_default(Action, From, To).

-spec is_room_action_allowed_by_default(Action :: mam_iq:action(),
                                        From :: jid:jid(), To :: jid:jid()) -> boolean().
is_room_action_allowed_by_default(Action, From, To) ->
    case mam_iq:action_type(Action) of
        set -> is_room_owner(From, To);
        get -> can_access_room(From, To)
    end.


-spec is_room_owner(User :: jid:jid(), Room :: jid:jid()) -> boolean().
is_room_owner(User, Room) ->
    mongoose_hooks:is_muc_room_owner(Room#jid.lserver, false, Room, User).


%% @doc Return true if user element should be removed from results
-spec is_user_identity_hidden(User :: jid:jid(), Room :: jid:jid()) -> boolean().
is_user_identity_hidden(User, Room) ->
    case mongoose_hooks:can_access_identity(Room#jid.lserver, false, Room, User) of
        CanAccess when is_boolean(CanAccess) -> not CanAccess
    end.

-spec can_access_room(User :: jid:jid(), Room :: jid:jid()) -> boolean().
can_access_room(User, Room) ->
    mongoose_hooks:can_access_room(Room#jid.lserver, false, Room, User).

-spec handle_mam_iq(mam_iq:action(), From :: jid:jid(), jid:jid(), jlib:iq()) ->
                           jlib:iq() | {error, any(), jlib:iq()} | ignore.
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
                              jlib:iq() | {error, any(), jlib:iq()}.
handle_set_prefs(ArcJID = #jid{},
                 IQ = #iq{sub_el = PrefsEl}) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} = parse_prefs(PrefsEl),
    ?DEBUG("Parsed data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
           [DefaultMode, AlwaysJIDs, NeverJIDs]),
    {ok, Host} = mongoose_subhosts:get_host(ArcJID#jid.lserver),
    ArcID = archive_id_int(Host, ArcJID),
    Res = set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs),
    handle_set_prefs_result(Res, DefaultMode, AlwaysJIDs, NeverJIDs, IQ).

handle_set_prefs_result(ok, DefaultMode, AlwaysJIDs, NeverJIDs, IQ) ->
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, IQ#iq.xmlns),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_set_prefs_result({error, Reason},
                        _DefaultMode, _AlwaysJIDs, _NeverJIDs, IQ) ->
    return_error_iq(IQ, Reason).


-spec handle_get_prefs(jid:jid(), jlib:iq()) ->
                              jlib:iq() | {error, any(), jlib:iq()}.
handle_get_prefs(ArcJID=#jid{}, IQ=#iq{}) ->
    {ok, Host} = mongoose_subhosts:get_host(ArcJID#jid.lserver),
    ArcID = archive_id_int(Host, ArcJID),
    Res = get_prefs(Host, ArcID, ArcJID, always),
    handle_get_prefs_result(Res, IQ).

handle_get_prefs_result({DefaultMode, AlwaysJIDs, NeverJIDs}, IQ) ->
    ?DEBUG("Extracted data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
           [DefaultMode, AlwaysJIDs, NeverJIDs]),
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, IQ#iq.xmlns),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_get_prefs_result({error, Reason}, IQ) ->
    return_error_iq(IQ, Reason).

-spec handle_set_message_form(From :: jid:jid(), ArcJID :: jid:jid(),
                              IQ :: jlib:iq()) ->
                                  jlib:iq() | ignore | {error, term(), jlib:iq()}.
handle_set_message_form(#jid{} = From, #jid{} = ArcJID, IQ) ->
    {ok, Host} = mongoose_subhosts:get_host(ArcJID#jid.lserver),
    ArcID = archive_id_int(Host, ArcJID),
    Params0 = mam_iq:form_to_lookup_params(IQ, mod_mam_params:max_result_limit(?MODULE, Host),
                                           mod_mam_params:default_result_limit(?MODULE, Host),
                                           mod_mam_params:extra_params_module(?MODULE, Host)),
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID),
    Result = lookup_messages(Host, Params),
    handle_lookup_result(Result, From, IQ, Params).

-spec handle_lookup_result({ok, mod_mam:lookup_result()} | {error, term()}, jid:jid(),
                           jlib:iq(), map()) ->
    jlib:iq() | ignore | {error, term(), jlib:iq()}.
handle_lookup_result(Result, From, IQ, #{owner_jid := ArcJID} = Params) ->
    case Result of
        {error, Reason} ->
            report_issue(Reason, mam_muc_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, Res} ->
            send_messages_and_iq_result(Res, From, IQ, Params)
    end.

send_messages_and_iq_result({TotalCount, Offset, MessageRows}, From,
                            #iq{xmlns = MamNs, sub_el = QueryEl} = IQ,
                            #{owner_jid := ArcJID} = Params) ->
    %% Forward messages
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    {FirstMessID, LastMessID} = forward_messages(From, ArcJID, MamNs,
                                                 QueryID, MessageRows, true),

    %% Make fin iq
    IsComplete = is_complete_result_page(TotalCount, Offset, MessageRows, Params),
    IsStable = true,
    ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
    FinElem = make_fin_element(IQ#iq.xmlns, IsComplete, IsStable, ResultSetEl),
    IQ#iq{type = result, sub_el = [FinElem]}.

forward_messages(From, ArcJID, MamNs, QueryID, MessageRows, SetClientNs) ->
    %% Forward messages
    {FirstMessID, LastMessID, HideUser} =
        case MessageRows of
            [] -> {undefined, undefined, undefined};
            [_ | _] -> {message_row_to_ext_id(hd(MessageRows)),
                        message_row_to_ext_id(lists:last(MessageRows)),
                        is_user_identity_hidden(From, ArcJID)}
        end,
    [send_message(ArcJID, From, message_row_to_xml(MamNs, From, HideUser, SetClientNs, Row,
                                                   QueryID))
     || Row <- MessageRows],
    {FirstMessID, LastMessID}.

-spec handle_get_message_form(jid:jid(), jid:jid(), jlib:iq()) ->
                                     jlib:iq().
handle_get_message_form(_From = #jid{lserver = Host}, _ArcJID = #jid{}, IQ = #iq{}) ->
    return_message_form_iq(Host, IQ).

%% ----------------------------------------------------------------------
%% Backend wrappers

-spec archive_id_int(jid:server(), jid:jid()) -> integer() | undefined.
archive_id_int(Host, ArcJID = #jid{}) ->
    mongoose_hooks:mam_muc_archive_id(Host, undefined, ArcJID).


-spec archive_size(jid:server(), mod_mam:archive_id(), jid:jid())
                  -> integer().
archive_size(Host, ArcID, ArcJID = #jid{}) ->
    mongoose_hooks:mam_muc_archive_size(Host, 0, ArcID, ArcJID).

-spec get_behaviour(jid:server(), mod_mam:archive_id(),
                    LocJID :: jid:jid(), RemJID :: jid:jid(),
                    DefaultBehaviour :: 'always') -> any().
get_behaviour(Host, ArcID,
              LocJID = #jid{},
              RemJID = #jid{}, DefaultBehaviour) ->
    mongoose_hooks:mam_muc_get_behaviour(Host, DefaultBehaviour, ArcID, LocJID, RemJID).


-spec set_prefs(Host :: jid:server(), ArcID :: mod_mam:archive_id(),
                ArcJID :: jid:jid(), DefaultMode :: mod_mam:archive_behaviour(),
                AlwaysJIDs :: [jid:literal_jid()],
                NeverJIDs :: [jid:literal_jid()]) -> any().
set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    mongoose_hooks:mam_muc_set_prefs(Host, {error, not_implemented},
                                     ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs).


%% @doc Load settings from the database.
-spec get_prefs(Host :: jid:server(), ArcID :: mod_mam:archive_id(),
                ArcJID :: jid:jid(), GlobalDefaultMode :: mod_mam:archive_behaviour())
               -> mod_mam:preference() | {error, Reason :: term()}.
get_prefs(Host, ArcID, ArcJID, GlobalDefaultMode) ->
    mongoose_hooks:mam_muc_get_prefs(Host, {GlobalDefaultMode, [], []}, ArcID, ArcJID).

-spec remove_archive(jid:server(), mod_mam:archive_id() | undefined,
                     jid:jid()) -> 'ok'.
remove_archive(Host, ArcID, ArcJID = #jid{}) ->
    mongoose_hooks:mam_muc_remove_archive(Host, ok, ArcID, ArcJID),
    ok.


%% See description in mod_mam.
-spec lookup_messages(Host :: jid:server(), Params :: map()) ->
    {ok, mod_mam:lookup_result()}
    | {error, 'policy-violation'}
    | {error, Reason :: term()}.%Result :: any(),
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
            mongoose_hooks:mam_muc_lookup_messages(Host, {ok, {0, 0, []}}, Params)
    end.


-spec archive_message(jid:server(), MessId :: mod_mam:message_id(),
                      ArcId :: mod_mam:archive_id(), LocJID :: jid:jid(),
                      SenderJID :: jid:jid(), SrcJID :: jid:jid(), Dir :: 'incoming',
                      packet()) -> any().
archive_message(Host, MessID, ArcID, LocJID, SenderJID, SrcJID, Dir, Packet) ->
    mongoose_hooks:mam_muc_archive_message(Host, ok, MessID, ArcID,
                                           LocJID, SenderJID, SrcJID, Dir, Packet).

%% ----------------------------------------------------------------------
%% Helpers

-spec message_row_to_xml(binary(), jid:jid(), boolean(), boolean(), row(), binary() | undefined) ->
                                exml:element().
message_row_to_xml(MamNs, ReceiverJID, HideUser, SetClientNs, {MessID, SrcJID, Packet}, QueryID) ->

    {Microseconds, _NodeMessID} = decode_compact_uuid(MessID),
    DateTime = calendar:now_to_universal_time(microseconds_to_now(Microseconds)),
    BExtMessID = mess_id_to_external_binary(MessID),
    Packet1 = maybe_delete_x_user_element(HideUser, ReceiverJID, Packet),
    Packet2 = mod_mam_utils:maybe_set_client_xmlns(SetClientNs, Packet1),
    Packet3 = replace_from_to_attributes(SrcJID, Packet2),
    wrap_message(MamNs, Packet3, QueryID, BExtMessID, DateTime, SrcJID).

maybe_delete_x_user_element(true, ReceiverJID, Packet) ->
    PacketJID = packet_to_x_user_jid(Packet),
    case jid:are_bare_equal(ReceiverJID, PacketJID) of
        false ->
            delete_x_user_element(Packet);
        true -> %% expose identity for user's own messages
            Packet
    end;
maybe_delete_x_user_element(false, _ReceiverJID, Packet) ->
    Packet.

%% From XEP-0313:
%% When sending out the archives to a requesting client, the 'to' of the
%% forwarded stanza MUST be empty, and the 'from' MUST be the occupant JID
%% of the sender of the archived message.
%% However, Smack crashes if 'to' is present, so it is removed.
replace_from_to_attributes(SrcJID, Packet = #xmlel{attrs = Attrs}) ->
    NewAttrs = jlib:replace_from_to_attrs(jid:to_binary(SrcJID), undefined, Attrs),
    Packet#xmlel{attrs = NewAttrs}.

-spec message_row_to_ext_id(row()) -> binary().
message_row_to_ext_id({MessID, _, _}) ->
    mess_id_to_external_binary(MessID).

-spec handle_error_iq(mongoose_acc:t(), jid:lserver(), jid:jid(), atom(),
    {error, term(), jlib:iq()} | jlib:iq() | ignore) -> {mongoose_acc:t(), jlib:iq() | ignore}.
handle_error_iq(Acc, Host, _To, _Action, {error, _Reason, IQ}) ->
    mongoose_metrics:update(Host, modMucMamDroppedIQ, 1),
    {Acc, IQ};
handle_error_iq(Acc, _Host, _To, _Action, IQ) ->
    {Acc, IQ}.

return_error_iq(IQ, {Reason, {stacktrace, _Stacktrace}}) ->
    return_error_iq(IQ, Reason);
return_error_iq(IQ, timeout) ->
    {error, timeout, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:service_unavailable()]}};
return_error_iq(IQ, item_not_found) ->
    {error, item_not_found, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:item_not_found()]}};
return_error_iq(IQ, not_implemented) ->
    {error, not_implemented, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:feature_not_implemented()]}};
return_error_iq(IQ, missing_with_jid) ->
    Error =  mongoose_xmpp_errors:bad_request(<<"en">>,
                               <<"Limited set of queries allowed in the conversation mode.",
                                 "Missing with_jid filter">>),
    {error, bad_request, IQ#iq{type = error, sub_el = [Error]}};
return_error_iq(IQ, Reason) ->
    {error, Reason, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:internal_server_error()]}}.

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

return_message_form_iq(Host, IQ) ->
    IQ#iq{type = result, sub_el = [message_form(?MODULE, Host, IQ#iq.xmlns)]}.


% the stacktrace is a big lie
report_issue({Reason, {stacktrace, Stacktrace}}, Issue, ArcJID, IQ) ->
    report_issue(Reason, Stacktrace, Issue, ArcJID, IQ);
report_issue(Reason, Issue, ArcJID, IQ) ->
    report_issue(Reason, [], Issue, ArcJID, IQ).

report_issue(item_not_found, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(missing_with_jid, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(not_implemented, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(timeout, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(Reason, Stacktrace, Issue, #jid{lserver = LServer, luser = LUser}, IQ) ->
    ?ERROR_MSG("issue=~p, server=~p, user=~p, reason=~p, iq=~p, stacktrace=~p",
               [Issue, LServer, LUser, Reason, IQ, Stacktrace]).

-spec is_archivable_message(MUCHost :: ejabberd:lserver(), Dir :: incoming | outgoing,
                            Packet :: exml:element()) -> boolean().
is_archivable_message(MUCHost, Dir, Packet) ->
    {ok, Host} = mongoose_subhosts:get_host(MUCHost),
    {M, F} = mod_mam_params:is_archivable_message_fun(?MODULE, Host),
    ArchiveChatMarkers = mod_mam_params:archive_chat_markers(?MODULE, Host),
    erlang:apply(M, F, [?MODULE, Dir, Packet, ArchiveChatMarkers]).
