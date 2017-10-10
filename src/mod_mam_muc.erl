%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc XEP-0313: Message Archive Management
%%%
%%% The module uses several backend modules:
%%%
%%% <ul>
%%% <li>Preference manager ({@link mod_mam_muc_odbc_prefs});</li>
%%% <li>Writer ({@link mod_mam_muc_odbc_arch} or {@link mod_mam_muc_odbc_async_pool_writer});</li>
%%% <li>Archive manager ({@link mod_mam_muc_odbc_arch});</li>
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
-xep([{xep, 313}, {version, "0.2"}]).
-xep([{xep, 313}, {version, "0.3"}]).
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

%% private
-export([archive_message/8]).
-export([lookup_messages/2]).
-export([archive_id_int/2]).
-export([set_params/1]).
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
        [maybe_add_arcid_elems/5,
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
         is_last_page/4]).

%% Deprecations
 -import(mod_mam_utils,
         [start_deprecation_reminder/0,
          maybe_log_deprecation_error/0]).

%% ejabberd
-import(mod_mam_utils,
        [send_message/3]).


-include_lib("ejabberd.hrl").
-include_lib("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-callback is_complete_message(Module :: atom(), Dir :: atom(), Packet :: any()) ->
    boolean().

%% ----------------------------------------------------------------------
%% Datetime types
%% Microseconds from 01.01.1970

-type unix_timestamp() :: mod_mam:unix_timestamp().

%% ----------------------------------------------------------------------
%% Other types
-type action() :: 'mam_get_prefs'
                | 'mam_lookup_messages'
                | 'mam_purge_multiple_messages'
                | 'mam_purge_single_message'
                | 'mam_set_prefs'
                | 'mam_set_message_form'
                | 'mam_get_message_form'.
-type packet() :: any().
-type row_batch() :: {TotalCount :: non_neg_integer(),
                      Offset :: non_neg_integer(),
                      MessageRows :: [row()]}.
-type row() :: {mod_mam:message_id(), ejabberd:jid(), jlib:xmlel()}.

-export_type([row/0, row_batch/0]).

%% ----------------------------------------------------------------------
%% API

-spec delete_archive(ejabberd:server(), ejabberd:user()) -> 'ok'.
delete_archive(SubHost, RoomName) when is_binary(SubHost), is_binary(RoomName) ->
    ?DEBUG("Remove room ~p from ~p.", [RoomName, SubHost]),
    ArcJID = jid:make(RoomName, SubHost, <<>>),
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    ArcID = archive_id_int(Host, ArcJID),
    remove_archive(Host, ArcID, ArcJID),
    ok.


-spec archive_size(ejabberd:server(), ejabberd:user()) -> integer().

archive_size(SubHost, RoomName) when is_binary(SubHost), is_binary(RoomName) ->
    ArcJID = jid:make(RoomName, SubHost, <<>>),
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    ArcID = archive_id_int(Host, ArcJID),
    archive_size(Host, ArcID, ArcJID).


-spec archive_id(ejabberd:server(), ejabberd:user()) -> integer().
archive_id(SubHost, RoomName) when is_binary(SubHost), is_binary(RoomName) ->
    ArcJID = jid:make(RoomName, SubHost, <<>>),
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    archive_id_int(Host, ArcJID).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for MUC archives

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?DEBUG("mod_mam_muc starting", []),
    case gen_mod:get_opt(add_archived_element, Opts, undefined) of
        undefined -> ok;
        _ ->
            start_deprecation_reminder(),
            maybe_log_deprecation_error()
    end,
    compile_params_module(Opts),
    %% MUC host.
    MUCHost = gen_mod:get_opt_subhost(Host, Opts, mod_muc:default_host()),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    mod_disco:register_feature(MUCHost, ?NS_MAM),
    mod_disco:register_feature(MUCHost, ?NS_MAM_03),
    mod_disco:register_feature(MUCHost, ?NS_MAM_04),
    mod_disco:register_feature(MUCHost, ?NS_MAM_06),
    gen_iq_handler:add_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_03,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_04,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_06,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    ejabberd_hooks:add(filter_room_packet, MUCHost, ?MODULE,
                       filter_room_packet, 90),
    ejabberd_hooks:add(forget_room, MUCHost, ?MODULE, forget_room, 90),
    ok.

-spec stop(Host :: ejabberd:server()) -> any().

stop(Host) ->
    MUCHost = gen_mod:get_module_opt_subhost(Host, mod_mam_muc, mod_muc:default_host()),
    ?DEBUG("mod_mam stopping", []),
    ejabberd_hooks:delete(filter_room_packet, MUCHost, ?MODULE, filter_room_packet, 90),
    ejabberd_hooks:delete(forget_room, MUCHost, ?MODULE, forget_room, 90),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_03),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_04),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, MUCHost, ?NS_MAM_06),
    mod_disco:unregister_feature(MUCHost, ?NS_MAM),
    mod_disco:unregister_feature(MUCHost, ?NS_MAM_03),
    mod_disco:unregister_feature(MUCHost, ?NS_MAM_04),
    mod_disco:unregister_feature(MUCHost, ?NS_MAM_06),
    ok.

%% ----------------------------------------------------------------------
%% hooks and handlers for MUC

%% @doc Handle public MUC-message.
-spec filter_room_packet(Packet :: packet(), EventData :: list()) -> packet().
filter_room_packet(Packet, EventData) ->
    ?DEBUG("Incoming room packet.", []),
    IsArchivable = mod_mam_muc_params:is_archivable_message(?MODULE, incoming, Packet),
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
-spec archive_room_packet(Packet :: packet(), FromNick :: ejabberd:user(),
                          FromJID :: ejabberd:jid(), RoomJID :: ejabberd:jid(),
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
                                     RoomJID, SrcJID, SrcJID, incoming, Packet1),
            %% Packet2 goes to archive, Packet to other users
            case Result of
                ok ->
                    maybe_add_arcid_elems(RoomJID,
                                             mess_id_to_external_binary(MessID),
                                             Packet,
                                             add_archived_element(),
                                             add_stanzaid_element());
                {error, _} -> Packet
            end;
        false -> Packet
    end.


%% @doc `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec room_process_mam_iq(From :: ejabberd:jid(), To :: ejabberd:jid(), Acc :: mongoose_acc:t(),
                          IQ :: ejabberd:iq()) -> {mongoose_acc:t(), ejabberd:iq() | 'ignore'}.
room_process_mam_iq(From = #jid{lserver = Host}, To, Acc, IQ) ->
    Action = iq_action(IQ),
    Res = case is_action_allowed(Action, From, To) of
        true ->
            case wait_shaper(Host, Action, From) of
                ok ->
                    handle_error_iq(Host, To, Action,
                                    handle_mam_iq(Action, From, To, IQ));
                {error, max_delay_reached} ->
                    ejabberd_hooks:run(mam_muc_drop_iq, Host,
                                       [Host, To, IQ, Action, max_delay_reached]),
                    return_max_delay_reached_error_iq(IQ)
            end;
        false -> return_action_not_allowed_error_iq(IQ)
    end,
    {Acc, Res}.


%% #rh
%% @doc This hook is called from `mod_muc:forget_room(Host, Name)'.
-spec forget_room(map(), ejabberd:lserver(), binary()) -> map().
forget_room(Acc, LServer, RoomName) ->
    forget_room(LServer, RoomName),
    Acc.

%% @doc This hook is called from `mod_muc:forget_room(Host, Name)'.
-spec forget_room(ejabberd:lserver(), binary()) -> 'ok'.
forget_room(LServer, RoomName) ->
    delete_archive(LServer, RoomName).

%% ----------------------------------------------------------------------
%% Internal functions

-spec is_action_allowed(atom(), ejabberd:jid(), ejabberd:jid()) -> boolean().
is_action_allowed(Action, From, To = #jid{lserver = Host}) ->
    case acl:match_rule(Host, Action, From, default) of
        allow -> true;
        deny -> false;
        default -> is_action_allowed_by_default(Action, From, To)
    end.

-spec is_action_allowed_by_default(Action :: action(), From :: ejabberd:jid(),
                                   To :: ejabberd:jid()) -> boolean().
is_action_allowed_by_default(Action, From, To) ->
    is_room_action_allowed_by_default(Action, From, To).


-spec is_room_action_allowed_by_default(Action :: action(),
                                        From :: ejabberd:jid(), To :: ejabberd:jid()) -> boolean().
is_room_action_allowed_by_default(Action, From, To) ->
    case action_type(Action) of
        set -> is_room_owner(From, To);
        get -> can_access_room(From, To)
    end.


-spec is_room_owner(User :: ejabberd:jid(), Room :: ejabberd:jid()) -> boolean().
is_room_owner(User, Room) ->
    ejabberd_hooks:run_fold(is_muc_room_owner, Room#jid.lserver, false, [Room, User]).


%% @doc Return true if user element should be removed from results
-spec is_user_identity_hidden(User :: ejabberd:jid(), Room :: ejabberd:jid()) -> boolean().
is_user_identity_hidden(User, Room) ->
    case ejabberd_hooks:run_fold(can_access_identity, Room#jid.lserver, false, [Room, User]) of
        CanAccess when is_boolean(CanAccess) -> not CanAccess;
        _ -> true
    end.

-spec can_access_room(User :: ejabberd:jid(), Room :: ejabberd:jid()) -> boolean().
can_access_room(User, Room) ->
    ejabberd_hooks:run_fold(can_access_room, Room#jid.lserver, false, [Room, User]).


-spec action_type(action()) -> 'get' | 'set'.
action_type(mam_get_prefs) -> get;
action_type(mam_set_prefs) -> set;
action_type(mam_lookup_messages) -> get;
action_type(mam_set_message_form) -> get;
action_type(mam_get_message_form) -> get;
action_type(mam_purge_single_message) -> set;
action_type(mam_purge_multiple_messages) -> set.


-spec action_to_shaper_name(action()) -> atom().
action_to_shaper_name(Action) ->
    list_to_atom(atom_to_list(Action) ++ "_shaper").


-spec action_to_global_shaper_name(action()) -> atom().
action_to_global_shaper_name(Action) -> list_to_atom(atom_to_list(Action) ++ "_global_shaper").


-spec handle_mam_iq('mam_get_prefs', From :: ejabberd:jid(), ejabberd:jid(),
                    ejabberd:iq()) ->
                           ejabberd:iq() | {error, any(), ejabberd:iq()}.
handle_mam_iq(Action, From, To, IQ) ->
    case Action of
        mam_get_prefs ->
            handle_get_prefs(To, IQ);
        mam_set_prefs ->
            handle_set_prefs(To, IQ);
        mam_lookup_messages ->
            handle_lookup_messages(From, To, IQ);
        mam_set_message_form ->
            handle_set_message_form(From, To, IQ);
        mam_get_message_form ->
            handle_get_message_form(From, To, IQ);
        mam_purge_single_message ->
            handle_purge_single_message(To, IQ);
        mam_purge_multiple_messages ->
            handle_purge_multiple_messages(To, IQ)
    end.


-spec iq_action(ejabberd:iq()) -> action().
iq_action(IQ = #iq{xmlns = ?NS_MAM}) ->
    iq_action02(IQ);
iq_action(IQ = #iq{xmlns = ?NS_MAM_03}) ->
    iq_action03(IQ);
iq_action(IQ = #iq{xmlns = ?NS_MAM_04}) ->
    iq_action03(IQ);
iq_action(IQ = #iq{xmlns = ?NS_MAM_06}) ->
    iq_action03(IQ).

iq_action02(#iq{type = Action, sub_el = SubEl = #xmlel{name = Category}}) ->
    case {Action, Category} of
        {set, <<"prefs">>} -> mam_set_prefs;
        {get, <<"prefs">>} -> mam_get_prefs;
        {get, <<"query">>} -> mam_lookup_messages;
        {set, <<"purge">>} ->
            case exml_query:attr(SubEl, <<"id">>) of
                undefined -> mam_purge_multiple_messages;
                _ -> mam_purge_single_message
            end
    end.

iq_action03(#iq{type = Action, sub_el = #xmlel{name = Category}}) ->
    case {Action, Category} of
        {set, <<"prefs">>} -> mam_set_prefs;
        {get, <<"prefs">>} -> mam_get_prefs;
        {get, <<"query">>} -> mam_get_message_form;
        {set, <<"query">>} -> mam_set_message_form
                              %% Purge is NOT official extention, it is not implemented for
                              %% XEP-0313 v0.3. Use v0.2 namespace if you really want it.
    end.

-spec handle_set_prefs(ejabberd:jid(), ejabberd:iq()) ->
                              ejabberd:iq() | {error, any(), ejabberd:iq()}.
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


-spec handle_get_prefs(ejabberd:jid(), ejabberd:iq()) ->
                              ejabberd:iq() | {error, any(), ejabberd:iq()}.
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


-spec handle_lookup_messages(From :: ejabberd:jid(), ArcJID :: ejabberd:jid(),
                             IQ :: ejabberd:iq()) -> ejabberd:iq() | {error, any(), ejabberd:iq()}.
handle_lookup_messages(
  From = #jid{},
  ArcJID = #jid{},
  IQ = #iq{xmlns = MamNs, sub_el = QueryEl}) ->
    {ok, Host} = mongoose_subhosts:get_host(ArcJID#jid.lserver),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    Params0 = mam_iq:query_to_lookup_params(QueryEl),
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID),
    case lookup_messages(Host, Params) of
        {error, 'policy-violation'} ->
            ?DEBUG("Policy violation by ~p.", [jid:to_binary(From)]),
            ErrorEl = jlib:stanza_errort(<<"">>, <<"modify">>, <<"policy-violation">>,
                                         <<"en">>, <<"Too many results">>),
            IQ#iq{type = error, sub_el = [ErrorEl]};
        {error, Reason} ->
            report_issue(Reason, mam_muc_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, {TotalCount, Offset, MessageRows}} ->
            {FirstMessID, LastMessID, HideUser} =
                case MessageRows of
                    [] -> {undefined, undefined, undefined};
                    [_ | _] -> {message_row_to_ext_id(hd(MessageRows)),
                                message_row_to_ext_id(lists:last(MessageRows)),
                                is_user_identity_hidden(From, ArcJID)}
                end,
            SetClientNs = false,
            [send_message(ArcJID, From, message_row_to_xml(MamNs, From, HideUser, SetClientNs,
                                                           Row, QueryID))
             || Row <- MessageRows],
            ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
            ResultQueryEl = result_query(ResultSetEl, MamNs),
            %% On receiving the query, the server pushes to the client a series of
            %% messages from the archive that match the client's given criteria,
            %% and finally returns the <iq/> result.
            IQ#iq{type = result, sub_el = [ResultQueryEl]}
    end.


-spec handle_set_message_form(From :: ejabberd:jid(), ArcJID :: ejabberd:jid(),
                              IQ :: ejabberd:iq()) ->
                                     ejabberd:iq() | ignore | {error, term(), ejabberd:iq()}.
handle_set_message_form(#jid{} = From, #jid{} = ArcJID,
                        IQ = #iq{xmlns = MamNs, sub_el = QueryEl}) ->
    {ok, Host} = mongoose_subhosts:get_host(ArcJID#jid.lserver),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    Params0 = mam_iq:form_to_lookup_params(QueryEl),
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID),
    PageSize = maps:get(page_size, Params),
    case lookup_messages(Host, Params) of
        {error, Reason} ->
            report_issue(Reason, mam_muc_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, {TotalCount, Offset, MessageRows}} when IQ#iq.xmlns =:= ?NS_MAM_03 ->
            ResIQ = IQ#iq{type = result, sub_el = []},
            %% Server accepts the query
            ejabberd_router:route(ArcJID, From, jlib:iq_to_xml(ResIQ)),


            %% Forward messages
            {FirstMessID, LastMessID, HideUser} =
                case MessageRows of
                    [] -> {undefined, undefined, undefined};
                    [_ | _] -> {message_row_to_ext_id(hd(MessageRows)),
                                message_row_to_ext_id(lists:last(MessageRows)),
                                is_user_identity_hidden(From, ArcJID)}
                end,
            SetClientNs = true,
            [send_message(ArcJID, From, message_row_to_xml(MamNs, From, HideUser, SetClientNs, Row,
                                                           QueryID))
             || Row <- MessageRows],

            %% Make fin message
            IsLastPage = is_last_page(PageSize, TotalCount, Offset, MessageRows),
            IsStable = true,
            ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
            FinMsg = make_fin_message(IQ#iq.xmlns, IsLastPage, IsStable, ResultSetEl, QueryID),
            ejabberd_sm:route(ArcJID, From, FinMsg),

            %% IQ was sent above
            ignore;
        {ok, {TotalCount, Offset, MessageRows}} ->
            %% Forward messages
            {FirstMessID, LastMessID, HideUser} =
                case MessageRows of
                    [] -> {undefined, undefined, undefined};
                    [_ | _] -> {message_row_to_ext_id(hd(MessageRows)),
                                message_row_to_ext_id(lists:last(MessageRows)),
                                is_user_identity_hidden(From, ArcJID)}
                end,
            SetClientNs = true,
            [send_message(ArcJID, From, message_row_to_xml(MamNs, From, HideUser, SetClientNs, Row,
                                                           QueryID))
             || Row <- MessageRows],

            %% Make fin iq
            IsLastPage = is_last_page(PageSize, TotalCount, Offset, MessageRows),
            IsStable = true,
            ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
            FinElem = make_fin_element(IQ#iq.xmlns, IsLastPage, IsStable, ResultSetEl),
            IQ#iq{type = result, sub_el = [FinElem]}
    end.

-spec handle_get_message_form(ejabberd:jid(), ejabberd:jid(), ejabberd:iq()) ->
                                     ejabberd:iq().
handle_get_message_form(_From = #jid{lserver = Host}, _ArcJID = #jid{}, IQ = #iq{}) ->
    return_message_form_iq(Host, IQ).


%% @doc Purging multiple messages.
-spec handle_purge_multiple_messages(ejabberd:jid(), ejabberd:iq()) ->
                                            ejabberd:iq() | {error, any(), ejabberd:iq()}.
handle_purge_multiple_messages(ArcJID = #jid{},
                               IQ = #iq{sub_el = PurgeEl}) ->
    Now = p1_time_compat:system_time(micro_seconds),
    {ok, Host} = mongoose_subhosts:get_host(ArcJID#jid.lserver),
    ArcID = archive_id_int(Host, ArcJID),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = mam_iq:elem_to_start_microseconds(PurgeEl),
    End = mam_iq:elem_to_end_microseconds(PurgeEl),
    %% Set borders.
    Borders = borders_decode(PurgeEl),
    %% Filtering by contact.
    With = mam_iq:elem_to_with_jid(PurgeEl),
    Res = purge_multiple_messages(Host, ArcID, ArcJID, Borders,
                                  Start, End, Now, With),
    return_purge_multiple_message_iq(IQ, Res).


-spec handle_purge_single_message(ejabberd:jid(), ejabberd:iq()) ->
                                         ejabberd:iq() | {error, any(), ejabberd:iq()}.
handle_purge_single_message(ArcJID = #jid{},
                            IQ = #iq{sub_el = PurgeEl}) ->
    Now = p1_time_compat:system_time(micro_seconds),
    {ok, Host} = mongoose_subhosts:get_host(ArcJID#jid.lserver),
    ArcID = archive_id_int(Host, ArcJID),
    BExtMessID = exml_query:attr(PurgeEl, <<"id">>, <<>>),
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    PurgingResult = purge_single_message(Host, MessID, ArcID, ArcJID, Now),
    return_purge_single_message_iq(IQ, PurgingResult).

%% ----------------------------------------------------------------------
%% Backend wrappers

-spec archive_id_int(ejabberd:server(), ejabberd:jid()) -> integer() | undefined.
archive_id_int(Host, ArcJID = #jid{}) ->
    ejabberd_hooks:run_fold(mam_muc_archive_id, Host, undefined, [Host, ArcJID]).


-spec archive_size(ejabberd:server(), mod_mam:archive_id(), ejabberd:jid())
                  -> integer().
archive_size(Host, ArcID, ArcJID = #jid{}) ->
    ejabberd_hooks:run_fold(mam_muc_archive_size, Host, 0, [Host, ArcID, ArcJID]).


-spec get_behaviour(ejabberd:server(), mod_mam:archive_id(),
                    LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(),
                    DefaultBehaviour :: 'always') -> any().
get_behaviour(Host, ArcID,
              LocJID = #jid{},
              RemJID = #jid{}, DefaultBehaviour) ->
    ejabberd_hooks:run_fold(mam_muc_get_behaviour, Host, DefaultBehaviour,
                            [Host, ArcID, LocJID, RemJID]).


-spec set_prefs(Host :: ejabberd:server(), ArcID :: mod_mam:archive_id(),
                ArcJID :: ejabberd:jid(), DefaultMode :: mod_mam:archive_behaviour(),
                AlwaysJIDs :: [ejabberd:literal_jid()],
                NeverJIDs :: [ejabberd:literal_jid()]) -> any().
set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    ejabberd_hooks:run_fold(mam_muc_set_prefs, Host, {error, not_implemented},
                            [Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs]).


%% @doc Load settings from the database.
-spec get_prefs(Host :: ejabberd:server(), ArcID :: mod_mam:archive_id(),
                ArcJID :: ejabberd:jid(), GlobalDefaultMode :: mod_mam:archive_behaviour())
               -> mod_mam:preference() | {error, Reason :: term()}.
get_prefs(Host, ArcID, ArcJID, GlobalDefaultMode) ->
    ejabberd_hooks:run_fold(mam_muc_get_prefs, Host,
                            {GlobalDefaultMode, [], []},
                            [Host, ArcID, ArcJID]).

-spec remove_archive(ejabberd:server(), mod_mam:archive_id() | undefined,
                     ejabberd:jid()) -> 'ok'.
remove_archive(Host, ArcID, ArcJID = #jid{}) ->
    ejabberd_hooks:run(mam_muc_remove_archive, Host, [Host, ArcID, ArcJID]),
    ok.


%% See description in mod_mam.
-spec lookup_messages(Host :: ejabberd:server(), Params :: map()) ->
    {ok, mod_mam:lookup_result()}
    | {error, 'policy-violation'}
    | {error, Reason :: term()}.%Result :: any(),
lookup_messages(Host, #{search_text := SearchText} = Params) ->
    case SearchText /= undefined andalso not mod_mam_utils:has_full_text_search(?MODULE, Host) of
        true -> %% Use of disabled full text search
            {error, 'not-supported'};
        false ->
            ejabberd_hooks:run_fold(mam_muc_lookup_messages, Host, {ok, {0, 0, []}},
                                    [Host, Params])
    end.


-spec archive_message(ejabberd:server(), MessId :: mod_mam:message_id(),
                      ArcId :: mod_mam:archive_id(), LocJID :: ejabberd:jid(),
                      RemJID :: ejabberd:jid(), SrcJID :: ejabberd:jid(), Dir :: 'incoming',
                      packet()) -> any().
archive_message(Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    ejabberd_hooks:run_fold(mam_muc_archive_message, Host, ok,
                            [Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet]).


-spec purge_single_message(Host :: ejabberd:server(),
                           MessID :: mod_mam:message_id(),
                           ArcID :: mod_mam:archive_id(),
                           ArcJID :: ejabberd:jid(),
                           Now :: unix_timestamp()) ->
                                  ok  | {error, 'not-found'}
                                      | {error, Reason :: term()}.
purge_single_message(Host, MessID, ArcID, ArcJID, Now) ->
    ejabberd_hooks:run_fold(mam_muc_purge_single_message, Host, ok,
                            [Host, MessID, ArcID, ArcJID, Now]).


-spec purge_multiple_messages(Host :: ejabberd:server(),
                              ArcID :: mod_mam:archive_id(), ArcJID :: ejabberd:jid(),
                              Borders :: mod_mam:borders() | undefined,
                              Start :: unix_timestamp() | undefined,
                              End :: unix_timestamp() | undefined,
                              Now :: unix_timestamp(), WithJID :: ejabberd:jid() | undefined) ->
                                     ok | {error, Reason :: term()}.
purge_multiple_messages(Host, ArcID, ArcJID, Borders,
                        Start, End, Now, WithJID) ->
    ejabberd_hooks:run_fold(mam_muc_purge_multiple_messages, Host, ok,
                            [Host, ArcID, ArcJID, Borders, Start, End, Now, WithJID]).


-spec wait_shaper(ejabberd:server(), action(), ejabberd:jid())
                 -> 'ok' | {'error', 'max_delay_reached'}.
wait_shaper(Host, Action, From) ->
    case shaper_srv:wait(Host, action_to_shaper_name(Action), From, 1) of
        ok ->
            shaper_srv:wait(Host, action_to_global_shaper_name(Action), global, 1);
        Err ->
            Err
    end.

%% ----------------------------------------------------------------------
%% Helpers

-spec message_row_to_xml(binary(), jid(), boolean(), boolean(), row(), binary() | undefined) ->
                                jlib:xmlel().
message_row_to_xml(MamNs, ReceiverJID, HideUser, SetClientNs, {MessID, SrcJID, Packet}, QueryID) ->

    {Microseconds, _NodeMessID} = decode_compact_uuid(MessID),
    DateTime = calendar:now_to_universal_time(microseconds_to_now(Microseconds)),
    BExtMessID = mess_id_to_external_binary(MessID),
    Packet1 = maybe_delete_x_user_element(HideUser, ReceiverJID, Packet),
    Packet2 = maybe_set_client_xmlns(SetClientNs, Packet1),
    Packet3 = replace_from_to_attributes(SrcJID, Packet2),
    wrap_message(MamNs, Packet3, QueryID, BExtMessID, DateTime, SrcJID).

maybe_set_client_xmlns(true, Packet) ->
    set_client_xmlns(Packet);
maybe_set_client_xmlns(false, Packet) ->
    Packet.

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
replace_from_to_attributes(SrcJID, Packet = #xmlel{attrs = Attrs}) ->
    NewAttrs = jlib:replace_from_to_attrs(jid:to_binary(SrcJID), <<>>, Attrs),
    Packet#xmlel{attrs = NewAttrs}.

-spec message_row_to_ext_id(row()) -> binary().
message_row_to_ext_id({MessID, _, _}) ->
    mess_id_to_external_binary(MessID).

set_client_xmlns(M) ->
    xml:replace_tag_attr(<<"xmlns">>, <<"jabber:client">>, M).

handle_error_iq(Host, To, Action, {error, Reason, IQ}) ->
    ejabberd_hooks:run(mam_muc_drop_iq, Host,
                       [Host, To, IQ, Action, Reason]),
    IQ;
handle_error_iq(_Host, _To, _Action, IQ) ->
    IQ.

return_error_iq(IQ, {Reason, {stacktrace, _Stacktrace}}) ->
    return_error_iq(IQ, Reason);
return_error_iq(IQ, timeout) ->
    {error, timeout, IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]}};
return_error_iq(IQ, not_implemented) ->
    {error, not_implemented, IQ#iq{type = error, sub_el = [?ERR_FEATURE_NOT_IMPLEMENTED]}};
return_error_iq(IQ, missing_with_jid) ->
    Error =  ?ERRT_BAD_REQUEST(<<"en">>,
                               <<"Limited set of queries allowed in the conversation mode.",
                                 "Missing with_jid filter">>),
    {error, bad_request, IQ#iq{type = error, sub_el = [Error]}};
return_error_iq(IQ, Reason) ->
    {error, Reason, IQ#iq{type = error, sub_el = [?ERR_INTERNAL_SERVER_ERROR]}}.

-spec return_action_not_allowed_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_action_not_allowed_error_iq(IQ) ->
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"not-allowed">>,
                                 <<"en">>, <<"The action is not allowed.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

return_purge_multiple_message_iq(IQ, ok) ->
    return_purge_success(IQ);
return_purge_multiple_message_iq(IQ, {error, Reason}) ->
    return_error_iq(IQ, Reason).

-spec return_purge_success(ejabberd:iq()) -> ejabberd:iq().
return_purge_success(IQ) ->
    IQ#iq{type = result, sub_el = []}.

-spec return_purge_not_found_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_purge_not_found_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"item-not-found">>, <<"en">>,
                                 <<"The provided UID did not match any message",
                                   "stored in archive.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.


-spec return_max_delay_reached_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_max_delay_reached_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = ?ERRT_RESOURCE_CONSTRAINT(
                 <<"en">>, <<"The action is cancelled because of flooding.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.


-spec return_purge_single_message_iq(ejabberd:iq(),
                                     ok  | {error, 'not-found'}
                                     | {error, Reason :: term()}) ->
                                            ejabberd:iq()
                                                | {error, any(), ejabberd:iq()}.
return_purge_single_message_iq(IQ, ok) ->
    return_purge_success(IQ);
return_purge_single_message_iq(IQ, {error, 'not-found'}) ->
    return_purge_not_found_error_iq(IQ);
return_purge_single_message_iq(IQ, {error, Reason}) ->
    return_error_iq(IQ, Reason).


return_message_form_iq(Host, IQ) ->
    IQ#iq{type = result, sub_el = [message_form(?MODULE, Host, IQ#iq.xmlns)]}.


report_issue({Reason, {stacktrace, Stacktrace}}, Issue, ArcJID, IQ) ->
    report_issue(Reason, Stacktrace, Issue, ArcJID, IQ);
report_issue(Reason, Issue, ArcJID, IQ) ->
    report_issue(Reason, [], Issue, ArcJID, IQ).

report_issue(timeout, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(not_implemented, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(missing_with_jid, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(Reason, Stacktrace, Issue, #jid{lserver = LServer, luser = LUser}, IQ) ->
    ?ERROR_MSG("issue=~p, server=~p, user=~p, reason=~p, iq=~p, stacktrace=~p",
               [Issue, LServer, LUser, Reason, IQ, Stacktrace]).


%% ----------------------------------------------------------------------
%% Dynamic params module

%% compile_params_module([
%%      {add_archived_element, boolean()}
%%      ])
compile_params_module(Params) ->
    CodeStr = params_helper(Params),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_mam_muc_params.erl", Code).

params_helper(Params) ->
    %% Try is_complete_message opt for backwards compatibility
    {IsArchivableModule, IsArchivableFunction} =
        case proplists:get_value(is_archivable_message, Params) of
            undefined ->
                case proplists:get_value(is_complete_message, Params) of
                    undefined -> {mod_mam_utils, is_archivable_message};
                    OldStyleMod -> {OldStyleMod, is_complete_message}
                end;

            Mod -> {Mod, is_archivable_message}
        end,

    Format =
        io_lib:format(
          "-module(mod_mam_muc_params).~n"
          "-compile(export_all).~n"
          "add_archived_element() -> ~p.~n"
          "add_stanzaid_element() -> not ~p.~n"
          "is_archivable_message(Mod, Dir, Packet) -> ~p:~p(Mod, Dir, Packet).~n"
          "params() -> ~p.~n",
          [proplists:get_bool(add_archived_element, Params),
           proplists:get_bool(no_stanzaid_element, Params),
           IsArchivableModule, IsArchivableFunction,
           Params]),
    binary_to_list(iolist_to_binary(Format)).

set_params(Params) ->
    compile_params_module(Params).

%% @doc Enable support for `<archived/>' element from MAM v0.2
-spec add_archived_element() -> boolean().
add_archived_element() ->
    mod_mam_muc_params:add_archived_element().

-spec add_stanzaid_element() -> boolean().
add_stanzaid_element() ->
    mod_mam_muc_params:add_stanzaid_element().
