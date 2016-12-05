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
%%% <li>date (using `now()');</li>
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
         room_process_mam_iq/3,
         forget_room/2]).

%% private
-export([archive_message/8]).
-export([lookup_messages/13]).
-export([archive_id_int/2]).
%% ----------------------------------------------------------------------
%% Imports

-import(mod_mam_utils,
        [maybe_microseconds/1,
         microseconds_to_now/1]).

%% UID
-import(mod_mam_utils,
        [generate_message_id/0,
         decode_compact_uuid/1]).

%% XML
-import(mod_mam_utils,
        [replace_archived_elem/3,
         replace_x_user_element/4,
         delete_x_user_element/1,
         packet_to_x_user_jid/1,
         get_one_of_path/2,
         wrap_message/6,
         result_set/4,
         result_query/2,
         result_prefs/4,
         make_fin_message/5,
         make_fin_element/4,
         parse_prefs/1,
         borders_decode/1,
         decode_optimizations/1,
         form_borders_decode/1,
         form_decode_optimizations/1]).

%% Forms
-import(mod_mam_utils,
        [form_field_value_s/2,
         message_form/1]).

%% Other
-import(mod_mam_utils,
        [maybe_integer/2,
         mess_id_to_external_binary/1,
         is_last_page/4]).

%% ejabberd
-import(mod_mam_utils,
        [send_message/3]).


-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
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
%% Constants

default_result_limit() -> 50.

max_result_limit() -> 50.

%% ----------------------------------------------------------------------
%% API

-spec delete_archive(ejabberd:server(), ejabberd:user()) -> 'ok'.
delete_archive(Server, User)
  when is_binary(Server), is_binary(User) ->
    ?DEBUG("Remove user ~p from ~p.", [User, Server]),
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    remove_archive(Host, ArcID, ArcJID),
    ok.


-spec archive_size(ejabberd:server(), ejabberd:user()) -> integer().
archive_size(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    archive_size(Host, ArcID, ArcJID).


-spec archive_id(ejabberd:server(), ejabberd:user()) -> integer().
archive_id(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    archive_id_int(Host, ArcJID).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for MUC archives

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(ServerHost, Opts) ->
    ?DEBUG("mod_mam_muc starting", []),
    compile_params_module(Opts),
    %% MUC host.
    Host = gen_mod:get_opt_host(ServerHost, Opts, <<"conference.@HOST@">>),
    start_host_mapping(Host, ServerHost),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    mod_disco:register_feature(Host, ?NS_MAM),
    mod_disco:register_feature(Host, ?NS_MAM_03),
    mod_disco:register_feature(Host, ?NS_MAM_04),
    gen_iq_handler:add_iq_handler(mod_muc_iq, Host, ?NS_MAM,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(mod_muc_iq, Host, ?NS_MAM_03,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(mod_muc_iq, Host, ?NS_MAM_04,
                                  ?MODULE, room_process_mam_iq, IQDisc),
    ejabberd_hooks:add(filter_room_packet, Host, ?MODULE,
                       filter_room_packet, 90),
    ejabberd_hooks:add(forget_room, Host, ?MODULE, forget_room, 90),
    ok.

-spec stop(Host :: ejabberd:server()) -> any().
stop(ServerHost) ->
    %% MUC host.
    Host = gen_mod:get_module_opt_host(
             ServerHost, ?MODULE, <<"conference.@HOST@">>),
    ?DEBUG("mod_mam stopping", []),
    ejabberd_hooks:delete(filter_room_packet, Host, ?MODULE, filter_room_packet, 90),
    ejabberd_hooks:delete(forget_room, Host, ?MODULE, forget_room, 90),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, Host, ?NS_MAM),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, Host, ?NS_MAM_03),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, Host, ?NS_MAM_04),
    mod_disco:unregister_feature(Host, ?NS_MAM),
    mod_disco:unregister_feature(Host, ?NS_MAM_03),
    mod_disco:unregister_feature(Host, ?NS_MAM_04),
    stop_host_mapping(Host, ServerHost),
    ok.

%% ----------------------------------------------------------------------
%% Host to ServerHost mapping

-record(mam_host, {host :: ejabberd:server(),
                   server_host :: ejabberd:server()}).

-spec start_host_mapping(ejabberd:server(), ejabberd:server()) -> any().
start_host_mapping(Host, ServerHost) ->
    mnesia:create_table(mam_host,
                        [{ram_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, mam_host)}]),
    mnesia:add_table_copy(mam_host, node(), ram_copies),
    mnesia:dirty_write(#mam_host{host = Host, server_host = ServerHost}).


-spec stop_host_mapping(ejabberd:server(), ejabberd:server()) -> any().
stop_host_mapping(Host, ServerHost) ->
    mnesia:dirty_delete_object(
      #mam_host{host = Host, server_host = ServerHost}).


-spec server_host(ejabberd:jid()) -> ejabberd:server().
server_host(#jid{lserver = Host}) ->
    server_host1(Host, mnesia:dirty_read(mam_host, Host)).

server_host1(_Host, [#mam_host{server_host = ServerHost}]) ->
    ServerHost.

%% ----------------------------------------------------------------------
%% hooks and handlers for MUC

%% @doc Handle public MUC-message.
-spec filter_room_packet(Packet :: packet(), EventData :: list()) -> packet().
filter_room_packet(Packet, EventData) ->
    ?DEBUG("Incoming room packet.", []),
    IsComplete = call_is_complete_message(?MODULE, incoming, Packet),
    case IsComplete of
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
archive_room_packet(Packet, FromNick, FromJID = #jid{}, RoomJID = #jid{}, Role, Affiliation) ->
    Host = server_host(RoomJID),
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
            case {Result, add_archived_element()} of
                {ok, true} ->
                    BareRoomJID = jid:to_binary(RoomJID),
                    replace_archived_elem(BareRoomJID,
                                          mess_id_to_external_binary(MessID),
                                          Packet);
                {ok, false} -> Packet;
                {{error, _}, _} -> Packet
            end;
        false -> Packet
    end.


%% @doc `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec room_process_mam_iq(From :: ejabberd:jid(), To :: ejabberd:jid(),
                          IQ :: ejabberd:iq()) -> ejabberd:iq() | 'ignore'.
room_process_mam_iq(From = #jid{lserver = Host}, To, IQ) ->
    Action = iq_action(IQ),
    case is_action_allowed(Action, From, To) of
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
    end.


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


-spec is_room_owner(From :: ejabberd:jid(), To :: ejabberd:jid()) -> boolean().
is_room_owner(From, To) ->
    ejabberd_hooks:run_fold(is_muc_room_owner, To#jid.lserver, false, [To, From]).


%% @doc Return true if user element should be removed from results
-spec is_user_identity_hidden(From :: ejabberd:jid(), ArcJID :: ejabberd:jid()) -> boolean().
is_user_identity_hidden(From, ArcJID) ->
    case mod_muc_room:can_access_identity(ArcJID, From) of
        {error, _} -> true;
        {ok, CanAccess} -> (not CanAccess)
    end.

-spec can_access_room(From :: ejabberd:jid(), To :: ejabberd:jid()) -> boolean().
can_access_room(From, To) ->
    ejabberd_hooks:run_fold(can_access_room, To#jid.lserver, false, [From, To]).


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
    iq_action03(IQ).

iq_action02(#iq{type = Action, sub_el = SubEl = #xmlel{name = Category}}) ->
    case {Action, Category} of
        {set, <<"prefs">>} -> mam_set_prefs;
        {get, <<"prefs">>} -> mam_get_prefs;
        {get, <<"query">>} -> mam_lookup_messages;
        {set, <<"purge">>} ->
            case xml:get_tag_attr_s(<<"id">>, SubEl) of
                <<>> -> mam_purge_multiple_messages;
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
    Host = server_host(ArcJID),
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
handle_get_prefs(ArcJID = #jid{}, IQ = #iq{}) ->
    Host = server_host(ArcJID),
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
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = xml:get_tag_attr_s(<<"queryid">>, QueryEl),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = elem_to_start_microseconds(QueryEl),
    End = elem_to_end_microseconds(QueryEl),
    %% Filtering by contact.
    With = elem_to_with_jid(QueryEl),
    RSM = fix_rsm(jlib:rsm_decode(QueryEl)),
    Borders = borders_decode(QueryEl),
    Limit = elem_to_limit(QueryEl),
    PageSize = min(max_result_limit(),
                   maybe_integer(Limit, default_result_limit())),
    LimitPassed = Limit =/= <<>>,
    IsSimple = decode_optimizations(QueryEl),
    case lookup_messages(Host, ArcID, ArcJID, RSM, Borders,
                         Start, End, Now, With,
                         PageSize, LimitPassed, max_result_limit(), IsSimple) of
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
handle_set_message_form(
  From = #jid{},
  ArcJID = #jid{},
  IQ = #iq{xmlns = MamNs, sub_el = QueryEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = xml:get_tag_attr_s(<<"queryid">>, QueryEl),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = form_to_start_microseconds(QueryEl),
    End = form_to_end_microseconds(QueryEl),
    %% Filtering by contact.
    With = form_to_with_jid(QueryEl),
    RSM = fix_rsm(jlib:rsm_decode(QueryEl)),
    Borders = form_borders_decode(QueryEl),
    Limit = elem_to_limit(QueryEl),
    PageSize = min(max_result_limit(),
                   maybe_integer(Limit, default_result_limit())),
    %% Whether or not the client query included a <set/> element,
    %% the server MAY simply return its limited results.
    %% So, disable 'policy-violation'.
    LimitPassed = true,
    IsSimple = form_decode_optimizations(QueryEl),

    case lookup_messages(Host, ArcID, ArcJID, RSM, Borders,
                         Start, End, Now, With,
                         PageSize, LimitPassed, max_result_limit(), IsSimple) of
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
handle_get_message_form(_From = #jid{}, _ArcJID = #jid{}, IQ = #iq{}) ->
    return_message_form_iq(IQ).


%% @doc Purging multiple messages.
-spec handle_purge_multiple_messages(ejabberd:jid(), ejabberd:iq()) ->
                                            ejabberd:iq() | {error, any(), ejabberd:iq()}.
handle_purge_multiple_messages(ArcJID = #jid{},
                               IQ = #iq{sub_el = PurgeEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = elem_to_start_microseconds(PurgeEl),
    End = elem_to_end_microseconds(PurgeEl),
    %% Set borders.
    Borders = borders_decode(PurgeEl),
    %% Filtering by contact.
    With = elem_to_with_jid(PurgeEl),
    Res = purge_multiple_messages(Host, ArcID, ArcJID, Borders,
                                  Start, End, Now, With),
    return_purge_multiple_message_iq(IQ, Res).


-spec handle_purge_single_message(ejabberd:jid(), ejabberd:iq()) ->
                                         ejabberd:iq() | {error, any(), ejabberd:iq()}.
handle_purge_single_message(ArcJID = #jid{},
                            IQ = #iq{sub_el = PurgeEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    BExtMessID = xml:get_tag_attr_s(<<"id">>, PurgeEl),
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
-spec lookup_messages(Host :: ejabberd:server(),
                      ArchiveID :: mod_mam:archive_id(),
                      ArchiveJID :: ejabberd:jid(),
                      RSM :: jlib:rsm_in()  | undefined,
                      Borders :: mod_mam:borders()  | undefined,
                      Start :: mod_mam:unix_timestamp()  | undefined,
                      End :: mod_mam:unix_timestamp()  | undefined,
                      Now :: mod_mam:unix_timestamp(),
                      WithJID :: ejabberd:jid()  | undefined,
                      PageSize :: non_neg_integer(), LimitPassed :: boolean(),
                      MaxResultLimit :: non_neg_integer(),
                      IsSimple :: boolean()  | opt_count) ->
                             {ok, mod_mam:lookup_result()}
                                 | {error, 'policy-violation'}
                                 | {error, Reason :: term()}.%Result :: any(),
lookup_messages(Host, ArcID, ArcJID, RSM, Borders, Start, End, Now,
                WithJID, PageSize, LimitPassed, MaxResultLimit, IsSimple) ->
    ejabberd_hooks:run_fold(mam_muc_lookup_messages, Host, {ok, {0, 0, []}},
                            [Host, ArcID, ArcJID, RSM, Borders,
                             Start, End, Now, WithJID,
                             PageSize, LimitPassed, MaxResultLimit, IsSimple]).


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


-spec maybe_jid(ejabberd:literal_jid()) -> 'error' | 'undefined' | ejabberd:jid().
maybe_jid(<<>>) ->
    undefined;
maybe_jid(JID) when is_binary(JID) ->
    jid:from_binary(JID).


%% @doc Convert id into internal format.
-spec fix_rsm('none' | jlib:rsm_in()) -> 'undefined' | jlib:rsm_in().
fix_rsm(none) ->
    undefined;
fix_rsm(RSM = #rsm_in{direction = aft, id = <<>>}) ->
    RSM#rsm_in{direction = undefined, id = undefined}; %% First page
fix_rsm(RSM = #rsm_in{direction = aft, id = undefined}) ->
    RSM#rsm_in{direction = undefined}; %% First page
fix_rsm(RSM = #rsm_in{id = undefined}) ->
    RSM;
fix_rsm(RSM = #rsm_in{id = <<>>}) ->
    RSM#rsm_in{id = undefined};
fix_rsm(RSM = #rsm_in{id = BExtMessID}) when is_binary(BExtMessID) ->
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    RSM#rsm_in{id = MessID}.


-spec elem_to_start_microseconds(jlib:xmlel()) -> 'undefined' | non_neg_integer().
elem_to_start_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"start">>}, cdata])).


-spec elem_to_end_microseconds(jlib:xmlel()) -> 'undefined' | non_neg_integer().
elem_to_end_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"end">>}, cdata])).


-spec elem_to_with_jid(jlib:xmlel()) -> 'error' | 'undefined' | ejabberd:jid().
elem_to_with_jid(El) ->
    maybe_jid(xml:get_path_s(El, [{elem, <<"with">>}, cdata])).


%% @doc This element's name is "limit". But it must be "max" according XEP-0313.
-spec elem_to_limit(jlib:xmlel()) -> any().
elem_to_limit(QueryEl) ->
    get_one_of_path(QueryEl, [
                              [{elem, <<"set">>}, {elem, <<"max">>}, cdata],
                              [{elem, <<"set">>}, {elem, <<"limit">>}, cdata]
                             ]).

-spec form_to_start_microseconds(_) -> 'undefined' | non_neg_integer().
form_to_start_microseconds(El) ->
    maybe_microseconds(form_field_value_s(El, <<"start">>)).


-spec form_to_end_microseconds(_) -> 'undefined' | non_neg_integer().
form_to_end_microseconds(El) ->
    maybe_microseconds(form_field_value_s(El, <<"end">>)).


-spec form_to_with_jid(jlib:xmlel()) -> 'error' | 'undefined' | ejabberd:jid().
form_to_with_jid(El) ->
    maybe_jid(form_field_value_s(El, <<"with">>)).


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


return_message_form_iq(IQ) ->
    IQ#iq{type = result, sub_el = [message_form(IQ#iq.xmlns)]}.


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
    binary_to_list(iolist_to_binary(io_lib:format(
                                      "-module(mod_mam_muc_params).~n"
                                      "-compile(export_all).~n"
                                      "add_archived_element() -> ~p.~n"
                                      "is_complete_message() -> ~p.~n",
                                      [proplists:get_bool(add_archived_element, Params),
                                       proplists:get_value(is_complete_message, Params,
                                                           mod_mam_utils)]))).

%% @doc Enable support for `<archived/>' element from MAM v0.2
-spec add_archived_element() -> boolean().
add_archived_element() ->
    mod_mam_muc_params:add_archived_element().

call_is_complete_message(Module, Dir, Packet) ->
    M = mod_mam_muc_params:is_complete_message(),
    M:is_complete_message(Module, Dir, Packet).

