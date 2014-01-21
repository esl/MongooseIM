%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc XEP-0313: Message Archive Management
%%%
%%% The module uses several backend modules:
%%%
%%% <ul>
%%% <li>Preference manager ({@link mod_mam_muc_odbc_prefs});</li>
%%% <li>Writer ({@link mod_mam_muc_odbc_arch} or {@link mod_mam_muc_odbc_async_writer});</li>
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

%% ----------------------------------------------------------------------
%% Exports

%% Client API
-export([delete_archive/2,
         archive_size/2,
         archive_id/2]).

%% Utils
-export([create_dump_file/2,
         restore_dump_file/3]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd room handlers
-export([filter_room_packet/4,
         archive_room_packet/4,
         room_process_mam_iq/3,
         forget_room/2]).

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
         get_one_of_path/2,
         is_complete_message/3,
         wrap_message/5,
         result_set/4,
         result_query/1,
         result_prefs/3,
         parse_prefs/1,
         borders_decode/1,
         decode_optimizations/1]).

%% Other
-import(mod_mam_utils,
        [maybe_integer/2,
         is_function_exist/3,
         mess_id_to_external_binary/1]).

%% ejabberd
-import(mod_mam_utils,
        [send_message/3]).


-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

%% ----------------------------------------------------------------------
%% Datetime types
%% Microseconds from 01.01.1970
-type unix_timestamp() :: non_neg_integer().

%% ----------------------------------------------------------------------
%% XMPP types
-type server_host() :: binary().
-type literal_jid() :: binary().


%% ----------------------------------------------------------------------
%% Other types
-type archive_behaviour() :: atom(). % roster | always | never.
-type message_id() :: non_neg_integer().
-type archive_id() :: non_neg_integer().
-type action() :: atom().

%% ----------------------------------------------------------------------
%% Constants

mam_ns_string() -> "urn:xmpp:mam:tmp".

mam_ns_binary() -> <<"urn:xmpp:mam:tmp">>.

default_result_limit() -> 50.

max_result_limit() -> 50.

%% ----------------------------------------------------------------------
%% API

delete_archive(Server, User)
    when is_binary(Server), is_binary(User) ->
    ?DEBUG("Remove user ~p from ~p.", [User, Server]),
    ArcJID = jlib:make_jid(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    remove_archive(Host, ArcID, ArcJID),
    ok.

archive_size(Server, User)
    when is_binary(Server), is_binary(User) ->
    ArcJID = jlib:make_jid(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    archive_size(Host, ArcID, ArcJID).

archive_id(Server, User)
    when is_binary(Server), is_binary(User) ->
    ArcJID = jlib:make_jid(User, Server, <<>>),
    Host = server_host(ArcJID),
    archive_id_int(Host, ArcJID).

%% ----------------------------------------------------------------------
%% Utils API

new_iterator(ArcJID=#jid{}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    new_iterator(Host, ArcID, ArcJID, undefined,
        undefined, undefined, undefined, Now, undefined, 50).

new_iterator(Host, ArcID, ArcJID, RSM, Borders,
             Start, End, Now, WithJID, PageSize) ->
    fun() ->
        {ok, {TotalCount, Offset, MessageRows}} =
        lookup_messages(Host, ArcID, ArcJID, RSM, Borders,
                        Start, End, Now,
                        WithJID, PageSize, true, PageSize, false),
        Data = [exml:to_iolist(message_row_to_dump_xml(M))
                || M <- MessageRows],
        Cont = case is_last_page(TotalCount, Offset, PageSize) of
            false ->
                fun() -> {error, eof} end;
            true ->
                new_iterator(
                    Host, ArcID, ArcJID, after_rsm(MessageRows), Borders,
                    Start, End, Now, WithJID, PageSize)
            end,
        {ok, {Data, Cont}}
        end.

after_rsm(MessageRows) ->
    {MessID,_SrcJID,_Packet} = lists:last(MessageRows),
    #rsm_in{direction = aft, id = MessID}.

is_last_page(TotalCount, Offset, PageSize) ->
    Offset - PageSize >= TotalCount.


create_dump_file(ArcJID, OutFileName) ->
    mod_mam_dump:create_dump_file(new_iterator(ArcJID), OutFileName).

-spec restore_dump_file(ArcJID, InFileName, Opts) -> ok | {error, Reason} when
    ArcJID :: #jid{},
    InFileName :: file:filename(),
    Opts :: [Opt],
    Opt :: {rewrite_jids, RewriterF | Substitutions} | new_message_ids,
    RewriterF :: fun((BinJID) -> BinJID),
    Substitutions :: [{BinJID, BinJID}],
    BinJID :: binary(),
    Reason :: term().
restore_dump_file(ArcJID, InFileName, Opts) ->
    try
        restore_dump_file_unsave(ArcJID, InFileName, Opts)
    catch Type:Reason ->
        Trace = erlang:get_stacktrace(),
        lager:error("Error ~p:~p occured while restoring ~p from file ~ts.~nTrace: ~p",
                    [Type, Reason, jlib:jid_to_binary(ArcJID), InFileName, Trace]),
        {error, Reason}
    end.

restore_dump_file_unsave(ArcJID, InFileName, Opts) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    WriterF = fun(MessID, FromJID, _ToJID, MessElem) ->
            archive_message(Host, MessID, ArcID, ArcJID,
                            FromJID, FromJID, incoming, MessElem)
        end,
    mod_mam_dump:restore_dump_file(WriterF, InFileName, Opts).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for MUC archives

start(ServerHost, Opts) ->
    %% MUC host.
    Host = gen_mod:get_opt_host(ServerHost, Opts, <<"conference.@HOST@">>),
    start_host_mapping(Host, ServerHost),
    ?DEBUG("mod_mam_muc starting", []),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    mod_disco:register_feature(Host, mam_ns_binary()),
    gen_iq_handler:add_iq_handler(mod_muc_iq, Host, mam_ns_binary(),
                                  ?MODULE, room_process_mam_iq, IQDisc),
    ejabberd_hooks:add(filter_room_packet, Host, ?MODULE,
                       filter_room_packet, 90),
    ejabberd_hooks:add(archive_room_packet, Host, ?MODULE,
                       archive_room_packet, 90),
    ejabberd_hooks:add(forget_room, Host, ?MODULE, forget_room, 90),
    ok.

stop(ServerHost) ->
    %% MUC host.
    Host = gen_mod:get_module_opt_host(
        ServerHost, ?MODULE, <<"conference.@HOST@">>),
    ?DEBUG("mod_mam stopping", []),
    ejabberd_hooks:delete(filter_room_packet, Host, ?MODULE, filter_room_packet, 90),
    ejabberd_hooks:delete(archive_room_packet, Host, ?MODULE, archive_room_packet, 90),
    ejabberd_hooks:delete(forget_room, Host, ?MODULE, forget_room, 90),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, Host, mam_ns_string()),
    mod_disco:unregister_feature(Host, mam_ns_binary()),
    stop_host_mapping(Host, ServerHost),
    ok.

%% ----------------------------------------------------------------------
%% Host to ServerHost mapping

-record(mam_host, {host, server_host}).

start_host_mapping(Host, ServerHost) ->
    mnesia:create_table(mam_host,
            [{ram_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, mam_host)}]),
    mnesia:dirty_write(#mam_host{host = Host, server_host = ServerHost}).

stop_host_mapping(Host, ServerHost) ->
    mnesia:dirty_delete_object(
        #mam_host{host = Host, server_host = ServerHost}).

server_host(#jid{lserver=Host}) ->
    [#mam_host{
        server_host = ServerHost
    }] = mnesia:dirty_read(mam_host, Host),
    ServerHost.

%% ----------------------------------------------------------------------
%% hooks and handlers for MUC

%% @doc Handle public MUC-message.
-spec filter_room_packet(Packet, FromNick, FromJID, RoomJID) -> Packet when
    Packet :: term(),
    FromNick :: binary(),
    RoomJID :: #jid{},
    FromJID :: #jid{}.
filter_room_packet(Packet, FromNick,
                   FromJID=#jid{},
                   RoomJID=#jid{}) ->
    ?DEBUG("Incoming room packet.", []),
    IsComplete = is_complete_message(?MODULE, incoming, Packet),
    case IsComplete of
        true -> archive_room_packet(Packet, FromNick, FromJID, RoomJID);
        false -> Packet
    end.

%% @doc Archive without validation.
-spec archive_room_packet(Packet, FromNick, FromJID, RoomJID) -> Packet when
    Packet :: term(),
    FromNick :: binary(),
    RoomJID :: #jid{},
    FromJID :: #jid{}.
archive_room_packet(Packet, FromNick,
                    FromJID=#jid{},
                    RoomJID=#jid{}) ->
    Host = server_host(RoomJID),
    ArcID = archive_id_int(Host, RoomJID),
    %% Occupant JID <room@service/nick>
    SrcJID = jlib:jid_replace_resource(RoomJID, FromNick),
    IsInteresting =
    case get_behaviour(Host, ArcID, RoomJID, SrcJID, always) of
        always -> true;
        never  -> false;
        roster -> true
    end,
    case IsInteresting of
        true -> 
            MessID = generate_message_id(),
            Result = archive_message(Host, MessID, ArcID,
                                     RoomJID, FromJID, SrcJID, incoming, Packet),
            case Result of
                ok ->
                    BareRoomJID = jlib:jid_to_binary(RoomJID),
                    replace_archived_elem(BareRoomJID,
                                          mess_id_to_external_binary(MessID),
                                          Packet);
                {error, _} -> Packet
            end;
        false -> Packet
    end.

%% `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives 
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec room_process_mam_iq(From, To, IQ) -> IQ | ignore when
    From :: jid(),
    To :: jid(),
    IQ :: #iq{}.
room_process_mam_iq(From=#jid{lserver=Host}, To, IQ) ->
    Action = iq_action(IQ),
    case is_action_allowed(Action, From, To) of
        true  -> 
            case wait_shaper(Host, Action, From) of
                ok ->
                    handle_mam_iq(Action, From, To, IQ);
                {error, max_delay_reached} ->
                    return_max_delay_reached_error_iq(IQ)
            end;
        false -> return_action_not_allowed_error_iq(IQ)
    end.

%% This hook is called from `mod_muc:forget_room(Host, Name)'.
forget_room(LServer, RoomName) ->
    delete_archive(LServer, RoomName).

%% ----------------------------------------------------------------------
%% Internal functions

is_action_allowed(Action, From, To=#jid{lserver=Host}) ->
    case acl:match_rule(Host, Action, From) of
        allow   -> true;
        deny    -> false;
        default -> is_action_allowed_by_default(Action, From, To)
    end.

-spec is_action_allowed_by_default(Action, From, To) -> boolean() when
    Action  :: action(),
    From    :: jid(),
    To      :: jid().
is_action_allowed_by_default(Action, From, To) ->
    is_room_action_allowed_by_default(Action, From, To).

is_room_action_allowed_by_default(Action, From, To) ->
    case action_type(Action) of
        set -> is_room_owner(From, To);
        get -> true
    end.

is_room_owner(From, To) ->
    case mod_mam_room:is_room_owner(To, From) of
        {error, _} -> false;
        {ok, IsOwner} -> IsOwner
    end.

action_type(mam_get_prefs)                  -> get;
action_type(mam_set_prefs)                  -> set;
action_type(mam_lookup_messages)            -> get;
action_type(mam_purge_single_message)       -> set;
action_type(mam_purge_multiple_messages)    -> set.

-spec action_to_shaper_name(action()) -> atom().
action_to_shaper_name(Action) ->
    list_to_atom(atom_to_list(Action) ++ "_shaper").

-spec action_to_global_shaper_name(action()) -> atom().
action_to_global_shaper_name(Action) -> list_to_atom(atom_to_list(Action) ++ "_global_shaper").

handle_mam_iq(Action, From, To, IQ) ->
    case Action of
    mam_get_prefs ->
        handle_get_prefs(To, IQ);
    mam_set_prefs ->
        handle_set_prefs(To, IQ);
    mam_lookup_messages ->
        handle_lookup_messages(From, To, IQ);
    mam_purge_single_message ->
        handle_purge_single_message(To, IQ);
    mam_purge_multiple_messages ->
        handle_purge_multiple_messages(To, IQ)
    end.

iq_action(#iq{type = Action, sub_el = SubEl = #xmlel{name = Category}}) ->
    case {Action, Category} of
        {set, <<"prefs">>} -> mam_set_prefs;
        {get, <<"prefs">>} -> mam_get_prefs;
        {get, <<"query">>} -> mam_lookup_messages;
        {set, <<"purge">>} ->
            case xml:get_tag_attr_s(<<"id">>, SubEl) of
                <<>> -> mam_purge_multiple_messages;
                _    -> mam_purge_single_message
            end
    end.

handle_set_prefs(ArcJID=#jid{},
                 IQ=#iq{sub_el = PrefsEl}) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} = parse_prefs(PrefsEl),
    ?DEBUG("Parsed data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
              [DefaultMode, AlwaysJIDs, NeverJIDs]),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs),
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]}.

handle_get_prefs(ArcJID=#jid{}, IQ=#iq{}) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    {DefaultMode, AlwaysJIDs, NeverJIDs} =
        get_prefs(Host, ArcID, ArcJID, always),
    ?DEBUG("Extracted data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
              [DefaultMode, AlwaysJIDs, NeverJIDs]),
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]}.
    
handle_lookup_messages(
        From=#jid{},
        ArcJID=#jid{},
        IQ=#iq{sub_el = QueryEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = xml:get_tag_attr_s(<<"queryid">>, QueryEl),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = elem_to_start_microseconds(QueryEl),
    End   = elem_to_end_microseconds(QueryEl),
    %% Filtering by contact.
    With  = elem_to_with_jid(QueryEl),
    RSM   = fix_rsm(jlib:rsm_decode(QueryEl)),
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
        ?DEBUG("Policy violation by ~p.", [jlib:jid_to_binary(From)]),
        ErrorEl = ?STANZA_ERRORT(<<"">>, <<"modify">>, <<"policy-violation">>,
                                 <<"en">>, <<"Too many results">>),          
        IQ#iq{type = error, sub_el = [ErrorEl]};
    {ok, {TotalCount, Offset, MessageRows}} ->
        {FirstMessID, LastMessID} =
            case MessageRows of
                []    -> {undefined, undefined};
                [_|_] -> {message_row_to_ext_id(hd(MessageRows)),
                          message_row_to_ext_id(lists:last(MessageRows))}
            end,
        [send_message(ArcJID, From, message_row_to_xml(M, QueryID))
         || M <- MessageRows],
        ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
        ResultQueryEl = result_query(ResultSetEl),
        %% On receiving the query, the server pushes to the client a series of
        %% messages from the archive that match the client's given criteria,
        %% and finally returns the <iq/> result.
        IQ#iq{type = result, sub_el = [ResultQueryEl]}
    end.

%% Purging multiple messages.
handle_purge_multiple_messages(ArcJID=#jid{},
                               IQ=#iq{sub_el = PurgeEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = elem_to_start_microseconds(PurgeEl),
    End   = elem_to_end_microseconds(PurgeEl),
    %% Set borders.
    Borders = borders_decode(PurgeEl),
    %% Filtering by contact.
    With  = elem_to_with_jid(PurgeEl),
    purge_multiple_messages(Host, ArcID, ArcJID, Borders, Start, End, Now, With),
    return_purge_success(IQ).

handle_purge_single_message(ArcJID=#jid{},
                            IQ=#iq{sub_el = PurgeEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    BExtMessID = xml:get_tag_attr_s(<<"id">>, PurgeEl),
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    PurgingResult = purge_single_message(Host, MessID, ArcID, ArcJID, Now),
    return_purge_single_message_iq(IQ, PurgingResult).

%% ----------------------------------------------------------------------
%% Backend wrappers

archive_id_int(Host, ArcJID=#jid{}) ->
    ejabberd_hooks:run_fold(mam_muc_archive_id, Host, undefined, [Host, ArcJID]).

archive_size(Host, ArcID, ArcJID=#jid{}) ->
    ejabberd_hooks:run_fold(mam_muc_archive_size, Host, 0, [Host, ArcID, ArcJID]).

get_behaviour(Host, ArcID,
              LocJID=#jid{},
              RemJID=#jid{}, DefaultBehaviour) ->
    ejabberd_hooks:run_fold(mam_muc_get_behaviour, Host, DefaultBehaviour,
        [Host, ArcID, LocJID, RemJID]).

set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    ejabberd_hooks:run_fold(mam_muc_set_prefs, Host, ok,
        [Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs]).

%% @doc Load settings from the database.
-spec get_prefs(Host, ArcID, ArcJID, GlobalDefaultMode) -> Result when
    Host        :: server_host(),
    ArcID       :: archive_id(),
    ArcJID      :: jid(),
    DefaultMode :: archive_behaviour(),
    GlobalDefaultMode :: archive_behaviour(),
    Result      :: {DefaultMode, AlwaysJIDs, NeverJIDs},
    AlwaysJIDs  :: [literal_jid()],
    NeverJIDs   :: [literal_jid()].
get_prefs(Host, ArcID, ArcJID, GlobalDefaultMode) ->
    ejabberd_hooks:run_fold(mam_muc_get_prefs, Host,
        {GlobalDefaultMode, [], []},
        [Host, ArcID, ArcJID]).

remove_archive(Host, ArcID, ArcJID=#jid{}) ->
    ejabberd_hooks:run(mam_muc_remove_archive, Host, [Host, ArcID, ArcJID]),
    ok.

-spec lookup_messages(Host, ArcID, ArcJID, RSM, Borders,
                      Start, End, Now, WithJID,
                      PageSize, LimitPassed, MaxResultLimit, IsSimple) ->
    {ok, {TotalCount, Offset, MessageRows}} | {error, 'policy-violation'}
    when
    Host    :: server_host(),
    ArcID   :: archive_id(),
    ArcJID  :: #jid{},
    RSM     :: #rsm_in{} | undefined,
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: #jid{} | undefined,
    PageSize :: non_neg_integer(),
    LimitPassed :: boolean(),
    MaxResultLimit :: non_neg_integer(),
    IsSimple :: boolean() | opt_count,
    TotalCount :: non_neg_integer(),
    Offset  :: non_neg_integer(),
    MessageRows :: list(tuple()).
lookup_messages(Host, ArcID, ArcJID, RSM, Borders, Start, End, Now,
                WithJID, PageSize, LimitPassed, MaxResultLimit, IsSimple) ->
    ejabberd_hooks:run_fold(mam_muc_lookup_messages, Host, {ok, {0, 0, []}},
        [Host, ArcID, ArcJID, RSM, Borders,
         Start, End, Now, WithJID,
         PageSize, LimitPassed, MaxResultLimit, IsSimple]).

archive_message(Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    ejabberd_hooks:run_fold(mam_muc_archive_message, Host, ok,
        [Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet]).

-spec purge_single_message(Host, MessID, ArcID, ArcJID, Now) ->
    ok | {error, 'not-found'} when
    Host   :: server_host(),
    MessID :: message_id(),
    ArcID  :: archive_id(),
    ArcJID :: jid(),
    Now :: unix_timestamp().
purge_single_message(Host, MessID, ArcID, ArcJID, Now) ->
    ejabberd_hooks:run_fold(mam_muc_purge_single_message, Host, ok,
        [Host, MessID, ArcID, ArcJID, Now]).

-spec purge_multiple_messages(Host, ArcID, ArcJID, Borders,
                              Start, End, Now, WithJID) -> ok
    when
    Host    :: server_host(),
    ArcID   :: archive_id(),
    ArcJID  :: jid(),
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: jid() | undefined.
purge_multiple_messages(Host, ArcID, ArcJID, Borders,
                        Start, End, Now, WithJID) ->
    ejabberd_hooks:run_fold(mam_muc_purge_multiple_messages, Host, ok,
        [Host, ArcID, ArcJID, Borders, Start, End, Now, WithJID]).

wait_shaper(Host, Action, From) ->
    case shaper_srv:wait(Host, action_to_shaper_name(Action), From, 1) of
        ok ->
            shaper_srv:wait(Host, action_to_global_shaper_name(Action), global, 1);
        Err ->
            Err
    end.

%% ----------------------------------------------------------------------
%% Helpers

message_row_to_xml({MessID,SrcJID,Packet}, QueryID) ->
    {Microseconds, _NodeMessID} = decode_compact_uuid(MessID),
    DateTime = calendar:now_to_universal_time(microseconds_to_now(Microseconds)),
    BExtMessID = mess_id_to_external_binary(MessID),
    wrap_message(Packet, QueryID, BExtMessID, DateTime, SrcJID).

message_row_to_ext_id({MessID,_,_}) ->
    mess_id_to_external_binary(MessID).

message_row_to_dump_xml(M) ->
     xml:get_subtag(message_row_to_xml(M, undefined), <<"result">>).

maybe_jid(<<>>) ->
    undefined;
maybe_jid(JID) when is_binary(JID) ->
    jlib:binary_to_jid(JID).

%% @doc Convert id into internal format.
fix_rsm(none) ->
    undefined;
fix_rsm(RSM=#rsm_in{id = undefined}) ->
    RSM;
fix_rsm(RSM=#rsm_in{id = <<>>}) ->
    RSM#rsm_in{id = undefined};
fix_rsm(RSM=#rsm_in{id = BExtMessID}) when is_binary(BExtMessID) ->
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    RSM#rsm_in{id = MessID}.

elem_to_start_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"start">>}, cdata])).

elem_to_end_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"start">>}, cdata])).

elem_to_with_jid(El) ->
    maybe_jid(xml:get_path_s(El, [{elem, <<"with">>}, cdata])).

%% This element's name is "limit".
%% But it must be "max" according XEP-0313.
elem_to_limit(QueryEl) ->
    get_one_of_path(QueryEl, [
        [{elem, <<"set">>}, {elem, <<"max">>}, cdata],
        [{elem, <<"set">>}, {elem, <<"limit">>}, cdata]
    ]).

return_purge_success(IQ) ->
    IQ#iq{type = result, sub_el = []}.

return_action_not_allowed_error_iq(IQ) ->
    ErrorEl = ?STANZA_ERRORT(<<"">>, <<"cancel">>, <<"not-allowed">>,
         <<"en">>, <<"The action is not allowed.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

return_purge_not_found_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = ?STANZA_ERRORT(<<"">>, <<"cancel">>, <<"item-not-found">>,
         <<"en">>, <<"The provided UID did not match any message stored in archive.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

return_max_delay_reached_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = ?ERRT_RESOURCE_CONSTRAINT(
        <<"en">>, <<"The action is cancelled because of flooding.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

return_purge_single_message_iq(IQ, ok) ->
    return_purge_success(IQ);
return_purge_single_message_iq(IQ, {error, 'not-found'}) ->
    return_purge_not_found_error_iq(IQ).
