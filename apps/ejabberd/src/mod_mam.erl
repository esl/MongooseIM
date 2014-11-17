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
%%% This module handles simple archives.
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
-module(mod_mam).
-behavior(gen_mod).

%% ----------------------------------------------------------------------
%% Exports

%% Client API
-export([delete_archive/2,
         archive_size/2,
         archive_id/2]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([process_mam_iq/3,
         user_send_packet/3,
         remove_user/2,
         filter_packet/1]).

%% Utils
-export([create_dump_file/2,
         restore_dump_file/3]).

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
        [send_message/3,
         is_jid_in_user_roster/2]).


-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

%% ----------------------------------------------------------------------
%% Datetime types
%% Microseconds from 01.01.1970
-type unix_timestamp() :: non_neg_integer().

%% ----------------------------------------------------------------------
%% Other types
-type archive_behaviour()   :: atom(). % roster | always | never.
-type message_id()          :: non_neg_integer().

-type archive_id()          :: non_neg_integer().

-type action()              :: atom().
-type borders()             :: #mam_borders{}.
-type lookup_result() :: {TotalCount :: non_neg_integer(),
                          Offset :: non_neg_integer(),
                          MessageRows :: list(tuple())}.

%% Internal types
-type iterator_fun() :: fun(() -> {'ok',{_,_}}).
-type rewriter_fun() :: fun((JID :: ejabberd:literal_jid())
                                                    -> ejabberd:literal_jid()).
-type restore_option() :: {rewrite_jids, rewriter_fun() | [{binary(), binary()}]}
                        | new_message_ids.

-type preference() :: {DefaultMode :: archive_behaviour(),
                       AlwaysJIDs  :: [ejabberd:literal_jid()],
                       NeverJIDs   :: [ejabberd:literal_jid()]}.
-export_type([rewriter_fun/0,
              borders/0,
              preference/0,
              archive_behaviour/0,
              iterator_fun/0,
              unix_timestamp/0,
              archive_id/0,
              lookup_result/0
            ]).

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
    ArcJID = jlib:make_jid(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    remove_archive(Host, ArcID, ArcJID),
    ok.


-spec archive_size(ejabberd:server(), ejabberd:user()) -> integer().
archive_size(Server, User)
    when is_binary(Server), is_binary(User) ->
    ArcJID = jlib:make_jid(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    archive_size(Host, ArcID, ArcJID).


-spec archive_id(ejabberd:server(), ejabberd:user()) -> integer().
archive_id(Server, User)
    when is_binary(Server), is_binary(User) ->
    ArcJID = jlib:make_jid(User, Server, <<>>),
    Host = server_host(ArcJID),
    archive_id_int(Host, ArcJID).


%% ----------------------------------------------------------------------
%% Utils API

-spec new_iterator(ejabberd:jid()) -> iterator_fun().
new_iterator(ArcJID=#jid{}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    new_iterator(Host, ArcID, ArcJID, undefined, undefined,
        undefined, undefined, Now, undefined, 50).


-spec new_iterator(ejabberd:server(), ArcID :: archive_id(),
          ArcJID :: ejabberd:jid(), RSM :: 'undefined', Borders :: 'undefined',
          Start :: 'undefined', End :: 'undefined', Now :: non_neg_integer(),
          WithJID :: 'undefined', PageSize :: 50) -> iterator_fun().
new_iterator(Host, ArcID, ArcJID, RSM, Borders, Start, End, Now,
             WithJID, PageSize) ->
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


-spec after_rsm(MRows :: [any(),...]) -> jlib:rsm_in().
after_rsm(MessageRows) ->
    {MessID,_SrcJID,_Packet} = lists:last(MessageRows),
    #rsm_in{direction = aft, id = MessID}.


-spec is_last_page(Total :: non_neg_integer(), Off :: non_neg_integer(),
                   PageSize :: 50) -> boolean().
is_last_page(TotalCount, Offset, PageSize) ->
    Offset - PageSize >= TotalCount.


-spec create_dump_file(ArcJID :: ejabberd:jid(), file:name())
                                                -> 'ok' | {'error',atom()}.
create_dump_file(ArcJID, OutFileName) ->
    mod_mam_dump:create_dump_file(new_iterator(ArcJID), OutFileName).


-spec restore_dump_file(ArcJID :: ejabberd:simple_jid() | ejabberd:jid(),
    InFilename :: file:filename(), Opts :: [restore_option()]) -> {'error',_}.
restore_dump_file(ArcJID, InFileName, Opts) ->
    try
        restore_dump_file_unsave(ArcJID, InFileName, Opts)
    catch Type:Reason ->
        Trace = erlang:get_stacktrace(),
        lager:error("Error ~p:~p occured while restoring ~p from file ~ts.~nTrace: ~p",
                    [Type, Reason, jlib:jid_to_binary(ArcJID), InFileName, Trace]),
        {error, Reason}
    end.


-spec restore_dump_file_unsave(ArcJID :: ejabberd:simple_jid() | ejabberd:jid(),
    InFilename :: file:filename(), Opts :: [restore_option()]) -> {'error',_}.
restore_dump_file_unsave(ArcJID, InFileName, Opts) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    WriterF = fun(MessID, FromJID, ToJID, MessElem) ->
            case ArcJID of
                FromJID ->
                    archive_message(Host, MessID, ArcID,
                                    ArcJID, ToJID, FromJID,
                                    outgoing, MessElem);
                ToJID ->
                    archive_message(Host, MessID, ArcID,
                                    ArcJID, FromJID, FromJID,
                                    incoming, MessElem);
                _ ->
                    {error, no_local_jid}
            end
        end,
    mod_mam_dump:restore_dump_file(WriterF, InFileName, Opts).


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?DEBUG("mod_mam starting", []),
    ejabberd_users:start(Host),
    %% `parallel' is the only one recommended here.
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    mod_disco:register_feature(Host, ?NS_MAM),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM,
                                  ?MODULE, process_mam_iq, IQDisc),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    ok.


-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    ?DEBUG("mod_mam stopping", []),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM),
    mod_disco:unregister_feature(Host, ?NS_MAM),
    ok.

%% ----------------------------------------------------------------------
%% hooks and handlers

%% `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec process_mam_iq(From :: ejabberd:jid(), To :: ejabberd:jid(),
                     IQ :: ejabberd:iq()) -> ejabberd:iq() | ignore.
process_mam_iq(From=#jid{lserver=Host}, To, IQ) ->
    Action = iq_action(IQ),
    case is_action_allowed(Action, From, To) of
        true  ->
            case wait_shaper(Host, Action, From) of
                ok ->
                    handle_error_iq(Host, To, Action,
                        handle_mam_iq(Action, From, To, IQ));
                {error, max_delay_reached} ->
                    ejabberd_hooks:run(mam_drop_iq, Host,
                        [Host, To, IQ, Action, max_delay_reached]),
                    return_max_delay_reached_error_iq(IQ)
            end;
        false ->
            ejabberd_hooks:run(mam_drop_iq, Host,
                [Host, To, IQ, Action, action_not_allowed]),
            return_action_not_allowed_error_iq(IQ)
    end.


%% @doc Handle an outgoing message.
%%
%% Note: for outgoing messages, the server MUST use the value of the 'to'
%%       attribute as the target JID.
-spec user_send_packet(From :: ejabberd:jid(), To :: ejabberd:jid(),
                       Packet :: jlib:xmlel()) -> 'ok'.
user_send_packet(From, To, Packet) ->
    ?DEBUG("Send packet~n    from ~p ~n    to ~p~n    packet ~p.",
              [From, To, Packet]),
    handle_package(outgoing, false, From, To, From, Packet),
    ok.


%% @doc Handle an incoming message.
%%
%% Note: For incoming messages, the server MUST use the value of the
%%       'from' attribute as the target JID.
%%
%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
-type fpacket() :: {From :: ejabberd:jid(),
                    To :: ejabberd:jid(),
                    Packet :: jlib:xmlel()}.
-spec filter_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_packet(drop) ->
    drop;
filter_packet({From, To=#jid{luser=LUser, lserver=LServer}, Packet}) ->
    ?DEBUG("Receive packet~n    from ~p ~n    to ~p~n    packet ~p.",
              [From, To, Packet]),
    Packet2 =
    case ejabberd_users:is_user_exists(LUser, LServer) of
    false -> Packet;
    true ->
        case handle_package(incoming, true, To, From, From, Packet) of
            undefined -> Packet;
            MessID ->
                ?DEBUG("Archived incoming ~p", [MessID]),
                BareTo = jlib:jid_to_binary(jlib:jid_remove_resource(To)),
                replace_archived_elem(BareTo, MessID, Packet)
        end
    end,
    {From, To, Packet2}.


%% @doc A ejabberd's callback with diferent order of arguments.
-spec remove_user(ejabberd:user(), ejabberd:server()) -> 'ok'.
remove_user(User, Server) ->
    delete_archive(Server, User).

%% ----------------------------------------------------------------------
%% Internal functions

-spec server_host(ejabberd:jid()) -> ejabberd:lserver().
server_host(#jid{lserver=LServer}) ->
    LServer.


-spec is_action_allowed(Action :: action(), From :: ejabberd:jid(),
                        To :: ejabberd:jid()) -> boolean().
is_action_allowed(Action, From, To=#jid{lserver=Host}) ->
    case acl:match_rule(Host, Action, From) of
        allow   -> true;
        deny    -> false;
        default -> is_action_allowed_by_default(Action, From, To)
    end.

-spec is_action_allowed_by_default(Action :: action(), From :: ejabberd:jid(),
                                   To :: ejabberd:jid()) -> boolean().
is_action_allowed_by_default(_Action, From, To) ->
    compare_bare_jids(From, To).


-spec compare_bare_jids(ejabberd:simple_jid() | ejabberd:jid(),
                        ejabberd:simple_jid() | ejabberd:jid()) -> boolean().
compare_bare_jids(JID1, JID2) ->
    jlib:jid_remove_resource(JID1) =:=
    jlib:jid_remove_resource(JID2).


-spec action_to_shaper_name(action()) -> atom().
action_to_shaper_name(Action) -> list_to_atom(atom_to_list(Action) ++ "_shaper").


-spec action_to_global_shaper_name(action()) -> atom().
action_to_global_shaper_name(Action) -> list_to_atom(atom_to_list(Action) ++ "_global_shaper").


-spec handle_mam_iq('mam_get_prefs', From :: ejabberd:jid(), To :: ejabberd:jid(),
                    IQ :: ejabberd:iq()) -> ejabberd:iq().
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


-spec iq_action(ejabberd:iq()) -> action().
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


-spec handle_set_prefs(ejabberd:jid(), ejabberd:iq()) -> ejabberd:iq().
handle_set_prefs(ArcJID=#jid{},
                 IQ=#iq{sub_el = PrefsEl}) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} = parse_prefs(PrefsEl),
    ?DEBUG("Parsed data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
              [DefaultMode, AlwaysJIDs, NeverJIDs]),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    Res = set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs),
    handle_set_prefs_result(Res, DefaultMode, AlwaysJIDs, NeverJIDs, IQ).

handle_set_prefs_result(ok, DefaultMode, AlwaysJIDs, NeverJIDs, IQ) ->
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_set_prefs_result({error, Reason},
                        _DefaultMode, _AlwaysJIDs, _NeverJIDs, IQ) ->
    return_error_iq(IQ, Reason).


-spec handle_get_prefs(ejabberd:jid(), IQ :: ejabberd:iq()) -> ejabberd:iq().
handle_get_prefs(ArcJID=#jid{}, IQ=#iq{}) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    Res = get_prefs(Host, ArcID, ArcJID, always),
    handle_get_prefs_result(Res, IQ).

handle_get_prefs_result({DefaultMode, AlwaysJIDs, NeverJIDs}, IQ) ->
    ?DEBUG("Extracted data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
              [DefaultMode, AlwaysJIDs, NeverJIDs]),
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_get_prefs_result({error, Reason}, IQ) ->
    return_error_iq(IQ, Reason).
    

-spec handle_lookup_messages(From :: ejabberd:jid(), ArcJID :: ejabberd:jid(),
                             IQ :: ejabberd:iq()) -> ejabberd:iq().
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
        ErrorEl = jlib:stanza_errort(<<"">>, <<"modify">>, <<"policy-violation">>,
                                 <<"en">>, <<"Too many results">>),
        IQ#iq{type = error, sub_el = [ErrorEl]};
    {error, Reason} ->
        return_error_iq(IQ, Reason);
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


%% @doc Purging multiple messages
-spec handle_purge_multiple_messages(ejabberd:jid(), IQ :: ejabberd:iq()
                                    ) -> ejabberd:iq().
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
    Res = purge_multiple_messages(Host, ArcID, ArcJID, Borders,
                                  Start, End, Now, With),
    return_purge_multiple_message_iq(IQ, Res).


-spec handle_purge_single_message(ejabberd:jid(), IQ :: ejabberd:iq()
                                 ) -> ejabberd:iq().
handle_purge_single_message(ArcJID=#jid{},
                            IQ=#iq{sub_el = PurgeEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    BExtMessID = xml:get_tag_attr_s(<<"id">>, PurgeEl),
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    PurgingResult = purge_single_message(Host, MessID, ArcID, ArcJID, Now),
    return_purge_single_message_iq(IQ, PurgingResult).


-spec handle_package(Dir :: incoming | outgoing, ReturnMessID :: boolean(),
    LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(), SrcJID :: ejabberd:jid(),
    Packet :: jlib:xmlel()) -> MaybeMessID :: binary() | undefined.
handle_package(Dir, ReturnMessID,
               LocJID=#jid{},
               RemJID=#jid{},
               SrcJID=#jid{}, Packet) ->
    IsComplete = is_complete_message(?MODULE, Dir, Packet),
    case IsComplete of
        true ->
        Host = server_host(LocJID),
        ArcID = archive_id_int(Host, LocJID),
        IsInteresting =
        case get_behaviour(Host, ArcID, LocJID, RemJID, always) of
            always -> true;
            never  -> false;
            roster -> is_jid_in_user_roster(LocJID, RemJID)
        end,
        case IsInteresting of
            true ->
            MessID = generate_message_id(),
            Result = archive_message(Host, MessID, ArcID,
                                     LocJID, RemJID, SrcJID, Dir, Packet),
            case {ReturnMessID, Result} of
                {true, ok} -> mess_id_to_external_binary(MessID);
                _ -> undefined
            end;
            false -> undefined
        end;
        false -> undefined
    end.

%% ----------------------------------------------------------------------
%% Backend wrappers

-spec archive_id_int(ejabberd:server(), ejabberd:jid()) -> integer().
archive_id_int(Host, ArcJID=#jid{}) ->
    ejabberd_hooks:run_fold(mam_archive_id, Host, undefined, [Host, ArcJID]).


-spec archive_size(ejabberd:server(), archive_id(), ejabberd:jid()) -> integer().
archive_size(Host, ArcID, ArcJID=#jid{}) ->
    ejabberd_hooks:run_fold(mam_archive_size, Host, 0, [Host, ArcID, ArcJID]).


-spec get_behaviour(ejabberd:server(), archive_id(), LocJID :: ejabberd:jid(),
    RemJID :: ejabberd:jid(), Default :: 'always') -> atom().
get_behaviour(Host, ArcID,
              LocJID=#jid{},
              RemJID=#jid{}, DefaultBehaviour) ->
    ejabberd_hooks:run_fold(mam_get_behaviour, Host, DefaultBehaviour,
        [Host, ArcID, LocJID, RemJID]).


-spec set_prefs(ejabberd:server(), archive_id(), ArcJID :: ejabberd:jid(),
                DefaultMode :: atom(), AlwaysJIDs :: [ejabberd:literal_jid()],
                NeverJIDs :: [ejabberd:literal_jid()]) -> any().
set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    ejabberd_hooks:run_fold(mam_set_prefs, Host, ok,
        [Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs]).


%% @doc Load settings from the database.
-spec get_prefs(Host :: ejabberd:server(), ArcID :: archive_id(),
                ArcJID :: ejabberd:jid(), GlobalDefaultMode :: archive_behaviour()
                ) -> preference() | {error, Reason :: term()}.
get_prefs(Host, ArcID, ArcJID, GlobalDefaultMode) ->
    ejabberd_hooks:run_fold(mam_get_prefs, Host,
        {GlobalDefaultMode, [], []},
        [Host, ArcID, ArcJID]).


-spec remove_archive(ejabberd:server(), archive_id(), ejabberd:jid()) -> 'ok'.
remove_archive(Host, ArcID, ArcJID=#jid{}) ->
    ejabberd_hooks:run(mam_remove_archive, Host, [Host, ArcID, ArcJID]),
    ok.

%% `IsSimple' can contain three values:
%% - true - do not count records (useful during pagination, when we already
%%          know how many messages we have from a previous query);
%% - false - count messages (slow, according XEP-0313);
%% - opt_count - count messages (same as false, fast for small result sets)
%%
%% The difference between false and opt_count is that with IsSimple=false we count
%% messages first and then extract a messages on a page (if count is not zero).
%% If IsSimple=opt_count we extract a page and then calculate messages (if required).
%% `opt_count' can be passed inside an IQ.
%% Same for mod_mam_muc.
-spec lookup_messages(Host :: ejabberd:server(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid(),
        RSM :: jlib:rsm_in() | undefined, Borders :: mod_mam:borders() | undefined,
        Start :: mod_mam:unix_timestamp() | undefined,
        End :: mod_mam:unix_timestamp() | undefined, Now :: mod_mam:unix_timestamp(),
        WithJID :: ejabberd:jid() | undefined, PageSize :: non_neg_integer(),
        LimitPassed :: boolean(), MaxResultLimit :: non_neg_integer(),
        IsSimple :: boolean() | opt_count) ->
            {ok, mod_mam:lookup_result()}
            | {error, 'policy-violation'}
            | {error, Reason :: term()}.
lookup_messages(Host, ArcID, ArcJID, RSM, Borders, Start, End, Now,
                WithJID, PageSize, LimitPassed, MaxResultLimit, IsSimple) ->
    ejabberd_hooks:run_fold(mam_lookup_messages, Host, {ok, {0, 0, []}},
        [Host, ArcID, ArcJID, RSM, Borders,
         Start, End, Now, WithJID,
         PageSize, LimitPassed, MaxResultLimit, IsSimple]).


-spec archive_message(Host :: ejabberd:server(), MessID :: message_id(),
    ArcID :: archive_id(), LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(),
    SrcJID :: ejabberd:jid(), Dir :: incoming | outgoing, Packet :: term()
    ) -> ok | {error, timeout}.
archive_message(Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    ejabberd_hooks:run_fold(mam_archive_message, Host, ok,
        [Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet]).


-spec purge_single_message(Host :: ejabberd:server(), MessID :: message_id(),
    ArcID  :: archive_id(), ArcJID :: ejabberd:jid(), Now :: unix_timestamp()
    ) -> ok | {error, 'not-found'} | {error, Reason :: term()}.
purge_single_message(Host, MessID, ArcID, ArcJID, Now) ->
    ejabberd_hooks:run_fold(mam_purge_single_message, Host, ok,
        [Host, MessID, ArcID, ArcJID, Now]).

-spec purge_multiple_messages(Host :: ejabberd:server(), ArcID :: archive_id(),
    ArcJID  :: ejabberd:jid(), Borders :: borders() | undefined,
    Start :: unix_timestamp() | undefined, End :: unix_timestamp() | undefined,
    Now :: unix_timestamp(), WithJID :: ejabberd:jid() | undefined) ->
        ok | {error, Reason :: term()}.
purge_multiple_messages(Host, ArcID, ArcJID, Borders, Start, End, Now, WithJID) ->
    ejabberd_hooks:run_fold(mam_purge_multiple_messages, Host, ok,
        [Host, ArcID, ArcJID, Borders, Start, End, Now, WithJID]).


-spec wait_shaper(ejabberd:server(), action(), ejabberd:jid()
                  ) -> 'ok' | {'error','max_delay_reached'}.
wait_shaper(Host, Action, From) ->
    case shaper_srv:wait(Host, action_to_shaper_name(Action), From, 1) of
        ok ->
            shaper_srv:wait(Host, action_to_global_shaper_name(Action), global, 1);
        Err ->
            Err
    end.

%% ----------------------------------------------------------------------
%% Helpers

-type messid_jid_packet() :: {MessId :: integer(),
                              SrcJID :: ejabberd:jid(),
                              Packet :: jlib:xmlel()}.
-spec message_row_to_xml(messid_jid_packet(), QueryId :: binary()) -> jlib:xmlel().
message_row_to_xml({MessID,SrcJID,Packet}, QueryID) ->
    {Microseconds, _NodeMessID} = decode_compact_uuid(MessID),
    DateTime = calendar:now_to_universal_time(microseconds_to_now(Microseconds)),
    BExtMessID = mess_id_to_external_binary(MessID),
    wrap_message(Packet, QueryID, BExtMessID, DateTime, SrcJID).


-spec message_row_to_ext_id(messid_jid_packet()) -> binary().
message_row_to_ext_id({MessID,_,_}) ->
    mess_id_to_external_binary(MessID).


-spec message_row_to_dump_xml(messid_jid_packet()) -> false | jlib:xmlel().
message_row_to_dump_xml(M) ->
     xml:get_subtag(message_row_to_xml(M, undefined), <<"result">>).


-spec maybe_jid(binary()) -> 'error' | 'undefined' | ejabberd:jid().
maybe_jid(<<>>) ->
    undefined;
maybe_jid(JID) when is_binary(JID) ->
    jlib:binary_to_jid(JID).


%% @doc Convert id into internal format.
-spec fix_rsm('none' | jlib:rsm_in()) -> 'undefined' | jlib:rsm_in().
fix_rsm(none) ->
    undefined;
fix_rsm(RSM=#rsm_in{id = undefined}) ->
    RSM;
fix_rsm(RSM=#rsm_in{id = <<>>}) ->
    RSM#rsm_in{id = undefined};
fix_rsm(RSM=#rsm_in{id = BExtMessID}) when is_binary(BExtMessID) ->
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    RSM#rsm_in{id = MessID}.


-spec elem_to_start_microseconds(_) -> 'undefined' | non_neg_integer().
elem_to_start_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"start">>}, cdata])).


-spec elem_to_end_microseconds(_) -> 'undefined' | non_neg_integer().
elem_to_end_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"end">>}, cdata])).


-spec elem_to_with_jid(jlib:xmlel()) -> 'error' | 'undefined' | ejabberd:jid().
elem_to_with_jid(El) ->
    maybe_jid(xml:get_path_s(El, [{elem, <<"with">>}, cdata])).


%% @doc This element's name is "limit". But it must be "max" according XEP-0313.
-spec elem_to_limit(any()) -> any().
elem_to_limit(QueryEl) ->
    get_one_of_path(QueryEl, [
        [{elem, <<"set">>}, {elem, <<"max">>}, cdata],
        [{elem, <<"set">>}, {elem, <<"limit">>}, cdata]
    ]).

handle_error_iq(Host, To, Action, IQ=#iq{type = {error, Reason}}) ->
    ejabberd_hooks:run(mam_drop_iq, Host,
        [Host, To, IQ, Action, Reason]),
    IQ#iq{type = error};
handle_error_iq(_Host, _To, _Action, IQ) ->
    IQ.

-spec return_action_not_allowed_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_action_not_allowed_error_iq(IQ) ->
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"not-allowed">>,
         <<"en">>, <<"The action is not allowed.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

return_purge_multiple_message_iq(IQ, ok) ->
    return_purge_success(IQ);
return_purge_multiple_message_iq(IQ, {error, Reason}) ->
    return_error_iq(IQ, Reason).

-spec return_purge_single_message_iq(ejabberd:iq(),
        ok|{error, 'not-found'}|{error, Reason :: term()}
                                    ) -> ejabberd:iq().
return_purge_single_message_iq(IQ, ok) ->
    return_purge_success(IQ);
return_purge_single_message_iq(IQ, {error, 'not-found'}) ->
    return_purge_not_found_error_iq(IQ);
return_purge_single_message_iq(IQ, {error, Reason}) ->
    return_error_iq(IQ, Reason).

-spec return_purge_success(ejabberd:iq()) -> ejabberd:iq().
return_purge_success(IQ) ->
    IQ#iq{type = result, sub_el = []}.

-spec return_purge_not_found_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_purge_not_found_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"item-not-found">>,
         <<"en">>, <<"The provided UID did not match any message stored in archive.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.


-spec return_max_delay_reached_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_max_delay_reached_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = ?ERRT_RESOURCE_CONSTRAINT(
        <<"en">>, <<"The action is cancelled because of flooding.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.


-spec return_error_iq(ejabberd:iq(), Reason :: term()) -> ejabberd:iq().
return_error_iq(IQ, timeout) ->
    IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]};
return_error_iq(IQ, _Reason) ->
    IQ#iq{type = error, sub_el = [?ERR_INTERNAL_SERVER_ERROR]}.


