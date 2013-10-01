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
         restore_dump_file/3,
         debug_info/1]).

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
         is_complete_message/1,
         wrap_message/5,
         result_set/4,
         result_query/1,
         result_prefs/3,
         parse_prefs/1]).

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
%% XMPP types
-type server_host() :: binary().
-type literal_jid() :: binary().
-type elem() :: #xmlel{}.


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

debug_info(Host) ->
    AM = archive_module(Host),
    WM = writer_module(Host),
    PM = prefs_module(Host),
    [{archive_module, AM},
     {writer_module, WM},
     {prefs_module, PM}].


new_iterator(ArcJID=#jid{}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    new_iterator(Host, ArcID, ArcJID, undefined,
        undefined, undefined, Now, undefined, 50).

new_iterator(Host, ArcID, ArcJID, RSM, Start, End, Now, WithJID,
             PageSize) ->
    fun() ->
        {ok, {TotalCount, Offset, MessageRows}} =
        lookup_messages(Host, ArcID, ArcJID, RSM, Start, End, Now,
                        WithJID, PageSize, true, PageSize),
        Data = [exml:to_iolist(message_row_to_dump_xml(M))
                || M <- MessageRows],
        Cont = case is_last_page(TotalCount, Offset, PageSize) of
            false ->
                fun() -> {error, eof} end;
            true ->
                new_iterator(
                    Host, ArcID, ArcJID, after_rsm(MessageRows),
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

-spec restore_dump_file(ArcJID, InFileName, Opts) -> ok when
    ArcJID :: #jid{},
    InFileName :: file:filename(),
    Opts :: [Opt],
    Opt :: {rewrite_jids, RewriterF | Substitutions},
    RewriterF :: fun((BinJID) -> BinJID),
    Substitutions :: [{BinJID, BinJID}],
    BinJID :: binary().
restore_dump_file(ArcJID=#jid{}, InFileName, Opts) ->
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

start(Host, Opts) ->
    ?DEBUG("mod_mam starting", []),
    ejabberd_users:start(Host),
    [start_module(Host, M) || M <- required_modules(Host)],
    %% `parallel' is the only one recommended here.
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    mod_disco:register_feature(Host, mam_ns_binary()),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, mam_ns_binary(),
                                  ?MODULE, process_mam_iq, IQDisc),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ok.

stop(Host) ->
    ?DEBUG("mod_mam stopping", []),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 90),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, mam_ns_string()),
    mod_disco:unregister_feature(Host, mam_ns_binary()),
    [stop_module(Host, M) || M <- required_modules(Host)],
    ok.

%% ----------------------------------------------------------------------
%% Control modules

start_module(Host, M) ->
    case is_function_exist(M, start, 2) of
        true  -> M:start(Host, ?MODULE);
        false -> ok
    end,
    ok.

stop_module(Host, M) ->
    case is_function_exist(M, stop, 2) of
        true  -> M:stop(Host, ?MODULE);
        false -> ok
    end,
    ok.

required_modules(Host) ->
    expand_modules(Host, base_modules(Host)).

expand_modules(Host, Mods) ->
    expand_modules(Host, Mods, []).
    
expand_modules(Host, [H|T], Acc) ->
    %% Do not load the same module twice.
    ReqMods = skip_expanded_modules(required_modules(Host, H), Acc),
    expand_modules(Host, T, [H] ++ ReqMods ++ Acc);
expand_modules(_, [], Acc) ->
    lists:reverse(Acc).

required_modules(Host, M) ->
    case is_function_exist(M, required_modules, 2) of
        true  -> M:required_modules(Host, ?MODULE);
        false -> []
    end.

skip_expanded_modules(Mods, ExpandedMods) ->
    [M || M <- Mods, not lists:member(M, ExpandedMods)].

base_modules(Host) ->
    [prefs_module(Host),
     archive_module(Host),
     writer_module(Host),
     user_module(Host)].

prefs_module(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, prefs_module, mod_mam_odbc_prefs).

archive_module(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, archive_module, mod_mam_odbc_arch).

writer_module(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, writer_module, mod_mam_odbc_arch).

user_module(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, user_module, mod_mam_odbc_user).

%% ----------------------------------------------------------------------
%% hooks and handlers

%% `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives 
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec process_mam_iq(From, To, IQ) -> IQ | ignore when
    From :: jid(),
    To :: jid(),
    IQ :: #iq{}.
process_mam_iq(From=#jid{lserver=Host}, To, IQ) ->
    Action = iq_action(IQ),
    case is_action_allowed(Action, From, To) of
        true  -> 
            case shaper_srv:wait(Host, action_to_shaper_name(Action), From, 1) of
                ok ->
                    handle_mam_iq(Action, From, To, IQ);
                {error, max_delay_reached} ->
                    return_max_delay_reached_error_iq(IQ)
            end;
        false -> return_action_not_allowed_error_iq(IQ)
    end.

%% @doc Handle an outgoing message.
%%
%% Note: for outgoing messages, the server MUST use the value of the 'to' 
%%       attribute as the target JID. 
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
-spec filter_packet(Value) -> Value when
    Value :: {From, To, Packet} | drop,
    From :: jid(),
    To :: jid(),
    Packet :: elem().
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
remove_user(User, Server) ->
    delete_archive(Server, User).

%% ----------------------------------------------------------------------
%% Internal functions

server_host(#jid{lserver=LServer}) ->
    LServer.

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
is_action_allowed_by_default(_Action, From, To) ->
    compare_bare_jids(From, To).

compare_bare_jids(JID1, JID2) ->
    jlib:jid_remove_resource(JID1) =:=
    jlib:jid_remove_resource(JID2).

-spec action_to_shaper_name(action()) -> atom().
action_to_shaper_name(Action) -> list_to_atom(atom_to_list(Action) ++ "_shaper").

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
    wait_flushing(Host, ArcID, ArcJID),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = elem_to_start_microseconds(QueryEl),
    End   = elem_to_end_microseconds(QueryEl),
    %% Filtering by contact.
    With  = elem_to_with_jid(QueryEl),
    RSM   = fix_rsm(jlib:rsm_decode(QueryEl)),
    Limit = elem_to_limit(QueryEl),
    PageSize = min(max_result_limit(),
                   maybe_integer(Limit, default_result_limit())),
    LimitPassed = Limit =/= <<>>,
    case lookup_messages(Host, ArcID, ArcJID, RSM, Start, End, Now, With,
                         PageSize, LimitPassed, max_result_limit()) of
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
    wait_flushing(Host, ArcID, ArcJID),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = elem_to_start_microseconds(PurgeEl),
    End   = elem_to_end_microseconds(PurgeEl),
    %% Filtering by contact.
    With  = elem_to_with_jid(PurgeEl),
    purge_multiple_messages(Host, ArcID, ArcJID, Start, End, Now, With),
    return_purge_success(IQ).

handle_purge_single_message(ArcJID=#jid{},
                            IQ=#iq{sub_el = PurgeEl}) ->
    Now = mod_mam_utils:now_to_microseconds(now()),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    wait_flushing(Host, ArcID, ArcJID),
    BExtMessID = xml:get_tag_attr_s(<<"id">>, PurgeEl),
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    PurgingResult = purge_single_message(Host, MessID, ArcID, ArcJID, Now),
    return_purge_single_message_iq(IQ, PurgingResult).

-spec handle_package(Dir, ReturnMessID, LocJID, RemJID, SrcJID, Packet) ->
    MaybeMessID when
    Dir :: incoming | outgoing,
    ReturnMessID :: boolean(),
    LocJID :: jid(),
    RemJID :: jid(),
    SrcJID :: jid(),
    Packet :: elem(),
    MaybeMessID :: binary() | undefined.
handle_package(Dir, ReturnMessID,
               LocJID=#jid{},
               RemJID=#jid{},
               SrcJID=#jid{}, Packet) ->
    IsComplete = is_complete_message(Packet),
    Host = server_host(LocJID),
    ArcID = archive_id_int(Host, LocJID),
    case IsComplete of
        true ->
        IsInteresting =
        case get_behaviour(Host, ArcID, LocJID, RemJID, always) of
            always -> true;
            never  -> false;
            roster -> is_jid_in_user_roster(LocJID, RemJID)
        end,
        case IsInteresting of
            true -> 
            MessID = generate_message_id(),
            archive_message(Host, MessID, ArcID,
                            LocJID, RemJID, SrcJID, Dir, Packet),
            case ReturnMessID of
                true  -> mess_id_to_external_binary(MessID);
                false -> undefined
            end;
            false -> undefined
        end;
        false -> undefined
    end.

%% ----------------------------------------------------------------------
%% Backend wrappers

archive_id_int(Host, ArcJID=#jid{}) ->
    UM = user_module(Host),
    UM:archive_id(Host, ?MODULE, ArcJID).

archive_size(Host, ArcID, ArcJID=#jid{}) ->
    AM = archive_module(Host),
    AM:archive_size(Host, ?MODULE, ArcID, ArcJID).

get_behaviour(Host, ArcID,
              LocJID=#jid{},
              RemJID=#jid{}, DefaultBehaviour) ->
    M = prefs_module(Host),
    M:get_behaviour(Host, ?MODULE,
                    ArcID, LocJID, RemJID, DefaultBehaviour).

set_prefs(Host, ArcID, ArcJID, AlwaysJIDs, NeverJIDs, DefaultMode) ->
    M = prefs_module(Host),
    M:set_prefs(Host, ?MODULE,
                ArcID, ArcJID, AlwaysJIDs, NeverJIDs, DefaultMode).

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
    M = prefs_module(Host),
    M:get_prefs(Host, ?MODULE, ArcID, ArcJID, GlobalDefaultMode).

remove_archive(Host, ArcID, ArcJID=#jid{}) ->
    wait_flushing(Host, ArcID, ArcJID),
    PM = prefs_module(Host),
    AM = archive_module(Host),
    UM = user_module(Host),
    PM:remove_archive(Host, ?MODULE, ArcID, ArcJID),
    AM:remove_archive(Host, ?MODULE, ArcID, ArcJID),
    UM:remove_archive(Host, ?MODULE, ArcID, ArcJID),
    ok.

-spec lookup_messages(Host, ArcID, ArcJID, RSM, Start, End, Now, WithJID,
                      PageSize, LimitPassed, MaxResultLimit) ->
    {ok, {TotalCount, Offset, MessageRows}} | {error, 'policy-violation'}
    when
    Host    :: server_host(),
    ArcID   :: archive_id(),
    ArcJID  :: #jid{},
    RSM     :: #rsm_in{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: #jid{} | undefined,
    PageSize :: non_neg_integer(),
    LimitPassed :: boolean(),
    MaxResultLimit :: non_neg_integer(),
    TotalCount :: non_neg_integer(),
    Offset  :: non_neg_integer(),
    MessageRows :: list(tuple()).
lookup_messages(Host, ArcID, ArcJID, RSM, Start, End, Now,
                WithJID, PageSize, LimitPassed, MaxResultLimit) ->
    AM = archive_module(Host),
    AM:lookup_messages(Host, ?MODULE,
                       ArcID, ArcJID, RSM, Start, End, Now, WithJID,
                       PageSize, LimitPassed, MaxResultLimit).

archive_message(Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    M = writer_module(Host),
    M:archive_message(Host, ?MODULE,
                      MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet).


-spec purge_single_message(Host, MessID, ArcID, ArcJID, Now) ->
    ok | {error, 'not-found'} when
    Host   :: server_host(),
    MessID :: message_id(),
    ArcID  :: archive_id(),
    ArcJID :: jid(),
    Now :: unix_timestamp().
purge_single_message(Host, MessID, ArcID, ArcJID, Now) ->
    AM = archive_module(Host),
    AM:purge_single_message(Host, ?MODULE,
                            MessID, ArcID, ArcJID, Now).

-spec purge_multiple_messages(Host, ArcID, ArcJID, Start, End, Now, WithJID) -> ok
    when
    Host    :: server_host(),
    ArcID   :: archive_id(),
    ArcJID  :: jid(),
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined,
    Now     :: unix_timestamp(),
    WithJID :: jid() | undefined.
purge_multiple_messages(Host, ArcID, ArcJID, Start, End, Now, WithJID) ->
    AM = archive_module(Host),
    AM:purge_multiple_messages(Host, ?MODULE,
                               ArcID, ArcJID, Start, End, Now, WithJID).

wait_flushing(Host, ArcID, ArcJID) ->
    M = writer_module(Host),
    M:wait_flushing(Host, ?MODULE, ArcID, ArcJID).

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

