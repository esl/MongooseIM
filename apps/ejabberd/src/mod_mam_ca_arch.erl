%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_con_ca_arch).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([archive_size/4,
         archive_message/9,
         lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Helpers
-export([get_conversations_after/3,
         test_query/1]).

%% Internal exports
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ----------------------------------------------------------------------
%% Imports

%% UID
-import(mod_mam_utils,
        [encode_compact_uuid/2]).

%% JID serialization
-import(mod_mam_utils,
        [jid_to_opt_binary/2,
         expand_minified_jid/2]).

%% Other
-import(mod_mam_utils,
        [maybe_min/2,
         maybe_max/2,
         apply_start_border/2,
         apply_end_border/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(state, {
    host,
    conn,
    insert_query,
    remove_archive_query,
    test_query,
    calc_count_handler,
    extract_messages_handler,
    extract_messages_r_handler,
    query_refs,
    query_refs_count}).

-record(mam_ca_filter, {
    local_jid,
    remote_jid,
    start_id,
    end_id
}).

-record(prepared_query, {
          query_id,
          query_types}).

-record(mam_message, {
  id :: non_neg_integer(),
  user_jid :: binary(),
  from_jid :: binary(),
  remote_bare_jid :: binary(),
  remote_resource :: binary(),
  is_incoming :: boolean(),
  message binary()
}).

-type worker() :: pid() | atom().

%% ----------------------------------------------------------------------
%% Types

-type filter() :: #mam_ca_filter{}.
-type message_id() :: non_neg_integer().
-type user_id() :: non_neg_integer().
-type server_hostname() :: binary().
-type server_host() :: binary().
-type unix_timestamp() :: non_neg_integer().


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    create_worker_pool(Host),
    mod_mam_con_ca_sup:start(Host, cassandra_config(Host)),
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            start_pm(Host, Opts);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            start_muc(Host, Opts);
        false ->
            ok
    end.

stop(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            stop_pm(Host);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            stop_muc(Host);
        false ->
            ok
    end,
    delete_worker_pool(Host),
    mod_mam_con_ca_sup:stop(Host).

cassandra_config(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, cassandra_config, [])
    ++ [{servers, [{"localhost", 9042, 1}]},
        {socket_options, [{connect_timeout, 4000}]},
        {keyspace, "mam"},
        {credentials, undefined}].

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

start_pm(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

stop_pm(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:add(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:add(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.

stop_muc(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, no_writer, false) of
        true ->
            ok;
        false ->
            ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, archive_message, 50)
    end,
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 50),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ejabberd_hooks:delete(mam_purge_single_message, Host, ?MODULE, purge_single_message, 50),
    ejabberd_hooks:delete(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 50),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

create_worker_pool(Host) ->
    pg2:create(group_name(Host)).

delete_worker_pool(Host) ->
    pg2:delete(group_name(Host)).

register_worker(Host, WorkerPid) ->
    pg2:join(group_name(Host), WorkerPid).

select_worker(Host, UserJID) ->
    case pg2:get_local_members(group_name(Host)) of
        [] ->
            error({no_worker, Host});
        Workers ->
            N = erlang:phash2(UserJID, length(Workers)) + 1,
            lists:nth(N, Workers)
    end.

group_name(Host) ->
    {mam_ca, node(), Host}.

start_link(Host, Addr, Port, ClientOptions) ->
    gen_server:start_link(?MODULE, [Host, Addr, Port, ClientOptions], []).

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

archive_size(Size, _Host, _UserID, _UserJID) when is_integer(Size) ->
    %% TODO
    Size.



%% ----------------------------------------------------------------------
%% INSERT MESSAGE

insert_query_sql() ->
    "INSERT INTO mam_con_message "
        "(id, user_jid, remote_bare_jid, remote_resource, is_incoming, message) "
        "VALUES (?, ?, ?, ?, ?, ?)".

archive_message(Result, Host, MessID, UserID,
                     LocJID, RemJID, SrcJID, Dir, Packet) ->
    try
        archive_message_2(Result, Host, MessID, UserID,
                        LocJID, RemJID, SrcJID, Dir, Packet)
    catch _Type:Reason ->
        {error, Reason}
    end.

archive_message_2(_Result, Host, MessID, _UserID,
                  LocJID=#jid{},
                  RemJID=#jid{lresource=BRemResource},
                 SrcJID=#jid{}, Dir, Packet) ->
    BLocJID = bare_jid(LocJID),
    BRemJID = bare_jid(RemJID),
    BSrcJID = full_jid(SrcJID),
    IsIncoming = Dir =:= incoming,
    BPacket = packet_to_stored_binary(Packet),
    Message = #mam_message{
      id = MessID,
      user_jid = BLocJID,
      from_jid = BSrcJID,
      remote_bare_jid = BRemJID,
      remote_resource = BRemResource,
      is_incoming = IsIncoming,
      message = BPacket
    },
    Worker = select_worker(Host, LocJID),
    write_message(Worker, Message).

write_message(Worker, Message) ->
    gen_server:cast(Worker, {write_message, Message}).

message_to_params(#mam_message{
      id = MessID,
      user_jid = BLocJID,
      from_jid = BSrcJID,
      remote_bare_jid = BRemJID,
      remote_resource = BRemResource,
      is_incoming = IsIncoming,
      message = BPacket
    }) ->
    [MessID, BLocJID, BSrcJID, BRemJID, BRemResource, IsIncoming, BPacket].


%% ----------------------------------------------------------------------
%% TEST CONNECTION

test_query_sql() ->
    "SELECT now() FROM system.local". %% "SELECT 1" for cassandra

test_query(Host) ->
    Workers = pg2:get_local_members(group_name(Host)),
    [{Worker, (catch gen_server:call(Worker, test_query))} || Worker <- Workers].


%% ----------------------------------------------------------------------
%% REMOVE ARCHIVE

remove_archive_query_sql() ->
    "DELETE FROM mam_message WHERE user_jid = ?".

remove_archive(Host, _UserID, UserJID) ->
    Worker = select_worker(Host, LocJID),
    BUserJID = bare_jid(UserJID),
    gen_server:cast(Worker, {remove_archive, BUserJID}),
    ok.



after_id(ID, Filter=#mam_ca_filter{start_id = AfterID}) ->
    Filter#mam_ca_filter{start_id = maybe_max(ID + 1, AfterID)}.

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID - 1, BeforeID)}.

to_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID, BeforeID)}.

rows_to_uniform_format(Host, UserJID, WithJID, IsLocLower, MessageRows) ->
    [row_to_uniform_format(UserJID, WithJID, IsLocLower, Row) || Row <- MessageRows].

row_to_uniform_format(UserJID, _WithJID, IsLocLower, [MessID, IsFromLower, Data])
    when IsFromLower =:= IsLocLower ->
    Packet = binary_to_term(Data),
    {MessID, UserJID, Packet};
row_to_uniform_format(_UserJID, WithJID, _IsLocLower, [MessID, _IsFromLower, Data]) ->
    Packet = binary_to_term(Data),
    {MessID, WithJID, Packet}.

row_to_message_id([MessID,_,_]) ->
    MessID.

-spec purge_single_message(_Result, Host, MessID, _UserID, UserJID,
                           Now) ->
    ok  | {error, 'not-supported'} when
      Host :: server_host(), MessID :: message_id(),
      _UserID :: user_id(), UserJID :: #jid{},
      Now :: unix_timestamp().
purge_single_message(_Result, Host, MessID, _UserID, _UserJID, _Now) ->
   {error, 'not-supported'}.


-spec purge_multiple_messages(_Result, Host, _UserID, UserJID, Borders,
                              Start, End, Now, WithJID) ->
    {error, 'not-supported'} when
      Host :: server_host(), _UserID :: user_id(),
      UserJID :: #jid{}, Borders :: #mam_borders{},
      Start :: unix_timestamp()  | undefined,
      End :: unix_timestamp()  | undefined,
      Now :: unix_timestamp(),
      WithJID :: #jid{}  | undefined.
purge_multiple_messages(_Result, Host, _UserID, UserJID, Borders,
                        Start, End, _Now, WithJID) ->
   {error, 'not-supported'}.


%% Each record is a tuple of form
%% `{<<"13663125233">>,<<"bob@localhost">>,<<"res1">>,<<binary>>}'.
%% Columns are `["id","from_jid","message"]'.
-spec extract_messages(Worker, Host, Filter, IOffset, IMax, ReverseLimit) ->
    [Row] when
    Worker  :: worker(),
    Host    :: server_hostname(),
    Filter  :: filter(),
    IOffset :: non_neg_integer(),
    IMax    :: pos_integer(),
    ReverseLimit :: boolean(),
    Row :: list().
extract_messages(_Worker, _Host, _Filter, _IOffset, 0, _) ->
    [];
extract_messages(Worker, Host, Filter, 0, IMax, false) ->
    ResultF = gen_server:call(Worker,
        {extract_messages, {Filter, IMax}}),
    {ok, Result} = ResultF(),
    seestar_result:rows(Result);
extract_messages(Worker, Host, Filter, 0, IMax, true) ->
    ResultF = gen_server:call(Worker,
        {extract_messages_r, {Filter, IMax}}),
    {ok, Result} = ResultF(),
    lists:reverse(seestar_result:rows(Result));
extract_messages(_Worker, _Host, _Filter, _IOffset, _IMax, _) ->
    error(offset_not_supported).


%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
-spec calc_index(Worker, Host, Filter, MessID) -> Count
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    MessID       :: message_id(),
    Count        :: non_neg_integer().
calc_index(Worker, Host, Filter, MessID) ->
    calc_count(Worker, Host, to_id(MessID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
-spec calc_before(Worker, Host, Filter, MessID) -> Count
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    MessID       :: message_id(),
    Count        :: non_neg_integer().
calc_before(Worker, Host, Filter, MessID) ->
    calc_count(Worker, Host, before_id(MessID, Filter)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_con_message WHERE "
-spec calc_count(Worker, Host, Filter) -> Count
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    Count        :: non_neg_integer().
calc_count(Worker, Host, Filter) ->
    ResultF = gen_server:call(Worker, {calc_count, Filter}),
    {ok, Result} = ResultF(),
    [[Count]] = seestar_result:rows(Result),
    Count.


-spec prepare_filter(BUserJID, BRemJID, Borders, Start, End) -> filter()
    when
    BUserJID :: binary(),
    BRemJID :: binary(),
    Borders :: #mam_borders{} | undefined,
    Start   :: unix_timestamp() | undefined,
    End     :: unix_timestamp() | undefined.
prepare_filter(BUserJID, BRemJID, Borders, Start, End) ->
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    prepare_filter_params(BUserJID, BRemJID, StartID2, EndID2).

prepare_filter_params(BUserJID, BRemJID, StartID, EndID) ->
    #mam_ca_filter{
        user_jid = BUserJID,
        remote_jid = BRemJID,
        start_id = StartID,
        end_id = EndID
    }.

eval_filter_params(#mam_ca_filter{
        user_jid = BUserJID,
        remote_jid = BRemJID,
        start_id = StartID,
        end_id = EndID
    }) ->
    [BUserJID, BRemJID | prepare_filter_opt_params(StartID, EndID)].

select_filter(#mam_ca_filter{
        start_id = StartID,
        end_id = EndID
    }) ->
    select_filter(StartID, EndID).


-spec select_filter(StartID, EndID) ->
    all  | 'end'  | start  | start_end when
      StartID :: integer()  | undefined,
      EndID :: integer()  | undefined.
select_filter(undefined, undefined) ->
    all;
select_filter(undefined, _) ->
    'end';
select_filter(_, undefined) ->
    start;
select_filter(_, _) ->
    start_end.

prepare_filter_sql(StartID, EndID) ->
    case StartID of
       undefined -> "";
       _         -> " AND id >= ?"
    end ++
    case EndID of
       undefined -> "";
       _         -> " AND id <= ?"
    end.

prepare_filter_opt_params(StartID, EndID) ->
    [Value || Value <- [StartID, EndID], Value =/= undefined].

filter_to_sql() ->
    [{select_filter(StartID, EndID),
      prepare_filter_sql(StartID, EndID)}
     || StartID        <- [undefined, 0],
          EndID        <- [undefined, 0]].

-spec calc_offset(Worker, Host, Filter, PageSize, TotalCount, RSM) -> Offset
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    PageSize     :: non_neg_integer(),
    TotalCount   :: non_neg_integer(),
    RSM          :: #rsm_in{} | undefined,
    Offset       :: non_neg_integer().
calc_offset(_W, _LS, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
    when is_integer(Index) ->
    Index;
%% Requesting the Last Page in a Result Set
calc_offset(_W, _LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Worker, Host, F, PS, _TC, #rsm_in{direction = before, id = ID})
    when is_integer(ID) ->
    max(0, calc_before(Worker, Host, F, ID) - PS);
calc_offset(Worker, Host, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
    when is_integer(ID) ->
    calc_index(Worker, Host, F, ID);
calc_offset(_W, _LS, _F, _PS, _TC, _RSM) ->
    0.

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

serialize_jid(JID) ->
    jid:to_binary(jid:to_lower(jid:to_bare(JID))).

bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_binary(jid:to_bare(jid:to_lower(JID))).

full_jid(undefined) -> undefined;
full_jid(JID) ->
    jid:to_binary(jid:to_lower(JID)).

unserialize_jid(BJID) ->
    jid:from_binary(BJID).

is_user_exists(#jid{lserver=LServer, luser=LUser}) ->
    ejabberd_users:is_user_exists(LUser, LServer).

%%====================================================================
%% Internal SQL part
%%====================================================================

extract_messages_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, extract_messages_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_r_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, extract_messages_r_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

calc_count_handler(ConnPid) ->
    dict:from_list([{FilterName, prepare_query(ConnPid, calc_count_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_sql(Filter) ->
    "SELECT id, is_from_lower, message FROM mam_con_message "
        "WHERE user_jid = ? AND remote_jid = ? " ++
        Filter ++ " ORDER BY id LIMIT ?".

extract_messages_r_sql(Filter) ->
    "SELECT id, is_from_lower, message FROM mam_con_message "
        "WHERE user_jid = ? AND remote_jid = ? " ++
        Filter ++ " ORDER BY id DESC LIMIT ?".

calc_count_sql(Filter) ->
    "SELECT COUNT(*) FROM mam_con_message "
        "WHERE user_jid = ? AND remote_jid = ? " ++ Filter.

execute_extract_messages(ConnPid, Handler, {Filter, IMax}) when is_integer(IMax) ->
    Params = eval_filter_params(Filter) ++ [IMax],
    FilterName = select_filter(Filter),
    PreparedQuery = dict:fetch(FilterName, Handler),
    execute_prepared_query(ConnPid, PreparedQuery, Params).

execute_calc_count(ConnPid, Handler, Filter) ->
    Params = eval_filter_params(Filter),
    FilterName = select_filter(Filter),
    PreparedQuery = dict:fetch(FilterName, Handler),
    execute_prepared_query(ConnPid, PreparedQuery, Params).

prepare_query(ConnPid, Query) ->
    {ok, Res} = seestar_session:prepare(ConnPid, Query),
    Types = seestar_result:types(Res),
    QueryID = seestar_result:query_id(Res),
    #prepared_query{query_id=QueryID, query_types=Types}.

execute_prepared_query(ConnPid, #prepared_query{query_id=QueryID, query_types=Types}, Params) ->
    seestar_session:execute_async(ConnPid, QueryID, Types, Params, one).


save_query_ref(From, QueryRef, State=#state{query_refs=Refs, query_refs_count=RefsCount}) ->
    Refs2 = dict:store(QueryRef, From, Refs),
    put(query_refs_count, RefsCount+1),
    State#state{query_refs=Refs2, query_refs_count=RefsCount+1}.

forward_query_respond(ResultF, QueryRef,
    State=#state{query_refs=Refs, query_refs_count=RefsCount}) ->
    case dict:find(QueryRef, Refs) of
        {ok, From} ->
            Refs2 = dict:erase(QueryRef, Refs),
            gen_server:reply(From, ResultF),
            put(query_refs_count, RefsCount-1),
            State#state{query_refs=Refs2, query_refs_count=RefsCount-1};
        error ->
%           lager:warning("Ignore response ~p ~p", [QueryRef, ResultF()]),
            State
    end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Addr, Port, ClientOptions]) ->
    async_spawn(Addr, Port, ClientOptions),
    State = #state{host=Host},
    {ok, State}.

init_connection(ConnPid, State=#state{host=Host}) ->
    erlang:monitor(process, ConnPid),
    register_worker(Host, self()),

    InsertQuery = prepare_query(ConnPid, insert_query_sql()),
    TestQuery = prepare_query(ConnPid, test_query_sql()),
    RemoveArchiveQuery = prepare_query(ConnPid, remove_archive_query_sql()),

    ExHandler = extract_messages_handler(ConnPid),
    RevExHandler = extract_messages_r_handler(ConnPid),
    CountHandler = calc_count_handler(ConnPid),
    put(query_refs_count, 0),
    State#state{
        host=Host,
        conn=ConnPid,
        query_refs=dict:new(),
        query_refs_count=0,
        insert_query=InsertQuery,
        test_query=TestQuery,
        remove_archive_query=RemoveArchiveQuery,
        extract_messages_handler=ExHandler,
        extract_messages_r_handler=RevExHandler,
        calc_count_handler=CountHandler}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_, From, State=#state{conn=undefined}) ->
    {reply, no_connection, State};
handle_call({extract_messages, Args}, From,
    State=#state{conn=ConnPid, extract_messages_handler=Handler}) ->
    QueryRef = execute_extract_messages(ConnPid, Handler, Args),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({extract_messages_r, Args}, From,
    State=#state{conn=ConnPid, extract_messages_r_handler=Handler}) ->
    QueryRef = execute_extract_messages(ConnPid, Handler, Args),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({calc_count, Filter}, From,
    State=#state{conn=ConnPid, calc_count_handler=Handler}) ->
    QueryRef = execute_calc_count(ConnPid, Handler, Filter),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call(test_query, From,
    State=#state{conn=ConnPid, test_query=TestQuery}) ->
    QueryRef = execute_prepared_query(ConnPid, TestQuery, []),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call(_, _From, State=#state{}) ->
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(_, State=#state{conn=undefined}) ->
    {noreply, State};
handle_cast({write_message, Message},
    State=#state{conn=ConnPid, insert_query=InsertQuery}) ->
    Params = message_to_params(Message),
    execute_prepared_query(ConnPid, InsertQuery, Params),
    {noreply, State};
handle_cast({remove_archive, BUserJID},
    State=#state{conn=ConnPid, remove_archive_query=RemoveArchiveQuery}) ->
    Params = [BUserJID],
    execute_prepared_query(ConnPid, RemoveArchiveQuery, Params),
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Strange cast message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------


handle_info({connection_result, {ok, ConnPid}}, State=#state{conn=undefined}) ->
    State2 = init_connection(ConnPid, State),
    {noreply, State2};
handle_info({connection_result, Reason}, State=#state{conn=undefined}) ->
    ?ERROR_MSG("issue=\"Fail to connect to Cassandra\", reason=~1000p", [Reason]),
    {stop, {connection_result, Reason}, State};
handle_info(_, State=#state{conn=undefined}) ->
    {noreply, State};
handle_info({seestar_response, QueryRef, ResultF}, State) ->
    {noreply, forward_query_respond(ResultF, QueryRef, State)};
handle_info({'DOWN', _, process, Pid, Reason}, State=#state{conn=Pid}) ->
    ?ERROR_MSG("issue=\"Cassandra connection closed\", reason=~1000p", [Reason]),
    {stop, {dead_connection, Pid, Reason}, State};
handle_info(Msg, State) ->
    ?WARNING_MSG("Strange info message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers
%%--------------------------------------------------------------------

async_spawn(Addr, Port, ClientOptions) ->
    ?INFO_MSG("issue=\"Connecting to Cassandra\", address=~p, port=~p", [Addr, Port]),
    Parent = self(),
    proc_lib:spawn_link(fun() ->
          ConnectOptions = proplists:get_value(socket_options, ClientOptions, []),
          ?DEBUG("issue=\"seestar_session:start_link\", address=~p, port=~p, "
                 "client_options=~p, connect_options=~p",
                 [Addr, Port, ClientOptions, ConnectOptions]),
          Res = (catch seestar_session:start_link(Addr, Port, ClientOptions, ConnectOptions)),
          ?DEBUG("issue=\"seestar_session:start_link result\", result=~p",
                 [Res]),
          Parent ! {connection_result, Res},
          case Res of
              {ok, Pid} ->
                  Mon = erlang:monitor(process, Pid),
                  receive
                      {'DOWN', Mon, process, Pid, _} -> ok
                  end;
              _ ->
                ok
          end
      end).

%% ----------------------------------------------------------------------
%% Optimizations

packet_to_stored_binary(Packet) ->
    %% Module implementing mam_message behaviour
    Module = db_message_format(),
    Module:encode(Packet).

stored_binary_to_packet(Bin) ->
    %% Module implementing mam_message behaviour
    Module = db_message_format(),
    Module:decode(Bin).

%% ----------------------------------------------------------------------
%% Dynamic params module

%% compile_params_module([
%%      {db_message_format, module()}
%%      ])
compile_params_module(Params) ->
    CodeStr = params_helper(expand_simple_param(Params)),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_mam_ca_arch_params.erl", Code).

expand_simple_param(Params) ->
    lists:flatmap(fun(simple) -> simple_params();
                     ({simple,true}) -> simple_params();
                     (Param) -> [Param]
                  end, Params).

simple_params() ->
    [{db_message_format, mam_message_xml}].

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
        "-module(mod_mam_ca_arch_params).~n"
        "-compile(export_all).~n"
        "db_message_format() -> ~p.~n",
        [proplists:get_value(db_message_format, Params, mam_message_compressed_eterm)]))).

-spec db_message_format() -> module().
db_message_format() ->
    mod_mam_ca_arch_params:db_message_format().
