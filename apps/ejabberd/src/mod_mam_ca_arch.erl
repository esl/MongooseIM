%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_ca_arch).

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

%% Helpers for debugging
-export([test_query/1,
         queue_length/1,
         queue_lengths/1]).

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
    delete_query,
    remove_archive_query,
    message_id_to_remote_jid_query,
    test_query,
    calc_count_handler,
    list_message_ids_handler,
    extract_messages_handler,
    extract_messages_r_handler,
    query_refs,
    query_refs_count}).

-record(mam_ca_filter, {
    user_jid,
    with_jid,
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
  remote_jid :: binary(),
  from_jid :: binary(),
  with_jid :: binary(),
  message :: binary()
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
    compile_params_module(Opts),
    create_worker_pool(Host),
    mod_mam_ca_sup:start(Host, cassandra_config(Host)),
    start_pm(Host, Opts).

stop(Host) ->
    stop_pm(Host),
    delete_worker_pool(Host),
    mod_mam_ca_sup:stop(Host).

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

archive_size(Size, Host, _UserID, UserJID) when is_integer(Size) ->
    Worker = select_worker(Host, UserJID),
    Borders = Start = End = WithJID = undefined,
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    calc_count(Worker, Host, Filter).


%% ----------------------------------------------------------------------
%% INSERT MESSAGE

insert_query_sql() ->
    "INSERT INTO mam_message "
        "(id, user_jid, from_jid, remote_jid, with_jid, message) "
        "VALUES (?,?,?,?,?,?)".

archive_message(Result, Host, MessID, _UserID,
                     LocJID, RemJID, SrcJID, Dir, Packet) ->
    try
        archive_message_2(Result, Host, MessID,
                        LocJID, RemJID, SrcJID, Dir, Packet)
    catch _Type:Reason ->
        {error, Reason}
    end.

archive_message_2(_Result, Host, MessID,
                  LocJID=#jid{},
                  RemJID=#jid{},
                 SrcJID=#jid{}, _Dir, Packet) ->
    BLocJID = bare_jid(LocJID),
    BRemBareJID = bare_jid(RemJID),
    BRemFullJID = full_jid(RemJID),
    BSrcJID = full_jid(SrcJID),
    BPacket = packet_to_stored_binary(Packet),
    Message = #mam_message{
      id = MessID,
      user_jid = BLocJID,
      from_jid = BSrcJID,
      remote_jid = BRemFullJID,
      message = BPacket
    },
    WithJIDs = lists:usort([<<>>, BRemFullJID, BRemBareJID]),
    Messages = [Message#mam_message{with_jid = BWithJID} || BWithJID <- WithJIDs],
    Worker = select_worker(Host, LocJID),
    write_messages(Worker, Messages).

write_messages(Worker, Messages) ->
    gen_server:cast(Worker, {write_messages, Messages}).

message_to_params(#mam_message{
      id = MessID,
      user_jid = BLocJID,
      from_jid = BSrcJID,
      remote_jid = BRemJID,
      with_jid = BWithJID,
      message = BPacket
    }) ->
    [MessID, BLocJID, BSrcJID, BRemJID, BWithJID, BPacket].


%% ----------------------------------------------------------------------
%% DELETE MESSAGE

delete_query_sql() ->
    "DELETE FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? AND id = ?".

delete_messages(Worker, Messages) ->
    gen_server:cast(Worker, {delete_messages, Messages}).

delete_message_to_params(#mam_message{
      id = MessID,
      user_jid = BLocJID,
      with_jid = BWithJID
    }) ->
    [BLocJID, BWithJID, MessID].


%% ----------------------------------------------------------------------
%% TEST CONNECTION

test_query_sql() ->
    "SELECT now() FROM system.local". %% "SELECT 1" for cassandra

test_query(Host) ->
    Workers = pg2:get_local_members(group_name(Host)),
    [{Worker, (catch gen_server:call(Worker, test_query))} || Worker <- Workers].


%% ----------------------------------------------------------------------
%% QUEUE LENGTH

%% For metrics.
queue_length(Host) ->
    Len = lists:sum(queue_lengths(Host)),
    {ok, Len}.

queue_lengths(Host) ->
    Workers = pg2:get_local_members(group_name(Host)),
    [worker_queue_length(Worker) || Worker <- Workers].

worker_queue_length(Worker) ->
    %% We really don't want to call process, because it can does not respond
    Info = erlang:process_info(Worker, [message_queue_len, dictionary]),
    case Info of
    undefined -> %% dead
        0;
    [{message_queue_len, ExtLen}, {dictionary, Dict}] ->
        %% External queue contains not only queued queries but also waiting responds.
        %% But it's usually 0.
        IntLen = proplists:get_value(query_refs_count, Dict, 0),
        ExtLen + IntLen
    end.


%% ----------------------------------------------------------------------
%% REMOVE ARCHIVE

remove_archive_query_sql() ->
    "DELETE FROM mam_message WHERE user_jid = ?".

remove_archive(Host, _UserID, UserJID) ->
    Worker = select_worker(Host, UserJID),
    BUserJID = bare_jid(UserJID),
    %% Wait until deleted
    gen_server:call(Worker, {remove_archive, BUserJID}),
    ok.


%% ----------------------------------------------------------------------
%% GET FULL REMOTE JID

message_id_to_remote_jid_sql() ->
    "SELECT remote_jid FROM mam_message "
        "WHERE user_jid = ? AND with_jid = '' AND id = ?".

message_id_to_remote_jid(Worker, BUserJID, MessID) ->
    ResultF = gen_server:call(Worker,
        {message_id_to_remote_jid, BUserJID, MessID}),
    {ok, Result} = ResultF(),
    case seestar_result:rows(Result) of
        [] ->
            {error, not_found};
        [[BRemFullJID]] ->
            {ok, BRemFullJID}
    end.


%% ----------------------------------------------------------------------
%% SELECT MESSAGES

-spec lookup_messages(Result :: any(), Host :: ejabberd:server(),
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
    {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.
lookup_messages({error, _Reason}=Result, _Host,
                     _UserID, _UserJID, _RSM, _Borders,
                     _Start, _End, _Now, _WithJID,
                     _PageSize, _LimitPassed, _MaxResultLimit,
                     _IsSimple) ->
    Result;
lookup_messages(_Result, Host,
                _UserID, UserJID, RSM, Borders,
                Start, End, Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit,
                IsSimple) ->
    Worker = select_worker(Host, UserJID),
    try
        lookup_messages_2(Worker, Host,
                          UserJID, RSM, Borders,
                          Start, End, WithJID,
                          PageSize, LimitPassed, MaxResultLimit,
                          IsSimple)
    catch _Type:Reason ->
        S = erlang:get_stacktrace(),
        {error, {Reason, S}}
    end.

lookup_messages_2(Worker, Host,
                  UserJID = #jid{}, RSM, Borders,
                  Start, End, WithJID,
                  PageSize, _LimitPassed, _MaxResultLimit,
                  _IsSimple=true) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    lookup_messages_simple(Worker, Host, UserJID, RSM, PageSize, Filter);
lookup_messages_2(Worker, Host,
                  UserJID = #jid{}, RSM, Borders,
                  Start, End, WithJID,
                  PageSize, LimitPassed, MaxResultLimit,
                  _IsSimple) ->
    %% Query with offset calculation
    %% We cannot just use ODBC code because "LIMIT X,Y" is not supported by cassandra
    %% Not all queries are optimal. You would like to disable something for production
    %% once you know how you will call bd
    Strategy = rsm_to_strategy(RSM),
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    Result =
    case Strategy of
        last_page ->
            lookup_messages_last_page(Worker, Host, UserJID, RSM, PageSize, Filter);
        by_offset ->
            lookup_messages_by_offset(Worker, Host, UserJID, RSM, PageSize, Filter);
        first_page ->
            lookup_messages_first_page(Worker, Host, UserJID, RSM, PageSize, Filter);
        before_id ->
            lookup_messages_before_id(Worker, Host, UserJID, RSM, PageSize, Filter);
        after_id ->
            lookup_messages_after_id(Worker, Host, UserJID, RSM, PageSize, Filter)
    end,
    check_result_for_policy_violation(Result, MaxResultLimit, LimitPassed).

rsm_to_strategy(#rsm_in{direction = before, id = undefined}) ->
    last_page;
rsm_to_strategy(#rsm_in{direction = undefined, index = 0}) ->
    first_page;
rsm_to_strategy(#rsm_in{direction = undefined, index = Offset}) when is_integer(Offset) ->
    by_offset;
rsm_to_strategy(#rsm_in{direction = before, id = Id}) when is_integer(Id) ->
    before_id;
rsm_to_strategy(#rsm_in{direction = aft, id = Id}) when is_integer(Id) ->
    after_id;
rsm_to_strategy(#rsm_in{}) ->
    first_page;
rsm_to_strategy(undefined) ->
    first_page.

lookup_messages_simple(Worker, Host, UserJID,
                       #rsm_in{direction = aft, id = ID},
                       PageSize, Filter) ->
    %% Get last rows from result set
    MessageRows = extract_messages(Worker, Host, after_id(ID, Filter), PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(Worker, Host, UserJID,
                       #rsm_in{direction = before, id = ID},
                       PageSize, Filter) ->
    MessageRows = extract_messages(Worker, Host, before_id(ID, Filter), PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(Worker, Host, UserJID,
                       #rsm_in{direction = undefined, index = Offset},
                       PageSize, Filter) ->
    %% Apply offset
    StartId = offset_to_start_id(Worker, Filter, Offset), %% POTENTIALLY SLOW AND NOT SIMPLE :)
    MessageRows = extract_messages(Worker, Host, from_id(StartId, Filter), PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(Worker, Host, UserJID,
                _,
                PageSize, Filter) ->
    MessageRows = extract_messages(Worker, Host, Filter, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}}.

lookup_messages_last_page(Worker, Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          0, Filter) ->
    %% Last page
    TotalCount = calc_count(Worker, Host, Filter),
    {ok, {TotalCount, TotalCount, []}};
lookup_messages_last_page(Worker, Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(Worker, Host, Filter, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Worker, Host, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_by_offset(Worker, Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          0, Filter) when is_integer(Offset) ->
    %% By offset
    TotalCount = calc_count(Worker, Host, Filter),
    {ok, {TotalCount, Offset, []}};
lookup_messages_by_offset(Worker, Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) when is_integer(Offset) ->
    %% By offset
    StartId = offset_to_start_id(Worker, Filter, Offset), %% POTENTIALLY SLOW
    MessageRows = extract_messages(Worker, Host, from_id(StartId, Filter), PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Worker, Host, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_first_page(Worker, Host, UserJID,
                           _,
                           0, Filter) ->
    %% First page, just count
    TotalCount = calc_count(Worker, Host, Filter),
    {ok, {TotalCount, 0, []}};
lookup_messages_first_page(Worker, Host, UserJID,
                           _,
                           PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(Worker, Host, Filter, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            %% Total number of messages is less than one page
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Worker, Host, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_before_id(Worker, Host, UserJID,
                          RSM = #rsm_in{direction = before, id = ID},
                          PageSize, Filter) ->
    TotalCount = calc_count(Worker, Host, Filter),
    Offset     = calc_offset(Worker, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Worker, Host, before_id(ID, Filter), PageSize, true),
    {ok, {TotalCount, Offset, rows_to_uniform_format(MessageRows)}}.

lookup_messages_after_id(Worker, Host, UserJID,
                         RSM = #rsm_in{direction = aft, id = ID},
                         PageSize, Filter) ->
    TotalCount = calc_count(Worker, Host, Filter),
    Offset     = calc_offset(Worker, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Worker, Host, after_id(ID, Filter), PageSize, false),
    {ok, {TotalCount, Offset, rows_to_uniform_format(MessageRows)}}.


check_result_for_policy_violation(Result={ok, {TotalCount, Offset, _}},
                                  MaxResultLimit, LimitPassed)
    when is_integer(TotalCount), is_integer(Offset) ->
    %% If a query returns a number of stanzas greater than this limit and the
    %% client did not specify a limit using RSM then the server should return
    %% a policy-violation error to the client.
    case is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) of
        true ->
            {error, 'policy-violation'};
        false ->
            Result
    end;
check_result_for_policy_violation(Result, _MaxResultLimit, _LimitPassed) ->
    Result.

is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) ->
    TotalCount - Offset > MaxResultLimit andalso not LimitPassed.


after_id(ID, Filter=#mam_ca_filter{start_id = AfterID}) ->
    Filter#mam_ca_filter{start_id = maybe_max(ID + 1, AfterID)}.

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID - 1, BeforeID)}.

to_id(ID, Filter=#mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID, BeforeID)}.

from_id(ID, Filter=#mam_ca_filter{start_id = AfterID}) ->
    Filter#mam_ca_filter{start_id = maybe_max(ID, AfterID)}.


rows_to_uniform_format(MessageRows) ->
    [row_to_uniform_format(Row) || Row <- MessageRows].

row_to_uniform_format([MessID,BSrcJID,Data]) ->
    SrcJID = unserialize_jid(BSrcJID),
    Packet = stored_binary_to_packet(Data),
    {MessID, SrcJID, Packet}.

row_to_message_id([MessID,_,_]) ->
    MessID.

-spec purge_single_message(_Result, Host, MessID, _UserID, UserJID,
                           Now) ->
    ok  | {error, 'not-supported'} when
      Host :: server_host(), MessID :: message_id(),
      _UserID :: user_id(), UserJID :: #jid{},
      Now :: unix_timestamp().
purge_single_message(_Result, Host, MessID, _UserID, UserJID, _Now) ->
    Worker = select_worker(Host, UserJID),
    BUserJID = bare_jid(UserJID),
    Result = message_id_to_remote_jid(Worker, BUserJID, MessID),
    case Result of
        {ok, BRemFullJID} ->
            RemFullJID = unserialize_jid(BRemFullJID),
            RemBareJID = jid:to_bare(RemFullJID),
            BRemBareJID = jid:to_binary(RemBareJID),
            %% Remove duplicates if RemFullJID =:= RemBareJID
            BWithJIDs = lists:usort([BRemFullJID, BRemBareJID, <<>>]), %% 2 or 3
            %% Set some fields
            %% To remove record we need to know user_jid, with_jid and id.
            Messages = [#mam_message{
              id = MessID,
              user_jid = BUserJID,
              remote_jid = RemFullJID, %% set the field for debugging
              with_jid = BWithJID
              } || BWithJID <- BWithJIDs],
            delete_messages(Worker, Messages),
            ok;
        {error, _} ->
            ok
    end.

-spec purge_multiple_messages(_Result, Host, _UserID, UserJID, Borders,
                              Start, End, Now, WithJID) ->
    ok when
      Host :: server_host(), _UserID :: user_id(),
      UserJID :: #jid{}, Borders :: #mam_borders{},
      Start :: unix_timestamp()  | undefined,
      End :: unix_timestamp()  | undefined,
      Now :: unix_timestamp(),
      WithJID :: #jid{}  | undefined.
purge_multiple_messages(_Result, Host, UserID, UserJID, Borders,
                        Start, End, Now, WithJID) ->
   %% Simple query without calculating offset and total count
   Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
   Worker = select_worker(Host, UserJID),
   Limit = 500, %% TODO something smarter
   ResultF = gen_server:call(Worker, {list_message_ids, {Filter, Limit}}),
   {ok, Result} = ResultF(),
   MessIds = seestar_result:rows(Result),
   %% TODO can be faster
   %% TODO rate limiting
   [purge_single_message(ok, Host, MessID, UserID, UserJID, Now) || [MessID] <- MessIds],
   ok.


%% Offset is not supported
%% Each record is a tuple of form
%% `{<<"13663125233">>,<<"bob@localhost">>,<<"res1">>,<<binary>>}'.
%% Columns are `["id","from_jid","message"]'.
-spec extract_messages(Worker, Host, Filter, IMax, ReverseLimit) ->
    [Row] when
    Worker  :: worker(),
    Host    :: server_hostname(),
    Filter  :: filter(),
    IMax    :: pos_integer(),
    ReverseLimit :: boolean(),
    Row :: list().
extract_messages(_Worker, _Host, _Filter, 0, _) ->
    [];
extract_messages(Worker, _Host, Filter, IMax, false) ->
    ResultF = gen_server:call(Worker,
        {extract_messages, {Filter, IMax}}),
    {ok, Result} = ResultF(),
    seestar_result:rows(Result);
extract_messages(Worker, _Host, Filter, IMax, true) ->
    ResultF = gen_server:call(Worker,
        {extract_messages_r, {Filter, IMax}}),
    {ok, Result} = ResultF(),
    lists:reverse(seestar_result:rows(Result)).


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
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(Worker, Host, Filter) -> Count
    when
    Worker       :: worker(),
    Host         :: server_hostname(),
    Filter       :: filter(),
    Count        :: non_neg_integer().
calc_count(Worker, _Host, Filter) ->
    ResultF = gen_server:call(Worker, {calc_count, Filter}),
    {ok, Result} = ResultF(),
    [[Count]] = seestar_result:rows(Result),
    Count.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
-spec offset_to_start_id(Worker, Filter, Offset) -> Id
    when
    Worker       :: worker(),
    Offset       :: non_neg_integer(),
    Filter       :: filter(),
    Id           :: non_neg_integer() | undefined.
offset_to_start_id(Worker, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    ResultF = gen_server:call(Worker, {list_message_ids, {Filter, Offset+1}}),
    {ok, Result} = ResultF(),
    Ids = seestar_result:rows(Result),
    case Ids of
        [] -> unfefined;
        [_|_] -> [StartId] = lists:last(Ids), StartId
    end.

prepare_filter(UserJID, Borders, Start, End, WithJID) ->
    BUserJID = bare_jid(UserJID),
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID   = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2   = apply_end_border(Borders, EndID),
    BWithJID = maybe_full_jid(WithJID), %% it's NOT optional field
    prepare_filter_params(BUserJID, BWithJID, StartID2, EndID2).

prepare_filter_params(BUserJID, BWithJID, StartID, EndID) ->
    #mam_ca_filter{
        user_jid = BUserJID,
        with_jid = BWithJID,
        start_id = StartID,
        end_id = EndID
    }.

eval_filter_params(#mam_ca_filter{
        user_jid = BUserJID,
        with_jid = BWithJID,
        start_id = StartID,
        end_id = EndID
    }) ->
    Optional = [Value || Value <- [StartID, EndID], Value =/= undefined],
    [BUserJID, BWithJID|Optional].

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

bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_binary(jid:to_bare(jid:to_lower(JID))).

full_jid(undefined) -> undefined;
full_jid(JID) ->
    jid:to_binary(jid:to_lower(JID)).

maybe_full_jid(undefined) -> <<>>;
maybe_full_jid(JID) ->
    jid:to_binary(jid:to_lower(JID)).

unserialize_jid(BJID) ->
    jid:from_binary(BJID).

%%====================================================================
%% Internal SQL part
%%====================================================================

extract_messages_handler(Conn) ->
    dict:from_list([{FilterName, prepare_query(Conn, extract_messages_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_r_handler(Conn) ->
    dict:from_list([{FilterName, prepare_query(Conn, extract_messages_r_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

calc_count_handler(Conn) ->
    dict:from_list([{FilterName, prepare_query(Conn, calc_count_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

list_message_ids_handler(Conn) ->
    dict:from_list([{FilterName, prepare_query(Conn, list_message_ids_sql(Filter))}
                    || {FilterName, Filter} <- filter_to_sql()]).

extract_messages_sql(Filter) ->
    "SELECT id, from_jid, message FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++
        Filter ++ " ORDER BY with_jid, id LIMIT ?".

extract_messages_r_sql(Filter) ->
    "SELECT id, from_jid, message FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++
        Filter ++ " ORDER BY with_jid DESC, id DESC LIMIT ?".

calc_count_sql(Filter) ->
    "SELECT COUNT(*) FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++ Filter.

list_message_ids_sql(Filter) ->
    "SELECT id FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++ Filter ++
        " ORDER BY with_jid, id LIMIT ?".

execute_extract_messages(Conn, Handler, {Filter, IMax}) when is_integer(IMax) ->
    Params = eval_filter_params(Filter) ++ [IMax],
    FilterName = select_filter(Filter),
    PreparedQuery = dict:fetch(FilterName, Handler),
    execute_prepared_query(Conn, PreparedQuery, Params).

execute_calc_count(Conn, Handler, Filter) ->
    Params = eval_filter_params(Filter),
    FilterName = select_filter(Filter),
    PreparedQuery = dict:fetch(FilterName, Handler),
    execute_prepared_query(Conn, PreparedQuery, Params).

execute_list_message_ids(Conn, Handler, {Filter, IMax})  when is_integer(IMax) ->
    Params = eval_filter_params(Filter) ++ [IMax],
    FilterName = select_filter(Filter),
    PreparedQuery = dict:fetch(FilterName, Handler),
    execute_prepared_query(Conn, PreparedQuery, Params).

prepare_query(Conn, Query) ->
    {ok, Res} = seestar_session:prepare(Conn, Query),
    Types = seestar_result:types(Res),
    QueryID = seestar_result:query_id(Res),
    #prepared_query{query_id=QueryID, query_types=Types}.

execute_prepared_query(Conn, #prepared_query{query_id=QueryID, query_types=Types}, Params) ->
    seestar_session:execute_async(Conn, QueryID, Types, Params, one).


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

init_connection(ConnPid, Conn, State=#state{host=Host}) ->
    erlang:monitor(process, ConnPid),
    register_worker(Host, self()),

    InsertQuery = prepare_query(Conn, insert_query_sql()),
    DeleteQuery = prepare_query(Conn, delete_query_sql()),
    TestQuery = prepare_query(Conn, test_query_sql()),
    RemoveArchiveQuery = prepare_query(Conn, remove_archive_query_sql()),
    MessIdToRemJidQuery = prepare_query(Conn, message_id_to_remote_jid_sql()),

    ExHandler = extract_messages_handler(Conn),
    RevExHandler = extract_messages_r_handler(Conn),
    CountHandler = calc_count_handler(Conn),
    ListIdsHandler = list_message_ids_handler(Conn),
    put(query_refs_count, 0),
    State#state{
        host=Host,
        conn=Conn,
        query_refs=dict:new(),
        query_refs_count=0,
        insert_query=InsertQuery,
        delete_query=DeleteQuery,
        test_query=TestQuery,
        remove_archive_query=RemoveArchiveQuery,
        message_id_to_remote_jid_query=MessIdToRemJidQuery,
        extract_messages_handler=ExHandler,
        extract_messages_r_handler=RevExHandler,
        calc_count_handler=CountHandler,
        list_message_ids_handler=ListIdsHandler}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_, _From, State=#state{conn=undefined}) ->
    {reply, no_connection, State};
handle_call({extract_messages, Args}, From,
    State=#state{conn=Conn, extract_messages_handler=Handler}) ->
    QueryRef = execute_extract_messages(Conn, Handler, Args),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({extract_messages_r, Args}, From,
    State=#state{conn=Conn, extract_messages_r_handler=Handler}) ->
    QueryRef = execute_extract_messages(Conn, Handler, Args),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({calc_count, Filter}, From,
    State=#state{conn=Conn, calc_count_handler=Handler}) ->
    QueryRef = execute_calc_count(Conn, Handler, Filter),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({list_message_ids, Args}, From,
    State=#state{conn=Conn, list_message_ids_handler=Handler}) ->
    QueryRef = execute_list_message_ids(Conn, Handler, Args),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({message_id_to_remote_jid, BUserJID, MessID}, From,
    State=#state{conn=Conn, message_id_to_remote_jid_query=MessIdToRemJidQuery}) ->
    Params = [BUserJID, MessID],
    QueryRef = execute_prepared_query(Conn, MessIdToRemJidQuery, Params),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call(test_query, From,
    State=#state{conn=Conn, test_query=TestQuery}) ->
    QueryRef = execute_prepared_query(Conn, TestQuery, []),
    {noreply, save_query_ref(From, QueryRef, State)};
handle_call({remove_archive, BUserJID}, From,
    State=#state{conn=Conn, remove_archive_query=RemoveArchiveQuery}) ->
    Params = [BUserJID],
    QueryRef = execute_prepared_query(Conn, RemoveArchiveQuery, Params),
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
handle_cast({write_messages, Messages},
    State=#state{conn=Conn, insert_query=InsertQuery}) ->
    [execute_prepared_query(Conn, InsertQuery, message_to_params(M)) || M <- Messages],
    {noreply, State};
handle_cast({delete_messages, Messages},
    State=#state{conn=Conn, delete_query=DeleteQuery}) ->
    [execute_prepared_query(Conn, DeleteQuery, delete_message_to_params(M)) || M <- Messages],
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


handle_info({connection_result, {ok, ConnPid, Conn}}, State=#state{conn=undefined}) ->
    State2 = init_connection(ConnPid, Conn, State),
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
