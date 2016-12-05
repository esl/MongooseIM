%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_cassandra_arch).
-behaviour(mongoose_cassandra).

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

%% mongoose_cassandra_worker callbacks
-export([prepared_queries/0]).

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

-callback encode(binary()) -> binary().
-callback decode(binary()) -> binary().

-record(mam_ca_filter, {
          user_jid,
          with_jid,
          remote_jid,
          start_id,
          end_id
         }).

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
    start_pm(Host, Opts).

stop(Host) ->
    stop_pm(Host).

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
%% mongoose_cassandra_worker callbacks

prepared_queries() ->
    [{insert_query, insert_query_cql()},
     {delete_query, delete_query_cql()},
     {remove_archive_query, remove_archive_query_cql()},
     {message_id_to_remote_jid_query, message_id_to_remote_jid_cql()}]
        ++ extract_messages_queries()
        ++ extract_messages_r_queries()
        ++ calc_count_queries()
        ++ list_message_ids_queries().

%% ----------------------------------------------------------------------
%% Helpers

select_worker(UserJID) ->
    PoolName = pool_name(UserJID),
    mongoose_cassandra_sup:select_worker(PoolName, UserJID).

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

archive_size(Size, Host, _UserID, UserJID) when is_integer(Size) ->
    Worker = select_worker(UserJID),
    Borders = Start = End = WithJID = undefined,
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    calc_count(Worker, UserJID, Host, Filter).


%% ----------------------------------------------------------------------
%% INSERT MESSAGE

insert_query_cql() ->
    "INSERT INTO mam_message "
        "(id, user_jid, from_jid, remote_jid, with_jid, message) "
        "VALUES (?,?,?,?,?,?)".

archive_message(Result, Host, MessID, _UserID,
                LocJID, RemJID, SrcJID, Dir, Packet) ->
    try
        archive_message2(Result, Host, MessID,
                         LocJID, RemJID, SrcJID, Dir, Packet)
    catch _Type:Reason ->
            {error, Reason}
    end.

archive_message2(_Result, _Host, MessID,
                  LocJID = #jid{},
                  RemJID = #jid{},
                  SrcJID = #jid{}, _Dir, Packet) ->
    BLocJID = bare_jid(LocJID),
    BRemBareJID = bare_jid(RemJID),
    BRemFullJID = full_jid(RemJID),
    BSrcJID = full_jid(SrcJID),
    BPacket = packet_to_stored_binary(Packet),
    Message = #mam_message{
                 id         = MessID,
                 user_jid   = BLocJID,
                 from_jid   = BSrcJID,
                 remote_jid = BRemFullJID,
                 message    = BPacket
                },
    WithJIDs = lists:usort([<<>>, BRemFullJID, BRemBareJID]),
    Messages = [Message#mam_message{with_jid = BWithJID} || BWithJID <- WithJIDs],
    write_messages(LocJID, Messages).

write_messages(UserJID, Messages) ->
    PoolName = pool_name(UserJID),
    MultiParams = [message_to_params(M) || M <- Messages],
    mongoose_cassandra_worker:cql_query_pool_multi_async(PoolName, UserJID, ?MODULE, insert_query,
                                                         MultiParams).

message_to_params(#mam_message{
                     id         = MessID,
                     user_jid   = BLocJID,
                     from_jid   = BSrcJID,
                     remote_jid = BRemJID,
                     with_jid   = BWithJID,
                     message    = BPacket
                    }) ->
    [MessID, BLocJID, BSrcJID, BRemJID, BWithJID, BPacket].


%% ----------------------------------------------------------------------
%% DELETE MESSAGE

delete_query_cql() ->
    "DELETE FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? AND id = ?".

delete_messages(Worker, UserJID, Messages) ->
    MultiParams = [delete_message_to_params(M) || M <- Messages],
    mongoose_cassandra_worker:cql_query_multi_async(Worker, UserJID, ?MODULE, delete_query,
                                                    MultiParams).

delete_message_to_params(#mam_message{
                            id       = MessID,
                            user_jid = BLocJID,
                            with_jid = BWithJID
                           }) ->
    [BLocJID, BWithJID, MessID].


%% ----------------------------------------------------------------------
%% REMOVE ARCHIVE

remove_archive_query_cql() ->
    "DELETE FROM mam_message WHERE user_jid = ?".

remove_archive(_Host, _UserID, UserJID) ->
    BUserJID = bare_jid(UserJID),
    PoolName = pool_name(UserJID),
    Params = [BUserJID],
    %% Wait until deleted
    mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, remove_archive_query,
                                             Params),
    ok.


%% ----------------------------------------------------------------------
%% GET FULL REMOTE JID

message_id_to_remote_jid_cql() ->
    "SELECT remote_jid FROM mam_message "
        "WHERE user_jid = ? AND with_jid = '' AND id = ?".

message_id_to_remote_jid(Worker, UserJID, BUserJID, MessID) ->
    Params = [BUserJID, MessID],
    {ok, Rows} = mongoose_cassandra_worker:cql_query(Worker, UserJID, ?MODULE,
                                                     message_id_to_remote_jid_query, Params),
    case Rows of
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
lookup_messages({error, _Reason} = Result, _Host,
                _UserID, _UserJID, _RSM, _Borders,
                _Start, _End, _Now, _WithJID,
                _PageSize, _LimitPassed, _MaxResultLimit,
                _IsSimple) ->
    Result;
lookup_messages(_Result, Host,
                _UserID, UserJID, RSM, Borders,
                Start, End, _Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit,
                IsSimple) ->
    try
        Worker = select_worker(UserJID),
        lookup_messages2(Worker, Host,
                         UserJID, RSM, Borders,
                         Start, End, WithJID,
                         PageSize, LimitPassed, MaxResultLimit,
                         IsSimple)
    catch _Type:Reason ->
            S = erlang:get_stacktrace(),
            {error, {Reason, S}}
    end.

lookup_messages2(Worker, Host,
                  UserJID = #jid{}, RSM, Borders,
                 Start, End, WithJID,
                 PageSize, _LimitPassed, _MaxResultLimit,
                  _IsSimple = true) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    lookup_messages_simple(Worker, Host, UserJID, RSM, PageSize, Filter);
lookup_messages2(Worker, Host,
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
    MessageRows = extract_messages(Worker, UserJID, Host, after_id(ID, Filter), PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(Worker, Host, UserJID,
                       #rsm_in{direction = before, id = ID},
                       PageSize, Filter) ->
    MessageRows = extract_messages(Worker, UserJID, Host, before_id(ID, Filter), PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(Worker, Host, UserJID,
                       #rsm_in{direction = undefined, index = Offset},
                       PageSize, Filter) ->
    %% Apply offset
    StartId = offset_to_start_id(Worker, UserJID, Filter,
                                 Offset), %% POTENTIALLY SLOW AND NOT SIMPLE :)
    MessageRows = extract_messages(Worker, UserJID, Host, from_id(StartId, Filter), PageSize,
                                   false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(Worker, Host, UserJID,
                       _,
                       PageSize, Filter) ->
    MessageRows = extract_messages(Worker, UserJID, Host, Filter, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}}.

lookup_messages_last_page(Worker, Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          0, Filter) ->
    %% Last page
    TotalCount = calc_count(Worker, UserJID, Host, Filter),
    {ok, {TotalCount, TotalCount, []}};
lookup_messages_last_page(Worker, Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(Worker, UserJID, Host, Filter, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(Worker, UserJID, Host, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_by_offset(Worker, Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          0, Filter) when is_integer(Offset) ->
    %% By offset
    TotalCount = calc_count(Worker, UserJID, Host, Filter),
    {ok, {TotalCount, Offset, []}};
lookup_messages_by_offset(Worker, Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) when is_integer(Offset) ->
    %% By offset
    StartId = offset_to_start_id(Worker, UserJID, Filter, Offset), %% POTENTIALLY SLOW
    MessageRows = extract_messages(Worker, UserJID, Host, from_id(StartId, Filter), PageSize,
                                   false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Worker, UserJID, Host, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_first_page(Worker, Host, UserJID,
                           _,
                           0, Filter) ->
    %% First page, just count
    TotalCount = calc_count(Worker, UserJID, Host, Filter),
    {ok, {TotalCount, 0, []}};
lookup_messages_first_page(Worker, Host, UserJID,
                           _,
                           PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(Worker, UserJID, Host, Filter, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            %% Total number of messages is less than one page
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(Worker, UserJID, Host, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_before_id(Worker, Host, UserJID,
                          RSM = #rsm_in{direction = before, id = ID},
                          PageSize, Filter) ->
    TotalCount = calc_count(Worker, UserJID, Host, Filter),
    Offset = calc_offset(Worker, UserJID, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Worker, UserJID, Host, before_id(ID, Filter), PageSize, true),
    {ok, {TotalCount, Offset, rows_to_uniform_format(MessageRows)}}.

lookup_messages_after_id(Worker, Host, UserJID,
                         RSM = #rsm_in{direction = aft, id = ID},
                         PageSize, Filter) ->
    TotalCount = calc_count(Worker, UserJID, Host, Filter),
    Offset = calc_offset(Worker, UserJID, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(Worker, UserJID, Host, after_id(ID, Filter), PageSize, false),
    {ok, {TotalCount, Offset, rows_to_uniform_format(MessageRows)}}.


check_result_for_policy_violation(Result = {ok, {TotalCount, Offset, _}},
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


after_id(ID, Filter = #mam_ca_filter{start_id = AfterID}) ->
    Filter#mam_ca_filter{start_id = maybe_max(ID + 1, AfterID)}.

before_id(undefined, Filter) ->
    Filter;
before_id(ID, Filter = #mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID - 1, BeforeID)}.

to_id(ID, Filter = #mam_ca_filter{end_id = BeforeID}) ->
    Filter#mam_ca_filter{end_id = maybe_min(ID, BeforeID)}.

from_id(ID, Filter = #mam_ca_filter{start_id = AfterID}) ->
    Filter#mam_ca_filter{start_id = maybe_max(ID, AfterID)}.


rows_to_uniform_format(MessageRows) ->
    [row_to_uniform_format(Row) || Row <- MessageRows].

row_to_uniform_format([MessID, BSrcJID, Data]) ->
    SrcJID = unserialize_jid(BSrcJID),
    Packet = stored_binary_to_packet(Data),
    {MessID, SrcJID, Packet}.

row_to_message_id([MessID, _, _]) ->
    MessID.

-spec purge_single_message(_Result, Host, MessID, _UserID, UserJID,
                           Now) ->
                                  ok  | {error, 'not-supported'} when
      Host :: server_host(), MessID :: message_id(),
      _UserID :: user_id(), UserJID :: jid(),
      Now :: unix_timestamp().
purge_single_message(_Result, _Host, MessID, _UserID, UserJID, _Now) ->
    Worker = select_worker(UserJID),
    BUserJID = bare_jid(UserJID),
    Result = message_id_to_remote_jid(Worker, UserJID, BUserJID, MessID),
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
                           id         = MessID,
                           user_jid   = BUserJID,
                           remote_jid = BRemFullJID, %% set the field for debugging
                           with_jid   = BWithJID
                          }           || BWithJID <- BWithJIDs],
            delete_messages(Worker, UserJID, Messages),
            ok;
        {error, _} ->
            ok
    end.

-spec purge_multiple_messages(_Result, Host, _UserID, UserJID, Borders,
                              Start, End, Now, WithJID) ->
                                     ok when
      Host :: server_host(), _UserID :: user_id(),
      UserJID :: jid(), Borders :: mam_borders(),
      Start :: unix_timestamp()  | undefined,
      End :: unix_timestamp()  | undefined,
      Now :: unix_timestamp(),
      WithJID :: jid()  | undefined.
purge_multiple_messages(_Result, Host, UserID, UserJID, Borders,
                        Start, End, Now, WithJID) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    PoolName = pool_name(UserJID),
    Limit = 500, %% TODO something smarter
    QueryName = {list_message_ids_query, select_filter(Filter)},
    Params = eval_filter_params(Filter) ++ [Limit],
    {ok, Rows} = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, QueryName,
                                                          Params),
    %% TODO can be faster
    %% TODO rate limiting
    [purge_single_message(ok, Host, MessID, UserID, UserJID, Now) || [MessID] <- Rows],
    ok.


%% Offset is not supported
%% Each record is a tuple of form
%% `{<<"13663125233">>,<<"bob@localhost">>,<<"res1">>,<<binary>>}'.
%% Columns are `["id","from_jid","message"]'.
-spec extract_messages(Worker, UserJID, Host, Filter, IMax, ReverseLimit) ->
                              [Row] when
      Worker :: worker(),
      UserJID :: jlib:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      IMax :: pos_integer(),
      ReverseLimit :: boolean(),
      Row :: list().
extract_messages(_Worker, _UserJID, _Host, _Filter, 0, _) ->
    [];
extract_messages(Worker, UserJID, _Host, Filter, IMax, false) ->
    QueryName = {extract_messages_query, select_filter(Filter)},
    Params = eval_filter_params(Filter) ++ [IMax],
    {ok, Rows} = mongoose_cassandra_worker:cql_query(Worker, UserJID, ?MODULE, QueryName, Params),
    Rows;
extract_messages(Worker, UserJID, _Host, Filter, IMax, true) ->
    QueryName = {extract_messages_r_query, select_filter(Filter)},
    Params = eval_filter_params(Filter) ++ [IMax],
    {ok, Rows} = mongoose_cassandra_worker:cql_query(Worker, UserJID, ?MODULE, QueryName, Params),
    lists:reverse(Rows).


%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
-spec calc_index(Worker, UserJID, Host, Filter, MessID) -> Count
                                                               when
      Worker :: worker(),
      UserJID :: jlib:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_index(Worker, UserJID, Host, Filter, MessID) ->
    calc_count(Worker, UserJID, Host, to_id(MessID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
-spec calc_before(Worker, UserJID, Host, Filter, MessID) -> Count
                                                                when
      Worker :: worker(),
      UserJID :: jlib:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_before(Worker, UserJID, Host, Filter, MessID) ->
    calc_count(Worker, UserJID, Host, before_id(MessID, Filter)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(Worker, UserJID, Host, Filter) -> Count
                                                       when
      Worker :: worker(),
      UserJID :: jlib:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      Count :: non_neg_integer().
calc_count(Worker, UserJID, _Host, Filter) ->
    QueryName = {calc_count_query, select_filter(Filter)},
    Params = eval_filter_params(Filter),
    {ok, [[Count]]} = mongoose_cassandra_worker:cql_query(Worker, UserJID, ?MODULE, QueryName,
                                                          Params),
    Count.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
-spec offset_to_start_id(Worker, UserJID, Filter, Offset) -> Id
                                                                 when
      Worker :: worker(),
      UserJID :: jlib:jid(),
      Offset :: non_neg_integer(),
      Filter :: filter(),
      Id :: non_neg_integer() | undefined.
offset_to_start_id(Worker, UserJID, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    QueryName = {list_message_ids_query, select_filter(Filter)},
    Params = eval_filter_params(Filter) ++ [Offset + 1],
    {ok, RowsIds} = mongoose_cassandra_worker:cql_query(Worker, UserJID, ?MODULE, QueryName,
                                                        Params),
    case RowsIds of
        [] -> unfefined;
        [_ | _] -> [StartId] = lists:last(RowsIds), StartId
    end.

prepare_filter(UserJID, Borders, Start, End, WithJID) ->
    BUserJID = bare_jid(UserJID),
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID = maybe_encode_compact_uuid(End, 255),
    StartID2 = apply_start_border(Borders, StartID),
    EndID2 = apply_end_border(Borders, EndID),
    BWithJID = maybe_full_jid(WithJID), %% it's NOT optional field
    prepare_filter_params(BUserJID, BWithJID, StartID2, EndID2).

prepare_filter_params(BUserJID, BWithJID, StartID, EndID) ->
    #mam_ca_filter{
       user_jid = BUserJID,
       with_jid = BWithJID,
       start_id = StartID,
       end_id   = EndID
      }.

eval_filter_params(#mam_ca_filter{
                      user_jid = BUserJID,
                      with_jid = BWithJID,
                      start_id = StartID,
                      end_id   = EndID
                     }) ->
    Optional = [Value || Value <- [StartID, EndID], Value =/= undefined],
    [BUserJID, BWithJID | Optional].

select_filter(#mam_ca_filter{
                 start_id = StartID,
                 end_id   = EndID
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

prepare_filter_cql(StartID, EndID) ->
    case StartID of
        undefined -> "";
        _ -> " AND id >= ?"
    end ++
        case EndID of
            undefined -> "";
            _ -> " AND id <= ?"
        end.

filter_to_cql() ->
    [{select_filter(StartID, EndID),
      prepare_filter_cql(StartID, EndID)}
     || StartID <- [undefined, 0],
        EndID <- [undefined, 0]].

-spec calc_offset(Worker, UserJID, Host, Filter, PageSize, TotalCount, RSM) -> Offset
                                                                                   when
      Worker :: worker(),
      UserJID :: jlib:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      PageSize :: non_neg_integer(),
      TotalCount :: non_neg_integer(),
      RSM :: rsm_in() | undefined,
      Offset :: non_neg_integer().
% calc_offset(_W, _UserJID, _LS, _F, _PS, _TC, #rsm_in{direction = undefined, index = Index})
%   when is_integer(Index) ->
%     Index;
%% Requesting the Last Page in a Result Set
calc_offset(_W, _UserJID, _LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(Worker, UserJID, Host, F, PS, _TC, #rsm_in{direction = before, id = ID})
  when is_integer(ID) ->
    max(0, calc_before(Worker, UserJID, Host, F, ID) - PS);
calc_offset(Worker, UserJID, Host, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
  when is_integer(ID) ->
    calc_index(Worker, UserJID, Host, F, ID);
calc_offset(_W, _UserJID, _LS, _F, _PS, _TC, _RSM) ->
    0.

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    encode_compact_uuid(Microseconds, NodeID).

bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_binary(jid:to_bare(jid:to_lower(JID))).

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

extract_messages_queries() ->
    [{{extract_messages_query, FilterName}, extract_messages_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

extract_messages_r_queries() ->
    [{{extract_messages_r_query, FilterName}, extract_messages_r_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

calc_count_queries() ->
    [{{calc_count_query, FilterName}, calc_count_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

list_message_ids_queries() ->
    [{{list_message_ids_query, FilterName}, list_message_ids_cql(Filter)}
     || {FilterName, Filter} <- filter_to_cql()].

extract_messages_cql(Filter) ->
    "SELECT id, from_jid, message FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++
        Filter ++ " ORDER BY with_jid, id LIMIT ?".

extract_messages_r_cql(Filter) ->
    "SELECT id, from_jid, message FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++
        Filter ++ " ORDER BY with_jid DESC, id DESC LIMIT ?".

calc_count_cql(Filter) ->
    "SELECT COUNT(*) FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++ Filter.

list_message_ids_cql(Filter) ->
    "SELECT id FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++ Filter ++
        " ORDER BY with_jid, id LIMIT ?".


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
    code:load_binary(Mod, "mod_mam_cassandra_arch_params.erl", Code).

expand_simple_param(Params) ->
    lists:flatmap(fun(simple) -> simple_params();
                     ({simple, true}) -> simple_params();
                     (Param) -> [Param]
                  end, Params).

simple_params() ->
    [{db_message_format, mam_message_xml}].

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
                                      "-module(mod_mam_cassandra_arch_params).~n"
                                      "-compile(export_all).~n"
                                      "db_message_format() -> ~p.~n"
                                      "pool_name() -> ~p.~n",
                                      [proplists:get_value(db_message_format, Params,
                                                           mam_message_compressed_eterm),
                                       proplists:get_value(pool_name, Params, default)
                                      ]))).

-spec db_message_format() -> module().
db_message_format() ->
    mod_mam_cassandra_arch_params:db_message_format().

-spec pool_name(jlib:jid()) -> term().
pool_name(_UserJid) ->
    mod_mam_cassandra_arch_params:pool_name().
