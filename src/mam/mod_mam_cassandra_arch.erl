%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc ODBC backend for Message Archive Management.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_cassandra_arch).
-behaviour(mongoose_cassandra).
-behaviour(ejabberd_gen_mam_archive).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([archive_size/4,
         archive_message/9,
         lookup_messages/3,
         remove_archive/4,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% mongoose_cassandra callbacks
-export([prepared_queries/0]).

%% ----------------------------------------------------------------------
%% Imports

%% Other
-import(mod_mam_utils,
        [maybe_min/2,
         maybe_max/2,
         bare_jid/1,
         full_jid/1
         ]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_rsm.hrl").

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
          remote_jid :: binary() | undefined,
          from_jid :: binary() | undefined,
          with_jid = <<>> :: binary(),
          message :: binary() | undefined
         }).

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
    [
     {insert_offset_hint_query, insert_offset_hint_query_cql()},
     {prev_offset_query, prev_offset_query_cql()},
     {insert_query, insert_query_cql()},
     {delete_query, delete_query_cql()},
     {select_for_removal_query, select_for_removal_query_cql()},
     {remove_archive_query, remove_archive_query_cql()},
     {remove_archive_offsets_query, remove_archive_offsets_query_cql()},
     {message_id_to_remote_jid_query, message_id_to_remote_jid_cql()}]
        ++ extract_messages_queries()
        ++ extract_messages_r_queries()
        ++ calc_count_queries()
        ++ list_message_ids_queries().

%% ----------------------------------------------------------------------
%% Internal functions and callbacks

archive_size(Size, Host, _UserID, UserJID) when is_integer(Size) ->
    PoolName = pool_name(UserJID),
    Borders = Start = End = WithJID = undefined,
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    calc_count(PoolName, UserJID, Host, Filter).


%% ----------------------------------------------------------------------
%% INSERT MESSAGE

insert_query_cql() ->
    "INSERT INTO mam_message "
        "(id, user_jid, from_jid, remote_jid, with_jid, message) "
        "VALUES (?, ?, ?, ?, ?, ?)".

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
    mongoose_cassandra:cql_write_async(PoolName, UserJID, ?MODULE, insert_query, MultiParams).

message_to_params(#mam_message{
                     id         = MessID,
                     user_jid   = BLocJID,
                     from_jid   = BSrcJID,
                     remote_jid = BRemJID,
                     with_jid   = BWithJID,
                     message    = BPacket
                    }) ->
    #{id => MessID, user_jid => BLocJID, from_jid => BSrcJID,
      remote_jid => BRemJID, with_jid => BWithJID, message => BPacket}.


%% ----------------------------------------------------------------------
%% DELETE MESSAGE

delete_query_cql() ->
    "DELETE FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? AND id = ?".

delete_messages(PoolName, UserJID, Messages) ->
    MultiParams = [delete_message_to_params(M) || M <- Messages],
    mongoose_cassandra:cql_write(PoolName, UserJID, ?MODULE, delete_query,
                                 MultiParams).

delete_message_to_params(#mam_message{
                            id       = MessID,
                            user_jid = BLocJID,
                            with_jid = BWithJID
                           }) ->
    #{user_jid => BLocJID, with_jid => BWithJID, id => MessID}.


%% ----------------------------------------------------------------------
%% REMOVE ARCHIVE

remove_archive_query_cql() ->
    "DELETE FROM mam_message WHERE user_jid = ? AND with_jid = ?".

remove_archive_offsets_query_cql() ->
    "DELETE FROM mam_message WHERE user_jid = ? AND with_jid = ?".

select_for_removal_query_cql() ->
    "SELECT DISTINCT user_jid, with_jid FROM mam_message WHERE user_jid = ?".

remove_archive(Acc, _Host, _UserID, UserJID) ->
    BUserJID = bare_jid(UserJID),
    PoolName = pool_name(UserJID),
    Params = #{user_jid => BUserJID},
    %% Wait until deleted

    DeleteFun =
        fun(Rows, _AccIn) ->
                mongoose_cassandra:cql_write(PoolName, UserJID, ?MODULE,
                                             remove_archive_query, Rows),
                mongoose_cassandra:cql_write(PoolName, UserJID, ?MODULE,
                                             remove_archive_offsets_query, Rows)
        end,

    mongoose_cassandra:cql_foldl(PoolName, UserJID, ?MODULE,
                                 select_for_removal_query, Params, DeleteFun, []),
    Acc.


%% ----------------------------------------------------------------------
%% GET FULL REMOTE JID

message_id_to_remote_jid_cql() ->
    "SELECT remote_jid FROM mam_message "
        "WHERE user_jid = ? AND with_jid = '' AND id = ? ALLOW FILTERING".

message_id_to_remote_jid(PoolName, UserJID, BUserJID, MessID) ->
    Params = #{user_jid => BUserJID, id => MessID, with_jid => <<>>},
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE,
                                             message_id_to_remote_jid_query, Params),
    case Rows of
        [] ->
            {error, not_found};
        [#{remote_jid := RemoteJID}] ->
            {ok, RemoteJID}
    end.


%% ----------------------------------------------------------------------
%% SELECT MESSAGES

-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
  {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.
lookup_messages({error, _Reason} = Result, _Host, _Params) ->
    Result;
lookup_messages(_Result, _Host, #{search_text := <<_/binary>>}) ->
    {error, 'not-supported'};
lookup_messages(_Result, Host,
                #{owner_jid := UserJID, rsm := RSM, borders := Borders,
                  start_ts := Start, end_ts := End, with_jid := WithJID,
                  search_text := undefined, page_size := PageSize,
                  limit_passed := LimitPassed, max_result_limit := MaxResultLimit,
                  is_simple := IsSimple}) ->
    try
        PoolName = pool_name(UserJID),
        lookup_messages2(PoolName, Host,
                         UserJID, RSM, Borders,
                         Start, End, WithJID,
                         PageSize, LimitPassed, MaxResultLimit,
                         IsSimple)
    catch _Type:Reason ->
            S = erlang:get_stacktrace(),
            {error, {Reason, S}}
    end.

lookup_messages2(PoolName, Host,
                 UserJID = #jid{}, RSM, Borders,
                 Start, End, WithJID,
                 PageSize, _LimitPassed, _MaxResultLimit,
                 _IsSimple = true) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    lookup_messages_simple(PoolName, Host, UserJID, RSM, PageSize, Filter);
lookup_messages2(PoolName, Host,
                 UserJID = #jid{}, RSM, Borders,
                 Start, End, WithJID,
                 PageSize, LimitPassed, MaxResultLimit,
                 _IsSimple) ->
    %% Query with offset calculation
    %% We cannot just use ODBC code because "LIMIT X, Y" is not supported by cassandra
    %% Not all queries are optimal. You would like to disable something for production
    %% once you know how you will call bd
    Strategy = rsm_to_strategy(RSM),
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    Result =
        case Strategy of
            last_page ->
                lookup_messages_last_page(PoolName, Host, UserJID, RSM, PageSize, Filter);
            by_offset ->
                lookup_messages_by_offset(PoolName, Host, UserJID, RSM, PageSize, Filter);
            first_page ->
                lookup_messages_first_page(PoolName, Host, UserJID, RSM, PageSize, Filter);
            before_id ->
                lookup_messages_before_id(PoolName, Host, UserJID, RSM, PageSize, Filter);
            after_id ->
                lookup_messages_after_id(PoolName, Host, UserJID, RSM, PageSize, Filter)
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

lookup_messages_simple(PoolName, Host, UserJID,
                       #rsm_in{direction = aft, id = ID},
                       PageSize, Filter) ->
    %% Get last rows from result set
    MessageRows = extract_messages(PoolName, UserJID, Host, after_id(ID, Filter), PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(PoolName, Host, UserJID,
                       #rsm_in{direction = before, id = ID},
                       PageSize, Filter) ->
    MessageRows = extract_messages(PoolName, UserJID, Host, before_id(ID, Filter), PageSize, true),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(PoolName, Host, UserJID,
                       #rsm_in{direction = undefined, index = Offset},
                       PageSize, Filter) ->
    %% Apply offset
    StartId = offset_to_start_id(PoolName, UserJID, Filter,
                                 Offset), %% POTENTIALLY SLOW AND NOT SIMPLE :)
    MessageRows = extract_messages(PoolName, UserJID, Host, from_id(StartId, Filter), PageSize,
                                   false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}};
lookup_messages_simple(PoolName, Host, UserJID,
                       _,
                       PageSize, Filter) ->
    MessageRows = extract_messages(PoolName, UserJID, Host, Filter, PageSize, false),
    {ok, {undefined, undefined, rows_to_uniform_format(MessageRows)}}.

lookup_messages_last_page(PoolName, Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          0, Filter) ->
    %% Last page
    TotalCount = calc_count(PoolName, UserJID, Host, Filter),
    {ok, {TotalCount, TotalCount, []}};
lookup_messages_last_page(PoolName, Host, UserJID,
                          #rsm_in{direction = before, id = undefined},
                          PageSize, Filter) ->
    %% Last page
    MessageRows = extract_messages(PoolName, UserJID, Host, Filter, PageSize, true),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            FirstID = row_to_message_id(hd(MessageRows)),
            Offset = calc_count(PoolName, UserJID, Host, before_id(FirstID, Filter)),
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_by_offset(PoolName, Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          0, Filter) when is_integer(Offset) ->
    %% By offset
    TotalCount = calc_count(PoolName, UserJID, Host, Filter),
    {ok, {TotalCount, Offset, []}};
lookup_messages_by_offset(PoolName, Host, UserJID,
                          #rsm_in{direction = undefined, index = Offset},
                          PageSize, Filter) when is_integer(Offset) ->
    %% By offset
    StartId = offset_to_start_id(PoolName, UserJID, Filter, Offset), %% POTENTIALLY SLOW
    MessageRows = extract_messages(PoolName, UserJID, Host, from_id(StartId, Filter), PageSize,
                                   false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            {ok, {Offset + MessageRowsCount, Offset,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(PoolName, UserJID, Host, after_id(LastID, Filter)),
            {ok, {Offset + MessageRowsCount + CountAfterLastID, Offset,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_first_page(PoolName, Host, UserJID,
                           _,
                           0, Filter) ->
    %% First page, just count
    TotalCount = calc_count(PoolName, UserJID, Host, Filter),
    {ok, {TotalCount, 0, []}};
lookup_messages_first_page(PoolName, Host, UserJID,
                           _,
                           PageSize, Filter) ->
    %% First page
    MessageRows = extract_messages(PoolName, UserJID, Host, Filter, PageSize, false),
    MessageRowsCount = length(MessageRows),
    case MessageRowsCount < PageSize of
        true ->
            %% Total number of messages is less than one page
            {ok, {MessageRowsCount, 0,
                  rows_to_uniform_format(MessageRows)}};
        false ->
            LastID = row_to_message_id(lists:last(MessageRows)),
            CountAfterLastID = calc_count(PoolName, UserJID, Host, after_id(LastID, Filter)),
            {ok, {MessageRowsCount + CountAfterLastID, 0,
                  rows_to_uniform_format(MessageRows)}}
    end.

lookup_messages_before_id(PoolName, Host, UserJID,
                          RSM = #rsm_in{direction = before, id = ID},
                          PageSize, Filter) ->
    TotalCount = calc_count(PoolName, UserJID, Host, Filter),
    Offset = calc_offset(PoolName, UserJID, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(PoolName, UserJID, Host, before_id(ID, Filter), PageSize, true),
    {ok, {TotalCount, Offset, rows_to_uniform_format(MessageRows)}}.

lookup_messages_after_id(PoolName, Host, UserJID,
                         RSM = #rsm_in{direction = aft, id = ID},
                         PageSize, Filter) ->
    TotalCount = calc_count(PoolName, UserJID, Host, Filter),
    Offset = calc_offset(PoolName, UserJID, Host, Filter, PageSize, TotalCount, RSM),
    MessageRows = extract_messages(PoolName, UserJID, Host, after_id(ID, Filter), PageSize, false),
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

row_to_uniform_format(#{from_jid := FromJID, message := Msg, id := MsgID}) ->
    SrcJID = jid:from_binary(FromJID),
    Packet = stored_binary_to_packet(Msg),
    {MsgID, SrcJID, Packet}.

row_to_message_id(#{id := MsgID}) ->
    MsgID.

-spec purge_single_message(_Result, Host, MessID, _UserID, UserJID,
                           Now) ->
                                  ok  | {error, 'not-supported'} when
      Host :: server_host(), MessID :: message_id(),
      _UserID :: user_id(), UserJID :: jid:jid(),
      Now :: unix_timestamp().
purge_single_message(_Result, _Host, MessID, _UserID, UserJID, _Now) ->
    PoolName = pool_name(UserJID),
    BUserJID = bare_jid(UserJID),
    Result = message_id_to_remote_jid(PoolName, UserJID, BUserJID, MessID),
    case Result of
        {ok, BRemFullJID} ->
            RemFullJID = jid:from_binary(BRemFullJID),
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
                          }           || BWithJID <- BWithJIDs, is_binary(BWithJID)],
            delete_messages(PoolName, UserJID, Messages),
            ok;
        {error, _} ->
            ok
    end.

-spec purge_multiple_messages(_Result, Host, _UserID, UserJID, Borders,
                              Start, End, Now, WithJID) ->
                                     ok when
      Host :: server_host(), _UserID :: user_id(),
      UserJID ::jid:jid(), Borders :: mod_mam:borders(),
      Start :: unix_timestamp()  | undefined,
      End :: unix_timestamp()  | undefined,
      Now :: unix_timestamp(),
      WithJID :: jid:jid()  | undefined.
purge_multiple_messages(_Result, Host, UserID, UserJID, Borders,
                        Start, End, Now, WithJID) ->
    %% Simple query without calculating offset and total count
    Filter = prepare_filter(UserJID, Borders, Start, End, WithJID),
    PoolName = pool_name(UserJID),
    Limit = 500, %% TODO something smarter
    QueryName = {list_message_ids_query, select_filter(Filter)},
    Params = maps:put('[limit]', Limit, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, QueryName,
                                             Params),
    %% TODO can be faster
    %% TODO rate limiting
    [purge_single_message(ok, Host, Id, UserID, UserJID, Now)
     || #{id := Id} <- Rows],
    ok.


%% Offset is not supported
%% Each record is a tuple of form
%% `{<<"13663125233">>, <<"bob@localhost">>, <<"res1">>, <<binary>>}'.
%% Columns are `["id", "from_jid", "message"]'.
-spec extract_messages(PoolName, UserJID, Host, Filter, IMax, ReverseLimit) ->
                              [Row] when
      PoolName :: mongoose_cassandra:pool_name(),
      UserJID :: jid:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      IMax :: pos_integer(),
      ReverseLimit :: boolean(),
      Row :: mongoose_cassandra:row().
extract_messages(_Worker, _UserJID, _Host, _Filter, 0, _) ->
    [];
extract_messages(PoolName, UserJID, _Host, Filter, IMax, false) ->
    QueryName = {extract_messages_query, select_filter(Filter)},
    Params = maps:put('[limit]', IMax, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, QueryName, Params),
    Rows;
extract_messages(PoolName, UserJID, _Host, Filter, IMax, true) ->
    QueryName = {extract_messages_r_query, select_filter(Filter)},
    Params = maps:put('[limit]', IMax, eval_filter_params(Filter)),
    {ok, Rows} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, QueryName, Params),
    lists:reverse(Rows).


%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
-spec calc_index(PoolName, UserJID, Host, Filter, MessID) -> Count
                                                                 when
      PoolName :: mongoose_cassandra:pool_name(),
      UserJID :: jid:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_index(PoolName, UserJID, Host, Filter, MessID) ->
    calc_count(PoolName, UserJID, Host, to_id(MessID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
-spec calc_before(PoolName, UserJID, Host, Filter, MessID) -> Count
                                                                  when
      PoolName :: mongoose_cassandra:pool_name(),
      UserJID :: jid:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      MessID :: message_id(),
      Count :: non_neg_integer().
calc_before(PoolName, UserJID, Host, Filter, MessID) ->
    calc_count(PoolName, UserJID, Host, before_id(MessID, Filter)).


%% @doc Get the total result set size.
%% "SELECT COUNT(*) as "count" FROM mam_message WHERE "
-spec calc_count(PoolName, UserJID, Host, Filter) -> Count
                                                         when
      PoolName :: mongoose_cassandra:pool_name(),
      UserJID :: jid:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      Count :: non_neg_integer().
calc_count(PoolName, UserJID, _Host, Filter) ->
    QueryName = {calc_count_query, select_filter(Filter)},
    Params = eval_filter_params(Filter),
    {ok, [#{count := Count}]} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, QueryName,
                                              Params),
    Count.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
%% Uses previously calculated offsets to speed up queries
-spec offset_to_start_id(PoolName, UserJID, Filter, Offset) -> Id
                                                                   when
      PoolName :: mongoose_cassandra:pool_name(),
      UserJID :: jid:jid(),
      Offset :: non_neg_integer(),
      Filter :: filter(),
      Id :: non_neg_integer() | undefined.
offset_to_start_id(PoolName, UserJID, Filter, Offset) when is_integer(Offset), Offset >= 0,
                                                           Offset =< 100 ->
    calc_offset_to_start_id(PoolName, UserJID, Filter, Offset);
offset_to_start_id(PoolName, UserJID, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    Params = maps:put(offset, Offset, eval_filter_params(Filter)),
    %% Try to find already calculated nearby offset to reduce query size
    case mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, prev_offset_query, Params) of
        {ok, []} -> %% No hints, just calculate offset sloooowly
            StartId = calc_offset_to_start_id(PoolName, UserJID, Filter, Offset),
            maybe_save_offset_hint(PoolName, UserJID, Filter, 0, Offset, StartId);
        {ok, [#{offset := PrevOffset, id := PrevId}]} ->
            %% Offset hint found, use it to reduce query size
            case Offset of
                PrevOffset -> PrevId;
                _ ->
                    StartId = calc_offset_to_start_id(PoolName, UserJID,
                                                      Filter#mam_ca_filter{start_id = PrevId},
                                                      Offset - PrevOffset + 1),
                    maybe_save_offset_hint(PoolName, UserJID, Filter, PrevOffset, Offset, StartId)
            end
    end.

%% @doc Saves offset hint for future use in order to speed up queries with similar offset
%% Hint is save only if previous offset hint was 50+ entires from current query
%% This function returns given StartId as passthrough for convenience
-spec maybe_save_offset_hint(PoolName :: mongoose_cassandra:pool_name(), UserJID :: jid:jid(),
                             Filter :: filter(), HintOffset :: non_neg_integer(),
                             NewOffset :: non_neg_integer(),
                             StartId :: non_neg_integer() | undefined) ->
    StartId :: non_neg_integer() | undefined.
maybe_save_offset_hint(_PoolName, _UserJID, _Filter, _HintOffset, _NewOffset,
                       StartId = undefined) ->
    StartId;
maybe_save_offset_hint(PoolName, UserJID, Filter, HintOffset, NewOffset, StartId) ->
    case abs(NewOffset - HintOffset) > 50 of
        true ->
            #mam_ca_filter{user_jid = FUserJID, with_jid = FWithJID} = Filter,
            Row = #{user_jid => FUserJID, with_jid => FWithJID, offset => NewOffset, id => StartId},
            mongoose_cassandra:cql_write(PoolName, UserJID, ?MODULE,
                                         insert_offset_hint_query, [Row]);
        false ->
            skip
    end,
    StartId.

%% @doc Convert offset to index of the first entry
%% Returns undefined if not there are not enough rows
-spec calc_offset_to_start_id(PoolName, UserJID, Filter, Offset) -> Id
                                                                        when
      PoolName :: mongoose_cassandra:pool_name(),
      UserJID :: jid:jid(),
      Offset :: non_neg_integer(),
      Filter :: filter(),
      Id :: non_neg_integer() | undefined.
calc_offset_to_start_id(PoolName, UserJID, Filter, Offset) when is_integer(Offset), Offset >= 0 ->
    QueryName = {list_message_ids_query, select_filter(Filter)},
    Params = maps:put('[limit]', Offset + 1, eval_filter_params(Filter)),
    {ok, RowsIds} = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, QueryName, Params),
    case RowsIds of
        [] -> undefined;
        [_ | _] ->
            maps:get(id, lists:last(RowsIds))
    end.

%% @doc Get closest offset -> message id 'hint' for specified offset
prev_offset_query_cql() ->
    "SELECT id, offset FROM mam_message_offset WHERE user_jid = ? and with_jid = ? "
        "and offset <= ? LIMIT 1".

%% @doc Insert offset -> message id 'hint'
insert_offset_hint_query_cql() ->
    "INSERT INTO mam_message_offset(user_jid, with_jid, id, offset) VALUES(?, ?, ?, ?)".

prepare_filter(UserJID, Borders, Start, End, WithJID) ->
    BUserJID = bare_jid(UserJID),
    {StartID, EndID} = mod_mam_utils:calculate_msg_id_borders(Borders, Start, End),
    BWithJID = maybe_full_jid(WithJID), %% it's NOT optional field
    prepare_filter_params(BUserJID, BWithJID, StartID, EndID).

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
    Optional = maps:filter(fun(_K, V) -> V =/= undefined end,
                           #{start_id => StartID, end_id =>EndID}),
    maps:merge(#{user_jid => BUserJID, with_jid => BWithJID}, Optional).

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
        _ -> " AND id >= :start_id"
    end ++
        case EndID of
            undefined -> "";
            _ -> " AND id <= :end_id"
        end.

filter_to_cql() ->
    [{select_filter(StartID, EndID), prepare_filter_cql(StartID, EndID)}
     || StartID <- [undefined, 0], EndID <- [undefined, 0]].

-spec calc_offset(PoolName, UserJID, Host, Filter, PageSize, TotalCount, RSM) -> Offset
                                                                                     when
      PoolName :: mongoose_cassandra:pool_name(),
      UserJID :: jid:jid(),
      Host :: server_hostname(),
      Filter :: filter(),
      PageSize :: non_neg_integer(),
      TotalCount :: non_neg_integer(),
      RSM :: jlib:rsm_in() | undefined,
      Offset :: non_neg_integer().
%% Requesting the Last Page in a Result Set
calc_offset(_W, _UserJID, _LS, _F, PS, TC, #rsm_in{direction = before, id = undefined}) ->
    max(0, TC - PS);
calc_offset(PoolName, UserJID, Host, F, PS, _TC, #rsm_in{direction = before, id = ID})
  when is_integer(ID) ->
    max(0, calc_before(PoolName, UserJID, Host, F, ID) - PS);
calc_offset(PoolName, UserJID, Host, F, _PS, _TC, #rsm_in{direction = aft, id = ID})
  when is_integer(ID) ->
    calc_index(PoolName, UserJID, Host, F, ID);
calc_offset(_W, _UserJID, _LS, _F, _PS, _TC, _RSM) ->
    0.

-spec maybe_full_jid(undefined | jid:jid()) -> undefined | binary().
maybe_full_jid(undefined) -> <<>>;
maybe_full_jid(JID) ->
    jid:to_binary(jid:to_lower(JID)).

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
        Filter ++ " ORDER BY id LIMIT ?".

extract_messages_r_cql(Filter) ->
    "SELECT id, from_jid, message FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++
        Filter ++ " ORDER BY id DESC LIMIT ?".

calc_count_cql(Filter) ->
    "SELECT COUNT(*) FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++ Filter.

list_message_ids_cql(Filter) ->
    "SELECT id FROM mam_message "
        "WHERE user_jid = ? AND with_jid = ? " ++ Filter ++
        " ORDER BY id LIMIT ?".


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

-spec pool_name(jid:jid()) -> term().
pool_name(_UserJid) ->
    mod_mam_cassandra_arch_params:pool_name().
