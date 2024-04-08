%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(rdbms_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% We need assert from it
-include("mam_helper.hrl").

-import(domain_helper, [host_type/0]).

-import(distributed_helper, [mim/0, rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, global_rdbms_queries},
     {group, tagged_rdbms_queries}
    ].

groups() ->
    [
     {global_rdbms_queries, [], rdbms_queries_cases()},
     {tagged_rdbms_queries, [], rdbms_queries_cases()}
    ].

rdbms_queries_cases() ->
    [select_one_works_case,
     select_ascii_string_works_case,
     read_int32_case,
     read_int64_case,
     read_unicode_case,
     read_unicode250_case,
     read_ascii_char_case,
     read_ascii_string_case,
     read_binary_8k_case,
     read_binary_65k_case,
     read_binary_16m_case,
     read_enum_char_case,
     read_boolean_case,

     read_prep_int32_case,
     read_prep_int64_case,
     read_prep_unicode_case,
     read_prep_unicode250_case,
     read_prep_ascii_char_case,
     read_prep_ascii_string_case,
     read_prep_binary_8k_case,
     read_prep_binary_65k_case,
     read_prep_binary_16m_case,
     read_prep_enum_char_case,
     read_prep_boolean_case,
     select_current_timestamp_case,

     select_like_case,
     select_like_prep_case,

     insert_batch_with_null_case,
     test_cast_insert,
     test_request_insert,
     test_wrapped_request,
     test_failed_wrapper,
     test_request_transaction,
     test_restart_transaction_with_execute,
     test_restart_transaction_with_execute_eventually_passes,
     test_failed_transaction_with_execute_wrapped,
     test_failed_wrapper_transaction,
     test_incremental_upsert,
     arguments_from_two_tables].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case not ct_helper:is_ct_running()
         orelse mongoose_helper:is_rdbms_enabled(host_type()) of
        false -> {skip, rdbms_or_ct_not_running};
        true ->
            %% Warning: inject_module does not really work well with --rerun-big-tests flag
            mongoose_helper:inject_module(?MODULE),
            escalus:init_per_suite(Config)
    end.

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(tagged_rdbms_queries, Config) ->
    ExtraConfig = stop_global_default_pool(),
    start_local_host_type_pool(ExtraConfig),
    ExtraConfig ++ Config;
init_per_group(global_rdbms_queries, Config) ->
    [{tag, global} | Config].

end_per_group(tagged_rdbms_queries, Config) ->
    restart_global_default_pool(Config);
end_per_group(global_rdbms_queries, Config) ->
    Config.

init_per_testcase(test_incremental_upsert, Config) ->
    erase_inbox(Config),
    escalus:init_per_testcase(test_incremental_upsert, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config)
    when CaseName =:= test_restart_transaction_with_execute;
         CaseName =:= test_restart_transaction_with_execute_eventually_passes;
         CaseName =:= test_failed_transaction_with_execute_wrapped;
         CaseName =:= test_failed_wrapper;
         CaseName =:= test_failed_wrapper_transaction ->
    rpc(mim(), meck, unload, []),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(test_incremental_upsert, Config) ->
    erase_inbox(Config),
    escalus:end_per_testcase(test_incremental_upsert, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Data for cases
%%--------------------------------------------------------------------

int32_values() ->
    [1, -1, 0, 42, 2147483647, null].

int64_values() ->
    [9223372036854775807,
     null].

ascii_string_values() ->
    [<<>>, <<"1">>, <<"test">>,
     <<"\\">>, <<"\\\\">>,
     <<"'">>, <<"''">>, <<"'''">>,
     <<"\"">>, <<"\"\"">>,
     <<"\r\n">>, <<"\r">>, <<"\n">>,
     binary:copy(<<"a">>, 250), null].

unicode_values() ->
    ascii_string_values() ++
    [<<"юникод"/utf8>>, <<"😁"/utf8>>]
    ++
    %% Would fail with binary_data_8k and mssql.
    %% For some reason mssql returns string "7878...." of length 4000.
    %% What is 78? 16#78 = 120 = $x.
    %% i.e. half of 8000 bytes for data.
    %% Probably 2 bytes encoding is used for this.
%   [binary:copy(<<$x>>, 4001),
    %% Helps to debug if we don't consume all data from a buffer.
    %% Than there would be a gap of missing numbers in the middle.
    %% 1000 of 1-es, 1000 of 2-s, ..., 1000 of 10-s.
    %%
    %% In one version of eodbc, it returns 5,5,5,5... instead of 1,1,1,1...
    %%
    %% Also,
    %% eodbc:sql_query(Conn, "SELECT convert(varbinary(max), binary_data_8k) FROM test_types") = gives correct result.
    %% but
    %% eodbc:sql_query(Conn, "SELECT binary_data_8k FROM test_types") = gives not correct result.
    %%
    %% eodbc:sql_query(Conn, "SELECT convert(varbinary(1000), binary_data_8k) FROM test_types") = gives correct result.
    %% gives 010101.... as expected
    [iolist_to_binary([lists:duplicate(1000, X) || X <- lists:seq(1, 10)]),
     binary:copy(<<$a>>, 10000),
    %% There is a bug with 8001 chars limit in upstream odbc
    %% We use a fork arcusfelis/eodbc, that has the bug fixed
    %% https://bugs.erlang.org/browse/ERL-421
     binary:copy(<<10>>, 10000), null].

binary_values() ->
    [<<0>>, <<"255">>,
     <<240,159>>, %% Incomplete unicode
     <<240,159,0>>, %% Invalid unicode
    iolist_to_binary(lists:seq(0, 255)),
    %% one kilobyte
    binary:copy(<<1>>, 1024),
    %% two kilobytes
    binary:copy(<<2>>, 2048),
    binary:copy(<<5>>, 1024*5),
    %% There is a bug with 8001 chars limit in upstream odbc
    %% We use a fork arcusfelis/eodbc, that has the bug fixed
    %% https://bugs.erlang.org/browse/ERL-421
    binary:copy(<<8>>, 8002),
    binary:copy(<<0>>, 100000),
    null
    ] ++
    case is_odbc() orelse is_pgsql() of
        true ->
            [];
        false ->
            %% FIXME long data causes timeout with mssql
            %%
            %% FIXME %% epgsql_sock:handle_info/2 is not optimized
            %% The query takes 30 seconds on Postgres
            %% mongoose_rdbms:sql_query(<<"localhost">>, <<"SELECT binary_data_16m FROM test_types">>).
            [binary:copy(<<16>>, 16777215)]
    end.

binary_8k_values() ->
    truncate_binaries(8000, unicode_values() ++ binary_values()).

binary_65k_values() ->
    truncate_binaries(65535, unicode_values() ++ binary_values()).

binary_16m_values() ->
    truncate_binaries(16777215, unicode_values() ++ binary_values()).

ascii_char_values() ->
    [<<"a">>, <<"b">>].

enum_char_values() ->
    [<<"A">>, <<"B">>, <<"C">>].

simple_like_texts() ->
    [#{text => <<"hello user!">>,
       not_matching => [<<"hi">>, <<"help">>],
       matching => [<<"hello">>, <<"user">>, <<"hell">>]},
     #{text => <<60,79,67,32,59,48,63,58,48>>,
       not_matching => [<<62,66,64,48,65,66,53,66>>],
       matching => [<<60,79,67>>]}].

like_texts() ->
    simple_like_texts() ++
        [#{text => <<"abc%">>,
           not_matching => [<<"ab%">>, <<"%bc%">>],
           matching => [<<"abc%">>, <<"abc">>]},
         #{text => <<"żółć_"/utf8>>,
           not_matching => [<<"_ółć_"/utf8>>],
           matching => [<<"żół"/utf8>>, <<"ółć_"/utf8>>]}].

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

%% Checks, that we at least can connect to db
select_one_works_case(Config) ->
    ?assert_equal({selected, [{<<"1">>}]},
                  %% Postgres direct driver returns <<"1">>
                  %% Everyone else return 1
                  selected_to_binary(sql_query(Config, <<"SELECT 1">>))).

%% Should be binary.
%% Unicode is another case.
select_ascii_string_works_case(Config) ->
    ?assert_equal({selected, [{<<"ascii">>}]},
                  sql_query(Config, <<"SELECT 'ascii'">>)).

%% Writes and reads values of this datatype back
read_int32_case(Config) ->
    Values = int32_values(),
    [check_int32(Config, Value) || Value <- Values].

read_int64_case(Config) ->
    Values = int32_values() ++ int64_values(),
    [check_int64(Config, Value) || Value <- Values].

read_unicode_case(Config) ->
    [check_unicode(Config, Value) || Value <- unicode_values()].

read_unicode250_case(Config) ->
    [check_unicode250(Config, Value) || Value <- unicode_values(), byte_size(Value) < 250].

read_ascii_char_case(Config) ->
    [check_ascii_char(Config, Value) || Value <- ascii_char_values()].

read_ascii_string_case(Config) ->
    [check_ascii_string(Config, Value)
     || Value <- ascii_char_values() ++ ascii_string_values()].

read_binary_8k_case(Config) ->
    [check_binary_8k(Config, Value) || Value <- binary_8k_values()].

read_binary_65k_case(Config) ->
    [check_binary_65k(Config, Value) || Value <- binary_65k_values()].

read_binary_16m_case(Config) ->
    [check_binary_16m(Config, Value) || Value <- binary_16m_values()].

read_enum_char_case(Config) ->
    [check_enum_char(Config, Value) || Value <- enum_char_values()].

read_boolean_case(Config) ->
    [check_boolean(Config, Value) || Value <- [true, false]].

%%--------------------------------------------------------------------
%% Prepared cases
%%--------------------------------------------------------------------

read_prep_int32_case(Config) ->
    Values = int32_values(),
    [check_prep_int32(Config, Value) || Value <- Values].

read_prep_int64_case(Config) ->
    Values = int32_values() ++ int64_values(),
    [check_prep_int64(Config, Value) || Value <- Values].

read_prep_unicode_case(Config) ->
    [check_prep_unicode(Config, Value) || Value <- unicode_values()].

read_prep_unicode250_case(Config) ->
    [check_prep_unicode250(Config, Value) || Value <- unicode_values(), byte_size(Value) < 250].

read_prep_ascii_char_case(Config) ->
    [check_prep_ascii_char(Config, Value) || Value <- ascii_char_values()].

read_prep_ascii_string_case(Config) ->
    [check_prep_ascii_string(Config, Value)
     || Value <- ascii_char_values() ++ ascii_string_values()].

read_prep_binary_8k_case(Config) ->
    [check_prep_binary_8k(Config, Value) || Value <- binary_8k_values()].

read_prep_binary_65k_case(Config) ->
    [check_prep_binary_65k(Config, Value) || Value <- binary_65k_values()].

read_prep_binary_16m_case(Config) ->
    [check_prep_binary_16m(Config, Value) || Value <- binary_16m_values()].

read_prep_enum_char_case(Config) ->
    [check_prep_enum_char(Config, Value) || Value <- enum_char_values()].

read_prep_boolean_case(Config) ->
    [check_prep_boolean(Config, Value) || Value <- [0, 1]].

select_current_timestamp_case(Config) ->
    ok = rpc(mim(), mongoose_rdbms_timestamp, prepare, []),
    Res = case ?config(tag, Config) of
              global ->
                  rpc(mim(), mongoose_rdbms_timestamp, select, []);
              Tag ->
                  rpc(mim(), mongoose_rdbms_timestamp, select, [host_type(), Tag])
          end,
    assert_is_integer(Res).

assert_is_integer(X) when is_integer(X) ->
    X.

truncate_binaries(Len, List) ->
    [truncate_binary(Len, Bin) || Bin <- List].

truncate_binary(Len, Bin) when byte_size(Bin) > Len ->
    binary:part(Bin, {0,Len});
truncate_binary(_Len, Bin) ->
    Bin.

safe_binary(Len, Bin) when byte_size(Bin) > Len ->
    #{what => truncated_safe_binary,
      truncated_length => Len,
      total_length => byte_size(Bin),
      truncated_binary => binary:part(Bin, {0,Len})};
safe_binary(_Len, Bin) ->
    Bin.

arguments_from_two_tables(Config) ->
    erase_users(Config),
    sql_prepare(Config, select_multi_args, users, [password, 'last.seconds'],
                <<"SELECT users.username from users "
                  " LEFT JOIN last ON (last.username = users.username) "
                  " WHERE users.password = ? AND last.seconds > ?">>),
    UserInsert = "INSERT INTO users (username, server, password) VALUES ",
    sql_query(Config, UserInsert ++ "('alice', 'domain', 'secret')"),
    sql_query(Config, UserInsert ++ "('bob', 'domain', 'billy')"),
    LastInsert = "INSERT INTO last (username, server, seconds, state) VALUES ",
    sql_query(Config, LastInsert ++ "('alice', 'domain', 1615368268, 'ok')"),
    sql_query(Config, LastInsert ++ "('bob', 'domain', 1610000000, 'cool')"),
    SelectResult = sql_execute(Config, select_multi_args, [<<"secret">>, 1611000000]),
    ?assert_equal({selected, [{<<"alice">>}]}, SelectResult),
    erase_users(Config),
    ok.

%% Ensures that ODBC uses a correct type when encoding NULL
%% and it does not interfere with non-null values
insert_batch_with_null_case(Config) ->
    erase_table(Config),
    sql_prepare(Config, insert_batch, test_types, [unicode, unicode],
                <<"INSERT INTO test_types(unicode) VALUES (?), (?)">>),
    sql_execute(Config, insert_batch, [null, <<"check1">>]),
    sql_execute(Config, insert_batch, [<<"check2">>, null]),
    SelectResult = sql_query(Config, "SELECT unicode FROM test_types"),
    %% Sorting with null values is DB specific, so sort it with Erlang
    ?assert_equal({selected, [{null}, {null}, {<<"check1">>}, {<<"check2">>}]},
                  selected_to_sorted(SelectResult)).

test_cast_insert(Config) ->
    erase_table(Config),
    sql_prepare(Config, insert_one, test_types, [unicode],
                <<"INSERT INTO test_types(unicode) VALUES (?)">>),
    sql_execute_cast(Config, insert_one, [<<"check1">>]),
    sql_query_cast(Config, <<"INSERT INTO test_types(unicode) VALUES ('check2')">>),
    mongoose_helper:wait_until(
      fun() ->
              SelectResult = sql_query(Config, "SELECT unicode FROM test_types"),
              ?assertEqual({selected, [{<<"check1">>}, {<<"check2">>}]},
                           selected_to_sorted(SelectResult))
      end, ok, #{name => cast_queries}).

test_request_insert(Config) ->
    erase_table(Config),
    sql_prepare(Config, insert_one, test_types, [unicode],
                <<"INSERT INTO test_types(unicode) VALUES (?)">>),
    sql_execute_request(Config, insert_one, [<<"check1">>]),
    sql_query_request(Config, <<"INSERT INTO test_types(unicode) VALUES ('check2')">>),
    mongoose_helper:wait_until(
      fun() ->
              SelectResult = sql_query(Config, "SELECT unicode FROM test_types"),
              ?assertEqual({selected, [{<<"check1">>}, {<<"check2">>}]},
                           selected_to_sorted(SelectResult))
      end, ok, #{name => request_queries}).

test_wrapped_request(Config) ->
    % given
    erase_table(Config),
    sql_prepare(Config, insert_one, test_types, [unicode],
                <<"INSERT INTO test_types(unicode) VALUES (?)">>),
    rpc(mim(), mongoose_metrics, ensure_metric, [global, [test_metric], histogram]),
    WrapperFun = fun(SqlExecute) ->
        {Time, Result} = timer:tc(SqlExecute),
        mongoose_metrics:update(global, [test_metric], Time),
        Result
    end,

    % when
    sql_execute_wrapped_request_and_wait_response(Config, insert_one, [<<"check1">>], WrapperFun),

    % then
    mongoose_helper:wait_until(
        fun() ->
            SelectResult = sql_query(Config, "SELECT unicode FROM test_types"),
            ?assertEqual({selected, [{<<"check1">>}]}, selected_to_sorted(SelectResult))
        end, ok, #{name => request_queries}),

    {ok, Metric} = rpc(mim(), mongoose_metrics, get_metric_value, [global, [test_metric]]),
    MetricValue = proplists:get_value(mean, Metric),
    ?assert(MetricValue > 0).

test_failed_wrapper(Config) ->
    % given
    erase_table(Config),
    sql_prepare(Config, insert_one, test_types, [unicode],
                <<"INSERT INTO test_types(unicode) VALUES (?)">>),
    ok = rpc(mim(), meck, new, [supervisor, [passthrough, no_link, unstick]]),
    WrapperFun = fun(_SqlExecute) ->
        error(wrapper_crashed)
    end,

    % when
    Result = sql_execute_wrapped_request_and_wait_response(Config, insert_one, [<<"check1">>], WrapperFun),

    % then
    ?assertEqual({reply,{error,wrapper_crashed}}, Result),
    ?assertEqual([], rpc(mim(), meck, history, [supervisor])).

test_request_transaction(Config) ->
    erase_table(Config),
    Queries = [<<"INSERT INTO test_types(unicode) VALUES ('check1')">>,
               <<"INSERT INTO test_types(unicode) VALUES ('check2')">>],
    sql_transaction_request(Config, Queries),
    mongoose_helper:wait_until(
      fun() ->
              SelectResult = sql_query(Config, "SELECT unicode FROM test_types"),
              ?assertEqual({selected, [{<<"check1">>}, {<<"check2">>}]},
                           selected_to_sorted(SelectResult))
      end, ok, #{name => request_queries}).

test_restart_transaction_with_execute(Config) ->
    erase_table(Config),
    prepare_insert_int8(Config),
    ok = rpc(mim(), meck, new, [mongoose_rdbms_backend, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_rdbms_backend, execute, 4,
                                   {error, simulated_db_error}]),
    %% Check that mocking works
    {error, simulated_db_error} = sql_execute(Config, insert_int8, [1]),
    %% Executed by the MIM node
    HostType = host_type(),
    Pid = self(),
    F = fun() -> Pid ! called, mongoose_rdbms:execute(HostType, insert_int8, [2]) end,
    {aborted, #{reason := simulated_db_error}} = sql_transaction(Config, F),
    called_times(11), %% 1 first run + 10 restarts
    ok.

test_restart_transaction_with_execute_eventually_passes(Config) ->
    erase_table(Config),
    prepare_insert_int8(Config),
    ok = rpc(mim(), meck, new, [mongoose_rdbms_backend, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_rdbms_backend, execute, 4,
                                   {error, simulated_db_error}]),
    %% Check that mocking works
    {error, simulated_db_error} = sql_execute(Config, insert_int8, [1]),
    %% Executed by the MIM node
    HostType = host_type(),
    Pid = self(),
    F = fun() -> Pid ! called, fail_times(3, Pid, HostType) end,
    {atomic, ok} = sql_transaction(Config, F),
    called_times(3),
    ok.

test_failed_transaction_with_execute_wrapped(Config) ->
    % given
    HostType = host_type(),
    Pid = self(),
    erase_table(Config),
    prepare_insert_int8(Config),
    ok = rpc(mim(), meck, new, [mongoose_rdbms_backend, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_rdbms_backend, execute, 4,
                                   {error, simulated_db_error}]),
    WrapperFun = fun(SqlExecute) ->
        Pid ! msg_before,
        Result = SqlExecute(),
        Pid ! msg_after,
        Result
    end,

    % when
    F = fun() -> mongoose_rdbms:execute_wrapped_request(HostType, insert_int8, [2], WrapperFun) end,
    {aborted, #{reason := simulated_db_error}} = sql_transaction(Config, F),

    % then
    check_not_received(msg_after).

test_failed_wrapper_transaction(Config) ->
    % given
    erase_table(Config),
    prepare_insert_int8(Config),
    ok = rpc(mim(), meck, new, [supervisor, [passthrough, no_link, unstick]]),
    WrapperFun = fun(_SqlExecute) ->
        error(wrapper_crashed)
    end,

    % when
    ScopeAndTag = scope_and_tag(Config),
    F = fun() -> sql_execute_wrapped_request(ScopeAndTag, insert_one, [<<"check1">>], WrapperFun) end,
    sql_transaction(Config, F),

    % then
    ?assertEqual([], rpc(mim(), meck, history, [supervisor])).

prepare_insert_int8(Config) ->
    Q = <<"INSERT INTO test_types(", (escape_column(<<"int8">>))/binary, ") VALUES (?)">>,
    sql_prepare(Config, insert_int8, test_types, [int8], Q).

fail_times(N, Pid, HostType) ->
    case update_counter(Pid) + 1 of
        N ->
            ok;
        _ ->
            mongoose_rdbms:execute(HostType, insert_int8, [2])
    end.

%% Returns old value
update_counter(Pid) ->
    Key = {test_counter, Pid},
    N = case get(Key) of undefined -> 0; X -> X end,
    put(Key, N + 1),
    N.

called_times(0) ->
    %% Check that there are no more calls
    receive called -> error(unexpected) after 0 -> ok end;
called_times(N) ->
    receive called -> ok after 5000 -> error({called_times_timeout, N}) end,
    called_times(N - 1).

test_incremental_upsert(Config) ->
    case is_odbc() of
        true ->
            ok;
        false ->
            do_test_incremental_upsert(Config)
    end.

do_test_incremental_upsert(Config) ->
    KeyFields = [<<"luser">>, <<"lserver">>, <<"remote_bare_jid">>],
    InsertFields = KeyFields ++ [<<"msg_id">>, <<"content">>, <<"unread_count">>, <<"timestamp">>],

    Key = [<<"alice">>, <<"localhost">>, <<"bob@localhost">>],
    Insert = [<<"alice">>, <<"localhost">>, <<"bob@localhost">>, <<"msg_id">>, <<"content">>, 1],
    sql_prepare_upsert(Config, upsert_incr, inbox,
                       InsertFields, [<<"timestamp">>], KeyFields, <<"timestamp">>),
    sql_execute_upsert(Config, upsert_incr, Insert ++ [42], [42], Key),
    sql_execute_upsert(Config, upsert_incr, Insert ++ [43], [43], Key),
    sql_execute_upsert(Config, upsert_incr, Insert ++ [0], [0], Key),
    SelectResult = sql_query(Config, <<"SELECT timestamp FROM inbox">>),
    ?assertEqual({selected, [{<<"43">>}]}, selected_to_binary(SelectResult)).

%%--------------------------------------------------------------------
%% Text searching
%%--------------------------------------------------------------------

select_like_case(Config) ->
    %% Non-prepared queries don't support proper LIKE escaping
    [check_like(Config, TextMap) || TextMap <- simple_like_texts()].

select_like_prep_case(Config) ->
    [check_like_prep(Config, TextMap) || TextMap <- like_texts()].

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
tag() ->
    extra_tag.

scope_and_tag(Config) ->
    case ?config(tag, Config) of
        global -> [host_type()];
        Tag -> [host_type(), Tag]
    end.

sql_query(Config, Query) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, sql_query, ScopeAndTag ++ [Query]).

sql_prepare(_Config, Name, Table, Fields, Query) ->
    escalus_ejabberd:rpc(mongoose_rdbms, prepare, [Name, Table, Fields, Query]).

sql_prepare_upsert(_Config, Name, Table, Insert, Update, Unique, Incr) ->
    escalus_ejabberd:rpc(rdbms_queries, prepare_upsert, [host_type(), Name, Table, Insert, Update, Unique, Incr]).

sql_execute(Config, Name, Parameters) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, execute, ScopeAndTag ++ [Name, Parameters]).

sql_execute_cast(Config, Name, Parameters) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, execute_cast, ScopeAndTag ++ [Name, Parameters]).

sql_query_cast(Config, Query) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, sql_query_cast, ScopeAndTag ++ [Query]).

sql_execute_request(Config, Name, Parameters) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, execute_request, ScopeAndTag ++ [Name, Parameters]).

sql_execute_wrapped_request(ScopeAndTag, Name, Parameters, WrapperFun) ->
    slow_rpc(mongoose_rdbms, execute_wrapped_request, ScopeAndTag ++ [Name, Parameters, WrapperFun]).

sql_execute_wrapped_request_and_wait_response(Config, Name, Parameters, WrapperFun) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(?MODULE, execute_wrapped_request_and_wait_response, ScopeAndTag ++ [Name, Parameters, WrapperFun]).

execute_wrapped_request_and_wait_response(HostType, Tag, Name, Parameters, WrapperFun) ->
    RequestId = mongoose_rdbms:execute_wrapped_request(HostType, Tag, Name, Parameters, WrapperFun),
    gen_server:wait_response(RequestId, 100).

execute_wrapped_request_and_wait_response(HostType, Name, Parameters, WrapperFun) ->
    RequestId = mongoose_rdbms:execute_wrapped_request(HostType, Name, Parameters, WrapperFun),
    gen_server:wait_response(RequestId, 100).

sql_execute_upsert(Config, Name, Insert, Update, Unique) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(rdbms_queries, execute_upsert, ScopeAndTag ++ [Name, Insert, Update, Unique]).

sql_query_request(Config, Query) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, sql_query_request, ScopeAndTag ++ [Query]).

sql_transaction_request(Config, Query) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, sql_transaction_request, ScopeAndTag ++ [Query]).

sql_transaction(Config, F) ->
    ScopeAndTag = scope_and_tag(Config),
    slow_rpc(mongoose_rdbms, sql_transaction, ScopeAndTag ++ [F]).

escape_null(_Config) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_null, []).

escape_string(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_string, [Value]).

escape_binary(_Config, Value) ->
    slow_rpc(mongoose_rdbms, escape_binary, [host_type(), Value]).

escape_boolean(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_boolean, [Value]).

escape_like(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_like, [Value]).

escape_prepared_like(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_prepared_like, [Value]).

unescape_binary(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, unescape_binary, [host_type(), Value]).

use_escaped(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, use_escaped, [Value]).

use_escaped_like(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, use_escaped_like, [Value]).

escape_string_or_null(Config, null) ->
    escape_null(Config);
escape_string_or_null(Config, TextValue) ->
    escape_string(Config, TextValue).

escape_binary_or_null(Config, null) ->
    escape_null(Config);
escape_binary_or_null(Config, Value) ->
    escape_binary(Config, Value).

decode_boolean(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, to_bool, [Value]).

erase_table(Config) ->
    {updated, _} = sql_query(Config, <<"DELETE FROM test_types">>).

erase_users(Config) ->
    {updated, _} = sql_query(Config, <<"DELETE FROM users">>),
    {updated, _} = sql_query(Config, <<"DELETE FROM last">>).

erase_inbox(Config) ->
    {updated, _} = sql_query(Config, <<"DELETE FROM inbox">>).

check_int32(Config, Value) ->
    check_generic_integer(Config, Value, <<"int32">>).

check_int64(Config, Value) ->
    check_generic_integer(Config, Value, <<"int64">>).

check_generic_integer(Config, Value, Column) ->
    EraseResult = erase_table(Config),
    InsertQuery = <<"INSERT INTO test_types (", Column/binary, ") "
                        "VALUES (", (integer_or_null_to_binary(Value))/binary, ")">>,
    SelectQuery = <<"SELECT ", Column/binary, " FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{integer_to_binary_or_null(Value)}]},
                        selected_to_binary(SelectResult),
                        #{column => Column,
                          erase_result => EraseResult,
                          test_value => Value,
                          insert_query => InsertQuery,
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

integer_or_null_to_binary(null) -> <<"NULL">>;
integer_or_null_to_binary(X) -> integer_to_binary(X).

integer_to_binary_or_null(null) -> null;
integer_to_binary_or_null(X) -> integer_to_binary(X).

%% Helper function to transform values to an uniform format.
%% Single tuple, single element case.
%% In ODBC int32 is integer, but int64 is binary.
selected_to_binary({selected, [{Value}]}) when is_integer(Value) ->
    {selected, [{integer_to_binary(Value)}]};
selected_to_binary(Other) ->
    Other.

selected_to_sorted({selected, Rows}) ->
    {selected, lists:sort(Rows)};
selected_to_sorted(Other) ->
    Other.

value_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
value_to_binary(Value) ->
    Value.

check_unicode250(Config, Value) ->
    check_unicode_generic(Config, Value, <<"unicode250">>).

check_unicode(Config, Value) ->
    check_unicode_generic(Config, Value, <<"unicode">>).

check_unicode_generic(Config, Value, Column) ->
    SValue = escape_string_or_null(Config, Value),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (", Column, ") "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    SelectQuery = <<"SELECT ", Column/binary, " FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{Value}]},
                        SelectResult,
                        #{column => Column,
                          erase_result => EraseResult,
                          expected_length => byte_size_or_null(Value),
                          selected_length => maybe_selected_length(Config, SelectResult),
                          compare_selected => compare_selected(Config, SelectResult, Value),
                          test_value => Value,
                          insert_query => InsertQuery,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

check_ascii_char(Config, Value) ->
    SValue = escape_string_or_null(Config, Value),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (ascii_char) "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    SelectQuery = <<"SELECT ascii_char FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{Value}]},
                        SelectResult,
                        #{erase_result => EraseResult,
                          test_value => Value,
                          insert_query => InsertQuery,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

check_ascii_string(Config, Value) ->
    SValue = escape_string_or_null(Config, Value),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (ascii_string) "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    SelectQuery = <<"SELECT ascii_string FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{Value}]},
                        SelectResult,
                        #{erase_result => EraseResult,
                          test_value => Value,
                          insert_query => InsertQuery,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

check_binary_8k(Config, Value) ->
    check_binary(Config, Value, <<"binary_data_8k">>).

check_binary_65k(Config, Value) ->
    check_binary(Config, Value, <<"binary_data_65k">>).

check_binary_16m(Config, Value) ->
    check_binary(Config, Value, <<"binary_data_16m">>).

check_binary(Config, Value, Column) ->
    SValue = escape_binary_or_null(Config, Value),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (", Column, ") "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    SelectQuery = <<"SELECT ", Column/binary, " FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{Value}]},
                        selected_unescape(Config, SelectResult),
                        #{erase_result => EraseResult,
                          inserted_length => byte_size_or_null(Value),
                          %% pgsql+odbc can truncate binaries
                          maybe_selected_length => maybe_selected_length(Config, SelectResult),
                          maybe_selected_tail => maybe_selected_tail(Config, SelectResult),
                          compare_selected => compare_selected(Config, selected_unescape(Config, SelectResult), Value),
                          test_value => Value,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

byte_size_or_null(null) ->
    null;
byte_size_or_null(Value) ->
    byte_size(Value).

check_enum_char(Config, Value) when is_binary(Value) ->
    SValue = escape_string_or_null(Config, Value),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (enum_char) "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    SelectQuery = <<"SELECT enum_char FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{Value}]},
                        SelectResult,
                        #{erase_result => EraseResult,
                          test_value => Value,
                          insert_query => InsertQuery,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

check_boolean(Config, Value) when is_boolean(Value) ->
    SValue = escape_boolean(Config, Value),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (bool_flag) "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    SelectQuery = <<"SELECT bool_flag FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{Value}]},
                        selected_decode_boolean(Config, SelectResult),
                        #{erase_result => EraseResult,
                          test_value => Value,
                          insert_query => InsertQuery,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

selected_unescape(_Config, {selected, [{null}]}) ->
    {selected, [{null}]};
selected_unescape(Config, {selected, [{Value}]}) ->
    {selected, [{unescape_binary(Config, Value)}]};
selected_unescape(_Config, Other) ->
    Other.

selected_decode_boolean(Config, {selected, [{Value}]}) ->
    {selected, [{decode_boolean(Config, Value)}]};
selected_decode_boolean(_Config, Other) ->
    Other.

selected_boolean_to_binary_int(Config, {selected, [{Value}]}) ->
    {selected, [{boolean_to_binary_int(decode_boolean(Config, Value))}]};
selected_boolean_to_binary_int(_Config, Other) ->
    Other.

boolean_to_binary_int(true) -> <<"1">>;
boolean_to_binary_int(false) -> <<"0">>.

maybe_selected_length(_Config, {selected, [{Value}]}) when is_binary(Value) ->
    byte_size(Value);
maybe_selected_length(_Config, _Other) ->
    unknown.

maybe_selected_tail(Config, Selected) ->
    maybe_selected_tail(Config, Selected, 100).

maybe_selected_tail(_Config, {selected, [{Value}]}, TailLen)
  when is_binary(Value), byte_size(Value) > TailLen ->
    binary:part(Value, {byte_size(Value), -TailLen});
maybe_selected_tail(_Config, {selected, [{Value}]}, _TailLen) ->
    Value;
maybe_selected_tail(_Config, _Other, _TailLen) ->
    unknown.

check_prep_int32(Config, Value) ->
    check_generic_prep_integer(Config, Value, <<"int32">>).

check_prep_int64(Config, Value) ->
    check_generic_prep_integer(Config, Value, <<"int64">>).

check_prep_unicode(Config, Value) ->
    check_generic_prep(Config, Value, <<"unicode">>).

check_prep_unicode250(Config, Value) ->
    check_generic_prep(Config, Value, <<"unicode250">>).

%% Char is ascii string of length one
check_prep_ascii_char(Config, Value) ->
    check_generic_prep(Config, Value, <<"unicode">>).

%% Varchar
check_prep_ascii_string(Config, Value) ->
    check_generic_prep(Config, Value, <<"ascii_string">>).

check_prep_binary_65k(Config, Value) ->
    %% MSSQL returns binaries in HEX encoding
    check_generic_prep(Config, Value, <<"binary_data_65k">>, unescape_binary).

check_prep_binary_8k(Config, Value) ->
    %% MSSQL returns binaries in HEX encoding
    check_generic_prep(Config, Value, <<"binary_data_8k">>, unescape_binary).

check_prep_binary_16m(Config, Value) ->
    %% MSSQL returns binaries in HEX encoding
    check_generic_prep(Config, Value, <<"binary_data_16m">>, unescape_binary).

check_generic_prep_integer(Config, Value, Column) ->
    check_generic_prep(Config, Value, Column).

check_prep_enum_char(Config, Value) ->
    check_generic_prep(Config, Value, <<"enum_char">>).

check_prep_boolean(Config, Value) ->
    check_generic_prep(Config, Value, <<"bool_flag">>, boolean_to_binary_int).

%% Data types
%% {ok, Conn} = odbc:connect("DSN=mongoose-mssql;UID=sa;PWD=mongooseim_secret+ESL123", []).
%% odbc:describe_table(Conn, "test_types").
%% [{"unicode",{sql_wvarchar,536870911}},
%%  {"binary_data_65k",'SQL_VARBINARY'},
%%  {"ascii_char",{sql_char,1}},
%%  {"ascii_string",{sql_varchar,250}},
%%  {"int32",sql_integer},
%%  {"int64",'SQL_BIGINT'},
%%  {"int8",sql_tinyint}]

check_generic_prep(Config, Value, Column) ->
    check_generic_prep(Config, Value, Column, to_binary).

check_generic_prep(Config, Value, Column, TransformResult) ->
    EraseResult = erase_table(Config),

    InsertQuery = <<"INSERT INTO test_types (", Column/binary, ") "
                        "VALUES (?)">>,
    SelectQuery = <<"SELECT ", Column/binary, " FROM test_types">>,
    Name = list_to_atom("insert_" ++ binary_to_list(Column)),
    Table = test_types,
    Fields = [binary_to_atom(Column, utf8)],
    PrepareResult = sql_prepare(Config, Name, Table, Fields, InsertQuery),
    Parameters = [Value],
    InsertResult = sql_execute(Config, Name, Parameters),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{value_to_binary(Value)}]},
                        transform_selected(TransformResult, Config, SelectResult),
                        #{column => Column,
                          erase_result => EraseResult,
                          test_value => Value,
                          insert_query => InsertQuery,
                          prepare_result => PrepareResult,
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}),
    check_generic_filtered_prep(Config, Value, Column, TransformResult),
    case is_odbc() of
        true ->
            %% TOP is mssql feature, all other databases use LIMIT.
            check_generic_filtered_top_prep(Config, Value, Column, TransformResult);
        false ->
            ok
    end.

%% We want to ensure that variable substitution works in SELECTS too.
%% We also want to check the result value is encoded correctly.
check_generic_filtered_prep(_Config, null, _Column, _TransformResult) ->
    skip_null_test;
check_generic_filtered_prep(Config, Value, Column, TransformResult) ->
    SelectQuery = <<"SELECT ", Column/binary,
            " FROM test_types WHERE ", Column/binary, " = ?">>,
    Name = list_to_atom("select_filtered_" ++ binary_to_list(Column)),
    Table = test_types,
    Fields = [binary_to_atom(Column, utf8)],
    PrepareResult = sql_prepare(Config, Name, Table, Fields, SelectQuery),
    Parameters = [Value],
    SelectResult = sql_execute(Config, Name, Parameters),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{value_to_binary(Value)}]},
                        transform_selected(TransformResult, Config, SelectResult),
                        #{column => Column,
                          test_value => Value,
                          prepare_result => PrepareResult,
                          select_query => SelectQuery,
                          select_result => SelectResult}).

check_generic_filtered_top_prep(_Config, null, _Column, _TransformResult) ->
    skip_null_test;
check_generic_filtered_top_prep(Config, Value, Column, TransformResult) ->
    %% SQL Server requires you to place parenthesis around the argument to top if you pass in a variable:
    %% https://stackoverflow.com/questions/7038818/ms-sql-exception-incorrect-syntax-near-p0
    SelectQuery = <<"SELECT TOP (?) ", Column/binary,
            " FROM test_types WHERE ", Column/binary, " = ?">>,
    Name = list_to_atom("select_filtered_top_" ++ binary_to_list(Column)),
    Table = test_types,
    Fields = [limit, binary_to_atom(Column, utf8)],
    PrepareResult = sql_prepare(Config, Name, Table, Fields, SelectQuery),
    Parameters = [30, Value],
    SelectResult = sql_execute(Config, Name, Parameters),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{value_to_binary(Value)}]},
                        transform_selected(TransformResult, Config, SelectResult),
                        #{column => Column,
                          test_value => Value,
                          prepare_result => PrepareResult,
                          select_query => SelectQuery,
                          select_result => SelectResult}).



transform_selected(to_binary, _Config, SelectResult) ->
    selected_to_binary(SelectResult);
transform_selected(unescape_binary, Config, SelectResult) ->
    selected_unescape(Config, SelectResult);
transform_selected(boolean_to_binary_int, Config, SelectResult) ->
    selected_boolean_to_binary_int(Config, SelectResult).

%% To KISS, we just test on a table with one row.
check_like(Config, TextMap = #{text := TextValue,
                               matching := MatchingList,
                               not_matching := NotMatchingList}) ->
    SValue = escape_string_or_null(Config, TextValue),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (unicode) "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    InsertResult = sql_query(Config, InsertQuery),
    Info = #{erase_result => EraseResult,
             insert_query => InsertQuery,
             insert_query_binary => iolist_to_binary(InsertQuery),
             insert_result => InsertResult,
             text_map => TextMap},
    [check_like_matching(Config, TextValue, Matching, Info)
     || Matching <- MatchingList],
    [check_like_not_matching(Config, TextValue, NotMatching, Info)
     || NotMatching <- NotMatchingList].

check_like_matching(Config, TextValue, Matching, Info) ->
    SLike = escape_like(Config, Matching),
    SelectQuery = ["SELECT unicode FROM test_types "
                    "WHERE unicode LIKE ", use_escaped_like(Config, SLike)],
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{TextValue}]},
                        SelectResult,
                        Info#{pattern => Matching,
                              select_query => SelectQuery,
                              select_result => SelectResult}).

check_like_not_matching(Config, _TextValue, NotMatching, Info) ->
    SLike = escape_like(Config, NotMatching),
    SelectQuery = ["SELECT unicode FROM test_types "
                    "WHERE unicode LIKE ", use_escaped_like(Config, SLike)],
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, []},
                        SelectResult,
                        Info#{pattern => NotMatching,
                              select_query => SelectQuery,
                              select_result => SelectResult}).

compare_selected(_Config, {selected, [{SelValue}]}, Value) ->
    drop_common_prefix(0, SelValue, Value);
compare_selected(_Config, _, _Value) ->
    nomatch.

drop_common_prefix(Pos, <<X, SelValue/binary>>, <<X, Value/binary>>) ->
    drop_common_prefix(Pos+1, SelValue, Value);
drop_common_prefix(Pos, SelValue, Value) ->
    #{pos => Pos,
      selected_suffix => safe_binary(100, SelValue),
      expected_suffix => safe_binary(100, Value)}.

db_engine() ->
    escalus_ejabberd:rpc(mongoose_rdbms, db_engine, [host_type()]).

is_odbc() ->
    db_engine() == odbc.

is_pgsql() ->
    db_engine() == pgsql.

is_mysql() ->
    db_engine() == mysql.

stop_global_default_pool() ->
    Pools = rpc(mim(), mongoose_config, get_opt, [outgoing_pools]),
    [GlobalRdbmsPool] = [Pool || Pool = #{type := rdbms, scope := global, tag := default} <- Pools],
    ok = rpc(mim(), mongoose_wpool, stop, [rdbms, global, default]),
    Extra = maybe_stop_service_domain_db(),
    [{tag, tag()}, {global_default_rdbms_pool, GlobalRdbmsPool} | Extra].

restart_global_default_pool(Config) ->
    GlobalRdbmsPool = ?config(global_default_rdbms_pool, Config),
    rpc(mim(), mongoose_wpool, start_configured_pools, [[GlobalRdbmsPool]]),
    maybe_restart_service_domain_db(Config).

maybe_stop_service_domain_db() ->
    case rpc(mim(), erlang, whereis, [service_domain_db]) of
        undefined ->
            [];
        ServiceDomainDB when is_pid(ServiceDomainDB) ->
            ok = rpc(mim(), sys, suspend, [ServiceDomainDB]),
            [{service_domain_db, ServiceDomainDB}]
    end.

maybe_restart_service_domain_db(Config) ->
    case ?config(service_domain_db, Config) of
        undefined ->
            ok;
        ServiceDomainDB ->
            ok = rpc(mim(), sys, resume, [ServiceDomainDB])
    end.

start_local_host_type_pool(Config) ->
    GlobalRdbmsPool = ?config(global_default_rdbms_pool, Config),
    LocalHostTypePool = GlobalRdbmsPool#{scope := host_type(), tag := tag()},
    rpc(mim(), mongoose_wpool, start_configured_pools, [[LocalHostTypePool], [host_type()]]).

escape_column(Name) ->
    case is_mysql() of
        true ->
            <<"`", Name/binary, "`">>;
        false ->
            Name
    end.

slow_rpc(M, F, A) ->
    Node = ct:get_config({hosts, mim, node}),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    Res = escalus_rpc:call(Node, M, F, A, timer:seconds(30), Cookie),
    case Res of
        {badrpc, timeout} ->
            {badrpc, {timeout, M, F}};
        _ ->
            Res
    end.

check_not_received(Msg) ->
    receive
        Msg ->
            error({msg_received, Msg});
        _ ->
            check_not_received(Msg)
    after 0 ->
        ok
    end.

check_like_prep(Config, TextMap = #{text := TextValue,
                               matching := MatchingList,
                               not_matching := NotMatchingList}) ->
    EraseResult = erase_table(Config),
    Name = insert_unicode_prep,
    SelName = select_unicode_prep,
    Table = test_types,
    Fields = [<<"unicode">>],
    InsertQuery = <<"INSERT INTO test_types (unicode) VALUES (?)">>,
    SelectQuery = <<"SELECT unicode FROM test_types WHERE unicode LIKE ? ESCAPE '$'">>,
    PrepareResult = sql_prepare(Config, Name, Table, Fields, InsertQuery),
    PrepareSelResult = sql_prepare(Config, SelName, Table, Fields, SelectQuery),
    Parameters = [TextValue],
    InsertResult = sql_execute(Config, Name, Parameters),
    Info = #{erase_result => EraseResult,
             insert_query => InsertQuery,
             prepare_result => PrepareResult,
             insert_result => InsertResult,
             prepare_select_result => PrepareSelResult,
             text_map => TextMap},
    [check_like_matching_prep(SelName, Config, TextValue, Matching, Info)
     || Matching <- MatchingList],
    [check_like_not_matching_prep(SelName, Config, TextValue, NotMatching, Info)
     || NotMatching <- NotMatchingList].

check_like_matching_prep(SelName, Config, TextValue, Matching, Info) ->
    SMatching = escape_prepared_like(Config, Matching),
    Parameters = [<<"%", SMatching/binary, "%">>],
    SelectResult = sql_execute(Config, SelName, Parameters),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{TextValue}]},
                        SelectResult,
                        Info#{pattern => Matching,
                              select_result => SelectResult}).

check_like_not_matching_prep(SelName, Config, _TextValue, NotMatching, Info) ->
    SNotMatching = escape_prepared_like(Config, NotMatching),
    Parameters = [<<"%", SNotMatching/binary, "%">>],
    SelectResult = sql_execute(Config, SelName, Parameters),
    %% Compare as binaries
    ?assert_equal_extra({selected, []},
                        SelectResult,
                        Info#{pattern => NotMatching,
                              select_result => SelectResult}).
