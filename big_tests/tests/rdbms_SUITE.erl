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
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

%% We need assert from it
-include("mam_helper.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, rdbms_queries}].

groups() ->
    G = [{rdbms_queries, [sequence], rdbms_queries_cases()}],
    ct_helper:repeat_all_until_all_ok(G).

rdbms_queries_cases() ->
    [select_one_works_case,
     select_ascii_string_works_case,
     read_int32_case,
     read_int64_case,
     read_unicode_case,
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
     read_prep_ascii_char_case,
     read_prep_ascii_string_case,
     read_prep_binary_8k_case,
     read_prep_binary_65k_case,
     read_prep_binary_16m_case,
     read_prep_enum_char_case,
     read_prep_boolean_case,

     select_like_case].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case not ct_helper:is_ct_running() orelse mongoose_helper:is_rdbms_enabled(host()) of
        false -> {skip, rdbms_or_ct_not_running};
        true -> escalus:init_per_suite(Config)
    end.

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Data for cases
%%--------------------------------------------------------------------

int32_values() ->
    [1, -1, 0, 42, 2147483647].

int64_values() ->
    [9223372036854775807].

ascii_string_values() ->
    [<<>>, <<"1">>, <<"test">>,
     <<"\\">>, <<"\\\\">>,
     <<"'">>, <<"''">>, <<"'''">>,
     <<"\"">>, <<"\"\"">>,
     <<"\r\n">>, <<"\r">>, <<"\n">>,
     binary:copy(<<"a">>, 250)].

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
     binary:copy(<<10>>, 10000)].

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
    binary:copy(<<0>>, 100000)
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

like_texts() ->
    [#{text => <<"hello user!">>,
       not_matching => [<<"hi">>, <<"help">>],
       matching => [<<"hello">>, <<"user">>, <<"hell">>]}
    ].

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

truncate_binaries(Len, List) ->
    [truncate_binary(Len, Bin) || Bin <- List].

truncate_binary(Len, Bin) when byte_size(Bin) > Len ->
    binary:part(Bin, {0,Len});
truncate_binary(Len, Bin) when is_binary(Bin), is_integer(Len) ->
    Bin.

safe_binary(Len, Bin) when byte_size(Bin) > Len ->
    #{what => truncated_safe_binary,
      truncated_length => Len,
      total_length => byte_size(Bin),
      truncated_binary => binary:part(Bin, {0,Len})};
safe_binary(Len, Bin) when is_binary(Bin), is_integer(Len) ->
    Bin.

%%--------------------------------------------------------------------
%% Text searching
%%--------------------------------------------------------------------

select_like_case(Config) ->
    [check_like(Config, TextMap) || TextMap <- like_texts()].

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

host() ->
    ct:get_config({hosts, mim, domain}).

sql_query(_Config, Query) ->
    slow_rpc(mongoose_rdbms, sql_query, [host(), Query]).

sql_prepare(_Config, Name, Table, Fields, Query) ->
    escalus_ejabberd:rpc(mongoose_rdbms, prepare, [Name, Table, Fields, Query]).

sql_execute(_Config, Name, Parameters) ->
    slow_rpc(mongoose_rdbms, execute, [host(), Name, Parameters]).

escape_string(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_string, [Value]).

escape_binary(_Config, Value) ->
    slow_rpc(mongoose_rdbms, escape_binary, [host(), Value]).

escape_boolean(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_boolean, [Value]).

escape_like(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, escape_like, [Value]).

unescape_binary(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, unescape_binary, [host(), Value]).

use_escaped(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, use_escaped, [Value]).

use_escaped_like(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, use_escaped_like, [Value]).

decode_boolean(_Config, Value) ->
    escalus_ejabberd:rpc(mongoose_rdbms, to_bool, [Value]).

erase_table(Config) ->
    sql_query(Config, <<"TRUNCATE TABLE test_types">>).

check_int32(Config, Value) ->
    check_generic_integer(Config, Value, <<"int32">>).

check_int64(Config, Value) ->
    check_generic_integer(Config, Value, <<"int64">>).

check_generic_integer(Config, Value, Column) ->
    EraseResult = erase_table(Config),
    InsertQuery = <<"INSERT INTO test_types (", Column/binary, ") "
                        "VALUES (", (integer_to_binary(Value))/binary, ")">>,
    SelectQuery = <<"SELECT ", Column/binary, " FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{integer_to_binary(Value)}]},
                        selected_to_binary(SelectResult),
                        #{column => Column,
                          erase_result => EraseResult,
                          test_value => Value,
                          insert_query => InsertQuery,
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

%% Helper function to transform values to an uniform format.
%% Single tuple, single element case.
%% In ODBC int32 is integer, but int64 is binary.
selected_to_binary({selected, [{Value}]}) when is_integer(Value) ->
    {selected, [{integer_to_binary(Value)}]};
selected_to_binary(Other) ->
    Other.

value_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
value_to_binary(Value) ->
    Value.

check_unicode(Config, Value) when is_binary(Value) ->
    SValue = escape_string(Config, Value),
    EraseResult = erase_table(Config),
    InsertQuery = ["INSERT INTO test_types (unicode) "
                        "VALUES (", use_escaped(Config, SValue), ")"],
    SelectQuery = <<"SELECT unicode FROM test_types">>,
    InsertResult = sql_query(Config, InsertQuery),
    SelectResult = sql_query(Config, SelectQuery),
    %% Compare as binaries
    ?assert_equal_extra({selected, [{Value}]},
                        SelectResult,
                        #{erase_result => EraseResult,
                          expected_length => byte_size(Value),
                          selected_length => maybe_selected_length(Config, SelectResult),
                          compare_selected => compare_selected(Config, SelectResult, Value),
                          test_value => Value,
                          insert_query => InsertQuery,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

check_ascii_char(Config, Value) when is_binary(Value) ->
    SValue = escape_string(Config, Value),
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

check_ascii_string(Config, Value) when is_binary(Value) ->
    SValue = escape_string(Config, Value),
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

check_binary(Config, Value, Column) when is_binary(Value) ->
    SValue = escape_binary(Config, Value),
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
                          inserted_length => byte_size(Value),
                          %% pgsql+odbc can truncate binaries
                          maybe_selected_length => maybe_selected_length(Config, SelectResult),
                          maybe_selected_tail => maybe_selected_tail(Config, SelectResult),
                          compare_selected => compare_selected(Config, selected_unescape(Config, SelectResult), Value),
                          test_value => Value,
                          insert_query_binary => iolist_to_binary(InsertQuery),
                          select_query => SelectQuery,
                          select_result => SelectResult,
                          insert_result => InsertResult}).

check_enum_char(Config, Value) when is_binary(Value) ->
    SValue = escape_string(Config, Value),
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

maybe_selected_length(Config, {selected, [{Value}]}) when is_binary(Value) ->
    byte_size(Value);
maybe_selected_length(_Config, Other) ->
    unknown.

maybe_selected_tail(Config, Selected) ->
    maybe_selected_tail(Config, Selected, 100).

maybe_selected_tail(Config, {selected, [{Value}]}, TailLen)
  when is_binary(Value), byte_size(Value) > TailLen ->
    binary:part(Value, {byte_size(Value), -TailLen});
maybe_selected_tail(Config, {selected, [{Value}]}, _TailLen) ->
    Value;
maybe_selected_tail(_Config, _Other, _TailLen) ->
    unknown.

check_prep_int32(Config, Value) ->
    check_generic_prep_integer(Config, Value, <<"int32">>).

check_prep_int64(Config, Value) ->
    check_generic_prep_integer(Config, Value, <<"int64">>).

check_prep_unicode(Config, Value) ->
    check_generic_prep(Config, Value, <<"unicode">>).

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
    check_generic_filtered_prep(Config, Value, Column, TransformResult).

%% We want to ensure that variable substitution works in SELECTS too.
%% We also want to check the result value is encoded correctly.
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
    SValue = escape_string(Config, TextValue),
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

check_like_not_matching(Config, TextValue, NotMatching, Info) ->
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

compare_selected(Config, {selected, [{SelValue}]}, Value) ->
    drop_common_prefix(0, SelValue, Value);
compare_selected(_Config, _, _Value) ->
    nomatch.

drop_common_prefix(Pos, <<X, SelValue/binary>>, <<X, Value/binary>>) ->
    drop_common_prefix(Pos+1, SelValue, Value);
drop_common_prefix(Pos, SelValue, Value) ->
    #{pos => Pos,
      selected_suffix => safe_binary(100, SelValue),
      expected_suffix => safe_binary(100, Value)}.

is_odbc() ->
    escalus_ejabberd:rpc(mongoose_rdbms, db_engine, [host()]) == odbc.

is_pgsql() ->
    escalus_ejabberd:rpc(mongoose_rdbms, db_engine, [host()]) == pgsql.

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
