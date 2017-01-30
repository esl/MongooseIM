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

-module(mongoose_rdbms_odbc).
-author('konrad.zemek@erlang-solutions.com').
-behaviour(mongoose_rdbms).

-export([escape_format/1, connect/1, disconnect/1, query/3, is_error_duplicate/1]).

%% API

-spec escape_format(Host :: ejabberd:server()) -> atom().
escape_format(Host) ->
    Key = {odbc_server_type, Host},
    case ejabberd_config:get_local_option_or_default(Key, odbc) of
        pgsql ->
            hex;
        mssql ->
            mssql_hex;
        _ ->
            simple_escape
    end.

-spec connect(Args :: any()) -> {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings) when is_list(Settings) ->
    ok = application:ensure_started(odbc),
    Opts = [{scrollable_cursors, off},
            {binary_strings, on},
            {timeout, 5000}],
    odbc:connect(Settings, Opts).

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    odbc:disconnect(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> term().
query(Connection, Query, Timeout) when is_binary(Query) ->
    query(Connection, [Query], Timeout);
query(Connection, Query, Timeout) ->
    parse(odbc:sql_query(Connection, Query, Timeout)).

-spec is_error_duplicate(Reason :: string()) -> boolean().
is_error_duplicate("ERROR: duplicate" ++ _) -> true;
is_error_duplicate(_Reason) -> false.

%% Helpers

-spec parse(odbc:result_tuple() | [odbc:result_tuple()] | {error, string()}) ->
        [{updated, non_neg_integer()} | {selected, [tuple()]}] |
        {updated, non_neg_integer()} | {selected, [tuple()]} | {error, string()}.
parse(Items) when is_list(Items) ->
    [parse(Item) || Item <- Items];
parse({selected, _FieldNames, Rows}) ->
    {selected, Rows};
parse(Other) ->
    Other.
