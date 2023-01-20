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

-module(mongoose_rdbms_pgsql).
-author('konrad.zemek@erlang-solutions.com').
-behaviour(mongoose_rdbms_backend).

-include_lib("epgsql/include/epgsql.hrl").

-type options() :: #{host := string(),
                     port := inet:port_number(),
                     database := string(),
                     username := string(),
                     password := string(),
                     atom() => any()}.

-export([escape_binary/1, unescape_binary/1, connect/2, disconnect/1,
         query/3, prepare/5, execute/4]).

%% API

-spec escape_binary(binary()) -> iodata().
escape_binary(Bin) when is_binary(Bin) ->
    [<<"decode('">>, base64:encode(Bin), <<"','base64')">>].

-spec unescape_binary(binary()) -> binary().
unescape_binary(<<"\\x", Bin/binary>>) ->
    base16:decode(Bin);
unescape_binary(Bin) when is_binary(Bin) ->
    Bin.

-spec connect(options(), QueryTimeout :: non_neg_integer()) ->
                     {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Options, QueryTimeout) ->
    case epgsql:connect(db_opts(Options)) of
        {ok, Pid} ->
            epgsql:squery(Pid, [<<"SET statement_timeout=">>, integer_to_binary(QueryTimeout)]),
            epgsql:squery(Pid, <<"SET standard_conforming_strings=off">>),
            {ok, Pid};
        Error ->
            Error
    end.

-spec disconnect(Connection :: epgsql:connection()) -> ok.
disconnect(Connection) ->
    epgsql:close(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
query(Connection, Query, _Timeout) ->
    pgsql_to_rdbms(epgsql:squery(Connection, Query)).

-spec prepare(Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, term()} | {error, any()}.
prepare(Connection, Name, _Table, _Fields, Statement) ->
    BinName = [atom_to_binary(Name, latin1)],
    ReplacedStatement = replace_question_marks(Statement),
    case epgsql:parse(Connection, BinName, ReplacedStatement, []) of
        {ok, _} -> epgsql:describe(Connection, statement, BinName);
        Error   -> Error
    end.

-spec execute(Connection :: term(), StatementRef :: term(), Params :: [term()],
              Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
execute(Connection, StatementRef, Params, _Timeout) ->
    pgsql_to_rdbms(epgsql:prepared_query(Connection, StatementRef, Params)).

%% Helpers

-spec db_opts(options()) -> epgsql:connect_opts().
db_opts(Options) ->
    BasicOpts = maps:with([host, port, database, username, password], Options),
    TLSOpts = tls_opts(Options),
    maps:merge(BasicOpts#{codecs => [{mongoose_rdbms_pgsql_codec_boolean, []}]}, TLSOpts).

tls_opts(#{tls := TLSOpts}) ->
    #{ssl => ssl_mode(TLSOpts),
      ssl_opts => just_tls:make_ssl_opts(maps:remove(required, TLSOpts))};
tls_opts(#{}) ->
    #{}.

ssl_mode(#{required := true}) -> required;
ssl_mode(#{required := false}) -> true.

-spec pgsql_to_rdbms(epgsql:reply(term())) -> mongoose_rdbms:query_result().
pgsql_to_rdbms(Items) when is_list(Items) ->
    lists:reverse([pgsql_to_rdbms(Item) || Item <- Items]);
pgsql_to_rdbms({error, #error{codename = unique_violation}}) ->
    {error, duplicate_key};
pgsql_to_rdbms({error, #error{message = Message}}) ->
    {error, unicode:characters_to_list(Message)};
pgsql_to_rdbms({ok, Count}) ->
    {updated, Count};
pgsql_to_rdbms({ok, Count, _Column, Value}) ->
    {updated, Count, Value};
pgsql_to_rdbms({ok, _Columns, Rows}) ->
    {selected, Rows}.

-spec replace_question_marks(Statement :: iodata()) -> iodata().
replace_question_marks(Statement) when is_list(Statement) ->
    replace_question_marks(iolist_to_binary(Statement));
replace_question_marks(Statement) when is_binary(Statement) ->
    [Head | Parts] = binary:split(Statement, <<"?">>, [global]),
    Placeholders = [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(Parts))],
PartsWithPlaceholders = lists:zipwith(fun(A, B) -> [A, B] end, Placeholders, Parts),
                      [Head | PartsWithPlaceholders].
