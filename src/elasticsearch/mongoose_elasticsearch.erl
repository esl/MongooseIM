%%------------------------------------------------------------------
%% Copyright 2018 Erlang Solutions Ltd.
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
%%
%% @doc Provides functions to interact with ElasticSearch cluster
%% via HTTP API.
%%------------------------------------------------------------------
-module(mongoose_elasticsearch).

-export([health/0]).
-export([insert_document/4]).
-export([search/3]).
-export([count/3]).
-export([delete_by_query/3]).

-type index() :: binary().
-type type() :: binary().
-type document() :: map().
-type id() :: binary().
-type query() :: map().

-export_type([index/0]).
-export_type([type/0]).
-export_type([document/0]).
-export_type([id/0]).
-export_type([query/0]).

-include("mongoose.hrl").

-define(POOL_NAME, mongoose_wpool:make_pool_name(elastic, global, default)).

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

%% @doc Returns the health status of the ElasticSearch cluster.
%%
%% See https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-health.html for
%% more information.
-spec health() -> {ok, Resp :: map()} | {error, term()}.
health() ->
    case catch tirerl:health(?POOL_NAME) of
        {'EXIT', _} = Err ->
            {error, Err};
        Other ->
            Other
    end.

%% @doc Tries to insert a document into given ElasticSearch index.
%%
%% See https://www.elastic.co/guide/en/elasticsearch/reference/6.2/docs-index_.html for more
%% information.
-spec insert_document(index(), type(), id(), document()) -> {ok, Resp :: map()} | {error, term()}.
insert_document(Index, Type, Id, Document) ->
    case tirerl:insert_doc(?POOL_NAME, Index, Type, Id, Document) of
        {ok, #{<<"_id">> := Id}} = Resp ->
            Resp;
        {error, _} = Err ->
            Err
    end.

%% @doc Runs a search query on a given ElasticSearch index.
%%
%% See https://www.elastic.co/guide/en/elasticsearch/reference/6.2/search.html for more information.
-spec search(index(), type(), query()) -> {ok, Resp :: map()} | {error, term()}.
search(Index, Type, SearchQuery) ->
    tirerl:search(?POOL_NAME, Index, Type, SearchQuery).

%% @doc Retrieves count of documents matching given search query in given ElasticSearch index.
%%
%% See https://www.elastic.co/guide/en/elasticsearch/reference/6.2/search-count.html for more
%% information.
-spec count(index(), type(), query()) -> {ok, Count :: non_neg_integer()} | {error, term()}.
count(Index, Type, SearchQuery) ->
    case tirerl:count(?POOL_NAME, Index, Type, SearchQuery, []) of
        {ok, #{<<"count">> := Count}} when is_integer(Count), Count >= 0 ->
            {ok, Count};
        {error, _} = Err ->
            Err
    end.

%% @doc Deletes documents matching a query in a given ElasticSearch index.
%%
%% See https://www.elastic.co/guide/en/elasticsearch/reference/6.2/docs-delete-by-query.html for
%% more information.
-spec delete_by_query(index(), type(), query()) -> ok | {error, term()}.
delete_by_query(Index, Type, SearchQuery) ->
    case tirerl:delete_by_query(?POOL_NAME, Index, Type, SearchQuery, []) of
        {ok, _} ->
            ok;
        {error, _} = Err ->
            Err
    end.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

