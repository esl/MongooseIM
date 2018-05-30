-module(mongoose_elasticsearch).

-export([start/0]).
-export([stop/0]).
-export([health/0]).

-include("mongoose.hrl").

-define(POOL_NAME, elasticsearch).

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

%% @doc Starts the pool of connections to ElasticSearch cluster.
%%
%% Currently connections are opened only to a single node.
-spec start() -> ignore | ok | no_return().
start() ->
    Opts = ejabberd_config:get_local_option(elasticsearch_server),
    case Opts of
        undefined ->
            ignore;
        _ ->
            tirerl:start(),
            start_pool(Opts)
    end.

%% @doc Stops the pool of connections to ElasticSearch cluster.
-spec stop() -> ok.
stop() ->
    stop_pool(),
    tirerl:stop().

%% @doc Returns the health status of the ElasticSearch cluster.
%%
%% See https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-health.html for
%% more information.
-spec health() -> {error, term()} | {ok, map()}.
health() ->
    tirerl:health(?POOL_NAME).

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

-spec start_pool(list()) -> ok | no_return().
start_pool(Opts) ->
    Host = proplists:get_value(host, Opts, "localhost"),
    Port = proplists:get_value(port, Opts, 9200),
    case tirerl:start_pool(?POOL_NAME, [{host, list_to_binary(Host)}, {port, Port}]) of
        {ok, _} ->
            ?INFO_MSG("Started pool of connections to ElasticSearch at ~p:~p",
                          [Host, Port]),
            ok;
        {ok, _, _} ->
            ?INFO_MSG("Started pool of connections to ElasticSearch at ~p:~p",
                          [Host, Port]),
            ok;
        {error, _} = Err ->
            ?ERROR_MSG("Failed to start pool of connections to ElasticSearch at ~p:~p: ~p",
                       [Host, Port, Err]),
            error(Err)
    end.

-spec stop_pool() -> any().
stop_pool() ->
    tirerl:stop_pool(?POOL_NAME).

