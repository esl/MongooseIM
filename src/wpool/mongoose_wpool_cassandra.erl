-module(mongoose_wpool_cassandra).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
init() ->
    {ok, []} = application:ensure_all_started(cqerl),
    application:set_env(cqerl, maps, true).

start(HostType, Tag, WpoolOptsIn, CqerlOpts) ->
    PoolSize = proplists:get_value(workers, WpoolOptsIn, 20),
    application:set_env(cqerl, num_clients, PoolSize),
    ExtConfig = extend_config(CqerlOpts),
    Servers = proplists:get_value(servers, ExtConfig),
    set_cluster_config(Tag, Servers, ExtConfig),
    Res = cqerl_cluster:add_nodes(Tag, Servers, ExtConfig),
    case lists:keyfind(error, 1, Res) of
        false ->
            ok;
        _ ->
            erlang:throw({not_all_nodes_added, Res})
    end,
    Name = mongoose_wpool:make_pool_name(cassandra, HostType, Tag),
    Worker = {mongoose_cassandra_worker, [Tag]},
    WpoolOpts = [{worker, Worker} | WpoolOptsIn],
    mongoose_wpool:start_sup_pool(cassandra, Name, WpoolOpts).

stop(_, _) ->
    ok.

%% --------------------------------------------------------------
%% Internal functions
extend_config(PoolConfig) ->
    Defaults = #{
        servers     => [{"localhost", 9042}],
        tcp_opts    => [{keepalive, true}],
        keyspace    => mongooseim
    },
    ConfigMap = maps:merge(Defaults, PoolConfig),
    maps:to_list(ConfigMap).

%% make the config survive the restart of 'cqerl_cluster' in case of a network failure
set_cluster_config(Tag, Servers, ExtConfig) ->
    Clusters = application:get_env(cqerl, cassandra_clusters, []),
    ClusterConfig = {Tag, {Servers, ExtConfig}},
    NewClusters = lists:keystore(Tag, 1, Clusters, ClusterConfig),
    application:set_env(cqerl, cassandra_clusters, NewClusters).
