-module(mongoose_wpool_cassandra).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

init() ->
    {ok, []} = application:ensure_all_started(cqerl),
    application:set_env(cqerl, maps, true).

start(Host, Tag, WpoolOptsIn, CqerlOpts) ->
    PoolSize = proplists:get_value(workers, WpoolOptsIn, 20),
    application:set_env(cqerl, num_clients, PoolSize),
    ExtConfig = extend_config(CqerlOpts),
    Res = cqerl_cluster:add_nodes(Tag, proplists:get_value(servers, ExtConfig), ExtConfig),
    case lists:keyfind(error, 1, Res) of
        false ->
            ok;
        _ ->
            erlang:throw({not_all_nodes_added, Res})
    end,
    Name = mongoose_wpool:make_pool_name(cassandra, Host, Tag),
    Worker = {mongoose_cassandra_worker, [Tag]},
    WpoolOpts = [{worker, Worker} | WpoolOptsIn],
    mongoose_wpool:start_sup_pool(cassandra, Name, WpoolOpts).

stop(_, _) ->
    ok.

extend_config(PoolConfig) ->
    Defaults = #{
        servers     => [{"localhost", 9042}],
        tcp_opts    => [{keepalive, true}],
        keyspace    => mongooseim
    },

    ConfigMap = maps:merge(Defaults, maps:from_list(PoolConfig)),
    maps:to_list(ConfigMap).

