-module(mongoose_wpool_rabbit).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

init() ->
    application:ensure_all_started(amqp_client).

start(Host, Tag, WpoolOptsIn,
      AMQPOpts = #{confirms_enabled := Confirms, max_worker_queue_len := MaxQueueLen}) ->
    PoolName = mongoose_wpool:make_pool_name(rabbit, Host, Tag),
    Worker = {mongoose_rabbit_worker,
              [{amqp_client_opts, amqp_client_opts(AMQPOpts)},
               {host, Host},
               {pool_tag, Tag},
               {confirms, Confirms},
               {max_queue_len, MaxQueueLen}]},
    WpoolOpts = [{worker, Worker} | WpoolOptsIn],
    mongoose_wpool:start_sup_pool(rabbit, PoolName, WpoolOpts).

stop(_, _) ->
    ok.

amqp_client_opts(AMQPOpts) ->
    AMQPNetworkOpts = maps:with([amqp_host, amqp_port, amqp_username, amqp_password], AMQPOpts),
    KVOpts = maps:to_list(AMQPNetworkOpts),
    mongoose_amqp:network_params(KVOpts).
