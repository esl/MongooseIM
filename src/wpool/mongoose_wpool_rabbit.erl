-module(mongoose_wpool_rabbit).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

-define(MAX_WORKER_MSG_QUEUE_LEN, 1000).

init() ->
    application:ensure_all_started(amqp_client).

start(Host, Tag, WpoolOptsIn, AMQPOpts) ->
    IsConfirmEnabled = proplists:get_value(confirms_enabled, AMQPOpts, false),
    PoolName = mongoose_wpool:make_pool_name(rabbit, Host, Tag),
    Worker = {mongoose_rabbit_worker,
              [{amqp_client_opts, amqp_client_opts(AMQPOpts)},
               {host, Host},
               {pool_tag, Tag},
               {confirms, IsConfirmEnabled},
               {max_queue_len, proplists:get_value(max_worker_queue_len,
                                                   AMQPOpts,
                                                   ?MAX_WORKER_MSG_QUEUE_LEN)}]},
    WpoolOpts = [{worker, Worker} | WpoolOptsIn],
    mongoose_wpool:start_sup_pool(rabbit, PoolName, WpoolOpts).

stop(_, _) ->
    ok.

amqp_client_opts(AMQPOpts) ->
    Opts = [{host, proplists:get_value(amqp_host, AMQPOpts)},
            {port, proplists:get_value(amqp_port, AMQPOpts)},
            {username, list_to_bin_or_undef(proplists:get_value(amqp_username,
                                                                AMQPOpts))},
            {password, list_to_bin_or_undef(proplists:get_value(amqp_password,
                                                                AMQPOpts))}],
    VerifiedOpts = verify_opts(Opts),
    mongoose_amqp:network_params(VerifiedOpts).

list_to_bin_or_undef(Val) when is_list(Val) ->
    binary:list_to_bin(Val);
list_to_bin_or_undef(_) ->
    undefined.

verify_opts(Opts) ->
    lists:filter(fun({_Opt, undefined}) -> false;
                    (_Other) -> true
                 end, Opts).

