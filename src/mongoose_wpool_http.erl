-module(mongoose_wpool_http).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).

init() ->
    case ets:info(mongoose_http_client) of
        undefined ->
            Heir = case whereis(ejabberd_sup) of
                       undefined -> [];
                       Pid -> [{heir, Pid, undefined}]
                   end,
            ets:new(mongoose_http_client,
                    [named_table, public, {read_concurrency, true} | Heir]),
            ok;
        _ ->
            ok
    end.

start(Host, Tag, WpoolOptsIn, ConnOpts) ->
    Name = mongoose_wpool:make_pool_name(http, Host, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    PathPrefix = list_to_binary(gen_mod:get_opt(path_prefix, ConnOpts, "/")),
    RequestTimeout = gen_mod:get_opt(request_timeout, ConnOpts, 2000),
    case wpool:start_sup_pool(Name, WpoolOpts) of
        {ok, Pid} ->
            ets:insert(mongoose_http_client, {Tag, PathPrefix, RequestTimeout}),
            {ok, Pid};
        Other ->
            Other
    end.

wpool_spec(WpoolOptsIn, ConnOpts) ->
    Server = gen_mod:get_opt(server, ConnOpts),
    HttpOpts = gen_mod:get_opt(http_opts, ConnOpts, []),
    Worker = {fusco, {Server, HttpOpts}},
    [{worker, Worker} | WpoolOptsIn].

%%%===================================================================
%%% Internal functions
%%%===================================================================

