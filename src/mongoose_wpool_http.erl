-module(mongoose_wpool_http).
-behaviour(mongoose_wpool).

-export([start/4]).

start(Host, Tag, WpoolOptsIn, ConnOpts) ->
    Name = mongoose_wpool:make_pool_name(http, Host, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    wpool:start_sup_pool(Name, WpoolOpts).

wpool_spec(WpoolOptsIn, ConnOpts) ->
    Server = gen_mod:get_opt(server, ConnOpts),
    HttpOpts = gen_mod:get_opt(http_opts, ConnOpts, []),
    Worker = {fusco, {Server, HttpOpts}},
    [{worker, Worker} | WpoolOptsIn].

%%%===================================================================
%%% Internal functions
%%%===================================================================

