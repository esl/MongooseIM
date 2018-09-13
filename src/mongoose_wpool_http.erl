-module(mongoose_wpool_http).
-behaviour(mongoose_wpool).

-export([wpool_spec/2]).

wpool_spec(WpoolOptsIn, ConnOpts) ->
    Server = gen_mod:get_opt(server, ConnOpts),
    HttpOpts = gen_mod:get_opt(http_opts, ConnOpts, []),
    Worker = {fusco, {Server, HttpOpts}},
    [{worker, Worker} | WpoolOptsIn].

%%%===================================================================
%%% Internal functions
%%%===================================================================

