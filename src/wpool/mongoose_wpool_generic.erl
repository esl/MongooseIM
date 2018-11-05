-module(mongoose_wpool_generic).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

init() ->
    ok.

start(Host, Tag, WpoolOptsIn, _ConnOpts) ->
    Name = mongoose_wpool:make_pool_name(generic, Host, Tag),
    mongoose_wpool:start_sup_pool(generic, Name, WpoolOptsIn).

stop(_, _) ->
    ok.
