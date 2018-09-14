-module(mongoose_wpool_generic).
-behaviour(mongoose_wpool).

-export([start/4]).

start(Host, Tag, WpoolOptsIn, _ConnOpts) ->
    Name = mongoose_wpool:make_pool_name(generic, Host, Tag),
    wpool:start_sup_pool(Name, WpoolOptsIn).
