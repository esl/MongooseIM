-module(mongoose_wpool_generic).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
init() ->
    ok.

start(HostType, Tag, WpoolOptsIn, _ConnOpts) ->
    ProcName = mongoose_wpool:make_pool_name(generic, HostType, Tag),
    mongoose_wpool:start_sup_pool(generic, ProcName, WpoolOptsIn).

stop(_, _) ->
    ok.
