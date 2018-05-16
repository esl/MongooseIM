-module(test).

-include("mongoose_acc.hrl").

-compile(export_all).

test() ->
    ?new_acc().
