-module(mongoose_lib_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [pmap_works].

init_per_suite(C) ->
    C.

end_per_suite(C) ->
    C.

pmap_works(_C) ->
    ?assertEqual([{ok, 1}, {ok, 2}, {ok, 3}],
                 mongoose_lib:pmap(fun(X) -> X end, [1, 2, 3])),
    ?assertMatch([{ok, 1}, {error, {oops, _}}, {ok, 3}],
                 mongoose_lib:pmap(fun(2) -> error(oops); (X) -> X end,
                                   [1, 2, 3])),
    ?assertMatch([_, {error, timeout}, _],
                 mongoose_lib:pmap(fun(2) -> timer:sleep(50000); (X) -> X end,
                                   [1, 2, 3], 10)).
