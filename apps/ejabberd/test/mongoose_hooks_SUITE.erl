-module(mongoose_hooks_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-define(HOST, <<"localhost">>).
-define(PRT(X, Y), ct:pal("~p: ~p~n", [X, Y])).

all() ->
    [runoldstyle,
     runoldstyleboth,
     runnewstyle,
     runnewstyleboth,
     check_packet_modification
    ].


init_per_testcase(_, C) ->
    application:ensure_all_started(exometer),
    ejabberd_hooks:start_link(),
    ets:new(local_config, [named_table]),
    ets:new(hookruns, [named_table]),
    register_hooks(),
    C.

runoldstyle(_) ->
    ok = ejabberd_hooks:run(ahook, []),
    ok = ejabberd_hooks:run(oldhook, ?HOST, [1]),
    assert_hook_run(oldstylehook),
    ok = ejabberd_hooks:run(newhook, ?HOST, [1]),
    assert_hook_run(newstylehook),
    ok.

runoldstyleboth(_) ->
    ok = ejabberd_hooks:run(bothhooks, ?HOST, [1]),
    assert_hook_run(oldstylehook),
    assert_hook_run(newstylehook),
    ok.

runnewstyle(_) ->
    P1 = #xmlel{name = "packet"},
    ejabberd_hooks:run(ahook, [{packet, P1}]),
    ejabberd_hooks:run(oldhook, ?HOST, [{packet, P1}, 1]),
    assert_hook_run(oldstylehook),
    ejabberd_hooks:run(newhook, ?HOST, [{packet, P1}, 1]),
    assert_hook_run(newstylehook),
    ok.

runnewstyleboth(_) ->
    P1 = #xmlel{name = "packet"},
    ejabberd_hooks:run(bothhooks, ?HOST, [{packet, P1}, 1]),
    assert_hook_run(oldstylehook),
    assert_hook_run(newstylehook),
    ok.

check_packet_modification(_) ->
    P1 = #xmlel{name = "packet"},
    P2 = ejabberd_hooks:run(bothhooks, ?HOST, [{packet, P1}, 1]),
    "packet_a_b" = P2#xmlel.name,
    ok.

register_hooks() ->
    ejabberd_hooks:add(oldhook, ?HOST, ?MODULE, oldstylehook, 10),
    mongoose_hooks:add(newhook, ?HOST, ?MODULE, newstylehook, 20),
    mongoose_hooks:add(newhook, ?HOST, ?MODULE, newstylehook1, 30),
    ejabberd_hooks:add(bothhooks, ?HOST, ?MODULE, oldstylehook, 10),
    mongoose_hooks:add(bothhooks, ?HOST, ?MODULE, newstylehook, 20),
    mongoose_hooks:add(bothhooks, ?HOST, ?MODULE, newstylehook1, 30),
    ok.


oldstylehook(Arg1) when is_integer(Arg1) ->
    hook_ran(oldstylehook),
    ok.

newstylehook(nopacket, _) ->
    hook_ran(newstylehook, nopacket),
    nopacket;
newstylehook({packet, P}, _) ->
    hook_ran(newstylehook, withpacket),
    {packet, P#xmlel{name = P#xmlel.name ++ "_a"}}.

newstylehook1(nopacket, _) ->
    nopacket;
newstylehook1({packet, P}, _) ->
    {packet, P#xmlel{name = P#xmlel.name ++ "_b"}}.

hook_ran(HookName) ->
    hook_ran(HookName, 1).
hook_ran(HookName, Tag) ->
    ets:insert(hookruns, {HookName, Tag}).

assert_hook_run(HookName) ->
    [_] = ets:lookup(hookruns, HookName),
    ok.

assert_hook_not_run(HookName) ->
    [] = ets:lookup(hookruns, HookName).

