-module(mongoose_hooks_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-define(HOST, <<"localhost">>).
-define(PRT(X, Y), ct:pal("~p: ~p~n", [X, Y])).

all() ->
    [%{group, run},
     %{group, runfold}
%%     {group, runstop}
        {group, runfoldstop}
    ].

groups() ->
    [{run, [sequence],
        [run_oldstyle,
         run_oldstyle_both,
         run_newstyle,
         run_newstyle_both
        ]},
     {runfold, [sequence],
         [runfold_oldstyle,
          runfold_newstyle]
     },
        {runstop, [sequence],
            [
                run_oldstyle_stop_old,
                run_oldstyle_stop_new,
                run_newstyle_stop_old,
                run_newstyle_stop_new
                ]
        },
        {runfoldstop, [sequence],
            [
                runfold_oldstyle_stop_old,
                runfold_oldstyle_stop_new,
                runfold_newstyle_stop_old,
                runfold_newstyle_stop_new
            ]
        }
    ].

%%% initialisation

init_per_testcase(_, C) ->
    application:ensure_all_started(exometer),
    ejabberd_hooks:start_link(),
    ets:new(local_config, [named_table]),
    ets:new(handlerruns, [named_table]),
    register_handlers(),
    C.

register_handlers() ->
    ejabberd_hooks:add(oldhook, ?HOST, ?MODULE, oldstylehandler, 10),
    mongoose_hooks:add(newhook, ?HOST, ?MODULE, newstylehandler, 20),
    mongoose_hooks:add(newhook, ?HOST, ?MODULE, newstylehandler1, 30),
    ejabberd_hooks:add(bothhooks, ?HOST, ?MODULE, oldstylehandler, 10),
    mongoose_hooks:add(bothhooks, ?HOST, ?MODULE, newstylehandler, 20),
    mongoose_hooks:add(bothhooks, ?HOST, ?MODULE, newstylehandler1, 30),
    ejabberd_hooks:add(hookfold, ?HOST, ?MODULE, oldstylefolding, 10),
    mongoose_hooks:add(hookfold, ?HOST, ?MODULE, newstylefolding, 20),
    mongoose_hooks:add(hookfold, ?HOST, ?MODULE, newstylefolding1, 30),
    ok.

%%% tests

run_oldstyle(_) ->
    ok = ejabberd_hooks:run(ahook, []),
    ok = ejabberd_hooks:run(oldhook, ?HOST, [1]),
    assert_handler_ran(oldstylehandler),
    ok = ejabberd_hooks:run(newhook, ?HOST, [1]),
    assert_handler_ran(newstylehandler),
    ok.

run_oldstyle_both(_) ->
    ok = ejabberd_hooks:run(bothhooks, ?HOST, [1]),
    assert_handler_ran(oldstylehandler),
    assert_handler_ran(newstylehandler),
    assert_handler_ran(newstylehandler1),
    ok.

run_newstyle(_) ->
    P1 = #xmlel{name = "packet"},
    mongoose_hooks:run(ahook, P1, []),
    mongoose_hooks:run(oldhook, ?HOST, P1, [1]),
    assert_handler_ran(oldstylehandler),
    mongoose_hooks:run(newhook, ?HOST, P1, [1]),
    assert_handler_ran(newstylehandler),
    ok.

run_newstyle_both(_) ->
    P1 = #xmlel{name = "packet"},
    P2 = mongoose_hooks:run(bothhooks, ?HOST, P1, [1]),
    assert_handler_ran(oldstylehandler),
    assert_handler_ran(newstylehandler),
    assert_handler_ran(newstylehandler1),
    "packet_a_b" = P2#xmlel.name,
    ok.

runfold_oldstyle(_) ->
    112 = ejabberd_hooks:run_fold(hookfold, ?HOST, 1, []),
    assert_handler_ran(oldstylefolding),
    assert_handler_ran(newstylefolding),
    assert_handler_ran(newstylefolding1),
    ok.

runfold_newstyle(_) ->
    P = #xmlel{name = "packet"},
    {P1, 112} = mongoose_hooks:run_fold(hookfold, ?HOST, 1, P, []),
    "packet_a_b" = P1#xmlel.name,
    assert_handler_ran(oldstylefolding),
    assert_handler_ran(newstylefolding),
    assert_handler_ran(newstylefolding1),
    ok.

run_oldstyle_stop_old(_) ->
    ejabberd_hooks:add(bothhooks, ?HOST, ?MODULE, oldstylestop, 25),
    stopped = ejabberd_hooks:run(bothhooks, ?HOST, [1]),
    assert_handler_ran(oldstylehandler),
    assert_handler_ran(newstylehandler),
    assert_handler_ran(oldstylestop),
    assert_handler_not_ran(newstylehandler1),
    ok.

run_oldstyle_stop_new(_) ->
    mongoose_hooks:add(bothhooks, ?HOST, ?MODULE, newstylestop, 25),
    stopped = ejabberd_hooks:run(bothhooks, ?HOST, [1]),
    assert_handler_ran(oldstylehandler),
    assert_handler_ran(newstylehandler),
    assert_handler_ran(newstylestop),
    assert_handler_not_ran(newstylehandler1),
    ok.

run_newstyle_stop_old(_) ->
    P = #xmlel{name = "packet"},
    ejabberd_hooks:add(bothhooks, ?HOST, ?MODULE, oldstylestop, 25),
    P1 = mongoose_hooks:run(bothhooks, ?HOST, P, [1]),
    assert_handler_ran(oldstylehandler),
    assert_handler_ran(newstylehandler),
    assert_handler_ran(oldstylestop),
    assert_handler_not_ran(newstylehandler1),
    "packet_a" = P1#xmlel.name,
    ok.

run_newstyle_stop_new(_) ->
    P = #xmlel{name = "packet"},
    mongoose_hooks:add(bothhooks, ?HOST, ?MODULE, newstylestop, 25),
    P1 = mongoose_hooks:run(bothhooks, ?HOST, P, [1]),
    assert_handler_ran(oldstylehandler),
    assert_handler_ran(newstylehandler),
    assert_handler_ran(newstylestop),
    assert_handler_not_ran(newstylehandler1),
    "packet_a_c" = P1#xmlel.name,
    ok.

runfold_oldstyle_stop_old(_) ->
    ejabberd_hooks:add(hookfold, ?HOST, ?MODULE, oldstylefoldstop, 25),
    stopped = ejabberd_hooks:run_fold(hookfold, ?HOST, 1, []),
    ejabberd_hooks:add(hookfold, ?HOST, ?MODULE, oldstylefoldstop_val, 22),
    345 = ejabberd_hooks:run_fold(hookfold, ?HOST, 1, []),
    ok.

runfold_oldstyle_stop_new(_) ->
    mongoose_hooks:add(hookfold, ?HOST, ?MODULE, newstylefoldstop, 25),
    stopped = ejabberd_hooks:run_fold(hookfold, ?HOST, 1, []),
    mongoose_hooks:add(hookfold, ?HOST, ?MODULE, newstylefoldstop_val, 22),
    2345 = ejabberd_hooks:run_fold(hookfold, ?HOST, 1, []),
    assert_handler_not_ran(newstylefolding1),
    ok.

runfold_newstyle_stop_old(_) ->
    P = #xmlel{name = "packet"},
    ejabberd_hooks:add(hookfold, ?HOST, ?MODULE, oldstylefoldstop, 25),
    stopped = mongoose_hooks:run_fold(hookfold, ?HOST, 1, P, []),
    ejabberd_hooks:add(hookfold, ?HOST, ?MODULE, oldstylefoldstop_val, 22),
    {#xmlel{name = "packet_a"}, 345} = mongoose_hooks:run_fold(hookfold, ?HOST, 1, P, []),
    ok.

runfold_newstyle_stop_new(_) ->
    P = #xmlel{name = "packet"},
    mongoose_hooks:add(hookfold, ?HOST, ?MODULE, newstylefoldstop, 25),
    stopped = mongoose_hooks:run_fold(hookfold, ?HOST, 1, P, []),
    mongoose_hooks:add(hookfold, ?HOST, ?MODULE, newstylefoldstop_val, 22),
    {#xmlel{name = "packet_a_d"}, 2345} = mongoose_hooks:run_fold(hookfold, ?HOST, 1, P, []),
    ok.

%%% handlers

oldstylehandler(Arg1) when is_integer(Arg1) ->
    handler_ran(oldstylehandler),
    ok.

newstylehandler(nopacket, _) ->
    handler_ran(newstylehandler, nopacket),
    nopacket;
newstylehandler({packet, P}, _) ->
    handler_ran(newstylehandler, withpacket),
    {packet, P#xmlel{name = P#xmlel.name ++ "_a"}}.

newstylehandler1(nopacket, _) ->
    handler_ran(newstylehandler1, nopacket),
    nopacket;
newstylehandler1({packet, P}, _) ->
    handler_ran(newstylehandler1, withpacket),
    {packet, P#xmlel{name = P#xmlel.name ++ "_b"}}.

oldstylefolding(Arg1) when is_integer(Arg1) ->
    handler_ran(oldstylefolding, Arg1),
    Arg1 + 1.

newstylefolding(nopacket, A) ->
    handler_ran(newstylefolding, nopacket),
    {nopacket, A + 10};
newstylefolding({packet, P}, A) ->
    handler_ran(newstylefolding, withpacket),
    {{packet, P#xmlel{name = P#xmlel.name ++ "_a"}}, A + 10}.

newstylefolding1(nopacket, A) ->
    handler_ran(newstylefolding1, nopacket),
    {nopacket, A + 100};
newstylefolding1({packet, P}, A) ->
    handler_ran(newstylefolding1, withpacket),
    {{packet, P#xmlel{name = P#xmlel.name ++ "_b"}}, A + 100}.

oldstylestop(_) ->
    handler_ran(oldstylestop),
    stop.
newstylestop(nopacket, _) ->
    handler_ran(newstylestop),
    {stop, nopacket};
newstylestop({packet, P}, _) ->
    handler_ran(newstylestop),
    {stop, {packet, P#xmlel{name = P#xmlel.name ++ "_c"}}}.

oldstylefoldstop(_) ->
    stop.
oldstylefoldstop_val(_) ->
    {stop, 345}.

newstylefoldstop(_, _) ->
    stop.

newstylefoldstop_val(nopacket, _) ->
    {stop, 2345};
newstylefoldstop_val({packet, P}, _) ->
    {stop, {packet, P#xmlel{name = P#xmlel.name ++ "_d"}}, 2345}.

%%% helpers

handler_ran(HandlerName) ->
    handler_ran(HandlerName, 1).
handler_ran(HandlerName, Tag) ->
    ct:pal("{HandlerName, Tag}: ~p", [{HandlerName, Tag}]),
    ets:insert(handlerruns, {HandlerName, Tag}).

assert_handler_ran(HandlerName) ->
    [_] = ets:lookup(handlerruns, HandlerName),
    ok.

assert_handler_not_ran(HandlerName) ->
    [] = ets:lookup(handlerruns, HandlerName).

