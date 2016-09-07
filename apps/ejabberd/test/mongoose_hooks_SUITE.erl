-module(mongoose_hooks_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-define(HOST, <<"localhost">>).
-define(PRT(X, Y), ct:pal("~p: ~p~n", [X, Y])).

all() ->
    [{group, run}].

groups() ->
    [{run, [sequence],
        [run_oldstyle,
         run_oldstyle_both,
         run_newstyle,
         run_newstyle_both,
         run_check_packet_modification
        ]}
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
    ok.

run_newstyle(_) ->
    P1 = #xmlel{name = "packet"},
    ejabberd_hooks:run(ahook, [{packet, P1}]),
    ejabberd_hooks:run(oldhook, ?HOST, [{packet, P1}, 1]),
    assert_handler_ran(oldstylehandler),
    ejabberd_hooks:run(newhook, ?HOST, [{packet, P1}, 1]),
    assert_handler_ran(newstylehandler),
    ok.

run_newstyle_both(_) ->
    P1 = #xmlel{name = "packet"},
    ejabberd_hooks:run(bothhooks, ?HOST, [{packet, P1}, 1]),
    assert_handler_ran(oldstylehandler),
    assert_handler_ran(newstylehandler),
    ok.

run_check_packet_modification(_) ->
    P1 = #xmlel{name = "packet"},
    P2 = ejabberd_hooks:run(bothhooks, ?HOST, [{packet, P1}, 1]),
    "packet_a_b" = P2#xmlel.name,
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
    nopacket;
newstylehandler1({packet, P}, _) ->
    {packet, P#xmlel{name = P#xmlel.name ++ "_b"}}.

%%% helpers

handler_ran(HandlerName) ->
    handler_ran(HandlerName, 1).
handler_ran(HandlerName, Tag) ->
    ets:insert(handlerruns, {HandlerName, Tag}).

assert_handler_ran(HandlerName) ->
    [_] = ets:lookup(handlerruns, HandlerName),
    ok.

assert_handler_not_ran(HandlerName) ->
    [] = ets:lookup(handlerruns, HandlerName).

