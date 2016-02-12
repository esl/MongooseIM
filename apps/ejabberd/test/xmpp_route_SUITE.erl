-module(xmpp_route_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").


all() ->
    [ success_with_module_implementing_behaviour,
      fail_with_module_not_implementing_behaviour,
      fail_when_do_route_crashes
    ].

init_per_suite(C) ->
    application:start(p1_stringprep),
    application:ensure_all_started(lager),
    C.

success_with_module_implementing_behaviour(_C) ->
    meck:new(xmpp_router_correct, [non_strict]),
    meck:expect(xmpp_router_correct, do_route,
                fun(_From, _To, _Packet) -> ok end),
    ok = xmpp_router:route(xmpp_router_correct,
                           jid:from_binary(<<"ala@localhost">>),
                           jid:from_binary(<<"bob@localhost">>),
                           message()),
    meck:validate(xmpp_router_correct),
    meck:unload(xmpp_router_correct),
    ok.

fail_with_module_not_implementing_behaviour(_C) ->
    meck:new(xmpp_router_incorrect, [non_strict]),
    meck:expect(xmpp_router_incorrect, no_do_route,
                fun(_From, _To, _Packet) -> ok end),
    meck:new(lager, [unstick, passthrough]),
    ok = xmpp_router:route(xmpp_router_incorrect,
                           jid:from_binary(<<"ala@localhost">>),
                           jid:from_binary(<<"bob@localhost">>),
                           message()),
    meck:validate(lager),
    meck:unload(xmpp_router_incorrect),
    meck:unload(lager),
    ok.


fail_when_do_route_crashes(_C) ->
    meck:new(xmpp_router_crashing, [non_strict]),
    meck:expect(xmpp_router_crashing, do_route,
                fun(_From, _To, _Packet) -> meck:exception(error, sth_wrong) end),
    meck:new(lager, [unstick, passthrough]),
    ok = xmpp_router:route(xmpp_router_crashing,
                           jid:from_binary(<<"ala@localhost">>),
                           jid:from_binary(<<"bob@localhost">>),
                           message()),
    meck:validate(xmpp_router_crashing),
    meck:validate(lager),
    meck:unload(xmpp_router_crashing),
    meck:unload(lager),
    ok.

message() ->
    #xmlel{name = <<"message">>}.

