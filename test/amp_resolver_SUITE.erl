-module(amp_resolver_SUITE).
%% @doc Tests for resoloving server-side amp_strategy()
%%      with amp_condition()/valu() pairs.
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr LLC

-compile([export_all]).

-include("amp.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [{group, resolver}].

groups() ->
    [{resolver, [parallel],
      [deliver_notify_match_test,
       deliver_notify_undecided_test,
       deliver_notify_no_match_test,
       deliver_error_drop_match_test,
       deliver_error_drop_no_match_test,
       match_resource_match_test,
       match_resource_no_match_test,
       fold_on_match_test]
     }].

%% Test cases

deliver_notify_match_test(_) ->
    [?ae(match, check_cond(#amp_strategy{deliver = [D]},
                           #amp_rule{condition = deliver, value = D, action = notify})) ||
        D <- rule_deliver_values()],
    ok.

deliver_notify_undecided_test(_) ->
    [?ae(undecided, check_cond(#amp_strategy{deliver = Ds},
                               #amp_rule{condition = deliver, value = Dr, action = notify})) ||
        Ds <- strategy_deliver_multiple_values(),
        Dr <- Ds],
    ok.

deliver_notify_no_match_test(_) ->
    [?ae(no_match, check_cond(#amp_strategy{deliver = Ds},
                              #amp_rule{condition = deliver, value = Dr, action = notify})) ||
        Ds <- strategy_deliver_values(),
        Dr <- rule_deliver_values(),
        Ds == undefined orelse not lists:member(Dr, Ds)],
    ok.

deliver_error_drop_match_test(_) ->
    [?ae(match, check_cond(#amp_strategy{deliver = Ds},
                           #amp_rule{condition = deliver, value = Dr, action = A})) ||
        Ds <- strategy_deliver_values(),
        Dr <- rule_deliver_values(),
        [Dr] == Ds orelse (Ds /= undefined andalso Dr /= none andalso lists:member(Dr, Ds)),
        A <- [error, drop]],
    ok.

deliver_error_drop_no_match_test(_) ->
    [?ae(no_match, check_cond(#amp_strategy{deliver = Ds},
                              #amp_rule{condition = deliver, value = Dr, action = A})) ||
        Ds <- strategy_deliver_values(),
        Dr <- rule_deliver_values(),
        [Dr] /= Ds,
        Ds == undefined orelse Dr == none orelse not lists:member(Dr, Ds),
        A <- [error, drop]],
    ok.

match_resource_match_test(_) ->
    [?ae(match, check_cond(#amp_strategy{'match-resource' = Ms},
                           #amp_rule{condition = 'match-resource', value = Mr, action = A})) ||
        Ms <- strategy_match_resource_values(),
        Mr <- rule_match_resource_values(),
        Ms /= undefined,
        Mr == any orelse Mr == Ms,
        A <- rule_actions()],
    ok.

match_resource_no_match_test(_) ->
    [?ae(no_match, check_cond(#amp_strategy{'match-resource' = Ms},
                              #amp_rule{condition = 'match-resource', value = Mr, action = A})) ||
        Ms <- strategy_match_resource_values(),
        Mr <- rule_match_resource_values(),
        Ms == undefined orelse (Mr /= any andalso Mr /= Ms),
        A <- rule_actions()],
    ok.

fold_on_match_test(_) ->
    [?ae(Acc, check_cond(Acc,
                        #amp_strategy{},
                        #amp_rule{condition = 'match-resource', value = any, action = notify})) ||
        Acc <- [match, undecided]],
    ok.

%% Helpers

strategy_deliver_values() -> [undefined | strategy_deliver_single_values() ++
                                  strategy_deliver_multiple_values()].
strategy_deliver_single_values() -> [[none], [stored], [direct]].
strategy_deliver_multiple_values() -> [[direct, stored], [direct, none]].
strategy_match_resource_values() -> [undefined, other, exact].

rule_actions() -> [notify, error, drop].
rule_deliver_values() -> [none, stored, direct].
rule_match_resource_values() -> [exact, other, any].

check_cond(Strategy, Rule) ->
    check_cond(no_match, Strategy, Rule).

check_cond(HookAcc, Strategy, Rule) ->
    ct:log("check condition (acc: ~p)~nstrategy: ~p~nrule: ~p", [HookAcc, Strategy, Rule]),
    amp_resolver:check_condition(HookAcc, Strategy, Rule).
