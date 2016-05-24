-module(amp_resolver_SUITE).
%% @doc Tests for resoloving server-side amp_strategy()
%%      with amp_condition()/valu() pairs.
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr LLC

-compile([export_all]).

-include_lib("ejabberd/include/amp.hrl").
-include_lib("exml/include/exml.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(prop_helper, [prop/2, readable_bitstring/0]).

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [{group, deliver_strategy},
     {group, match_resource_strategy},
     {group, null_strategy},
     {group, folding}
    ].

groups() ->
    [{deliver_strategy, [parallel],
      [deliver_none_matches,
       deliver_none_non_matches,
       deliver_direct_matches,
       deliver_direct_non_matches,
       deliver_forward_matches,
       deliver_forward_non_matches,
       deliver_undecided]},
     {match_resource_strategy, [parallel],
      [match_res_any_matches,
       match_res_any_matches_everything_except_undefined,
       match_res_exact_matches,
       match_res_exact_non_matches,
       match_res_other_matches,
       match_res_other_non_matches]},
     {null_strategy, [no_valid_rules_match_null_strategy]},
     {folding, [fold_on_true_is_always_true]}
    ].

fold_on_true_is_always_true(_) ->
    prop(fold_on_true_is_always_true,
         ?FORALL({Strategy, #amp_rule{condition=C, value=V}},
                 {amp_gen:strategy(), amp_gen:valid_rule()},
                 true == amp_resolver:check_condition(true, Strategy, C, V))).

deliver_none_matches(_)        -> strategy_match_prop(deliver, none).
deliver_none_non_matches(_)    -> strategy_nomatch_prop(deliver, none).
deliver_direct_matches(_)      -> strategy_match_prop(deliver, direct).
deliver_direct_non_matches(_)  -> strategy_nomatch_prop(deliver, direct).
deliver_forward_matches(_)     -> strategy_match_prop(deliver, forward).
deliver_forward_non_matches(_) -> strategy_nomatch_prop(deliver, forward).

deliver_undecided(_)           -> strategy_undecided_delivery_prop().

match_res_any_matches(_)       -> strategy_match_prop('match-resource', any).
match_res_exact_matches(_)     -> strategy_match_prop('match-resource', exact).
match_res_exact_non_matches(_) -> strategy_nomatch_prop('match-resource', exact).
match_res_other_matches(_)     -> strategy_match_prop('match-resource', other).
match_res_other_non_matches(_) -> strategy_nomatch_prop('match-resource', other).

match_res_any_matches_everything_except_undefined(_) ->
    %% Amp rules with match-resource = 'any' should actually match
    %% both 'other' and 'exact' server-side matching strategies.
    %% They should only fail to match on an undefined strategy
    MatchExactStrat = #amp_strategy{ 'match-resource' = exact },
    MatchOtherStrat = #amp_strategy{ 'match-resource' = other },
    ?ae(match, check_cond(MatchExactStrat, 'match-resource', 'any')),
    ?ae(match, check_cond(MatchOtherStrat, 'match-resource', 'any')).

no_valid_rules_match_null_strategy(_) ->
    prop(no_valid_rules_match_null_strategy,
         ?FORALL(#amp_rule{condition=C, value=V}, amp_gen:valid_rule(),
                 no_match == check_cond(amp_strategy:null_strategy(), C, V))).

%% @doc Aspect is deliver, expire-at, or match-resource
strategy_match_prop(Aspect, Value) ->
    prop(list_to_atom(a2l(Aspect) ++ "_" ++ a2l(Value) ++ "_matches"),
         ?FORALL({S, #amp_rule{condition=C, value=V}},
                 {amp_gen:strategy({Aspect, Value}), amp_gen:valid_rule()},
                 ?IMPLIES({C, V} == {Aspect, Value},
                          match == check_cond(S, C, Value)))).

strategy_undecided_delivery_prop() ->
    prop(delivery_prop_undecided,
         ?FORALL({S = #amp_strategy{deliver = D}, #amp_rule{condition=C, value=V}},
                 {amp_gen:strategy(), amp_gen:valid_rule()},
                 ?IMPLIES(C == deliver andalso lists:member(V, D) andalso length(D) > 1,
                          undecided == check_cond(S, C, V)))).

strategy_nomatch_prop(Aspect, Value) ->
    prop(list_to_atom(a2l(Aspect) ++ "_" ++ a2l(Value) ++ "_doesnt_match"),
         ?FORALL({S, #amp_rule{condition=C, value=V}},
                 {amp_gen:strategy({Aspect, Value}), amp_gen:valid_rule()},
                 %% the 'any' bit filters out out match-res=any. Ugly hack, sry.
                 %% If we allowed any (which is like a Kleene star), then
                 %% the property that it doesn't match wouldn't hold.
                 ?IMPLIES(C == Aspect andalso V =/= Value andalso V =/= 'any',
                          no_match == check_cond(S, C, V)))).

%% Short-cuts
check_cond(S, C, V) -> amp_resolver:check_condition(no_match, S, C, V).
a2l(A) -> atom_to_list(A).
