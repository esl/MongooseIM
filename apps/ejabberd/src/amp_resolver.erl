-module(amp_resolver).
%% @doc This module is responsible for checking whether particular AMP semantics
%%      apply for a given message.

-export([check_condition/4,
         verify_support/2
        ]).

-include_lib("ejabberd/include/amp.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-spec verify_support(any(), amp_rules()) -> [ amp_rule_support() ].
verify_support(HookAcc, Rules) ->
    HookAcc ++ [ verify_rule_support(Rule) || Rule <- Rules ].

-spec verify_rule_support(amp_rule()) -> amp_rule_support().
verify_rule_support(#amp_rule{action = drop} = Rule) ->
    {error, 'unsupported-actions', Rule};
verify_rule_support(#amp_rule{action = alert} = Rule) ->
    {error, 'unsupported-actions', Rule};
verify_rule_support(#amp_rule{condition = 'expire-at'} = Rule) ->
    {error, 'unsupported-conditions', Rule};
verify_rule_support(Rule) ->
    {supported, Rule}.

-spec check_condition(any(), amp_strategy(), amp_condition(), amp_value())
                          -> boolean().
check_condition(HookAcc, Strategy, Condition, Value) ->
    case HookAcc of
        true -> true;
        _    -> resolve(Strategy, Condition, Value)
    end.

-spec resolve(amp_strategy(), amp_condition(), amp_value()) -> boolean().
resolve(#amp_strategy{} = S , 'deliver', Value) ->
    S#amp_strategy.deliver =:= Value;
resolve(#amp_strategy{} = S , 'match-resource', Value) when Value =:= 'any' ->
    S#amp_strategy.'match-resource' =/= undefined;
resolve(#amp_strategy{} = S , 'match-resource', Value)  ->
    S#amp_strategy.'match-resource' =:= Value;
resolve(#amp_strategy{} = S , 'expire-at', Value) ->
    %% WARNING: This rule is not supported by mod_amp
    S#amp_strategy.'expire-at' =:= Value;
resolve(#amp_strategy{}, _, _) -> false.
