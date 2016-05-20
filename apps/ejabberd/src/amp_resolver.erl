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

-spec check_condition(amp_match_result(), amp_strategy(), amp_condition(), amp_value()) ->
                             amp_match_result().
check_condition(HookAcc, Strategy, Condition, Value) ->
    case HookAcc of
        no_match -> resolve(Strategy, Condition, Value);
        match -> match
    end.

-spec resolve(amp_strategy(), amp_condition(), amp_value()) -> amp_match_result().
resolve(#amp_strategy{deliver = [Value]}, deliver, Value) -> match;
resolve(#amp_strategy{deliver = Values}, deliver, Value) ->
    case lists:member(Value, Values) of
        true -> undecided;
        false -> no_match
    end;
resolve(#amp_strategy{'match-resource' = undefined}, 'match-resource', any) -> no_match;
resolve(#amp_strategy{}, 'match-resource', any) -> match;
resolve(#amp_strategy{'match-resource' = Value}, 'match-resource', Value) -> match;
resolve(#amp_strategy{}, 'match-resource', _Value) -> no_match;
resolve(#amp_strategy{}, _, _) -> no_match.
