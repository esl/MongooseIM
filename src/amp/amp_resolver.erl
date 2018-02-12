-module(amp_resolver).
%% @doc This module is responsible for checking whether particular AMP semantics
%%      apply for a given message.

-export([check_condition/3,
         verify_support/2
        ]).

-include("amp.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-spec verify_support(any(), amp_rules()) -> [ amp_rule_support() ].
verify_support(HookAcc, Rules) ->
    HookAcc ++ [ verify_rule_support(Rule) || Rule <- Rules ].

-spec verify_rule_support(amp_rule()) -> amp_rule_support().
verify_rule_support(#amp_rule{action = alert} = Rule) ->
    {error, 'unsupported-actions', Rule};
verify_rule_support(#amp_rule{condition = 'expire-at'} = Rule) ->
    {error, 'unsupported-conditions', Rule};
verify_rule_support(Rule) ->
    {supported, Rule}.

-spec check_condition(amp_match_result(), amp_strategy(), amp_rule()) -> amp_match_result().
check_condition(HookAcc, Strategy, Rule) ->
    case HookAcc of
        no_match -> resolve(Strategy, Rule);
        MatchResult -> MatchResult
    end.

-spec resolve(amp_strategy(), amp_rule()) -> amp_match_result().
resolve(#amp_strategy{deliver = [Value]}, #amp_rule{condition = deliver, value = Value}) -> match;
resolve(#amp_strategy{deliver = Values}, #amp_rule{condition = deliver, value = Value, action = notify})
  when is_list(Values) ->
    case lists:member(Value, Values) of
        true -> undecided;
        false -> no_match
    end;
resolve(#amp_strategy{deliver = Values}, #amp_rule{condition = deliver, value = Value, action = Action})
  when is_list(Values), Action == drop orelse Action == error, Value /= none ->
    case lists:member(Value, Values) of
        true -> match;
        false -> no_match
    end;
resolve(#amp_strategy{'match-resource' = Value}, #amp_rule{condition = 'match-resource', value = any})
  when Value /= undefined -> match;
resolve(#amp_strategy{'match-resource' = Value}, #amp_rule{condition = 'match-resource', value = Value}) ->
    match;
resolve(#amp_strategy{}, #amp_rule{}) -> no_match.
