-module(amp_gen).
%% Generators for XEP-0079 models
-compile([export_all]).

-include("amp.hrl").
-include_lib("proper/include/proper.hrl").

strategy() ->
    ?LET({DeliverVal, MatchResourceVal, ExpireAtVal},
         {list(valid_value_binary(<<"deliver">>)),
          valid_value_binary(<<"match-resource">>, server_side),
          valid_value_binary(<<"expire-at">>)},
         #amp_strategy{deliver = remove_duplicates([b2a(D) || D <- DeliverVal]),
                       'match-resource' = b2a(MatchResourceVal),
                       'expire-at' = ExpireAtVal}).

strategy({deliver, Value}) ->
    ?LET(S, strategy(),
         S#amp_strategy{deliver=[Value]});
strategy({'match-resource', Value}) ->
    ?LET(S, strategy(),
         S#amp_strategy{'match-resource'=Value}).

valid_rule() ->
    ?LET({C,V,A}, valid_cva_binaries(),
         amp:binaries_to_rule(C,V,A)).

valid_cva_binaries() ->
    ?LET(Condition, valid_condition_binary(),
         {Condition, valid_value_binary(Condition), valid_action_binary()}).

valid_condition_binary() ->
    oneof(?AMP_LEGAL_CONDITIONS).
valid_value_binary(<<"deliver">>) ->
    oneof(?AMP_LEGAL_DELIVER_VALUES);
valid_value_binary(<<"match-resource">>) ->
    oneof(?AMP_LEGAL_MATCH_RESOURCE_VALUES);
valid_value_binary(<<"expire-at">>) ->
    prop_helper:readable_bitstring().
valid_action_binary() ->
    oneof(?AMP_LEGAL_ACTIONS).

valid_value_binary(<<"match-resource">>, server_side) ->
    oneof(lists:delete(<<"any">>, ?AMP_LEGAL_MATCH_RESOURCE_VALUES)).

invalid_cva_binaries() ->
    {prop_helper:readable_bitstring(),
     prop_helper:readable_bitstring(),
     prop_helper:readable_bitstring()}.

%% Helpers

remove_duplicates(L) ->
    lists:foldl(fun(Elem, Res) ->
                        case lists:member(Elem, Res) of
                            true -> Res;
                            false -> [Elem | Res]
                        end
                end, [], L).

b2a(V) -> erlang:binary_to_atom(V, utf8).
