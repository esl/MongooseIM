-module(mongoose_config_SUITE).
-author("mikhail uvarov").
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").


all() -> [
    does_pattern_match_case,
    flatten_state_case,
    parse_config_with_underscore_pattern_case
].

init_per_suite(C) ->
    stringprep:start(),
    C.

end_per_suite(C) -> C.
init_per_testcase(_TC, C) -> C.
end_per_testcase(_TC, C) -> C.


%% TESTS

%% Check each pattern with cases that match or not
does_pattern_match_case(_C) ->
    [check_case(Case) || Case <- match_cases()],
    ok.

check_case(Case=#{pattern := Pattern, matches := Matches}) ->
    [check_match(Match, Pattern) || Match <- Matches],
    [check_nomatch(Match, Pattern) || Match <- maps:get(nomatches, Case, [])],
    ok.

check_match(Subject, Pattern) ->
    Match = mongoose_config:does_pattern_match(Subject, Pattern),
    case Match of
        true ->
            ok;
        false ->
            ct:fail(#{issue => check_match_failed,
                      subject => Subject,
                      pattern => Pattern})
    end.

check_nomatch(Subject, Pattern) ->
    Match = mongoose_config:does_pattern_match(Subject, Pattern),
    case Match of
        false ->
            ok;
        true ->
            ct:fail(#{issue => check_nomatch_failed,
                      subject => Subject,
                      pattern => Pattern})
    end.

match_cases() ->
    [
        #{pattern => '_',
          matches => [
            1, {}, [1]
          ]},

        #{pattern => [{'_', <<"host">>}],
          matches => [
            [{test, <<"host">>}]
          ],
          nomatches => [
            [{test, <<"host2">>}],
            [{test, <<"host">>}, key2]
          ]
         }

    ].


flatten_state_case(_C) ->
    State = mongoose_config:parse_terms(cool_mod_mam_config()),
    ?assertEqual(cool_mod_mam_config_flatten(),
                 mongoose_config:flatten_state(State)).

cool_mod_mam_config() ->
    [{hosts, [<<"localhost">>]},
     {modules, [{mod_mam, [{pool, cool_pool}]}]}].

cool_mod_mam_config_flatten() ->
    [{[l,odbc_pools],[]},
     {[h,<<"localhost">>,modules],flatten},
     {[h,<<"localhost">>,module,mod_mam],flatten},
     {[h,<<"localhost">>,module_opt,mod_mam,pool],cool_pool}].


%% Check that underscore is not treated as a config macro by config parser
parse_config_with_underscore_pattern_case(_C) ->
    mongoose_config:parse_terms(node_specific_cool_mod_mam_config()).

node_specific_cool_mod_mam_config() ->
    [{node_specific_options, [ {[h,'_',module_opt,mod_mam,pool],cool_pool} ]},
     {hosts, [<<"localhost">>]},
     {modules, [{mod_mam, [{pool, cool_pool}]}]}].
