-module(mongoose_config_SUITE).
-author("mikhail uvarov").
-compile([export_all]).


all() -> [
    does_pattern_match_case
].

init_per_suite(C) -> C.
end_per_suite(C) -> C.
init_per_testcase(_TC, C) -> C.
end_per_testcase(_TC, C) -> C.


%% TESTS

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
