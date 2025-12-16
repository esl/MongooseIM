%% Escript used to filter the test spec according to a list of allowed suites
%% This is used to rerun failed tests on CircleCI
%%
%% Arguments: TEST_SPEC SUITE1 SUITE2 ..
%% Example arguments: default.spec muc_SUITE rdbms_SUITE

-module(select_suites_to_run).
-export([main/1]).

main([SpecFile | SuiteStrings]) ->
    Suites = [list_to_atom(Str) || Str <- SuiteStrings],
    io:format("Allowed suites: ~p~n", [Suites]),
    {ok, OldTerms} = file:consult(SpecFile),
    NewTerms = lists:flatmap(fun(Term) -> filter_term(Term, Suites) end, OldTerms),
    write_terms(SpecFile, NewTerms),
    ok.

filter_term(Term, Suites) when is_tuple(Term), tuple_size(Term) >= 3 ->
    case element(1, Term) of
        suites -> keep_suite_term(Term, Suites);
        groups -> keep_suite_term(Term, Suites);
        cases -> keep_suite_term(Term, Suites);
        skip_groups -> keep_suite_term(Term, Suites);
        skip_cases -> keep_suite_term(Term, Suites);
        skip_suites ->
            case element(3, Term) of
                SuiteList when is_list(SuiteList) ->
                    Filtered = [S || S <- SuiteList, lists:member(S, Suites)],
                    case Filtered of
                        [] -> [];
                        _ -> [setelement(3, Term, Filtered)]
                    end;
                Suite when is_atom(Suite) ->
                    keep_suite_term(Term, Suites);
                _ ->
                    [Term]
            end;
        _ ->
            [Term]
    end;
filter_term(Term, _Suites) ->
    [Term].

keep_suite_term(Term, Suites) ->
    case lists:member(element(3, Term), Suites) of
        true -> [Term];
        false -> []
    end.

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).
