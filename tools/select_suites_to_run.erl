%% Escript used to filter the test spec according to a list of allowed suites
%% This is used to rerun failed tests on CircleCI
%%
%% Arguments: TEST_SPEC SUITE1 SUITE2 ..
%% Example arguments: default.spec muc_SUITE rdbms_SUITE

-module(select_suites_to_run).
-export([main/1]).

main([SpecFile | SuiteStrings]) ->
    Suites = lists:usort([list_to_atom(Str) || Str <- SuiteStrings, Str =/= []]),
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
            filter_skip_suites_term(Term, Suites);
        _ ->
            [Term]
    end;
filter_term(Term, Suites) when is_tuple(Term), tuple_size(Term) =:= 2, element(1, Term) =:= skip_suites ->
    filter_skip_suites_term(Term, Suites);
filter_term(Term, _Suites) ->
    [Term].

keep_suite_term(Term, Suites) ->
    case lists:member(element(3, Term), Suites) of
        true -> [Term];
        false -> []
    end.

filter_skip_suites_term(Term, Suites) when is_tuple(Term) ->
    case skip_suites_payload(Term) of
        {ok, _Index, Suite} when is_atom(Suite) ->
            case lists:member(Suite, Suites) of
                true -> [Term];
                false -> []
            end;
        {ok, Index, SuiteList} when is_list(SuiteList) ->
            Filtered = [S || S <- SuiteList, skip_suites_member(S, Suites)],
            case Filtered of
                [] -> [];
                _ -> [setelement(Index, Term, Filtered)]
            end;
        error ->
            [Term]
    end.

skip_suites_payload(Term) ->
    case tuple_size(Term) of
        2 -> {ok, 2, element(2, Term)};
        N when N >= 3 -> {ok, 3, element(3, Term)};
        _ -> error
    end.

skip_suites_member(Suite, Suites) when is_atom(Suite) ->
    lists:member(Suite, Suites);
skip_suites_member({Suite, _Reason}, Suites) when is_atom(Suite) ->
    lists:member(Suite, Suites);
skip_suites_member({Suite, _Reason, _Extra}, Suites) when is_atom(Suite) ->
    lists:member(Suite, Suites);
skip_suites_member(_Other, _Suites) ->
    false.

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).
