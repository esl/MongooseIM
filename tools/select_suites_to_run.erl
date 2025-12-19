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

%% Filter spec terms according to the allowed suites list.
%%
%% Purpose: used by CircleCI "rerun failed" jobs to rewrite a Common Test spec so that only
%% selected suites (typically the ones that failed) are kept.
%%
%% Behaviour:
%% - `{suites|groups|cases|skip_groups|skip_cases, Any, Suite}`: keep only if `Suite` is in `Suites`.
%% - `{skip_suites, Payload}` / `{skip_suites, Any, Payload}`:
%%   * atom payload: keep only if it is in `Suites`
%%   * list payload: keep only entries that refer to suites in `Suites`; drop term if list becomes empty
%%   * other payload: keep unchanged
%% - Any other term: keep unchanged.

filter_term({skip_suites, Payload} = Term, Suites) ->
    filter_skip_suites_payload(2, Term, Payload, Suites);
filter_term({skip_suites, _Any, Payload} = Term, Suites) ->
    filter_skip_suites_payload(3, Term, Payload, Suites);
filter_term({suites, _Any, _Suite} = Term, Suites) ->
    keep_suite_term(Term, Suites);
filter_term({groups, _Any, _Suite} = Term, Suites) ->
    keep_suite_term(Term, Suites);
filter_term({cases, _Any, _Suite} = Term, Suites) ->
    keep_suite_term(Term, Suites);
filter_term({skip_groups, _Any, _Suite} = Term, Suites) ->
    keep_suite_term(Term, Suites);
filter_term({skip_cases, _Any, _Suite} = Term, Suites) ->
    keep_suite_term(Term, Suites);
filter_term(Term, _Suites) ->
    [Term].

keep_suite_term({_Tag, _Any, Suite} = Term, Suites) ->
    case lists:member(Suite, Suites) of
        true -> [Term];
        false -> []
    end.

skip_suites_member(Suite, Suites) when is_atom(Suite) ->
    lists:member(Suite, Suites);
skip_suites_member({Suite, _Reason}, Suites) when is_atom(Suite) ->
    lists:member(Suite, Suites);
skip_suites_member({Suite, _Reason, _Extra}, Suites) when is_atom(Suite) ->
    lists:member(Suite, Suites);
skip_suites_member(_Other, _Suites) ->
    false.

%% Helper to filter the payload of a skip_suites term.
%% If the payload is a single suite atom, we keep the term if the suite is allowed.
%% If the payload is a list of suites, we filter the list and update the term.
%% If the payload is something else, we keep the term as is.
filter_skip_suites_payload(Index, Term, Payload, Suites) ->
    case Payload of
        Suite when is_atom(Suite) ->
            case lists:member(Suite, Suites) of
                true -> [Term];
                false -> []
            end;
        SuiteList when is_list(SuiteList) ->
            Filtered = [S || S <- SuiteList, skip_suites_member(S, Suites)],
            case Filtered of
                [] -> [];
                _ -> [setelement(Index, Term, Filtered)]
            end;
        _Other ->
            [Term]
    end.

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).
