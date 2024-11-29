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
    NewTerms = lists:filter(fun(Term) -> filter_term(Term, Suites) end, OldTerms),
    write_terms(SpecFile, NewTerms),
    ok.

filter_term(_Term, []) ->
    true;
filter_term(Term, Suites) when element(1, Term) == suites;
                               element(1, Term) == groups;
                               element(1, Term) == cases ->
    lists:member(element(3, Term), Suites);
filter_term(_Term, _Suites) ->
    true.

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).
