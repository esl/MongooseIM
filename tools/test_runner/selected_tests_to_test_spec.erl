-module(selected_tests_to_test_spec).
-export([main/0]).
-export([main/1]).

main() ->
    main([]).

%% If there are only big tests in arguments, small tests would be disabled
%% If there are only small tests in arguments, big tests would be disabled
main([]) ->
    erlang:halt();
main(AtomArgs) ->
    io:format("AtomArgs ~p~n", [AtomArgs]),
    SmallSpecs = small_specs(AtomArgs),
    BigSpecs = big_specs(AtomArgs),
    io:format("SmallSpecs ~p~n", [SmallSpecs]),
    io:format("BigSpecs ~p~n", [BigSpecs]),
    write_small_tests_spec(SmallSpecs),
    write_big_tests_spec(BigSpecs),
    erlang:halt().

small_specs(AtomArgs) ->
    lists:filter(fun is_small_spec/1, AtomArgs).

big_specs(AtomArgs) ->
    lists:filter(fun is_big_spec/1, AtomArgs).

is_small_spec(Atom) ->
    FileName = atom_to_list(spec_to_module(Atom)) ++ ".erl",
    Path = filename:join("test", FileName),
    does_file_exist(Path).

is_big_spec(Atom) ->
    FileName = atom_to_list(spec_to_module(Atom)) ++ ".erl",
    Path = filename:join("big_tests/tests", FileName),
    does_file_exist(Path).

spec_to_module(Atom) ->
    list_to_atom(hd(string:tokens(atom_to_list(Atom), ":")) ++ "_SUITE").

does_file_exist(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _} ->
            true;
        _ ->
            false
    end.


%% We should write some file (even an empty one), if there are any AtomArgs
write_small_tests_spec(SmallSpecs) ->
    Specs = make_small_tests_spec(SmallSpecs),
    write_terms("auto_small_tests.spec", Specs),
    ok.

write_big_tests_spec(BigSpecs) ->
    Specs = make_big_tests_spec(BigSpecs),
    {ok, OldTerms} = file:consult("big_tests/auto_big_tests.spec"),
    Terms = remove_all_specs(OldTerms),
    write_terms("big_tests/auto_big_tests.spec", Terms ++ Specs),
    ok.

make_small_tests_spec(SmallSpecs) ->
    [make_test_spec("test", SmallSpec) || SmallSpec <- SmallSpecs].

make_big_tests_spec(BigSpecs) ->
    [make_test_spec("tests", BigSpec) || BigSpec <- BigSpecs].

%% Make something like:
% {groups, "test", acc_SUITE, [basic], {cases, [store_and_retrieve]}}.
make_test_spec(Dir, Atom) ->
    SubAtoms = lists:map(fun list_to_atom/1,
                         string:tokens(atom_to_list(Atom), ":")),
    make_test_spec_sub_atoms(Dir, SubAtoms).

make_test_spec_sub_atoms(Dir, [ModulePart]) ->
    %% Run the whole suite
    {suites, Dir, spec_to_module(ModulePart)};
make_test_spec_sub_atoms(Dir, SubAtoms) ->
    %% Run a part of suite
    Module = spec_to_module(hd(SubAtoms)),
    Last = lists:last(SubAtoms),
    try is_test_case(Module, Last) of
        true ->
            Groups = sub_atoms_to_groups(SubAtoms),
            case Groups of
                [] ->
                    {cases, Dir, Module, [Last]};
                [_|_] ->
                    {groups, Dir, Module, Groups, {cases, [Last]}}
            end;
        false ->
            Groups = tl(SubAtoms),
            {groups, Dir, Module, Groups}
    catch
        error:undef ->
            io:format("issue=module_cannot_be_loaded run_the_whole_module_instead module=~p", [Module]),
            {suites, Dir, spec_to_module(hd(SubAtoms))}
    end.

sub_atoms_to_groups(SubAtoms) ->
    lists:droplast(tl(SubAtoms)).

%% Naive check
%% Groups do not need a function to be exported
is_test_case(Module, GroupOrTest) ->
    %% Load module
    Module:module_info(),
    erlang:function_exported(Module, GroupOrTest, 1).

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).

remove_all_specs(Terms) ->
    [X || X <- Terms, not is_spec_term(X)].

is_spec_term(X) when element(1, X) == suites;
                     element(1, X) == groups;
                     element(1, X) == cases ->
    true;
is_spec_term(_) ->
    false.
