-module(presets_to_dbs).
-export([main/0]).
-export([main/1]).

main() ->
    main([]).

main(Opts) ->
    PresetNames = opts_to_presets(Opts),
    DbNames = presets_to_dbs(PresetNames),
    print_dbs(DbNames),
    erlang:halt().

-spec presets_to_dbs(atom()) -> [atom()].
presets_to_dbs(PresetNames) ->
    {ok, Config} = file:consult("big_tests/test.config"),
    Presets = proplists:get_value(ejabberd_presets, Config, []),
    Presets2 = lists:filter(fun({Name,_}) -> lists:member(Name, PresetNames) end, Presets),
    Dbs = lists:flatmap(fun({_,Opts}) -> proplists:get_value(dbs, Opts, []) end, Presets2),
    lists:usort(Dbs).

print_dbs(DbNames) ->
    [io:format("~p~n", [D]) || D <- DbNames].

%% Presets can be passed as a list of arguments "preset1" "preset2"
%% or as a list inside an argument "preset1 preset2"
opts_to_presets(Opts) when is_list(Opts) ->
    OptsStrings = lists:map(fun atom_to_list/1, Opts),
    Joined = string:join(OptsStrings, " "),
    Presets = string:tokens(Joined, " "),
    lists:map(fun list_to_atom/1, Presets).
