-module(apply_templates).
-export([main/0]).
-export([main/1]).

main() ->
    main([]).

main([NodeAtom, BuildDirAtom]) ->
    log("NodeAtom=~p~n", [NodeAtom]),
    log("BuildDirAtom=~p~n", [BuildDirAtom]),
    BuildDir = atom_to_list(BuildDirAtom),
    RelDir = BuildDir ++ "/rel/mongooseim",
    Templates = templates(RelDir, NodeAtom),
    log("Templates:~n~p~n", [Templates]),
    Vars0 = overlay_vars(NodeAtom),
    Vars = Vars0#{output_dir => list_to_binary(RelDir)},
    log("Vars:~n~p~n", [Vars]),
    [render_template(In, Out, Vars) || {In, Out} <- Templates],
    erlang:halt().


overlay_vars(Node) ->
    File = "rel/" ++ atom_to_list(Node) ++ ".vars-toml.config",
    ensure_binary_strings(maps:from_list(read_vars(File))).

read_vars(File) ->
    {ok, Terms} = file:consult(File),
    lists:flatmap(fun({Key, Val}) ->
                          [{Key, Val}];
                     (IncludedFile) when is_list(IncludedFile) ->
                          Path = filename:join(filename:dirname(File), IncludedFile),
                          read_vars(Path)
                  end, Terms).

%% bbmustache tries to iterate over lists, so we need to make them binaries
ensure_binary_strings(Vars) ->
    maps:map(fun(_K, V) when is_list(V) -> list_to_binary(V);
                (_K, V) -> V
             end, Vars).

%% Based on rebar.config overlay section
templates(RelDir, NodeAtom) ->
    simple_templates(RelDir) ++ erts_templates(RelDir)
        ++ disco_template(RelDir, NodeAtom).

simple_templates(RelDir) ->
    [{In, RelDir ++ "/" ++ Out} || {In, Out} <- simple_templates()].

simple_templates() ->
    [
     {"rel/files/mongooseim",       "bin/mongooseim"},
     {"rel/files/mongooseimctl",    "bin/mongooseimctl"},
     {"rel/files/app.config",       "etc/app.config"},
     {"rel/files/vm.args",          "etc/vm.args"},
     {"rel/files/vm.dist.args",     "etc/vm.dist.args"},
     {"rel/files/mongooseim.toml",  "etc/mongooseim.toml"}
    ].

erts_templates(RelDir) ->
    %% Usually one directory
    ErtsDirs = filelib:wildcard(RelDir ++ "/erts-*"),
    [{"rel/files/nodetool", ErtsDir ++ "/bin/nodetool"} || ErtsDir <- ErtsDirs].

disco_template(RelDir, NodeAtom) ->
    case lists:member(NodeAtom, [mim1, mim2, mim3]) of
        true ->
            [{"rel/files/cets_disco.txt", RelDir ++ "/etc/cets_disco.txt"}];
        false ->
            []
    end.

render_template(In, Out, Vars) ->
    BinIn = bbmustache:parse_file(In),
    %% Do render twice to allow templates in variables
    BinTmp = bbmustache:compile(BinIn, Vars, render_opts()),
    BinOut = bbmustache:render(BinTmp, Vars, render_opts()),
    case file:write_file(Out, BinOut) of
        ok ->
            ok;
        Other ->
            error({write_file_failed, Out, Other})
    end.

render_opts() ->
    [{key_type, atom}].

%% Prints if VERBOSE env variable is set
log(Format, Args) ->
    case os:getenv("VERBOSE") of
        "1" ->
            io:format(Format, Args);
        _ ->
            ok
    end.
