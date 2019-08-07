-module(apply_templates).
-export([main/0]).
-export([main/1]).

main() ->
    main([]).

main([NodeAtom, RelDirAtom]) ->
    log("NodeAtom=~p~n", [NodeAtom]),
    log("RelDir=~p~n", [RelDirAtom]),
    RelDir = atom_to_list(RelDirAtom),
    Templates = templates(RelDir),
    log("Templates:~n~p~n", [Templates]),
    Vars = overlay_vars(NodeAtom),
    log("Vars:~n~p~n", [Vars]),
    [render_template(In, Out, Vars) || {In, Out} <- Templates],
    erlang:halt().


overlay_vars(Node) ->
    Vars = consult_map("rel/vars.config"),
    NodeVars = consult_map("rel/" ++ atom_to_list(Node) ++ ".vars.config"),
    %% NodeVars override Vars
    maps:merge(Vars, NodeVars).

consult_map(File) ->
    {ok, Vars} = file:consult(File),
    maps:from_list(Vars).

%% Based on rebar.config overlay section
templates(RelDir) ->
    simple_templates(RelDir) ++ erts_templates(RelDir).

simple_templates(RelDir) ->
    [{In, RelDir ++ "/" ++ Out} || {In, Out} <- simple_templates()].

simple_templates() ->
    [
     {"rel/files/mongooseim",       "bin/mongooseim"},
     {"rel/files/mongooseimctl",    "bin/mongooseimctl"},
     {"rel/files/app.config",       "etc/app.config"},
     {"rel/files/vm.args",          "etc/vm.args"},
     {"rel/files/vm.dist.args",     "etc/vm.dist.args"},
     {"rel/files/mongooseim.cfg",   "etc/mongooseim.cfg"}
    ].

erts_templates(RelDir) ->
    %% Usually one directory
    ErtsDirs = filelib:wildcard(RelDir ++ "/rel/mongooseim/erts-*"),
    [{"rel/files/nodetool", ErtsDir ++ "/bin/nodetool"} || ErtsDir <- ErtsDirs].

render_template(In, Out, Vars) ->
    {ok, BinIn} = file:read_file(In),
    BinOut = bbmustache:render(BinIn, Vars, render_opts()),
    file:write_file(Out, BinOut).

render_opts() ->
    [{escape_fun, fun(X) -> X end}, {key_type, atom}, {value_serializer, fun(X) -> X end}].

%% Prints if VERBOSE env variable is set
log(Format, Args) ->
    case os:getenv("VERBOSE") of
        "1" ->
            io:format(Format, Args);
        _ ->
            ok
    end.
