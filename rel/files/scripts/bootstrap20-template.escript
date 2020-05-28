#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noinput

-mode(compile).
-include_lib("kernel/include/file.hrl").

main(_) ->
    io:format("Template script started~n", []),
    MIM_DIR = os:getenv("MIM_DIR"),
    TemplateConfigPath = filename:join(MIM_DIR, "etc/templates.ini"),
    case file:read_file(TemplateConfigPath) of
        {ok, TemplateConfigBin} ->
            init_and_template(MIM_DIR, TemplateConfigPath, TemplateConfigBin);
        _ ->
            io:format("Skip templating because config ~p does not exist",
                      [TemplateConfigPath])
    end.

init_and_template(MIM_DIR, TemplateConfigPath, TemplateConfigBin) ->
    check_mim_dir(MIM_DIR),
    import_libraries(MIM_DIR),
    ensure_deps(),
    TemplateConfig = parse_template_config(TemplateConfigPath, TemplateConfigBin),

    %% Options defined in the ini config file
    FileOpts = maps:from_list(proplists:get_value(options, TemplateConfig, [])),

    LowerEnvVars = [{string:to_lower(K), V} || {K, V} <- os:list_env_vars()],

    %% Add all env variables with prefix MIM_
    EnvVars = maps:from_list([{list_to_atom(K), list_to_binary(V)}
                              || {"mim_" ++ K, V} <- LowerEnvVars]),

    io:format("Found ~p env variables~n", [maps:size(EnvVars)]),

    %% EnvVars have higher priority
    Opts = maps:merge(FileOpts, EnvVars),

    %% Dump variables into a file for debugging
    VarsPath = filename:join(MIM_DIR, "etc/final_template_vars.config"),
    file:write_file(VarsPath, io_lib:format("~p.", [Opts])),

    InputDir = filename:join(MIM_DIR, "templates"),
    OutputDir = filename:join(MIM_DIR, "etc"),

    InputFiles = list_dir_safe(InputDir),
    io:format("InputFiles ~p~n", [InputFiles]),
    [render_file(filename:join(InputDir, File), filename:join(OutputDir, File), Opts)
     || File <- InputFiles],

    ok.

list_dir_safe(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Files;
        Other ->
            io:format("Failed to list directory ~p~n Reason ~p~n", [Dir, Other]),
            []
    end.

render_file(Src, Dst, Params) ->
    {ok, #file_info{mode = Mode}} = file:read_file_info(Src),
    {ok, Bin} = file:read_file(Src),
    Out = bbmustache:render(Bin, Params, mustache_opts()),
    ok = filelib:ensure_dir(Dst),
    Result = file:write_file(Dst, Out),
    io:format("Result file written ~p: ~p~n", [Dst, Result]),
    ok = file:change_mode(Dst, Mode).

mustache_opts() ->
    [{key_type, atom}].

check_mim_dir(false) ->
    halt_with_error("MIM_DIR is not set");
check_mim_dir(MIM_DIR) ->
    io:format("MIM_DIR is set to ~p~n", [MIM_DIR]),
    ok.

halt_with_error(Reason) ->
    io:format("ERROR ~ts", [Reason]),
    halt(1).

import_libraries(MIM_DIR) ->
    Pattern = filename:join(MIM_DIR, "lib/*/ebin"),
    Paths = filelib:wildcard(Pattern),
    code:add_paths(Paths).

ensure_deps() ->
    ensure_dep(eini),
    ensure_dep(bbmustache),
    ok.

ensure_dep(Dep) ->
    case application:ensure_all_started(Dep) of
        {ok, _} ->
            ok;
        Other ->
            io:format("ensure_all_started for ~p returns ~p~n", [Dep, Other]),
            halt_with_error("ensure_dep_failed")
    end.

parse_template_config(TemplateConfigPath, TemplateConfigBin) ->
    case eini:parse(TemplateConfigBin) of
        {ok, Config} ->
            Config;
        Other ->
            io:format("Failed to parse ~p~n Reason ~p~n", [TemplateConfigPath, Other]),
            halt_with_error("parse_template_config_failed")
    end.
