-module(run_common_test).

-export([main/1, analyze/2]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).

%% DEBUG: compile time settings
-define(PRINT_ERRORS, false).
-define(PRINT_STATS, false).

%%
%% Entry
%%

-record(opts, {test,
               spec,
               cover,
               preset = all}).

%% Accepted options formatted as:
%% {opt_name, opt_index_in_opts_record, fun value_sanitizer/1}.
%% -spec value_sanitizer(string()) -> NewValue :: any().
opts() ->
    [{test,   #opts.test,   fun quick_or_full/1},
     {spec,   #opts.spec,   fun list_to_atom/1},
     {cover,  #opts.cover,  fun bool_or_module_list/1},
     {preset, #opts.preset, fun preset/1}].

%% Raw args are 'key=val' atoms.
%% Args are {key :: atom(), val :: string()} pairs.
%% "=" is an invalid character in option name or value.
main(RawArgs) ->
    Args = [raw_to_arg(Raw) || Raw <- RawArgs],
    Opts = args_to_opts(Args),
    try
        Results = run(Opts),
        %% Waiting for messages to be flushed
        timer:sleep(50),
        process_results(Results)
    catch Type:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        error_logger:error_msg("TEST CRASHED~n Error type: ~p~n Reason: ~p~n Stacktrace:~n~p~n",
                               [Type, Reason, Stacktrace]),
        %% Waiting for messages to be flushed
        timer:sleep(50),
        init:stop("Test failed")
    end.

run(#opts{test = quick, cover = Cover, spec = Spec}) ->
    do_run_quick_test(tests_to_run(Spec), Cover);
run(#opts{test = full, spec = Spec, preset = Preset, cover = Cover}) ->
    run_test(tests_to_run(Spec), case Preset of
                                     all -> all;
                                     undefined -> all;
                                     _   -> [Preset]
                                 end, Cover).

%%
%% Helpers
%%

args_to_opts(Args) ->
    {Args, Opts} = lists:foldl(fun set_opt/2, {Args, #opts{}}, opts()),
    Opts.

raw_to_arg(RawArg) ->
    ArgVal = atom_to_list(RawArg),
    [Arg, Val] = string:tokens(ArgVal, "="),
    {list_to_atom(Arg), Val}.

set_opt({Opt, Index, Sanitizer}, {Args, Opts}) ->
    Value = Sanitizer(proplists:get_value(Opt, Args)),
    {Args, setelement(Index, Opts, Value)}.

quick_or_full("quick") -> quick;
quick_or_full("full")  -> full.

preset(undefined) -> undefined;
preset(Preset) -> list_to_atom(Preset).

read_file(ConfigFile) when is_list(ConfigFile) ->
    {ok, CWD} = file:get_cwd(),
    filename:join([CWD, ConfigFile]),
    {ok, Props} = file:consult(ConfigFile),
    Props.

tests_to_run(TestSpec) ->
    TestSpecFile = atom_to_list(TestSpec),
    [
     {spec, TestSpecFile}
    ].

save_count(Test, Configs) ->
    Repeat = case proplists:get_value(repeat, Test) of
        undefined -> 1;
        Other     -> Other
    end,
    Times = case length(Configs) of
        0 -> 1;
        N -> N
    end,
    file:write_file("/tmp/ct_count", integer_to_list(Repeat*Times)).

run_test(Test, PresetsToRun, CoverOpts) ->
    prepare_cover(Test, CoverOpts),
    error_logger:info_msg("Presets to run ~p", [PresetsToRun]),
    {ConfigFile, Props} = get_ct_config(Test),
    case proplists:lookup(ejabberd_presets, Props) of
        {ejabberd_presets, Presets} ->
            Presets1 = case PresetsToRun of
                           all ->
                               Presets;
                           _ ->
                               error_logger:info_msg("Skip presets ~p",
                                                     [ preset_names(Presets) -- PresetsToRun ]),
                               lists:filter(fun({Preset,_}) ->
                                                    lists:member(Preset, PresetsToRun)
                                            end, Presets)
                       end,
            Length = length(Presets1),
            Names = preset_names(Presets),
            error_logger:info_msg("Starting test of ~p configurations: ~n~p~n",
                                  [Length, Names]),
            Zip = lists:zip(lists:seq(1, Length), Presets1),
            R = [ run_config_test(Preset, Test, N, Length) || {N, Preset} <- Zip ],
            save_count(Test, Presets1),
            analyze_coverage(Test, CoverOpts),
            R;
        _ ->
            error_logger:info_msg("Presets were not found in the config file ~ts",
                                  [ConfigFile]),
            R = do_run_quick_test(Test, CoverOpts),
            analyze_coverage(Test, CoverOpts),
            R
    end.

get_ct_config([{spec, Spec}]) ->
    Props = read_file(Spec),
    ConfigFile = case proplists:lookup(config, Props) of
        {config, [Config]} -> Config;
        _                  -> "test.config"
    end,
    {ok, ConfigProps} = file:consult(ConfigFile),
    {ConfigFile, ConfigProps}.

preset_names(Presets) ->
    [Preset||{Preset, _} <- Presets].

do_run_quick_test(Test, CoverOpts) ->
    prepare_cover(Test, CoverOpts),
    Result = ct:run_test(Test),
    case Result of
        {error, Reason} ->
            throw({ct_error, Reason});
        {Ok, Failed, {UserSkipped, AutoSkipped}} ->
            analyze_coverage(Test, CoverOpts),
            save_count(Test, []),
            [{ok, {Ok, Failed, UserSkipped, AutoSkipped}}]
    end.

run_config_test({Name, Variables}, Test, N, Tests) ->
    enable_preset(Name, Variables, Test, N, Tests),
    Result = ct:run_test([{label, Name} | Test]),
    case Result of
        {error, Reason} ->
            throw({ct_error, Reason});
        {Ok, Failed, {UserSkipped, AutoSkipped}} ->
            {ok, {Ok, Failed, UserSkipped, AutoSkipped}}
    end.

enable_preset(Name, PresetVars, Test, N, Tests) ->
    %% TODO: Do this with a multicall, otherwise it's not as fast as possible (not parallelized).
    %%       A multicall requires the function to be defined on the other side, though.
    Rs = [ enable_preset_on_node(host_node(H), PresetVars, host_vars(H))
           || H <- get_hosts(Test) ],
    [ok] = lists:usort(Rs),
    error_logger:info_msg("Configuration ~p of ~p: ~p started.~n",
                          [N, Tests, Name]).

backend(Node) ->
    rpc:call(Node, ejabberd_config, get_global_option, [sm_backend]).

enable_preset_on_node(Node, PresetVars, HostVars) ->
    {ok, Cwd} = call(Node, file, get_cwd, []),
    Cfg = filename:join([Cwd, "..", "..", "rel", "files", "ejabberd.cfg"]),
    Vars = filename:join([Cwd, "..", "..", "rel", "reltool_vars", HostVars]),
    CfgFile = filename:join([Cwd, "etc", "ejabberd.cfg"]),
    {ok, Template} = call(Node, file, read_file, [Cfg]),
    {ok, Default} = call(Node, file, consult, [Vars]),
    NewVars = lists:foldl(fun ({Var, Val}, Acc) ->
                              lists:keystore(Var, 1, Acc, {Var, Val})
                          end, Default, PresetVars),
    LTemplate = binary_to_list(Template),
    NewCfgFile = mustache:render(LTemplate, dict:from_list(NewVars)),
    ok = call(Node, file, write_file, [CfgFile, NewCfgFile]),
    call(Node, application, stop, [ejabberd]),
    call(Node, application, start, [ejabberd]),
    ok.

call(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            error_logger:error_msg("RPC call ~p:~p/~p to node ~p failed because ~p",
                                   [M, F, length(A), Node, Reason]), 
            {badrpc, Reason};
        Result ->
            Result
    end.

get_apps() ->
    case file:list_dir("../../apps/") of
        {ok, Filenames} -> lists:map(fun list_to_atom/1, Filenames);
        {error, _Reason} -> error("ejabberd parent project not found (expected apps in ../../apps)")
    end.


prepare_cover(Test, true) ->
    io:format("Preparing cover~n"),
    prepare(Test);
prepare_cover(_, _) ->
    ok.

analyze_coverage(Test, true) ->
    analyze(Test, true);
analyze_coverage(Test, ModuleList) when is_list(ModuleList) ->
    analyze(Test, ModuleList);
analyze_coverage(_, _) ->
    ok.

prepare(Test) ->
    Apps = get_apps(),
    Nodes = get_ejabberd_nodes(Test),
    io:format("cover: compiling modules for nodes ~p~n", [Nodes]),
    Compiled = multicall(Nodes, mongoose_cover_helper, start, [Apps],
                         cover_timeout()),
    io:format("cover: compiled ~p~n", [Compiled]).

analyze(Test, CoverOpts) ->
    io:format("Coverage analyzing~n"),
    Nodes = get_ejabberd_nodes(Test),
    multicall(Nodes, mongoose_cover_helper, analyze, [], cover_timeout()),
    Files = filelib:wildcard("/tmp/*.coverdata"),
    [cover:import(File) || File <- Files],
    cover:export("/tmp/mongoose_combined.coverdata"),
    case os:getenv("TRAVIS_JOB_ID") of
        false ->
            make_html(modules_to_analyze(CoverOpts));
        _ ->
            ok
    end.

make_html(Modules) ->
    {ok, Root} = file:get_cwd(),
    SortScript = Root ++ "/priv/sorttable.js",
    os:cmd("cp " ++ SortScript ++ " " ++ ?CT_REPORT),
    FilePath = case file:read_file(?CT_REPORT++"/index.html") of
        {ok, IndexFileData} ->
            R = re:replace(IndexFileData, "<a href=\"all_runs.html\">ALL RUNS</a>", "& <a href=\"cover.html\" style=\"margin-right:5px\">COVER</a>"),
            file:write_file(?CT_REPORT++"/index.html", R),
            ?CT_REPORT++"/cover.html";
        _ -> skip
    end,
    CoverageDir = filename:dirname(FilePath)++"/coverage",
    file:make_dir(CoverageDir),
    {ok, File} = file:open(FilePath, [write]),
    file:write(File, get_cover_header()),
    Fun = fun(Module, {CAcc, NCAcc}) ->
                  FileName = lists:flatten(io_lib:format("~s.COVER.html",[Module])),

                  case cover:analyse(Module, module) of
                      {ok, {Module, {C, NC}}} ->
                          file:write(File, row(atom_to_list(Module), C, NC, percent(C,NC),"coverage/"++FileName)),
                          FilePathC = filename:join([CoverageDir, FileName]),
                          catch cover:analyse_to_file(Module, FilePathC, [html]),
                          {CAcc + C, NCAcc + NC};
                      _ ->
                          {CAcc, NCAcc}
                  end
          end,
    {CSum, NCSum} = lists:foldl(Fun, {0, 0}, Modules),
    file:write(File, row("Summary", CSum, NCSum, percent(CSum, NCSum), "#")),
    file:close(File).

get_hosts(Test) ->
    {_File, Props} = get_ct_config(Test),
    {hosts, Hosts} = lists:keyfind(hosts, 1, Props),
    %% `mim` is our assumed cluster name - it has to be defined in test.config
    dict:fetch(mim, group_by(fun host_cluster/1, Hosts)).

get_ejabberd_nodes(Test) ->
    [ host_node(H) || H <- get_hosts(Test) ].

percent(0, _) -> 0;
percent(C, NC) when C /= 0; NC /= 0 -> round(C / (NC+C) * 100);
percent(_, _)                       -> 100.

row(Row, C, NC, Percent, Path) ->
    [
        "<tr>",
        "<td><a href='", Path, "'>", Row, "</a></td>",
        "<td>", integer_to_list(Percent), "%</td>",
        "<td>", integer_to_list(C), "</td>",
        "<td>", integer_to_list(NC), "</td>",
        "<td>", integer_to_list(C+NC), "</td>",
        "</tr>\n"
    ].

get_cover_header() ->
    "<html>\n<head></head>\n<body bgcolor=\"white\" text=\"black\" link=\"blue\" vlink=\"purple\" alink=\"red\">\n"
    "<head><script src='sorttable.js'></script></head>"
    "<h1>Coverage for application 'MongooseIM'</h1>\n"
    "<table class='sortable' border=3 cellpadding=5>\n"
    "<tr><th>Module</th><th>Covered (%)</th><th>Covered (Lines)</th><th>Not covered (Lines)</th><th>Total (Lines)</th></tr>".

bool_or_module_list("true") ->
    true;
bool_or_module_list(ModuleList) when is_list(ModuleList) ->
    [ list_to_atom(L) || L <- string:tokens("asd,qwe,zxc", ",") ];
bool_or_module_list(_) ->
    false.

modules_to_analyze(true) ->
    cover:imported_modules();
modules_to_analyze(ModuleList) when is_list(ModuleList) ->
    ModuleList.

add({X1, X2, X3, X4},
    {Y1, Y2, Y3, Y4}) ->
    {X1 + Y1,
     X2 + Y2,
     X3 + Y3,
     X4 + Y4}.

process_results(CTResults) ->
    Ok = 0,
    Failed = 0,
    UserSkipped = 0,
    AutoSkipped = 0,
    Errors = [],
    process_results(CTResults, {{Ok, Failed, UserSkipped, AutoSkipped}, Errors}).

process_results([], {StatsAcc, Errors}) ->
    print_errors(Errors),
    print_stats(StatsAcc),
    init:stop(exit_code(StatsAcc));
process_results([ {ok, RunStats} | T ], {StatsAcc, Errors}) ->
    process_results(T, {add(RunStats, StatsAcc), Errors});
process_results([ Error | T ], {StatsAcc, Errors}) ->
    process_results(T, {StatsAcc, [Error | Errors]}).

print_errors(Errors) ->
    ?PRINT_ERRORS andalso [ print(standard_error, "~p~n", [E]) || E <- Errors ].

print_stats(Stats) ->
    ?PRINT_STATS andalso do_print_stats(Stats).

do_print_stats({Ok, Failed, _UserSkipped, AutoSkipped}) when Ok == 0;
                                                          Failed > 0;
                                                          AutoSkipped > 0 ->
    print(standard_error, "Tests:~n", []),
    Ok == 0 andalso print(standard_error,         "  ok          : ~b~n", [Ok]),
    Failed > 0 andalso print(standard_error,      "  failed      : ~b~n", [Failed]),
    AutoSkipped > 0 andalso print(standard_error, "  auto-skipped: ~b~n", [AutoSkipped]).

%% Fail if there are failed test cases, auto skipped cases,
%% or the number of passed tests is 0 (which is also strange - a misconfiguration?).
%% StatsAcc is similar (Skipped are not a tuple) to the success result from ct:run_test/1:
%%
%%     {Ok, Failed, UserSkipped, AutoSkipped}
%%
exit_code({Ok, Failed, _UserSkipped, AutoSkipped})
  when Ok == 0; Failed > 0; AutoSkipped > 0 ->
    1;
exit_code({_, _, _, _}) ->
    0.

print(Handle, Fmt, Args) ->
    io:format(Handle, Fmt, Args).

multicall(Nodes, M, F, A, Timeout) ->
    {Rs, [] = _BadNodes} = rpc:multicall(Nodes, M, F, A, Timeout),
    Rs.

cover_timeout() ->
    timer:seconds(60).

%% Source: https://gist.github.com/jbpotonnier/1310406
group_by(F, L) ->
    lists:foldr(fun ({K, V}, D) -> dict:append(K, V, D) end,
                dict:new(),
                [ {F(X), X} || X <- L ]).

host_cluster(Host) -> host_param(cluster, Host).
host_node(Host)    -> host_param(node, Host).
host_vars(Host)    -> host_param(vars, Host).

host_param(Name, {_, Params}) ->
    {Name, Param} = lists:keyfind(Name, 1, Params),
    Param.
