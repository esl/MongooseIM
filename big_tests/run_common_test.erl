%% During dev you would use something similar to:
%% TEST_HOSTS="mim" ./tools/test.sh -c false -s false -p pgsql_cets
%%
%% If you also want to start just mim1 node use:
%% DEV_NODES="mim1" TEST_HOSTS="mim" ./tools/test.sh -c false -s false -p pgsql_cets
%%
%% TEST_HOSTS variable contains host names from hosts in big_tests/test.config.
%% DEV_NODES variable contains release names from profiles in rebar.config.
%% Release names are also used to name directories in the _build directory.
%%
%% Valid TEST_HOSTS are mim, mim2, mim3, fed, reg.
%% Valid DEV_NODES are mim1, mim2, mim3, fed1, reg1.
%%
%% Example with two nodes:
%% DEV_NODES="mim1 mim2" TEST_HOSTS="mim mim2" ./tools/test.sh -c false -s false -p pgsql_cets
%%
%% Environment variable PRESET_ENABLED is true by default.
%% PRESET_ENABLED=false disables preset application and forces to run
%% one preset.
-module(run_common_test).

-export([init/0, main/1, analyze/2]).

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
               preset = all,
               hooks}).

%% Accepted options formatted as:
%% {opt_name, opt_index_in_opts_record, fun value_sanitizer/1}.
%% -spec value_sanitizer(string()) -> NewValue :: any().
opts() ->
    [{test,   #opts.test,   fun quick_or_full/1},
     {spec,   #opts.spec,   fun list_to_atom/1},
     {cover,  #opts.cover,  fun bool_or_module_list/1},
     {preset, #opts.preset, fun preset/1},
     {hooks,  #opts.hooks,  fun module_list/1}].

%% Raw args are 'key=val' atoms.
%% Args are {key :: atom(), val :: string()} pairs.
%% "=" is an invalid character in option name or value.
main(RawArgs) ->
    init(),
    Args = [raw_to_arg(Raw) || Raw <- RawArgs],
    Opts = apply_preset_enabled(args_to_opts(Args)),
    try
        CTRunDirsBeforeRun = ct_run_dirs(),
        Results = run(Opts),
        %% Waiting for messages to be flushed
        timer:sleep(50),
        CTRunDirsAfterRun = ct_run_dirs(),
        ExitStatusByGroups = exit_status_by_groups(CTRunDirsBeforeRun, CTRunDirsAfterRun, Results),
        ExitStatusByTestCases = process_results(Results),
        case ExitStatusByGroups of
            undefined ->
                io:format("Exiting by test cases summary: ~p~n", [ExitStatusByTestCases]),
                init:stop(ExitStatusByTestCases);
            _ when is_integer(ExitStatusByGroups) ->
                %% FIXME: This is incorrect assumption, it ignores results of all individual
                %% tests cases and groups w/o 'repeat_until_all_ok' flag. So we can return
                %% false positive result here. It's not too critical, because test.sh script
                %% recalculates return code using summarise-ct-results utility. However, it
                %% may result in squashing ct output, since execution of run_common_test.erl
                %% is wrapped using silent_exec.sh tool.
                io:format("Exiting by groups summary: ~p~n",  [ExitStatusByGroups]),
                init:stop(ExitStatusByGroups)
        end
    catch Type:Reason:StackTrace ->
        io:format("TEST CRASHED~n Error type: ~p~n Reason: ~p~n Stacktrace:~n~p~n",
                               [Type, Reason, StackTrace]),
        error_logger:error_msg("TEST CRASHED~n Error type: ~p~n Reason: ~p~n Stacktrace:~n~p~n",
                               [Type, Reason, StackTrace]),
        %% Waiting for messages to be flushed
        timer:sleep(5000),
        init:stop("run_common_test:main/1 crashed")
    end.

init() ->
    {ok, _} = application:ensure_all_started(jid).

run(#opts{test = quick, cover = Cover, spec = Spec}) ->
    do_run_quick_test(tests_to_run(Spec), Cover);
run(#opts{test = full, spec = Spec, preset = Preset, cover = Cover, hooks = HookModules}) ->
    run_test(tests_to_run(Spec) ++ ct_hooks(HookModules),
             case Preset of
                 all -> all;
                 undefined -> all;
                 _ when is_list(Preset) -> Preset;
                 _   -> [Preset]
             end, Cover).

apply_preset_enabled(#opts{} = Opts) ->
    case os:getenv("PRESET_ENABLED") of
        "false" ->
            io:format("PRESET_ENABLED is set to false, enabling quick mode~n"),
            Opts#opts{test = quick};
        _ ->
            Opts
    end.


%%
%% Helpers
%%

repo_dir() ->
    case os:getenv("REPO_DIR") of
        false ->
            init:stop("Environment variable REPO_DIR is undefined");
        Value ->
            Value
    end.

args_to_opts(Args) ->
    {Args, Opts} = lists:foldl(fun set_opt/2, {Args, #opts{}}, opts()),
    Opts.

raw_to_arg(RawArg) ->
    ArgVal = atom_to_list(RawArg),
    case string:tokens(ArgVal, "=") of
        [Arg, Val] ->
            {list_to_atom(Arg), Val};
        [Arg] ->
            {list_to_atom(Arg), ""}
    end.

set_opt({Opt, Index, Sanitizer}, {Args, Opts}) ->
    Value = Sanitizer(proplists:get_value(Opt, Args)),
    {Args, setelement(Index, Opts, Value)}.

quick_or_full("quick") -> quick;
quick_or_full("full")  -> full.

preset(undefined) -> undefined;
preset(PresetList) ->
    [list_to_atom(Preset) || Preset <- string:tokens(PresetList, " ")].

read_file(ConfigFile) when is_list(ConfigFile) ->
    {ok, CWD} = file:get_cwd(),
    filename:join([CWD, ConfigFile]),
    {ok, Props} = handle_file_error(ConfigFile, file:consult(ConfigFile)),
    Props.

tests_to_run(TestSpec) ->
    TestSpecFile = atom_to_list(TestSpec),
    [
     {spec, TestSpecFile}
    ] ++ ct_opts().

ct_hooks([]) ->
    [];
ct_hooks(HookModules) ->
    [{ct_hooks, HookModules}].

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
    {ConfigFiles, Props} = get_ct_config(Test),
    prepare_cover(Props, CoverOpts),
    error_logger:info_msg("Presets to run ~p", [PresetsToRun]),
    case get_presets(Props) of
        {ok, Presets} ->
            Presets1 = case PresetsToRun of
                           all ->
                               Presets;
                           _ ->
                               assert_all_presets_present(PresetsToRun, Presets),
                               error_logger:info_msg("Skip presets ~p",
                                                     [ preset_names(Presets) -- PresetsToRun ]),
                               lists:filter(fun({Preset,_}) ->
                                                    lists:member(Preset, PresetsToRun)
                                            end, Presets)
                       end,
            Length = length(Presets1),
            Names = preset_names(Presets1),
            error_logger:info_msg("Starting test of ~p configurations: ~n~p~n",
                                  [Length, Names]),
            Zip = lists:zip(lists:seq(1, Length), Presets1),
            R = [ run_config_test(Props, Preset, Test, N, Length) || {N, Preset} <- Zip ],
            save_count(Test, Presets1),
            analyze_coverage(Props, CoverOpts),
            R;
        {error, not_found} ->
            error_logger:info_msg("Presets were not found in the config files ~ts",
                                  [ConfigFiles]),
            R = do_run_quick_test(Test, CoverOpts),
            analyze_coverage(Props, CoverOpts),
            R
    end.

get_presets(Props) ->
    case proplists:lookup(presets, Props) of
        {presets, Presets} ->
            case proplists:lookup(toml, Presets) of
                {toml, Preset} ->
                    {ok, Preset};
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

get_ct_config(Opts) ->
    Spec = proplists:get_value(spec, Opts),
    Props = read_file(Spec),
    ConfigFiles = proplists:get_value(config, Props, ["test.config"]),
    % Apply the files in reverse, like ct will do when running the tests
    ConfigProps = merge_vars([read_file(File) || File <- lists:reverse(ConfigFiles)]),
    {ConfigFiles, ConfigProps}.

preset_names(Presets) ->
    [Preset||{Preset, _} <- Presets].

do_run_quick_test(Test, CoverOpts) ->
    prepare_cover(Test, CoverOpts),
    load_test_modules(Test),
    Result = ct:run_test(Test),
    case Result of
        {error, Reason} ->
            throw({ct_error, Reason});
        {Ok, Failed, {UserSkipped, AutoSkipped}} ->
            analyze_coverage(Test, CoverOpts),
            save_count(Test, []),
            [{ok, {Ok, Failed, UserSkipped, AutoSkipped}}]
    end.

run_config_test(Props, {Name, Variables}, Test, N, Tests) ->
    enable_preset(Props, Name, Variables, N, Tests),
    load_test_modules(Test),
    Result = ct:run_test([{label, Name} | Test]),
    case Result of
        {error, Reason} ->
            throw({ct_error, Reason});
        {Ok, Failed, {UserSkipped, AutoSkipped}} ->
            {ok, {Ok, Failed, UserSkipped, AutoSkipped}}
    end.

enable_preset(Props, Name, PresetVars, N, Tests) ->
    %% TODO: Do this with a multicall, otherwise it's not as fast as possible (not parallelized).
    %%       A multicall requires the function to be defined on the other side, though.
    Rs = [ maybe_enable_preset_on_node(host_node(H), PresetVars,
                                       host_vars(H), host_name(H))
           || H <- get_hosts_to_enable_preset(Props) ],
    [ok] = lists:usort(Rs),
    error_logger:info_msg("Configuration ~p of ~p: ~p started.~n",
                          [N, Tests, Name]).

%% Specify just some nodes to run the tests on:
%% TEST_HOSTS="mim" ./tools/test.sh -p pgsql_cets
maybe_enable_preset_on_node(Node, PresetVars, HostVars, HostName) ->
    case is_test_host_enabled(HostName) of
        true ->
            enable_preset_on_node(Node, PresetVars, HostVars);
        false ->
            error_logger:info_msg("Skip enable_preset_on_node for node=~p host=~p",
                                  [Node, HostName]),
            ok
    end.

%% Check, that node is listed in TEST_HOSTS list (if TEST_HOSTS envvar is set).
is_test_host_enabled(HostName) ->
    case os:getenv("TEST_HOSTS") of
        false ->
            true; %% By default all hosts are enabled
        EnvValue -> %% EnvValue examples are "mim" or "mim mim2"
            BinHosts = binary:split(iolist_to_binary(EnvValue), <<" ">>, [global]),
            lists:member(atom_to_binary(HostName, utf8), BinHosts)
    end.

enable_preset_on_node(Node, PresetVars, HostVarsFilePrefix) ->
    {ok, Cwd} = call(Node, file, get_cwd, []),
    TemplatePath = filename:join([repo_dir(), "rel", "files", "mongooseim.toml"]),
    NodeVarsPath = filename:join([repo_dir(), "rel", HostVarsFilePrefix ++ ".vars-toml.config"]),

    {ok, Template} = handle_file_error(TemplatePath, file:read_file(TemplatePath)),
    NodeVars = read_vars(NodeVarsPath),

    TemplatedConfig = template_config(Template, NodeVars ++ PresetVars),
    CfgPath = filename:join([Cwd, "etc", "mongooseim.toml"]),
    ok = call(Node, file, write_file, [CfgPath, TemplatedConfig]),
    call(Node, application, stop, [mongooseim]),
    call(Node, application, start, [mongooseim]),
    ok.

template_config(Template, RawVars) ->
    MergedVars = ensure_binary_strings(maps:from_list(RawVars)),
    %% Render twice to replace variables in variables
    Tmp = bbmustache:render(Template, MergedVars, [{key_type, atom}]),
    bbmustache:render(Tmp, MergedVars, [{key_type, atom}]).

merge_vars([Vars1, Vars2|Rest]) ->
    Vars = lists:foldl(fun ({Var, Val}, Acc) ->
                               lists:keystore(Var, 1, Acc, {Var, Val})
                       end, Vars1, Vars2),
    merge_vars([Vars|Rest]);
merge_vars([Vars]) -> Vars.

read_vars(File) ->
    {ok, Terms} = handle_file_error(File, file:consult(File)),
    lists:flatmap(fun({Key, Val}) ->
                          [{Key, Val}];
                     (IncludedFile) when is_list(IncludedFile) ->
                          Path = filename:join(filename:dirname(File), IncludedFile),
                          read_vars(Path)
                  end, Terms).

%% bbmustache tries to iterate over lists, so we need to make them binaries
ensure_binary_strings(Vars) ->
    maps:map(fun(dbs, V) -> V;
                (_K, []) -> <<"\n">>; % empty binary is considered falsey in conditions
                (_K, V) when is_list(V) -> list_to_binary(V);
                (_K, V) -> V
              end, Vars).

call(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            error_logger:error_msg("RPC call ~p:~p/~p to node ~p failed because ~p",
                                   [M, F, length(A), Node, Reason]),
            {badrpc, Reason};
        Result ->
            Result
    end.

prepare_cover(Props, true) ->
    io:format("Preparing cover~n"),
    prepare(Props);
prepare_cover(_, _) ->
    ok.

analyze_coverage(Props, true) ->
    analyze(Props, true);
analyze_coverage(Props, ModuleList) when is_list(ModuleList) ->
    analyze(Props, ModuleList);
analyze_coverage(_, _) ->
    ok.

prepare(Props) ->
    Nodes = get_mongoose_nodes(Props),
    maybe_compile_cover(Nodes),
    block_nodes(Props).

maybe_compile_cover([]) ->
    io:format("cover: skip cover compilation~n", []),
    ok;
maybe_compile_cover([CoverNode|_] = Nodes) ->
    io:format("cover: compiling modules for nodes ~p~n", [Nodes]),
    import_code_paths(hd(Nodes)),

    cover_start(CoverNode, Nodes),
    Dir = call(hd(Nodes), code, lib_dir, [mongooseim, ebin]),

    %% Time is in microseconds
    {Time, Compiled} = timer:tc(fun() ->
                            Results = cover_compile_dir(CoverNode, Dir),
                            Ok = [X || X = {ok, _} <- Results],
                            NotOk = Results -- Ok,
                            #{ok => length(Ok), failed => NotOk}
                        end),
    github_actions_fold("cover compiled output", fun() ->
            io:format("cover: compiled ~p~n", [Compiled])
        end),
    report_progress("~nCover compilation took ~ts~n", [microseconds_to_string(Time)]),
    ok.

analyze(Props, CoverOpts) ->
    unblock_nodes(Props),
    io:format("Coverage analyzing~n"),
    Nodes = get_mongoose_nodes(Props),
    analyze(Props, CoverOpts, Nodes).

analyze(_Props, _CoverOpts, []) ->
    ok;
analyze(_Props, CoverOpts, [CoverNode|_] = Nodes) ->
    %% Import small tests cover
    Files = filelib:wildcard(repo_dir() ++ "/_build/**/cover/*.coverdata"),
    io:format("Files: ~p", [Files]),
    report_time("Import cover data into run_common_test node", fun() ->
            [cover_import(CoverNode, File) || File <- Files]
        end),
    report_time("Export merged cover data", fun() ->
                        cover_export(CoverNode, "/tmp/mongoose_combined.coverdata")
		end),
    case {os:getenv("GITHUB_RUN_ID"), os:getenv("CIRCLECI")} of
        {false, false} ->
            Mods = modules_to_analyze(CoverNode, CoverOpts),
            Txt = "Export HTML report for " ++ integer_to_list(length(Mods)) ++ " modules",
            report_time(Txt, fun() -> make_html(CoverNode, Mods) end);
        _ ->
            ok
    end,
    case os:getenv("KEEP_COVER_RUNNING") of
        "1" ->
            io:format("Skip stopping cover~n"),
            ok;
        _ ->
            report_time("Stopping cover on MongooseIM nodes", fun() ->
                        cover_stop(CoverNode, Nodes)
                    end)
    end.

make_html(CoverNode, Modules) ->
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
    Fun = fun(Module, {CAcc, NCAcc, Skipped, Failed, OK}) ->
                  FileName = lists:flatten(io_lib:format("~s.COVER.html",[Module])),

                  %% We assume that import_code_paths/1 was called earlier
                  case cover_analyse(CoverNode, Module) of
                      {ok, {Module, {C, NC}}} ->
                          FilePathC = filename:join([Root, CoverageDir, FileName]),
                          case cover_analyse_to_html_file(CoverNode, Module, FilePathC) of
                              {ok, _} ->
                                  file:write(File, row(atom_to_list(Module), C, NC, percent(C,NC),"coverage/"++FileName)),
                                  {CAcc + C, NCAcc + NC, Skipped, Failed, OK + 1};
                              Other ->
                                  %% Do not report link, if the destination file is not written
                                  file:write(File, row_no_link(atom_to_list(Module), C, NC, percent(C,NC))),
                                  error_logger:error_msg("issue=cover_analyse_to_html_file_failed module=~p reason=~p",
                                                         [Module, Other]),
                                  {CAcc + C, NCAcc + NC, Skipped, Failed + 1, OK}
                          end;
                      Reason ->
                          error_logger:error_msg("issue=cover_analyse_failed module=~p reason=~p",
                                                 [Module, Reason]),
                          {CAcc, NCAcc, Skipped + 1, Failed, OK}
                  end
          end,
    {CSum, NCSum, Skipped2, Failed2, OK2} = lists:foldl(Fun, {0, 0, 0, 0, 0}, Modules),
    file:write(File, row("Summary", CSum, NCSum, percent(CSum, NCSum), "#")),
    file:close(File),
    report_progress("make_html: ok - ~p, skipped - ~p, failed - ~p~n", [OK2, Skipped2, Failed2]),
    ok.

get_hosts_to_enable_preset(Props) ->
    [Host || Host <- get_all_hosts(Props), should_enable_preset(host_cluster(Host))].

should_enable_preset(mim) -> true;
should_enable_preset(reg) -> true;
should_enable_preset(_) -> false.

get_all_hosts(Props) ->
    {hosts, Hosts} = lists:keyfind(hosts, 1, Props),
    Hosts.

get_mongoose_nodes(Props) ->
    [ host_node(H) || H <- get_all_hosts(Props), is_test_host_enabled(host_name(H)) ].

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

row_no_link(Row, C, NC, Percent) ->
    [
        "<tr>",
        "<td>", Row, "</td>",
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
bool_or_module_list("false") ->
    false;
bool_or_module_list(undefined) ->
    false;
bool_or_module_list(ModuleList) when is_list(ModuleList) ->
    module_list(ModuleList).

module_list(undefined) ->
    [];
module_list(ModuleList) ->
    [ list_to_atom(L) || L <- string:tokens(ModuleList, ", ") ].

modules_to_analyze(CoverNode, true) ->
    lists:usort(cover_all_modules(CoverNode));
modules_to_analyze(_CoverNode, ModuleList) when is_list(ModuleList) ->
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
    write_stats_into_vars_file(StatsAcc),
    print_errors(Errors),
    print_stats(StatsAcc),
    exit_code(StatsAcc);
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

write_stats_into_vars_file(Stats) ->
    file:write_file("/tmp/ct_stats_vars", [format_stats_as_vars(Stats)]).

format_stats_as_vars({Ok, Failed, UserSkipped, AutoSkipped}) ->
    io_lib:format("CT_COUNTER_OK=~p~n"
                  "CT_COUNTER_FAILED=~p~n"
                  "CT_COUNTER_USER_SKIPPED=~p~n"
                  "CT_COUNTER_AUTO_SKIPPED=~p~n",
                  [Ok, Failed, UserSkipped, AutoSkipped]).

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

host_cluster(Host) -> host_param(cluster, Host).
host_node(Host)    -> host_param(node, Host).
host_vars(Host)    -> host_param(vars, Host).
host_name({HostName,_}) -> HostName.

host_param(Name, {_, Params}) ->
    {Name, Param} = lists:keyfind(Name, 1, Params),
    Param.

host_param(Name, {_, Params}, Default) ->
    proplists:get_value(Name, Params, Default).

report_time(Description, Fun) ->
	report_progress("~nExecuting ~ts~n", [Description]),
	Start = os:timestamp(),
    try
        Fun()
    after
        Microseconds = timer:now_diff(os:timestamp(), Start),
        Time = microseconds_to_string(Microseconds),
		report_progress("~ts took ~ts~n", [Description, Time])
	end.

microseconds_to_string(Microseconds) ->
	Milliseconds = Microseconds div 1000,
    SecondsFloat = Milliseconds / 1000,
    io_lib:format("~.3f seconds", [SecondsFloat]).

%% Writes onto GitHub actions console directly
report_progress(Format, Args) ->
    Message = io_lib:format(Format, Args),
    file:write_file("/tmp/progress", Message, [append]).

github_actions_fold(Description, Fun) ->
    case os:getenv("GITHUB_RUN_ID") of
        false ->
            Fun();
        _ ->
            io:format("github_actions_fold:start:~ts~n", [Description]),
            Result = Fun(),
            io:format("github_actions_fold:end:~ts~n", [Description]),
            Result
    end.

%% Import code paths from a running node.
%% It allows cover:analyse/2 to find source file by calling
%% Module:module_info(compiled).
import_code_paths(FromNode) when is_atom(FromNode) ->
    Paths = call(FromNode, code, get_path, []),
    code:add_paths(Paths).

%% Gets result of file operation and prints filename, if we have any issues.
handle_file_error(FileName, {error, Reason}) ->
    error_logger:error_msg("issue=file_operation_error filename=~p reason=~p",
                           [FileName, Reason]),
    {error, Reason};
handle_file_error(_FileName, Other) ->
    Other.

%% ------------------------------------------------------------------

ct_run_dirs() ->
    filelib:wildcard("ct_report/ct_run*").

exit_status_by_groups(CTRunDirsBeforeRun, CTRunDirsAfterRun, Results) ->
    NewCTRunDirs = CTRunDirsAfterRun -- CTRunDirsBeforeRun,
    case NewCTRunDirs of
        [] ->
            io:format("WARNING: ct_run directory has not been created~nResults ~p~n",  [Results]),
            undefined;
        [_] ->
            anaylyze_groups_runs(hd(NewCTRunDirs))
    end.

anaylyze_groups_runs(LatestCTRun) ->
    case file:consult(LatestCTRun ++ "/all_groups.summary") of
        {ok, Terms} ->
            proplists:get_value(total_failed, Terms, undefined);
      {error, Error} ->
            error_logger:error_msg("Error reading all_groups.summary: ~p~n", [Error]),
            undefined
    end.


ct_opts() ->
    %% Tell Common Tests that it should not compile any more files
    %% (they are compiled by rebar)
    case os:getenv("SKIP_AUTO_COMPILE") of
        "true" ->
            [{auto_compile, false}];
        _ ->
            []
    end.


%% Common Tests does not try to use code autoloading feature
%% for loading suites.
%% So, if we want to use SKIP_AUTO_COMPILE, we need to put tests, where Common tests
%% can find it. Or we can load modules instead (what we do here).
load_test_modules(Opts) ->
    Spec = proplists:get_value(spec, Opts),
    %% Read test spec properties
    Props = read_file(Spec),
    Modules = lists:usort(test_modules(Props)),
    [try_load_module(M) || M <- Modules].

test_modules([H|T]) when is_tuple(H) ->
    test_modules_list(tuple_to_list(H)) ++ test_modules(T);
test_modules([_|T]) ->
    test_modules(T);
test_modules([]) ->
    [].

test_modules_list([suites, _, Suite|_]) ->
    [Suite];
test_modules_list([groups, _, Suite|_]) ->
    [Suite];
test_modules_list([cases, _, Suite|_]) ->
    [Suite];
test_modules_list(_) ->
    [].

try_load_module(Module) ->
    case code:is_loaded(Module) of
        true -> already_loaded;
        _ -> code:load_file(Module)
    end.

assert_all_presets_present(PresetsToCheck, PresetConfs) ->
    lists:foreach(fun(Preset) ->
                    assert_preset_present(Preset, PresetConfs)
                  end, PresetsToCheck).

assert_preset_present(small_tests, _PresetConfs) ->
    ok;
assert_preset_present(Preset, PresetConfs) ->
    case lists:keymember(Preset, 1, PresetConfs) of
        true -> ok;
        false ->
            error_logger:error_msg("Preset not found ~p~n", [Preset]),
            error({preset_not_found, Preset})
    end.

assert_list(X) when is_list(X) -> X.

%% We use mim1 as a main node.
%% Only the main node supports meck
%% (other nodes should not use meck for the cover compiled modules).
cover_start(CoverNode, Nodes) ->
    {ok, _} = cover_call(CoverNode, start, [Nodes]),
    CoverNode = cover_call(CoverNode, get_main_node, []),
    ok.

cover_stop(CoverNode, Nodes) ->
    cover_call(CoverNode, stop, [Nodes]).

cover_all_modules(CoverNode) ->
    List1 = assert_list(cover_call(CoverNode, imported_modules, [])),
    List2 = assert_list(cover_call(CoverNode, modules, [])),
    List1 ++ List2.

cover_analyse_to_html_file(CoverNode, Module, FilePathC) ->
    catch cover_call(CoverNode, analyse_to_file, [Module, FilePathC, [html]]).

cover_analyse(CoverNode, Module) ->
    cover_call(CoverNode, analyse, [Module, module]).

cover_export(CoverNode, ToFile) ->
    cover_call(CoverNode, export, [ToFile]).

cover_import(CoverNode, FromFile) ->
    cover_call(CoverNode, import, [FromFile]).

cover_compile_dir(CoverNode, Dir) ->
    cover_call(CoverNode, compile_beam_directory, [Dir]).

cover_call(CoverNode, Fun, Args) ->
    rpc:call(CoverNode, cover, Fun, Args).

block_nodes(Props) ->
   [block_node(Node, BlockNode, Props) || {Node, BlockNode} <- block_nodes_specs(Props)],
   ok.

unblock_nodes(Props) ->
   [unblock_node(Node, BlockNode, Props) || {Node, BlockNode} <- block_nodes_specs(Props)],
   ok.

%% Reads `blocks_hosts' parameter for the host from `test.config'.
%% Returns a list of blocks to do like `[{mim, fed}]'.
block_nodes_specs(Props) ->
    EnabledHosts = [ H || H <- get_all_hosts(Props), is_test_host_enabled(host_name(H)) ],
    [{host_name(H), BlockName}
     || H <- EnabledHosts, BlockName <- host_param(blocks_hosts, H, []),
        is_test_host_enabled(BlockName)].

host_name_to_node(Name, Props) ->
    Hosts = get_all_hosts(Props),
    Host = proplists:get_value(Name, Hosts, []),
    case proplists:get_value(node, Host) of
        undefined ->
            error({host_name_to_node_failed, Name, Props});
        Node ->
            Node
    end.

%% Do not allow node Name to talk to node BlockName
block_node(Name, BlockName, Props) ->
    Node = host_name_to_node(Name, Props),
    BlockNode = host_name_to_node(BlockName, Props),
    rpc_call(Node, erlang, set_cookie, [BlockNode, make_bad_cookie(Name, BlockNode)]),
    rpc_call(Node, erlang, disconnect_node, [BlockNode]),
    Cond = fun() -> lists:member(BlockNode, rpc_call(Node, erlang, nodes, [])) end,
    wait_helper:wait_until(Cond, false).

unblock_node(Name, BlockName, Props) ->
    Node = host_name_to_node(Name, Props),
    BlockNode = host_name_to_node(BlockName, Props),
    DefCookie = rpc_call(Node, erlang, get_cookie, []),
    rpc_call(Node, erlang, set_cookie, [BlockNode, DefCookie]).

make_bad_cookie(Name, BlockName) ->
    list_to_atom(atom_to_list(Name) ++ "_blocks_" ++ atom_to_list(BlockName)).

rpc_call(Node, M, F, Args) ->
    case rpc:call(Node, M, F, Args) of
        {badrpc, Reason} ->
            error({rpc_call_failed, Reason, Node, {M, F, Args}});
        Res ->
            Res
    end.
