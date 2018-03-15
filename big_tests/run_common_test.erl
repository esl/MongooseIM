%% During dev you would use something similar to:
%% DEV_NODES="mim1" ./tools/travis-test.sh -e false -c false -s false -p odbc_mssql_mnesia
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
        io:format("TEST CRASHED~n Error type: ~p~n Reason: ~p~n Stacktrace:~n~p~n",
                               [Type, Reason, Stacktrace]),
        error_logger:error_msg("TEST CRASHED~n Error type: ~p~n Reason: ~p~n Stacktrace:~n~p~n",
                               [Type, Reason, Stacktrace]),
        %% Waiting for messages to be flushed
        timer:sleep(5000),
        init:stop("run_common_test:main/1 crashed")
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
    {ok, Props} = handle_file_error(ConfigFile, file:consult(ConfigFile)),
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
    {ok, ConfigProps} = handle_file_error(ConfigFile, file:consult(ConfigFile)),
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
    Rs = [ maybe_enable_preset_on_node(host_node(H), PresetVars, host_vars(H), rel_name(H))
           || H <- get_hosts(Test) ],
    [ok] = lists:usort(Rs),
    error_logger:info_msg("Configuration ~p of ~p: ~p started.~n",
                          [N, Tests, Name]).

backend(Node) ->
    rpc:call(Node, ejabberd_config, get_global_option, [sm_backend]).

%% Specify just some nodes to run the tests on:
%% DEV_NODES="mim1" ./tools/travis-test.sh -p odbc_mssql_mnesia
maybe_enable_preset_on_node(Node, PresetVars, HostVars, RelName) ->
    case is_node_enabled(RelName) of
        true ->
            enable_preset_on_node(Node, PresetVars, HostVars);
        false ->
            error_logger:info_msg("Skip enable_preset_on_node ~p", [Node]),
            ok
    end.

%% Check, that node is listed in DEV_NODES list (if DEV_NODES envvar is set).
is_node_enabled(RelName) ->
    case os:getenv("DEV_NODES") of
        false ->
            true;
        EnvValue ->
            DevNodes = binary:split(iolist_to_binary(EnvValue), <<" ">>, [global]),
            lists:member(atom_to_binary(RelName, utf8), DevNodes)
    end.

enable_preset_on_node(Node, PresetVars, HostVars) ->
    {ok, Cwd} = call(Node, file, get_cwd, []),
    Cfg = filename:join([repo_dir(), "rel", "files", "ejabberd.cfg"]),
    Vars = filename:join([repo_dir(), "rel", HostVars]),
    CfgFile = filename:join([Cwd, "etc", "ejabberd.cfg"]),
    {ok, Template} = handle_file_error(Cfg, file:read_file(Cfg)),
    {ok, Default} = handle_file_error(Vars, file:consult(Vars)),
    NewVars = lists:foldl(fun ({Var, Val}, Acc) ->
                              lists:keystore(Var, 1, Acc, {Var, Val})
                          end, Default, PresetVars),
    NewCfgFile = bbmustache:render(Template, NewVars, [{key_type, atom}]),
    ok = call(Node, file, write_file, [CfgFile, NewCfgFile]),
    call(Node, application, stop, [mongooseim]),
    call(Node, application, start, [mongooseim]),
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
    [mongooseim].

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
    import_code_paths(hd(Nodes)),
    %% Time is in microseconds
    {Time, Compiled} = timer:tc(fun() ->
                    multicall(Nodes, mongoose_cover_helper, start, [Apps],
                              cover_timeout())
                        end),
    travis_fold("cover compiled output", fun() ->
            io:format("cover: compiled ~p~n", [Compiled])
        end),
    report_progress("~nCover compilation took ~ts~n", [microseconds_to_string(Time)]),
    ok.

analyze(Test, CoverOpts) ->
    io:format("Coverage analyzing~n"),
    Nodes = get_ejabberd_nodes(Test),
    report_time("Export cover data from MongooseIM nodes", fun() ->
            multicall(Nodes, mongoose_cover_helper, analyze, [], cover_timeout())
        end),
    Files = filelib:wildcard(repo_dir() ++ "/_build/**/cover/*.coverdata"),
    io:format("Files: ~p", [Files]),
    report_time("Import cover data into run_common_test node", fun() ->
            [cover:import(File) || File <- Files]
        end),
    report_time("Export merged cover data", fun() ->
			cover:export("/tmp/mongoose_combined.coverdata")
		end),
    report_time("Export merged cover data in codecov.json format", fun() ->
            export_codecov_json()
        end),
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

                  %% We assume that import_code_paths/1 was called earlier
                  case cover:analyse(Module, module) of
                      {ok, {Module, {C, NC}}} ->
                          file:write(File, row(atom_to_list(Module), C, NC, percent(C,NC),"coverage/"++FileName)),
                          FilePathC = filename:join([CoverageDir, FileName]),
                          catch cover:analyse_to_file(Module, FilePathC, [html]),
                          {CAcc + C, NCAcc + NC};
                      Reason ->
                          error_logger:error_msg("issue=cover_analyse_failed module=~p reason=~p",
                                                 [Module, Reason]),
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
    timer:minutes(3).

%% Source: https://gist.github.com/jbpotonnier/1310406
group_by(F, L) ->
    lists:foldr(fun ({K, V}, D) -> dict:append(K, V, D) end,
                dict:new(),
                [ {F(X), X} || X <- L ]).

host_cluster(Host) -> host_param(cluster, Host).
host_node(Host)    -> host_param(node, Host).
host_vars(Host)    -> host_param(vars, Host).
%% Directory name in _build
rel_name(Host)     -> host_param(relname, Host).

host_param(Name, {_, Params}) ->
    {Name, Param} = lists:keyfind(Name, 1, Params),
    Param.

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

%% Writes onto travis console directly
report_progress(Format, Args) ->
    Message = io_lib:format(Format, Args),
    file:write_file("/tmp/progress", Message, [append]).

travis_fold(Description, Fun) ->
    case os:getenv("TRAVIS_JOB_ID") of
        false ->
            Fun();
        _ ->
            io:format("travis_fold:start:~ts~n", [Description]),
            Result = Fun(),
            io:format("travis_fold:end:~ts~n", [Description]),
            Result
    end.

%% Import code paths from a running node.
%% It allows cover:analyse/2 to find source file by calling
%% Module:module_info(compiled).
import_code_paths(FromNode) when is_atom(FromNode) ->
    Paths = rpc:call(FromNode, code, get_path, []),
    code:add_paths(Paths).

%% covecov.json cover format
%% ------------------------------------------------------------------

%% Writes codecov.json into root of the repo directory
%%
%% Format:
%%
%% {"coverage": {
%%     "src/mod_mam.erl": [0, 1, 4, null]
%% }}
%%
%% Where:
%%
%% - 0 - zero hits
%% - null - lines, that are not executed (comments, patterns...)
%% - 1 - called once
%% - 4 - called 4 times
export_codecov_json() ->
    Modules = cover:imported_modules(),
    %% Result is a flat map of `{{Module, Line}, CallTimes}'
    {result, Result, _} = cover:analyse(Modules, calls, line),
    Mod2Data = lists:foldl(fun add_cover_line_into_array/2, #{}, Result),
    JSON = maps:fold(fun format_array_to_list/3, [], Mod2Data),
    Binary = jiffy:encode(#{<<"coverage">> => {JSON}}),
    file:write_file(repo_dir() ++ "/codecov.json", Binary).

add_cover_line_into_array({{Module, Line}, CallTimes}, Acc) ->
    %% Set missing lines to null
    CallsPerLineArray = maps:get(Module, Acc, array:new({default, null})),
    Acc#{Module => array:set(Line, CallTimes, CallsPerLineArray)}.

format_array_to_list(Module, CallsPerLineArray, Acc) ->
    ListOfCallTimes = array:to_list(CallsPerLineArray),
    BinPath = list_to_binary(get_source_path(Module)),
    [{BinPath, ListOfCallTimes}|Acc].

%% We assume that all mongooseim modules are loaded on this node
get_source_path(Module) when is_atom(Module) ->
    try
        AbsPath = proplists:get_value(source, Module:module_info(compile)),
        string_prefix(AbsPath, get_repo_dir())
    catch Error:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        error_logger:warning_msg("issue=get_source_path_failed module=~p "
                                  "reason=~p:~p stacktrace=~1000p",
                                 [Module, Error, Reason, Stacktrace]),
        atom_to_list(Module) ++ ".erl"
    end.

get_repo_dir() ->
    %% We make an assumption, that mongooseim.erl is in src/ directory
    MongoosePath = proplists:get_value(source, mongooseim:module_info(compile)),
    string_suffix(MongoosePath, "src/mongooseim.erl").

%% Removes string prefix
%% string:prefix/2 that works for any version of erlang and does not return nomatch
string_prefix([H|String], [H|Prefix]) ->
    string_prefix(String, Prefix);
string_prefix(String, []) ->
    String.

%% Removes string suffix
string_suffix(String, Suffix) ->
    StringR = lists:reverse(String),
    SuffixR = lists:reverse(Suffix),
    lists:reverse(string_prefix(StringR, SuffixR)).

%% Gets result of file operation and prints filename, if we have any issues.
handle_file_error(FileName, {error, Reason}) ->
    error_logger:error_msg("issue=file_operation_error filename=~p reason=~p",
                           [FileName, Reason]),
    {error, Reason};
handle_file_error(_FileName, Other) ->
    Other.

%% ------------------------------------------------------------------
