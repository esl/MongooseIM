%% @doc Common filename functions
-module(path_helper).
%% Paths
-export([repo_dir/1]).
-export([test_dir/1]).
-export([ct_run_dir/1]).
-export([ct_run_dir_in_browser/1]).
-export([data_dir/2]).

%% Path transformation
-export([canonicalize_path/1]).

%% @doc Get repository root directory
repo_dir(Config) ->
    get_env_var("REPO_DIR").

%% @doc Get `big_tests/' directory
test_dir(Config) ->
    get_env_var("TEST_DIR").

%% @doc Returns`big_tests/ct_report/ct_run.*' directory
%% Run it from a test case functions only (not group or suite functions)
ct_run_dir(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    %% Remove: *SUITE.logs/run.*/log_private/
    RunDir = path_helper:test_dir(Config),
    filename:absname(filename:join([PrivDir, "..", "..", ".."])).

%% @doc Returns path, corresponding to `ct_run_dir' in browser
ct_run_dir_in_browser(Config) ->
    "../..".

%% @doc Unsafe version of `filename:safe_relative_path/1'
canonicalize_path(Path) -> canonicalize_path(filename:split(Path), []).

canonicalize_path([], Acc) -> filename:join(lists:reverse(Acc));
canonicalize_path([".." | Path], [_ | Acc]) -> canonicalize_path(Path, Acc);
canonicalize_path(["." | Path], Acc) -> canonicalize_path(Path, Acc);
canonicalize_path([Elem | Path], Acc) -> canonicalize_path(Path, [Elem | Acc]).


get_env_var(VarName) ->
    case os:getenv(VarName) of
        false ->
            ct:fail({undefined_envvar, VarName});
        Value ->
            Value
    end.

%% Hand-made data_dir from Common Tests
data_dir(SuiteName, Config) ->
    filename:join([test_dir(Config), "tests", atom_to_list(SuiteName) ++ "_data"]).
