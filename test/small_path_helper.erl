%% @doc Common filename functions for Small Tests
%%
%% Big tests version is named just path_helper.
-module(small_path_helper).
%% Paths
-export([repo_dir/1]).
-export([test_dir/1]).

%% @doc Get repository root directory
repo_dir(_Config) ->
    %% Full path of present working directory.
    %% We are using the fact that rebar3 runs from inside the repo root.
    %% There is one case when it's not true:
    %% if someone is using MongooseIM as a dependency.
    %%
    %% Big tests use a separate env variable here, but for small tests
    %% we still want "./rebar3 ct" to work without specifying any extra args.
    get_env_var("PWD").

%% @doc Get `test/' directory
test_dir(Config) ->
    filename:join(repo_dir(Config), "test").

get_env_var(VarName) ->
    case os:getenv(VarName) of
        false ->
            ct:fail({undefined_envvar, VarName});
        Value ->
            Value
    end.
