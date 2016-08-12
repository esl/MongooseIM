%%% @doc Common Test Example Common Test Hook module.
-module(ct_progress_hook).

%% @doc Add the following line in your *.spec file to enable
%% reasonable, progress error reporting for your common tests:
%% {ct_hooks, [ct_progress_hook]}.

%% Callbacks
-export([id/1]).
-export([init/2]).
-export([post_end_per_testcase/4]).
-record(state, { }).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_progress_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    {ok, #state{  }}.

%% @doc Called after each test case.
post_end_per_testcase(TC, _Config, Return, State) ->
    case Return of
        {fail, _} ->
            file:write_file("/tmp/progress", "-", [append]);
        {skip, _} ->
            file:write_file("/tmp/progress", "?", [append]);
        _ ->
            file:write_file("/tmp/progress", "+", [append])
    end,
    {Return, State}.
