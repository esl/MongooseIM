%%% @doc Common Test Example Common Test Hook module.
-module(ct_progress_hook).

%% @doc Add the following line in your *.spec file to enable
%% reasonable, progress error reporting for your common tests:
%% {ct_hooks, [ct_progress_hook]}.

%% Callbacks
-export([id/1]).
-export([init/2]).
-export([post_init_per_suite/4,
         post_init_per_group/4,
         post_init_per_testcase/4]).
-export([post_end_per_suite/4,
         post_end_per_group/4,
         post_end_per_testcase/4]).
-record(state, { }).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_progress_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    {ok, #state{  }}.

post_init_per_suite(_SuiteName, _Config, Return, State) ->
    handle_return(Return, false),
    {Return, State}.

post_init_per_group(_GroupName, _Config, Return, State) ->
    handle_return(Return, false),
    {Return, State}.

post_init_per_testcase(_TC, _Config, Return, State) ->
    handle_return(Return, false),
    {Return, State}.

post_end_per_suite(_SuiteName, _Config, Return, State) ->
    handle_return(Return, false),
    {Return, State}.

post_end_per_group(_GroupName, _Config, Return, State) ->
    handle_return(Return, false),
    {Return, State}.

%% @doc Called after each test case.
post_end_per_testcase(_TC, _Config, Return, State) ->
    handle_return(Return, true),
    {Return, State}.

handle_return(Return, ReportSuccess) ->
    case Return of
        {'EXIT', _} ->
            file:write_file("/tmp/progress", "-", [append]);
        {fail, _} ->
            file:write_file("/tmp/progress", "-", [append]);
        {error, _} ->
            file:write_file("/tmp/progress", "-", [append]);
        {skip, _} ->
            file:write_file("/tmp/progress", "?", [append]);
        _ when ReportSuccess ->
            file:write_file("/tmp/progress", "+", [append]);
        _ ->
            ok
    end.
