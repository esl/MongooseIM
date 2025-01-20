-module(ct_test_hook).
-export([id/1]). %% required to prevent multiple hook executions
-export([init/2, terminate/1]).
-export([post_all/3, post_groups/2]).
-export([on_tc_fail/4, on_tc_skip/4]).
-export([pre_init_per_suite/3, post_init_per_suite/4,
         pre_end_per_suite/3, post_end_per_suite/4]).
-export([pre_init_per_group/4, post_init_per_group/5,
         pre_end_per_group/4, post_end_per_group/5]).
-export([pre_init_per_testcase/4, post_init_per_testcase/5,
         pre_end_per_testcase/4, post_end_per_testcase/5]).

-record(state,{log = []}).
-define(MFA, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}).
% -define(ADD_LOG(State), (State)).
-define(ADD_LOG(State), (State#state{log = [?MFA | State#state.log]})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CT hook callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
id(_Opts)->
    start_tracing(),
    ?MODULE.

init(_Id, _Opts) ->
    start_tracing(),
    {ok, ?ADD_LOG(#state{})}.

terminate(_CTHState) ->
    stop_tracing().

on_tc_fail(_SuiteName, _TestName, _Reason, CTHState) ->
    ?ADD_LOG(CTHState).

on_tc_skip(_SuiteName, _TestName, _Reason, CTHState) ->
    ?ADD_LOG(CTHState).

post_all(_SuiteName, Return, _GroupDefs) -> Return.

post_groups(_SuiteName, GroupDefs) -> GroupDefs.

pre_init_per_suite(_SuiteName, InitData, CTHState) when is_list(InitData)->
    {[{ct_test_hook, true} | InitData], ?ADD_LOG(CTHState)};
pre_init_per_suite(_SuiteName, InitData, CTHState) ->
    {InitData, ?ADD_LOG(CTHState)}.

post_init_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, ?ADD_LOG(CTHState)}.

pre_end_per_suite(_SuiteName, EndData, CTHState) ->
    {EndData, ?ADD_LOG(CTHState)}.

post_end_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, ?ADD_LOG(CTHState)}.

pre_init_per_group(_SuiteName, _GroupName, InitData, CTHState) ->
    {InitData, ?ADD_LOG(CTHState)}.

post_init_per_group(_SuiteName, _GroupName, _Config, Return, CTHState) ->
    {Return, ?ADD_LOG(CTHState)}.

pre_end_per_group(_SuiteName, _GroupName, EndData, CTHState) ->
    {EndData, ?ADD_LOG(CTHState)}.

post_end_per_group(_SuiteName, _GroupName, _Config, Return, CTHState) ->
    {Return, ?ADD_LOG(CTHState)}.

pre_init_per_testcase(_SuiteName, _TestcaseName, InitData, CTHState) ->
    {InitData, ?ADD_LOG(CTHState)}.

post_init_per_testcase(_SuiteName, _TestcaseName, _Config, Return, CTHState) ->
    {Return, ?ADD_LOG(CTHState)}.

pre_end_per_testcase(_SuiteName, _TestcaseName, EndData, CTHState) ->
    {EndData, ?ADD_LOG(CTHState)}.

post_end_per_testcase(_SuiteName, _TestcaseName, _Config, Return, CTHState) ->
    {Return, ?ADD_LOG(CTHState)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_tracing() ->
    %% this function is idempotent and can be safely called multiple times
    case dbg:tracer(process, {fun tracer_fun/2, ok}) of
        {ok, _Pid} ->
            dbg:p(all, call),
            % dbg:tp(?MODULE, []),
            dbg:tp(ct_markdown_errors_hook, cx),
            ok;
        {error, already_started} ->
            ok
    end.

tracer_fun(Msg, State) ->
    io:format("~n!DBG!: ~p~n", [Msg]),
    State.

stop_tracing() ->
    %% this function is idempotent and can be safely called multiple times
    timer:sleep(10000),
    dbg:stop().
