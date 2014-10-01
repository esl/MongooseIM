%%% @doc Common Test Example Common Test Hook module.
-module(ct_tty_hook).

%% @doc Add the following line in your *.spec file to enable
%% reasonable, tty error reporting for your common tests:
%% {ct_hooks, [ct_tty_hook]}.

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).
-record(state, { total, suite_total, ts, tcs, data }).

-define(RPC(M,F,A), escalus_ejabberd:rpc(M, F, A)).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_tty_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    {ok, #state{ total = 0, data = [] }}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(_Suite,Config,State) ->
    {Config, State#state{ suite_total = 0, tcs = [] }}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(Suite,_Config,Return,State) ->
    check_server_purity(Suite),
    Data = {suites, Suite, State#state.suite_total, lists:reverse(State#state.tcs)},
    {Return, State#state{ data = [Data | State#state.data] ,
                          total = State#state.total + State#state.suite_total } }.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC,Config,State) ->
    {Config, State#state{ ts = now(), total = State#state.suite_total + 1 } }.

%% @doc Called after each test case.
post_end_per_testcase(TC,_Config,Return,State) ->
    TCInfo = {testcase, TC, Return, timer:now_diff(now(), State#state.ts)},
    {Return, State#state{ ts = undefined, tcs = [TCInfo | State#state.tcs] } }.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.
on_tc_skip(_TC, Reason, State) ->
    case Reason of
        %% Just quit if files were not build correctly
        {tc_user_skip, "Make failed"} -> erlang:halt(1);
        _                             -> State
    end.

%% @doc Called when the scope of the CTH is done
terminate(State) ->
    lists:foreach(fun (SuiteData) ->
                          print_suite(SuiteData)
                  end,
                  State#state.data),
    ok.


aggregate_results(Cases) ->
    Oks =   [ C || {testcase, _, ok, _} =         C <- Cases ],
    Fails = [ C || {testcase, _, {error, _}, _} = C <- Cases ],
    {length(Oks), length(Fails)}.

print_suite({suites, Name,_, TestCases}) ->
    case aggregate_results(TestCases) of
        {Oks, 0} -> print_suite_ok(Name, Oks);
        {Oks, Fails} -> print_suite_failed(Name,Oks,Fails,TestCases)
    end, ok.

print_suite_ok(Name, OkCount) ->
    io:format("~n====== Suite OK: ~p (All ~p tests passed)~n",
           [Name, OkCount]).
print_suite_failed(Name, OkCount, FailCount, Cases) ->
    ct:pal("~n====== Suite FAILED: ~p (~p of ~p tests failed)~n",
           [Name, FailCount, OkCount+FailCount]),
    lists:foreach(fun maybe_print_test_case/1, Cases).

maybe_print_test_case({testcase, _Name,ok,_})              -> ok;
maybe_print_test_case({testcase, Name,{error, Content},_}) ->
    io:format("~n====== Test name: ~p", [Name]),
    io:format("~n====== Reason:    ~p~n", [Content]).

check_server_purity(Suite) ->
    case is_mongoose() of
        true ->
            case catch do_check_server_purity(Suite) of
                [] ->
                    ok;
                R ->
                    ct:pal(warning,
                           "Suite: ~p finished dirty. Other suites may fail because of that. "
                           "Details:~n~p",[Suite, R])
            end;
        _ ->
            ok
    end.

do_check_server_purity(_Suite) ->
    Funs = [fun check_sessions/0,
            fun check_registered_users/0,
            fun check_registered_users_count/0,
            fun check_offline_messages/0,
            fun check_active_users/0,
            fun check_privacy/0,
            fun check_private/0,
            fun check_vcard/0,
            fun check_roster/0,
            fun check_carboncopy/0],
    lists:flatmap(fun(F) -> F() end, Funs).

is_mongoose() ->
    Apps = escalus_ejabberd:rpc(application, loaded_applications, []),
    lists:keymember(mongooseim, 1, Apps).

check_sessions() ->
    case ?RPC(ejabberd_sm, get_full_session_list, []) of
        [] -> [];
        Sessions -> [{opened_sessions, Sessions}]
    end.

check_registered_users() ->
    case ?RPC(ejabberd_auth, dirty_get_registered_users, []) of
        [] -> [];
        Users -> [{registered_users, Users}]
    end.

check_registered_users_count() ->
    D = escalus_ct:get_config(ejabberd_domain),
    case ?RPC(ejabberd_auth, get_vh_registered_users_number, [D]) of
        0 -> [];
        N -> [{registered_users_count, N}]
    end.

check_offline_messages() ->
    generic_via_mongoose_helper(total_offline_messages).

check_active_users() ->
    generic_via_mongoose_helper(total_active_users).

check_privacy() ->
    generic_via_mongoose_helper(total_privacy_items).

check_private() ->
    generic_via_mongoose_helper(total_private_items).

check_vcard() ->
    generic_via_mongoose_helper(total_vcard_items).

check_roster() ->
    generic_via_mongoose_helper(total_roster_items).

check_carboncopy() ->
    D = escalus_ct:get_config(ejabberd_domain),
    case ?RPC(gen_mod, is_loaded, [D, mod_carboncopy]) of
        true ->
            do_check_carboncopy();
        _ ->
            []
    end.

generic_via_mongoose_helper(Function) ->
    case mongoose_helper:Function() of
        0 -> [];
        false -> [];
        N -> [{Function, N}]
    end.

do_check_carboncopy() ->
    case ?RPC(ets, tab2list, [carboncopy]) of
        [] -> [];
        L -> [{remaining_carbon_copy_settings, L}]
    end.