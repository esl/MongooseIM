%%% @doc Common Test Example Common Test Hook module.
-module(ct_mongoose_hook).

%% @doc Add the following line in your *.spec file
%% to check server purity after every suite:
%% {ct_hooks, [ct_mongoose_hook]}.

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

-define(RPC(M,F,A), escalus_ejabberd:rpc(M, F, A)).
-record(state, {print_group, print_case}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_mongoose_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, Opts) ->
    Unfolded = proplists:unfold(Opts),
    PrintGroup = proplists:get_value(print_group, Unfolded, false),
    PrintCase = proplists:get_value(print_case, Unfolded, false),
    {ok, #state{print_group = PrintGroup, print_case = PrintCase }}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,State) ->
    Preset = case application:get_env(common_test, test_label) of
                 {ok, Value} -> Value;
                 _ -> undefined
             end,
    NewConfig = [{preset, Preset} | Config],
    maybe_print_on_server(true, "SUITE", Suite, "starting"),
    {NewConfig, State}.

%% @doc Called after init_per_suite.
post_init_per_suite(Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(Suite, Config, Return, State) ->
    maybe_print_on_server(true, "SUITE", Suite, "finished"),
    check_server_purity(Suite, Config),
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(Group,Config,State) ->
    maybe_print_on_server(State#state.print_group, "GROUP", Group, "starting"),
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(Group,_Config,Return,State) ->
    maybe_print_on_server(State#state.print_group, "GROUP", Group, "finished"),
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(TC,Config,State) ->
    maybe_print_on_server(State#state.print_case, "TEST CASE", TC, "starting"),
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(TC,_Config,Return,State) ->
    maybe_print_on_server(State#state.print_case, "TEST CASE", TC, "finished"),
    {Return, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.
on_tc_skip(_TC, _Reason, State) ->
    State.

%% @doc Called when the scope of the CTH is done
terminate(_State) ->
    ok.

maybe_print_on_server(false, _, _, _) ->
    ok;
maybe_print_on_server(true, Event, EventName, EvenType) ->
    escalus_ejabberd:rpc(error_logger, warning_msg,
                         ["====== ~s ~p ~s", [Event, EventName, EvenType]]).

check_server_purity(Suite, Config) ->
    case escalus_server:name(Config) of
        mongooseim ->
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
        fun check_roster/0],
    lists:flatmap(fun(F) -> F() end, Funs).

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
