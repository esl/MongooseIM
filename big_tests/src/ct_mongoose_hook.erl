%%% @doc this hook performs server purity check after every suite
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

-import(distributed_helper, [mim/0,
                             rpc/4]).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_mongoose_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    domain_helper:insert_configured_domains(),
    {ok, #{}}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,State) ->
    Preset = case application:get_env(common_test, test_label) of
                 {ok, Value} -> Value;
                 _ -> undefined
             end,
    DataDir = path_helper:data_dir(Suite, Config),
    NewConfig = [{preset, Preset}, {mim_data_dir, DataDir} | Config],
    {NewConfig, State}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(Suite, Config, Return, State) ->
    check_server_purity(Suite, Config),
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(TC,Config,State) ->
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(TC,_Config,Return,State) ->
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
    domain_helper:delete_configured_domains(),
    ok.

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
    case rpc(mim(), ejabberd_sm, get_full_session_list, []) of
        [] -> [];
        Sessions -> [{opened_sessions, Sessions}]
    end.

check_registered_users() ->
    lists:flatmap(fun check_registered_users/1, mim_domains()).

check_registered_users_count() ->
    lists:flatmap(fun check_registered_users_count/1, mim_domains()).

check_registered_users(Domain) ->
    case rpc(mim(), ejabberd_auth, get_vh_registered_users, [Domain]) of
        [] -> [];
        Users -> [{registered_users, Domain, Users}]
    end.

check_registered_users_count(Domain) ->
    case rpc(mim(), ejabberd_auth, get_vh_registered_users_number, [Domain]) of
        0 -> [];
        N -> [{registered_users_count, Domain, N}]
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

generic_via_mongoose_helper(Function) ->
    case mongoose_helper:Function() of
        0 -> [];
        false -> [];
        N -> [{Function, N}]
    end.

mim_domains() ->
    [ct:get_config({hosts, mim, domain}),
     ct:get_config({hosts, mim, secondary_domain})].
