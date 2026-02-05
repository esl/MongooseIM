%%%-------------------------------------------------------------------
%%% @doc Small test suite for broadcast_manager error/defensive paths.
%%%
%%% This suite covers error cases that are difficult to reproduce in normal
%%% operation, such as unexpected messages in gen_server callbacks and DB
%%% operation failures. Uses meck to mock mod_broadcast_backend and related
%%% modules without starting a real database.
%%%-------------------------------------------------------------------
-module(broadcast_manager_SUITE).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("src/broadcast/mod_broadcast.hrl").


%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    %% manager_unexpected_messages
    handle_call_unknown_returns_not_implemented_and_alive/1,
    handle_cast_unknown_does_not_crash/1,
    handle_info_unknown_does_not_crash/1,
    %% db_error_paths
    db_create_job_error_propagates/1,
    db_get_running_jobs_error_in_resume/1,
    db_set_job_started_error_logged/1,
    db_set_job_aborted_admin_error_logged/1,
    db_get_running_jobs_error_in_abort_for_domain/1,
    %% supervisor_error_paths
    supervisor_start_child_error_on_job_creation/1,
    supervisor_start_child_error_on_resume/1,
    stop_job_when_worker_not_found/1
]).

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [{group, manager_unexpected_messages},
     {group, db_error_paths},
     {group, supervisor_error_paths}].

groups() ->
    [{manager_unexpected_messages, [], manager_unexpected_messages_tests()},
     {db_error_paths, [], db_error_path_tests()},
     {supervisor_error_paths, [], supervisor_error_path_tests()}].

manager_unexpected_messages_tests() ->
    [handle_call_unknown_returns_not_implemented_and_alive,
     handle_cast_unknown_does_not_crash,
     handle_info_unknown_does_not_crash].

db_error_path_tests() ->
    [db_create_job_error_propagates,
     db_get_running_jobs_error_in_resume,
     db_set_job_started_error_logged,
     db_set_job_aborted_admin_error_logged,
     db_get_running_jobs_error_in_abort_for_domain].

supervisor_error_path_tests() ->
    [supervisor_start_child_error_on_job_creation,
     supervisor_start_child_error_on_resume,
     stop_job_when_worker_not_found].

%%====================================================================
%% Suite setup/teardown
%%====================================================================

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Group setup/teardown
%%====================================================================

init_per_group(_Group, Config) ->
    setup_mocks_for_manager(),
    Config.

end_per_group(_Group, _Config) ->
    teardown_mocks(),
    ok.

%%====================================================================
%% Test case setup/teardown
%%====================================================================

init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = start_test_manager(),
    [{manager_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    Pid = ?config(manager_pid, Config),
    stop_test_manager(Pid),
    reset_meck_defaults(),
    Config.

%%====================================================================
%% Manager unexpected messages tests
%%====================================================================

handle_call_unknown_returns_not_implemented_and_alive(Config) ->
    Pid = ?config(manager_pid, Config),
    {error, not_implemented} = gen_server:call(Pid, somebody_set_us_up_the_bomb),
    assert_manager_survived(Pid).

handle_cast_unknown_does_not_crash(Config) ->
    Pid = ?config(manager_pid, Config),
    ok = gen_server:cast(Pid, somebody_set_us_up_the_bomb),
    assert_manager_survived(Pid).

handle_info_unknown_does_not_crash(Config) ->
    Pid = ?config(manager_pid, Config),
    Pid ! somebody_set_us_up_the_bomb,
    assert_manager_survived(Pid).

%%====================================================================
%% DB error path tests
%%====================================================================

db_create_job_error_propagates(Config) ->
    Pid = ?config(manager_pid, Config),
    meck:expect(mod_broadcast_backend, create_job,
                fun(_HostType, _JobSpec) ->
                    {error, somebody_set_us_up_the_bomb}
                end),
    JobSpec = valid_job_spec(),
    {error, somebody_set_us_up_the_bomb} = gen_server:call(Pid, {start_job, JobSpec}),
    assert_manager_survived(Pid).

db_get_running_jobs_error_in_resume(Config) ->
    Pid = ?config(manager_pid, Config),
    stop_test_manager(Pid),

    meck:expect(mod_broadcast_backend, get_running_jobs,
                fun(_HostType) ->
                    meck:exception(error, somebody_set_us_up_the_bomb)
                end),

    {ok, NewPid} = start_test_manager(),
    assert_manager_survived(NewPid).

db_set_job_started_error_logged(Config) ->
    Pid = ?config(manager_pid, Config),
    meck:expect(mod_broadcast_backend, create_job,
                fun(_HostType, _JobSpec) ->
                    {ok, 12345}
                end),
    meck:expect(mod_broadcast_backend, set_job_started,
                fun(_HostType, _JobId) ->
                    {error, somebody_set_us_up_the_bomb}
                end),
    meck:expect(supervisor, start_child,
                fun(_SupName, _ChildSpec) ->
                    {ok, spawn_link(fun() -> receive stop -> ok end end)}
                end),

    JobSpec = valid_job_spec(),
    {ok, 12345} = gen_server:call(Pid, {start_job, JobSpec}),
    assert_manager_survived(Pid).

db_set_job_aborted_admin_error_logged(Config) ->
    Pid = ?config(manager_pid, Config),
    JobId = 99999,

    FakeWorkerPid = spawn_link(fun() -> receive stop -> ok end end),
    meck:expect(supervisor, which_children,
                fun(_SupName) ->
                    [{JobId, FakeWorkerPid, worker, [broadcast_worker]}]
                end),
    meck:expect(broadcast_worker, stop,
                fun(WorkerPid) ->
                    WorkerPid ! stop,
                    ok
                end),
    meck:expect(mod_broadcast_backend, set_job_aborted_admin,
                fun(_HostType, _JobId) ->
                    {error, somebody_set_us_up_the_bomb}
                end),

    ok = gen_server:call(Pid, {stop_job, JobId}),
    assert_manager_survived(Pid),
    true = meck:called(broadcast_worker, stop, [FakeWorkerPid]).

db_get_running_jobs_error_in_abort_for_domain(Config) ->
    Pid = ?config(manager_pid, Config),
    meck:expect(mod_broadcast_backend, get_running_jobs,
                fun(_HostType) ->
                    meck:exception(error, somebody_set_us_up_the_bomb)
                end),

    ok = gen_server:call(Pid, {abort_running_jobs_for_domain, <<"test.domain">>}),
    assert_manager_survived(Pid).

%%====================================================================
%% Supervisor error path tests
%%====================================================================

supervisor_start_child_error_on_job_creation(Config) ->
    Pid = ?config(manager_pid, Config),
    meck:expect(mod_broadcast_backend, create_job,
                fun(_HostType, _JobSpec) ->
                    {ok, 12345}
                end),
    meck:expect(supervisor, start_child,
                fun(_SupName, _ChildSpec) ->
                    {error, somebody_set_us_up_the_bomb}
                end),

    JobSpec = valid_job_spec(),
    {ok, 12345} = gen_server:call(Pid, {start_job, JobSpec}),
    assert_manager_survived(Pid).

supervisor_start_child_error_on_resume(Config) ->
    Pid = ?config(manager_pid, Config),
    stop_test_manager(Pid),

    meck:expect(mod_broadcast_backend, get_running_jobs,
                fun(_HostType) ->
                    {ok, [sample_broadcast_job()]}
                end),
    meck:expect(supervisor, start_child,
                fun(_SupName, _ChildSpec) ->
                    {error, somebody_set_us_up_the_bomb}
                end),

    {ok, NewPid} = start_test_manager(),
    assert_manager_survived(NewPid).

stop_job_when_worker_not_found(Config) ->
    Pid = ?config(manager_pid, Config),
    meck:expect(supervisor, which_children, fun(_SupName) -> [] end),

    {error, not_live} = gen_server:call(Pid, {stop_job, 12345}),
    assert_manager_survived(Pid).

%%====================================================================
%% Helper functions
%%====================================================================

setup_mocks_for_manager() ->
    meck:new(gen_mod, [no_link, passthrough]),
    meck:expect(gen_mod, get_module_proc,
                fun(_HostType, Module) ->
                    list_to_atom("test_" ++ atom_to_list(Module))
                end),

    meck:new(mod_broadcast_backend, [no_link]),

    meck:new(ejabberd_auth, [no_link]),
    meck:expect(ejabberd_auth, does_user_exist,
                fun(_HostType, _Jid, stored) ->
                    true
                end),
    meck:expect(ejabberd_auth, get_vh_registered_users_number,
                fun(_Domain) ->
                    {ok, 1000}
                end),

    meck:new(supervisor, [no_link, unstick, passthrough]),

    meck:new(broadcast_worker, [no_link]),

    meck:new(mongoose_instrument, [no_link]),
    meck:expect(mongoose_instrument, execute,
                fun(_Event, _Labels, _Measurements) -> ok end),

    % Initial mock for certain functions but they may be overridden in tests
    reset_meck_defaults().

reset_meck_defaults() ->
    meck:expect(mod_broadcast_backend, get_running_jobs,
                fun(_HostType) -> {ok, []} end),
    meck:expect(mod_broadcast_backend, create_job,
                fun(_HostType, _JobSpec) -> {ok, 1} end),
    meck:expect(mod_broadcast_backend, set_job_started,
                fun(_HostType, _JobId) -> ok end),
    meck:expect(mod_broadcast_backend, set_job_aborted_admin,
                fun(_HostType, _JobId) -> ok end),
    meck:expect(supervisor, which_children,
                fun(_SupName) -> [] end),
    meck:expect(supervisor, start_child,
                fun(_SupName, _ChildSpec) -> {ok, spawn(fun() -> ok end)} end),
    meck:expect(broadcast_worker, stop,
                fun(_Pid) -> ok end).

teardown_mocks() ->
    catch meck:unload(gen_mod),
    catch meck:unload(mod_broadcast_backend),
    catch meck:unload(ejabberd_auth),
    catch meck:unload(supervisor),
    catch meck:unload(broadcast_worker),
    catch meck:unload(mongoose_instrument),
    ok.

start_test_manager() ->
    gen_server:start(broadcast_manager, host_type(), []).

stop_test_manager(Pid) ->
    catch gen_server:stop(Pid, normal, 1000),
    ok.

valid_job_spec() ->
    #{name => <<"Test Broadcast">>,
      domain => <<"test.domain">>,
      sender => jid:make_noprep(<<"admin">>, <<"test.domain">>, <<>>),
      subject => <<"Test Subject">>,
      body => <<"Test message body">>,
      message_rate => 100,
      recipient_group => all_users_in_domain}.

sample_broadcast_job() ->
    #broadcast_job{id = 999,
                   name = <<"Test">>,
                   host_type = host_type(),
                   domain = <<"test.domain">>,
                   sender = jid:make_noprep(<<"admin">>, <<"test.domain">>, <<>>),
                   subject = <<"Subject">>,
                   body = <<"Body">>,
                   message_rate = 100,
                   recipient_group = all_users_in_domain,
                   owner_node = node(),
                   recipient_count = 10,
                   recipients_processed = 5,
                   execution_state = running,
                   abortion_reason = undefined,
                   created_at = {{2026, 2, 4}, {12, 0, 0}},
                   started_at = {{2026, 2, 4}, {12, 0, 1}},
                   stopped_at = undefined}.

assert_manager_survived(Pid) ->
    %% If a manager is able to reply to a call, then it is alive
    {error, not_implemented} = gen_server:call(Pid, somebody_set_us_up_the_bomb).

host_type() ->
    <<"test_host_type">>.