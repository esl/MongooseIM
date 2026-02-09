%%%-------------------------------------------------------------------
%%% @doc Integration test suite for mod_broadcast module.
%%%
%%% Tests the broadcast functionality via mod_broadcast_api module.
%%% Database correctness is validated indirectly via API calls.
%%% Requires RDBMS backend.
%%%-------------------------------------------------------------------
-module(mod_broadcast_SUITE).
-author('piotr.nosek@erlang-solutions.com').

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% lifecycle
-export([start_broadcast_ok_returns_job_id/1,
         start_broadcast_running_job_limit_exceeded/1,
         start_broadcast_two_domains_both_ok/1,
         resume_jobs_after_restart/1,
         manager_restart_is_idempotent_to_live_job_workers/1,
         abort_broadcast_running_ok/1,
         abort_broadcast_not_found/1,
         abort_broadcast_not_running_returns_not_running/1,
         delete_inactive_by_ids_skips_running/1,
         delete_inactive_by_domain_empty_ok/1,
         delete_inactive_by_domain_deletes_only_inactive/1,
         broadcast_instrumentation_metrics/1]).
%% validation
-export([start_broadcast_sender_not_found/1,
         start_broadcast_bad_message_rate/1,
         start_broadcast_bad_name/1,
         start_broadcast_bad_subject/1,
         start_broadcast_bad_body/1,
         start_broadcast_string_limits_ok/1]).
%% retrieval
-export([get_broadcast_not_found/1,
         get_broadcast_ok_returns_expected_fields/1,
         get_broadcasts_empty_ok/1,
         get_broadcasts_pagination_basic/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [domain/0, secondary_domain/0]).
-import(broadcast_helper, [slow_job_spec/3,
                           fast_job_spec/3,
                           start_broadcast/1,
                           get_broadcast/2,
                           get_broadcasts/3,
                           abort_broadcast/2,
                           delete_inactive_broadcasts_by_ids/2,
                           delete_inactive_broadcasts_by_domain/1,
                           does_worker_for_job_exist/2,
                           wait_until_worker_started/2,
                           broadcast_job_to_map/1]).

%%====================================================================
%% CT callbacks
%%====================================================================

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    case (not ct_helper:is_ct_running())
         orelse mongoose_helper:is_rdbms_enabled(domain()) of
        true ->
            [{group, lifecycle},
             {group, validation},
             {group, retrieval}];
        false ->
            {skip, require_rdbms}
    end.

groups() ->
    [{lifecycle, [sequence], lifecycle_tests()},
     {validation, [], validation_tests()},
     {retrieval, [], retrieval_tests()}].

lifecycle_tests() ->
    [start_broadcast_ok_returns_job_id,
    start_broadcast_running_job_limit_exceeded,
     start_broadcast_two_domains_both_ok,
     resume_jobs_after_restart,
     manager_restart_is_idempotent_to_live_job_workers,
     abort_broadcast_running_ok,
     abort_broadcast_not_found,
     abort_broadcast_not_running_returns_not_running,
     delete_inactive_by_ids_skips_running,
     delete_inactive_by_domain_empty_ok,
     delete_inactive_by_domain_deletes_only_inactive,
     broadcast_instrumentation_metrics].

validation_tests() ->
    [start_broadcast_sender_not_found,
     start_broadcast_bad_message_rate,
     start_broadcast_bad_name,
     start_broadcast_bad_subject,
     start_broadcast_bad_body,
     start_broadcast_string_limits_ok].

retrieval_tests() ->
    [get_broadcast_not_found,
     get_broadcast_ok_returns_expected_fields,
     get_broadcasts_empty_ok,
     get_broadcasts_pagination_basic].

%%====================================================================
%% Suite setup/teardown
%%====================================================================

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules([domain(), secondary_domain()], Config),
    Config2 = instrument_helper:ensure_frequent_probes(Config1),
    ensure_mod_broadcast_started(domain()),
    ensure_mod_broadcast_started(secondary_domain()),
    broadcast_helper:create_many_users(100),
    clean_broadcast_jobs_and_verify(),
    escalus:init_per_suite(Config2).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    broadcast_helper:delete_many_users(100),
    dynamic_modules:restore_modules(Config),
    instrument_helper:restore_probe_interval(Config),
    escalus:end_per_suite(Config).

%%====================================================================
%% Group setup/teardown
%%====================================================================

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%====================================================================
%% Test case setup/teardown
%%====================================================================

init_per_testcase(broadcast_instrumentation_metrics = TestCase, Config) ->
    instrument_helper:start(instrument_helper:declared_events(mod_broadcast)),
    escalus:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(broadcast_instrumentation_metrics = TestCase, Config) ->
    instrument_helper:stop(),
    clean_broadcast_jobs_and_verify(),
    escalus:end_per_testcase(TestCase, Config);
end_per_testcase(resume_jobs_after_restart = TestCase, Config) ->
    delete_rogue_broadcast_job(secondary_domain()),
    clean_broadcast_jobs_and_verify(),
    escalus:end_per_testcase(TestCase, Config);
end_per_testcase(TestCase, Config) ->
    clean_broadcast_jobs_and_verify(),
    escalus:end_per_testcase(TestCase, Config).

%%====================================================================
%% Lifecycle tests
%%====================================================================

start_broadcast_ok_returns_job_id(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid, JobName),
        {ok, JobId} = start_broadcast(JobSpec),
        assert_non_neg_integer(JobId)
    end).

start_broadcast_running_job_limit_exceeded(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid, JobName),
        {ok, _JobId1} = start_broadcast(JobSpec),
        {running_job_limit_exceeded, _} = start_broadcast(JobSpec)
    end).

start_broadcast_two_domains_both_ok(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], fun(Alice, AliceBis) ->
        AliceJid = escalus_client:short_jid(Alice),
        AliceBisJid = escalus_client:short_jid(AliceBis),
        JobSpecA = slow_job_spec(domain(), AliceJid, JobName),
        JobSpecB = slow_job_spec(secondary_domain(), AliceBisJid, JobName),
        {ok, JobIdA} = start_broadcast(JobSpecA),
        {ok, JobIdB} = start_broadcast(JobSpecB),
        assert_non_neg_integer(JobIdA),
        assert_non_neg_integer(JobIdB),
        ?assertNotEqual(JobIdA, JobIdB)
    end).

resume_jobs_after_restart(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], fun(Alice, AliceBis) ->
        DomainA = domain(),
        DomainB = secondary_domain(),
        AliceJid = escalus_client:short_jid(Alice),
        AliceBisJid = escalus_client:short_jid(AliceBis),
        JobSpecA = slow_job_spec(DomainA, AliceJid, JobName),
        JobSpecB = slow_job_spec(DomainB, AliceBisJid, JobName),

        {ok, JobIdA} = start_broadcast(JobSpecA),
        {ok, JobIdB} = start_broadcast(JobSpecB),

        true = does_worker_for_job_exist(DomainA, JobIdA),
        true = does_worker_for_job_exist(DomainB, JobIdB),

        stop_mod_broadcast(DomainA),
        stop_mod_broadcast(DomainB),

        update_job_owner_node(DomainB, JobIdB, bogus_owner_node()),

        ensure_mod_broadcast_started(DomainA),
        ensure_mod_broadcast_started(DomainB),

        true = does_worker_for_job_exist(DomainA, JobIdA),
        false = does_worker_for_job_exist(DomainB, JobIdB)
    end).

manager_restart_is_idempotent_to_live_job_workers(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        HostType = domain(),
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(HostType, AliceJid, JobName),
        {ok, JobId} = start_broadcast(JobSpec),

        wait_until_worker_started(HostType, JobId),
        WorkersBefore = get_worker_ids_and_pids(HostType),
        ?assert(lists:keymember(JobId, 1, WorkersBefore)),

        ManagerPid = get_manager_pid(HostType),
        MonitorRef = erlang:monitor(process, ManagerPid),

        exit(ManagerPid, kill),
        ok = wait_for_manager_down(MonitorRef, ManagerPid),

        wait_for_manager_restart(HostType, ManagerPid),

        WorkersAfter = get_worker_ids_and_pids(HostType),
        ?assertEqual(lists:sort(WorkersBefore), lists:sort(WorkersAfter))
    end).

abort_broadcast_running_ok(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid, JobName),
        {ok, JobId} = start_broadcast(JobSpec),
        {ok, JobId} = abort_broadcast(domain(), JobId),
        %% Verify state changed (poll with bounded retries)
        wait_until_job_state(domain(), JobId, abort_admin)
    end).

abort_broadcast_not_found(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], fun(_Alice, AliceBis) ->
        AliceBisJid = escalus_client:short_jid(AliceBis),
        %% Create a job in domain B
        ForeignDomainJobID = create_job_in_domain(secondary_domain(), AliceBisJid, JobName),
        NonExistentID = 999999,

        {broadcast_not_found, _} = abort_broadcast(domain(), NonExistentID),
        {broadcast_not_found, _} = abort_broadcast(domain(), ForeignDomainJobID)
    end).

abort_broadcast_not_running_returns_not_running(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid, JobName),
        {ok, JobId} = start_broadcast(JobSpec),
        %% First abort it
        {ok, _} = abort_broadcast(domain(), JobId),
        wait_until_job_state(domain(), JobId, abort_admin),
        %% Now try to abort again - it's already not running
        {not_running, _} = abort_broadcast(domain(), JobId)
    end).

delete_inactive_by_ids_skips_running(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid, JobName),

        {ok, InactiveId} = start_and_abort_job(JobSpec),
        {ok, RunningId} = start_broadcast(JobSpec),

        BogusId = 999999,
        {ok, [InactiveId]} = delete_inactive_broadcasts_by_ids(domain(),
                                                               [RunningId, InactiveId, BogusId]),

        {broadcast_not_found, _} = get_broadcast(domain(), InactiveId),
        {ok, _} = get_broadcast(domain(), RunningId)
    end).

delete_inactive_by_domain_empty_ok(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(_Alice) ->
        {ok, []} = get_broadcasts(domain(), 10, 0),
        {ok, []} = delete_inactive_broadcasts_by_domain(domain())
    end).

delete_inactive_by_domain_deletes_only_inactive(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid, JobName),

        {ok, InactiveId} = start_and_abort_job(JobSpec),
        {ok, RunningId} = start_broadcast(JobSpec),

        {ok, [InactiveId]} = delete_inactive_broadcasts_by_domain(domain()),
        ?assertMatch({broadcast_not_found, _},
                     get_broadcast(domain(), InactiveId)),
        ?assertMatch({ok, _}, get_broadcast(domain(), RunningId))
    end).

broadcast_instrumentation_metrics(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        Labels = labels(),

        wait_for_live_jobs_count(0),

        %% Phase 1: Fast job that finishes normally
        TS0 = instrument_helper:timestamp(),
        FastJobSpec = fast_job_spec(domain(), AliceJid, JobName),
        {ok, FastJobId} = start_broadcast(FastJobSpec),
        wait_until_job_state(domain(), FastJobId, finished),

        instrument_helper:assert(mod_broadcast_jobs_started, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 1, min_timestamp => TS0}),
        instrument_helper:assert(mod_broadcast_jobs_finished, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 1, min_timestamp => TS0}),
        instrument_helper:assert(mod_broadcast_jobs_aborted_admin, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 0, min_timestamp => TS0}),
        instrument_helper:assert(mod_broadcast_jobs_aborted_error, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 0, min_timestamp => TS0}),

        instrument_helper:assert(mod_broadcast_recipients_processed, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => positive, min_timestamp => TS0}),
        instrument_helper:assert(mod_broadcast_recipients_success, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => positive, min_timestamp => TS0}),
        instrument_helper:assert(mod_broadcast_recipients_skipped, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 0, min_timestamp => TS0}),

        %% Phase 2: Slow job that gets aborted
        TS1 = instrument_helper:timestamp(),
        SlowJobSpec = slow_job_spec(domain(), AliceJid, JobName),
        {ok, SlowJobId} = start_broadcast(SlowJobSpec),

        wait_for_live_jobs_count(1),

        {ok, SlowJobId} = abort_broadcast(domain(), SlowJobId),
        wait_until_job_state(domain(), SlowJobId, abort_admin),

        wait_for_live_jobs_count(0),

        instrument_helper:assert(mod_broadcast_jobs_started, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 1, min_timestamp => TS1}),
        instrument_helper:assert(mod_broadcast_jobs_aborted_admin, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 1, min_timestamp => TS1}),
        instrument_helper:assert(mod_broadcast_jobs_aborted_error, Labels,
                                 fun(#{count := 1}) -> true end,
                                 #{expected_count => 0, min_timestamp => TS1})
    end).

%%====================================================================
%% Validation tests
%%====================================================================

start_broadcast_sender_not_found(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(_Alice) ->
        NonExistentJid = jid:make_noprep(<<"nonexistent">>, domain(), <<>>),
        JobSpec = slow_job_spec(domain(), NonExistentJid, JobName),
        {sender_not_found, _} = start_broadcast(JobSpec)
    end).

start_broadcast_bad_message_rate(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        InvalidRates = [
            % too low
            0, -1,
            % too high
            1001, 9999],
        lists:foreach(fun(Rate) ->
            JobSpec = (slow_job_spec(domain(), AliceJid, JobName))#{message_rate := Rate},
            Result = start_broadcast(JobSpec),
            ?assertMatch({bad_parameter, _}, Result,
                         io_lib:format("message_rate ~p should fail", [Rate]))
        end, InvalidRates)
    end).

start_broadcast_bad_name(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Empty and too long
        InvalidNames = [<<>>, binary:copy(<<"a">>, 251)],
        lists:foreach(fun(Name) ->
            JobSpec = (slow_job_spec(domain(), AliceJid, JobName))#{name := Name},
            Result = start_broadcast(JobSpec),
            ?assertMatch({bad_parameter, _}, Result,
                         io_lib:format("name length ~p should fail", [byte_size(Name)]))
        end, InvalidNames)
    end).

start_broadcast_bad_subject(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Too long (>1024)
        TooLongSubject = binary:copy(<<"a">>, 1025),
        JobSpec = (slow_job_spec(domain(), AliceJid, JobName))#{subject := TooLongSubject},
        Result = start_broadcast(JobSpec),
        ?assertMatch({bad_parameter, _}, Result)
    end).

start_broadcast_bad_body(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Empty and too long
        InvalidBodies = [<<>>, binary:copy(<<"a">>, 16001)],
        lists:foreach(fun(Body) ->
            JobSpec = (slow_job_spec(domain(), AliceJid, JobName))#{body := Body},
            Result = start_broadcast(JobSpec),
            ?assertMatch({bad_parameter, _}, Result,
                         io_lib:format("body length ~p should fail", [byte_size(Body)]))
        end, InvalidBodies)
    end).

start_broadcast_string_limits_ok(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Test boundary values that should be accepted
        BoundaryCases = [
            %% {field, min_value, max_value}
            {name, binary:copy(<<"a">>, 1), binary:copy(<<"a">>, 250)},
            {subject, <<>>, binary:copy(<<"a">>, 1024)},
            {body, binary:copy(<<"a">>, 1), binary:copy(<<"a">>, 16000)}
        ],
        lists:foreach(fun({Field, MinVal, MaxVal}) ->
            %% Test minimum
            JobSpecMin = maps:put(Field, MinVal, slow_job_spec(domain(), AliceJid, JobName)),
            ResultMin = start_and_abort_job(JobSpecMin),
            ?assertMatch({ok, _}, ResultMin,
                         io_lib:format("~p min value should succeed", [Field])),

            %% Test maximum
            JobSpecMax = maps:put(Field, MaxVal, slow_job_spec(domain(), AliceJid, JobName)),
            ResultMax = start_and_abort_job(JobSpecMax),
            ?assertMatch({ok, _}, ResultMax,
                         io_lib:format("~p max value should succeed", [Field]))
        end, BoundaryCases)
    end).

%%====================================================================
%% Retrieval tests
%%====================================================================

get_broadcast_not_found(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], fun(_Alice, AliceBis) ->
        %% First create a job in domain B
        AliceBisJid = escalus_client:short_jid(AliceBis),
        ForeignDomainJobID = create_job_in_domain(secondary_domain(), AliceBisJid, JobName),

        NonExistentID = 999999,
        {broadcast_not_found, _} = get_broadcast(domain(), ForeignDomainJobID),
        {broadcast_not_found, _} = get_broadcast(domain(), NonExistentID)
    end).

get_broadcast_ok_returns_expected_fields(Config) ->
    JobName = ?FUNCTION_NAME,
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid, JobName),
        #{name := ExpectedName,
          domain := ExpectedDomain,
          sender := ExpectedSender,
          subject := ExpectedSubject,
          body := ExpectedBody,
          message_rate := ExpectedRate,
          recipient_group := ExpectedRecipientGroup} = JobSpec,
        ExpectedNode = maps:get(node, distributed_helper:mim()),

        {ok, JobId} = start_broadcast(JobSpec),
        Job = broadcast_job_to_map(get_broadcast(domain(), JobId)),

        %% Verify expected fields are present and match
        #{id := JobId,
          domain := ExpectedDomain,
          name := ExpectedName,
          sender := ExpectedSender,
          subject := ExpectedSubject,
          body := ExpectedBody,
          message_rate := ExpectedRate,
          execution_state := running,
          owner_node := ExpectedNode,
          create_timestamp := {{_, _, _}, {_, _, _}},
          stop_timestamp := undefined,
          abortion_reason := undefined,
          recipient_group := ExpectedRecipientGroup,
          recipient_count := RecipientCount,
          recipients_processed := RecipientsProcessed} = Job,

        assert_non_neg_integer(RecipientCount),
        assert_non_neg_integer(RecipientsProcessed),

        abort_broadcast(domain(), JobId),
        wait_until_job_state(domain(), JobId, abort_admin),

        AbortedJob = broadcast_job_to_map(get_broadcast(domain(), JobId)),
        #{execution_state := abort_admin,
          start_timestamp := {{_, _, _}, {_, _, _}},
          stop_timestamp := {{_, _, _}, {_, _, _}}} = AbortedJob
    end).

get_broadcasts_empty_ok(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(_Alice) ->
        {ok, []} = get_broadcasts(domain(), 10, 0)
    end).

get_broadcasts_pagination_basic(Config) ->
    JobName = atom_to_binary(?FUNCTION_NAME, utf8),
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),

        {ok, JobId1} = start_and_abort_job(slow_job_spec(domain(), AliceJid, <<JobName/binary, "-pag1">>)),
        {ok, JobId2} = start_and_abort_job(slow_job_spec(domain(), AliceJid, <<JobName/binary, "-pag2">>)),
        {ok, JobId3} = start_and_abort_job(slow_job_spec(domain(), AliceJid, <<JobName/binary, "-pag3">>)),

        {ok, AllBroadcasts} = get_broadcasts(domain(), 100, 0),
        assert_ids_in_jobs([JobId3, JobId2, JobId1], AllBroadcasts),
        {ok, MostRecentTwo} = get_broadcasts(domain(), 2, 0),
        assert_ids_in_jobs([JobId3, JobId2], MostRecentTwo),
        {ok, AllButFirst} = get_broadcasts(domain(), 100, 1),
        assert_ids_in_jobs([JobId2, JobId1], AllButFirst)
    end).

%%====================================================================
%% Test helpers
%%====================================================================

ensure_mod_broadcast_started(HostType) ->
    dynamic_modules:ensure_modules(HostType, [{mod_broadcast, #{backend => rdbms}}]).

stop_mod_broadcast(HostType) ->
    {stopped, _} = dynamic_modules:stop(HostType, mod_broadcast).

assert_non_neg_integer(Value) when is_integer(Value), Value >= 0 ->
    ok.

assert_ids_in_jobs(ExpectedIds, Jobs) ->
    ActualIds = [maps:get(id, broadcast_job_to_map(Job)) || Job <- Jobs],
    ?assertEqual(ExpectedIds, ActualIds).

create_job_in_domain(Domain, SenderJid, TestName) ->
    JobSpec = slow_job_spec(Domain, SenderJid, TestName),
    {ok, JobId} = start_broadcast(JobSpec),
    JobId.

start_and_abort_job(JobSpec) ->
    Domain = maps:get(domain, JobSpec),
    {ok, JobId} = start_broadcast(JobSpec),
    {ok, _} = abort_broadcast(Domain, JobId),
    wait_until_job_state(Domain, JobId, abort_admin),
    {ok, JobId}.

wait_until_job_state(Domain, JobId, ExpectedState) ->
    wait_helper:wait_until(
        fun() ->
            case get_broadcast(Domain, JobId) of
                {ok, JobRecord} ->
                    case broadcast_job_to_map(JobRecord) of
                        #{execution_state := State} when State =:= ExpectedState ->
                            ok;
                        #{execution_state := Other} ->
                            {error, {not_expected_state, Other}}
                    end;
                {broadcast_not_found, _} ->
                    {error, not_found}
            end
        end,
        ok,
        #{name => wait_for_job_state}).

wait_for_live_jobs_count(ExpectedCount) ->
    F = fun(#{count := Count}) -> Count =:= ExpectedCount end,
    instrument_helper:wait_and_assert_new(mod_broadcast_live_jobs, labels(), F).

wait_for_manager_down(MonitorRef, ManagerPid) ->
    receive
        {'DOWN', MonitorRef, process, ManagerPid, _Reason} ->
            ok
    after 5000 ->
        {error, manager_stop_timeout}
    end.

wait_for_manager_restart(HostType, OldPid) ->
    wait_helper:wait_until(
        fun() -> get_manager_pid(HostType) end,
        undefined,
        #{validator => fun(Pid) -> is_pid(Pid) andalso Pid =/= OldPid end,
          name => wait_for_manager_restart}).

labels() ->
    #{host_type => domain_helper:host_type()}.

%%====================================================================
%% RPC proxies
%%====================================================================

call_manager(Function, Args) ->
    rpc(mim(), broadcast_manager, Function, Args).

get_manager_pid(HostType) ->
    ProcName = rpc(mim(), gen_mod, get_module_proc, [HostType, broadcast_manager]),
    rpc(mim(), erlang, whereis, [ProcName]).

get_worker_ids_and_pids(HostType) ->
    Children = call_manager(get_supervisor_children, [HostType]),
    [{Id, Pid} || {Id, Pid, _Type, _Modules} <- Children, is_integer(Id), is_pid(Pid)].

clean_broadcast_jobs_and_verify() ->
    broadcast_helper:clean_broadcast_jobs(),
    {selected, []} = sql_query(domain(), ["SELECT * FROM broadcast_jobs"]),
    [] = get_worker_ids_and_pids(domain()),
    [] = get_worker_ids_and_pids(secondary_domain()),
    assert_empty_worker_map(domain()),
    assert_empty_worker_map(secondary_domain()),
    ok.

assert_empty_worker_map(HostType) ->
    WorkerMap = call_manager(get_worker_map, [HostType]),
    case maps:size(WorkerMap) of
        0 ->
            ok;
        _Positive ->
            WorkerList = [ {JobId, Pid, is_process_alive(Pid)}
                          || {JobId, Pid} <- maps:to_list(WorkerMap)],
            ct:fail({manager_has_live_workers, HostType, WorkerList})
    end.

%%====================================================================
%% Direct DB operations
%%====================================================================

update_job_owner_node(Domain, JobId, OwnerNode) ->
    Query = ["UPDATE broadcast_jobs SET owner_node = ",
             "'", OwnerNode, "'",
             " WHERE id = ", integer_to_binary(JobId),
             " AND server = ", "'", Domain, "'"],
    {updated, 1} = sql_query(Domain, Query),
    ok.

bogus_owner_node() ->
    <<"silly_goose@nowhere">>.

delete_rogue_broadcast_job(Domain) ->
    Query = ["DELETE FROM broadcast_jobs",
             " WHERE owner_node = '", bogus_owner_node(), "'"],
    {updated, 1} = sql_query(Domain, Query),
    ok.

sql_query(HostType, Query) ->
    rpc(mim(), mongoose_rdbms, sql_query, [HostType, Query]).
