%%%-------------------------------------------------------------------
%%% @doc Small test suite for broadcast_worker defensive/error paths.
%%%
%%% This suite covers error cases that are difficult to reproduce in normal
%%% operation, such as DB failures, stuck workers, unexpected events, and
%%% crash recovery scenarios. Uses meck to mock dependencies without
%%% starting a real database or router.
%%%-------------------------------------------------------------------
-module(broadcast_worker_SUITE).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("mod_broadcast.hrl").

-include("log_helper.hrl").

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
    %% stop_paths
    stop_force_kills_stuck_worker/1,
    %% init_paths
    load_job_failure_stops_worker/1,
    resume_finished_worker_exits_normal/1,
    %% batch_loading_paths
    persist_before_load_failure_aborts/1,
    load_next_batch_failure_aborts/1,
    %% sending_paths
    batch_to_batch_transition_completes/1,
    route_exception_skips_recipient_and_finishes/1,
    %% finalization_paths
    final_state_persist_failure_exits_normal/1,
    %% unexpected_events
    unexpected_event_in_sending_batch_survives/1,
    %% message_id
    message_id_is_deterministic/1
]).

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [{group, stop_paths},
     {group, init_paths},
     {group, batch_loading_paths},
     {group, sending_paths},
     {group, finalization_paths},
     {group, unexpected_events},
     {group, message_id}].

groups() ->
    [{stop_paths, [], stop_path_tests()},
     {init_paths, [], init_path_tests()},
     {batch_loading_paths, [], batch_loading_path_tests()},
     {sending_paths, [], sending_path_tests()},
     {finalization_paths, [], finalization_path_tests()},
     {unexpected_events, [], unexpected_event_tests()},
     {message_id, [], message_id_tests()}].

stop_path_tests() ->
    [stop_force_kills_stuck_worker].

init_path_tests() ->
    [load_job_failure_stops_worker,
     resume_finished_worker_exits_normal].

batch_loading_path_tests() ->
    [persist_before_load_failure_aborts,
     load_next_batch_failure_aborts].

sending_path_tests() ->
    [batch_to_batch_transition_completes,
     route_exception_skips_recipient_and_finishes].

finalization_path_tests() ->
    [final_state_persist_failure_exits_normal].

unexpected_event_tests() ->
    [unexpected_event_in_sending_batch_survives].

message_id_tests() ->
    [message_id_is_deterministic].

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
    setup_mocks_for_worker(),
    Config.

end_per_group(_Group, _Config) ->
    teardown_mocks(),
    ok.

%%====================================================================
%% Test case setup/teardown
%%====================================================================

init_per_testcase(stop_force_kills_stuck_worker, Config) ->
    log_helper:set_up(),
    Config;
init_per_testcase(message_id_is_deterministic, Config) ->
    Config;
init_per_testcase(_TestCase, Config) ->
    reset_meck_defaults(),
    Config.

end_per_testcase(stop_force_kills_stuck_worker, _Config) ->
    log_helper:tear_down(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Stop path tests
%%====================================================================

stop_force_kills_stuck_worker(_Config) ->
    StuckWorker = spawn(fun() -> receive _ -> exit(timeout) end end),

    log_helper:subscribe(),
    ok = broadcast_worker:stop(StuckWorker),
    ?assertLog(warning, #{what := broadcast_worker_killed_timeout, worker_pid := StuckWorker}),
    ok.

%%====================================================================
%% Init path tests
%%====================================================================

load_job_failure_stops_worker(_Config) ->
    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _JobId) ->
                    meck:exception(error, get_job_failed)
                end),

    {error, {load_failed, {error, get_job_failed}}} =
        start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _JobId) ->
                    {ok, broadcast_helper:sample_broadcast_job()}
                end),
    meck:expect(mod_broadcast_backend, get_worker_state,
                fun(_HostType, _JobId) ->
                    meck:exception(error, get_state_failed)
                end),

        {error, {load_failed, {error, get_state_failed}}} =
            start_and_monitor_worker(broadcast_helper:host_type(), 12345),
    ok.

resume_finished_worker_exits_normal(_Config) ->
    meck:expect(mod_broadcast_backend, get_worker_state,
                fun(_HostType, _JobId) ->
                    {ok, #broadcast_worker_state{cursor = undefined,
                                                  recipients_processed = 100,
                                                  finished = true}}
                end),
    meck:expect(mod_broadcast_backend, update_worker_state,
                fun(_HostType, _JobId, _State) -> ok end),

    {ok, {Pid, MonRef}} = start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    %% Worker should exit normally without loading any batches
    receive
        {'DOWN', MonRef, process, Pid, normal} -> ok
    after 1000 ->
        ct:fail(worker_did_not_exit_normal)
    end,

    %% Verify no batch loading occurred
    false = meck:called(mongoose_gen_auth, get_registered_users_snapshot, '_'),
    ok.

%%====================================================================
%% Batch loading path tests
%%====================================================================

persist_before_load_failure_aborts(_Config) ->
    meck:expect(mod_broadcast_backend, update_worker_state,
                fun(_HostType, _JobId, _State) ->
                    meck:exception(error, db_write_failed)
                end),

    {ok, {Pid, MonRef}} = start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    receive
        {'DOWN', MonRef, process, Pid, {error, {persist_state_failed, {error, db_write_failed}}}} -> ok
    after 1000 ->
        ct:fail(worker_did_not_abort)
    end,
    ok.

load_next_batch_failure_aborts(_Config) ->
    meck:expect(mod_broadcast_backend, update_worker_state,
                fun(_HostType, _JobId, _State) -> ok end),
    meck:expect(mongoose_gen_auth, get_registered_users_snapshot,
                fun(_AuthMod, _HostType, _Domain, _Params) ->
                    {error, snapshot_unavailable}
                end),

    {ok, {Pid, MonRef}} = start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    receive
        {'DOWN', MonRef, process, Pid, {error, {load_batch_failed, snapshot_unavailable}}} -> ok
    after 1000 ->
        ct:fail(worker_did_not_abort)
    end,
    ok.

%%====================================================================
%% Sending path tests
%%====================================================================

batch_to_batch_transition_completes(_Config) ->
    Self = self(),
    BatchCallCount = counters:new(1, [atomics]),

    meck:expect(mod_broadcast_backend, update_worker_state,
                fun(_HostType, _JobId, _State) -> ok end),
    meck:expect(mongoose_gen_auth, get_registered_users_snapshot,
                fun(_AuthMod, _HostType, _Domain, Params) ->
                    Count = counters:get(BatchCallCount, 1) + 1,
                    counters:put(BatchCallCount, 1, Count),
                    Self ! {batch_loaded, Count},
                    case Count of
                        1 ->
                            #{limit := _, snapshot_timestamp := _} = Params,
                            false = maps:is_key(cursor, Params),
                            {ok, {[<<"user1">>, <<"user2">>], <<"cursor1">>}};
                        2 ->
                            #{limit := _, cursor := <<"cursor1">>} = Params,
                            false = maps:is_key(snapshot_timestamp, Params),
                            {ok, {[<<"user3">>], undefined}}
                    end
                end),

    {ok, {Pid, MonRef}} = start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    receive {batch_loaded, 1} -> ok after 1000 -> ct:fail(batch_1_not_loaded) end,
    receive {batch_loaded, 2} -> ok after 1000 -> ct:fail(batch_2_not_loaded) end,

    receive
        {'DOWN', MonRef, process, Pid, normal} -> ok
    after 2000 ->
        ct:fail(worker_did_not_finish)
    end,

    3 = meck:num_calls(ejabberd_router, route, '_'),
    ok.

route_exception_skips_recipient_and_finishes(_Config) ->
    RouteCallCount = counters:new(1, [atomics]),

    meck:expect(mod_broadcast_backend, update_worker_state,
                fun(_HostType, _JobId, _State) -> ok end),
    meck:expect(mongoose_gen_auth, get_registered_users_snapshot,
                fun(_AuthMod, _HostType, _Domain, _Params) ->
                    {ok, {[<<"user1">>, <<"user2">>, <<"user3">>], undefined}}
                end),
    meck:expect(ejabberd_router, route,
                fun(_From, _To, _Acc, _Stanza) ->
                    Count = counters:get(RouteCallCount, 1) + 1,
                    counters:put(RouteCallCount, 1, Count),
                    case Count of
                        2 ->
                            meck:exception(error, router_crashed);
                        _ ->
                            ok
                    end
                end),

    {ok, {Pid, MonRef}} = start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    receive
        {'DOWN', MonRef, process, Pid, normal} -> ok
    after 2000 ->
        ct:fail(worker_did_not_finish)
    end,

    3 = meck:num_calls(ejabberd_router, route, ['_', '_', '_', '_']),
    1 = meck:num_calls(mongoose_instrument, execute,
                       [mod_broadcast_recipients_skipped, '_', '_']),
    ok.

%%====================================================================
%% Finalization path tests
%%====================================================================

final_state_persist_failure_exits_normal(_Config) ->
    PersistCallCount = counters:new(1, [atomics]),

    meck:expect(mod_broadcast_backend, update_worker_state,
                fun(_HostType, _JobId, _State) ->
                    Count = counters:get(PersistCallCount, 1) + 1,
                    counters:put(PersistCallCount, 1, Count),
                    case Count of
                        1 ->
                            %% First persist (before batch) succeeds
                            ok;
                        2 ->
                            %% Final persist (in finished state) fails
                            meck:exception(error, final_persist_crashed)
                    end
                end),
    meck:expect(mongoose_gen_auth, get_registered_users_snapshot,
                fun(_AuthMod, _HostType, _Domain, _Params) ->
                    {ok, {[<<"user1">>], undefined}}
                end),

    {ok, {Pid, MonRef}} = start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    %% Worker should still exit normally despite final persist failure
    receive
        {'DOWN', MonRef, process, Pid, normal} -> ok
    after 2000 ->
        ct:fail(worker_did_not_finish_normal)
    end,
    ok.

%%====================================================================
%% Unexpected event tests
%%====================================================================

unexpected_event_in_sending_batch_survives(_Config) ->
    %% Use slow message rate to have time to inject events
    Job = broadcast_helper:sample_broadcast_job(1),
    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _JobId) -> {ok, Job} end),
    meck:expect(mongoose_gen_auth, get_registered_users_snapshot,
                fun(_AuthMod, _HostType, _Domain, _Params) ->
                    %% Multiple recipients to ensure we stay in sending_batch
                    {ok, {[<<"user1">>, <<"user2">>, <<"user3">>], undefined}}
                end),

    Self = self(),
    meck:expect(ejabberd_router, route,
                fun(_From, _To, _Acc, _Stanza) ->
                    Self ! in_sending_batch,
                    ok
                end),

    {ok, {Pid, _MonRef}} = start_and_monitor_worker(broadcast_helper:host_type(), 12345),

    receive in_sending_batch -> ok after 1000 -> ct:fail(not_in_sending_batch) end,

    gen_statem:cast(Pid, unexpected_cast),
    Pid ! unexpected_info,

    ok = broadcast_worker:stop(Pid).

%%====================================================================
%% Message ID tests
%%====================================================================

message_id_is_deterministic(_Config) ->
    JobId = 12345,
    Recipient = jid:make_bare(<<"alice">>, <<"example.com">>),

    %% Call make_message_id multiple times
    MsgId1 = broadcast_worker:make_message_id(JobId, Recipient),
    MsgId2 = broadcast_worker:make_message_id(JobId, Recipient),
    MsgId3 = broadcast_worker:make_message_id(JobId, Recipient),

    %% All should be identical
    MsgId1 = MsgId2,
    MsgId2 = MsgId3,

    %% Should start with "mb-" prefix
    <<"mb-", _Rest/binary>> = MsgId1,

    %% Different recipient should produce different ID
    OtherRecipient = jid:make_bare(<<"bob">>, <<"example.com">>),
    OtherRecipientMsgId = broadcast_worker:make_message_id(JobId, OtherRecipient),
    ?assertNotEqual(MsgId1, OtherRecipientMsgId),

    %% Different job ID should produce different ID
    OtherJobMsgId = broadcast_worker:make_message_id(99999, Recipient),
    ?assertNotEqual(MsgId1, OtherJobMsgId),
    ok.

%%====================================================================
%% Helper functions
%%====================================================================

setup_mocks_for_worker() ->
    meck:new(mod_broadcast_backend, [no_link]),
    meck:new(mongoose_gen_auth, [no_link]),
    meck:new(ejabberd_router, [no_link]),
    meck:new(mongoose_instrument, [no_link]),
    meck:new(mongoose_acc, [no_link]),

    meck:expect(mongoose_instrument, execute,
                fun(_Event, _Labels, _Measurements) -> ok end),
    meck:expect(mongoose_acc, new,
                fun(_Opts) -> #{} end),

    reset_meck_defaults().

reset_meck_defaults() ->
    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _JobId) -> {ok, broadcast_helper:sample_broadcast_job()} end),
    meck:expect(mod_broadcast_backend, get_worker_state,
                fun(_HostType, _JobId) -> {error, not_found} end),
    meck:expect(mod_broadcast_backend, update_worker_state,
                fun(_HostType, _JobId, _State) -> ok end),
    meck:expect(mongoose_gen_auth, get_registered_users_snapshot,
                fun(_AuthMod, _HostType, _Domain, _Params) ->
                    {ok, {[<<"user1">>], undefined}}
                end),
    meck:expect(ejabberd_router, route,
                fun(_From, _To, _Acc, _Stanza) -> ok end),
    meck:reset(ejabberd_router).

teardown_mocks() ->
    catch meck:unload(),
    ok.

start_and_monitor_worker(HostType, JobId) ->
    case gen_statem:start_monitor(broadcast_worker, {HostType, JobId}, []) of
        {ok, Pid, MonRef} ->
            {ok, {Pid, MonRef}};
        Error ->
            Error
    end.
