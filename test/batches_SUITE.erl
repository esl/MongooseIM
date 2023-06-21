-module(batches_SUITE).
-compile([export_all, nowarn_export_all]).
-behaviour(gen_server).

-include_lib("stdlib/include/assert.hrl").
-define(mod(N), list_to_atom(atom_to_list(?FUNCTION_NAME) ++ integer_to_list(N))).

all() ->
    [
     {group, cache},
     {group, async_workers}
    ].

groups() ->
    [
     {cache, [sequence],
      [
       internal_starts_another_cache,
       external_does_not_start_another_cache,
       internal_stop_does_stop_the_cache,
       external_stop_does_nothing,
       shared_cache_inserts_in_shared_table
      ]},
     {async_workers, [sequence],
      [
       broadcast_reaches_all_workers,
       broadcast_reaches_all_keys,
       filled_batch_raises_batch_metric,
       unfilled_batch_raises_flush_metric,
       timeouts_and_canceled_timers_do_not_need_to_log_messages,
       prepare_task_works,
       sync_flushes_down_everything,
       sync_aggregates_down_everything,
       aggregating_error_is_handled_and_can_continue,
       aggregation_might_produce_noop_requests,
       async_request,
       retry_request,
       retry_request_cancelled,
       retry_request_cancelled_in_verify_function,
       ignore_msg_when_waiting_for_reply,
       async_request_fails
      ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(telemetry),
    meck:new(mongoose_metrics, [stub_all, no_link]),
    Config.

end_per_suite(_Config) ->
    meck:unload().

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    pg:start_link(mim_scope),
    mim_ct_sup:start_link(ejabberd_sup),
    meck:new(gen_mod, [passthrough]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(gen_mod),
    ok.

cache_config() ->
    config_parser_helper:default_mod_config(mod_cache_users).

cache_config(internal) ->
    Def = config_parser_helper:default_mod_config(mod_cache_users),
    Def#{module => internal};
cache_config(Module) ->
    #{module => Module}.

%% Tests
internal_starts_another_cache(_) ->
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), cache_config()),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), cache_config(internal)),
    L = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ?assertEqual(2, length(L)).

external_does_not_start_another_cache(_) ->
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), cache_config()),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), cache_config(?mod(1))),
    L = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ?assertEqual(1, length(L)).

internal_stop_does_stop_the_cache(_) ->
    meck:expect(gen_mod, get_module_opt, fun(_, _, module, _) -> internal end),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), cache_config()),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), cache_config(internal)),
    L1 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ct:pal("Value ~p~n", [L1]),
    mongoose_user_cache:stop_cache(host_type(), ?mod(2)),
    L2 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ct:pal("Value ~p~n", [L2]),
    ?assertNotEqual(L1, L2).

external_stop_does_nothing(_) ->
    meck:expect(gen_mod, get_module_opt, fun(_, _, module, _) -> ?mod(1) end),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), cache_config()),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), cache_config(?mod(1))),
    L1 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    mongoose_user_cache:stop_cache(host_type(), ?mod(2)),
    L2 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ?assertEqual(L1, L2).

shared_cache_inserts_in_shared_table(_) ->
    meck:expect(gen_mod, get_module_opt, fun(_, _, module, _) -> ?mod(1) end),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), cache_config()),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), cache_config(?mod(1))),
    mongoose_user_cache:merge_entry(host_type(), ?mod(2), some_jid(), #{}),
    ?assert(mongoose_user_cache:is_member(host_type(), ?mod(1), some_jid())).

aggregation_might_produce_noop_requests(_) ->
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    Requestor = fun(1, _) -> timer:sleep(1), gen_server:send_request(Server, 1);
                   (_, _) -> drop end,
    Opts = (default_aggregator_opts(Server))#{pool_id => ?FUNCTION_NAME,
                                              request_callback => Requestor},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    [ gen_server:cast(Pid, {task, key, N}) || N <- lists:seq(1, 1000) ],
    async_helper:wait_until(
      fun() -> gen_server:call(Server, get_acc) end, 1).

broadcast_reaches_all_workers(_) ->
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    WPoolOpts = (default_aggregator_opts(Server))#{pool_type => aggregate,
                                                   pool_size => 10},
    {ok, _} = mongoose_async_pools:start_pool(host_type(), ?FUNCTION_NAME, WPoolOpts),
    mongoose_async_pools:broadcast_task(host_type(), ?FUNCTION_NAME, key, 1),
    async_helper:wait_until(
      fun() -> gen_server:call(Server, get_acc) end, 10).

broadcast_reaches_all_keys(_) ->
    HostType = host_type(),
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    Tid = ets:new(table, [public, {read_concurrency, true}]),
    Req = fun(Task, _) ->
                  case ets:member(Tid, continue) of
                      true ->
                          gen_server:send_request(Server, Task);
                      false ->
                          async_helper:wait_until(fun() -> ets:member(Tid, continue) end, true),
                          gen_server:send_request(Server, 0)
                  end
          end,
    WPoolOpts = (default_aggregator_opts(Server))#{pool_type => aggregate,
                                                   pool_size => 3,
                                                   request_callback => Req},
    {ok, _} = mongoose_async_pools:start_pool(HostType, ?FUNCTION_NAME, WPoolOpts),
    [ mongoose_async_pools:put_task(HostType, ?FUNCTION_NAME, N, 1) || N <- lists:seq(0, 1000) ],
    mongoose_async_pools:broadcast(HostType, ?FUNCTION_NAME, -1),
    ets:insert(Tid, {continue, true}),
    async_helper:wait_until(
      fun() -> gen_server:call(Server, get_acc) end, 0).

filled_batch_raises_batch_metric(_) ->
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             batch_size => 1,
             flush_interval => 1000,
             flush_callback => fun(_, _) -> ok end,
             flush_extra => #{host_type => host_type(), queue_length => 0}},
    {ok, Pid} = gen_server:start_link(mongoose_batch_worker, Opts, []),
    gen_server:cast(Pid, {task, key, ok}),
    MetricName = [mongoose_async_pools, '_', batch_flushes],
    async_helper:wait_until(
      fun() -> 0 < meck:num_calls(mongoose_metrics, update, ['_', MetricName, '_']) end, true).

unfilled_batch_raises_flush_metric(_) ->
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             batch_size => 1000,
             flush_interval => 5,
             flush_callback => fun(_, _) -> ok end,
             flush_extra => #{host_type => host_type(), queue_length => 0}},
    {ok, Pid} = gen_server:start_link(mongoose_batch_worker, Opts, []),
    gen_server:cast(Pid, {task, key, ok}),
    MetricName = [mongoose_async_pools, '_', timed_flushes],
    async_helper:wait_until(
      fun() -> 0 < meck:num_calls(mongoose_metrics, update, ['_', MetricName, '_']) end, true).

timeouts_and_canceled_timers_do_not_need_to_log_messages(_) ->
    Timeout = 10,
    QueueSize = 2,
    meck:new(logger, [passthrough, unstick]),
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             batch_size => QueueSize,
             flush_interval => Timeout,
             flush_callback => fun(_, _) -> ok end,
             flush_extra => #{host_type => host_type(), queue_length => 0}},
    {ok, Pid} = gen_server:start_link(mongoose_batch_worker, Opts, []),
    [ gen_server:cast(Pid, {task, ok}) || _ <- lists:seq(1, QueueSize) ],
    ct:sleep(Timeout*2),
    ?assertEqual(0, meck:num_calls(logger, macro_log, '_')).

prepare_task_works(_) ->
    Timeout = 1000,
    QueueSize = 2,
    T = self(),
    meck:new(logger, [passthrough, unstick]),
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             batch_size => QueueSize,
             flush_interval => Timeout,
             prep_callback => fun(0, _) -> {error, bad};
                                 (A, _) -> {ok, A + 1}
                              end,
             flush_callback => fun(Tasks, _) -> T ! {tasks, Tasks}, ok end,
             flush_extra => #{host_type => host_type(), queue_length => 0}},
    {ok, Pid} = gen_server:start_link(mongoose_batch_worker, Opts, []),
    [ gen_server:cast(Pid, {task, N}) || N <- lists:seq(0, QueueSize) ],
    receive
        {tasks, Tasks} ->
            ?assertEqual([ N + 1 || N <- lists:seq(1, QueueSize) ], Tasks)
    after
        Timeout*2 -> ct:fail(no_answer_received)
    end,
    ?assert(0 < meck:num_calls(logger, macro_log, '_')).

sync_flushes_down_everything(_) ->
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             batch_size => 5000,
             flush_interval => 5000,
             flush_callback => fun(_, _) -> ok end,
             flush_extra => #{host_type => host_type(), queue_length => 0}},
    {ok, Pid} = gen_server:start_link(mongoose_batch_worker, Opts, []),
    ?assertEqual(skipped, gen_server:call(Pid, sync)),
    gen_server:cast(Pid, {task, key, ok}),
    ?assertEqual(ok, gen_server:call(Pid, sync)),
    MetricName = [mongoose_async_pools, '_', timed_flushes],
    ?assert(0 < meck:num_calls(mongoose_metrics, update, ['_', MetricName, '_'])).

sync_aggregates_down_everything(_) ->
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    Opts = (default_aggregator_opts(Server))#{pool_id => ?FUNCTION_NAME},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    ?assertEqual(skipped, gen_server:call(Pid, sync)),
    [ gen_server:cast(Pid, {task, key, N}) || N <- lists:seq(1, 1000) ],
    ?assertEqual(ok, gen_server:call(Pid, sync)),
    ?assertEqual(500500, gen_server:call(Server, get_acc)).

aggregating_error_is_handled_and_can_continue(_) ->
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    Requestor = fun(Task, _) -> timer:sleep(1), gen_server:send_request(Server, Task) end,
    Opts = (default_aggregator_opts(Server))#{pool_id => ?FUNCTION_NAME,
                                              request_callback => Requestor},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    [ gen_server:cast(Pid, {task, key, N}) || N <- lists:seq(1, 10) ],
    gen_server:cast(Pid, {task, return_error, return_error}),
    ct:sleep(100),
    [ gen_server:cast(Pid, {task, key, N}) || N <- lists:seq(11, 100) ],
    %% We don't call sync here because sync is force flushing,
    %% we want to test that it flushes alone
    ct:sleep(100),
    ?assert(55 < gen_server:call(Server, get_acc)).

async_request(_) ->
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    Opts = (default_aggregator_opts(Server))#{pool_id => ?FUNCTION_NAME},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    [ gen_server:cast(Pid, {task, key, N}) || N <- lists:seq(1, 1000) ],
    async_helper:wait_until(
      fun() -> gen_server:call(Server, get_acc) end, 500500).

retry_request(_) ->
    Opts = (retry_aggregator_opts())#{pool_id => retry_request},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    gen_server:cast(Pid, {task, key, 1}),
    receive_task_called(0),
    receive_task_called(1),
    gen_server:cast(Pid, {task, key, 1}),
    receive_task_called(0),
    receive_task_called(1),
    ensure_no_tasks_to_receive().

retry_request_cancelled(_) ->
    Opts = (retry_aggregator_opts())#{pool_id => retry_request_cancelled,
                                      request_callback => fun do_cancel_request/2},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    gen_server:cast(Pid, {task, key, 1}),
    receive_task_called(0),
    %% 3 retries
    receive_task_called(1),
    receive_task_called(2),
    receive_task_called(3),
    ensure_no_tasks_to_receive(),
    %% Second task gets started
    gen_server:cast(Pid, {task, key, 2}),
    receive_task_called(0).

retry_request_cancelled_in_verify_function(_) ->
    Opts = (retry_aggregator_opts())#{pool_id => retry_request_cancelled_in_verify_function,
                                      request_callback => fun do_request/2,
                                      verify_callback => fun validate_all_fails/3},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    gen_server:cast(Pid, {task, key, 1}),
    receive_task_called(0),
    %% 3 retries
    receive_task_called(1),
    receive_task_called(2),
    receive_task_called(3),
    ensure_no_tasks_to_receive(),
    %% Second task gets started
    gen_server:cast(Pid, {task, key, 2}),
    receive_task_called(0).

ignore_msg_when_waiting_for_reply(_) ->
    Opts = (retry_aggregator_opts())#{pool_id => ignore_msg_when_waiting_for_reply,
                                      request_callback => fun do_request_but_ignore_other_messages/2},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    gen_server:cast(Pid, {task, key, 1}),
    receive_task_called(0),
    %% Second task gets started
    gen_server:cast(Pid, {task, key, 2}),
    receive_task_called(0).

async_request_fails(_) ->
    %% Does request that crashes the gen_server, but not the aggregator
    {ok, Server} = gen_server:start({local, async_req_fails_server}, ?MODULE, [], []),
    Ref = monitor(process, Server),
    Opts = (default_aggregator_opts(async_req_fails_server))#{pool_id => ?FUNCTION_NAME},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    gen_server:cast(Pid, {task, key, {ack_and_die, self()}}),
    %% Acked and the server dies
    receive
        {'DOWN', R, process, _, _} when R =:= Ref -> ok
        after 5000 -> error(down_receive_timeout)
    end,
    receive
        {acked, S} when S =:= Server -> ok
        after 5000 -> error(acked_receive_timeout)
    end,
    %% Eventually the task is cancelled
    async_helper:wait_until(fun() -> element(4, sys:get_state(Pid)) end, no_request_pending),
    %% Check that aggregator still processes new tasks
    %% Start the task and wait for processing
    {ok, Server2} = gen_server:start({local, async_req_fails_server}, ?MODULE, [], []),
    gen_server:cast(Pid, {task, key, {ack, self()}}),
    receive
        {acked, S2} when S2 =:= Server2 -> ok
        after 5000 -> error(acked_receive_timeout)
    end,
    %% Check state
    1 = gen_server:call(Server2, get_acc).

%% helpers
host_type() ->
    <<"HostType">>.

some_jid() ->
    jid:make_noprep(<<"alice">>, <<"localhost">>, <<>>).

default_aggregator_opts(Server) ->
    #{host_type => host_type(),
      request_callback => requester(Server),
      aggregate_callback => fun aggregate_sum/3,
      verify_callback => fun validate_all_ok/3,
      flush_extra => #{host_type => host_type()}}.

retry_aggregator_opts() ->
    #{host_type => host_type(),
      request_callback => fun do_retry_request/2,
      aggregate_callback => fun aggregate_sum/3,
      verify_callback => fun validate_ok/3,
      flush_extra => #{host_type => host_type(), origin_pid => self()}}.

validate_all_ok(ok, _, _) ->
    ok.

validate_ok(ok, _, _) ->
    ok;
validate_ok({error, Reason}, _, _) ->
    {error, Reason}.

validate_all_fails(_, _, _) ->
    {error, all_fails}.

aggregate_sum(T1, T2, _) ->
    {ok, T1 + T2}.

requester(Server) ->
    fun(return_error, _) ->
            gen_server:send_request(Server, return_error);
       ({ack_and_die, _} = Task, _) ->
            gen_server:send_request(Server, Task);
       ({ack, _} = Task, _) ->
            gen_server:send_request(Server, Task);
       (Task, _) ->
            timer:sleep(1), gen_server:send_request(Server, Task)
    end.

%% Fails first task
do_retry_request(_Task, #{origin_pid := Pid, retry_number := Retry}) ->
    Pid ! {task_called, Retry},
    Ref = make_ref(),
    Reply = case Retry of 0 -> {error, simulate_error}; 1 -> ok end,
    %% Simulate gen_server call reply
    self() ! {[alias|Ref], Reply},
    Ref.

do_request(_Task, #{origin_pid := Pid, retry_number := Retry}) ->
    Pid ! {task_called, Retry},
    Ref = make_ref(),
    Reply = ok,
    %% Simulate gen_server call reply
    self() ! {[alias|Ref], Reply},
    Ref.

%% Fails all tries
do_cancel_request(_Task, #{origin_pid := Pid, retry_number := Retry}) ->
    Pid ! {task_called, Retry},
    Ref = make_ref(),
    Reply = {error, simulate_error},
    %% Simulate gen_server call reply
    self() ! {[alias|Ref], Reply},
    Ref.

%% Fails all tries
do_request_but_ignore_other_messages(_Task, #{origin_pid := Pid, retry_number := Retry}) ->
    Pid ! {task_called, Retry},
    Ref = make_ref(),
    Reply = ok,
    %% Just send an unexpected messages which should be ignored
    self() ! unexpected_msg_should_be_ignored,
    %% Simulate gen_server call reply
    self() ! {[alias|Ref], Reply},
    Ref.

init([]) ->
    {ok, 0}.

handle_call(get_acc, _From, Acc) ->
    {reply, Acc, Acc};
handle_call(return_error, _From, Acc) ->
    {reply, {error, return_error}, Acc};
handle_call({ack_and_die, Pid}, _From, Acc) ->
    Pid ! {acked, self()},
    error(oops);
handle_call({ack, Pid}, _From, Acc) ->
    Pid ! {acked, self()},
    {reply, ok, 1 + Acc};
handle_call(N, _From, Acc) ->
    {reply, ok, N + Acc}.

handle_cast(_Msg, Acc) ->
    {noreply, Acc}.

receive_task_called(ExpectedRetryNumber) ->
    receive
        {task_called, RetryNum} ->
            ExpectedRetryNumber = RetryNum
        after 5000 ->
            error(timeout)
    end.

ensure_no_tasks_to_receive() ->
    receive {task_called, _} -> error(unexpected_msg) after 0 -> ok end.
