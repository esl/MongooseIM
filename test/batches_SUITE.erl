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
       filled_batch_raises_batch_metric,
       unfilled_batch_raises_flush_metric,
       timeouts_and_canceled_timers_do_not_need_to_log_messages,
       prepare_task_works,
       sync_flushes_down_everything,
       sync_aggregates_down_everything,
       aggregating_error_is_handled,
       async_request
      ]}
    ].

init_per_suite(Config) ->
    meck:new(telemetry, [stub_all, no_link]),
    meck:new(mongoose_metrics, [stub_all, no_link]),
    Config.

end_per_suite(_Config) ->
    meck:unload().

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    pg:start_link(),
    mim_ct_sup:start_link(ejabberd_sup),
    meck:new(gen_mod, [passthrough]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(gen_mod),
    ok.

%% Tests
internal_starts_another_cache(_) ->
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), []),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), [{module, internal}]),
    L = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ?assertEqual(2, length(L)).

external_does_not_start_another_cache(_) ->
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), []),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), [{module, ?mod(1)}]),
    L = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ?assertEqual(1, length(L)).

internal_stop_does_stop_the_cache(_) ->
    meck:expect(gen_mod, get_module_opt, fun(_, _, module, _) -> internal end),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), []),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), [{module, internal}]),
    L1 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ct:pal("Value ~p~n", [L1]),
    mongoose_user_cache:stop_cache(host_type(), ?mod(2)),
    L2 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ct:pal("Value ~p~n", [L2]),
    ?assertNotEqual(L1, L2).

external_stop_does_nothing(_) ->
    meck:expect(gen_mod, get_module_opt, fun(_, _, module, _) -> ?mod(1) end),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), []),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), [{module, ?mod(1)}]),
    L1 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    mongoose_user_cache:stop_cache(host_type(), ?mod(2)),
    L2 = [S || S = {_Name, _Pid, worker, [segmented_cache]} <- supervisor:which_children(ejabberd_sup)],
    ?assertEqual(L1, L2).

shared_cache_inserts_in_shared_table(_) ->
    meck:expect(gen_mod, get_module_opt, fun(_, _, module, _) -> ?mod(1) end),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(1), []),
    mongoose_user_cache:start_new_cache(host_type(), ?mod(2), [{module, ?mod(1)}]),
    mongoose_user_cache:merge_entry(host_type(), ?mod(2), some_jid(), #{}),
    ?assert(mongoose_user_cache:is_member(host_type(), ?mod(1), some_jid())).

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
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             request_callback => fun(Task, _) -> timer:sleep(1), gen_server:send_request(Server, Task) end,
             aggregate_callback => fun(T1, T2, _) -> {ok, T1 + T2} end,
             verify_callback => fun(ok, _T, _) -> ok end,
             flush_extra => #{host_type => host_type()}},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    ?assertEqual(skipped, gen_server:call(Pid, sync)),
    [ gen_server:cast(Pid, {task, key, N}) || N <- lists:seq(1, 1000) ],
    ?assertEqual(ok, gen_server:call(Pid, sync)),
    ?assertEqual(500500, gen_server:call(Server, get_acc)).

aggregating_error_is_handled(_) ->
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             request_callback => fun(_, _) -> gen_server:send_request(Server, return_error) end,
             aggregate_callback => fun(T1, T2, _) -> {ok, T1 + T2} end,
             verify_callback => fun(ok, _T, _) -> ok end,
             flush_extra => #{host_type => host_type()}},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    gen_server:cast(Pid, {task, key, 0}),
    async_helper:wait_until(
      fun() -> gen_server:call(Server, get_acc) end, 0).

async_request(_) ->
    {ok, Server} = gen_server:start_link(?MODULE, [], []),
    Opts = #{host_type => host_type(),
             pool_id => ?FUNCTION_NAME,
             request_callback => fun(Task, _) -> timer:sleep(1), gen_server:send_request(Server, Task) end,
             aggregate_callback => fun(T1, T2, _) -> {ok, T1 + T2} end,
             verify_callback => fun(ok, _T, _) -> ok end,
             flush_extra => #{host_type => host_type()}},
    {ok, Pid} = gen_server:start_link(mongoose_aggregator_worker, Opts, []),
    [ gen_server:cast(Pid, {task, key, N}) || N <- lists:seq(1, 1000) ],
    async_helper:wait_until(
      fun() -> gen_server:call(Server, get_acc) end, 500500).

%% helpers
host_type() ->
    <<"HostType">>.

some_jid() ->
    jid:make_noprep(<<"alice">>, <<"localhost">>, <<>>).

init([]) ->
    {ok, 0}.

handle_call(get_acc, _From, Acc) ->
    {reply, Acc, Acc};
handle_call(return_error, _From, Acc) ->
    {reply, {error, return_error}, Acc};
handle_call(N, _From, Acc) ->
    {reply, ok, N + Acc}.

handle_cast(_Msg, Acc) ->
    {noreply, Acc}.
