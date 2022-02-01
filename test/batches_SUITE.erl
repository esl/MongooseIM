-module(batches_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-define(mod(N), list_to_atom(atom_to_list(?FUNCTION_NAME) ++ integer_to_list(N))).

all() ->
    [
     {group, cache},
     {group, batch_worker}
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
     {batch_worker, [sequence],
      [
       filled_batch_raises_batch_metric,
       unfilled_batch_raises_flush_metric,
       timeouts_and_canceled_timers_do_not_need_to_log_messages
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
    {ok, Pid} = gen_server:start_link(
                  mongoose_batch_worker,
                  {host_type(), ?FUNCTION_NAME, 1000, 1, fun(_, _) -> ok end,
                   #{host_type => host_type(), queue_length => 0}}, []),
    gen_server:cast(Pid, {task, ok}),
    MetricName = [mongoose_async_pools, '_', batch_flushes],
    async_helper:wait_until(
      fun() -> 0 < meck:num_calls(mongoose_metrics, update, ['_', MetricName, '_']) end, true).

unfilled_batch_raises_flush_metric(_) ->
    {ok, Pid} = gen_server:start_link(
                  mongoose_batch_worker,
                  {host_type(), ?FUNCTION_NAME, 5, 1000, fun(_, _) -> ok end,
                   #{host_type => host_type(), queue_length => 0}}, []),
    gen_server:cast(Pid, {task, ok}),
    MetricName = [mongoose_async_pools, '_', timed_flushes],
    async_helper:wait_until(
      fun() -> 0 < meck:num_calls(mongoose_metrics, update, ['_', MetricName, '_']) end, true).

timeouts_and_canceled_timers_do_not_need_to_log_messages(_) ->
    Timeout = 10,
    QueueSize = 2,
    meck:new(logger, [passthrough, unstick]),
    {ok, Pid} = gen_server:start_link(
                  mongoose_batch_worker,
                  {host_type(), ?FUNCTION_NAME, Timeout, QueueSize, fun(_, _) -> ok end,
                   #{host_type => host_type(), queue_length => 0}}, []),
    [ gen_server:cast(Pid, {task, ok}) || _ <- lists:seq(1, QueueSize) ],
    ct:sleep(Timeout*2),
    ?assertEqual(0, meck:num_calls(logger, macro_log, '_')).

%% helpers
host_type() ->
    <<"HostType">>.

some_jid() ->
    jid:make_noprep(<<"alice">>, <<"localhost">>, <<>>).
