%%% @doc Regression tests for the replaced-session diagnostic.
%%%
%%% The production incident: during a rolling restart the node that hosted
%%% a replaced session disappeared between "capture the replaced pids" and "check
%%% whether they exited". `rpc:call/4' then answered `{badrpc, nodedown}', which
%%% the old `mongoose_c2s:verify_process_alive/3' had no clause for, so a
%%% `case_clause' killed the brand new connection from inside the `gen_statem'
%%% timeout callback.
%%%
%%% These tests use real Erlang nodes (`peer'), started and stopped for real, so
%%% the cross-node cases exercise actual distribution rather than a mock. The
%%% barrier in each node-shutdown case is explicit: capture the pid while the node
%%% is up, wait for a real `nodedown', only then run the check.
-module(mongoose_c2s_replaced_probe_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log_helper.hrl").

-define(HOST_TYPE, <<"localhost">>).
-define(PROBE, mongoose_c2s_replaced_probe).

%% Everything the sanitised warning is allowed to carry. The old warning dumped
%% the whole #c2s_data{} record; nothing connection-scoped may come back.
-define(ALLOWED_WARNING_KEYS,
        [what, text, state_name, replaced_pids, replaced_nodes,
         alive_count, dead_count, unreachable_count, timeout_count,
         error_count, skipped_count]).

all() ->
    [{group, same_node},
     {group, cross_node}].

groups() ->
    [{same_node, [], [alive_local_process_is_reported_alive,
                      exited_local_process_is_reported_dead,
                      warning_is_sanitised_and_carries_counts,
                      confirmed_exit_is_not_logged,
                      broken_input_cannot_take_the_connection_down]},
     {cross_node, [], [alive_remote_process_is_reported_alive,
                       exited_remote_process_is_reported_dead,
                       node_down_after_capture_is_unreachable_not_dead,
                       node_down_after_capture_does_not_stop_the_c2s_callback,
                       node_down_after_capture_does_not_storm_the_logs,
                       node_restarted_after_capture_is_answered_by_the_new_node,
                       unresponsive_node_times_out_within_the_budget,
                       mixed_batch_shares_one_budget,
                       oversized_batch_is_capped,
                       probe_leaves_no_background_work_behind]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    mongoose_config:set_opts(#{default_server_name => <<"localhost">>,
                               default_server_domain => <<"localhost">>,
                               language => <<"en">>}),
    ensure_distributed(),
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, warning),
    %% Only this module talks at debug level, so the assertions below see the
    %% quiet branch without the rest of the system flooding the handler.
    ok = logger:set_module_level(?PROBE, debug),
    log_helper:set_up(),
    [{logger_primary_config, LoggerConfig} | Config].

end_per_suite(Config) ->
    log_helper:tear_down(),
    mongoose_config:erase_opts(),
    logger:unset_module_level(?PROBE),
    logger:set_primary_config(?config(logger_primary_config, Config)),
    ok.

init_per_testcase(_CaseName, Config) ->
    flush_logs(),
    log_helper:subscribe(),
    Config.

end_per_testcase(_CaseName, _Config) ->
    log_helper:unsubscribe(),
    flush_logs(),
    ok.

%%--------------------------------------------------------------------
%% Same node
%%--------------------------------------------------------------------

%% A replaced session that is still running is the whole point of the diagnostic.
alive_local_process_is_reported_alive(_Config) ->
    Pid = alive_pid(),
    ?assertEqual([{Pid, alive}], ?PROBE:probe([Pid])).

exited_local_process_is_reported_dead(_Config) ->
    Pid = exited_pid(),
    ?assertEqual([{Pid, dead}], ?PROBE:probe([Pid])).

warning_is_sanitised_and_carries_counts(_Config) ->
    Pid = alive_pid(),

    ok = ?PROBE:verify([Pid], session_established),

    {warning, Log} = ?receiveLog(warning, #{what := c2s_replaced_wait_timeout}),
    ?assertMatch(#{alive_count := 1, dead_count := 0, unreachable_count := 0,
                   timeout_count := 0, error_count := 0, skipped_count := 0,
                   state_name := session_established}, Log),
    ?assertEqual([Pid], maps:get(replaced_pids, Log)),
    ?assertEqual([node()], maps:get(replaced_nodes, Log)),
    %% No connection state, no acc, no packets - and nothing else we did not plan.
    ?assertEqual([], lists:sort(maps:keys(Log)) -- lists:sort(?ALLOWED_WARNING_KEYS)).

%% Old sessions that exited on time are the normal case and stay silent.
confirmed_exit_is_not_logged(_Config) ->
    ok = ?PROBE:verify([exited_pid()], session_established),
    ?assertNoLog(warning, #{what := _}),
    ?assertNoLog(info, #{what := _}),
    ?assertNoLog(debug, #{what := _}).

%% The diagnostic is wrapped: even a bug inside it cannot reach the state machine.
broken_input_cannot_take_the_connection_down(_Config) ->
    NotAPid = list_to_atom("definitely_not_a_pid"),

    ?assertEqual(ok, ?PROBE:verify([NotAPid], session_established)),

    {info, Log} = ?receiveLog(info, #{what := c2s_replaced_probe_failed}),
    ?assertMatch(#{class := error, reason := badarg}, Log).

%%--------------------------------------------------------------------
%% Cross node - real Erlang nodes, started and stopped for real
%%--------------------------------------------------------------------

alive_remote_process_is_reported_alive(_Config) ->
    {Peer, Node} = start_peer(),
    try
        Pid = remote_alive_pid(Node),
        ?assertEqual([{Pid, alive}], ?PROBE:probe([Pid])),
        ok = ?PROBE:verify([Pid], session_established),
        {warning, Log} = ?receiveLog(warning, #{what := c2s_replaced_wait_timeout}),
        ?assertEqual([Node], maps:get(replaced_nodes, Log))
    after
        stop_peer(Peer, Node)
    end.

exited_remote_process_is_reported_dead(_Config) ->
    {Peer, Node} = start_peer(),
    try
        Pid = remote_exited_pid(Node),
        ?assertEqual([{Pid, dead}], ?PROBE:probe([Pid]))
    after
        stop_peer(Peer, Node)
    end.

%% THE incident. Capture the pid while the node is up, shut the node down, then
%% run the check - the exact order that used to raise case_clause.
node_down_after_capture_is_unreachable_not_dead(_Config) ->
    {Peer, Node} = start_peer(),
    Pid = remote_alive_pid(Node),
    %% Barrier 1: the pid was captured while the node was still serving it.
    ?assertEqual([{Pid, alive}], ?PROBE:probe([Pid])),
    %% Barrier 2: the node is really gone before the check runs.
    stop_peer(Peer, Node),

    %% This is literally what the old code fed into a case with two clauses.
    ?assertEqual({badrpc, nodedown}, rpc:call(Node, erlang, is_process_alive, [Pid])),

    %% Unreachable is not evidence of death, and it is not an error either.
    ?assertEqual([{Pid, unreachable}], ?PROBE:probe([Pid])).

%% The production stack frame: gen_statem callback -> handle_timeout -> the check.
node_down_after_capture_does_not_stop_the_c2s_callback(_Config) ->
    {Peer, Node} = start_peer(),
    Pid = remote_alive_pid(Node),
    stop_peer(Peer, Node),

    Data = mongoose_c2s:create_data(#{host_type => ?HOST_TYPE,
                                      jid => jid:from_binary(<<"alice@localhost/res">>)}),
    ?assertEqual(keep_state_and_data,
                 mongoose_c2s:handle_event({timeout, replaced_wait_timeout}, [Pid],
                                           session_established, Data)).

%% A rolling shutdown replaces thousands of sessions at once. The expected
%% outcome must not produce a warning or an info line for every one of them.
node_down_after_capture_does_not_storm_the_logs(_Config) ->
    {Peer, Node} = start_peer(),
    Pid = remote_alive_pid(Node),
    stop_peer(Peer, Node),

    ok = ?PROBE:verify([Pid], session_established),

    ?assertNoLog(warning, #{what := _}),
    ?assertNoLog(info, #{what := _}),
    {debug, Log} = ?receiveLog(debug, #{what := c2s_replaced_probe_inconclusive}),
    ?assertMatch(#{alive_count := 0, dead_count := 0, unreachable_count := 1,
                   timeout_count := 0, error_count := 0}, Log),
    ?assertEqual([Node], maps:get(replaced_nodes, Log)).

%% A node that comes back under the same name answers for itself: the old
%% incarnation's process is genuinely gone, and nothing crashes on the way.
node_restarted_after_capture_is_answered_by_the_new_node(_Config) ->
    Name = list_to_atom("mongoose_probe_restart_" ++ os:getpid()),
    {Peer1, Node} = start_peer(#{name => Name}),
    Pid = remote_alive_pid(Node),
    stop_peer(Peer1, Node),
    {Peer2, Node} = start_peer(#{name => Name}),
    try
        pong = net_adm:ping(Node),
        ?assertEqual([{Pid, dead}], ?PROBE:probe([Pid]))
    after
        stop_peer(Peer2, Node)
    end.

%% A node that is connected but does not answer must cost one bounded wait, and
%% must never be reported as "the process is gone".
unresponsive_node_times_out_within_the_budget(_Config) ->
    Budget = 300,
    StarveMs = 2000,
    {Peer, Node} = start_peer(#{args => ["+S", "1"]}),
    try
        Pid = remote_alive_pid(Node),
        starve(Node, StarveMs),

        {Elapsed, Results} = timed(fun() -> ?PROBE:probe([Pid], #{budget => Budget}) end),

        ?assertEqual([{Pid, timeout}], Results),
        ?assert(Elapsed >= Budget),
        ?assert(Elapsed < Budget + 1000),
        timer:sleep(StarveMs)
    after
        stop_peer(Peer, Node)
    end.

%% One budget for the whole batch. Two unresponsive nodes must not cost two
%% timeouts, and the local pids must still be classified correctly.
mixed_batch_shares_one_budget(_Config) ->
    Budget = 400,
    StarveMs = 3000,
    {Peer1, Node1} = start_peer(#{args => ["+S", "1"]}),
    {Peer2, Node2} = start_peer(#{args => ["+S", "1"]}),
    try
        Local = alive_pid(),
        Gone = exited_pid(),
        Remote1 = remote_alive_pid(Node1),
        Remote2 = remote_alive_pid(Node2),
        starve(Node1, StarveMs),
        starve(Node2, StarveMs),

        {Elapsed, Results} =
            timed(fun() -> ?PROBE:probe([Local, Gone, Remote1, Remote2], #{budget => Budget}) end),

        ?assertEqual(alive, status_of(Local, Results)),
        ?assertEqual(dead, status_of(Gone, Results)),
        ?assertEqual(timeout, status_of(Remote1, Results)),
        ?assertEqual(timeout, status_of(Remote2, Results)),
        %% The point of the budget: not Budget per pid, not Budget per node.
        ?assert(Elapsed < 2 * Budget),
        timer:sleep(StarveMs)
    after
        stop_peer(Peer1, Node1),
        stop_peer(Peer2, Node2)
    end.

oversized_batch_is_capped(_Config) ->
    {Peer, Node} = start_peer(),
    try
        Pids = [remote_alive_pid(Node) || _ <- lists:seq(1, 3)],

        Results = ?PROBE:probe(Pids, #{max_remote => 1}),

        ?assertEqual(3, length(Results)),
        ?assertEqual(1, count_status(alive, Results)),
        ?assertEqual(2, count_status(skipped, Results))
    after
        stop_peer(Peer, Node)
    end.

%% "The connection closed while the diagnostic was running": killing the caller
%% must leave nothing behind - no spawned worker, no lingering remote process.
probe_leaves_no_background_work_behind(_Config) ->
    StarveMs = 2000,
    {Peer, Node} = start_peer(#{args => ["+S", "1"]}),
    try
        Pid = remote_alive_pid(Node),
        starve(Node, StarveMs),
        Caller = spawn(fun() -> ?PROBE:verify([Pid], session_established) end),
        Ref = erlang:monitor(process, Caller),
        timer:sleep(100),
        %% The diagnostic is still in flight - this is the "connection closed
        %% while we were probing" case.
        ?assert(erlang:is_process_alive(Caller)),

        exit(Caller, kill),

        receive {'DOWN', Ref, process, Caller, _Reason} -> ok
        after 5000 -> ct:fail(caller_did_not_die) end,
        timer:sleep(100),
        ?assertEqual([], probe_processes(node())),
        %% Let the remote node recover and check it kept nothing either.
        timer:sleep(StarveMs),
        ?assertEqual([], erpc:call(Node, ?MODULE, probe_processes, [Node], 5000)),
        ?assertEqual([], erpc:call(Node, ?MODULE, map_processes, [], 5000))
    after
        stop_peer(Peer, Node)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

ensure_distributed() ->
    case is_alive() of
        true ->
            ok;
        false ->
            Name = list_to_atom("mongoose_c2s_probe_ct_" ++ os:getpid()),
            {ok, _} = net_kernel:start(Name, #{name_domain => shortnames}),
            ok
    end.

start_peer() ->
    start_peer(#{}).

start_peer(Opts) ->
    Name = maps:get(name, Opts, peer:random_name(?MODULE)),
    %% The CT node runs with an explicit cookie (see rebar.config), which a peer
    %% would not pick up from ~/.erlang.cookie.
    Args = maps:get(args, Opts, []) ++
        ["-setcookie", atom_to_list(erlang:get_cookie()), "-pa", suite_code_dir()],
    {ok, Peer, Node} = peer:start_link(#{name => Name, args => Args, wait_boot => 30000}),
    {Peer, Node}.

%% Stop for real and wait for the real nodedown - no sleeps, no polling.
stop_peer(Peer, Node) ->
    monitor_node(Node, true),
    catch peer:stop(Peer),
    receive {nodedown, Node} -> ok
    after 30000 -> ct:fail({node_did_not_go_down, Node}) end,
    monitor_node(Node, false),
    ok.

%% code:which/1 answers `cover_compiled' under coverage, so ask the code path.
suite_code_dir() ->
    filename:dirname(code:where_is_file(atom_to_list(?MODULE) ++ ".beam")).

alive_pid() ->
    spawn(fun() -> receive stop -> ok end end).

exited_pid() ->
    Pid = spawn(fun() -> ok end),
    wait_until_dead(Pid),
    Pid.

remote_alive_pid(Node) ->
    erpc:call(Node, erlang, spawn, [timer, sleep, [infinity]], 30000).

remote_exited_pid(Node) ->
    Pid = erpc:call(Node, erlang, spawn, [erlang, self, []], 30000),
    wait_until_dead(Pid),
    Pid.

wait_until_dead(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> ok
    after 30000 -> ct:fail({process_still_alive, Pid}) end.

%% Occupy the peer's only scheduler at max priority: it stays connected but
%% cannot run the process erpc spawns there, which is a real rpc timeout.
starve(Node, Ms) ->
    {module, ?MODULE} = erpc:call(Node, code, ensure_loaded, [?MODULE], 30000),
    ?assertEqual(1, erpc:call(Node, erlang, system_info, [schedulers_online], 30000)),
    Busy = spawn_opt(Node, ?MODULE, busy, [self(), Ms], [{priority, max}]),
    %% Barrier: the probe below must meet a node that is already busy.
    receive {busy_started, Busy} -> ok
    after 30000 -> ct:fail({peer_not_busy, Node}) end,
    Busy.

busy(ReportTo, Ms) ->
    ReportTo ! {busy_started, self()},
    busy_until(erlang:monotonic_time(millisecond) + Ms).

busy_until(Deadline) ->
    case erlang:monotonic_time(millisecond) < Deadline of
        true -> busy_until(Deadline);
        false -> ok
    end.

timed(Fun) ->
    T0 = erlang:monotonic_time(millisecond),
    Result = Fun(),
    {erlang:monotonic_time(millisecond) - T0, Result}.

status_of(Pid, Results) ->
    proplists:get_value(Pid, Results).

count_status(Status, Results) ->
    length([Pid || {Pid, S} <- Results, S =:= Status]).

%% Called both locally and on the peer node.
probe_processes(_Node) ->
    [Pid || Pid <- erlang:processes(), runs_module(Pid, mongoose_c2s_replaced_probe)].

map_processes() ->
    [Pid || Pid <- erlang:processes(), runs_function(Pid, {lists, map, 2})].

runs_module(Pid, Module) ->
    case erlang:process_info(Pid, [initial_call, current_function]) of
        [{initial_call, {Module, _, _}}, _] -> true;
        [_, {current_function, {Module, _, _}}] -> true;
        _ -> false
    end.

runs_function(Pid, MFA) ->
    case erlang:process_info(Pid, current_function) of
        {current_function, MFA} -> true;
        _ -> false
    end.

flush_logs() ->
    receive {log, _} -> flush_logs()
    after 0 -> ok end.
