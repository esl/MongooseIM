%%% @doc Instrumentation tests for probes defined in mongoose_system_probes.erl

-module(system_probes_SUITE).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [host_type/1]).

all() ->
    [{group, cets},
     {group, mnesia},
     {group, system}].

groups() ->
    [{cets, [], cets_cases()},
     {mnesia, [], mnesia_cases()},
     {system, [parallel], system_cases()}].

cets_cases() ->
    [cets_info].

mnesia_cases() ->
    [mnesia_info].

system_cases() ->
    [system_up_time,
     system_tcp_ports,
     system_process_queue_lengths,
     system_info,
     system_memory,
     system_dist_data].

init_per_suite(Config) ->
    mongoose_helper:inject_module(?MODULE),
    Config1 = mongoose_helper:backup_and_set_config_option(
                Config, [instrumentation, probe_interval], 1),
    restart(mongoose_system_probes),
    instrument_helper:start(instrument_helper:declared_events(mongoose_system_probes, [])),
    require_rpc_nodes([mim, mim2]) ++ Config1.

end_per_suite(Config) ->
    mongoose_helper:restore_config_option(Config, [instrumentation, probe_interval]),
    restart(mongoose_system_probes),
    instrument_helper:stop().

init_per_group(DBGroup, Config) when DBGroup =:= cets;
                                     DBGroup =:= mnesia ->
    case rpc(mim(), mongoose_config, get_opt, [internal_databases]) of
        #{DBGroup := _} ->
            Config;
        #{} ->
            {skip, "Required internal database is not configured"}
    end;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(mnesia_info, Config) ->
    %% Make sure mim2 does not remain in the cluster in case of a failed test
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    Config;
end_per_testcase(_TestCase, Config) ->
    Config.

cets_info(_Config) ->
    instrument_helper:wait_and_assert_new(cets_info, #{}, fun check_cets_info/1).

%% Check that values are integers and there are no unknown fields
check_cets_info(Info) ->
    lists:all(fun(Name) -> is_integer(maps:get(Name, Info)) end, cets_metrics_names())
        andalso #{} =:= maps:without(cets_metrics_names(), Info).

cets_metrics_names() ->
    [available_nodes,
     unavailable_nodes,
     joined_nodes,
     discovered_nodes,
     discovery_works,
     remote_nodes_without_disco,
     remote_nodes_with_unknown_tables,
     remote_unknown_tables,
     remote_nodes_with_missing_tables,
     remote_missing_tables,
     conflict_nodes,
     conflict_tables].

mnesia_info(Config) ->
    wait_for_mnesia_info(#{db_nodes => 1, running_db_nodes => 1}),
    distributed_helper:add_node_to_cluster(mim2(), Config),
    wait_for_mnesia_info(#{db_nodes => 2, running_db_nodes => 2}),
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    wait_for_mnesia_info(#{db_nodes => 1, running_db_nodes => 1}).

wait_for_mnesia_info(Info) ->
    instrument_helper:wait_and_assert_new(mnesia_info, #{}, fun(I) -> I =:= Info end).

check_mnesia_info(#{db_nodes := DbNodes, running_db_nodes := RunningDbNodes}) ->
    DbNodes >= RunningDbNodes andalso RunningDbNodes >= 1.

system_up_time(_Config) ->
    #{seconds := UpTime} = rpc(mim(), mongoose_system_probes, probe, [system_up_time, #{}]),
    ?assert(UpTime > 0),

    % Uptime should increase second by second, but probing might cause skipping a value
    instrument_helper:wait_and_assert(
      system_up_time, #{},
      fun(#{seconds := NewUpTime}) -> NewUpTime - UpTime =:= 1 orelse NewUpTime - UpTime =:= 2 end).

system_tcp_ports(_Config) ->
    % A system without any users should have less than 100 TCP ports open
    instrument_helper:wait_and_assert(
      system_tcp_ports, #{},
      fun(#{count := Count}) -> Count > 0 andalso Count < 100 end),

    % Open additional ports, and check the increased value
    Pid = rpc(mim(), ?MODULE, spawn_process_with_ports, []),
    instrument_helper:wait_and_assert_new(
      system_tcp_ports, #{},
      fun(#{count := Count}) -> Count >= 1000 end),

    % Close additional ports, and wait for the value to decrease
    Pid ! stop,
    instrument_helper:wait_and_assert(
      system_tcp_ports, #{},
      fun(#{count := Count}) -> Count > 0 andalso Count < 100 end).

system_process_queue_lengths(_Config) ->
    % A system without any users shouldn't have more than 1000 messages accumulated
    instrument_helper:wait_and_assert(
      system_process_queue_lengths, #{},
      fun(#{total := Total}) -> Total >= 0 andalso Total < 1000 end),

    % Pid will accumulate more messages
    Pid = rpc(mim(), ?MODULE, spawn_process_with_queue, []),
    instrument_helper:wait_and_assert_new(
      system_process_queue_lengths, #{},
      fun(#{total := Total}) -> Total >= 10000 end),

    % Stop Pid, and wait for the value to decrease
    Pid ! stop,
    instrument_helper:wait_and_assert_new(
      system_process_queue_lengths, #{},
      fun(#{total := Total}) -> Total >= 0 andalso Total < 1000 end).

system_info(_Config) ->
    instrument_helper:wait_and_assert_new(system_info, #{}, fun check_system_info/1).

%% There should be at least one process, port and ETS, and limits shouldn't be reached.
check_system_info(#{port_count := PortCount, port_limit := PortLimit,
                    process_count := ProcessCount, process_limit := ProcessLimit,
                    ets_count := ETSCount, ets_limit := ETSLimit})
  when PortCount > 0, PortLimit > PortCount,
       ProcessCount > 0, ProcessLimit > ProcessCount,
       ETSCount > 0, ETSLimit > ETSCount ->
    true.

system_memory(_Config) ->
    instrument_helper:wait_and_assert_new(system_memory, #{}, fun check_system_memory/1).

%% There should be all types of memory consumption, with the constraints explained
%% in the docs for erlang:memory/0.
check_system_memory(
  #{total := Total, atom := Atom, atom_used := AtomUsed, binary := Binary, code := Code, ets := ETS,
    processes := Processes, processes_used := ProcessesUsed, system := System})
  when Total =:= Processes + System, Atom >= AtomUsed, AtomUsed > 0, Binary > 0, Code > 0, ETS > 0,
       Processes >= ProcessesUsed, System >= Atom + Binary + ETS ->
    true.

system_dist_data(_Config) ->
    instrument_helper:wait_and_assert_new(system_dist_data, #{}, fun check_system_dist_data/1).

%% There should be data sent and received already, and at least one connection (RPC)
check_system_dist_data(
  #{connections := Connections, recv_oct := RecvOct, recv_cnt := RecvCnt, recv_max := RecvMax,
    send_oct := SendOct, send_cnt := SendCnt, send_max := SendMax, send_pend := SendPend})
  when Connections >= 1, RecvOct > RecvCnt, RecvCnt > 0, RecvMax > 0,
       SendOct > SendCnt, SendCnt > 0, SendMax > 0, SendPend >= 0 ->
    true.

restart(Module) ->
    rpc(mim(), Module, stop, []),
    rpc(mim(), Module, start, []).

%% Functions injected to MIM

spawn_process_with_queue() ->
    spawn(fun accumulate_messages/0).

accumulate_messages() ->
    Self = self(),
    [Self ! {msg, I} || I <- lists:seq(1, 10000)],
    receive stop -> ok end.

spawn_process_with_ports() ->
    spawn(fun open_ports/0).

open_ports() ->
    Results = [gen_tcp:listen(0, []) || _ <- lists:seq(1, 1000)],
    Ports = lists:map(fun({ok, Port}) -> Port end, Results),
    receive stop -> ok end,
    lists:foreach(fun gen_tcp:close/1, Ports).
