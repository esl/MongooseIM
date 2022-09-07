-module(mongoose_mnesia_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

all() -> [{group, cluster}].

groups() ->
    [
        {cluster, [], [dirty_sync_never_returns_if_remote_node_restarts]}
    ].

init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

%% Tests

dirty_sync_never_returns_if_remote_node_restarts(Config) ->
    Node1 = start_node(test1),
    Node2 = start_node(test2),
    ok = rpc:call(Node1, mnesia, start, []),
    ok = rpc:call(Node2, mnesia, start, []),

    % Node2 joins Node1
    {ok, _} = rpc:call(Node2, mnesia, change_config, [extra_db_nodes, [Node1]]),

    {atomic, ok} = rpc:call(Node2, mnesia, create_table, [tab, [{ram_copies, [Node1, Node2]}, {attributes, [id, data]}]]),

    % Pause
    TM2 = rpc:call(Node2, erlang, whereis, [mnesia_tm]),
    true = rpc:block_call(Node2, erlang, suspend_process, [TM2]),

    % This would be waiting for a reply from a suspended mnesia_tm on Node2
    Inserter = spawn(Node1, mnesia, sync_dirty, [fun mnesia:write/1, [{tab, 1, 1}]]),

    timer:sleep(1000),
    Info = rpc:call(Node1, erlang, process_info, [Inserter, current_stacktrace]),
    {current_stacktrace, _} = Info, % still running
    [_] = rpc:call(Node1, ets, tab2list, [tab]),
    [] = rpc:call(Node2, ets, tab2list, [tab]),
    %% Don't allow the process to read db_nodes list yet
    true = rpc:block_call(Node1, erlang, suspend_process, [Inserter]),

    %% Simulate the node crash
    rpc:call(Node2, erlang, exit, [TM2, kill]),

    %% Let Node2 time to stop remaining mnesia processes, so start does not fail
    timer:sleep(1000),

    %% Start the node again
    ok = rpc:call(Node2, mnesia, start, []),
    {ok, _} = rpc:call(Node2, mnesia, change_config, [extra_db_nodes, [Node1]]),
    rpc:call(Node2, mnesia, wait_for_tables, [[tab], infinity]),
    %% The record has been copied from Node1 during the start time
    [_] = rpc:call(Node2, ets, tab2list, [tab]),

    true =rpc:block_call(Node1, erlang, resume_process, [Inserter]),

    %% Enough time for inserter to exit
    timer:sleep(1000),
    {current_stacktrace, Trace} = rpc:call(Node1, erlang, process_info, [Inserter, current_stacktrace]),
    ct:pal("Process ~p is still running, but should not ~p", [Inserter, Trace]).


%% Helpers

start_node(ShortName) ->
    Opts = [{monitor_master, true},
            {boot_timeout, 15}, %% in seconds
            {init_timeout, 10}, %% in seconds
            {startup_timeout, 10}], %% in seconds
    {ok, Node} = ct_slave:start(ShortName, Opts),
    {ok, Dir} = file:get_cwd(),
    ok = rpc:call(Node, file, set_cwd, [Dir]),
    ok = rpc:call(Node, code, add_pathsa, [lists:reverse(code_paths())]),
    Node.

stop_node(Name) ->
    ct_slave:stop(Name).

get_required_config(Key, Config) ->
    case proplists:get_value(Key, Config) of
        undefined ->
            ct:fail({get_required_config_failed, Key});
        Value ->
            Value
    end.

code_paths() ->
    [filename:absname(Path) || Path <- code:get_path()].
