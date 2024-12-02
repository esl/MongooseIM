-module(shutdown_SUITE).

-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, rpc/4]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

all() ->
    [{group, main}].

groups() ->
    [{main, [], cases()}].

cases() ->
    [shutdown,
     client_tries_to_connect_before_listener_stop].

init_per_suite(Config) ->
    mongoose_helper:inject_module(?MODULE),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    escalus:create_users(Config, escalus:get_users([geralt_s, alice])),
    Config.

end_per_group(_, Config) ->
    escalus:delete_users(Config, escalus:get_users([geralt_s, alice])),
    Config.

init_per_testcase(shutdown = TC, Config) ->
    logger_ct_backend:start(),
    escalus:init_per_testcase(TC, Config);
init_per_testcase(client_tries_to_connect_before_listener_stop = TC, Config) ->
    escalus:init_per_testcase(TC, Config);
init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(shutdown = TC, Config) ->
    logger_ct_backend:stop(),
    escalus:end_per_testcase(TC, Config);
end_per_testcase(client_tries_to_connect_before_listener_stop = TC, Config) ->
    rpc(mim(), meck, unload, []),
    escalus:end_per_testcase(TC, Config);
end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

shutdown(Config) ->
    UserSpec = escalus_users:get_userspec(Config, geralt_s),
    {ok, Alice, _} = escalus_connection:start(UserSpec),
    logger_ct_backend:capture(error),
    ejabberd_node_utils:restart_application(mongooseim),
    logger_ct_backend:stop_capture(),
    FilterFun = fun(_, Msg) -> re:run(Msg, "event_not_registered") /= nomatch end,
    [] = logger_ct_backend:recv(FilterFun),
    %% Ensure that Alice gets a shutdown stanza
    escalus:assert(is_stream_error, [<<"system-shutdown">>, <<>>],
                   escalus_client:wait_for_stanza(Alice)),
    escalus:assert(is_stream_end, escalus_client:wait_for_stanza(Alice)),
    true = escalus_connection:wait_for_close(Alice, timer:seconds(1)).

client_tries_to_connect_before_listener_stop(Config) ->
    %% Ensures that user would not be able to connect while we are stopping
    %% the listeners and other c2s processes
    UserSpec = escalus_users:get_userspec(Config, geralt_s),
    PortNum = proplists:get_value(port, UserSpec),
    %% Ask MongooseIM to pause the stopping process
    %% so we can check that listeners were suspended correctly
    block_listener(),
    %% Check that the listener is working
    {ok, ConnPort} = gen_tcp:connect("127.0.0.1", PortNum, []),
    %% Trigger the restarting logic in a separate parallel process
    RestPid = restart_application_non_blocking(),
    %% Wait until we are blocked in mongoose_listener:stop/0
    Called = wait_for_called(),
    {error, econnrefused} = gen_tcp:connect("127.0.0.1", PortNum, []),
    %% Resume to stop the listeners
    resume(Called),
    %% Check that the old TCP connections are closed
    receive_tcp_closed(ConnPort),
    %% Wait till mongooseim is fully restarted
    wait_for_down(RestPid).

block_listener() ->
    rpc(mim(), meck, new, [mongoose_listener, [no_link, passthrough]]),
    Pid = self(),
    F = fun() ->
            wait_for_resume(Pid),
            meck:passthrough([])
        end,
    rpc(mim(), meck, expect, [mongoose_listener, stop, F]).

restart_application_non_blocking() ->
    spawn_link(fun() -> ejabberd_node_utils:restart_application(mongooseim) end).

wait_for_called() ->
    receive
        {called, Called} ->
            Called
        after 5000 ->
            error(wait_for_called_timeout)
    end.

%% Blocks the mocked process until resume/1 is called
wait_for_resume(Pid) ->
    MonRef = erlang:monitor(process, Pid),
    Called = {self(), MonRef},
    Pid ! {called, Called},
    receive
        {'DOWN', MonRef, process, _, _} -> ok;
        {resume, MonRef} -> ok
    end.

%% Command to the mocked process to resume the operation
resume({Pid, MonRef}) ->
    Pid ! {resume, MonRef}.

wait_for_down(Pid) ->
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, _, _} -> ok
        after 5000 ->
            ct:pal("wait_for_down current_stacktrace ~p",
                   [rpc:pinfo(Pid, current_stacktrace)]),
            ct:fail(wait_for_down_timeout)
    end.

%% Waits until the TCP socket is closed
receive_tcp_closed(ConnPort) ->
    receive
        {tcp_closed, ConnPort} ->
            ok
        after 5000 ->
            ct:fail(wait_for_tcp_close_timeout)
    end.
