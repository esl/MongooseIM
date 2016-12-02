-module(ejabberd_listener_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_helper, [copy/2,
                          data/2]).

-define(DEFAULT_PORTS, [5222, 5280, 5269]).
-define(ALT_PORTS, [5222, 5280, 5296]). %% yes it is different


all() ->
    [tcp_socket_is_started_with_default_backlog,
     tcp_socket_is_started_with_options,
     udp_socket_is_started_with_defaults,
     tcp_start_stop_reload].

init_per_testcase(udp_socket_is_started_with_defaults, C) ->
    meck:new(gen_udp, [unstick, passthrough]),
    meck:expect(gen_udp, open, fun(Port, Opts) ->
                                       meck:passthrough([Port, Opts])
                               end),
    C;
init_per_testcase(_T, C) ->
    meck:new(gen_tcp, [unstick, passthrough]),
    meck:expect(gen_tcp, listen, fun(Port, Opts) ->
                                         meck:passthrough([Port, Opts])
                                 end),
    C.

end_per_testcase(udp_socket_is_started_with_defaults, C) ->
    meck:unload(gen_udp),
    C;
end_per_testcase(_T, C) ->
    meck:unload(gen_tcp),
    C.

tcp_socket_is_started_with_default_backlog(_C) ->
   {ok, _Pid} = listener_started([]),

   [{_Pid, {gen_tcp, listen, [_, Opts]}, _Result}] =  meck:history(gen_tcp),

    100 = proplists:get_value(backlog, Opts).


tcp_socket_is_started_with_options(_C) ->

    OverrideBacklog = {backlog, 50},
    {ok, _Pid} = listener_started([OverrideBacklog]),

    [{_Pid, {gen_tcp, listen, [_, Opts]}, _Result}] =  meck:history(gen_tcp),

    50 = proplists:get_value(backlog, Opts).


udp_socket_is_started_with_defaults(_C) ->
    {ok, _Pid} = receiver_started([]),

    [{_Pid, {gen_udp, open, [_, Opts]}, _Result}] =  meck:history(gen_udp),

    {0,0,0,0} = proplists:get_value(ip, Opts).

listener_started(Opts) ->
    proc_lib:start_link(ejabberd_listener, init, [tcp_port_ip(), ?MODULE, Opts]).

receiver_started(Opts) ->
    ets:new(listen_sockets, [named_table, public]),
    proc_lib:start_link(ejabberd_listener, init, [udp_port_ip(), ?MODULE, Opts]).

udp_port_ip() ->
    {1805, {0,0,0,0}, udp}.

tcp_port_ip() ->
    {1805, {0,0,0,0}, tcp}.

tcp_start_stop_reload(C) ->
    %% start server
    copy(data(C, "ejabberd.basic.cfg"), data(C, "ejabberd.cfg")),
    ejabberd_helper:start_ejabberd_with_config(C, "ejabberd.cfg"),
    ?assert(lists:keymember(ejabberd, 1, application:which_applications())),
    %% make sure all ports are open
    lists:map(fun assert_open/1, ?DEFAULT_PORTS),
    %% stop listeners, now they should be closed
    ejabberd_listener:stop_listeners(),
    lists:map(fun assert_closed/1, ?DEFAULT_PORTS),
    %% and start them all again
    ejabberd_listener:start_listeners(),
    lists:map(fun assert_open/1, ?DEFAULT_PORTS),
    %% alternative configuration differs only in that s2s listens on 5296 instea of 5269
    copy(data(C, "ejabberd.alt.cfg"), data(C, "ejabberd.cfg")),
    %% we want to make sure that connection to an unchanged port survives reload
    UnchPort = 5222,
    {ok, Sock} = gen_tcp:connect("localhost", UnchPort,[{active, false}, {packet, 2}]),
    assert_connected(Sock, UnchPort),
    %% and that to the change port does too (this is current implementation)
    ChgPort = 5269,
    {ok, Sock2} = gen_tcp:connect("localhost", ChgPort,[{active, false}, {packet, 2}]),
    assert_connected(Sock2, ChgPort),
    ejabberd_config:reload_local(),
    lists:map(fun assert_open/1, ?ALT_PORTS),
    assert_closed(5269),
    assert_connected(Sock, UnchPort),
    assert_connected(Sock2, ChgPort),
    ok = ejabberd_helper:stop_ejabberd(),
    ok.

assert_open(PortNo) ->
    case gen_tcp:connect("localhost", PortNo, [{active, false}, {packet, 2}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ok;
        E ->
            ct:fail("Failed: port ~p is closed, should be open; error was: ~p", [PortNo, E])
    end .

assert_closed(PortNo) ->
    case gen_tcp:connect("localhost", PortNo, [{active, false}, {packet, 2}]) of
        {ok, _Socket} ->
            ct:fail("Failed: port ~p is open, should be closed", [PortNo]);
        {error, econnrefused} ->
            ok;
        E ->
            ct:fail("Error trying port ~p: ~p", [PortNo, E])
    end .

assert_connected(Sock, Port) ->
    case gen_tcp:recv(Sock, 0, 500) of
        {error, timeout} ->
            ok;
        Else ->
            ct:fail("Failed: connection to ~p is broken, error was: ~p", [Port, Else])
    end.

%%assert_disconnected(Sock, Port) ->
%%    case gen_tcp:recv(Sock, 0, 500) of
%%        {error, timeout} ->
%%            ct:fail("Failed: connection to ~p is still open", [Port]),
%%            ok;
%%        _ ->
%%            ok
%%    end.
