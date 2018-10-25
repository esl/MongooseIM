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

init_per_testcase(_Case, Config) ->
    meck:new([gen_udp, gen_tcp], [unstick, passthrough]),
    meck:expect(gen_udp, open,
                fun(Port, Opts) -> meck:passthrough([Port, Opts]) end),
    meck:expect(gen_tcp, listen,
                fun(Port, Opts) -> meck:passthrough([Port, Opts]) end),
    Config.

end_per_testcase(_Case, Config) ->
    meck:unload(),
    Config.

init_per_suite(C) ->
   C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

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

listener_started(RawOpts) ->
    {tcp, Opts, SockOpts, Port, IPS} =
        ejabberd_listener:opts_to_listener_args(tcp_port_ip(), RawOpts),
    mongoose_tcp_listener:start_link(tcp_port_ip(), ?MODULE, Opts, SockOpts, Port, IPS).

receiver_started(RawOpts) ->
    ets:new(listen_sockets, [named_table, public]),
    {udp, Opts, SockOpts, Port, IPS} =
        ejabberd_listener:opts_to_listener_args(udp_port_ip(), RawOpts),
    mongoose_udp_listener:start_link(udp_port_ip(), ?MODULE, Opts, SockOpts, Port, IPS).

udp_port_ip() ->
    {1805, {0,0,0,0}, udp}.

tcp_port_ip() ->
    {1805, {0,0,0,0}, tcp}.

tcp_start_stop_reload(C) ->
    %% start server
    copy(data(C, "mongooseim.basic.cfg"), data(C, "mongooseim.cfg")),
    ejabberd_helper:start_ejabberd_with_config(C, "mongooseim.cfg"),
    ?assert(lists:keymember(mongooseim, 1, application:which_applications())),
    %% make sure all ports are open
    lists:map(fun assert_open/1, ?DEFAULT_PORTS),
    %% stop listeners, now they should be closed
    ejabberd_listener:stop_listeners(),
    lists:map(fun assert_closed/1, ?DEFAULT_PORTS),
    %% and start them all again
    ejabberd_listener:start_listeners(),
    lists:map(fun assert_open/1, ?DEFAULT_PORTS),
    %% alternative configuration differs only in that s2s listens on 5296 instea of 5269
    copy(data(C, "mongooseim.alt.cfg"), data(C, "mongooseim.cfg")),
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
    F = fun() ->
              gen_tcp:connect("localhost", PortNo, [{active, false}, {packet, 2}])
        end,
    async_helper:wait_until(F, {error, econnrefused}).


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
