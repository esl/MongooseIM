-module(mongoose_listener_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_helper, [copy/2,
                          data/2]).

-define(DEFAULT_PORTS, [5222, 5280, 5269]).

all() ->
    [tcp_socket_is_started_with_default_backlog,
     tcp_socket_is_started_with_options,
     tcp_socket_supports_proxy_protocol,
     tcp_socket_has_connection_details,
     tcp_socket_supports_proxy_protocol,
     tcp_start_stop_reload
     ].

init_per_testcase(_Case, Config) ->
    meck:new([gen_tcp], [unstick, passthrough]),
    meck:expect(gen_tcp, listen,
                fun(Port, Opts) -> meck:passthrough([Port, Opts]) end),
    Config.

end_per_testcase(_Case, Config) ->
    meck:unload(),
    Config.

init_per_suite(C) ->
    mnesia:start(),
    C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

tcp_socket_is_started_with_default_backlog(_C) ->
    ok = listener_started(#{}),
    [{_Pid, {gen_tcp, listen, [_, Opts]}, _Result}] =  meck:history(gen_tcp),
    100 = proplists:get_value(backlog, Opts).

tcp_socket_is_started_with_options(_C) ->
    ok = listener_started(#{backlog => 50}),
    [{_Pid, {gen_tcp, listen, [_, Opts]}, _Result}] =  meck:history(gen_tcp),
    50 = proplists:get_value(backlog, Opts).

tcp_socket_has_connection_details(_C) ->
    ok = listener_started(#{}),
    {Port, _, _} = tcp_port_ip(),

    meck:new(mongoose_transport),
    TestPid = self(),
    meck:expect(mongoose_transport, accept,
                fun(_Module, Socket, Opts, ConnectionDetails) ->
                        TestPid ! {socket_started, Socket, Opts, ConnectionDetails},
                        ok
                end),

    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    {ok, SrcPort} = inet:port(Socket),

    receive
        {socket_started, _Socket, _Opts, ConnectionDetails} ->
            ?assertEqual(#{proxy => false,
                           src_address => {127, 0, 0, 1},
                           src_port => SrcPort,
                           dest_address => {127, 0, 0, 1},
                           dest_port => Port}, ConnectionDetails)
    after
        5000 ->
            ct:fail(timeout_waiting_for_tcp)
    end.

tcp_socket_supports_proxy_protocol(_C) ->
    ok = listener_started(#{proxy_protocol => true}),

    CommonProxyInfo = #{src_address => {1, 2, 3, 4},
                        src_port => 444,
                        dest_address => {192, 168, 0, 1},
                        dest_port => 443,
                        version => 2},
    RanchProxyInfo = CommonProxyInfo#{command => proxy,
                                      transport_family => ipv4,
                                      transport_protocol => stream},
    {Port, _, _} = tcp_port_ip(),

    meck:new(mongoose_transport),
    TestPid = self(),
    meck:expect(mongoose_transport, accept,
                fun(_Module, Socket, Opts, ConnectionDetails) ->
                        TestPid ! {socket_started, Socket, Opts, ConnectionDetails},
                        ok
                end),

    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    ok = gen_tcp:send(Socket, [ranch_proxy_header:header(RanchProxyInfo)]),

    receive
        {socket_started, _Socket, _Opts, ConnectionDetails} ->
            ?assertEqual(CommonProxyInfo#{proxy => true}, ConnectionDetails)
    after
        5000 ->
            ct:fail(timeout_waiting_for_tcp_with_proxy_protocol)
    end.

listener_started(Opts) ->
    mim_ct_sup:start_link(ejabberd_sup),
    mongoose_listener_sup:start_link(),
    mongoose_listener:start_listener(maps:merge(listener_opts(), Opts)).

tcp_port_ip() ->
    {1805, {0, 0, 0, 0}, tcp}.

listener_opts() ->
    #{module => ?MODULE,
      port => 1805,
      ip_tuple => {0, 0, 0, 0},
      ip_address => "0",
      ip_version => 4,
      proto => tcp,
      num_acceptors => 1,
      backlog => 100,
      proxy_protocol => false}.

tcp_start_stop_reload(C) ->
    %% start server
    copy(data(C, "mongooseim.basic.toml"), data(C, "mongooseim.toml")),
    ejabberd_helper:start_ejabberd_with_config(C, "mongooseim.toml"),
    ?assert(lists:keymember(mongooseim, 1, application:which_applications())),
    %% make sure all ports are open
    lists:map(fun assert_open/1, ?DEFAULT_PORTS),
    %% stop listeners, now they should be closed
    Listeners = mongoose_config:get_opt(listen),
    lists:foreach(fun mongoose_listener:stop_listener/1, Listeners),
    lists:map(fun assert_closed/1, ?DEFAULT_PORTS),
    %% and start them all again
    lists:foreach(fun mongoose_listener:start_listener/1, Listeners),
    lists:map(fun assert_open/1, ?DEFAULT_PORTS),

    %% we want to make sure that connection to an unchanged port survives reload
    UnchPort = 5222,
    {ok, Sock} = gen_tcp:connect("localhost", UnchPort,[{active, false}, {packet, 2}]),
    assert_connected(Sock, UnchPort),

    %% and that to the changed port does too (this is current implementation)
    ChgPort = 5269,
    {ok, Sock2} = gen_tcp:connect("localhost", ChgPort,[{active, false}, {packet, 2}]),
    assert_connected(Sock2, ChgPort),

    ok = mongooseim:stop(),
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

start_listener(Opts) ->
    mongoose_tcp_listener:start_listener(Opts).
