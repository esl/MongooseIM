-module(ejabberd_listener_SUITE).

-compile([export_all]).

all() ->
    [tcp_socket_is_started_with_default_backlog,
     tcp_socket_is_started_with_options,
     udp_socket_is_started_with_defaults].

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
    ets:new(listen_sockets, [named_table, public]),
    proc_lib:start_link(ejabberd_listener, init, [tcp_port_ip(), ?MODULE, Opts]).

receiver_started(Opts) ->
    ets:new(listen_sockets, [named_table, public]),
    proc_lib:start_link(ejabberd_listener, init, [udp_port_ip(), ?MODULE, Opts]).

udp_port_ip() ->
    {1805, {0,0,0,0}, udp}.

tcp_port_ip() ->
    {1805, {0,0,0,0}, tcp}.
