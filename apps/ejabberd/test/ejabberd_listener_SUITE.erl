-module(ejabberd_listener_SUITE).

-compile([export_all]).

all() ->
    [tcp_socket_is_started_with_options].


tcp_socket_is_started_with_options(_C) ->

    meck:new(gen_tcp, [unstick, passthrough]),
    meck:expect(gen_tcp, listen, fun(Port, Opts) ->
                                         meck:passthrough([Port, Opts])
                                 end),

    OverrideBacklog = {backlog, 50},
    {ok, _Pid} = listener_started([OverrideBacklog]),

    [{_Pid, {gen_tcp, listen, [_, Opts]}, _Result}] =  meck:history(gen_tcp),

    DefaultBacklog = {backlog, 100},
    [DefaultBacklog, OverrideBacklog] = proplists:lookup_all(backlog, Opts),

    meck:unload(gen_tcp).

listener_started(Opts) ->
    ets:new(listen_sockets, [named_table, public]),
    proc_lib:start_link(ejabberd_listener, init, [tcp_port_ip(), ?MODULE, Opts]).

tcp_port_ip() ->
    {1805, {0,0,0,0}, tcp}.
