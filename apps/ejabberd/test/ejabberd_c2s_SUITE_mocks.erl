-module(ejabberd_c2s_SUITE_mocks).
-export([setup/0, teardown/0]).

setup() ->
    meck:new(ejabberd_sm),
    meck:expect(ejabberd_sm, close_session,
                fun(_SID, _User, _Server, _Resource) -> ok end),

    meck:new(ejabberd_socket),
    meck:expect(ejabberd_socket, close,
                fun(_) -> ok end),
    meck:expect(ejabberd_socket, send, fun(_,_) -> ok end),
    meck:expect(ejabberd_socket, peername,
                fun(_) -> {ok, {{127,0,0,0}, 50001}}  end),
    meck:expect(ejabberd_socket, monitor,
                fun(_) -> ok  end),

    meck:new(ejabberd_hooks),
    meck:expect(ejabberd_hooks, run, fun(_,_) -> ok end),
    meck:expect(ejabberd_hooks, run, fun(_,_,_) -> ok end),
    meck:expect(ejabberd_hooks, run_fold,
                fun(privacy_check_packet, _, _, _) -> allow end),
    meck:expect(ejabberd_hooks, run_fold,
                fun(check_bl_c2s, _, _) -> false end),

    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
                fun(max_fsm_queue) -> 100 end),
    meck:expect(ejabberd_config, get_global_option,
                fun(hosts) -> [<<"localhost">>] end),

    meck:new(randoms),
    meck:expect(randoms, get_string,
                fun() -> integer_to_list(random:uniform(10000)) end).

teardown() ->
    meck:unload(ejabberd_config),
    meck:unload(ejabberd_hooks),
    meck:unload(ejabberd_socket),
    meck:unload(ejabberd_sm),
    meck:unload(randoms).
