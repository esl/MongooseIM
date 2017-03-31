-module(ejabberd_c2s_SUITE_mocks).
-export([setup/0, teardown/0]).

setup() ->
    meck:new(ejabberd_sm),
    meck:expect(ejabberd_sm, close_session,
                fun(_SID, _User, _Server, _Resource, _Reason) -> ok end),

    meck:new(ejabberd_socket),
    meck:expect(ejabberd_socket, close,
                fun(_) -> ok end),
    meck:expect(ejabberd_socket, send, fun(_,_) -> ok end),
    meck:expect(ejabberd_socket, peername,
                fun(_) -> {ok, {{127,0,0,0}, 50001}}  end),
    meck:expect(ejabberd_socket, monitor,
                fun(_) -> ok  end),
    meck:expect(ejabberd_socket, change_shaper, fun(_,_) -> ok end),

    meck:new(ejabberd_hooks),
    meck:expect(ejabberd_hooks, run, fun(_,_) -> ok end),
    meck:expect(ejabberd_hooks, run, fun(_,_,_) -> ok end),
    meck:expect(ejabberd_hooks, run_fold,
                fun(privacy_check_packet, _, _, _) -> allow end),
    meck:expect(ejabberd_hooks, run_fold,
        fun(xmpp_send_element, _, A, _) -> A end),
    meck:expect(ejabberd_hooks, run_fold,
                fun(check_bl_c2s, _, _) -> false end),

    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option, fun default_local_option/1),
    meck:expect(ejabberd_config, get_global_option, fun default_global_option/1),
    meck:expect(acl, match_rule, fun(_,_,_) -> allow end),

    meck:new(randoms),
    meck:expect(randoms, get_string,
                fun() -> "57" end),

    meck:new(mongoose_metrics),
    meck:expect(mongoose_metrics, update, fun (_,_,_) -> ok end).

teardown() ->
    meck:unload().

default_local_option(max_fsm_queue) -> 100.

default_global_option(hosts) ->  [<<"localhost">>];
default_global_option({access,c2s_shaper,global}) ->  [];
default_global_option(language) ->  [<<"en">>].
