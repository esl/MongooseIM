-module(ejabberd_s2s_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).


all() -> gen_groups().

gen_groups() -> [s2s_in_start_stop_test, s2s_out_start_stop_test].

init_per_suite(C) ->
    application:start(p1_stringprep),
    application:start(mnesia),
    application:ensure_all_started(lager),
    start_supervisors(C).

init_per_testcase(_T, C) ->
    set_meck(C),
    C.

end_per_testcase(_T, C) ->
    unset_meck(C),
    C.

end_per_suite(C) ->
    application:stop(p1_stringrpep),
    application:stop(mnesia),
    C.

s2s_in_start_stop_test(_) ->
    given_s2s_in_started(),

    when_s2s_is_stopped(1805),

    %% then
    ok.

s2s_out_start_stop_test(_) ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(ejabberd_s2s_out, x),
    {ok, Pid} = given_s2s_out_started(),
    ct:print("~p", [erlang:process_info(Pid)]),
    timer:sleep(10000),
    true = erlang:is_process_alive(Pid).

send_message(_C) ->
    given_s2s_in_started(),
    FromBin = <<"ala@localhost/asd">>,
    From = jid(FromBin),
    ToBin = <<"bob@bob.com/xyz">>,
    To = jid(ToBin),
    Packet = #xmlel{name = <<"message">>,
                    attrs = [{<<"from">>, From},
                             {<<"to">>, To}],
                    children = [{xmlcdata, <<"Hi!">>}]},
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tp(ejabberd_s2s_out, start, x),
    ok = ejabberd_s2s:route(From, To, Packet).

given_s2s_in_started() ->
    create_s2s().

given_s2s_out_started() ->
    {ok, Pid} = ejabberd_s2s_out:start(<<"localhost">>, <<"bob.com">>, {new, randoms:get_string()}),
    ok = ejabberd_s2s_out:start_connection(Pid),
    {ok, Pid}.

when_s2s_is_stopped(Pid) ->
    stop_s2s(Pid).


create_s2s() ->
    ejabberd_listener:add_listener(1805, ejabberd_s2s_in, s2s_default_opts()).

s2s_default_opts() ->
    [{shaper, s2s_shaper},
     {max_stanza_size, 131072}].

stop_s2s(Port) ->
    ejabberd_listener:delete_listener(Port, ejabberd_s2s_in).

jid(Str) ->
    jlib:binary_to_jid(Str).

set_meck(_C) ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
                fun(listen) -> undefined;
                   (max_fsm_queue) -> 1000;
                   (s2s_use_starttls) -> false;
                   (s2s_certfile) -> undefined;
                   (s2s_ciphers) -> undefined;
                   ({route_subdomains, _}) -> false;
                   ({s2s_default_policy, <<"localhost">>}) -> allow;
                   ({{s2s_host, <<"bob.com">>}, <<"localhost">>}) -> allow;
                   ({s2s_addr, <<"bob.com">>}) -> {127, 0, 0, 1};
                   (outgoing_s2s_port) -> 5269;
                   (s2s_dns_options) -> undefined;
                   (outgoing_s2s_options) -> undefined

                end),
    meck:expect(ejabberd_config, add_local_option,
                fun(listen, _) -> ok end),
    meck:expect(ejabberd_config, get_global_option,
                fun(hosts) -> [<<"localhost">>]
                end),
    meck:new(acl),
    meck:expect(acl, match_rule,
                fun(_, max_s2s_connections, _) ->
                        1;
                   (_, max_s2s_connections_per_node, _) ->
                        1
                end),

    meck:new(ejabberd_hooks),
    meck:expect(ejabberd_hooks, run_fold,
                fun(s2s_allow_host, _, _, _) -> allow end),
    meck:expect(ejabberd_hooks, run_fold,
                fun(filter_packet, FromToPacket, _) -> FromToPacket;
                   (find_s2s_brdige, undefined, _) -> undefined
                end),
    meck:expect(ejabberd_hooks, run,
                fun(s2s_send_packet, _, _) -> ok end).

unset_meck(_C) ->
    meck:unload(ejabberd_config),
    meck:unload(acl),
    meck:unload(ejabberd_hooks).

start_supervisors(C) ->
    F = fun() ->
                meck:new(ejabberd_commands),
                meck:expect(ejabberd_commands, register_commands,
                            fun(_) -> ok end),
                mim_ct_sup:start_link(ejabberd_sup),
                ListnerSpec = {ejabberd_listener,
                                {ejabberd_listener, start_link, []},
                                permanent,
                                infinity,
                                supervisor,
                                [ejabberd_listener]},
                S2S = {ejabberd_s2s,
                       {ejabberd_s2s, start_link, []},
                       permanent,
                       brutal_kill,
                       worker,
                       [ejabberd_s2s]},
                Randoms = {randoms,
                           {randoms, start_link, []},
                           permanent,
                           brutal_kill,
                           worker,
                           [randoms]},
                S2SOut = {ejabberd_s2s_out_sup,
                          {ejabberd_tmp_sup, start_link,
                           [ejabberd_s2s_out_sup, ejabberd_s2s_out]},
                          permanent,
                          infinity,
                          supervisor,
                          [ejabberd_tmp_sup]},

                {ok, _} = supervisor:start_child(ejabberd_sup, ListnerSpec),
                {ok, _} = supervisor:start_child(ejabberd_sup, S2S),
                {ok, _} = supervisor:start_child(ejabberd_sup, S2SOut),
                {ok, _} = supervisor:start_child(ejabberd_sup, Randoms),

                receive
                    stop ->
                        ok
                end
        end,
    Pid = spawn(F),
    [{helper_proc, Pid} | C].
