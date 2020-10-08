-module(mod_bosh_SUITE).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-define(PORT, 5280).
-define(IP, {127, 0, 0, 1}).
-define(CUSTOM_HEADERS,
        [{<<"strict-transport-security">>, <<"max-age=31536000; includeSubDomains">>},
         {<<"access-control-allow-origin">>, <<"*">>}]).
-define(BOSH_BIND_PATH, "/http-bind").
-define(HANDSHAKE_TIMEOUT, 3000).
-define(eq(E, I), ?assertEqual(E, I)).
-define(FAST_PING_RATE, 500).
-define(NEW_TIMEOUT, 1200).
%The timeout is long enough to pass all test cases for ping interval settings
%using NEW_TIMEOUT value. In these tests we wait for at most 2 pings.
%The 300ms is just an additional overhead
-define(IDLE_TIMEOUT, ?NEW_TIMEOUT * 2 + 300).

all() ->
    custom_headers_tests().

custom_headers_tests() ->
    [verify_custom_headers].

init_per_suite(C) ->
    setup(),
    C.

end_per_suite(_) ->
    teardown(),
    ok.

init_per_testcase(_, C) ->
    C.

end_per_testcase(_, C) ->
    C.


setup() ->
    meck:unload(),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jid),
    meck:new(supervisor, [unstick, passthrough, no_link]),
    meck:new(gen_mod,[unstick, passthrough, no_link]),
    %% Set ping rate
    meck:expect(supervisor, start_child,
                fun(ejabberd_listeners, {_, {_, start_link, [_]}, transient,
                                         infinity, worker, [_]}) -> {ok, self()};
                   (A,B) -> meck:passthrough([A,B])
                end),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    %% Start websocket cowboy listening

    Opts = [{num_acceptors, 10},
            {max_connections, 1024},
            {modules, [{"_", "/http-bind", mod_bosh, [{custom_headers, ?CUSTOM_HEADERS}]}]}],
    ejabberd_cowboy:start_listener({?PORT, ?IP, tcp}, Opts).

teardown() ->
    meck:unload(),
    cowboy:stop_listener(ejabberd_cowboy:ref({?PORT, ?IP, tcp})),
    application:stop(cowboy),
    %% Do not stop jid, Erlang 21 does not like to reload nifs
    ok.

verify_custom_headers(_Config) ->
    application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open("127.0.0.1", ?PORT),
    {ok, http} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, ?BOSH_BIND_PATH),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, _Status, _Headers} ->
            ct:fail(no_bosh_http_response);
        {response, nofin, 200, Headers} ->
            {ok, _Body} = gun:await_body(ConnPid, StreamRef),
            RcvdHdrsSet = sets:from_list(Headers),
            CustomHdrsSet = sets:from_list(?CUSTOM_HEADERS),
            ?assert(sets:is_subset(CustomHdrsSet, RcvdHdrsSet))
    end,
    application:stop(gun).
