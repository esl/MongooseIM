-module(ejabberd_c2s_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/src/ejabberd_c2s.hrl").
-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).


all() -> [ c2s_start_stop_test ].

init_per_suite(C) ->
    stringprep:start(),
    xml:start(),
    C.

c2s_start_stop_test(_) ->
    {ok, C2SPid} = given_c2s_started(),

    when_c2s_is_stopped(C2SPid),

    %% then
    ?eq(false, erlang:is_process_alive(C2SPid)).


given_c2s_started() ->
    create_c2s().

when_c2s_is_stopped(Pid) ->
    stop_c2s(Pid).


create_c2s() ->
    ejabberd_c2s_SUITE_mocks:setup(),
    ejabberd_c2s:start_link({ejabberd_socket, self()}, c2s_default_opts()).

c2s_default_opts() ->
    [{access, c2s},
     {shaper, c2s_shaper},
     {max_stanza_size, 65536}].

stop_c2s(C2SPid) when is_pid(C2SPid) ->
    _R = ejabberd_c2s:stop(C2SPid),
    ejabberd_c2s_SUITE_mocks:teardown().

jid(Str) ->
    jlib:binary_to_jid(Str).

