-module(ejabberd_c2s_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/src/ejabberd_c2s.hrl").
-include_lib("exml/include/exml_stream.hrl").
-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(am(E, I), ?assertMatch(E, I)).
-define(ne(E, I), ?assert(E =/= I)).


all() -> [
          c2s_start_stop_test,
          stream_error_when_invalid_domain
         ].

init_per_suite(C) ->
    stringprep:start(),
    application:start(x),
    C.

end_per_suite(C) ->
    C.

init_per_testcase(_TC, C) ->
    ejabberd_c2s_SUITE_mocks:setup(),
    C.

end_per_testcase(_TC, C) ->
    ejabberd_c2s_SUITE_mocks:teardown(),
    C.

c2s_start_stop_test(_) ->
    {ok, C2SPid} = given_c2s_started(),

    when_c2s_is_stopped(C2SPid),

    %% then
    ?eq(false, erlang:is_process_alive(C2SPid)).


stream_error_when_invalid_domain(_) ->
    {ok, C2SPid} = given_c2s_started(),

    C2Sactions = when_stream_is_opened(C2SPid, stream_header(<<"badhost">>)),
    [StreamStart, StreamError, StreamEnd, CloseSocket] = C2Sactions,

    ?am({send,[_P, <<"<?xml version='1.0'?><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='57' from='localhost' xml:lang='en'>">>]},
        StreamStart),
    ?am({send,[_P, <<"<stream:error>",
                      "<host-unknown xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>",
                      "</stream:error>">>]}, StreamError),
    ?am({send,[_P,<<"</stream:stream>">>]}, StreamEnd),
    ?am({close,[_P]}, CloseSocket),
    ok.

when_stream_is_opened(C2SPid, Stanza) ->
    p1_fsm:send_event(C2SPid, Stanza),
    sync_c2s(C2SPid),
    lists:filtermap(filter_calls(ejabberd_socket, [send,close]),
                       meck:history(ejabberd_socket)).

filter_calls(ExpecetdMod, Funs) ->
    fun({_Pid, MFA, Return}) ->
            maybe_extract_function_with_args(MFA, Funs)
    end.

maybe_extract_function_with_args({_Mod, Fun, Args}, List) ->
    case lists:member(Fun, List) of
        true -> {true, {Fun, Args}};
        _ -> false
    end.

sync_c2s(C2SPid) -> catch sys:get_state(C2SPid).

stream_valid_header_response() ->
     <<"<?xml version='1.0'?><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='4436' from='localhost' xml:lang='en'>">>.

stream_header(Domain) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = [{<<"to">>, Domain},
                             {<<"xml:lang">>, <<"en">>},
                             {<<"version">>, <<"1.0">>},
                             {<<"xmlns">>, <<"jabber:client">>},
                             {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>}]}.

given_c2s_started() ->
    create_c2s().

when_c2s_is_stopped(Pid) ->
    stop_c2s(Pid),
    sync_c2s(Pid).


create_c2s() ->
    ejabberd_c2s:start_link({ejabberd_socket, self()}, c2s_default_opts()).

c2s_default_opts() ->
    [{access, c2s},
     {shaper, c2s_shaper},
     {max_stanza_size, 65536}].

stop_c2s(C2SPid) when is_pid(C2SPid) ->
    _R = ejabberd_c2s:stop(C2SPid).

jid(Str) ->
    jid:from_binary(Str).

