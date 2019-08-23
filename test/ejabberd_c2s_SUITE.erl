-module(ejabberd_c2s_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include("ejabberd_c2s.hrl").
-include_lib("exml/include/exml_stream.hrl").
-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(am(E, I), ?assertMatch(E, I)).
-define(ne(E, I), ?assert(E =/= I)).


all() -> [
          c2s_start_stop_test,
          stream_error_when_invalid_domain,
          session_established,
          send_error_when_waiting,
          c2s_is_killed_when_too_many_messages_in_the_queue
         ].

init_per_suite(C) ->
    ok = stringprep:start(),
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
    ?am({send, [_P,
         <<"<?xml version='1.0'?>",
             "<stream:stream xmlns='jabber:client' ",
             "xmlns:stream='http://etherx.jabber.org/streams' id='57' ",
             "from='localhost' xml:lang='en'>">>
         ]},
        StreamStart),
    ?am({send, [_P,
                <<"<stream:error>",
                "<host-unknown xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>",
                "</stream:error>">>]}, StreamError),
    ?am({send, [_P, <<"</stream:stream>">>]}, StreamEnd),
    ?am({close, [_P]}, CloseSocket),
    ok.

session_established(_) ->
    {ok, C2SPid} = given_c2s_started(),
    change_state_to(session_estasblished, C2SPid),
    ?eq(session_established, getstate(C2SPid)),
    Last = last_stanza(),
    ?eq(final_iq_response(), Last).

send_error_when_waiting(_) ->
    % this is a regression test for #1252 - when c2s is in state
    % wait_for_session_or_sm and it fails to send a message
    % it should be handled properly
    {ok, C2SPid} = given_c2s_started(),
    change_state_to(wait_for_session_or_sm, C2SPid),
    % here we break things to check how c2s will handle error while sending
    % message in this state
    meck:expect(ejabberd_socket, send, fun(_, _El) -> error_error_error  end),
    sync_c2s(C2SPid),
    p1_fsm:send_event(C2SPid, {xmlstreamelement, setsession_stanza()}),
    sync_c2s(C2SPid),
    [Close, StreamEnd, StreamError | _] = lists:reverse(received_stanzas()),
    ?eq(stream_error_response(),
        StreamError),
    ?eq(<<"</stream:stream>">>, StreamEnd),
    ?eq(close, Close),
    ok.

c2s_is_killed_when_too_many_messages_in_the_queue(_) ->
    meck:new(ejabberd_c2s, [passthrough]),
    MaxQueueSize = 50,
    Self = self(),
    %% We simulate a long running event during which
    %% there will be many messages put into C2S process message queue
    meck:expect(ejabberd_c2s, handle_event,
                fun(go_to_sleep, StateName, ProcState) ->
                        Self ! c2s_going_to_sleep,
                        ct:pal("going to sleep"),
                        receive continue -> ok end,
                        {next_state, StateName, ProcState};
                   (Event, StateName, ProcState) ->
                        meck:passthrough([Event, StateName, ProcState])
                end),
    {ok, C2SPid} = given_c2s_started([{max_fsm_queue, MaxQueueSize}]),

    %% We want to monitor the c2s process and not being linked to it
    Ref = erlang:monitor(process, C2SPid),
    erlang:unlink(C2SPid),

    p1_fsm_old:send_all_state_event(C2SPid, go_to_sleep),

    receive c2s_going_to_sleep -> ok end,

    meck:unload(ejabberd_c2s),

    %% We put MaxQueueSize + 1 messages to C2S Process message queue
    %% while it is asleep
    %% The process will be killed when it wakes up and tries to process
    %% next message

    [p1_fsm_old:send_all_state_event(C2SPid, {event, I}) ||
     I <- lists:seq(1, MaxQueueSize + 1)],
    C2SPid ! continue,

    receive
        {'DOWN', Ref, process, C2SPid, {process_limit,{max_queue, AllMessages}}} ->
            ct:pal("C2S dead due to message_queue_length=~p, max allowed was ~p",
                   [AllMessages, MaxQueueSize]);
        Msg ->
            ct:fail("Other msg: ~p", [Msg])
    after timer:seconds(5) ->
              {message_queue_len, N} = process_info(C2SPid, message_queue_len),
              ct:fail("timeout waiting c2s exit, with message_queue_len = ~p", [N])
    end,
    ok.

last_stanza() ->
    [H|_] = lists:reverse(received_stanzas()),
    H.

received_stanzas() ->
    Calls = lists:filtermap(filter_calls(ejabberd_socket, [send, close]),
                            meck:history(ejabberd_socket)),
%%    ct:pal("Calls: ~p", [Calls]),
    lists:map(fun extract_stanza/1, Calls).

extract_stanza({_, [_, S]}) -> S;
extract_stanza({close, _}) -> close.

change_state_to(Target, C2SPid) ->
    Curr = getstate(C2SPid),
    change_state_to(Curr, Target, C2SPid).

change_state_to(T, T, _) ->
    ok;
change_state_to(wait_for_stream, T, C2SPid) ->
    p1_fsm:send_event(C2SPid, stream_header(<<"localhost">>)),
    change_state_to(getstate(C2SPid), T, C2SPid);
change_state_to(wait_for_feature_before_auth, T, C2SPid) ->
    p1_fsm:send_event(C2SPid, {xmlstreamelement, auth_stanza()}),
    change_state_to(getstate(C2SPid), T, C2SPid);
change_state_to(wait_for_feature_after_auth, T, C2SPid) ->
    p1_fsm:send_event(C2SPid, {xmlstreamelement, bind_stanza()}),
    change_state_to(getstate(C2SPid), T, C2SPid);
change_state_to(wait_for_session_or_sm, T, C2SPid) ->
    p1_fsm:send_event(C2SPid, {xmlstreamelement, setsession_stanza()}),
    change_state_to(getstate(C2SPid), T, C2SPid);
change_state_to(_, _, _) ->
    error.

getstate(C2SPid) ->
    State = sync_c2s(C2SPid),
    [_, StateName | _] = State,
    StateName.

when_stream_is_opened(C2SPid, Stanza) ->
    p1_fsm:send_event(C2SPid, Stanza),
    sync_c2s(C2SPid),
    lists:filtermap(filter_calls(ejabberd_socket, [send, close]),
                       meck:history(ejabberd_socket)).

filter_calls(_ExpecetdMod, Funs) ->
    fun({_Pid, MFA, _Return}) ->
            maybe_extract_function_with_args(MFA, Funs)
    end.

maybe_extract_function_with_args({_Mod, Fun, Args}, List) ->
    case lists:member(Fun, List) of
        true -> {true, {Fun, Args}};
        _ -> false
    end.

sync_c2s(C2SPid) -> catch sys:get_state(C2SPid).

stream_valid_header_response() ->
     R = "<?xml version='1.0'?>"
         "<stream:stream xmlns='jabber:client' "
         "xmlns:stream='http://etherx.jabber.org/streams' id='4436' "
         "from='localhost' xml:lang='en'>",
    list_to_binary(R).

stream_header(Domain) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = [{<<"to">>, Domain},
                             {<<"xml:lang">>, <<"en">>},
                             {<<"version">>, <<"1.0">>},
                             {<<"xmlns">>, <<"jabber:client">>},
                             {<<"xmlns:stream">>,
                              <<"http://etherx.jabber.org/streams">>}]}.

auth_stanza() ->
    {xmlel, <<"auth">>,
        [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>},
         {<<"mechanism">>, <<"PLAIN">>}],
        [{xmlcdata, <<"AGFsaWNFOTkuODk3NzMzAG1hdHlncnlzYQ==">>}]}.

bind_stanza() ->
    {xmlel, <<"iq">>,
            [{<<"type">>, <<"set">>}, {<<"id">>, <<"4436">>}],
            [{xmlel, <<"bind">>,
                    [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>}],
                    [{xmlel, <<"resource">>, [], [{xmlcdata, <<"res1">>}]}]}]}.

setsession_stanza() ->
    {xmlel, <<"iq">>,
        [{<<"type">>, <<"set">>}, {<<"id">>, <<"4436">>}],
        [{xmlel, <<"session">>, [{<<"xmlns">>,
                                  <<"urn:ietf:params:xml:ns:xmpp-session">>}],
            []}]}.

given_c2s_started() ->
    given_c2s_started([]).

given_c2s_started(Opts) ->
    ejabberd_c2s:start_link({ejabberd_socket, self()},
                            Opts ++ c2s_default_opts()).

when_c2s_is_stopped(Pid) ->
    stop_c2s(Pid),
    sync_c2s(Pid).

c2s_default_opts() ->
    [{access, c2s},
     {shaper, c2s_shaper},
     {max_stanza_size, 65536}].

stop_c2s(C2SPid) when is_pid(C2SPid) ->
    _R = ejabberd_c2s:stop(C2SPid).

jid(Str) ->
    jid:from_binary(Str).

final_iq_response() ->
    <<"<iq type='result' id='4436'>"
          "<session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>"
      "</iq>">>.

stream_error_response() ->
    <<"<stream:error>",
      "<internal-server-error xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>",
      "</stream:error>">>.

