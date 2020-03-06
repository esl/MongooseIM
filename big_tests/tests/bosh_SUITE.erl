%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(bosh_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(INACTIVITY, 2). %% seconds
-define(MAX_WAIT, 5). %% seconds
-define(INVALID_RID_OFFSET, 999).

all() ->
    [
     {group, essential},

     {group, chat},
     {group, time},
     {group, acks},

     {group, essential_https},
     {group, chat_https},
     {group, interleave_requests_statem}
     ].

groups() ->
    G = [
         {essential, [shuffle], essential_test_cases()},
         {essential_https, [shuffle], essential_test_cases()},
         {chat, [shuffle], chat_test_cases()},
         {chat_https, [shuffle], chat_test_cases()},
         {time, [parallel], time_test_cases()},
         {acks, [shuffle], acks_test_cases()},
         {interleave_requests_statem, [parallel], [interleave_requests_statem]}
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

essential_test_cases() ->
    [create_and_terminate_session,
     accept_higher_hold_value,
     do_not_accept_0_hold_value,
     options_request,
     get_request,
     post_empty_body,
     put_request].

chat_test_cases() ->
    [
     interleave_requests,
     simple_chat,
     cdata_escape_chat,
     escape_attr_chat,
     cant_send_invalid_rid,
     multiple_stanzas,
     namespace,
     stream_error
    ].

time_test_cases() ->
    [disconnect_inactive,
     connection_interrupted,
     interrupt_long_poll_is_activity,
     reply_on_pause,
     cant_pause_for_too_long,
     pause_request_is_activity,
     reply_in_time
    ].

acks_test_cases() ->
    [
     server_acks,
     force_report,
     force_retransmission,
     force_cache_trimming
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite([{escalus_user_db, {module, escalus_ejabberd}} | Config]).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(time, Config) ->
    NewConfig = escalus_ejabberd:setup_option(max_wait(), Config),
    escalus_ejabberd:setup_option(inactivity(), NewConfig);
init_per_group(essential, Config) ->
    [{user, carol} | Config];
init_per_group(essential_https, Config) ->
    [{user, carol_s} | Config];
init_per_group(chat_https, Config) ->
    Config1 = escalus:create_users(Config, escalus:get_users([carol, carol_s, geralt, alice])),
    [{user, carol_s} | Config1];
init_per_group(_GroupName, Config) ->
    Config1 = escalus:create_users(Config, escalus:get_users([carol, carol_s, geralt, alice])),
    [{user, carol} | Config1].

end_per_group(time, Config) ->
    NewConfig = escalus_ejabberd:reset_option(max_wait(), Config),
    escalus_ejabberd:reset_option(inactivity(), NewConfig);
end_per_group(GroupName, Config)
    when GroupName =:= essential; GroupName =:= essential_https ->
    Config;
end_per_group(_GroupName, Config) ->
    R = escalus:delete_users(Config, escalus:get_users([carol, carol_s, geralt, alice])),
    mongoose_helper:clear_last_activity(Config, carol),
    Config,
    R.

init_per_testcase(server_acks = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(server_acks_opt(), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(server_acks = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(server_acks_opt(), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

create_and_terminate_session(Config) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    CarolSpec = proplists:get_value(?config(user, Config), NamedSpecs),
    Conn = escalus_connection:connect(CarolSpec),

    %% Assert there are no BOSH sessions on the server.
    [] = get_bosh_sessions(),

    Domain = ct:get_config({hosts, mim, domain}),
    Body = escalus_bosh:session_creation_body(get_bosh_rid(Conn), Domain),
    ok = bosh_send_raw(Conn, Body),
    escalus_connection:get_stanza(Conn, session_creation_response),

    %% Assert that a BOSH session was created.
    [_] = get_bosh_sessions(),

    Sid = get_bosh_sid(Conn),
    Terminate = escalus_bosh:session_termination_body(get_bosh_rid(Conn), Sid),
    ok = bosh_send_raw(Conn, Terminate),

    %% Assert the session was terminated.
    wait_for_zero_bosh_sessions().

accept_higher_hold_value(Config) ->
    #xmlel{attrs = RespAttrs} = send_specific_hold(Config, <<"2">>),
    {<<"hold">>, <<"1">>} = lists:keyfind(<<"hold">>, 1, RespAttrs).

do_not_accept_0_hold_value(Config) ->
    #xmlel{attrs = RespAttrs} = send_specific_hold(Config, <<"0">>),
    {<<"type">>, <<"terminate">>} = lists:keyfind(<<"type">>, 1, RespAttrs).


send_specific_hold(Config, HoldValue) ->
    {Server, Path, Client} = get_fusco_connection(Config),

    Rid = rand:uniform(1000000),
    Body0 = escalus_bosh:session_creation_body(2, <<"1.0">>, <<"en">>, Rid, Server, nil),
    #xmlel{attrs = Attrs0} = Body0,
    Attrs = lists:keyreplace(<<"hold">>, 1, Attrs0, {<<"hold">>, HoldValue}),
    Body = Body0#xmlel{attrs = Attrs},

    Result = fusco_request(Client, <<"POST">>, Path, exml:to_iolist(Body)),
    {{<<"200">>, <<"OK">>}, _Headers, RespBody, _, _} = Result,

    {ok, #xmlel{attrs = RespAttrs} = Resp} = exml:parse(RespBody),
    case lists:keyfind(<<"sid">>, 1, RespAttrs) of
        {<<"sid">>, SID} ->
            TerminateBody = escalus_bosh:session_termination_body(Rid + 1, SID),
            fusco_request(Client, <<"POST">>, Path, exml:to_iolist(TerminateBody));
        _ ->
            skip
    end,
    fusco_cp:stop(Client),
    Resp.

fusco_request(Client, Method, Path, Body) ->
    fusco_request(Client, Method, Path, Body, []).

fusco_request(Client, Method, Path, Body, HeadersIn) ->
    Headers = [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>} | HeadersIn],
    case fusco_cp:request(Client, Path, Method, Headers, Body, 2, 5000) of
        {ok, Result} ->
            Result;
        Other ->
            ct:fail(#{issue => http_request_failed,
                      reason => Other,
                      client => Client,
                      method => Method,
                      path => Path,
                      body => Body,
                      headers => HeadersIn})
    end.


options_request(Config) ->
    {Server, Path, Client} = get_fusco_connection(Config),
    Result = fusco_request(Client, <<"OPTIONS">>, Path, <<>>, [{<<"Origin">>, Server}]),
    fusco_cp:stop(Client),
    {{<<"200">>, <<"OK">>}, Headers, <<>>, _, _} = Result,
    <<"1728000">> = proplists:get_value(<<"access-control-max-age">>, Headers),
    <<"content-type">> = proplists:get_value(<<"access-control-allow-headers">>, Headers),
    <<"POST, OPTIONS, GET">> = proplists:get_value(<<"access-control-allow-methods">>, Headers),
    Server = proplists:get_value(<<"access-control-allow-origin">>, Headers).

get_request(Config) ->
    {_Server, Path, Client} = get_fusco_connection(Config),
    Result = fusco_request(Client, <<"GET">>, Path, <<>>),
    fusco_cp:stop(Client),
    {{<<"200">>, <<"OK">>}, _, _, _, _} = Result.

put_request(Config) ->
    {_Server, Path, Client} = get_fusco_connection(Config),
    Result = fusco_request(Client, <<"PUT">>, Path, <<"not allowed body">>),
    fusco_cp:stop(Client),
    {{<<"405">>, <<"Method Not Allowed">>}, _, _, _, _} = Result.

post_empty_body(Config) ->
    {_Server, Path, Client} = get_fusco_connection(Config),
    Result = fusco_request(Client, <<"POST">>, Path, <<>>),
    fusco_cp:stop(Client),
    {{<<"400">>, <<"Bad Request">>}, _, _, _, _} = Result.

get_fusco_connection(Config) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    CarolSpec = proplists:get_value(?config(user, Config), NamedSpecs),
    Server = proplists:get_value(server, CarolSpec),
    Path = proplists:get_value(path, CarolSpec),
    Port = proplists:get_value(port, CarolSpec),
    UseSSL = proplists:get_value(ssl, CarolSpec, false),
    {ok, Client} = fusco_cp:start_link({binary_to_list(Server), Port, UseSSL}, [], 1),
    {Server, Path, Client}.

stream_error(Config) ->
    escalus:story(
      Config, [{?config(user, Config), 1}],
      fun(Carol) ->
              %% Send a stanza with invalid 'from'
              %% attribute to trigger a stream error from
              %% the server.
              Domain = domain(),
              BadMessage = escalus_stanza:chat(
                             <<"not_carol@", Domain/binary>>,
                             <<"geralt@", Domain/binary>>,
                             <<"I am not Carol">>),
              escalus_client:send(Carol, BadMessage),
              escalus:assert(is_stream_error, [<<"invalid-from">>, <<>>],
                             escalus_client:wait_for_stanza(Carol)),
              %% connection should be closed, let's wait
              escalus_client:wait_for_close(Config, Carol, timer:seconds(1))
      end).

interleave_requests(Config) ->
    escalus:story(Config, [{geralt, 1}], fun(Geralt) ->

        Carol = start_client(Config, ?config(user, Config), <<"bosh">>),
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),

        Msg1 = <<"1st!">>,
        Msg2 = <<"2nd!">>,
        Msg3 = <<"3rd!">>,
        Msg4 = <<"4th!">>,

        send_message_with_rid(Carol, Geralt, Rid + 1, Sid, Msg2),
        send_message_with_rid(Carol, Geralt, Rid, Sid, Msg1),

        send_message_with_rid(Carol, Geralt, Rid + 2,   Sid, Msg3),
        send_message_with_rid(Carol, Geralt, Rid + 3,   Sid, Msg4),

        escalus:assert(is_chat_message, [Msg1],
                       escalus_client:wait_for_stanza(Geralt)),
        escalus:assert(is_chat_message, [Msg2],
                       escalus_client:wait_for_stanza(Geralt)),
        escalus:assert(is_chat_message, [Msg3],
                       escalus_client:wait_for_stanza(Geralt)),
        escalus:assert(is_chat_message, [Msg4],
                       escalus_client:wait_for_stanza(Geralt)),

        true = is_bosh_connected(Carol)
    end).

interleave_requests_statem(Config) ->
    %% cancel 30-seconds timetrap, start 5-minutes one
    ct:timetrap({minutes, 5}),
    true = bosh_interleave_reqs:test([{user, carol} | Config]).

interleave_requests_statem_https(Config) ->
    interleave_requests_statem([{user, carol_s} | Config]).

send_message_with_rid(From, To, Rid, Sid, Msg) ->
    Empty = escalus_bosh:empty_body(Rid, Sid),
    Chat = Empty#xmlel{
             children = [escalus_stanza:chat_to(To, Msg)]},
    bosh_send_raw(From, Chat).


simple_chat(Config) ->
    escalus:story(Config, [{?config(user, Config), 1}, {geralt, 1}], fun(Carol, Geralt) ->

        escalus_client:send(Carol, escalus_stanza:chat_to(Geralt, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus_client:wait_for_stanza(Geralt)),

        escalus_client:send(Geralt,
                            escalus_stanza:chat_to(Carol, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>],
                       escalus_client:wait_for_stanza(Carol))

        end).

cdata_escape_chat(Config) ->
    escalus:story(Config, [{?config(user, Config), 1}, {geralt, 1}], fun(Carol, Geralt) ->
        special_chars_helper:check_cdata_from_to(Carol, Carol, <<"Hi! & < > ">>),
        special_chars_helper:check_cdata_from_to(Geralt, Carol, <<"Hi there! & < > ">>)

    end).

escape_attr_chat(Config) ->
    escalus:story(Config, [{?config(user, Config), 1}, {geralt, 1}], fun(Carol, Geralt) ->
        special_chars_helper:check_attr_from_to(Carol, Geralt),
        special_chars_helper:check_attr_from_to(Geralt, Carol)
    end).

cant_send_invalid_rid(Config) ->
    escalus:story(Config, [{?config(user, Config), 1}], fun(Carol) ->
        %% ct:pal("This test will leave invalid rid, session not found"
        %%        " errors in the server log~n"),

        %% NOTICE 1
        %% This test will provoke the server to log the following message:
        %%
        %% mod_bosh_socket:handle_stream_event:401
        %% invalid rid XXX, expected YYY, difference ?INVALID_RID_OFFSET:

        %% NOTICE 2
        %% Escalus will try to close the session under test when the story
        %% completes. This will leave the following message in the log:
        %%
        %% mod_bosh:forward_body:265 session not found!
        
        %% NOTICE 3
        %% We enable quickfail mode, because sometimes request with invalid RID
        %% arrives before empty body req. with valid RID, so server returns an error
        %% only for the first req. and escalus_bosh in normal mode would get stuck
        %% since it wants to maintain order according to RIDs

        escalus_bosh:set_quickfail(Carol, true),

        InvalidRid = get_bosh_rid(Carol) + ?INVALID_RID_OFFSET,
        Sid = get_bosh_sid(Carol),
        Empty = escalus_bosh:empty_body(InvalidRid, Sid),
        bosh_send_raw(Carol, Empty),

        escalus:assert(is_stream_end, escalus:wait_for_stanza(Carol)),
        escalus_client:wait_for_close(Config, Carol, timer:seconds(1)),
        {ok, false} = wait_for_session_close(Sid)

        end).

multiple_stanzas(Config) ->
    escalus:story(Config, [{?config(user, Config), 1}, {geralt, 1}, {alice, 1}],
                  fun(Carol, Geralt, Alice) ->
                %% send a multiple stanza
                Server = escalus_client:server(Carol),
                Stanza1 = escalus_stanza:chat_to(Geralt, <<"Hello">>),
                Stanza2 = escalus_stanza:chat_to(Alice, <<"Hello">>),
                Stanza3 = escalus_stanza:service_discovery(Server),

                RID = get_bosh_rid(Carol),
                SID = get_bosh_sid(Carol),
                Body = escalus_bosh:empty_body(RID, SID),
                Stanza = Body#xmlel{children =
                                          [Stanza1, Stanza2, Stanza1, Stanza3]},
                bosh_send_raw(Carol, Stanza),

                %% check whether each of stanzas has been processed correctly
                escalus:assert(is_chat_message, [<<"Hello">>],
                               escalus_client:wait_for_stanza(Geralt)),
                escalus:assert(is_chat_message, [<<"Hello">>],
                               escalus_client:wait_for_stanza(Alice)),
                escalus:assert(is_chat_message, [<<"Hello">>],
                               escalus_client:wait_for_stanza(Geralt)),
                escalus:assert(is_iq_result, escalus:wait_for_stanza(Carol))
        end).

namespace(Config) ->
    escalus:story(Config, [{?config(user, Config), 1}, {geralt, 1}],
        fun(Carol, Geralt) ->
            %% send a multiple stanza
            Server = escalus_client:server(Carol),

            Stanza1 = escalus_stanza:service_discovery(Server),
            Stanza2 = escalus_stanza:chat_to(Carol, <<"Hello">>),
            Stanza3 = escalus_stanza:presence_direct(Carol, <<"available">>),

            RID = get_bosh_rid(Carol),
            SID = get_bosh_sid(Carol),
            Body = escalus_bosh:empty_body(RID, SID),
            Stanza = Body#xmlel{children=[Stanza1]},
            bosh_send_raw(Carol, Stanza),

            IQResp = escalus:wait_for_stanza(Carol),

            escalus:assert(is_iq, [<<"result">>], IQResp),
            escalus:assert(has_ns, [<<"jabber:client">>], IQResp),

            escalus_client:send(Geralt, Stanza2),
            escalus_client:send(Geralt, Stanza3),

            Message = escalus:wait_for_stanza(Carol),
            escalus:assert(is_chat_message, Message),
            escalus:assert(has_ns, [<<"jabber:client">>], Message),

            Presence  = escalus:wait_for_stanza(Carol),
            escalus:assert(is_presence, Presence),
            escalus:assert(has_ns, [<<"jabber:client">>], Presence)
        end).

disconnect_inactive(Config) ->
    escalus:fresh_story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        Sid = get_bosh_sid(Carol),
        {_, Sid, CarolSessionPid} = get_bosh_session(Sid),
        %% Don't send new long-polling requests waiting for server push.
        set_keepalive(Carol, false),

        %% Make Carol receive using the last remaining connection.
        escalus_client:send(Geralt,
                            escalus_stanza:chat_to(Carol, <<"Hello!">>)),

        escalus:assert(is_chat_message, [<<"Hello!">>],
                       escalus_client:wait_for_stanza(Carol)),

        %% Ensure all connections for Carol have been closed.
        [] = get_handlers(CarolSessionPid),

        %% Wait for disconnection because of inactivity timeout.
        %% Assert Carol has been disconnected due to inactivity.
        wait_for_session_close(Sid),
        %% We don't need to close the session in escalus_bosh:stop/1
        escalus_client:kill_connection(Config, Carol)

        end).

connection_interrupted(Config) ->
    escalus:fresh_story(Config, [{carol, 1}], fun(Carol) ->

        %% Sanity check - there should be one BOSH session belonging
        %% to Carol.
        %% Turn off Escalus auto-reply, so that inactivity is triggered.
        set_keepalive(Carol, false),

        Sid = get_bosh_sid(Carol),

        %% Terminate the connection, but don't notify the server.
        escalus_client:kill_connection(Config, Carol),

        %% Assert Carol has not been disconnected yet.
        timer:sleep(100),
        true = is_session_alive(Sid),

        %% Wait for disconnection because of inactivity timeout.
        %% Keep in mind this only works due to the max_wait also being lowered.
        %% In other words, wait timeout must happen, so that there are
        %% no requests held by the server for inactivity to cause disconnection.
        %% Assert Carol has been disconnected due to inactivity.
        wait_for_session_close(Sid, timer:seconds(?INACTIVITY) + timer:seconds(?MAX_WAIT))
        end).

%% Ensure that a new request replacing an existing long-poll does not start the
%% inactivity timer.
interrupt_long_poll_is_activity(ConfigIn) ->
    Config = escalus_users:update_userspec(ConfigIn, carol, bosh_wait, 10),

    escalus:fresh_story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        %% Sanity check - there should be one BOSH session belonging
        %% to Carol and one handler for Carol.
        Sid = get_bosh_sid(Carol),
        {_, _, CarolSessionPid} = get_bosh_session(Sid),
        {ok, 1} = wait_for_handler(CarolSessionPid, 1),

        %% Send a message.  A new connection should be established, and
        %% the existing long-poll connection should be closed.
        escalus_client:send(Carol,
                            escalus_stanza:chat_to(Geralt, <<"Hello!">>)),

        %% Wait until after the inactivity timeout (which should be less than
        %% the BOSH wait timeout).
        timer:sleep(2 * timer:seconds(?INACTIVITY)),
              
        %% No disconnection should have occurred.
        escalus_assert:has_no_stanzas(Carol),
        true = is_session_alive(Sid),
        {ok, 1} = wait_for_handler(CarolSessionPid, 1)

        end).

reply_on_pause(Config) ->
    escalus:fresh_story(Config, [{carol, 1}], fun(Carol) ->

        Sid = get_bosh_sid(Carol),
        {_, _, CarolSessionPid} = get_bosh_session(Sid),
        set_keepalive(Carol, false),

        %% Sanity check - there should be one handler for Carol.
        {ok, 1} = wait_for_handler(CarolSessionPid, 1),

        pause(Carol, 10),

        %% There should be no handlers for Carol,
        %% but the session should be alive.
        true = is_session_alive(Sid),
        0 = length(get_handlers(CarolSessionPid)),
        0 = get_bosh_requests(Carol)

        end).

cant_pause_for_too_long(Config) ->
    escalus:fresh_story(Config, [{carol, 1}], fun(Carol) ->

        Sid = get_bosh_sid(Carol),
        {_, _, CarolSessionPid} = get_bosh_session(Sid),
        set_keepalive(Carol, false),

        %% Sanity check - there should be one handler for Carol.
        {ok, 1} = wait_for_handler(CarolSessionPid, 1),

        pause(Carol, 10000),

        escalus:assert(is_stream_end, escalus:wait_for_stanza(Carol)),
        escalus_client:wait_for_close(Config, Carol, timer:seconds(1)),
        false = is_session_alive(Sid)

        end).

%% Ensure that a pause request causes inactivity timer cancellation.
pause_request_is_activity(Config) ->
    escalus:fresh_story(Config, [{carol, 1}], fun(Carol) ->

        Sid = get_bosh_sid(Carol),
        {_, _, CarolSessionPid} = get_bosh_session(Sid),
        set_keepalive(Carol, false),

        %% Sanity check - there should be one handler for Carol.
        {ok, 1} = wait_for_handler(CarolSessionPid, 1),

        %% Wait most of the allowed inactivity interval.
        timer:sleep(timer:seconds(?INACTIVITY - 1)),

        %% This should cancel the inactivity timer.
        pause(Carol, 10),
        timer:sleep(timer:seconds(?INACTIVITY - 1)),
        %% No disconnection should've occured.
        escalus_assert:has_no_stanzas(Carol),
        true = is_session_alive(Sid)

        end).

reply_in_time(ConfigIn) ->
    Config = escalus_users:update_userspec(ConfigIn, carol, bosh_wait, 1),
    escalus:fresh_story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        Wait = proplists:get_value(bosh_wait,
                                   escalus_users:get_userspec(Config, carol)),

        %% Don't send new long-polling requests waiting for server push.
        set_keepalive(Carol, false),

        %% Make Carol receive using the last remaining connection.
        escalus_client:send(Geralt,
                            escalus_stanza:chat_to(Carol, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>],
                       escalus_client:wait_for_stanza(Carol)),

        %% Sanity check - there should be no awaiting handlers.
        {_, _, CarolSessionPid} = get_bosh_session(get_bosh_sid(Carol)),
        0 = length(get_handlers(CarolSessionPid)),

        %% Send a single request and assert it's registered by server.
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),
        Empty = escalus_bosh:empty_body(Rid, Sid),
        bosh_send_raw(Carol, Empty),
        timer:sleep(100),
        {ok, 1} = wait_for_handler(CarolSessionPid, 1),

        %% Assert the server has responded to that request.
        wait_for_handler(CarolSessionPid, 0, timer:seconds(Wait) + 100)
        end).

server_acks(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->
        bosh_set_active(Carol, false),
        ExpectedRid = list_to_binary(integer_to_list(get_bosh_rid(Carol))),
        escalus_client:send(Carol, escalus_stanza:chat_to(Geralt, <<"1st!">>)),
        escalus_client:send(Carol, escalus_stanza:chat_to(Geralt, <<"2nd!">>)),
        timer:sleep(200),

        All = recv_all(Carol),
        ExpectedRid = exml_query:attr(hd(All), <<"ack">>),
        bosh_set_active(Carol, true)

        end).

force_report(Config) ->

    %% Carol stores current Rid1
    %% Carol sends msg1
    %% Carol sends msg2
    %% Geralt sends a reply to msg1
    %% Geralt sends a reply to msg2
    %% Carol recvs a reply to msg1
    %% Carol recvs a reply to msg2
    %% Carol sends an ack with Rid1 on empty BOSH wrapper
    %% server sends a report

    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        StaleRid = get_bosh_rid(Carol),
        escalus_client:send(Carol, chat_to(Geralt, <<"1st msg">>)),
        escalus_client:send(Carol, chat_to(Geralt, <<"2nd msg">>)),
        wait_for_stanzas(Geralt, 2),
        escalus_client:send(Geralt, chat_to(Carol, <<"1st rep">>)),
        escalus_client:send(Geralt, chat_to(Carol, <<"2nd rep">>)),
        escalus:assert(is_chat_message, [<<"1st rep">>],
                       wait_for_stanza(Carol)),
        escalus:assert(is_chat_message, [<<"2nd rep">>],
                       wait_for_stanza(Carol)),

        %% Turn on client acknowledgement checking for Carol
        {_, _, CarolSessionPid} = get_bosh_session(get_bosh_sid(Carol)),
        set_client_acks(CarolSessionPid, true),

        bosh_set_active(Carol, false),
        %% Send ack with StaleRid
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),
        BodyWithAck = ack_body(escalus_bosh:empty_body(Rid, Sid), StaleRid),
        bosh_send_raw(Carol, BodyWithAck),

        %% Turn off client acknowledgement checking - don't cause server error
        %% on subsequent requests without 'ack' attribute.
        set_client_acks(CarolSessionPid, false),

        %% Expect a server report
        timer:sleep(100),
        MaybeReport = bosh_recv(Carol),
        escalus:assert(is_bosh_report, [StaleRid+1], MaybeReport),
        bosh_set_active(Carol, true)

        end).

force_retransmission(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        %% `send_raw` must be used as the exact request structure
        %% is needed later for retransmission.
        %% Hence, construct the request manually.
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),
        Empty = escalus_bosh:empty_body(Rid, Sid),
        Chat = Empty#xmlel{
                children = [escalus_stanza:chat_to(Geralt, <<"1st msg!">>)]},

        %% Send msg, recv msg, send reply, recv reply.
        %% This synchronous sequence sets up the server
        %% to have the reply for Chat cached.
        bosh_send_raw(Carol, Chat),
        escalus:assert(is_chat_message, [<<"1st msg!">>],
                       wait_for_stanza(Geralt)),
        escalus_client:send(Geralt, chat_to(Carol, <<"1st rep!">>)),
        ChatResponse = wait_for_stanza(Carol),
        escalus:assert(is_chat_message, [<<"1st rep!">>], ChatResponse),

        %% Resend msg.
        bosh_resend_raw(Carol, Chat),

        %% Recv same reply again.
        ChatResponse = wait_for_stanza(Carol)

        end).

force_cache_trimming(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        Sid = get_bosh_sid(Carol),

        {_, _, CarolSessionPid} = get_bosh_session(Sid),
        set_client_acks(CarolSessionPid, true),

        %% Ack now
        Rid1 = get_bosh_rid(Carol),
        Ack1 = ack_body(escalus_bosh:empty_body(Rid1, Sid), Rid1-1),
        bosh_send_raw(Carol, Ack1),

        %% Exchange 2 messages
        Rid2 = get_bosh_rid(Carol),
        Chat = (escalus_bosh:empty_body(Rid2, Sid))#xmlel{
                children = [escalus_stanza:chat_to(Geralt, <<"1st msg!">>)]},
        bosh_send_raw(Carol, Chat),
        escalus:assert(is_chat_message, [<<"1st msg!">>],
                       wait_for_stanza(Geralt)),
        escalus_client:send(Geralt, chat_to(Carol, <<"1st rep!">>)),
        ChatResponse = wait_for_stanza(Carol),
        escalus:assert(is_chat_message, [<<"1st rep!">>], ChatResponse),

        %% Ack/Chat again
        Rid3 = get_bosh_rid(Carol),
        AckedChat = (ack_body(escalus_bosh:empty_body(Rid3, Sid), Rid2))#xmlel{
                children = [escalus_stanza:chat_to(Geralt, <<"2nd msg!">>)]},
        bosh_send_raw(Carol, AckedChat),
        escalus:assert(is_chat_message, [<<"2nd msg!">>],
                       wait_for_stanza(Geralt)),

        %% The cache should now contain only entries newer than Rid2.
        {_, _, CarolSessionPid} = get_bosh_session(Sid),
        Cache = get_cached_responses(CarolSessionPid),
        true = lists:all(fun({R, _, _}) when R > Rid2 -> true; (_) -> false end,
                         Cache),
        %% Not sure about this one...
        1 = length(Cache),

        set_client_acks(CarolSessionPid, false)

        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_bosh_sessions() ->
    rpc(mim(), mod_bosh_backend, get_sessions, []).

get_bosh_session(Sid) ->
    BoshSessions = get_bosh_sessions(),
    lists:keyfind(Sid, 2, BoshSessions).

get_handlers(BoshSessionPid) ->
    rpc(mim(), mod_bosh_socket, get_handlers, [BoshSessionPid]).

get_bosh_sid(#client{rcv_pid = Pid}) ->
    escalus_bosh:get_sid(Pid).

get_bosh_rid(#client{rcv_pid = Pid}) ->
    escalus_bosh:get_rid(Pid).

set_keepalive(#client{rcv_pid = Pid}, Keepalive) ->
    escalus_bosh:set_keepalive(Pid, Keepalive).

mark_as_terminated(#client{rcv_pid = Pid}) ->
    escalus_bosh:mark_as_terminated(Pid).

pause(#client{rcv_pid = Pid}, Seconds) ->
    escalus_bosh:pause(Pid, Seconds),
    timer:sleep(100).

start_client(Config, User, Res) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    UserSpec = [{keepalive, false} | proplists:get_value(User, NamedSpecs)],
    {ok, Client} = escalus_client:start(Config, UserSpec, Res),
    Client.

bosh_set_active(#client{rcv_pid = Pid}, Value) ->
    escalus_bosh:set_active(Pid, Value).

is_bosh_connected(#client{rcv_pid = Pid}) ->
    escalus_bosh:is_connected(Pid).

get_bosh_requests(#client{rcv_pid = Pid}) ->
    escalus_bosh:get_requests(Pid).

recv_all(Client) ->
    recv_all(bosh_recv(Client), Client, []).

recv_all(empty, _Client, Acc) ->
    lists:reverse(Acc);
recv_all(Element, Client, Acc) ->
    recv_all(bosh_recv(Client), Client, [Element | Acc]).

bosh_recv(#client{rcv_pid = Pid}) ->
    escalus_bosh:recv(Pid).

bosh_send_raw(#client{rcv_pid = Pid}, Body) ->
    escalus_bosh:send_raw(Pid, Body).

bosh_resend_raw(#client{rcv_pid = Pid}, Body) ->
    escalus_bosh:resend_raw(Pid, Body).

chat_to(Client, Content) ->
    escalus_stanza:chat_to(Client, Content).

wait_for_stanzas(Client, Count) ->
    escalus_client:wait_for_stanzas(Client, Count).

wait_for_stanza(Client) ->
    escalus_client:wait_for_stanza(Client).

ack_body(Body, Rid) ->
    Attrs = Body#xmlel.attrs,
    Ack = {<<"ack">>, list_to_binary(integer_to_list(Rid))},
    NewAttrs = lists:keystore(<<"ack">>, 1, Attrs, Ack),
    Body#xmlel{attrs = NewAttrs}.

set_client_acks(SessionPid, Enabled) ->
    rpc(mim(), mod_bosh_socket, set_client_acks, [SessionPid, Enabled]).

get_cached_responses(SessionPid) ->
    rpc(mim(), mod_bosh_socket, get_cached_responses, [SessionPid]).

inactivity() ->
    inactivity(?INACTIVITY).

inactivity(Value) ->
    {inactivity,
     fun() -> rpc(mim(), mod_bosh, get_inactivity, []) end,
     fun(V) -> rpc(mim(), mod_bosh, set_inactivity, [V]) end,
     Value}.

max_wait() ->
    max_wait(?MAX_WAIT).

max_wait(Value) ->
    {max_wait,
     fun() -> rpc(mim(), mod_bosh, get_max_wait, []) end,
     fun(V) -> rpc(mim(), mod_bosh, set_max_wait, [V]) end,
     Value}.

server_acks_opt() ->
    {server_acks,
     fun() -> rpc(mim(), mod_bosh, get_server_acks, []) end,
     fun(V) -> rpc(mim(), mod_bosh, set_server_acks, [V]) end,
     true}.

is_session_alive(Sid) ->
    BoshSessions = get_bosh_sessions(),
    lists:keymember(Sid, 2, BoshSessions).

wait_for_session_close(Sid) ->
    wait_for_session_close(Sid, ?INACTIVITY).

wait_for_session_close(Sid, LeftTime) ->
    mongoose_helper:wait_until(fun() -> is_session_alive(Sid) end, false,
                               #{
                                 time_left => timer:seconds(10),
                                 time_sleep => LeftTime, 
                                 name => is_session_alive
                                }).

wait_for_handler(Pid, Count) ->
    mongoose_helper:wait_until(fun() -> length(get_handlers(Pid)) end, Count,
                                #{   
                                 time_left => timer:seconds(10),
                                 time_sleep => timer:seconds(1),
                                 name => get_handlers
                                }).


wait_for_handler(Pid, Count, LeftTime) ->
    mongoose_helper:wait_until(fun() -> length(get_handlers(Pid)) end, Count, 
                               #{   
                                 time_left => LeftTime,
                                 time_sleep => timer:seconds(1),
                                 name => get_handlers
                                }).

wait_until_user_has_no_stanzas(User) ->
        mongoose_helper:wait_until(fun() -> 
                                       escalus_assert:has_no_stanzas(User) 
                                   end, ok, #{left_time => 2 * timer:seconds(?INACTIVITY)}).

domain() ->
    ct:get_config({hosts, mim, domain}).

wait_for_zero_bosh_sessions() ->
    mongoose_helper:wait_until(fun() ->
                                       length(get_bosh_sessions())
                               end,
                               0,
                               #{name => get_bosh_sessions}).
