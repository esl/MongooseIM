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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(INACTIVITY, 2).
-define(INVALID_RID_OFFSET, 999).

all() ->
    [{group, essential},
     {group, chat},
     {group, time},
     {group, acks}
    ].

groups() ->
    [{essential, [{repeat,10}], [create_and_terminate_session]},
     {chat, [shuffle, {repeat,10}], [interleave_requests,
                                     simple_chat,
                                     cdata_escape_chat,
                                     escape_attr_chat,
                                     cant_send_invalid_rid,
                                     multiple_stanzas,
                                     namespace,
                                     stream_error]},
     {time, [shuffle, {repeat,5}], [disconnect_inactive,
                                    interrupt_long_poll_is_activity,
                                    reply_on_pause,
                                    cant_pause_for_too_long,
                                    pause_request_is_activity,
                                    reply_in_time
                                   ]},
     {acks, [shuffle, {repeat,5}], [server_acks,
                                    force_report,
                                    force_retransmission,
                                    force_cache_trimming]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).


init_per_group(essential, Config) ->
    Config;
init_per_group(chat, Config) ->
    escalus_users:create_users(Config, {by_name, [carol, geralt, alice]});
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(essential, Config) ->
    Config;
end_per_group(chat, Config) ->
    escalus_users:delete_users(Config, {by_name, [carol, geralt, alice]});
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).


init_per_testcase(disconnect_inactive = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(inactivity(), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(interrupt_long_poll_is_activity = CaseName, Config) ->
    InactConfig = escalus_ejabberd:setup_option(inactivity(), Config),
    NewConfig = escalus_users:update_userspec(InactConfig, carol, bosh_wait, 10),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(reply_on_pause = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(inactivity(), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(pause_request_is_activity = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(inactivity(5), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(reply_in_time = CaseName, Config) ->
    InactConfig = escalus_ejabberd:setup_option(inactivity(10), Config),
    NewConfig = escalus_users:update_userspec(InactConfig, carol, bosh_wait, 3),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(server_acks = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(server_acks_opt(), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(disconnect_inactive = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(inactivity(), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(interrupt_long_poll_is_activity = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(inactivity(), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(reply_on_pause = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(inactivity(), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(pause_request_is_activity = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(inactivity(), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(reply_in_time = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(inactivity(), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
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
    CarolSpec = proplists:get_value(carol, NamedSpecs),
    {ok, Conn} = escalus_bosh:connect(CarolSpec),

    %% Assert there are no BOSH sessions on the server.
    0 = length(get_bosh_sessions()),

    Domain = escalus_config:get_config(ejabberd_domain, Config),
    Body = escalus_bosh:session_creation_body(get_bosh_rid(Conn), Domain),
    ok = escalus_bosh:send_raw(Conn, Body),
    escalus_connection:get_stanza(Conn, session_creation_response),

    %% Assert that a BOSH session was created.
    1 = length(get_bosh_sessions()),

    Sid = get_bosh_sid(Conn),
    Terminate = escalus_bosh:session_termination_body(get_bosh_rid(Conn), Sid),
    ok = escalus_bosh:send_raw(Conn, Terminate),

    timer:sleep(100),
    %% Assert the session was terminated.
    0 = length(get_bosh_sessions()).


stream_error(Config) ->
    escalus:story(
      Config, [{carol, 1}],
      fun(Carol) ->
              %% Send a stanza with invalid 'from'
              %% attribute to trigger a stream error from
              %% the server.
              BadMessage = escalus_stanza:chat(
                             <<"not_carol@localhost">>,
                             <<"geralt@localhost">>,
                             <<"I am not Carol">>),
              escalus_client:send(Carol, BadMessage),
              escalus:assert(is_stream_error, [<<"invalid-from">>, <<>>],
                             escalus_client:wait_for_stanza(Carol))
      end).

interleave_requests(Config) ->
    escalus:story(Config, [{geralt, 1}], fun(Geralt) ->

        Carol = start_client(Config, carol, <<"bosh">>),
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),

        Empty2 = escalus_bosh:empty_body(Rid + 1, Sid),
        Chat2 = Empty2#xmlel{
                children = [escalus_stanza:chat_to(Geralt, <<"2nd!">>)]},
        escalus_bosh:send_raw(Carol#client.conn, Chat2),

        Empty1 = escalus_bosh:empty_body(Rid, Sid),
        Chat1 = Empty1#xmlel{
                children = [escalus_stanza:chat_to(Geralt, <<"1st!">>)]},
        escalus_bosh:send_raw(Carol#client.conn, Chat1),

        escalus:assert(is_chat_message, [<<"1st!">>],
                       escalus_client:wait_for_stanza(Geralt)),
        escalus:assert(is_chat_message, [<<"2nd!">>],
                       escalus_client:wait_for_stanza(Geralt))

    end).


simple_chat(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        escalus_client:send(Carol, escalus_stanza:chat_to(Geralt, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus_client:wait_for_stanza(Geralt)),

        escalus_client:send(Geralt,
                            escalus_stanza:chat_to(Carol, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>],
                       escalus_client:wait_for_stanza(Carol))

        end).

cdata_escape_chat(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->
        special_chars_helper:check_cdata_from_to(Carol, Carol, <<"Hi! & < > ">>),
        special_chars_helper:check_cdata_from_to(Geralt, Carol, <<"Hi there! & < > ">>)

    end).

escape_attr_chat(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->
        special_chars_helper:check_attr_from_to(Carol, Geralt),
        special_chars_helper:check_attr_from_to(Geralt, Carol)
    end).

cant_send_invalid_rid(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->
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

        InvalidRid = get_bosh_rid(Carol) + ?INVALID_RID_OFFSET,
        Sid = get_bosh_sid(Carol),
        Empty = escalus_bosh:empty_body(InvalidRid, Sid),
        escalus_bosh:send_raw(Carol#client.conn, Empty),

        escalus:assert(is_stream_end, escalus:wait_for_stanza(Carol)),
        0 = length(get_bosh_sessions())

        end).

multiple_stanzas(Config) ->
    escalus:story(Config, [{carol, 1},{geralt, 1},{alice, 1}],
                  fun(Carol, Geralt, Alice) ->
                %% send a multiple stanza
                Server = escalus_client:server(Carol),
                Stanza1 = escalus_stanza:chat_to(Geralt, <<"Hello">>),
                Stanza2 = escalus_stanza:chat_to(Alice, <<"Hello">>),
                Stanza3 = escalus_stanza:service_discovery(Server),

                RID = escalus_bosh:get_rid(Carol#client.conn),
                SID = escalus_bosh:get_sid(Carol#client.conn),
                Body = escalus_bosh:empty_body(RID, SID),
                Stanza = Body#xmlel{children=[Stanza1,Stanza2,Stanza1,Stanza3]},
                escalus_bosh:send_raw(Carol#client.conn, Stanza),

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
    escalus:story(Config, [{carol, 1},{geralt, 1}],
        fun(Carol, Geralt) ->
            %% send a multiple stanza
            Server = escalus_client:server(Carol),

            Stanza1 = escalus_stanza:service_discovery(Server),
            Stanza2 = escalus_stanza:chat_to(Carol, <<"Hello">>),
            Stanza3 = escalus_stanza:presence_direct(Carol, <<"available">>),

            RID = escalus_bosh:get_rid(Carol#client.conn),
            SID = escalus_bosh:get_sid(Carol#client.conn),
            Body = escalus_bosh:empty_body(RID, SID),
            Stanza = Body#xmlel{children=[Stanza1]},
            escalus_bosh:send_raw(Carol#client.conn, Stanza),

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
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        %% Sanity check - there should be one BOSH session belonging
        %% to Carol.
        1 = length(get_bosh_sessions()),

        %% Don't send new long-polling requests waiting for server push.
        set_keepalive(Carol, false),

        %% Make Carol receive using the last remaining connection.
        escalus_client:send(Geralt,
                            escalus_stanza:chat_to(Carol, <<"Hello!">>)),

        escalus:assert(is_chat_message, [<<"Hello!">>],
                       escalus_client:wait_for_stanza(Carol)),

        %% Ensure all connections for Carol have been closed.
        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        [] = get_handlers(CarolSessionPid),

        %% Wait for disconnection because of inactivity timeout.
        timer:sleep(2 * timer:seconds(?INACTIVITY)),

        %% Assert Carol has been disconnected due to inactivity.
        0 = length(get_bosh_sessions()),

        %% We don't need to close the session in escalus_bosh:stop/1
        mark_as_terminated(Carol)

        end).

%% Ensure that a new request replacing an existing long-poll does not start the
%% inactivity timer.
interrupt_long_poll_is_activity(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        %% Sanity check - there should be one BOSH session belonging
        %% to Carol and one handler for Carol.
        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        1 = length(get_handlers(CarolSessionPid)),

        %% Send a message.  A new connection should be established, and
        %% the existing long-poll connection should be closed.
        escalus_client:send(Carol,
                            escalus_stanza:chat_to(Geralt, <<"Hello!">>)),

        %% Wait until after the inactivity timeout (which should be less than
        %% the BOSH wait timeout).
        timer:sleep(2 * timer:seconds(?INACTIVITY)),

        %% No disconnection should have occurred.
        escalus_assert:has_no_stanzas(Carol),
        1 = length(get_bosh_sessions()),
        1 = length(get_handlers(CarolSessionPid))

        end).

reply_on_pause(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        set_keepalive(Carol, false),

        %% Sanity check - there should be one handler for Carol.
        1 = length(get_handlers(CarolSessionPid)),

        pause(Carol, 10),

        %% There should be no handlers for Carol,
        %% but the session should be alive.
        1 = length(get_bosh_sessions()),
        0 = length(get_handlers(CarolSessionPid)),
        0 = escalus_bosh:get_requests(Carol#client.conn)

        end).

cant_pause_for_too_long(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        set_keepalive(Carol, false),

        %% Sanity check - there should be one handler for Carol.
        1 = length(get_handlers(CarolSessionPid)),

        pause(Carol, 10000),

        escalus:assert(is_stream_end, escalus:wait_for_stanza(Carol)),
        0 = length(get_bosh_sessions())

        end).

%% Ensure that a pause request causes inactivity timer cancellation.
pause_request_is_activity(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        set_keepalive(Carol, false),

        %% Sanity check - there should be one handler for Carol.
        1 = length(get_handlers(CarolSessionPid)),

        %% Wait most of the allowed inactivity interval.
        timer:sleep(timer:seconds(4)),

        %% This should cancel the inactivity timer.
        pause(Carol, 10),

        %% Wait a bit past the inactivity interval.
        timer:sleep(timer:seconds(4)),

        %% No disconnection should've occured.
        escalus_assert:has_no_stanzas(Carol),
        1 = length(get_bosh_sessions())

        end).

reply_in_time(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

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
        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        0 = length(get_handlers(CarolSessionPid)),

        %% Send a single request and assert it's registered by server.
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),
        Empty = escalus_bosh:empty_body(Rid, Sid),
        escalus_bosh:send_raw(Carol#client.conn, Empty),
        timer:sleep(100),
        1 = length(get_handlers(CarolSessionPid)),

        timer:sleep(timer:seconds(Wait) + 100),

        %% Assert the server has responded to that request.
        0 = length(get_handlers(CarolSessionPid))

        end).

server_acks(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        escalus_bosh:set_active(Carol#client.conn, false),
        ExpectedRid = list_to_binary(integer_to_list(get_bosh_rid(Carol))),
        escalus_client:send(Carol, escalus_stanza:chat_to(Geralt, <<"1st!">>)),
        escalus_client:send(Carol, escalus_stanza:chat_to(Geralt, <<"2nd!">>)),
        timer:sleep(200),

        All = recv_all(Carol),
        ExpectedRid = exml_query:attr(hd(All), <<"ack">>)

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
        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        set_client_acks(CarolSessionPid, true),

        escalus_bosh:set_active(Carol#client.conn, false),
        %% Send ack with StaleRid
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),
        BodyWithAck = ack_body(escalus_bosh:empty_body(Rid, Sid), StaleRid),
        escalus_bosh:send_raw(Carol#client.conn, BodyWithAck),

        %% Turn off client acknowledgement checking - don't cause server error
        %% on subsequent requests without 'ack' attribute.
        set_client_acks(CarolSessionPid, false),

        %% Expect a server report
        timer:sleep(100),
        MaybeReport = bosh_recv(Carol),
        escalus:assert(is_bosh_report, [StaleRid+1], MaybeReport)

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
        escalus_bosh:send_raw(Carol#client.conn, Chat),
        escalus:assert(is_chat_message, [<<"1st msg!">>],
                       wait_for_stanza(Geralt)),
        escalus_client:send(Geralt, chat_to(Carol, <<"1st rep!">>)),
        ChatResponse = wait_for_stanza(Carol),
        escalus:assert(is_chat_message, [<<"1st rep!">>], ChatResponse),

        %% Resend msg.
        escalus_bosh:resend_raw(Carol#client.conn, Chat),

        %% Recv same reply again.
        ChatResponse = wait_for_stanza(Carol)

        end).

force_cache_trimming(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        Sid = get_bosh_sid(Carol),

        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        set_client_acks(CarolSessionPid, true),

        %% Ack now
        Rid1 = get_bosh_rid(Carol),
        Ack1 = ack_body(escalus_bosh:empty_body(Rid1, Sid), Rid1-1),
        escalus_bosh:send_raw(Carol#client.conn, Ack1),

        %% Exchange 2 messages
        Rid2 = get_bosh_rid(Carol),
        Chat = (escalus_bosh:empty_body(Rid2, Sid))#xmlel{
                children = [escalus_stanza:chat_to(Geralt, <<"1st msg!">>)]},
        escalus_bosh:send_raw(Carol#client.conn, Chat),
        escalus:assert(is_chat_message, [<<"1st msg!">>],
                       wait_for_stanza(Geralt)),
        escalus_client:send(Geralt, chat_to(Carol, <<"1st rep!">>)),
        ChatResponse = wait_for_stanza(Carol),
        escalus:assert(is_chat_message, [<<"1st rep!">>], ChatResponse),

        %% Ack/Chat again
        Rid3 = get_bosh_rid(Carol),
        AckedChat = (ack_body(escalus_bosh:empty_body(Rid3, Sid), Rid2))#xmlel{
                children = [escalus_stanza:chat_to(Geralt, <<"2nd msg!">>)]},
        escalus_bosh:send_raw(Carol#client.conn, AckedChat),
        escalus:assert(is_chat_message, [<<"2nd msg!">>],
                       wait_for_stanza(Geralt)),

        %% The cache should now contain only entries newer than Rid2.
        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        Cache = get_cached_responses(CarolSessionPid),
        true = lists:all(fun({R,_,_}) when R > Rid2 -> true; (_) -> false end,
                         Cache),
        %% Not sure about this one...
        1 = length(Cache),

        set_client_acks(CarolSessionPid, false)

        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_bosh_sessions() ->
    Backend = escalus_ejabberd:rpc(mod_bosh_dynamic, backend, []),
    escalus_ejabberd:rpc(Backend, get_sessions, []).

get_handlers(BoshSessionPid) ->
    escalus_ejabberd:rpc(mod_bosh_socket, get_handlers, [BoshSessionPid]).

get_bosh_sid(#transport{} = Transport) ->
    escalus_bosh:get_sid(Transport);
get_bosh_sid(#client{conn = Conn}) ->
    get_bosh_sid(Conn).

get_bosh_rid(#client{} = C) ->
    escalus_bosh:get_rid(C#client.conn);
get_bosh_rid(Transport) ->
    escalus_bosh:get_rid(Transport).

set_keepalive(#client{} = C, Keepalive) ->
    escalus_bosh:set_keepalive(C#client.conn, Keepalive).

mark_as_terminated(#client{} = C) ->
    escalus_bosh:mark_as_terminated(C#client.conn).

pause(#client{} = C, Seconds) ->
    escalus_bosh:pause(C#client.conn, Seconds),
    timer:sleep(100).

start_client(Config, User, Res) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    UserSpec = proplists:get_value(User, NamedSpecs),
    {ok, Client} = escalus_client:start(Config, UserSpec, Res),
    Client.

recv_all(Client) ->
    recv_all(bosh_recv(Client), Client, []).

recv_all(empty, _Client, Acc) ->
    lists:reverse(Acc);
recv_all(Element, Client, Acc) ->
    recv_all(bosh_recv(Client), Client, [Element | Acc]).

bosh_recv(#client{} = C) ->
    escalus_bosh:recv(C#client.conn).

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
    escalus_ejabberd:rpc(mod_bosh_socket, set_client_acks,
                         [SessionPid, Enabled]).

get_cached_responses(SessionPid) ->
    escalus_ejabberd:rpc(mod_bosh_socket, get_cached_responses, [SessionPid]).

inactivity() ->
    inactivity(?INACTIVITY).

inactivity(Value) ->
    {inactivity,
     fun() -> escalus_ejabberd:rpc(mod_bosh, get_inactivity, []) end,
     fun(V) -> escalus_ejabberd:rpc(mod_bosh, set_inactivity, [V]) end,
     Value}.

server_acks_opt() ->
    {server_acks,
     fun() -> escalus_ejabberd:rpc(mod_bosh, get_server_acks, []) end,
     fun(V) -> escalus_ejabberd:rpc(mod_bosh, set_server_acks, [V]) end,
     true}.
