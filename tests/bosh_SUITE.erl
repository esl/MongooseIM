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

all() ->
    [{group, essential},
     {group, chat},
     {group, time},
     {group, acks}].

groups() ->
    [{essential, [{repeat,10}], [create_and_terminate_session]},
     {chat, [shuffle, {repeat,10}], [interleave_requests,
                                     simple_chat,
                                     cant_send_invalid_rid]},
     {time, [shuffle, {repeat,5}], [disconnect_inactive,
                                    reply_on_pause,
                                    cant_pause_for_too_long,
                                    reply_in_time]},
     {acks, [], [server_acks]}].

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
    escalus_users:create_users(Config, {by_name, [carol, geralt]});
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(essential, Config) ->
    Config;
end_per_group(chat, Config) ->
    escalus_users:delete_users(Config, {by_name, [carol, geralt]});
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).


init_per_testcase(disconnect_inactive = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(inactivity(), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(reply_on_pause = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(inactivity(), Config),
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
end_per_testcase(reply_on_pause = CaseName, Config) ->
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

interleave_requests(Config) ->
    escalus:story(Config, [{geralt, 1}], fun(Geralt) ->

        Carol = start_client(Config, carol, <<"bosh">>),
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),

        Empty2 = escalus_bosh:empty_body(Rid + 1, Sid),
        Chat2 = Empty2#xmlelement{
                children = [escalus_stanza:chat_to(Geralt, <<"2nd!">>)]},
        escalus_bosh:send_raw(Carol#client.conn, Chat2),

        Empty1 = escalus_bosh:empty_body(Rid, Sid),
        Chat1 = Empty1#xmlelement{
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

cant_send_invalid_rid(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        InvalidRid = get_bosh_rid(Carol) + 1593,
        Sid = get_bosh_sid(Carol),
        Empty = escalus_bosh:empty_body(InvalidRid, Sid),
        escalus_bosh:send_raw(Carol#client.conn, Empty),

        escalus:assert(is_stream_end, escalus:wait_for_stanza(Carol)),
        0 = length(get_bosh_sessions())

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
        0 = length(get_bosh_sessions())

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
        0 = length(get_handlers(CarolSessionPid))

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
    %% Geralt sends a reply to msg3
    %% Geralt sends a reply to msg4
    %% Carol recvs a reply to msg3
    %% Carol recvs a reply to msg4
    %% Carol sends an ack with Rid1 on empty BOSH wrapper
    %% server sends a report

    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        StaleRid = get_bosh_rid(Carol),
        escalus_client:send(Carol, chat_to(Geralt, <<"1st!">>)),
        escalus_client:send(Carol, chat_to(Geralt, <<"2nd!">>)),
        wait_for_stanzas(Geralt, 2),
        escalus_client:send(Geralt, chat_to(Carol, <<"3rd!">>)),
        escalus_client:send(Geralt, chat_to(Carol, <<"4th!">>)),
        escalus:assert(is_chat_message, [<<"3rd!">>], wait_for_stanza(Carol)),
        escalus:assert(is_chat_message, [<<"4th!">>], wait_for_stanza(Carol)),

        escalus_bosh:set_active(Carol#client.conn, false),
        %% Send ack with StaleRid
        Rid = get_bosh_rid(Carol),
        Sid = get_bosh_sid(Carol),
        BodyWithAck = ack_body(escalus_bosh:empty_body(Rid, Sid), StaleRid),
        escalus_bosh:send_raw(Carol#client.conn, BodyWithAck),
        %% Expect a server report
        MaybeReport = bosh_recv(Carol),
        error_logger:info_msg("report? ~p~n", [MaybeReport]),
        escalus:assert(is_bosh_report, [StaleRid+1], MaybeReport)

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
    Attrs = Body#xmlelement.attrs,
    Ack = {<<"ack">>, list_to_binary(integer_to_list(Rid))},
    NewAttrs = lists:keystore(<<"ack">>, 1, Attrs, Ack),
    Body#xmlelement{attrs = NewAttrs}.

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
