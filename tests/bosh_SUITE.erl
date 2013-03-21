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
     {group, chat}].

groups() ->
    [{essential, [{repeat,10}], [create_and_terminate_session]},
     {chat, [{repeat,10}], [interleave_requests,
                            simple_chat]},
     {inactivity, [{repeat,1}], [disconnect_inactive]}].

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
    OldInactivity = escalus_ejabberd:rpc(mod_bosh, get_inactivity, []),
    escalus_ejabberd:rpc(mod_bosh, set_inactivity, [?INACTIVITY]),
    escalus:init_per_testcase(CaseName,
                              [{old_inactivity, OldInactivity} | Config]);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(disconnect_inactive = CaseName, Config) ->
    case proplists:get_value(old_inactivity, Config) of
        undefined ->
            ok;
        OldInactivity ->
            escalus_ejabberd:rpc(mod_bosh, set_inactivity,
                                 [OldInactivity])
    end,
    escalus:end_per_testcase(CaseName, Config);
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
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Geralt)),

        escalus_client:send(Geralt, escalus_stanza:chat_to(Carol, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>], escalus_client:wait_for_stanza(Carol))

        end).

disconnect_inactive(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        %% Don't send new long-polling requests waiting for server push.
        set_keepalive(Carol, false),

        %% Make Carol receive using the last remaining connection.
        escalus_client:send(Geralt, escalus_stanza:chat_to(Carol, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>], escalus_client:wait_for_stanza(Carol)),

        %% Ensure all connections for Carols have been closed.
        [{_, _, CarolSessionPid}] = get_bosh_sessions(),
        [] = escalus_ejabberd:rpc(mod_bosh_socket, get_handlers, [CarolSessionPid]),

        %% Wait for disconnection because of inactivity timeout.
        timer:sleep(2 * timer:seconds(?INACTIVITY)),

        throw(fail)

        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_bosh_sessions() ->
    %% TODO: override for other backends
    escalus_ejabberd:rpc(ets, tab2list, [bosh_session]).

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

start_client(Config, User, Res) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    UserSpec = proplists:get_value(User, NamedSpecs),
    {ok, Client} = escalus_client:start(Config, UserSpec, Res),
    Client.
