%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
-module(mod_ping_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, client_ping},
     {group, server_ping},
     {group, server_ping_kill}].

groups() ->
    [{client_ping, [], [ping]},
     {server_ping, [], all_tests()},
     {server_ping_kill, [], all_tests()}
    ].

all_tests() ->
    [ping,
     active,
     active_keep_alive,
     server_ping_pong,
     server_ping_pang].
suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(client_ping, Config) ->
    start_mod_ping([]),
    escalus:create_users(Config, escalus:get_users([alice]));
init_per_group(server_ping, Config) ->
    start_mod_ping([{send_pings, true},
                    {ping_interval, 8},
                    {ping_req_timeout, 4}]),
    escalus:create_users(Config, escalus:get_users([alice]));
init_per_group(server_ping_kill, Config) ->
    start_mod_ping([{send_pings, true},
                    {ping_interval, 8},
                    {ping_req_timeout, 4},
                    {timeout_action, kill}]),
    [{timeout_action, kill} | escalus:create_users(Config, escalus:get_users([alice]))].

end_per_group(_GroupName, Config) ->
    Domain = ct:get_config(ejabberd_domain),
    dynamic_modules:stop(Domain, mod_ping),
    escalus:delete_users(Config, escalus:get_users([alice])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

start_mod_ping(Opts) ->
    Domain = ct:get_config(ejabberd_domain),
    dynamic_modules:start(Domain, mod_ping, Opts).

%%--------------------------------------------------------------------
%% Ping tests
%%--------------------------------------------------------------------
ping(Config) ->
    escalus:story(Config, [{alice, 1}],
        fun(Alice) ->
                Domain = ct:get_config(ejabberd_domain),
                PingReq = escalus_stanza:ping_request(Domain),
                escalus_client:send(Alice, PingReq),

                PingResp = escalus_client:wait_for_stanza(Alice),
                escalus:assert(is_iq_result, [PingReq], PingResp)
        end).

active(Config) ->
    escalus:story(Config, [{alice, 1}],
        fun(Alice) ->
                Domain = ct:get_config(ejabberd_domain),
                ct:sleep(timer:seconds(6)), % wait 6s (ping_interval is 8)
                escalus_client:send(Alice, escalus_stanza:ping_request(Domain)),
                escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Alice)),
                ct:sleep(timer:seconds(4)), % wait another 4s and check if connection we got ping req

                false = escalus_client:has_stanzas(Alice)
        end).

active_keep_alive(Config) ->
    escalus:story(Config, [{alice, 1}],
        fun(Alice) ->
                ct:sleep(timer:seconds(6)), % wait 6s (ping_interval is 8)
                Socket = Alice#client.socket,
                gen_tcp:send(Socket, <<"\n">>),
                ct:sleep(timer:seconds(4)), % wait another 4s and check if connection we got ping req

                false = escalus_client:has_stanzas(Alice)
        end).

server_ping_pong(Config) ->
    escalus:story(Config, [{alice, 1}],
        fun(Alice) ->
                PingReq = wait_for_ping_req(Alice),
                Pong = escalus_stanza:iq_result(PingReq),
                escalus_client:send(Alice, Pong)
        end).

server_ping_pang(ConfigIn) ->
    Domain = ct:get_config(ejabberd_domain),
    Metrics = [{[Domain, user_ping_timeout], 1}],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    escalus:story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_for_ping_req(Alice),
                %% do not resp to ping req
                ct:sleep(timer:seconds(5)), %% server waits 4s for the reply
                TimeoutAction = ?config(timeout_action, Config),
                check_connection(TimeoutAction, Alice)
        end).

check_connection(kill, Client) ->
    false = escalus_connection:is_connected(Client);
check_connection(_, Client) ->
    true = escalus_connection:is_connected(Client).

wait_for_ping_req(Alice) ->
    PingReq = escalus_client:wait_for_stanza(Alice, timer:seconds(10)),
    escalus:assert(is_iq_get, PingReq),
    <<"urn:xmpp:ping">> = exml_query:path(PingReq, [{element, <<"ping">>},
                                                    {attr, <<"xmlns">>}]),
    PingReq.

