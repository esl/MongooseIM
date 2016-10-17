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
     {server_ping, [parallel], all_tests()},
     {server_ping_kill, [parallel], all_tests()}
    ].

all_tests() ->
    [ping,
     active,
     active_keep_alive,
     server_ping_pong,
     server_ping_pang].
suite() ->
    escalus:suite().

ping_interval() ->
    6.

ping_req_timeout() ->
    3.

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(client_ping, Config) ->
    start_mod_ping([]),
    Config;
init_per_group(server_ping, Config) ->
    start_mod_ping([{send_pings, true},
                    {ping_interval, ping_interval()},
                    {ping_req_timeout, ping_req_timeout()}]),
    Config;
init_per_group(server_ping_kill, Config) ->
    start_mod_ping([{send_pings, true},
                    {ping_interval, ping_interval()},
                    {ping_req_timeout, ping_req_timeout()},
                    {timeout_action, kill}]),
    [{timeout_action, kill} | Config].

end_per_group(_GroupName, Config) ->
    Domain = ct:get_config(ejabberd_domain),
    dynamic_modules:stop(Domain, mod_ping),
    Config.

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
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                Domain = ct:get_config(ejabberd_domain),
                PingReq = escalus_stanza:ping_request(Domain),
                escalus_client:send(Alice, PingReq),

                PingResp = escalus_client:wait_for_stanza(Alice),
                escalus:assert(is_iq_result, [PingReq], PingResp)
        end).

active(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                Domain = ct:get_config(ejabberd_domain),
                wait_ping_interval(0.75),
                escalus_client:send(Alice, escalus_stanza:ping_request(Domain)),
                escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Alice)),
                % wait more time and check if connection got ping req
                wait_ping_interval(0.5),
                % it shouldn't as the ping was sent
                false = escalus_client:has_stanzas(Alice)
        end).

active_keep_alive(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_ping_interval(0.75),
                Socket = Alice#client.socket,
                gen_tcp:send(Socket, <<"\n">>),
                wait_ping_interval(0.5),

                false = escalus_client:has_stanzas(Alice)
        end).

server_ping_pong(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                PingReq = wait_for_ping_req(Alice),
                Pong = escalus_stanza:iq_result(PingReq),
                escalus_client:send(Alice, Pong)
        end).

server_ping_pang(ConfigIn) ->
    Domain = ct:get_config(ejabberd_domain),
    Metrics = [{[Domain, user_ping_timeout], 1}],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_for_ping_req(Alice),
                %% do not resp to ping req
                ct:sleep(timer:seconds(ping_req_timeout() + 1)),
                TimeoutAction = ?config(timeout_action, Config),
                check_connection(TimeoutAction, Alice)
        end).

wait_ping_interval(Ration) ->
    WaitTime = timer:seconds(ping_interval()) * Ration,
    ct:sleep(WaitTime).

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

