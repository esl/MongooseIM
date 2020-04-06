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
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, client_ping},
     {group, server_ping},
     {group, server_ping_kill}].

groups() ->
    % Don't make these parallel! Metrics tests will most probably fail
    % and injected hook will most probably won't work as expected.
    G = [{client_ping, [], [ping]},
         {server_ping, [], all_tests()},
         {server_ping_kill, [], all_tests()}
        ],
    ct_helper:repeat_all_until_all_ok(G).

client_ping_test_cases() ->
    [ping,
     wrong_ping].

all_tests() ->
    [ping,
     wrong_ping,
     active,
     active_keep_alive,
     server_ping_pong,
     server_ping_pang].

suite() ->
    escalus:suite().

ping_interval() ->
    3.

ping_req_timeout() ->
    2.

init_per_suite(Config) ->
    mongoose_helper:inject_module(?MODULE),
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
    Domain = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Domain, mod_ping),
    Config.

init_per_testcase(server_ping_pong = CN, Config) ->
    NConfig = setup_pong_hook(Config),
    escalus:init_per_testcase(CN, NConfig);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(server_ping_pong = CN, Config) ->
    NConfig = clear_pong_hook(Config),
    escalus:init_per_testcase(CN, NConfig);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

start_mod_ping(Opts) ->
    Domain = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Domain, mod_ping, Opts).

setup_pong_hook(Config) ->
    Pid = self(),
    Host = escalus_users:get_host(Config, alice),
    Handler = mongoose_helper:successful_rpc(?MODULE, setup_pong_hook, [Host, Pid]),
    [{pong_handler, Handler} | Config].

setup_pong_hook(Host, Pid) ->
    Handler = fun(_Acc, JID, _Response, _TDelta) ->
                      Pid ! {pong, jid:to_binary(jid:to_lower(JID))}
              end,
    ejabberd_hooks:add(user_ping_response, Host, Handler, 50),
    Handler.

clear_pong_hook(Config) ->
    {value, {_, Handler}, NConfig} = lists:keytake(pong_handler, 1, Config),
    Host = escalus_users:get_host(Config, alice),
    mongoose_helper:successful_rpc(?MODULE, clear_pong_hook, [Host, Handler]),
    NConfig.

clear_pong_hook(Host, Handler) ->
    ejabberd_hooks:delete(user_ping_response, Host, Handler, 50).

%%--------------------------------------------------------------------
%% Ping tests
%%--------------------------------------------------------------------
ping(ConfigIn) ->
    Domain = ct:get_config({hosts, mim, domain}),
    Metrics = [
        {[Domain, mod_ping, ping_response],0},
        {[Domain, mod_ping, ping_response_timeout],0}
    ],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                PingReq = escalus_stanza:ping_request(Domain),
                escalus_client:send(Alice, PingReq),

                PingResp = escalus_client:wait_for_stanza(Alice),
                escalus:assert(is_iq_result, [PingReq], PingResp)
        end).

wrong_ping(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun(Alice) ->
                            Domain = ct:get_config({hosts, mim, domain}),
                            IQ = escalus_stanza:iq(<<"get">>, [#xmlel{name = <<"unsupported">>,
                                                                      attrs = [{<<"xmlns">>, ?NS_PING}]
                            }]),
                            PingReq = escalus_stanza:to(IQ, Domain),
                            escalus_client:send(Alice, PingReq),

                            PingResp = escalus_client:wait_for_stanza(Alice),
                            escalus:assert(is_iq_error, [PingReq], PingResp)
                        end).

active(ConfigIn) ->
    Domain = ct:get_config({hosts, mim, domain}),
    Metrics = [
        {[Domain, mod_ping, ping_response],0},
        {[Domain, mod_ping, ping_response_timeout],0}
    ],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_ping_interval(0.75),
                escalus_client:send(Alice, escalus_stanza:ping_request(Domain)),
                escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Alice)),
                % wait more time and check if connection got ping req
                wait_ping_interval(0.5),
                % it shouldn't as the ping was sent
                false = escalus_client:has_stanzas(Alice)
        end).

active_keep_alive(ConfigIn) ->
    Domain = ct:get_config({hosts, mim, domain}),
    Metrics = [
        {[Domain, mod_ping, ping_response],0},
        {[Domain, mod_ping, ping_response_timeout],0}
    ],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_ping_interval(0.75),
                escalus_tcp:send(Alice#client.rcv_pid, #xmlcdata{content = "\n"}),
                wait_ping_interval(0.5),

                false = escalus_client:has_stanzas(Alice)
        end).

server_ping_pong(ConfigIn) ->
    Domain = ct:get_config({hosts, mim, domain}),
    Metrics = [
        {[Domain, mod_ping, ping_response], 5},
        {[Domain, mod_ping, ping_response_timeout], 0},
        {[Domain, mod_ping, ping_response_time], changed}
    ],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    %% We use 5 Alices because with just 1 sample the histogram may look like it hasn't changed
    %% due to exometer histogram implementation
    escalus:fresh_story(Config, [{alice, 5}],
        fun(Alice1, Alice2, Alice3, Alice4, Alice5) ->
                lists:foreach(fun(Alice) ->
                                      PingReq = wait_for_ping_req(Alice),
                                      Pong = escalus_stanza:iq_result(PingReq),
                                      escalus_client:send(Alice, Pong)
                              end, [Alice1, Alice2, Alice3, Alice4, Alice5]),
                wait_for_pong_hooks(5)
        end).

server_ping_pang(ConfigIn) ->
    Domain = ct:get_config({hosts, mim, domain}),
    Metrics = [
        {[Domain, mod_ping, ping_response], 0},
        {[Domain, mod_ping, ping_response_timeout], 1}
    ],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_for_ping_req(Alice),
                %% do not resp to ping req
                ct:sleep(timer:seconds(ping_req_timeout() + 0.5)),
                TimeoutAction = ?config(timeout_action, Config),
                check_connection(TimeoutAction, Alice),
                escalus_client:kill_connection(Config, Alice)
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

wait_for_pong_hooks(0) ->
    ok;
wait_for_pong_hooks(N) ->
    receive
        {pong, _} -> wait_for_pong_hooks(N-1)
    after
        5000 ->
            ct:fail({pong_hook_runs_missing, N})
    end.

