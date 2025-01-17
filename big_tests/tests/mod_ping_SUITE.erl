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
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(domain_helper, [domain/0, host_type/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [
     {group, client_ping},
     {group, server_ping},
     {group, server_ping_kill}
    ].

groups() ->
    [
     {client_ping, [parallel], [disco, ping]},
     {server_ping, [parallel], all_tests()},
     {server_ping_kill, [parallel], all_tests()}
    ].

client_ping_test_cases() ->
    [
     ping,
     wrong_ping
    ].

all_tests() ->
    [
     disco,
     ping,
     wrong_ping,
     active,
     active_keep_alive,
     server_ping_pong,
     server_ping_pang,
     service_unavailable_response
    ].

suite() ->
    escalus:suite().

ping_interval() ->
    timer:seconds(3).

ping_req_timeout() ->
    timer:seconds(2).

init_per_suite(Config) ->
    instrument_helper:start(instrument_helper:declared_events(mod_ping)),
    mongoose_helper:inject_module(?MODULE),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

init_per_group(client_ping, Config) ->
    start_mod_ping(#{}),
    Config;
init_per_group(server_ping, Config) ->
    start_mod_ping(#{send_pings => true,
                     ping_interval => ping_interval(),
                     ping_req_timeout => ping_req_timeout()}),
    Config;
init_per_group(server_ping_kill, Config) ->
    start_mod_ping(#{send_pings => true,
                     ping_interval => ping_interval(),
                     ping_req_timeout => ping_req_timeout(),
                     timeout_action => kill}),
    [{timeout_action, kill} | Config].

end_per_group(_GroupName, Config) ->
    HostType = host_type(),
    dynamic_modules:stop(HostType, mod_ping),
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
    HostType = host_type(),
    dynamic_modules:start(HostType, mod_ping, config_parser_helper:mod_config(mod_ping, Opts)).

setup_pong_hook(Config) ->
    Pid = self(),
    HostType = host_type(),
    mongoose_helper:successful_rpc(?MODULE, setup_pong_hook, [HostType, Pid]),
    [{pid, Pid} | Config].

setup_pong_hook(HostType, Pid) ->
    gen_hook:add_handler(user_ping_response, HostType,
                         fun ?MODULE:pong_hook_handler/3,
                         #{pid => Pid}, 50).

pong_hook_handler(Acc,
                  #{jid := JID} = _Params,
                  #{pid := Pid} = _Extra) ->
    Pid ! {pong, jid:to_binary(jid:to_lower(JID))},
    {ok, Acc}.

clear_pong_hook(Config) ->
    {value, {_, Pid}, NConfig} = lists:keytake(pid, 1, Config),
    HostType = host_type(),
    mongoose_helper:successful_rpc(?MODULE, clear_pong_hook, [HostType, Pid]),
    NConfig.

clear_pong_hook(HostType, Pid) ->
    gen_hook:delete_handler(user_ping_response, HostType,
                            fun ?MODULE:pong_hook_handler/3,
                            #{pid => Pid}, 50).

%%--------------------------------------------------------------------
%% Ping tests
%%--------------------------------------------------------------------
disco(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              escalus_client:send(Alice, escalus_stanza:disco_info(domain())),
              Response = escalus_client:wait_for_stanza(Alice),
              escalus:assert(has_feature, [?NS_PING], Response)
      end).

ping(Config) ->
    Domain = domain(),
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                PingReq = escalus_stanza:ping_request(Domain),
                escalus_client:send(Alice, PingReq),

                PingResp = escalus_client:wait_for_stanza(Alice),
                escalus:assert(is_iq_result, [PingReq], PingResp),
                assert_no_event(mod_ping_response, Alice),
                assert_no_event(mod_ping_response_timeout, Alice)
        end).

wrong_ping(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
            Domain = domain(),
            IQ = escalus_stanza:iq(<<"get">>, [#xmlel{name = <<"unsupported">>,
                                                      attrs = #{<<"xmlns">> => ?NS_PING}
            }]),
            PingReq = escalus_stanza:to(IQ, Domain),
            escalus_client:send(Alice, PingReq),

            PingResp = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_iq_error, [PingReq], PingResp)
        end).

service_unavailable_response(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
            PingReq = wait_for_ping_req(Alice),
            PingId = exml_query:attr(PingReq, <<"id">>),

            ErrorStanzaBody = [#xmlel{name = <<"ping">>, attrs = #{<<"xmlns">> => ?NS_PING}},
                               #xmlel{name = <<"error">>, attrs = #{<<"type">> => <<"cancel">>},
                               children = [#xmlel{name = <<"service-unavailable">>,
                                                  attrs = #{<<"xmlns">> => ?NS_STANZA_ERRORS}}]}],
            ErrorStanza = escalus_stanza:set_id(
                            escalus_stanza:iq(domain(), <<"error">>, ErrorStanzaBody), PingId),
            escalus_client:send(Alice, ErrorStanza),

            TimeoutAction = ?config(timeout_action, Config),
            check_connection(TimeoutAction, Alice),
            escalus_client:kill_connection(Config, Alice)
        end).

active(Config) ->
    Domain = domain(),
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_ping_interval(0.75),
                escalus_client:send(Alice, escalus_stanza:ping_request(Domain)),
                escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Alice)),
                % wait more time and check if connection got ping req
                wait_ping_interval(0.5),
                % it shouldn't as the ping was sent
                false = escalus_client:has_stanzas(Alice),
                assert_no_event(mod_ping_response, Alice),
                assert_no_event(mod_ping_response_timeout, Alice)
        end).

active_keep_alive(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                wait_ping_interval(0.75),
                escalus_tcp:send(Alice#client.rcv_pid, #xmlcdata{content = "\n"}),
                wait_ping_interval(0.5),

                false = escalus_client:has_stanzas(Alice),

                assert_no_event(mod_ping_response, Alice),
                assert_no_event(mod_ping_response_timeout, Alice)
        end).

server_ping_pong(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
            PingReq = wait_for_ping_req(Alice),
            Pong = escalus_stanza:iq_result(PingReq),
            escalus_client:send(Alice, Pong),
            wait_for_pong_hook(escalus_utils:get_jid(Alice)),
            assert_no_event(mod_ping_response_timeout, Alice),
            assert_event(mod_ping_response,
                fun(#{count := 1, time := Time, jid := Jid}) ->
                    is_integer(Time)
                    andalso Time >= 0
                    andalso eq_jid(Alice, Jid)
                end)
        end).

server_ping_pang(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
            wait_for_ping_req(Alice),
            %% do not resp to ping req
            ct:sleep(ping_req_timeout() + timer:seconds(1)/2),
            TimeoutAction = ?config(timeout_action, Config),
            check_connection(TimeoutAction, Alice),
            escalus_client:kill_connection(Config, Alice),
            assert_no_event(mod_ping_response, Alice),
            assert_event(mod_ping_response_timeout,
                fun(#{count := 1, jid := Jid}) -> eq_jid(Alice, Jid) end)
        end).

wait_ping_interval(Ration) ->
    WaitTime = ping_interval() * Ration,
    ct:sleep(WaitTime).

check_connection(kill, Client) ->
    wait_helper:wait_until(fun() -> escalus_connection:is_connected(Client) end, false);
check_connection(_, Client) ->
    true = escalus_connection:is_connected(Client).

wait_for_ping_req(Alice) ->
    PingReq = escalus_client:wait_for_stanza(Alice, timer:seconds(10)),
    escalus:assert(is_iq_get, PingReq),
    <<"urn:xmpp:ping">> = exml_query:path(PingReq, [{element, <<"ping">>},
                                                    {attr, <<"xmlns">>}]),
    PingReq.

wait_for_pong_hook(Alice) ->
    receive
        {pong, Alice} -> ok
    after
        5000 ->
            ct:fail({pong_hook_run_missing})
    end.

assert_event(EventName, F) ->
    instrument_helper:assert_one(EventName, #{host_type => host_type()}, F).

assert_no_event(EventName, User) ->
    F = fun(#{jid := Jid}) -> eq_jid(User, Jid) end,
    instrument_helper:assert_not_emitted(EventName, #{host_type => host_type()}, F).

eq_jid(User, Jid) -> jid:from_binary(escalus_utils:get_jid(User)) =:= Jid.
