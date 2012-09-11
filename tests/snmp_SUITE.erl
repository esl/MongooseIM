%%==============================================================================
%% Copyright 2011 Erlang Solutions Ltd.
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

-module(snmp_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SLEEP_TIME, 2000).
-define(RPC_LOOKUP(Table, Counter), escalus_ejabberd:rpc(ets, lookup, [Table, Counter])).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, general},
     {group, mod_privacy},
     {group, mod_privacy_odbc}
    ].

groups() ->
    [{general, [sequence], [generalUptime,
                            generalNodeName]},
     {mod_privacy, [sequence], mod_privacy_tests()},
     {mod_privacy_odbc, [sequence], mod_privacy_tests()}
    ].

suite() ->
    escalus:suite().

mod_privacy_tests() ->
    [modPrivacyGets,
     modPrivacySets,
     modPrivacySetsActive,
     modPrivacySetsDefault,
     modPrivacyStanzaBlocked,
     modPrivacyStanzaAll,
     modPrivacyPush,
     modPrivacyListLength
    ].


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(mod_privacy, Config) ->
    privacy_helper:restart_mod_privacy(""),
    escalus:create_users(Config);
init_per_group(mod_privacy_odbc, Config) ->
    privacy_helper:restart_mod_privacy("_odbc"),
    escalus:create_users(Config);
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(modPrivacyListLength, Config) ->
    Interval = round(0.5 * ?SLEEP_TIME / 1000), %% half of ?SLEEP_TIME in seconds
    ok = escalus_ejabberd:rpc(gen_server, call,
            [ejabberd_snmp_rt, {change_interval_rt, Interval}]),
    %% rest is the same; fallthrough is not a test name,
    %% I just want to call the generic clause
    init_per_testcase(fallthrough, Config);
init_per_testcase(CaseName, Config) ->
    escalus_ejabberd:rpc(mnesia, clear_table, [privacy]),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(modPrivacyListLength, Config) ->
    ok = escalus_ejabberd:rpc(gen_server, call,
           [ejabberd_snmp_rt, {change_interval_rt, 60}]),
    end_per_testcase(fallthrough, Config);
end_per_testcase(CaseName, Config) ->
    escalus_ejabberd:rpc(ejabberd_snmp_core, reset_counters, []),
    escalus_ejabberd:rpc(odbc_queries, clear_privacy_lists, [<<"localhost">>]),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

generalUptime(_Config) ->
    TMeasured = erlang:round(element(1,escalus_ejabberd:rpc(
                                         erlang, statistics,
                                         [wall_clock])) / 1000),
    {value, TCounter} = snmp_helper:get_counter_value(generalUptime),
    true = (TCounter - TMeasured) =< 1.

generalNodeName(_Config) ->
    Value = atom_to_list(escalus_ejabberd:rpc(erlang, node, [])),
    snmp_helper:assert_counter(Value, generalNodeName).


%% Privacy

modPrivacyGets(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Table = stats_mod_privacy,
        Counter = modPrivacyGets,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:wait_for_stanzas(Alice, 1),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacySets(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Table = stats_mod_privacy,
        Counter = modPrivacySets,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_list(Alice, <<"deny_bob">>),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacySetsActive(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Table = stats_mod_privacy,
        Counter = modPrivacySetsActive,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_list(Alice, <<"deny_bob">>),
        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:activate_list(Alice, <<"deny_bob">>),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacySetsDefault(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Table = stats_mod_privacy,
        Counter = modPrivacySetsDefault,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_list(Alice, <<"deny_bob">>),
        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_default_list(Alice, <<"deny_bob">>),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacyStanzaBlocked(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Table = stats_mod_privacy,
        Counter = modPrivacyStanzaBlocked,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        send_chat(Bob, Alice),
        %% No blocking yet
        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_and_activate(Alice, <<"deny_bob_message">>),
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"hey?!?">>)),
        %% One message blocked
        timer:sleep(?SLEEP_TIME),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacyStanzaAll(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Table = stats_mod_privacy,
        Counter = modPrivacyStanzaAll,

        %% This is needed *here* as some stanzas are sent in escalus:story,
        %% so after the per-testcase initialization.
        escalus_ejabberd:rpc(ejabberd_snmp_core, reset_counters, []),

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        send_chat(Bob, Alice),
        [{Counter, 2}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_and_activate(Alice, <<"deny_bob_message">>),
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"hey?!?">>)),
        timer:sleep(?SLEEP_TIME),
        [{Counter, 6}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacyPush(Config) ->
    escalus:story(Config, [3], fun(Alice1, _Alice2, _Alice3) ->

        Table = stats_mod_privacy,
        Counter = modPrivacyPush,

        %% No pushes for Alice should exist yet
        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_list(Alice1, <<"deny_bob">>),
        %% Alice has got 3 resources, so after 1 set, the counter should equal 3
        [{Counter, 3}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacyListLength(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Table = stats_mod_privacy,
        Counter = modPrivacyListLength,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        privacy_helper:set_list(Alice, <<"deny_bob">>),
        privacy_helper:set_list(Alice, <<"deny_3_items">>),
        timer:sleep(?SLEEP_TIME),
        %% First list has 1 item, second has 3
        [{Counter, 2}] = ?RPC_LOOKUP(Table, Counter)

        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

%% Bob successfully sends message to Alice.
send_chat(Bob, Alice) ->
    Msg = <<"Hi! What's your name?">>,
    escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg)),
    escalus:assert(is_chat_message, [Msg], escalus:wait_for_stanza(Alice)).
