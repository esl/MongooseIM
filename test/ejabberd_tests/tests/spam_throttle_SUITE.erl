%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(spam_throttle_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(NS_BLOCKING,     <<"urn:xmpp:blocking">>).

-define(SLEEP_TIME, 50).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, throttle}
    ].

groups() ->
    [
        {throttle, [sequence], throttle_test_cases()}
    ].

throttle_test_cases() ->
    [
        sendtoomuch
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).
%%    [{escalus_no_stanzas_after_story, true} |
%%     escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    set_modules([{maxrate, 10}, {span, 2}]),
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    dynamic_modules:stop(host(), mod_http_notification),
    ejabberd_node_utils:call_fun(mongoose_http_client, stop_pool, [http_pool]),
    ejabberd_node_utils:call_fun(mongoose_http_client, stop, []),
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

set_modules(Opts) ->
    ejabberd_node_utils:call_fun(mongoose_http_client, start, [[]]),
    ejabberd_node_utils:call_fun(mongoose_http_client,
        start_pool,
        [http_pool, [{server, "http://localhost:8000"}]]),
    dynamic_modules:start(host(), mod_spamctl, Opts),
    ok.

host() -> <<"localhost">>.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

sendtoomuch(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Send = fun(I) -> message_is_delivered(Alice,
                                                  Bob,
                                                  list_to_binary(integer_to_list(I)))
                   end,
            lists:map(Send, lists:seq(1, 10)),
            % this msg is not delivered because it exceeded spamctl limit
            message_is_not_delivered(Alice, [Bob], <<"11">>),
            Res = escalus:wait_for_stanza(Alice),
            escalus_assert:is_error(Res, <<"modify">>, <<"not-acceptable">>),
            % and nothing more would be delivered because spamctl terminated connection
            timer:sleep(1000),
            message_is_not_delivered(Alice, [Bob], <<"12">>),
            timer:sleep(2000),
            message_is_not_delivered(Alice, [Bob], <<"13">>),
            ok
        end).

%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

message_is_delivered(From, [To|_] = Tos, MessageText) ->
    BareTo = escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    escalus:send(From, escalus_stanza:chat_to(BareTo, MessageText)),
    [ escalus:assert(is_chat_message, [MessageText], escalus:wait_for_stanza(C)) ||
        C <- Tos ];
message_is_delivered(From, To, MessageText) ->
    BareTo =  escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    escalus:send(From, escalus_stanza:chat_to(BareTo, MessageText)),
    escalus:assert(is_chat_message, [MessageText], escalus:wait_for_stanza(To)).


message_is_not_delivered(From, [To|_] = Tos, MessageText) ->
    BareTo = escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    escalus:send(From, escalus_stanza:chat_to(BareTo, MessageText)),
    timer:sleep(300),
    clients_have_no_messages(Tos).

clients_have_no_messages(Cs) when is_list (Cs) -> [ client_has_no_messages(C) || C <- Cs ].

client_has_no_messages(C) -> escalus_assert:has_no_stanzas(C).

