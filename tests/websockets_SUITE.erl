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

-module(websockets_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds

all() ->
    [{group, ws_chat}].

groups() ->
    [{ws_chat, [sequence], [chat_msg, escape_chat_msg]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, geralt, oldie]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, geralt, oldie]}).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

chat_msg(Config) ->
    escalus:story(Config, [{alice, 1}, {geralt, 1}, {oldie, 1}], fun(Alice, Geralt, Oldie) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Geralt, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Geralt)),

        escalus_client:send(Geralt, escalus_stanza:chat_to(Alice, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>], escalus_client:wait_for_stanza(Alice)),

        escalus_client:send(Geralt, escalus_stanza:chat_to(Oldie, <<"Hey!">>)),
        escalus_assert:is_chat_message(<<"Hey!">>, escalus_client:wait_for_stanza(Oldie))

        end).

escape_chat_msg(Config) ->
    escalus:story(Config, [{alice, 1}, {geralt, 1}, {oldie, 1}], fun(Alice, Geralt, Oldie) ->
        Message1 = <<"Hi! & < >">>,
        escalus_client:send(Alice, escalus_stanza:chat_to(Geralt, Message1)),
        escalus:assert(is_chat_message, [Message1], escalus_client:wait_for_stanza(Geralt)),

        Message2 = <<"Hello! & < >">>,
        escalus_client:send(Geralt, escalus_stanza:chat_to(Alice, Message2)),
        escalus:assert(is_chat_message, [Message2], escalus_client:wait_for_stanza(Alice)),

        Message3= <<"Hey! & < >">>,
        escalus_client:send(Geralt, escalus_stanza:chat_to(Oldie, Message3)),
        escalus_assert:is_chat_message(Message3, escalus_client:wait_for_stanza(Oldie))

    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

