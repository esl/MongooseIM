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
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-import(distributed_helper, [mim/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds
-define(MAX_STANZA_SIZE, 200).

all() ->
    [metrics_test,
     {group, ws_chat},
     {group, wss_chat}].

groups() ->
    [{ws_chat, [sequence], test_cases()},
     {wss_chat, [sequence], test_cases()}].

test_cases() ->
    [chat_msg,
     escape_chat_msg,
     escape_attrs,
     too_big_stanza_is_rejected].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    Config2 = setup_listeners(Config1),
    escalus:create_users(Config2, escalus:get_users([alice, geralt, geralt_s, carol])).

end_per_suite(Config) ->
    Listeners = ?config(original_listeners, Config),
    [mongoose_helper:restart_listener(mim(), Listener) || Listener <- Listeners],
    Config1 = escalus:delete_users(Config, escalus:get_users([alice, geralt, geralt_s, carol])),
    escalus:end_per_suite(Config1).

init_per_group(GroupName, Config) ->
    case GroupName of
        wss_chat ->
            [{user, geralt_s} | Config];
        _ ->
            [{user, geralt} | Config]
    end.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

setup_listeners(Config) ->
    Listeners = mongoose_helper:get_listeners(mim(), #{module => ejabberd_cowboy}),
    [mongoose_helper:restart_listener(mim(), update_listener(Listener)) || Listener <- Listeners],
    [{original_listeners, Listeners} | Config].

update_listener(Listener = #{handlers := Handlers}) ->
    Listener#{handlers := lists:map(fun update_handler/1, Handlers)}.

update_handler(Handler = #{module := mod_websockets}) ->
    Handler#{max_stanza_size := ?MAX_STANZA_SIZE};
update_handler(Handler) ->
    Handler.

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

metrics_test(Config) ->
    MongooseMetrics = [{[global, data, xmpp, received, xml_stanza_size], changed},
                       {[global, data, xmpp, sent, xml_stanza_size], changed},
                       {[global, data, xmpp, received, c2s, websocket], changed},
                       {[global, data, xmpp, sent, c2s, websocket], changed},
                       {[global, data, xmpp, received, c2s, tcp], 0},
                       {[global, data, xmpp, sent, c2s, tcp], 0}],
    escalus:story([{mongoose_metrics, MongooseMetrics} | Config],
                  [{geralt, 1}, {geralt_s, 1}],
                  fun(Geralt, GeraltS) ->

        escalus_client:send(GeraltS, escalus_stanza:chat_to(Geralt, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Geralt)),

        escalus_client:send(Geralt, escalus_stanza:chat_to(GeraltS, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>], escalus_client:wait_for_stanza(GeraltS))

        end).

too_big_stanza_is_rejected(Config) ->
    escalus:story(
      Config, [{alice, 1}, {?config(user, Config), 1}],
      fun(Alice, Geralt) ->
              BigBody = base16:encode(crypto:strong_rand_bytes(?MAX_STANZA_SIZE)),
              escalus_client:send(Geralt, escalus_stanza:chat_to(Alice, BigBody)),
              escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>], escalus_client:wait_for_stanza(Geralt)),
              escalus:assert(is_stream_end, escalus_client:wait_for_stanza(Geralt)),
              true = escalus_connection:wait_for_close(Geralt, timer:seconds(1)),
              escalus_assert:has_no_stanzas(Alice)
      end).

chat_msg(Config) ->
    escalus:story(Config, [{alice, 1}, {?config(user, Config), 1}, {carol, 1}],
                  fun(Alice, Geralt, Carol) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Geralt, <<"Hi!">>)),
        FromAlice = escalus_client:wait_for_stanza(Geralt),
        escalus:assert(is_chat_message, [<<"Hi!">>], FromAlice),
        escalus:assert(has_ns, [<<"jabber:client">>], FromAlice),

        escalus_client:send(Geralt, escalus_stanza:chat_to(Alice, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>], escalus_client:wait_for_stanza(Alice)),

        escalus_client:send(Geralt, escalus_stanza:chat_to(Carol, <<"Hey!">>)),
        escalus_assert:is_chat_message(<<"Hey!">>, escalus_client:wait_for_stanza(Carol))

        end).

escape_chat_msg(Config) ->
    escalus:story(Config, [{alice, 1}, {?config(user, Config), 1}, {carol, 1}],
                  fun(Alice, Geralt, Carol) ->
        special_chars_helper:check_cdata_from_to(Alice, Geralt, <<"Hi! & < >">>),
        special_chars_helper:check_cdata_from_to(Geralt, Alice, <<"Hello! & < >">>),
        special_chars_helper:check_cdata_from_to(Geralt, Carol, <<"Hey! & < >">>)

    end).

escape_attrs(Config) ->
    escalus:story(Config, [{alice, 1}, {?config(user, Config), 1}, {carol, 1}],
                  fun(Alice, Geralt, Carol) ->
        special_chars_helper:check_attr_from_to(Alice, Geralt),
        special_chars_helper:check_attr_from_to(Geralt, Alice),
        special_chars_helper:check_attr_from_to(Geralt, Carol)

    end).
