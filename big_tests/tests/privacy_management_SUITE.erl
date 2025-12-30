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

-module(privacy_management_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus_xmlns.hrl").

-define(SLEEP_TIME, 50).

-import(privacy_helper, [assert_privacy_get_event/1,
                         assert_privacy_get_event/2,
                         assert_privacy_set_event/2,
                         assert_privacy_check_packet_event/3,
                         assert_privacy_push_item_event/2,
                         assert_privacy_push_item_event/3]).

-import(distributed_helper, [mim/0, rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, management}
    ].

groups() ->
    [
     {management, [parallel], management_test_cases()}
    ].

management_test_cases() ->
    [
     discover_support,
     get_all_lists,
     get_existing_list,
     get_many_lists,
     get_nonexistent_list,
     set_list,
     activate,
     activate_nonexistent,
     deactivate,
     default,
     %default_conflict,  % fails, as of bug #7073
     default_nonexistent,
     no_default,
     remove_list,
     get_all_lists_with_active,
     get_all_lists_with_default
    ].

blocking_test_cases() ->
    [
     block_jid_message,
     block_group_message,
     block_subscription_message,
     block_all_message,
     block_jid_presence_in,
     block_jid_presence_out,
     block_jid_iq,
     block_jid_all,
     block_jid_message_but_not_presence,
     newly_blocked_presense_jid_by_new_list,
     newly_blocked_presense_jid_by_list_change,
     newly_blocked_presence_not_notify_self,
     iq_reply_doesnt_crash_user_process,
     iq_with_to_attribute_is_treated_as_regular_one
    ].

allowing_test_cases() ->
    [allow_subscription_to_from_message,
     allow_subscription_both_message].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    ModConfig = [{mod_privacy, config_parser_helper:mod_config_with_auto_backend(mod_privacy)}],
    dynamic_modules:ensure_modules(HostType, ModConfig),
    instrument_helper:start(instrument_helper:declared_events(mod_privacy)),
    [{escalus_no_stanzas_after_story, true} | escalus:init_per_suite(Config1)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

discover_support(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Server = escalus_client:server(Alice),
        IqGet = escalus_stanza:disco_info(Server),
        Result = escalus:send_iq_and_wait_for_result(Alice, IqGet),
        escalus:assert(has_feature, [?NS_PRIVACY], Result)
    end).

get_all_lists(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result, escalus:wait_for_stanza(Alice)),
        assert_privacy_get_event(Alice)

        end).

get_all_lists_with_active(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_and_activate(Alice, {<<"deny_client">>, Bob}),

        TS = instrument_helper:timestamp(),
        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_active, [<<"deny_client">>],
                       escalus:wait_for_stanza(Alice)),
        assert_privacy_get_event(Alice, TS)

        end).

get_all_lists_with_default(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, {<<"deny_client">>, Bob}),
        privacy_helper:set_list(Alice, {<<"allow_client">>, Bob}),
        privacy_helper:set_default_list(Alice, <<"allow_client">>),

        TS = instrument_helper:timestamp(),
        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_default,
                       escalus:wait_for_stanza(Alice)),
        assert_privacy_get_event(Alice, TS)

        end).

get_nonexistent_list(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_lists([<<"public">>])),
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice)),
        assert_privacy_get_event(Alice)

        end).

get_many_lists(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_get_lists([<<"public">>, <<"private">>]),
        escalus_client:send(Alice, Request),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"modify">>, <<"bad-request">>),
        assert_privacy_get_event(Alice)

        end).

get_existing_list(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, {<<"deny_client">>, Bob}),

        TS = instrument_helper:timestamp(),
        escalus:send(Alice, escalus_stanza:privacy_get_lists([<<"deny_client">>])),
        Response = escalus:wait_for_stanza(Alice),

        <<"deny_client">> = exml_query:path(Response, [{element, <<"query">>},
                                                       {element, <<"list">>},
                                                       {attr, <<"name">>}]),
        assert_privacy_get_event(Alice, TS)

        end).

activate(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, {<<"deny_client">>, Bob}),

        Request = escalus_stanza:privacy_activate(<<"deny_client">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response),
        assert_privacy_set_event(Alice, #{active_count => 1})

        end).

activate_nonexistent(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_activate(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Response),
        assert_privacy_set_event(Alice, #{active_count => 1})

        end).

deactivate(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_deactivate(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response),
        assert_privacy_set_event(Alice, #{active_count => 1})

        end).

default(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, {<<"deny_client">>, Bob}),

        Request = escalus_stanza:privacy_set_default(<<"deny_client">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response),
        assert_privacy_set_event(Alice, #{default_count => 1})

        end).

default_conflict(Config) ->
    escalus:fresh_story(Config, [{alice, 2}, {bob, 1}], fun(Alice, Alice2, Bob) ->

        %% testcase setup
        %% setup list on server
        privacy_helper:send_set_list(Alice, {<<"deny_client">>, Bob}),
        privacy_helper:send_set_list(Alice, {<<"allow_client">>, Bob}),
        %% skip responses
        escalus_client:wait_for_stanzas(Alice, 4),
        %% make a default list for Alice2
        R1 = escalus_stanza:privacy_set_default(Alice2, <<"deny_client">>),
        escalus_client:send(Alice2, R1),
        escalus:assert_many([is_privacy_set, is_privacy_set, is_iq_result],
                            escalus_client:wait_for_stanzas(Alice2, 3)),
        %% setup done

        Request = escalus_stanza:privacy_set_default(<<"allow_client">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        %% TODO: should fail on this (result) and receive error
        %%       this is a bug and was filed to the esl redmine as Bug #7073
        %true = exmpp_iq:is_result(Response),
        %% but this should pass just fine
        escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Response),
        assert_privacy_set_event(Alice, #{default_count => 1})

        end).

default_nonexistent(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_set_default(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"cancel">>, <<"item-not-found">>),
        assert_privacy_set_event(Alice, #{default_count => 1})

        end).

no_default(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_no_default(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response),
        assert_privacy_set_event(Alice, #{default_count => 1})

        end).

set_list(Config) ->
    escalus:fresh_story(Config, [{alice, 3}, {bob, 1}], fun(Alice, Alice2, Alice3, Bob) ->

        privacy_helper:send_set_list(Alice, {<<"deny_client">>, Bob}),

        %% Verify that original Alice gets iq result and notification.
        %% It's a bit quirky as these come in undefined order
        %% (actually, they always came with 'push' first and then 'result',
        %% but I suppose it's not mandatory).
        AliceResponses = escalus_client:wait_for_stanzas(Alice, 2),
        escalus:assert_many([
            fun escalus_pred:is_iq_result/1,
            fun privacy_helper:is_privacy_list_push/1
        ], AliceResponses),

        %% Verify that other resources also get the push.
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Alice2)),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Alice3)),
        assert_privacy_push_item_event(Alice, 3)

        %% All in all, the spec requires the resources to reply
        %% (as to every iq), but it's omitted here.

        end).

remove_list(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:send_set_list(Alice, {<<"deny_client">>, Bob}),

        %% These are the pushed notification and iq result.
        escalus:assert_many([
            fun privacy_helper:is_privacy_list_push/1,
            is_iq_result
        ], escalus_client:wait_for_stanzas(Alice, 2)),
        assert_privacy_push_item_event(Alice, 1),

        %% Request list deletion by sending an empty list.
        TS = instrument_helper:timestamp(),
        RemoveRequest = escalus_stanza:privacy_set_list(
                          escalus_stanza:privacy_list(<<"someList">>, [])),
        escalus_client:send(Alice, RemoveRequest),

        %% These too are the pushed notification and iq result.
        escalus:assert_many([
            fun privacy_helper:is_privacy_list_push/1,
            is_iq_result
        ], escalus_client:wait_for_stanzas(Alice, 2)),
        assert_privacy_push_item_event(Alice, 1, TS),

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_lists([<<"someList">>])),

        %% Finally ensure that the list doesn't exist anymore.
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------
