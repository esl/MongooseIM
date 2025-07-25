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

-module(mod_blocking_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-import(config_parser_helper, [mod_config_with_auto_backend/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, manage},
        {group, effect},
        {group, offline},
        {group, errors},
        {group, pushes},
        {group, muc},
        {group, muc_light}
    ].

groups() ->
    [
         {manage, [parallel], manage_test_cases()},
         {effect, [parallel], effect_test_cases()},
         {offline, [sequence], offline_test_cases()},
         {errors, [parallel], error_test_cases()},
         {pushes, [parallel], push_test_cases()},
         {notify, [parallel], notify_test_cases()},
         {muc, [parallel], muc_test_cases()},
         {muc_light, [parallel], muc_light_test_cases()}
    ].

manage_test_cases() ->
    [
        discovering_support,
        get_block_list,
        add_user_to_blocklist,
        add_user_to_blocklist_with_white_spaces,
        add_another_user_to_blocklist,
        add_many_users_to_blocklist,
        remove_user_from_blocklist,
        remove_many_user_from_blocklist,
        clear_blocklist,
        invalid_block_request
    ].

effect_test_cases() ->
    [
        messages_from_blocked_user_dont_arrive,
        messages_from_unblocked_user_arrive_again,
        messages_from_any_blocked_resource_dont_arrive,
        blocking_doesnt_interfere,
        blocking_propagates_to_resources,
        iq_reply_doesnt_crash_user_process,
        iq_with_to_attribute_is_treated_as_regular_one
    ].

offline_test_cases() ->
    [
        messages_after_relogin,
        messages_arrive_after_unblock_and_relogin,
        blocking_and_relogin_many,
        clear_list_relogin
    ].

error_test_cases() ->
    [blocker_cant_send_to_blockee].

push_test_cases() ->
    [block_push_sent].

notify_test_cases() ->
    [notify_blockee].

muc_test_cases() ->
    [messages_from_blocked_user_dont_arrive_muc].

muc_light_test_cases() ->
    [messages_from_blocked_user_dont_arrive_muc_light].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    ModConfig = [{mod_blocking, mod_config_with_auto_backend(mod_blocking)}],
    dynamic_modules:ensure_modules(HostType, ModConfig),
    instrument_helper:start(instrument_helper:declared_events(mod_privacy)),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

init_per_group(muc, Config) ->
    muc_helper:load_muc(),
    mongoose_helper:ensure_muc_clean(),
    init_per_group(generic, Config);
init_per_group(muc_light, Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    MucLightConfig = config_parser_helper:mod_config(mod_muc_light, #{backend => Backend}),
    dynamic_modules:ensure_modules(HostType, [{mod_muc_light, MucLightConfig}]),
    init_per_group(generic, Config1);
init_per_group(_GroupName, Config) ->
    escalus_fresh:create_users(Config, escalus:get_users([alice, bob, kate, mike, john])).

end_per_group(muc, Config) ->
    mongoose_helper:ensure_muc_clean(),
    muc_helper:unload_muc(),
    Config;
end_per_group(muc_light, Config) ->
    muc_light_helper:clear_db(domain_helper:host_type()),
    dynamic_modules:restore_modules(Config),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

discovering_support(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}],
        fun(User1) ->
            Server = escalus_client:server(User1),
            IqGet = escalus_stanza:disco_info(Server),
            escalus_client:send(User1, IqGet),
            Result = escalus_client:wait_for_stanza(User1),
            escalus:assert(is_iq_result, [IqGet], Result),
            escalus:assert(has_feature, [?NS_BLOCKING], Result)
        end).


get_block_list(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}],
        fun(User1) ->
            Result = get_blocklist(User1),
            escalus:assert(is_iq_result, Result),
            escalus:assert(fun is_blocklist_result_empty/1, Result)
        end).

add_user_to_blocklist(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User2)
        end).

add_user_to_blocklist_with_white_spaces(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            BlockeeJIDs = [escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- [User2]],
            AddStanza = block_users_stanza_with_white_spaces(BlockeeJIDs),
            escalus_client:send(User1, AddStanza),
            Res = escalus:wait_for_stanza(User1),
            escalus:assert(is_iq_result, Res)
        end).

add_another_user_to_blocklist(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {mike, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User2)
        end).

add_many_users_to_blocklist(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
        fun(User1, User2, User3, User4) ->
            user_blocks(User1, [User2, User3, User4]),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User2),
            blocklist_contains_jid(BlockList, User3),
            blocklist_contains_jid(BlockList, User4)
        end).

remove_user_from_blocklist(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            user_unblocks(User1, User2),
            NewList = get_blocklist(User1),
            blocklist_doesnt_contain_jid(NewList, User2)
        end).

remove_many_user_from_blocklist(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(User1, User2, User3) ->
            user_blocks(User1, [User2, User3]),
            user_unblocks(User1, [User2, User3]),
            NewList = get_blocklist(User1),
            blocklist_doesnt_contain_jid(NewList, User2),
            blocklist_doesnt_contain_jid(NewList, User3)
        end).

clear_blocklist(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(User1, User2, User3) ->
            user_blocks(User1, [User2, User3]),
            user_unblocks_all(User1),
            NewList = get_blocklist(User1),
            blocklist_is_empty(NewList)
        end).

invalid_block_request(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}],
        fun(User1) ->
            St = block_users_stanza([]),
            escalus_client:send(User1, St),
            privacy_helper:gets_error(User1, <<"modify">>, <<"bad-request">>)
        end).

messages_from_blocked_user_dont_arrive(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            TS = instrument_helper:timestamp(),
            message(User2, User1, <<"Hi!">>),
            ct:sleep(100),
            escalus_assert:has_no_stanzas(User1),
            privacy_helper:gets_error(User2, <<"cancel">>, <<"service-unavailable">>),
            privacy_helper:assert_privacy_check_packet_event(User2, #{dir => out}, TS),
            privacy_helper:assert_privacy_check_packet_event(User1, #{dir => in, blocked_count => 1}, TS)
        end).

messages_from_blocked_user_dont_arrive_muc(ConfigIn) ->
    muc_helper:story_with_room(ConfigIn, [], [{alice, 1}, {bob, 1}], fun(Config, Alice, Bob) ->
        RoomJid = ?config(room, Config),
        BobNick = escalus_utils:get_username(Bob),
        escalus:send(Bob, muc_helper:stanza_muc_enter_room(RoomJid, BobNick)),
        escalus:wait_for_stanzas(Bob, 2),
        AliceNick = escalus_utils:get_username(Alice),
        escalus:send(Alice, muc_helper:stanza_muc_enter_room(RoomJid, AliceNick)),
        escalus:wait_for_stanza(Bob),
        escalus:wait_for_stanzas(Alice, 3),

        user_blocks(Alice, [Bob], muc),
        TS = instrument_helper:timestamp(),
        Stanza = escalus_stanza:groupchat_to(muc_helper:room_address(?config(room, Config)), <<"Hello">>),
        escalus:send(Bob, Stanza),
        ct:sleep(100),
        % We don't expect Bob to get an error from Alice as this would lead to
        % her being kicked out of the room
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:assert_privacy_check_packet_event(Bob, #{dir => out}, TS),
        privacy_helper:assert_privacy_check_packet_event(Alice, #{dir => in, blocked_count => 1}, TS)
    end).

messages_from_blocked_user_dont_arrive_muc_light(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomName = <<"blocking-testroom">>,
        muc_light_helper:create_room(RoomName, muc_light_helper:muc_host(),
                                     Alice, [Bob], Config, muc_light_helper:ver(1)),

        user_blocks(Alice, [Bob], muc_light),
        TS = instrument_helper:timestamp(),
        Stanza = escalus_stanza:groupchat_to(muc_light_helper:room_bin_jid(RoomName), <<"Hello">>),
        escalus:send(Bob, Stanza),
        ct:sleep(100),
        % We don't expect Bob to get an error from Alice as this would lead to
        % her being kicked out of the room
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:assert_privacy_check_packet_event(Bob, #{dir => out}, TS),
        privacy_helper:assert_privacy_check_packet_event(Alice, #{dir => in, blocked_count => 1}, TS)
    end).

messages_from_unblocked_user_arrive_again(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            %% given
            user_blocks(User1, [User2]),
            %% when
            user_unblocks(User1, User2),
            %% then
            message_is_delivered(User2, User1, <<"Hello again!">>)
        end).

messages_from_any_blocked_resource_dont_arrive(Config) ->
    escalus:fresh_story(
        Config, [{alice, 3}, {bob, 1}],
        fun(User1a, User1b, User1c, User2) ->
            %% given
            user_blocks(User2, [User1a]),
            %% then
            message_is_blocked_by_recipient(User1a, User2),
            message_is_blocked_by_recipient(User1b, User2),
            message_is_blocked_by_recipient(User1c, User2),
            ct:sleep(100),
            escalus_assert:has_no_stanzas(User2)
        end).

blocking_doesnt_interfere(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(User1, User2, User3) ->
            %% given
            user_blocks(User1, [User2]),
            %% then
            message_is_blocked_by_recipient(User2, User1),
            message_is_delivered(User3, [User1], <<"Ni hao.">>)
        end).

blocking_propagates_to_resources(Config) ->
    escalus:fresh_story(
        Config, [{alice, 2}, {bob, 1}],
        fun(User1a, User1b, User2) ->
            %% given
            user_blocks(User1a, [User2]),
            %% then
            client_gets_block_iq(User1b),
            % Alice can't send from any of her resources
            message_is_blocked_by_sender(User1a, User2),
            message_is_blocked_by_sender(User1b, User2),
            % Bob can't send to any of Alice's resources
            message_is_blocked_by_recipient(User2, User1a),
            message_is_blocked_by_recipient(User2, User1b)
        end).

iq_reply_doesnt_crash_user_process(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        QueryWithBlockingNS = escalus_stanza:query_el(?NS_BLOCKING, []),
        %% Send IQ reply with blocking ns
        %% Send message to check user process still alive
        privacy_helper:does_user_process_crash(Alice,
            Bob,
            <<"error">>,
            QueryWithBlockingNS,
            <<"Hello, Bob">>),

        privacy_helper:does_user_process_crash(Bob,
            Alice,
            <<"result">>,
            QueryWithBlockingNS,
            <<"Hello, Alice">>)
    end).

messages_after_relogin(Config) ->
    %% given
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2])
        end),
    %% XXX Because alice can receive presence unavalable from alice
    %% XXX It's a potential bug, please test it.
    %% XXX has_stanzas_but_shouldnt
    %% XXX reported as https://github.com/esl/MongooseIM/issues/1799
    mongoose_helper:kick_everyone(),
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            message_is_not_delivered(User2, [User1], <<"Hey alice, are you there?">>),
            message_is_delivered(User1, [User1], <<"Hey bob, carpe diem!">>)
        end).

messages_arrive_after_unblock_and_relogin(Config) ->
    %% given when
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_unblocks(User1, [User2])
        end),
    %% then
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            message_is_delivered(User2, [User1], <<"Hey bob, are you there?">>)
        end).

blocking_and_relogin_many(Config) ->
    %% given
    simple_story(Config,
        fun(User1, User2, User3, User4, _) ->
            user_blocks(User1, [User2, User3]),
            user_blocks(User1, [User3, User4])
        end),
    %% when
    simple_story(Config,
        fun(User1, User2, _, User4, User5) ->
            user_unblocks(User1,  [User4]),
            user_unblocks(User1,  [User4, User5]),
            user_unblocks(User1,  [User2, User5])
        end),
    %% then
    simple_story(Config,
        fun(User1, User2, User3, User4, _) ->
            message_is_delivered(User1, [User4], <<"Under the bridge!">>),
            message_is_not_delivered(User1, [User3], <<"Cant stop">>),
            client_gets_blocking_error(User1),
            message_is_delivered(User1, [User2], <<"House of th rising sun">>),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User3)
        end).

simple_story(Config, Fun) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}, {john, 1}],
        Fun
    ).

clear_list_relogin(Config) ->
    %% unexpected presence unavailable
    mongoose_helper:kick_everyone(),
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2])
        end),
    escalus:story(
        Config, [{alice, 1}],
        fun(User1) ->
            user_unblocks_all(User1)
        end),
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            message_is_delivered(User1, [User2], <<"Doom and gloom!">>)
        end).

blocker_cant_send_to_blockee(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            message(User1, User2, <<"I'm not talking to you!">>),
            client_gets_blocking_error(User1)
        end).

%% This test checks an edge case where a blocking IQ is sent to another user
%% This isn't allowed by the XEP, but the test ensures MIM handles it correctly
iq_with_to_attribute_is_treated_as_regular_one(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(User1, User2, User3) ->
            %% Alice sends a blocking IQ addressed to Bob
            Blockee = escalus_utils:jid_to_lower(escalus_client:short_jid(User3)),
            St = block_users_stanza([Blockee]),
            StanzaBlock = escalus_stanza:to(St, User2),
            escalus_client:send(User1, StanzaBlock),
            %% Bob should receive the blocking IQ sent by Alice
            StanzaReceived = escalus:wait_for_stanza(User2),
            escalus:assert(is_iq_set, StanzaReceived),
            %% Alice shouldn't receive any response from the server
            [] = escalus:wait_for_stanzas(User1, 1, 100),
            escalus_assert:has_no_stanzas(User1)
        end).

block_push_sent(Config) ->
    %% make sure privacy list push arrives to all the user's resources
    escalus:fresh_story(
        Config, [{alice, 2}, {bob, 2}],
        fun(User1a, User1b, User2a, _User2b) ->
            user_blocks(User1a, [User2a]),
            client_gets_block_iq(User1b),
            privacy_helper:assert_privacy_push_item_event(User1a, 2)
        end).

notify_blockee(Config) ->
    %% as XEP-0191 says, when we block a user he should receive 'unavailable', and a contrario.
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            %% make sure they're friends and Bob receives Alice's presences
            subscribe(Bob, Alice),
            escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
            escalus:assert(is_presence_with_type, [<<"available">>],
                           escalus:wait_for_stanza(Alice)),
            escalus:assert(is_presence_with_type, [<<"available">>],
                           escalus:wait_for_stanza(Bob)),
            user_blocks(Alice, [Bob]),
            escalus:assert(is_presence_with_type, [<<"unavailable">>],
                           escalus:wait_for_stanza(Bob)),
            user_unblocks(Alice, [Bob]),
            escalus:assert(is_presence_with_type, [<<"available">>],
                           escalus:wait_for_stanza(Bob))
        end).

%% common

%%
get_blocklist(User) ->
    TS = instrument_helper:timestamp(),
    IQGet = get_blocklist_stanza(),
    escalus_client:send(User, IQGet),
    Result = escalus_client:wait_for_stanza(User),
    privacy_helper:assert_privacy_get_event(User, TS),
    Result.

%%
%% stanza generators
%%

get_blocklist_stanza() ->
    Payload = #xmlel{name = <<"blocklist">>,
                     attrs=#{<<"xmlns">> => ?NS_BLOCKING}},
    #xmlel{name = <<"iq">>,
           attrs = #{<<"type">> => <<"get">>},
           children = [Payload]}.

block_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"block">>,
                     attrs=#{<<"xmlns">> => ?NS_BLOCKING},
                     children = Childs},
    set_iq(Payload).

block_users_stanza_with_white_spaces(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    % when client adds some white characters in blocking list
    WhiteSpacedChilds = Childs ++ [#xmlcdata{content = "\n"}],
    Payload = #xmlel{name = <<"block">>,
                     attrs=#{<<"xmlns">> => ?NS_BLOCKING},
                     children = WhiteSpacedChilds},
    set_iq(Payload).


block_user_stanza(UserToBlock) ->
   Payload = #xmlel{name = <<"block">>,
                    attrs=#{<<"xmlns">> => ?NS_BLOCKING},
                    children = [item_el(UserToBlock)]},
   set_iq(Payload).

unblock_user_stanza(UserToUnblock) ->
    Payload = #xmlel{name = <<"unblock">>,
                     attrs=#{<<"xmlns">> => ?NS_BLOCKING},
                     children = [item_el(UserToUnblock)]},
    set_iq(Payload).

unblock_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"unblock">>,
                     attrs=#{<<"xmlns">> => ?NS_BLOCKING},
                     children = Childs},
    set_iq(Payload).

unblock_all_stanza() ->
    Payload = #xmlel{name = <<"unblock">>,
        attrs= #{<<"xmlns">> => ?NS_BLOCKING},
        children = []
    },
    set_iq(Payload).

set_iq(Payload) ->
    #xmlel{name = <<"iq">>,
           attrs = #{<<"type">> => <<"set">>},
           children = [Payload]}.

item_el(User) when is_binary(User) ->
    #xmlel{name = <<"item">>,
        attrs = #{<<"jid">> => User}}.
%%
%% predicates
%%

is_xep191_not_available(#xmlel{} = Stanza) ->
    ErrorEl = exml_query:subelement(Stanza, <<"error">>),
    <<"error">> == exml_query:attr(Stanza, <<"type">>)
        andalso
        undefined =/= exml_query:subelement(ErrorEl, <<"not-acceptable">>)
        andalso
        undefined =/= exml_query:subelement(ErrorEl, <<"blocked">>)
        andalso
        <<"urn:xmpp:blocking:errors">> ==
            exml_query:path(ErrorEl, [{element, <<"blocked">>},
                {attr, <<"xmlns">>}]).


is_blocklist_result_empty(#xmlel{children = [Child]} = Stanza) ->
    true = escalus_pred:is_iq(Stanza),
    #xmlel{name = <<"blocklist">>,
           attrs = #{<<"xmlns">> := ?NS_BLOCKING},
           children = []} = Child,
    true.

blocklist_result_has(ExpectedUser, Stanza) ->
    true = escalus_pred:is_iq(Stanza),
    Blocklist = hd(Stanza#xmlel.children),
    #{<<"xmlns">> := ?NS_BLOCKING} = Blocklist#xmlel.attrs,
    Children = Blocklist#xmlel.children,
    <<"blocklist">> = Blocklist#xmlel.name,
    true == lists:member(ExpectedUser, get_blocklist_items(Children)).

is_xep191_push(Type, #xmlel{attrs = A, children = [#xmlel{name = Type,
                                                          attrs = Attrs}]}=Stanza) ->
    true = escalus_pred:is_iq_set(Stanza),
    #{<<"xmlns">> := ?NS_BLOCKING} = Attrs,
    #{<<"id">> := <<"push">>} = A,
    true.

is_xep191_push(Type, [], #xmlel{children = [#xmlel{name = Type, children = []}]}=Stanza) ->
    is_xep191_push(Type, Stanza);
is_xep191_push(Type, [], #xmlel{children = [#xmlel{name = Type, children = _}]}) ->
    false;
is_xep191_push(Type, JIDs, #xmlel{children = [#xmlel{name = Type,
                                                     attrs = Attrs,
                                                     children = Items}]}=Stanza) ->
    true = escalus_pred:is_iq_set(Stanza),
    #{<<"xmlns">> := ?NS_BLOCKING} = Attrs,
    F = fun(El) ->
            #xmlel{name = <<"item">>, attrs = #{<<"jid">> := Value}} = El,
            lists:member(Value, JIDs)
        end,
    TrueList = lists:map(F, Items),
    lists:all(fun(El) -> El end, TrueList);
is_xep191_push(_, _, _) ->
    false.

%%
%% helpers
%%

bare(C) ->  escalus_utils:jid_to_lower(escalus_client:short_jid(C)).

get_blocklist_items(Items) ->
    lists:map(fun(#xmlel{name = <<"item">>, attrs=A}) ->
                  maps:get(<<"jid">>, A)
              end, Items).

user_blocks(Blocker, Blockees) ->
    user_blocks(Blocker, Blockees, pm).

user_blocks(Blocker, Blockees, ChatType) when is_list(Blockees) ->
    TS = instrument_helper:timestamp(),
    BlockeeJIDs = blockee_jids(Blockees, ChatType),
    AddStanza = block_users_stanza(BlockeeJIDs),
    escalus_client:send(Blocker, AddStanza),
    Res = escalus:wait_for_stanzas(Blocker, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"block">>, BlockeeJIDs, E) end,
    Preds = [is_iq_result, CheckPush],
    escalus:assert_many(Preds, Res),
    privacy_helper:assert_privacy_set_event(Blocker, #{}, TS).

blockee_jids(Blockees, pm) ->
    [escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Blockees];
blockee_jids(Blockees, muc) ->
    MucHost = muc_helper:muc_host(),
    [<<MucHost/binary, "/", (escalus_client:username(B))/binary>> || B <- Blockees];
blockee_jids(Blockees, muc_light) ->
    MucHost = muc_light_helper:muc_host(),
    [<<MucHost/binary, "/", (escalus_client:short_jid(B))/binary>> || B <- Blockees].

blocklist_is_empty(BlockList) ->
    escalus:assert(is_iq_result, BlockList),
    escalus:assert(fun is_blocklist_result_empty/1, BlockList).

blocklist_contains_jid(BlockList, Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    escalus:assert(fun blocklist_result_has/2, [JID], BlockList).

user_unblocks(Unblocker, Unblockees) when is_list(Unblockees) ->
    TS = instrument_helper:timestamp(),
    UnblockeeJIDs = [ escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Unblockees ],
    AddStanza = unblock_users_stanza(UnblockeeJIDs),
    escalus_client:send(Unblocker, AddStanza),
    Res = escalus:wait_for_stanzas(Unblocker, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"unblock">>, UnblockeeJIDs, E) end,
    Preds = [is_iq_result, CheckPush],
    escalus:assert_many(Preds, Res),
    privacy_helper:assert_privacy_set_event(Unblocker, #{}, TS);
user_unblocks(Unblocker, Unblockee) ->
    TS = instrument_helper:timestamp(),
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Unblockee)),
    escalus_client:send(Unblocker, unblock_user_stanza(JID)),
    user_gets_remove_result(Unblocker, [JID]),
    privacy_helper:assert_privacy_set_event(Unblocker, #{}, TS).

blocklist_doesnt_contain_jid(BlockList, Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    escalus:assert(is_iq_result, BlockList),
    ?assertNot(blocklist_result_has(JID, BlockList)).

user_gets_remove_result(Client, ContactList) ->
    RemoveResult = escalus:wait_for_stanzas(Client, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"unblock">>, ContactList, E) end,
    Preds = [is_iq_result, CheckPush],
    escalus:assert_many(Preds, RemoveResult).


user_unblocks_all(User) ->
    escalus_client:send(User, unblock_all_stanza()),
    user_gets_remove_result(User, []).

message(From, To, MsgTxt) ->
    escalus_client:send(From, escalus_stanza:chat_to(To, MsgTxt)).

message_is_delivered(From, [To|_] = Tos, MessageText) ->
    BareTo = escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    TS = instrument_helper:timestamp(),
    escalus:send(From, escalus_stanza:chat_to(BareTo, MessageText)),
    [ escalus:assert(is_chat_message, [MessageText], escalus:wait_for_stanza(C)) ||
        C <- Tos ],
    privacy_helper:assert_privacy_check_packet_event(From, #{dir => out}, TS),
    privacy_helper:assert_privacy_check_packet_event(To, #{dir => in}, TS);
message_is_delivered(From, To, MessageText) ->
    BareTo =  escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    TS = instrument_helper:timestamp(),
    escalus:send(From, escalus_stanza:chat_to(BareTo, MessageText)),
    escalus:assert(is_chat_message, [MessageText], escalus:wait_for_stanza(To)),
    privacy_helper:assert_privacy_check_packet_event(From, #{dir => out}, TS),
    privacy_helper:assert_privacy_check_packet_event(To, #{dir => in}, TS).

message_is_blocked_by_recipient(From, To) ->
    TS = instrument_helper:timestamp(),
    message_is_not_delivered(From, [To], <<"You blocked me!">>),
    privacy_helper:gets_error(From, <<"cancel">>, <<"service-unavailable">>),
    privacy_helper:assert_privacy_check_packet_event(From, #{dir => out}, TS),
    privacy_helper:assert_privacy_check_packet_event(To, #{dir => in, blocked_count => 1}, TS).

message_is_blocked_by_sender(From, To) ->
    TS = instrument_helper:timestamp(),
    message_is_not_delivered(From, [To], <<"I blocked you!">>),
    client_gets_blocking_error(From),
    privacy_helper:assert_privacy_check_packet_event(From, #{dir => out, blocked_count => 1}, TS).

message_is_not_delivered(From, [To|_] = Tos, MessageText) ->
    escalus:send(From, escalus_stanza:chat_to(To, MessageText)),
    clients_have_no_messages(Tos).

clients_have_no_messages(Cs) when is_list (Cs) -> [ client_has_no_messages(C) || C <- Cs ].

client_has_no_messages(C) -> escalus_assert:has_no_stanzas(C).

client_gets_blocking_error(C) ->
    Stanza = escalus_client:wait_for_stanza(C),
    escalus:assert(fun is_xep191_not_available/1, [], Stanza).

client_gets_block_iq(C) ->
    escalus:assert(fun is_xep191_push/2, [<<"block">>], escalus:wait_for_stanza(C)).

flush(User) ->
    escalus:wait_for_stanzas(User, 10, 100).

add_sample_contact(Alice, Bob) ->
    escalus:send(Alice, escalus_stanza:roster_add_contact(Bob,
        [<<"friends">>],
        <<"Bobby">>)),

    Received = escalus:wait_for_stanzas(Alice, 2),
    escalus:assert_many([is_roster_set, is_iq_result], Received),

    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus:assert(count_roster_items, [1], Result),
    escalus:send(Alice, escalus_stanza:iq_result(Result)).


subscribe(Bob, Alice) ->
    %% Bob adds Alice as a contact
    add_sample_contact(Bob, Alice),
    %% He subscribes to her presences
    escalus:send(Bob, escalus_stanza:presence_direct(alice, <<"subscribe">>)),
    PushReq = escalus:wait_for_stanza(Bob),
    escalus:assert(is_roster_set, PushReq),
    escalus:send(Bob, escalus_stanza:iq_result(PushReq)),
    %% Alice receives subscription request
    Received = escalus:wait_for_stanza(Alice),
    escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),
    %% Alice adds new contact to his roster
    escalus:send(Alice, escalus_stanza:roster_add_contact(Bob,
        [<<"enemies">>],
        <<"Bob">>)),
    PushReqB = escalus:wait_for_stanza(Alice),
    escalus:assert(is_roster_set, PushReqB),
    escalus:send(Alice, escalus_stanza:iq_result(PushReqB)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
    %% Alice sends subscribed presence
    escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribed">>)),
    %% Bob receives subscribed
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
    check_subscription_stanzas(Stanzas, <<"subscribed">>),
    escalus:assert(is_presence, escalus:wait_for_stanza(Bob)),
    %% Alice receives roster push
    PushReqB1 = escalus:wait_for_stanza(Alice),
    escalus:assert(is_roster_set, PushReqB1),
    %% Alice sends presence
    escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus:assert(is_presence, escalus:wait_for_stanza(Bob)),
    escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),
    ok.


check_subscription_stanzas(Stanzas, Type) ->
    IsPresWithType = fun(S) -> escalus_pred:is_presence_with_type(Type, S) end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas).
