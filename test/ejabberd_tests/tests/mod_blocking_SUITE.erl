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
        {group, manage},
        {group, effect},
        {group, offline},
        {group, errors},
        {group, pushes}
    ].

groups() ->
    [
        {manage, [parallel], manage_test_cases()},
        {effect, [parallel], effect_test_cases()},
        {offline, [sequence], offline_test_cases()},
        {errors, [parallel], error_test_cases()},
        {pushes, [parallel], push_test_cases()},
        {notify, [parallel], notify_test_cases()}
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
        blocking_doesnt_interfere
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
    escalus:create_users(Config, escalus:get_users([alice, bob, carol, mike, geralt])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, mike, geralt])).

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
        Config, [{alice, 1}, {bob, 1}, {carol, 1}, {mike,1}],
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
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            user_blocks(User1, [User2, User3]),
            user_unblocks(User1, [User2, User3]),
            NewList = get_blocklist(User1),
            blocklist_doesnt_contain_jid(NewList, User2),
            blocklist_doesnt_contain_jid(NewList, User3)
        end).

clear_blocklist(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
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
            message(User2, User1, <<"Hi!">>),
            client_gets_nothing(User1),
            privacy_helper:gets_error(User2, <<"cancel">>, <<"service-unavailable">>)
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
            message_is_not_delivered(User1a, [User2], <<"roar!">>),
            message_is_not_delivered(User1b, [User2], <<"woof!">>),
            message_is_not_delivered(User1c, [User2], <<"grrr!">>)
        end).

blocking_doesnt_interfere(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            %% given
            user_blocks(User1, [User2]),
            %% then
            message_is_not_delivered(User2, [User1], <<"!@#@$@#$%">>),
            message_is_delivered(User3, [User1], <<"Ni hao.">>)
        end).

messages_after_relogin(Config) ->
    %% given
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2])
        end),
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
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {carol, 1}, {mike, 1}, {geralt, 1}],
        fun(User1, User2, User3, User4, _) ->
            user_blocks(User1, [User2, User3]),
            user_blocks(User1, [User3, User4])
        end),
    %% when
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {carol, 1}, {mike, 1}, {geralt, 1}],
        fun(User1, User2, _, User4, User5) ->
            user_unblocks(User1,  [User4]),
            user_unblocks(User1,  [User4, User5]),
            user_unblocks(User1,  [User2, User5])
        end),
    %% then
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {carol, 1},  {mike, 1}, {geralt, 1}],
        fun(User1, User2, User3, User4, _) ->
            message_is_delivered(User1, [User4], <<"Under the bridge!">>),
            message_is_not_delivered(User1, [User3], <<"Cant stop">>),
            client_gets_blocking_error(User1),
            message_is_delivered(User1, [User2], <<"House of th rising sun">>),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User3)
        end).

clear_list_relogin(Config) ->
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

block_push_sent(Config) ->
    %% make sure privacy list push arrives to all the user's resources
    escalus:fresh_story(
        Config, [{alice, 2}, {bob, 2}],
        fun(User1a, User1b, User2a, User2b) ->
            user_blocks(User1a, [User2a]),
            client_gets_block_iq(User1b)
        end).

notify_blockee(Config) ->
    %% as XEP-0191 says, when we block a user he should receive 'unavailable', and a contrario.
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            %% make sure they're friends and Bob receives Alice's presences
            subscribe(Bob, Alice),
            escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
            escalus:assert(is_presence_with_type, [<<"available">>], escalus:wait_for_stanza(Alice)),
            escalus:assert(is_presence_with_type, [<<"available">>], escalus:wait_for_stanza(Bob)),
            user_blocks(Alice, [Bob]),
            escalus:assert(is_presence_with_type, [<<"unavailable">>], escalus:wait_for_stanza(Bob)),
            user_unblocks(Alice, [Bob]),
            escalus:assert(is_presence_with_type, [<<"available">>], escalus:wait_for_stanza(Bob))
        end).

%% common

%%
get_blocklist(User) ->
    IQGet = get_blocklist_stanza(),
    escalus_client:send(User, IQGet),
    escalus_client:wait_for_stanza(User).

%%
%% stanza generators
%%

get_blocklist_stanza() ->
    Payload = #xmlel{name = <<"blocklist">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}]},
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"get">>}],
        children = [Payload]}.

block_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"block">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = Childs
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.

block_users_stanza_with_white_spaces(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    % when client adds some white characters in blocking list
    WhiteSpacedChilds = Childs ++ [{xmlcdata, "\n"}],
    Payload = #xmlel{name = <<"block">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = WhiteSpacedChilds
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.


%%block_user_stanza(UserToBlock) ->
%%    Payload = #xmlel{name = <<"block">>,
%%        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
%%        children = [item_el(UserToBlock)]
%%    },
%%    #xmlel{name = <<"iq">>,
%%        attrs = [{<<"type">>, <<"set">>}],
%%        children = Payload}.

unblock_user_stanza(UserToUnblock) ->
    Payload = #xmlel{name = <<"unblock">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = [item_el(UserToUnblock)]
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.

unblock_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"unblock">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = Childs
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.

unblock_all_stanza() ->
    Payload = #xmlel{name = <<"unblock">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = []
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.

item_el(User) when is_binary(User) ->
    #xmlel{name = <<"item">>,
        attrs = [{<<"jid">>, User}]}.
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


is_blocklist_result_empty(#xmlel{children = [#xmlel{name =Name,
    attrs = Attrs,
    children= Child}]} = Stanza) ->
    true = escalus_pred:is_iq(Stanza),
    <<"blocklist">> = Name,
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    [] = Child,
    true.

blocklist_result_has(ExpectedUser, Stanza) ->
    true = escalus_pred:is_iq(Stanza),
    Blocklist = hd(Stanza#xmlel.children),
    Attrs = Blocklist#xmlel.attrs,
    Children = Blocklist#xmlel.children,
    <<"blocklist">> = Blocklist#xmlel.name,
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    true == lists:member(ExpectedUser,get_blocklist_items(Children)).

is_xep191_push(Type, #xmlel{attrs = A, children = [#xmlel{name = Type,
    attrs = Attrs}]}=Stanza) ->
    true = escalus_pred:is_iq_set(Stanza),
    {<<"id">>, <<"push">>} = lists:keyfind(<<"id">>, 1, A),
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    true.

is_xep191_push(Type, [], #xmlel{children = [#xmlel{name = Type, children = []}]}=Stanza) ->
    is_xep191_push(Type, Stanza);
is_xep191_push(Type, [], #xmlel{children = [#xmlel{name = Type, children = Items}]}) ->
    ct:pal("JIDs: should be empty contains, ~p", [Items]),
    false;
is_xep191_push(Type, JIDs, #xmlel{attrs = A, children = [#xmlel{name = Type,
    attrs = Attrs, children = Items}]}=Stanza) ->
    true = escalus_pred:is_iq_set(Stanza),
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    F = fun(El) ->
        #xmlel{name = <<"item">>, attrs =  [{<<"jid">>, Value}]} = El,
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
        {_, R} = lists:keyfind(<<"jid">>, 1, A),
        R
              end, Items).

user_blocks(Blocker, Blockees) when is_list(Blockees) ->
    BlockeeJIDs = [ escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Blockees ],
    AddStanza = block_users_stanza(BlockeeJIDs),
    escalus_client:send(Blocker, AddStanza),
    Res = escalus:wait_for_stanzas(Blocker, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"block">>, BlockeeJIDs, E) end,
    Preds = [is_iq_result, CheckPush], %% why it sends additional presence from alice to alice, I don't know
    escalus:assert_many(Preds, Res).

blocklist_is_empty(BlockList) ->
    escalus:assert(is_iq_result, BlockList),
    escalus:assert(fun is_blocklist_result_empty/1, BlockList).

blocklist_contains_jid(BlockList, Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    escalus:assert(fun blocklist_result_has/2, [JID], BlockList).

user_unblocks(Unblocker, Unblockees) when is_list(Unblockees) ->
    UnblockeeJIDs = [ escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Unblockees ],
    AddStanza = unblock_users_stanza(UnblockeeJIDs),
    escalus_client:send(Unblocker, AddStanza),
    Res = escalus:wait_for_stanzas(Unblocker, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"unblock">>, UnblockeeJIDs, E) end,
    Preds = [is_iq_result, CheckPush],
    escalus:assert_many(Preds, Res);
user_unblocks(Unblocker, Unblockee) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Unblockee)),
    escalus_client:send(Unblocker, unblock_user_stanza(JID)),
    user_gets_remove_result(Unblocker, [JID]).

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

client_gets_nothing(Client) ->
    ct:sleep(500),
    escalus_assert:has_no_stanzas(Client).

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
    %% Alice receives subscription reqest
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

