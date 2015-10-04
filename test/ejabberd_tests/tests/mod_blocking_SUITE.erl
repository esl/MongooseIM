-module(mod_blocking_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NS_BLOCKING, <<"urn:xmpp:blocking">>).
-define(TEST_HTTP_HOST, "http://localhost:5285").

all() -> [
    {group, basic},
    {group, blocklist_management},
    {group, blocking_effects},
%%     {group, errors},
    {group, legacy_cleanup}
].

groups() ->
    [{basic, [discovering_support]},
     {blocklist_management, % [shuffle, {repeat, 3}],
      [
          get_block_list,
          add_user_to_blocklist,
          add_another_user_to_blocklist,
          add_many_users_to_blocklist,
          remove_user_from_blocklist,
          remove_many_user_from_blocklist,
          clear_blocklist,
          add_and_remove_many_to_blocklist
      ]},
     {blocking_effects, [{repeat, 2}],
      [
          messages_from_blocked_user_dont_arrive,
          messages_from_unblocked_user_arrive_again,
          messages_from_any_blocked_resource_dont_arrive,
          blocking_doesnt_interfere
      ]},
     {errors,
      [
          blocker_cant_send_to_blockee]},
     {legacy_cleanup,
      [noone_gets_custom_block_notifications,
       noone_gets_blocked_notification,
       noone_gets_unblock_notification,
       noone_gets_unblocked_notification,
       clear_blocklist_notifications
      ]}
    ].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(Name, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob,carol,mike, geralt]}).

end_per_group(Name, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob,carol, mike, geralt]}).







init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).



%%
%% CT tests
%%

discovering_support(Config) ->
    escalus:story(
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
    escalus:story(
        Config, [{alice, 1}],
        fun(User1) ->
            Result = get_blocklist(User1),
            io:format("Received stanza is ~n~p~n",[Result]),
            escalus:assert(is_iq_result, Result),
            escalus:assert(fun is_blocklist_result_empty/1, Result)
        end).

add_user_to_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            %% given
            user_blocks(User1, [User2]),
            %% when
            BlockList = get_blocklist(User1),
            io:format("Received stanza is ~n~p~n",[BlockList]),
            %% then
            blocklist_contains_jid(BlockList, User2)
        end).
add_another_user_to_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {mike, 1}],
        fun(User1, User2) ->
            %% given
            user_blocks(User1, [User2]),
            %% when
            BlockList = get_blocklist(User1),
            io:format("Received stanza is ~n~p~n",[BlockList]),
            %% then
            blocklist_contains_jid(BlockList, User2)
        end).

add_many_users_to_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {carol, 1}, {mike,1}],
        fun(User1, User2, User3, User4) ->
            %% given
            user_blocks(User1, [User2, User3, User4]),
            %% when
            BlockList = get_blocklist(User1),
            io:format("Received stanza is ~n~p~n",[BlockList]),
            %% then
            blocklist_contains_jid(BlockList, User2),
            blocklist_contains_jid(BlockList, User3),
            blocklist_contains_jid(BlockList, User4)
        end).


add_and_remove_many_to_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {carol, 1}, {mike,1}],
        fun(User1, User2, User3, User4) ->
            %% given
            user_blocks(User1, [User2, User3, User4]),
            user_blocks(User1, [User3, User4]),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User2),
            blocklist_contains_jid(BlockList, User3),
            blocklist_contains_jid(BlockList, User4),
            user_unblocks(User1, [User4]),
            BlockList2 = get_blocklist(User1),
            blocklist_contains_jid(BlockList2, User2),
            blocklist_contains_jid(BlockList2, User3),
            blocklist_doesnt_contain_jid(BlockList2, User4),
            message_is_delivered(User1, [User4], <<"Ni hao.">>),
            message_is_not_delivered(User1, [User2, User3], <<"Ni hao.">>)
        end).

noone_gets_custom_block_notifications(Config) ->
    escalus:story(
        Config, [{alice, 2}, {bob, 2}],
        fun(User1a, User1b, User2a, _) ->
            %% when
            user_blocks(User1a, [User2a]),
            %% then
            client_gets_block_iq(User1b)
        end).

noone_gets_blocked_notification(Config) ->
    escalus:story(
        Config, [{alice, 2}, {bob, 2}],
        fun(User1a, _, User2a, User2b) ->
            %% when
            user_blocks(User1a, [User2a]),
            %% then
            client_gets_nothing(User2b)
        end).

noone_gets_unblock_notification(Config) ->
    escalus:story(
        Config, [{alice, 2}, {bob, 2}],
        fun(User1a, User1b, User2a, User2b) ->
            %% given
            user_blocks(User1a, [User2a]),
            client_gets_block_iq(User1b),

            %% when
            user_unblocks(User1a, User2a),

            %% then
            client_gets_unblock_iq(User1b),
            client_has_no_messages(User2b),
            client_has_no_messages(User1b),
            client_has_no_messages(User1a)
        end).

noone_gets_unblocked_notification(Config) ->
    escalus:story(
        Config, [{alice, 2}, {bob, 2}],
        fun(User1a, User1b, User2a, User2b) ->
            %% given
            user_blocks(User1a, [User2a]),
            client_gets_block_iq(User1b),

            %% when
            user_unblocks(User1a, User2a),

            %% then
            client_has_no_messages(User2a),
            client_has_no_messages(User2b)
        end).

messages_from_blocked_user_dont_arrive(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            %% given
            user_blocks(User1, [User2]),
            %% when
            message_sent(User2, User1, <<"Hi!">>, <<"id-199">>),
            %% then
            client_gets_nothing(User1)
        end).

remove_user_from_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            %% given
            user_blocks(User1, [User2]),
            %% when
            user_unblocks(User1, User2),
            %% then
            NewList = get_blocklist(User1),
            blocklist_doesnt_contain_jid(NewList, User2)
        end).

remove_many_user_from_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            %% given
            user_blocks(User1, [User2, User3]),
            %% when
            user_unblocks(User1, [User2, User3]),
            %% then
            NewList = get_blocklist(User1),
            blocklist_doesnt_contain_jid(NewList, User2),
            blocklist_doesnt_contain_jid(NewList, User3)
        end).

messages_from_unblocked_user_arrive_again(Config) ->
    escalus:story(
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
    escalus:story(
        Config, [{alice, 3}, {bob, 1}],
        fun(User1a, User1b, User1c, User2) ->
            %% given
            user_blocks(User2, [User1a]),
            %% then
            message_is_not_delivered(User1a, [User2], <<"roar!">>),
            message_is_not_delivered(User1b, [User2], <<"woof!">>),
            message_is_not_delivered(User1c, [User2], <<"grrr!">>)
        end).

blocking_works_one_way(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            %% given
            user_blocks(User2, [User1]),
            %% then
            message_is_not_delivered(User1, [User2], <<"!@#@$@#$%">>),
            message_is_delivered(User2, [User1], <<"silence is golden">>)
        end).

blocking_doesnt_interfere(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            %% given
            user_blocks(User1, [User2]),
            %% then
            message_is_not_delivered(User2, [User1], <<"!@#@$@#$%">>),
            message_is_delivered(User3, [User1], <<"Ni hao.">>)
        end).

blocker_cant_send_to_blockee(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            %% given
            user_blocks(User1, [User2]),
            %% when
            message_sent(User1, User2,
                         <<"I'm not talking to you!">>, <<"id-392">>),
            %% then
            client_gets_blocking_error(User1)
        end).

clear_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            %% given
            user_blocks(User1, [User2, User3]),
            %% when
            user_unblocks_all(User1),
            %% then
            NewList = get_blocklist(User1),
            blocklist_is_empty(NewList)
        end).

clear_blocklist_notifications(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            %% given
            user_blocks(User1, [User2, User3]),
            %% when
            user_unblocks_all(User1),
            %% then
            client_gets_nothing(User2),
            client_gets_nothing(User3)
        end).

%%
%%  Test language
%%

user_blocks(Blocker, Blockees) when is_list(Blockees) ->
    BlockeeJIDs = [ escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Blockees ],
    AddStanza = block_users_stanza(BlockeeJIDs),
    escalus_client:send(Blocker, AddStanza),
    AddResult = escalus:wait_for_stanza(Blocker),
    BlockingPush = escalus:wait_for_stanza(Blocker),
    escalus:assert(is_iq_result, AddResult),
    escalus:assert(fun is_xep191_push/2,[<<"block">>],BlockingPush).

user_unblocks(Unblocker, Unblockees) when is_list(Unblockees) ->
    UnblockeeJIDs = [ escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Unblockees ],
    AddStanza = unblock_users_stanza(UnblockeeJIDs),
    escalus_client:send(Unblocker, AddStanza),
    AddResult = escalus:wait_for_stanza(Unblocker),
    BlockingPush = escalus:wait_for_stanza(Unblocker),
    escalus:assert(is_iq_result, AddResult),
    escalus:assert(fun is_xep191_push/2,[<<"unblock">>],BlockingPush);

user_unblocks(Unblocker, Unblockee) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Unblockee)),
    escalus_client:send(Unblocker, unblock_user_stanza(JID)),
    user_gets_remove_result(Unblocker),
    user_gets_xep0191_push(Unblocker).

user_unblocks_all(User) ->
    escalus_client:send(User, unblock_all_stanza()),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)),
    escalus:assert
    (fun is_xep191_push/2, [<<"unblock">>], escalus:wait_for_stanza(User)).

user_gets_remove_result(Client) ->
    RemoveResult = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_result, RemoveResult).

user_gets_xep0191_push(Client) ->
    RemovePush = escalus:wait_for_stanza(Client),
    escalus:assert(fun is_xep191_push/2,[<<"unblock">>],RemovePush).

blocklist_is_empty(BlockList) ->
    escalus:assert(is_iq_result, BlockList),
    escalus:assert(fun is_blocklist_result_empty/1, BlockList).

blocklist_doesnt_contain_jid(BlockList, Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    escalus:assert(is_iq_result, BlockList),
    ?assertNot(blocklist_result_has(JID, BlockList)).

blocklist_contains_jid(BlockList, Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    escalus:assert(fun blocklist_result_has/2, [JID], BlockList).

client_gets_blocking_error(C) ->
    Stanza = escalus_client:wait_for_stanza(C),
    escalus:assert(fun is_xep191_not_available/1, [], Stanza).

clients_get_block_iq(Cs) when is_list(Cs) -> [ client_gets_block_iq(C) || C <- Cs ].

client_gets_block_iq(C) ->
    escalus:assert(fun is_xep191_push/2, [<<"block">>], escalus:wait_for_stanza(C)).

clients_get_unblock_iq(Cs) when is_list(Cs) ->
    [ client_gets_unblock_iq(C) || C <- Cs ].

client_gets_unblock_iq(C) ->
    escalus:assert(fun is_xep191_push/2, [<<"unblock">>], escalus:wait_for_stanza(C)).

clients_have_no_messages(Cs) when is_list (Cs) -> [ client_has_no_messages(C) || C <- Cs ].

client_has_no_messages(C) -> escalus_assert:has_no_stanzas(C).

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

message_sent(From, To, MsgTxt, Id) ->
    chat_helper:message_sent(From, To, MsgTxt, Id),
    ok.

client_gets_nothing(Client) ->
    ct:sleep(500),
    escalus_assert:has_no_stanzas(Client).


%%
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
           children = Payload}.

block_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"block">>,
                     attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
                     children = Childs
    },
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}],
           children = Payload}.


block_user_stanza(UserToBlock) ->
    Payload = #xmlel{name = <<"block">>,
                     attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
                     children = [item_el(UserToBlock)]
    },
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}],
           children = Payload}.

unblock_user_stanza(UserToUnblock) ->
    Payload = #xmlel{name = <<"unblock">>,
                     attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
                     children = [item_el(UserToUnblock)]
    },
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}],
           children = Payload}.

unblock_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"unblock">>,
                     attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
                     children = Childs
    },
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}],
           children = Payload}.

unblock_all_stanza() ->
    Payload = #xmlel{name = <<"unblock">>,
                     attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
                     children = []
    },
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}],
           children = Payload}.

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
    % TODO maybe check items
    %ct:pal("push ~p",[Stanza]),
    true.
%%
%% helpers
%%
bare(C) ->  escalus_utils:jid_to_lower(escalus_client:short_jid(C)).

get_blocklist_items(Items) ->
    lists:map(fun(#xmlel{name = <<"item">>, attrs=A}) ->
        {_, R} = lists:keyfind(<<"jid">>, 1, A),
        R
              end, Items).
