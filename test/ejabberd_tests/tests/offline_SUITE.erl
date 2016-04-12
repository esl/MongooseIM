%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_offline* modules
%%% @end
%%%===================================================================

-module(offline_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(MAX_OFFLINE_MSGS, 100). % known server-side config

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, mod_offline_tests}].

all_tests() ->
    [offline_message_is_stored_and_delivered_at_login,
     error_message_is_not_stored,
     groupchat_message_is_not_stored,
     headline_message_is_not_stored,
     expired_messages_are_not_delivered,
     max_offline_messages_reached
    ].

groups() ->
    [{mod_offline_tests, [parallel, shuffle], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(C) -> escalus:init_per_suite(C).
end_per_suite(C) -> escalus_fresh:clean(), escalus:end_per_suite(C).
init_per_testcase(Name, C) -> escalus:init_per_testcase(Name, C).
end_per_testcase(Name, C) -> escalus:end_per_testcase(Name, C).

%%%===================================================================
%%% offline tests
%%%===================================================================

offline_message_is_stored_and_delivered_at_login(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
                logout(Bob),
                escalus:send(Alice, escalus_stanza:chat_to
                                      (Bob, <<"msgtxt">>)),
                NewBob = login_send_presence(FreshConfig, bob),
                Stanzas = escalus:wait_for_stanzas(NewBob, 2),
                escalus_new_assert:mix_match
                  ([is_presence, is_chat(<<"msgtxt">>)],
                   Stanzas)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

error_message_is_not_stored(Config) ->
    Story = fun(FreshConfig, Alice, Bob) ->
                    logout(Bob),
                    AliceJid = escalus_client:full_jid(Alice),
                    escalus:send(Alice, escalus_stanza:message
                              (AliceJid, Bob, <<"error">>, <<"msgtxt">>)),
                    NewBob = login_send_and_receive_presence(FreshConfig, bob),
                    ct:sleep(500),
                    false = escalus_client:has_stanzas(NewBob)
            end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

groupchat_message_is_not_stored(Config) ->
    Story = fun(FreshConfig, Alice, Bob) ->
                    logout(Bob),
                    AliceJid = escalus_client:full_jid(Alice),
                    escalus:send(Alice, escalus_stanza:message
                              (AliceJid, Bob, <<"groupchat">>, <<"msgtxt">>)),
                    NewBob = login_send_and_receive_presence(FreshConfig, bob),
                    ct:sleep(500),
                    false = escalus_client:has_stanzas(NewBob)
            end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

headline_message_is_not_stored(Config) ->
    Story = fun(FreshConfig, Alice, Bob) ->
                    logout(Bob),
                    AliceJid = escalus_client:full_jid(Alice),
                    escalus:send(Alice, escalus_stanza:message
                              (AliceJid, Bob, <<"headline">>, <<"msgtxt">>)),
                    NewBob = login_send_and_receive_presence(FreshConfig, bob),
                    ct:sleep(500),
                    false = escalus_client:has_stanzas(NewBob)
            end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

max_offline_messages_reached(Config) ->
    Story =
        fun(FreshConfig, Alice, B1, B2, B3, B4) ->
                BobsResources = [B1,B2,B3,B4],
                MessagesPerResource = ?MAX_OFFLINE_MSGS div length(BobsResources),

                logout(Alice),
                each_client_sends_messages_to(BobsResources, Alice,
                                              {count, MessagesPerResource}),

                send_message(B1, Alice, ?MAX_OFFLINE_MSGS+1),
                Packet = escalus:wait_for_stanza(B1),
                escalus:assert(is_error, [<<"wait">>, <<"resource-constraint">>], Packet),

                NewAlice = login_send_presence(FreshConfig, alice),
                Preds = [is_chat(make_chat_text(I))
                         || I <- repeat(lists:seq(1, MessagesPerResource),
                                           length(BobsResources))],
                escalus_new_assert:mix_match
                  ([is_presence | Preds],
                   escalus:wait_for_stanzas(NewAlice, ?MAX_OFFLINE_MSGS+1)),
                ct:sleep(500),
                false = escalus_client:has_stanzas(Alice)
            end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 4}], Story).

expired_messages_are_not_delivered(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
                BobJid = escalus_client:short_jid(Bob),
                logout(Bob),
                escalus:send(Alice,
                             make_message_with_expiry(BobJid, 600000, <<"long">>)),
                escalus:send(Alice,
                             make_message_with_expiry(BobJid, 1, <<"short">>)),

                ct:sleep(timer:seconds(2)),
                NewBob = login_send_presence(FreshConfig, bob),

                escalus_new_assert:mix_match
                  ([is_presence, is_chat(<<"long">>)],
                   escalus:wait_for_stanzas(NewBob, 2)),
                ct:sleep(500),
                false = escalus_client:has_stanzas(NewBob)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).


%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

%%%===================================================================
%%% Helpers
%%%===================================================================

logout(User) ->
    escalus_client:stop(User),
    timer:sleep(100).

login_send_presence(Config, User) ->
    {ok, Client} = escalus_client:start(Config, User, <<"new-session">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.

login_send_and_receive_presence(Config, User) ->
    Client = login_send_presence(Config, User),
    P = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_presence, P),
    Client.

each_client_sends_messages_to(Sources, Target, {count, N}) when is_list(Sources) ->
    par:map
        (fun(Source) ->
                 [ send_message(Source, Target, I) || I <- lists:seq(1,N) ]
         end,
         Sources).

send_message(From, To, I) ->
    escalus:send(From, escalus_stanza:chat_to(To, make_chat_text(I))),
    timer:sleep(100).

make_chat_text(I) ->
    Number = integer_to_binary(I),
    <<"Hi, Offline ", Number/binary>>.

make_message_with_expiry(Target, Expiry, Text) ->
    ExpiryBin = list_to_binary(integer_to_list(Expiry)),
    Stanza = escalus_stanza:chat_to(Target, Text),
    #xmlel{children = Children} = Stanza,
    ExpiryElem = #xmlel{name = <<"x">>,
                        attrs = [{<<"xmlns">>, <<"jabber:x:expire">>},
                                 {<<"seconds">>, ExpiryBin}]},
    Stanza#xmlel{children = [ExpiryElem | Children]}.

repeat(L,0) -> [];
repeat(L,N) -> L ++ repeat(L, N-1).
