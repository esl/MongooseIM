%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_offline* modules
%%% @end
%%%===================================================================

-module(offline_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MAX_OFFLINE_MSGS, 5).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, mod_offline_tests}].

all_tests() ->
    [simple_message,
     error_message,
     groupchat_message,
     headliine_message,
     max_offline_messages_reached].

groups() ->
    [{mod_offline_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1, {by_name, [alice, bob]}).

end_per_suite(Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(max_offline_messages_reached, Config) ->
    OldSettings = set_max_offline_messages([{5000,admin},
                                            {?MAX_OFFLINE_MSGS,all}]),
    escalus:init_per_testcase(max_offline_messages_reached,
                             [{max_offline, OldSettings} | Config]);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(max_offline_messages_reached, Config) ->
    OldSettings = ?config(max_offline, Config),
    set_max_offline_messages(OldSettings),
    escalus:end_per_testcase(max_offline_messages_reached, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% offline tests
%%%===================================================================

simple_message(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        escalus:send(Alice, escalus_stanza:chat_to(bob, <<"Hi, Offline!">>))
    end),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and the message
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"Hi, Offline!">>)],
                                 Stanzas),
    escalus_cleaner:clean(Config).

error_message(Config) ->
    not_stored_message(<<"error">>, Config).

groupchat_message(Config) ->
    not_stored_message(<<"groupchat">>, Config).

headliine_message(Config) ->
    not_stored_message(<<"headline">>, Config).

not_stored_message(Type, Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:full_jid(Alice),
        escalus:send(Alice, escalus_stanza:message(AliceJid, bob, Type, <<"Hi, Offline!">>))
    end),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and no message
    Presence = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence, Presence),

    ct:sleep(500),

    false = escalus_client:has_stanzas(Bob),

    escalus_cleaner:clean(Config).

max_offline_messages_reached(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        [send_message(Alice, I) || I <- lists:seq(1,?MAX_OFFLINE_MSGS+1)],
        Packet = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"wait">>, <<"resource-constraint">>], Packet),
        ct:sleep(100),
        false = escalus_client:has_stanzas(Alice)

    end),

    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and only 5 message
    Stanzas = escalus:wait_for_stanzas(Bob, ?MAX_OFFLINE_MSGS+1),

    Preds = [is_chat(make_chat_text(I)) || I <- lists:seq(1, ?MAX_OFFLINE_MSGS)],

    escalus_new_assert:mix_match([is_presence | Preds], Stanzas),


    ct:sleep(500),

    false = escalus_client:has_stanzas(Bob),

    escalus_cleaner:clean(Config).


%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

%%%===================================================================
%%% Helpers
%%%===================================================================

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.

set_max_offline_messages(Settings) ->
    OptionName = {access,max_user_offline_messages, global},
    Old = escalus_ejabberd:rpc(ejabberd_config, get_global_option, [OptionName]),
    {atomic, ok} = escalus_ejabberd:rpc(ejabberd_config, add_global_option, [OptionName, Settings]),
    Old.

send_message(Alice, I) ->
    escalus:send(Alice, escalus_stanza:chat_to(bob, make_chat_text(I))),
    timer:sleep(100).


make_chat_text(I) ->
    Number = list_to_binary(integer_to_list(I)),
    <<"Hi, Offline ", Number/binary>>.

