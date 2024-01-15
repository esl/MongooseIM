%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_offline* modules
%%% @end
%%%===================================================================

-module(offline_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(MAX_OFFLINE_MSGS, 100). % known server-side config

-define(DELAY_NS, <<"urn:xmpp:delay">>).
-define(AFFILIATION_NS, <<"urn:xmpp:muclight:0#affiliations">>).
-define(NS_FEATURE_MSGOFFLINE,  <<"msgoffline">>).

-import(domain_helper, [host_type/0]).
-import(mongoose_helper, [wait_for_n_offline_messages/2]).
-import(config_parser_helper, [mod_config/2, mod_config_with_auto_backend/1]).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, mod_offline_tests},
     {group, chatmarkers},
     {group, with_groupchat}].

all_tests() ->
    [disco_info_sm,
     offline_message_is_stored_and_delivered_at_login,
     error_message_is_not_stored,
     groupchat_message_is_not_stored,
     headline_message_is_not_stored,
     expired_messages_are_not_delivered,
     max_offline_messages_reached].

chat_markers_tests() ->
    [one2one_chatmarker_is_overriden_and_only_unique_markers_are_delivered,
     room_chatmarker_is_overriden_and_only_unique_markers_are_delivered].

groups() ->
    G = [{mod_offline_tests, [parallel], all_tests()},
         {with_groupchat, [], [groupchat_message_is_stored]},
         {chatmarkers, [], chat_markers_tests()}
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, create_config()),
    escalus:init_per_suite(Config1).

-spec create_config() -> [{mod_offline, gen_mod:module_opts()}].
create_config() ->
    [{mod_offline, mod_config_with_auto_backend(mod_offline)}].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(with_groupchat, C) ->
    Config = dynamic_modules:save_modules(host_type(), C),
    dynamic_modules:ensure_modules(host_type(), with_groupchat_modules()),
    Config;
init_per_group(chatmarkers, C) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        false ->
            {skip, require_rdbms};
        true ->
            Config = dynamic_modules:save_modules(host_type(), C),
            dynamic_modules:ensure_modules(host_type(), chatmarkers_modules()),
            Config
    end;
init_per_group(_, C) -> C.

with_groupchat_modules() ->
    OfflineBackend = mongoose_helper:get_backend_name(host_type(), mod_offline),
    MucLightBackend = mongoose_helper:mnesia_or_rdbms_backend(),
    [{mod_offline, config_with_groupchat_modules(OfflineBackend)},
     {mod_muc_light, mod_config(mod_muc_light, #{backend => MucLightBackend})}].

config_with_groupchat_modules(Backend) ->
    mod_config(mod_offline, #{store_groupchat_messages => true,
        backend => Backend}).

chatmarkers_modules() ->
    [{mod_smart_markers, config_parser_helper:default_mod_config(mod_smart_markers)},
     {mod_offline, config_with_groupchat_modules(rdbms)},
     {mod_offline_chatmarkers,
      mod_config(mod_offline_chatmarkers,
                 #{store_groupchat_messages => true})},
     {mod_muc_light, mod_config(mod_muc_light, #{backend => rdbms})}].

end_per_group(Group, C) when Group =:= chatmarkers;
                             Group =:= with_groupchat ->
    dynamic_modules:restore_modules(C),
    C;
end_per_group(_, C) -> C.

init_per_testcase(Name, C) -> escalus:init_per_testcase(Name, C).
end_per_testcase(Name, C) -> escalus:end_per_testcase(Name, C).

%%%===================================================================
%%% offline tests
%%%===================================================================

disco_info_sm(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
        fun(Alice) ->
                AliceJid = escalus_client:short_jid(Alice),
                escalus:send(Alice, escalus_stanza:disco_info(AliceJid)),
                Stanza = escalus:wait_for_stanza(Alice),
                escalus:assert(has_feature, [?NS_FEATURE_MSGOFFLINE], Stanza),
                escalus:assert(is_stanza_from, [AliceJid], Stanza)
        end).

offline_message_is_stored_and_delivered_at_login(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
                logout(FreshConfig, Bob),
                escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"msgtxt">>)),
                wait_for_n_offline_messages(Bob, 1),
                NewBob = login_send_presence(FreshConfig, bob),
                Stanzas = escalus:wait_for_stanzas(NewBob, 2),
                escalus_new_assert:mix_match
                  ([is_presence, is_chat(<<"msgtxt">>)],
                   Stanzas)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

error_message_is_not_stored(Config) ->
    Story = fun(FreshConfig, Alice, Bob) ->
                    logout(FreshConfig, Bob),
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
                    logout(FreshConfig, Bob),
                    AliceJid = escalus_client:full_jid(Alice),
                    escalus:send(Alice, escalus_stanza:message
                              (AliceJid, Bob, <<"groupchat">>, <<"msgtxt">>)),
                    NewBob = login_send_and_receive_presence(FreshConfig, bob),
                    ct:sleep(500),
                    false = escalus_client:has_stanzas(NewBob)
            end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

groupchat_message_is_stored(Config) ->
    Story = fun(FreshConfig, Alice, Bob) ->
        CreateRoomStanza = muc_light_helper:stanza_create_room(undefined, [],
                                                               [{Bob, member}]),
        logout(FreshConfig, Bob),
        escalus:send(Alice, CreateRoomStanza),
        AffMsg = escalus:wait_for_stanza(Alice),
        RoomJID = exml_query:attr(AffMsg, <<"from">>),
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomJID, <<"msgtxt">>)),
        wait_for_n_offline_messages(Bob, 2),
        NewBob = login_send_presence(FreshConfig, bob),
        Stanzas = escalus:wait_for_stanzas(NewBob, 3),
        escalus_new_assert:mix_match([is_presence, is_affiliation(),
                                      is_groupchat(<<"msgtxt">>)],
                                     Stanzas)
            end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

one2one_chatmarker_is_overriden_and_only_unique_markers_are_delivered(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
            logout(FreshConfig, Bob),
            escalus:send(Alice, one2one_chatmarker(Bob, <<"received">>, <<"123">>)),
            escalus:send(Alice, one2one_chatmarker(Bob, <<"received">>, <<"321">>)),
            escalus:send(Alice, one2one_chatmarker(Bob, <<"received">>, <<"322">>, <<"t1">>)),
            escalus:send(Alice, one2one_chatmarker(Bob, <<"received">>, <<"323">>, <<"t1">>)),
            escalus:send(Alice, one2one_chatmarker(Bob, <<"displayed">>, <<"319">>)),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"msgtxt">>)),
            wait_for_n_offline_messages(Bob, 1),
            NewBob = login_send_presence(FreshConfig, bob),
            Stanzas = escalus:wait_for_stanzas(NewBob, 6), %only 5 messages must be received
            escalus_new_assert:mix_match(
                [is_presence, is_chat(<<"msgtxt">>),
                 is_one2one_chatmarker(<<"received">>, <<"321">>),
                 is_one2one_chatmarker(<<"received">>, <<"323">>, <<"t1">>),
                 is_one2one_chatmarker(<<"displayed">>, <<"319">>)],
                Stanzas)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

room_chatmarker_is_overriden_and_only_unique_markers_are_delivered(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
            CreateRoomStanza = muc_light_helper:stanza_create_room(undefined, [],
                                                                   [{Bob, member}]),
            escalus:send(Alice, CreateRoomStanza),
            AffMsg = escalus:wait_for_stanza(Alice),
            RoomJID = exml_query:attr(AffMsg, <<"from">>),
            AffMsg2 = escalus:wait_for_stanza(Bob),
            RoomJID = exml_query:attr(AffMsg2, <<"from">>),
            logout(FreshConfig, Bob),
            escalus:send(Alice, room_chatmarker(RoomJID, <<"received">>, <<"123">>)),
            escalus:send(Alice, room_chatmarker(RoomJID, <<"received">>, <<"321">>)),
            escalus:send(Alice, room_chatmarker(RoomJID, <<"received">>, <<"322">>, <<"t1">>)),
            escalus:send(Alice, room_chatmarker(RoomJID, <<"received">>, <<"323">>, <<"t1">>)),
            escalus:send(Alice, room_chatmarker(RoomJID, <<"displayed">>, <<"319">>)),
            escalus:send(Alice, escalus_stanza:groupchat_to(RoomJID, <<"msgtxt">>)),
            wait_for_n_offline_messages(Bob, 1),
            NewBob = login_send_presence(FreshConfig, bob),
            Stanzas = escalus:wait_for_stanzas(NewBob, 6), %only 5 messages must be received
            escalus_new_assert:mix_match(
                [is_presence, is_groupchat(<<"msgtxt">>),
                 is_room_chatmarker(<<"received">>, <<"321">>),
                 is_room_chatmarker(<<"received">>, <<"323">>, <<"t1">>),
                 is_room_chatmarker(<<"displayed">>, <<"319">>)],
                Stanzas)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

headline_message_is_not_stored(Config) ->
    Story = fun(FreshConfig, Alice, Bob) ->
                    logout(FreshConfig, Bob),
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

                logout(FreshConfig, Alice),
                each_client_sends_messages_to(BobsResources, Alice,
                                              {count, MessagesPerResource}),
                wait_for_n_offline_messages(Alice, MessagesPerResource * 4),

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
                logout(FreshConfig, Bob),
                escalus:send(Alice,
                             make_message_with_expiry(BobJid, 600000, <<"long">>)),
                escalus:send(Alice,
                             make_message_with_expiry(BobJid, 1, <<"short">>)),

                ct:sleep(timer:seconds(2)),
                NewBob = login_send_presence(FreshConfig, bob),

                escalus_new_assert:mix_match
                  ([is_presence, is_chat(<<"long">>)],
                   escalus:wait_for_stanzas(NewBob, 2, 5000)),
                ct:sleep(500),
                false = escalus_client:has_stanzas(NewBob)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).


%%%===================================================================
%%% Custom predicates
%%%===================================================================

room_chatmarker(RoomJID, MarkerType, MessageId) ->
    ChatMarker = escalus_stanza:chat_marker(RoomJID, MarkerType, MessageId),
    escalus_stanza:setattr(ChatMarker, <<"type">>, <<"groupchat">>).

room_chatmarker(RoomJID, MarkerType, MessageId, ThreadID) ->
    ChatMarker = room_chatmarker(RoomJID, MarkerType, MessageId),
    add_thread_id(ChatMarker, ThreadID).

one2one_chatmarker(RecipientJID, MarkerType, MessageId) ->
    escalus_stanza:chat_marker(RecipientJID, MarkerType, MessageId).

one2one_chatmarker(RecipientJID, MarkerType, MessageId, ThreadID) ->
    ChatMarker = one2one_chatmarker(RecipientJID, MarkerType, MessageId),
    add_thread_id(ChatMarker, ThreadID).

add_thread_id(#xmlel{children = Children} = ChatMarker, ThreadID) ->
    ThreadEl = #xmlel{name = <<"thread">>,
                      children = [#xmlcdata{content = ThreadID}]},
    ChatMarker#xmlel{children = [ThreadEl | Children]}.

is_chat(Content) ->
    fun(Stanza) ->
        escalus_pred:is_chat_message(Content, Stanza) andalso
        has_element_with_ns(Stanza, <<"delay">>, ?DELAY_NS)
    end.

is_groupchat(Content) ->
    fun(Stanza) ->
        escalus_pred:is_groupchat_message(Content, Stanza) andalso
        has_element_with_ns(Stanza, <<"delay">>, ?DELAY_NS)
    end.

is_affiliation() ->
    fun(Stanza) ->
        has_element_with_ns(Stanza, <<"x">>, ?AFFILIATION_NS) andalso
        has_element_with_ns(Stanza, <<"delay">>, ?DELAY_NS)
    end.

is_one2one_chatmarker(Marker, Id) ->
    is_one2one_chatmarker(Marker, Id, undefined).

is_one2one_chatmarker(Marker, Id, ThreadID) ->
    fun(Stanza) ->
        is_chatmarker(Stanza, Marker, undefined, Id, ThreadID)
    end.

is_room_chatmarker(Marker, Id) ->
    is_room_chatmarker(Marker, Id, undefined).

is_room_chatmarker(Marker, Id, ThreadID) ->
    fun(Stanza) ->
        is_chatmarker(Stanza, Marker, <<"groupchat">>, Id, ThreadID)
    end.

is_chatmarker(Stanza, Marker, Type, Id, ThreadID) ->
    try
        escalus_pred:is_chat_marker(Marker, Id, Stanza) andalso
            escalus_pred:has_type(Type, Stanza) andalso
            has_thread_id(Stanza, ThreadID)
    catch
        _:_ -> false

    end.

has_thread_id(Stanza, undefined) ->
    undefined =:= exml_query:subelement(Stanza, <<"thread">>);
has_thread_id(Stanza, ThreadID) ->
    ThreadID == exml_query:path(Stanza, [{element, <<"thread">>}, cdata]).

has_element_with_ns(Stanza, Element, NS) ->
    [] =/= exml_query:subelements_with_name_and_ns(Stanza, Element, NS).

%%%===================================================================
%%% Helpers
%%%===================================================================
logout(Config, User) ->
    mongoose_helper:logout_user(Config, User).

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

repeat(_L, 0) -> [];
repeat(L, N) -> L ++ repeat(L, N-1).
