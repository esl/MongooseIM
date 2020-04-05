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

-define(DELAY_NS, <<"urn:xmpp:delay">>).
-define(AFFILIATION_NS, <<"urn:xmpp:muclight:0#affiliations">>).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, mod_offline_tests},
     {group, chatmarkers},
     {group, with_groupchat}].

all_tests() ->
    [offline_message_is_stored_and_delivered_at_login,
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

init_per_suite(C) -> escalus:init_per_suite(C).
end_per_suite(C) -> escalus_fresh:clean(), escalus:end_per_suite(C).

init_per_group(with_groupchat, C) ->
    OfflineBackend = mongoose_helper:get_backend_name(mod_offline_backend),
    MucLightBackend = mongoose_helper:mnesia_or_rdbms_backend(),
    Modules = [{mod_offline, [{store_groupchat_messages, true},
                              {backend, OfflineBackend}]},
               {mod_muc_light, [{backend, MucLightBackend}]}],
    Config = dynamic_modules:save_modules(domain(), C),
    dynamic_modules:ensure_modules(domain(), Modules),
    Config;
init_per_group(chatmarkers, C) ->
    case mongoose_helper:is_rdbms_enabled(domain())of
        false ->  {skip, require_rdbms};
    true->
        Modules = [{mod_offline, [{store_groupchat_messages, true},
                                  {backend, rdbms}]},
                   {mod_offline_chatmarkers,[{store_groupchat_messages, true}]},
                   {mod_muc_light, [{backend, rdbms}]}],
        Config = dynamic_modules:save_modules(domain(), C),
        dynamic_modules:ensure_modules(domain(), Modules),
        Config
    end;

init_per_group(_, C) -> C.

end_per_group(Group, C) when Group =:= chatmarkers;
                             Group =:= with_groupchat ->
    dynamic_modules:restore_modules(domain(), C),
    C;
end_per_group(_, C) -> C.

domain() ->
    ct:get_config({hosts, mim, domain}).

init_per_testcase(Name, C) -> escalus:init_per_testcase(Name, C).
end_per_testcase(Name, C) -> escalus:end_per_testcase(Name, C).

%%%===================================================================
%%% offline tests
%%%===================================================================

offline_message_is_stored_and_delivered_at_login(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
                logout(FreshConfig, Bob),
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

                send_message(B1, Alice, ?MAX_OFFLINE_MSGS+1),
                Packet = escalus:wait_for_stanza(B1, 5000),
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
wait_for_n_offline_messages(Client, N) ->
    LUser = escalus_utils:jid_to_lower(escalus_client:username(Client)),
    LServer = escalus_utils:jid_to_lower(escalus_client:server(Client)),
    WaitFn = fun() -> mongoose_helper:total_offline_messages({LUser, LServer}) end,
    mongoose_helper:wait_until(WaitFn, N).

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

repeat(L,0) -> [];
repeat(L,N) -> L ++ repeat(L, N-1).
