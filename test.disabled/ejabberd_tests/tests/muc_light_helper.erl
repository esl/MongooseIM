-module(muc_light_helper).

-compile([export_all]).

-include("mam_helper.hrl").
-include_lib("common_test/include/ct.hrl").

given_muc_light_room(Name, Creator, InitOccupants) ->
    CreateStanza = muc_light_SUITE:stanza_create_room(Name, [], InitOccupants),
    escalus:send(Creator, CreateStanza),
    Affiliations = [{Creator, owner} | InitOccupants],
    muc_light_SUITE:verify_aff_bcast(Affiliations, Affiliations),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Creator)).

when_muc_light_message_is_sent(Sender, Room, Body, Id) ->
    RoomJid = muc_light_SUITE:room_bin_jid(Room),
    Stanza = escalus_stanza:set_id(
               escalus_stanza:groupchat_to(RoomJid, Body), Id),
    escalus:send(Sender, Stanza),
    {Room, Body, Id}.

then_muc_light_message_is_received_by(Users, {Room, Body, Id}) ->
    F = muc_light_SUITE:gc_message_verify_fun(Room, Body, Id),
    [ F(escalus:wait_for_stanza(User)) || User <- Users ].

when_muc_light_affiliations_are_set(Sender, Room, Affiliations) ->
    Stanza = muc_light_SUITE:stanza_aff_set(Room, Affiliations),
    escalus:send(Sender, Stanza),
    {Room, Affiliations}.

then_muc_light_affiliations_are_received_by(Users, {_Room, Affiliations}) ->
    F = muc_light_SUITE:aff_msg_verify_fun(Affiliations),
    [ F(escalus:wait_for_stanza(User)) || User <- Users ].

when_archive_query_is_sent(Sender, RecipientJid, Config) ->
	P = ?config(props, Config),
    Request = case RecipientJid of
                  undefined -> mam_helper:stanza_archive_request(P, <<"q">>);
                  _ -> escalus_stanza:to(mam_helper:stanza_archive_request(P, <<"q">>), RecipientJid)
              end,
    escalus:send(Sender, Request).

then_archive_response_is(Receiver, Expected, Config) ->
	P = ?config(props, Config),
    Response = mam_helper:wait_archive_respond(P, Receiver),
    Stanzas = mam_helper:respond_messages(mam_helper:assert_respond_size(length(Expected), Response)),
    ParsedStanzas = [ mam_helper:parse_forwarded_message(Stanza) || Stanza <- Stanzas ],
    [ assert_archive_element(Element)
      || Element <- lists:zip(Expected, ParsedStanzas) ].

assert_archive_element({{create, Affiliations}, Stanza}) ->
    mam_helper:verify_archived_muc_light_aff_msg(Stanza, Affiliations, _IsCreate = true);
assert_archive_element({{affiliations, Affiliations}, Stanza}) ->
    mam_helper:verify_archived_muc_light_aff_msg(Stanza, Affiliations, _IsCreate = false);
assert_archive_element({{muc_message, Room, Sender, Body}, Stanza}) ->
    FromJid = escalus_utils:jid_to_lower(muc_light_room_jid(Room, Sender)),
    #forwarded_message{message_body = Body,
                       delay_from = FromJid} = Stanza;
assert_archive_element({{message, Sender, Body}, Stanza}) ->
    FromJid = escalus_utils:jid_to_lower(escalus_utils:get_jid(Sender)),
    #forwarded_message{message_body = Body, delay_from = FromJid} = Stanza.


muc_light_room_jid(Room, User) ->
    RoomJid = muc_light_SUITE:room_bin_jid(Room),
    UserJid = escalus_utils:get_short_jid(User),
    <<RoomJid/binary, $/, UserJid/binary>>.
