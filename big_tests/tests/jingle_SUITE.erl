-module(jingle_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0, mim2/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, all}].

groups() ->
    [{all, [parallel], test_cases()}].

test_cases() ->
    [jingle_session_is_established_for_full_jids,
     jingle_session_is_established_for_full_jids_on_different_nodes,
     resp_4xx_from_sip_proxy_results_in_session_terminate,
     jingle_session_is_established_when_calling_a_number,
     jingle_session_is_established_and_terminated_by_initiator,
     jingle_session_is_established_and_terminated_by_receiver,
     jingle_session_is_established_and_terminated_by_receiver_on_different_node,
     jingle_session_is_intiated_and_canceled_by_initiator,
     jingle_session_is_intiated_and_canceled_by_receiver,
     jingle_session_is_intiated_and_canceled_by_receiver_on_different_node,
     jingle_session_is_established_with_a_conference_room,
     jingle_session_is_terminated_on_other_receivers_devices,
     jingle_session_initiate_is_resent_on_demand,
     mongoose_replies_with_480_when_invitee_is_offline,
     mongoose_returns_404_when_not_authorized_user_tires_to_accept_a_session,
     mongoose_returns_404_when_nto_authorized_user_tries_to_cancel_a_session
    ].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    case rpc(mim(), application, get_application, [nksip]) of
        {ok, nksip} ->
            Port = 12345,
            Host = ct:get_config({hosts, mim, domain}),
            distributed_helper:add_node_to_cluster(mim2(), Config),
            dynamic_modules:start(mim(), Host, mod_jingle_sip, [{proxy_host, "localhost"},
                                                                {proxy_port, Port},
                                                                {username_to_phone,[{<<"2000006168">>, <<"+919177074440">>}]}]),
            dynamic_modules:start(mim2(), Host, mod_jingle_sip, [{proxy_host, "localhost"},
                                                                 {proxy_port, Port},
                                                                 {listen_port, 12346},
                                                                 {username_to_phone,[{<<"2000006168">>, <<"+919177074440">>}]}]),

            application:ensure_all_started(esip),
            spawn(fun() -> ets:new(jingle_sip_translator, [public, named_table]),
                           ets:new(jingle_sip_translator_bindings, [public, named_table]),
                           receive stop -> ok end end),
            esip:add_listener(12345, tcp, []),
            esip:set_config_value(module, jingle_sip_translator),
            escalus:init_per_suite(Config);
        undefined ->
            {skip, build_was_not_configured_with_jingle_sip}
    end.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(mim(), Host, mod_jingle_sip),
    dynamic_modules:stop(mim2(), Host, mod_jingle_sip),
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Cases
%%--------------------------------------------------------------------

jingle_session_is_established_for_full_jids(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest)
    end).

jingle_session_is_established_for_full_jids_on_different_nodes(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest)
    end).

resp_4xx_from_sip_proxy_results_in_session_terminate(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        {_InviteStanza, FirstIQSet} = send_initiate_and_wait_for_first_iq_set(Alice, <<"error.480@localhost">>),
        assert_is_session_terminate(FirstIQSet, <<"general-error">>)

    end).

jingle_session_is_established_when_calling_a_number(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        {InviteStanza, FirstIQSet} = send_initiate_and_wait_for_first_iq_set(Alice, <<"+488790@numbers.localhost">>),
        assert_ringing(InviteStanza, FirstIQSet),
        AcceptInfo = escalus:wait_for_stanza(Alice, timer:seconds(5)),
        assert_accept_response(InviteStanza, AcceptInfo),

        ok

    end).

jingle_session_is_established_with_a_conference_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        {InviteStanza, FirstIQSet} = send_initiate_and_wait_for_first_iq_set(Alice, <<"*901@numbers.localhost">>),
        assert_ringing(InviteStanza, FirstIQSet),
        AcceptInfo = escalus:wait_for_stanza(Alice, timer:seconds(5)),
        assert_accept_response(InviteStanza, AcceptInfo),

        TransportInfo = escalus:wait_for_stanza(Alice),
        assert_transport_info(InviteStanza, TransportInfo),

        ok
    end).

jingle_session_initiate_is_resent_on_demand(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 2}], fun(Alice, Bob, Bob2) ->
        %% Bob2 becomes unavailalbe
        push_helper:become_unavailable(Bob2),
        escalus:wait_for_stanza(Bob), %%Bob first device gets unavailable form the other
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        escalus_assert:has_no_stanzas(Bob2),

        SID = exml_query:path(InviteRequest, path_to_jingle_sid()),

        ResendSessionInitiateIQ = iq_get(jingle_element(SID, <<"existing-session-initiate">>, [])),

        %% Bob2 becomes available
        escalus:send(Bob2, escalus_stanza:presence(<<"available">>)),
        escalus:wait_for_stanzas(Bob2, 2), %% 2 presence stansa from Bob2 and Bob
        %% and asks for the session-initiate received by the other device
        %% this is to get the invite in new session (new browser window)
        escalus:send(Bob2, ResendSessionInitiateIQ),
        ResendResult = escalus:wait_for_stanza(Bob2),
        escalus:assert(is_iq_result, [ResendSessionInitiateIQ], ResendResult),
        InviteRequest2 = escalus:wait_for_stanza(Bob2),
        assert_same_sid(InviteRequest, InviteRequest2),
        assert_invite_request(InviteStanza, InviteRequest2),
        AliceShortJID = escalus_client:short_jid(Alice),
        escalus:assert(is_stanza_from, [AliceShortJID], InviteRequest2)


    end).

jingle_session_is_terminated_on_other_receivers_devices(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 2}], fun(Alice, Bob, Bob2) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),
        %% The other Bob's device also gets the invite
        InviteRequest2 = escalus:wait_for_stanza(Bob2),

        %% then bob accepts the call on one of the devices
        assert_same_sid(InviteRequest, InviteRequest2),
        accept_jingle_session(Alice, Bob2, InviteStanza, InviteRequest2),

        %% then Bob's first device gets cancel request
        Terminate = escalus:wait_for_stanza(Bob),
        assert_is_session_terminate(Terminate, <<"cancel">>)

    end).


mongoose_replies_with_480_when_invitee_is_offline(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Bob becomes unavailalbe
        push_helper:become_unavailable(Bob),
        jingle_sip_translator:send_invite(Alice, Bob, self()),

        receive
            {sip_resp, 480} ->
                ok;
            {sip_resp, Other} ->
                ct:fail("Received SIP resp: ~p", [Other])
        after timer:seconds(5) ->
                  ct:fail(timeout_waiting_for_sip_resp)
        end

    end).

mongoose_returns_404_when_not_authorized_user_tires_to_accept_a_session(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        {InviteStanza, _InviteRequest} = initiate_jingle_session(Alice, Bob),
        %% Then Kate tries to accept Alice's session
        AcceptStanza = escalus_stanza:to(jingle_accept(InviteStanza), Bob),
        escalus:send(Kate, AcceptStanza),
        AcceptResult = escalus:wait_for_stanza(Kate, timer:seconds(5)),
        escalus:assert(is_iq_error, AcceptResult),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], AcceptResult)


    end).

mongoose_returns_404_when_nto_authorized_user_tries_to_cancel_a_session(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        {_InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),
        %% Then Kate tries to cancel Bob's session

        Result = send_session_terminate_request(Kate, Alice, InviteRequest, <<"decline">>),

        escalus:assert(is_iq_error, Result),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Result)

    end).

jingle_session_is_established_and_terminated_by_initiator(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest),
        timer:sleep(1000),

        %% Then Alice (who initiated) terminates the call
        terminate_jingle_session(Alice, Bob, InviteStanza)

    end).

jingle_session_is_established_and_terminated_by_receiver(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest),

        timer:sleep(timer:seconds(5)),
        %% then Bob (who was invited to the call) terminates the call
        %% it's important that bob terminates the call from the invite he got
        terminate_jingle_session(Bob, Alice, InviteRequest)

    end).

jingle_session_is_established_and_terminated_by_receiver_on_different_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest),

        timer:sleep(timer:seconds(5)),
        %% then Bob (who was invited to the call) terminates the call
        %% it's important that bob terminates the call from the invite he got
        terminate_jingle_session(Bob, Alice, InviteRequest)

    end).


jingle_session_is_intiated_and_canceled_by_initiator(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {InviteStanza, _InviteRequest} = initiate_jingle_session(Alice, Bob),
        timer:sleep(1000),
        %% then Bob (who was invited to the call) terminates the call
        terminate_jingle_session(Alice, Bob, InviteStanza, <<"decline">>)

    end).

jingle_session_is_intiated_and_canceled_by_receiver(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {_InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        timer:sleep(1000),
        %% then Bob (who was invited to the call) terminates the call
        terminate_jingle_session(Bob, Alice, InviteRequest, <<"decline">>)
    end).

jingle_session_is_intiated_and_canceled_by_receiver_on_different_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, Bob) ->
        {_InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),

        timer:sleep(1000),
        %% then Bob (who was invited to the call) terminates the call
        terminate_jingle_session(Bob, Alice, InviteRequest, <<"decline">>)
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest) ->
    AcceptStanza = escalus_stanza:to(jingle_accept(InviteRequest), Alice),
    escalus:send(Bob, AcceptStanza),
    AcceptResult = escalus:wait_for_stanza(Bob, timer:seconds(5)),
    escalus:assert(is_iq_result, AcceptResult),

    AcceptInfo = escalus:wait_for_stanza(Alice, timer:seconds(5)),
    assert_accept_response(InviteStanza, AcceptInfo).

initiate_jingle_session(Alice, Bob) ->
    {InviteStanza, _FirstIQSet} = send_initiate_and_wait_for_first_iq_set(Alice, Bob),

    %jingle_sip_translator:send_invite(Alice, Bob),

    InviteRequest = escalus:wait_for_stanza(Bob, timer:seconds(5)),
    escalus:assert(is_iq_set, InviteRequest),
    assert_invite_request(InviteStanza, InviteRequest),
    {InviteStanza, InviteRequest}.

send_initiate_and_wait_for_first_iq_set(Alice, Bob) ->
    InviteStanza = escalus_stanza:to(jingle_initiate(), Bob),
    escalus:send(Alice, InviteStanza),
    SessionInitiateResult = escalus:wait_for_stanza(Alice, timer:seconds(5)),
    escalus:assert(is_iq_result, SessionInitiateResult),
    RingingStanza = escalus:wait_for_stanza(Alice, timer:seconds(5)),
    escalus:assert(is_iq_set, RingingStanza),
    {InviteStanza, RingingStanza}.

terminate_jingle_session(Terminator, Other, InviteStanza) ->
    terminate_jingle_session(Terminator, Other, InviteStanza, <<"success">>).

terminate_jingle_session(Terminator, Other, InviteStanza, Reason) ->
    TerminateResult = send_session_terminate_request(Terminator, Other,
                                                     InviteStanza, Reason),
    escalus:assert(is_iq_result, TerminateResult),

    TerminateInfo = escalus:wait_for_stanza(Other, timer:seconds(5)),
    assert_is_session_terminate(TerminateInfo, Reason).

send_session_terminate_request(Terminator, Other, InviteStanza, Reason) ->
    TerminateStanza = escalus_stanza:to(jingle_terminate(InviteStanza, Reason), Other),
    escalus:send(Terminator, TerminateStanza),
    escalus:wait_for_stanza(Terminator, timer:seconds(5)).

assert_invite_request(InviteStanza, InviteRequest) ->
    JingleEl = exml_query:subelement(InviteRequest, <<"jingle">>),
    ?assertEqual(<<"session-initiate">>,
                 (exml_query:attr(JingleEl, <<"action">>))),

    assert_different_sid(InviteStanza, InviteRequest),
    assert_session_description(JingleEl).

assert_ringing(InviteStanza, RingingStanza) ->
    JingleEl = exml_query:subelement(RingingStanza, <<"jingle">>),
    ?assertEqual(<<"session-info">>,
                 (exml_query:attr(JingleEl, <<"action">>))),
    ?assertMatch(#xmlel{}, exml_query:subelement(JingleEl, <<"ringing">>)),

    assert_same_sid(InviteStanza, RingingStanza).


assert_session_description(JingleEl) ->
    Contents = exml_query:subelements(JingleEl, <<"content">>),
    ?assertMatch([#xmlel{} | _], Contents),

    [assert_transport_with_candidate(Content) || Content <- Contents],

    case Contents of
        [_, _ | _ ] -> %% For at least 2 contents
            ?assertMatch((#xmlel{}), (exml_query:subelement(JingleEl, <<"group">>)));
        _ ->
            ok
    end.

assert_transport_with_candidate(Content) ->
    TransportEl = exml_query:subelement(Content, <<"transport">>),
    ?assertMatch((#xmlel{}), TransportEl),
    ?assertMatch([#xmlel{} | _], (exml_query:subelements(TransportEl, <<"candidate">>))).

assert_different_sid(Sent, Received) ->
    SIDPath = path_to_jingle_sid(),
    ?assertNotEqual((exml_query:path(Sent, SIDPath)),
                    (exml_query:path(Received, SIDPath))).
assert_same_sid(Sent, Received) ->
    SIDPath = path_to_jingle_sid(),
    ?assertEqual((exml_query:path(Sent, SIDPath)),
                 (exml_query:path(Received, SIDPath))).

path_to_jingle_sid() -> [{element, <<"jingle">>}, {attr, <<"sid">>}].

assert_accept_response(InviteStanza, AcceptResponse) ->
    JingleEl = exml_query:subelement(AcceptResponse, <<"jingle">>),
    ?assertEqual(<<"session-accept">>,
                 (exml_query:attr(JingleEl, <<"action">>))),

    assert_same_sid(InviteStanza, AcceptResponse),
    assert_session_description(JingleEl).

assert_is_session_terminate(FirstIQSet, ReasonName) ->
    JingleEl = exml_query:subelement(FirstIQSet, <<"jingle">>),
    ?assertEqual(<<"session-terminate">>,
                 (exml_query:attr(JingleEl, <<"action">>))),
    ?assertMatch((#xmlel{}),
                 exml_query:path(JingleEl,
                                 [{element, <<"reason">>}, {element, ReasonName}])).

assert_transport_info(InviteStanza, TransportInfo) ->
    JingleEl = exml_query:subelement(TransportInfo, <<"jingle">>),
    ?assertEqual(<<"transport-info">>,
                 (exml_query:attr(JingleEl, <<"action">>))),
    assert_same_sid(InviteStanza, TransportInfo).


jingle_stanza_addressed_to_bare_jid_is_delivered(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobBareJID = escalus_client:short_jid(Bob),
        Stanza = escalus_stanza:to(jingle_initiate(), BobBareJID),
        escalus:send(Alice, Stanza),
        R = escalus:wait_for_stanza(Bob),
        escalus:assert(is_iq_set, R),
        ?assertEqual(exml_query:attr(Stanza, <<"id">>), exml_query:attr(R, <<"id">>))
    end).

jingle_stanza_addressed_to_own_bare_jid_is_rejected(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceBareJID = escalus_client:short_jid(Alice),
        Stanza = escalus_stanza:to(jingle_initiate(), AliceBareJID),
        escalus:send(Alice, Stanza),
        R = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], R)
    end).


other_iq_stanza_addressed_to_bare_jid_are_not_routed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobBareJID = escalus_client:short_jid(Bob),
        Stanza = escalus_stanza:to(escalus_stanza:iq_set(<<"urn:unknown:iq:ns">>, []), BobBareJID),
        escalus:send(Alice, Stanza),
        Reply = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Reply)

    end).


jingle_initiate() ->
    I = jingle_element(<<"session-initiate">>, [content(audio), content(video), content_group([audio_1, video_1])]),
    iq_set(I).

iq_set(I) ->
    Stanza = escalus_stanza:iq_set_nonquery(<<"jabber:client">>, [I]),
    iq_with_id(Stanza).

iq_with_id(#xmlel{attrs = Attrs} = Stanza) ->
    NewAttrs = lists:keystore(<<"id">>, 1, Attrs, {<<"id">>, uuid:uuid_to_string(uuid:get_v4(), binary_standard)}),
    Stanza#xmlel{attrs = NewAttrs}.

iq_get(I) ->
    Stanza = #xmlel{name = <<"iq">>,
                    attrs = [{<<"xmlns">>, <<"jabber:client">>},
                             {<<"type">>, <<"get">>}],
                    children = [I]},
    iq_with_id(Stanza).

jingle_element(Action, Children) ->
    SID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    jingle_element(SID, Action, Children).

jingle_element(SID, Action, Children) ->
    #xmlel{name = <<"jingle">>,
           attrs = [{<<"action">>, Action},
                    {<<"sid">>, SID},
                    {<<"xmlns">>, <<"urn:xmpp:jingle:1">>}],
           children = Children}.

jingle_accept(InviteRequest) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    I = jingle_element(SID, <<"session-accept">>, [content(audio), content(video_disabled), content_group([audio])]),
    iq_set(I).

jingle_transport_info(InviteRequest, Creator, Media, TransportAttrs) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    iq_set(jingle_element(SID, <<"transport-info">>, [trickle_ice_candidate(Creator, Media, TransportAttrs)])).

jingle_terminate(InviteRequest, Reason) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlel{name = Reason}]},
    iq_set(jingle_element(SID, <<"session-terminate">>, [ReasonEl])).

content(audio) ->
    escalus_stanza:from_xml(<<"
      <content creator='initiator' name='audio_1' senders='both'>
         <description xmlns='urn:xmpp:jingle:apps:rtp:1' media='audio' ssrc='948015790'>
            <payload-type id='111' name='opus' clockrate='48000' channels='2'>
               <parameter name='minptime' value='10' />
               <parameter name='useinbandfec' value='1' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='transport-cc' />
            </payload-type>
            <payload-type id='103' name='ISAC' clockrate='16000' channels='1' />
            <payload-type id='104' name='ISAC' clockrate='32000' channels='1' />
            <payload-type id='9' name='G722' clockrate='8000' channels='1' />
            <payload-type id='102' name='ILBC' clockrate='8000' channels='1' />
            <payload-type id='0' name='PCMU' clockrate='8000' channels='1' />
            <payload-type id='8' name='PCMA' clockrate='8000' channels='1' />
            <payload-type id='106' name='CN' clockrate='32000' channels='1' />
            <payload-type id='105' name='CN' clockrate='16000' channels='1' />
            <payload-type id='13' name='CN' clockrate='8000' channels='1' />
            <payload-type id='110' name='telephone-event' clockrate='48000' channels='1' />
            <payload-type id='112' name='telephone-event' clockrate='32000' channels='1' />
            <payload-type id='113' name='telephone-event' clockrate='16000' channels='1' />
            <payload-type id='126' name='telephone-event' clockrate='8000' channels='1' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='1' uri='urn:ietf:params:rtp-hdrext:ssrc-audio-level' senders='both' />
            <rtcp-mux />
            <source xmlns='urn:xmpp:jingle:apps:rtp:ssma:0' ssrc='948015790'>
               <parameter name='cname' value='3dYy6Ys3wP//8AoS' />
               <parameter name='msid' value='c5f700e1-1897-41c2-9421-697103982067 a4423a33-ffb9-40e2-a300-8317d9d00a46' />
            </source>
         </description>
         <transport xmlns='urn:xmpp:jingle:transports:ice-udp:1' ufrag='TIxp' pwd='MkQXObfhEelTbQdRV1e0ADGh'>
            <fingerprint xmlns='urn:xmpp:jingle:apps:dtls:0' hash='sha-256' setup='actpass'>08:D7:8E:6D:A6:40:77:4C:CC:F8:46:68:80:F2:2A:B1:7B:A0:AF:02:02:CA:2A:2A:F4:35:1A:95:11:75:B2:F7</fingerprint>
         </transport>
      </content>">>);
content(video) ->
    escalus_stanza:from_xml(<<"
      <content creator='initiator' name='video_1' senders='responder'>
         <description xmlns='urn:xmpp:jingle:apps:rtp:1' media='video'>
            <payload-type id='96' name='VP8' clockrate='90000' channels='1'>
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='ccm' subtype='fir' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' subtype='pli' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='goog-remb' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='transport-cc' />
            </payload-type>
            <payload-type id='98' name='VP9' clockrate='90000' channels='1'>
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='ccm' subtype='fir' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' subtype='pli' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='goog-remb' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='transport-cc' />
            </payload-type>
            <payload-type id='100' name='red' clockrate='90000' channels='1' />
            <payload-type id='127' name='ulpfec' clockrate='90000' channels='1' />
            <payload-type id='97' name='rtx' clockrate='90000' channels='1'>
               <parameter name='apt' value='96' />
            </payload-type>
            <payload-type id='99' name='rtx' clockrate='90000' channels='1'>
               <parameter name='apt' value='98' />
            </payload-type>
            <payload-type id='101' name='rtx' clockrate='90000' channels='1'>
               <parameter name='apt' value='100' />
            </payload-type>
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='2' uri='urn:ietf:params:rtp-hdrext:toffset' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='3' uri='http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='4' uri='urn:3gpp:video-orientation' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='5' uri='http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='6' uri='http://www.webrtc.org/experiments/rtp-hdrext/playout-delay' senders='both' />
            <rtcp-mux />
         </description>
         <transport xmlns='urn:xmpp:jingle:transports:ice-udp:1' ufrag='TIxp' pwd='MkQXObfhEelTbQdRV1e0ADGh'>
            <fingerprint xmlns='urn:xmpp:jingle:apps:dtls:0' hash='sha-256' setup='actpass'>08:D7:8E:6D:A6:40:77:4C:CC:F8:46:68:80:F2:2A:B1:7B:A0:AF:02:02:CA:2A:2A:F4:35:1A:95:11:75:B2:F7</fingerprint>
         </transport>
      </content>">>);
content(video_disabled) ->
    escalus_stanza:from_xml(
      <<"<content creator='initiator' name='video' senders='none'>
      <description xmlns='urn:xmpp:jingle:apps:rtp:1' media='video'>
        <payload-type id='120' name='VP8' clockrate='90000' channels='1'/>
      </description>
      <transport xmlns='urn:xmpp:jingle:transports:ice-udp:1'>
        <fingerprint xmlns='urn:xmpp:jingle:apps:dtls:0' hash='sha-256'>99:A4:F2:DC:C0:9C:44:6E:29:7B:4C:4F:1A:00:5B:EA:24:2A:D9:3A:D1:6D:D8:C1:45:2D:E7:52:D8:E4:95:D1</fingerprint>
      </transport>
    </content>">>).

content_group(Channels) ->
    Contents = [#xmlel{name = <<"content">>,
                       attrs = [{<<"name">>, atom_to_binary(Channel, utf8)}]}
                || Channel <- Channels],
    #xmlel{name = <<"group">>,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:grouping:0">>},
                    {<<"semantics">>, <<"BUNDLE">>}],
           children = Contents}.

get_ice_candidates() ->
    [
     [{<<"foundation">>, <<"1293499931">>}, {<<"component">>, <<"1">>}, {<<"protocol">>, <<"udp">>}, {<<"priority">>, <<"2122260223">>}, {<<"ip">>, <<"172.86.160.16">>}, {<<"port">>, <<"46515">>}, {<<"type">>, <<"host">>}, {<<"generation">>, <<"0">>}, {<<"network">>, <<"1">>}, {<<"id">>, <<"1.1947885zlx">>}]
    ].

trickle_ice_candidate(Creator, Content, TransportAttrs) ->
    Candidate = #xmlel{name = <<"candidate">>,
                       attrs = TransportAttrs},
    Transport = #xmlel{name = <<"transport">>,
                       attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:transports:ice-udp:1">>},
                                {<<"ufrag">>, <<"7Gpn">>},
                                {<<"pwd">>, <<"MUOzzatqL2qP7n1uRC7msD+c">>}],
                       children = [Candidate]},
    #xmlel{name = <<"content">>,
           attrs = [{<<"name">>, Content},
                    {<<"creator">>, Creator}],
           children = [Transport]}.
