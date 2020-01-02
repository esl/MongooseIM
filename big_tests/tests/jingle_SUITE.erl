-module(jingle_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0, mim2/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-import(jingle_helper, [content/1,
                        content_group/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, all}].

groups() ->
    G = [{all, [parallel], test_cases()}],
    ct_helper:repeat_all_until_all_ok(G).

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
     mongoose_returns_404_when_nto_authorized_user_tries_to_cancel_a_session,
     mongoose_sends_reINVITE_on_source_remove_action,
     mongoose_sends_reINVITE_on_source_add_action,
     mongoose_sends_reINVITE_on_source_update_action

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

mongoose_sends_reINVITE_on_source_remove_action(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),
        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest),

        %% then Alice sends source-remove
        SourceRemoveStanza = escalus_stanza:to(jingle_source_remove(InviteStanza), Bob),
        escalus:send(Alice, SourceRemoveStanza),
        SourceRemoveResult = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [SourceRemoveStanza], SourceRemoveResult),
        SourceRemoveToBob = escalus:wait_for_stanza(Bob),
        assert_source_remove_action(SourceRemoveToBob, InviteRequest)
    end).

mongoose_sends_reINVITE_on_source_add_action(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),
        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest),

        %% then Alice sends source-remove
        SourceRemoveStanza = escalus_stanza:to(jingle_source_add(InviteStanza), Bob),
        escalus:send(Alice, SourceRemoveStanza),
        SourceRemoveResult = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [SourceRemoveStanza], SourceRemoveResult),
        SourceRemoveToBob = escalus:wait_for_stanza(Bob),
        assert_source_add_action(SourceRemoveToBob, InviteRequest)
    end).

mongoose_sends_reINVITE_on_source_update_action(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {InviteStanza, InviteRequest} = initiate_jingle_session(Alice, Bob),
        accept_jingle_session(Alice, Bob, InviteStanza, InviteRequest),

        %% then Alice sends source-remove
        SourceRemoveStanza = escalus_stanza:to(jingle_source_update(InviteStanza), Bob),
        escalus:send(Alice, SourceRemoveStanza),
        SourceRemoveResult = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [SourceRemoveStanza], SourceRemoveResult),
        SourceRemoveToBob = escalus:wait_for_stanza(Bob),
        assert_source_update_action(SourceRemoveToBob, InviteRequest)
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
    Ringing = exml_query:subelement(JingleEl, <<"ringing">>),
    ?assertMatch(#xmlel{}, Ringing),
    ?assertEqual(<<"urn:xmpp:jingle:apps:rtp:info:1">>, exml_query:attr(Ringing, <<"xmlns">>)),

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

assert_source_remove_action(SourceRemoveRequest, InviteRequest) ->
    assert_same_sid(InviteRequest, SourceRemoveRequest),
    JingleEl = exml_query:subelement(SourceRemoveRequest, <<"jingle">>),
    ?assertEqual(<<"source-remove">>,
                 (exml_query:attr(JingleEl, <<"action">>))).

assert_source_add_action(SourceRemoveRequest, InviteRequest) ->
    assert_same_sid(InviteRequest, SourceRemoveRequest),
    JingleEl = exml_query:subelement(SourceRemoveRequest, <<"jingle">>),
    ?assertEqual(<<"source-add">>,
                 (exml_query:attr(JingleEl, <<"action">>))).

assert_source_update_action(SourceRemoveRequest, InviteRequest) ->
    assert_same_sid(InviteRequest, SourceRemoveRequest),
    JingleEl = exml_query:subelement(SourceRemoveRequest, <<"jingle">>),
    ?assertEqual(<<"source-update">>,
                 (exml_query:attr(JingleEl, <<"action">>))).

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
    Audio = content(audio),
    Video = content(video),
    I = jingle_element(<<"session-initiate">>, [Audio, Video, content_group([Audio, Video])]),
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
    Audio = content(audio),
    Video = content(video_disabled),
    I = jingle_element(SID, <<"session-accept">>, [Audio, Video, content_group([Audio])]),
    iq_set(I).


jingle_source_remove(InviteRequest) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    Audio = content(audio_source_remove),
    Video = content(video_source_remove),
    I = jingle_element(SID, <<"source-remove">>, [Audio, Video,
                                                  content_group([Audio, Video])]),
    iq_set(I).

jingle_source_add(InviteRequest) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    Audio = content(audio_source_remove),
    Video = content(video_source_remove),
    I = jingle_element(SID, <<"source-add">>, [Audio, Video,
                                               content_group([Audio, Video])]),
    iq_set(I).

jingle_source_update(InviteRequest) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    Audio = content(audio_source_remove),
    Video = content(video_source_remove),
    I = jingle_element(SID, <<"source-update">>, [Audio, Video,
                                                  content_group([Audio, Video])]),
    iq_set(I).

jingle_transport_info(InviteRequest, Creator, Media, TransportAttrs) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    iq_set(jingle_element(SID, <<"transport-info">>, [trickle_ice_candidate(Creator, Media, TransportAttrs)])).

jingle_terminate(InviteRequest, Reason) ->
    SID = exml_query:path(InviteRequest, path_to_jingle_sid()),
    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlel{name = Reason}]},
    iq_set(jingle_element(SID, <<"session-terminate">>, [ReasonEl])).

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
