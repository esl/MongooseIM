-module(privacy_helper).

-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

gets_error(Who, Type) ->
    gets_error(Who, <<"cancel">>, Type).

gets_error(Who, Type, Subtype) ->
    Response = escalus_client:wait_for_stanza(Who),
    escalus_assert:is_error(Response, Type, Subtype).

%% Sets the list on server and makes it the active one.
set_and_activate(Client, {ListName, Target}) ->
    set_list(Client, {ListName, Target}),
    activate_list(Client, ListName);
set_and_activate(Client, ListName) ->
    set_list(Client, ListName),
    activate_list(Client, ListName).

send_set_list(Client, List) ->
    Stanza = escalus_stanza:privacy_set_list(privacy_list(List)),
    escalus:send(Client, Stanza),
    Stanza.

%% Sets the list on server.
set_list(Client, {ListName, Target}) ->
    TS = instrument_helper:timestamp(),
    Stanza = send_set_list(Client, {ListName, Target}),
    verify_set_list_response(Client, TS),
    verify_list(Client, ListName, Stanza);
set_list(Client, ListName) ->
    TS = instrument_helper:timestamp(),
    Stanza = send_set_list(Client, ListName),
    verify_set_list_response(Client, TS),
    verify_list(Client, ListName, Stanza).

set_list(Client, ListName, NewItems) ->
    TS = instrument_helper:timestamp(),
    PrivacyList = escalus_stanza:privacy_list(ListName, NewItems),
    Stanza = escalus_stanza:privacy_set_list(PrivacyList),
    escalus:send(Client, Stanza),
    verify_set_list_response(Client, TS),
    verify_list(Client, ListName, Stanza).

verify_set_list_response(Client, TS) ->
    Responses = escalus:wait_for_stanzas(Client, 2),
    escalus:assert_many([is_iq_result, is_privacy_set], Responses),
    assert_privacy_set_event(Client, #{}, TS).

verify_list(Client, ListName, Stanza) ->
    TS = instrument_helper:timestamp(),
    GetStanza = escalus_stanza:privacy_get_lists([ListName]),
    escalus:send(Client, GetStanza),
    GetResultStanza = escalus:wait_for_stanza(Client),
    escalus:assert(fun does_result_match_request/3, [Stanza, GetStanza], GetResultStanza),
    assert_privacy_get_event(Client, TS).

does_result_match_request(SetRequest, GetRequest, Result) ->
    escalus_pred:is_iq_result(GetRequest, Result) andalso
    does_privacy_list_match(SetRequest, Result).

does_privacy_list_match(Request, Result) ->
    does_privacy_list_name_match(Request, Result) andalso
    does_privacy_list_children_match(Request, Result).

does_privacy_list_name_match(Request, Result) ->
    NamePath = [{element, <<"query">>},
                {element, <<"list">>},
                {attr, <<"name">>}],

    ListName = exml_query:path(Request, NamePath),
    ListName == exml_query:path(Result, NamePath).

does_privacy_list_children_match(Request, Result) ->
    ChildrenPath = [{element, <<"query">>},
                    {element, <<"list">>}],
    RequestChildren = exml_query:subelements(exml_query:path(Request, ChildrenPath), <<"item">>),
    ResultChildren = exml_query:subelements(exml_query:path(Result, ChildrenPath), <<"item">>),
    lists:all(fun do_items_match/1, lists:zip(RequestChildren, ResultChildren)).

do_items_match({#xmlel{attrs = Attrs1}, #xmlel{attrs = Attrs2}}) ->
    L = [attr_match(Name, Value, Attrs2) || Name := Value <- Attrs1],
    lists:all(fun(E) -> true == E end, L).

attr_match(Name, ValueL, Attrs) ->
    case Attrs of
        #{Name := ValueR} when Name =:= <<"value">> ->
            escalus_utils:jid_to_lower(ValueL) == escalus_utils:jid_to_lower(ValueR);
        #{Name := ValueR} ->
            ValueL == ValueR;
        _ ->
            false
    end.


%% Make the list the active one.
activate_list(Client, ListName) ->
    TS = instrument_helper:timestamp(),
    escalus:send(Client, escalus_stanza:privacy_activate(ListName)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Client)),
    assert_privacy_set_event(Client, #{active_count => 1}, TS).

%% Make the list the default one.
set_default_list(Client, ListName) ->
    TS = instrument_helper:timestamp(),
    escalus:send(Client, escalus_stanza:privacy_set_default(ListName)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Client)),
    assert_privacy_set_event(Client, #{default_count => 1}, TS).

%% Is this iq a notification about a privacy list being changed?
is_privacy_list_push(Iq) ->
    escalus_pred:is_iq(<<"set">>, ?NS_PRIVACY, Iq)
    andalso
    undefined /= exml_query:path(Iq, [{element, <<"query">>},
                                      {element, <<"list">>}]).

is_presence_error(Stanza) ->
    escalus_pred:is_presence(Stanza)
    andalso
    escalus_pred:is_error(<<"modify">>, <<"not-acceptable">>, Stanza).

privacy_list({Name, #client{} = Target}) ->
    JID = escalus_utils:get_short_jid(Target),
    escalus_stanza:privacy_list(Name, list_content(Name, JID));
privacy_list({Name, Target}) ->
    escalus_stanza:privacy_list(Name, list_content(Name, Target));
privacy_list(Name) ->
    escalus_stanza:privacy_list(Name, list_content(Name)).


% Geralt is not used by the privacy tests, but lists have to have
% at least one element so we use him for a no-op.
list_content(<<"noop_list">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>,
                                             <<"geralt@localhost">>, [])
    ];
list_content(<<"deny_group_message">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>,
                                         <<"ignored">>, [<<"message">>])
    ];
list_content(<<"deny_unsubscribed_message">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>,
                                         <<"none">>, [<<"message">>])
    ];
list_content(<<"deny_all_message">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_all_message_but_subscription_from">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"allow">>, <<"subscription">>,
                                         <<"from">>, [<<"message">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_all_message_but_subscription_to">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"allow">>, <<"subscription">>,
                                         <<"to">>, [<<"message">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_all_message_but_subscription_both">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"allow">>, <<"subscription">>,
                                         <<"both">>, [<<"message">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_not_both_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>,
                                         <<"from">>, [<<"presence-out">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, <<"subscription">>,
                                         <<"to">>, [<<"presence-out">>]),
        escalus_stanza:privacy_list_item(<<"3">>, <<"deny">>, <<"subscription">>,
                                         <<"none">>, [<<"presence-out">>])
    ];
list_content(<<"deny_unsubscribed_presence_in">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>,
                                         <<"none">>, [<<"presence-in">>])
    ];
list_content(<<"deny_all_presence_in">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"presence-in">>])
    ];
list_content(<<"deny_group_presence_in">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>,
                                         <<"ignored">>, [<<"presence-in">>])
    ];
list_content(<<"deny_unsubscribed_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>,
                                         <<"none">>, [<<"presence-out">>])
    ];
list_content(<<"deny_all_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"presence-out">>])
    ];
list_content(<<"deny_group_iq">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>,
                                         <<"ignored">>, [<<"iq">>])
    ];
list_content(<<"deny_unsubscribed_iq">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>,
                                         <<"none">>, [<<"iq">>])
    ];
list_content(<<"deny_group_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>,
                                         <<"ignored">>, [<<"presence-out">>])
    ];
list_content(<<"deny_group_all">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>,
                                         <<"ignored">>, [])
    ];
list_content(<<"deny_unsubscribed_all">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>,
                                         <<"none">>, [])
    ];
list_content(<<"deny_all_all">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [])
    ];
list_content(<<"deny_all_iq">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"iq">>])
    ].

list_content(<<"deny_server_iq">>, LServer) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, LServer, [<<"iq">>])
    ];
list_content(<<"deny_client">>, JID) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, JID, [])
    ];
list_content(<<"allow_client">>, JID) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"allow">>, JID, [])
    ];
list_content(<<"deny_client_message">>, JID) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>,
                                             JID, [<<"message">>])
    ];
list_content(<<"deny_client_presence_in">>, JID) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>,
                                             JID, [<<"presence-in">>])
    ];
list_content(<<"deny_client_presence_out">>, JID) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>,
                                             JID, [<<"presence-out">>])
    ];
list_content(<<"deny_jid_all">>, JID) ->
    LServer = escalus_utils:get_server(JID),
    [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, JID, []),
        escalus_stanza:privacy_list_jid_item(<<"2">>, <<"deny">>, LServer, [])
    ];
list_content(<<"deny_3_items">>, JID) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, JID, []),
        escalus_stanza:privacy_list_jid_item(<<"2">>, <<"deny">>,
                                             <<"steve@localhost">>, []),
        escalus_stanza:privacy_list_jid_item(<<"3">>, <<"deny">>,
                                             <<"john@localhost">>, [])
    ].

%% Sends IQ reply and checks if user process still alive
does_user_process_crash(From, To, Type, Children) ->
    does_user_process_crash(From, To, Type, Children, <<"Hello stranger">>).
does_user_process_crash(From, To, Type, Children, Message) ->
    ToShortJid = escalus_utils:get_short_jid(To),
    IQWithType = escalus_stanza:iq(ToShortJid, Type, [Children]),
    escalus:send(From, IQWithType),
    ChatMsg = escalus_stanza:chat_to(ToShortJid, Message),
    escalus:send(From, ChatMsg),
    escalus:assert(is_chat_message,
                   [Message],
                   escalus:wait_for_stanza(To)).

send_and_check_blocked_message(Sender, Recipient) ->
    TS = instrument_helper:timestamp(),
    escalus_client:send(Sender, escalus_stanza:chat_to(Recipient, <<"Hi, blocker!">>)),
    timer:sleep(100),
    escalus_assert:has_no_stanzas(Recipient),
    privacy_helper:gets_error(Sender, <<"service-unavailable">>),
    assert_privacy_check_packet_event(Sender, #{dir => out}, TS),
    assert_privacy_check_packet_event(Recipient, #{dir => in, denied_count => 1}, TS).

%% Instrumentation events

assert_privacy_get_event(Client) ->
    assert_event(mod_privacy_get, measurements(Client)).

assert_privacy_get_event(Client, TS) ->
    assert_event(mod_privacy_get, measurements(Client), TS).

assert_privacy_set_event(Client, ExtraM) ->
    assert_event(mod_privacy_set, measurements(Client, ExtraM)).

assert_privacy_set_event(Client, ExtraM, TS) ->
    assert_event(mod_privacy_set, measurements(Client, ExtraM), TS).

assert_privacy_check_packet_event(Client, ExtraM) ->
    assert_event(mod_privacy_check_packet, measurements(Client, ExtraM)).

assert_privacy_check_packet_event(Client, ExtraM, TS) ->
    assert_event(mod_privacy_check_packet, measurements(Client, ExtraM), TS).

assert_privacy_push_item_event(Client, ExpCount) ->
    assert_event(mod_privacy_push_item, push_item_measurements(Client, ExpCount)).

assert_privacy_push_item_event(Client, ExpCount, TS) ->
    assert_event(mod_privacy_push_item, push_item_measurements(Client, ExpCount), TS).

measurements(Client, ExtraM) ->
    maps:merge(measurements(Client), ExtraM).

measurements(Client) ->
    ClientJid = jid:from_binary(escalus_utils:get_jid(Client)),
    #{jid => ClientJid, count => 1}.

push_item_measurements(Client, ExpCount) ->
    User = escalus_utils:get_username(Client),
    Server = escalus_utils:get_server(Client),
    #{user => User, server => Server, count => ExpCount}.

assert_event(Name, Measurements) ->
    instrument_helper:assert_one(Name, labels(), fun(M) -> M =:= Measurements end).

assert_event(Name, Measurements, TS) ->
    instrument_helper:assert(Name, labels(), fun(M) -> M =:= Measurements end,
                             #{expected_count => 1, min_timestamp => TS}).

labels() ->
    #{host_type => domain_helper:host_type()}.
