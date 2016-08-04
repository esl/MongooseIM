-module(privacy_helper).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").

-import(escalus_compat, [bin/1]).

-export([set_and_activate/2,
         set_list/2,
         set_list/3,
         send_set_list/2,
         activate_list/2,
         set_default_list/2,
         privacy_list/1,
         gets_error/2,
         gets_error/3,
         is_privacy_list_push/1,
         is_presence_error/1]).

gets_error(Who, Type) ->
    gets_error(Who, <<"cancel">>, Type).

gets_error(Who, Type, Subtype) ->
    Response = escalus_client:wait_for_stanza(Who),
    escalus_assert:is_error(Response, Type, Subtype).

%% Sets the list on server and makes it the active one.
set_and_activate(Client, ListName) ->
    set_list(Client, ListName),
    activate_list(Client, ListName).

send_set_list(Client, ListName) ->
    Stanza = escalus_stanza:privacy_set_list(privacy_list(ListName)),
    escalus:send(Client, Stanza),
    Stanza.

%% Sets the list on server.
set_list(Client, ListName) ->
    Stanza = send_set_list(Client, ListName),
    verify_set_list_response(Client),
    verify_list(Client, ListName, Stanza).

set_list(Client, ListName, NewItems) ->
    PrivacyList = escalus_stanza:privacy_list(ListName, NewItems),
    Stanza = escalus_stanza:privacy_set_list(PrivacyList),
    escalus:send(Client, Stanza),
    verify_set_list_response(Client),
    verify_list(Client, ListName, Stanza).

verify_set_list_response(Client) ->
    Responses = escalus:wait_for_stanzas(Client, 2),
    escalus:assert_many([is_iq_result, is_privacy_set], Responses).

verify_list(Client, ListName, Stanza) ->
    GetStanza = escalus_stanza:privacy_get_lists([ListName]),
    escalus:send(Client, GetStanza),
    GetResultStanza = escalus:wait_for_stanza(Client),
    escalus:assert(fun does_result_match_request/3, [Stanza, GetStanza], GetResultStanza).

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

do_items_match({#xmlel{attrs = Props1}, #xmlel{attrs = Props2}}) ->
    L = [attr_match(Name, Value, Props2) || {Name, Value} <- Props1],
    lists:all(fun(E) -> true == E end, L).

attr_match(<<"value">>, Value, Props) ->
    ValueL = escalus_utils:jid_to_lower(Value),
    ValueL == escalus_utils:jid_to_lower(proplists:get_value(<<"value">>, Props));
attr_match(Name, Value, Props) ->
    Value == proplists:get_value(Name, Props).

%% Make the list the active one.
activate_list(Client, ListName) ->
    escalus:send(Client, escalus_stanza:privacy_activate(ListName)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Client)).

%% Make the list the default one.
set_default_list(Client, ListName) ->
    escalus:send(Client, escalus_stanza:privacy_set_default(ListName)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Client)).

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

privacy_list(Name) ->
    escalus_stanza:privacy_list(Name, list_content(Name)).

% Geralt is not used by the privacy tests, but lists have to have
% at least one element so we use him for a no-op.
list_content(<<"noop_list">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, geralt, [])
    ];
list_content(<<"deny_bob">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, bob, [])
    ];
list_content(<<"allow_bob">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"allow">>, bob, [])
    ];
list_content(<<"deny_bob_message">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, bob, [<<"message">>])
    ];
list_content(<<"deny_group_message">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>, <<"ignored">>, [<<"message">>])
    ];
list_content(<<"deny_unsubscribed_message">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>, <<"none">>, [<<"message">>])
    ];
list_content(<<"deny_all_message">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_all_message_but_subscription_from">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"allow">>, <<"subscription">>, <<"from">>, [<<"message">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_all_message_but_subscription_to">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"allow">>, <<"subscription">>, <<"to">>, [<<"message">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_all_message_but_subscription_both">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"allow">>, <<"subscription">>, <<"both">>, [<<"message">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, [<<"message">>])
    ];
list_content(<<"deny_not_both_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>, <<"from">>, [<<"presence-out">>]),
        escalus_stanza:privacy_list_item(<<"2">>, <<"deny">>, <<"subscription">>, <<"to">>, [<<"presence-out">>]),
        escalus_stanza:privacy_list_item(<<"3">>, <<"deny">>, <<"subscription">>, <<"none">>, [<<"presence-out">>])
    ];
list_content(<<"deny_bob_presence_in">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, bob, [<<"presence-in">>])
    ];
list_content(<<"deny_group_presence_in">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>, <<"ignored">>, [<<"presence-in">>])
    ];
list_content(<<"deny_unsubscribed_presence_in">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>, <<"none">>, [<<"presence-in">>])
    ];
list_content(<<"deny_all_presence_in">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"presence-in">>])
    ];
list_content(<<"deny_bob_presence_out">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, bob, [<<"presence-out">>])
    ];
list_content(<<"deny_group_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>, <<"ignored">>, [<<"presence-out">>])
    ];
list_content(<<"deny_unsubscribed_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>, <<"none">>, [<<"presence-out">>])
    ];
list_content(<<"deny_all_presence_out">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"presence-out">>])
    ];
list_content(<<"deny_localhost_iq">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, <<"localhost">>, [<<"iq">>])
    ];
list_content(<<"deny_group_iq">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>, <<"ignored">>, [<<"iq">>])
    ];
list_content(<<"deny_unsubscribed_iq">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>, <<"none">>, [<<"iq">>])
    ];
list_content(<<"deny_all_iq">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [<<"iq">>])
    ];
list_content(<<"deny_jid_all">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, bob, []),
        escalus_stanza:privacy_list_jid_item(<<"2">>, <<"deny">>, <<"localhost">>, [])
    ];
 list_content(<<"deny_group_all">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"group">>, <<"ignored">>, [])
    ];
list_content(<<"deny_unsubscribed_all">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, <<"subscription">>, <<"none">>, [])
    ];
list_content(<<"deny_all_all">>) -> [
        escalus_stanza:privacy_list_item(<<"1">>, <<"deny">>, [])
    ];
list_content(<<"deny_3_items">>) -> [
        escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>, bob, []),
        escalus_stanza:privacy_list_jid_item(<<"2">>, <<"deny">>, <<"steve@localhost">>, []),
        escalus_stanza:privacy_list_jid_item(<<"3">>, <<"deny">>, <<"john@localhost">>, [])
    ].
