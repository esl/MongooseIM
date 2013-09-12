-module(privacy_helper).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").

-import(escalus_compat, [bin/1]).

-export([set_and_activate/2,
         set_list/2,
         send_set_list/2,
         activate_list/2,
         set_default_list/2,
         privacy_list/1,
         is_privacy_list_push/1,
         is_presence_error/1]).

%% Sets the list on server and makes it the active one.
set_and_activate(Client, ListName) ->
    set_list(Client, ListName),
    activate_list(Client, ListName).

send_set_list(Client, ListName) ->
    Stanza = escalus_stanza:privacy_set_list(privacy_list(ListName)),
    escalus:send(Client, Stanza).

%% Sets the list on server.
set_list(Client, ListName) ->
    send_set_list(Client, ListName),
    Responses = escalus:wait_for_stanzas(Client, 2),
    escalus:assert_many([is_iq_result, is_privacy_set], Responses).

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
