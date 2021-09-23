%%%----------------------------------------------------------------------
%%% File    : carboncopy_proper_tests_SUITE.erl
%%% Author  : Shambhu Prasad <shambhuprasad58@gmail.com>
%%% Purpose : Tests for mod_carboncopy.erl based on Proper API
%%% Created : 15 July 2014 by Shambhu Prasad <shambhuprasad58@gmail.com>
%%%-------------------------------------------------------------------

-module(carboncopy_proper_tests_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(xmlel_gen, [xmlel/3]).

all() ->
    [{group, mod_message_carbons_proper_tests}].

all_tests() ->
    [
     private_message_test,
     no_copy_type_test,
     empty_message_test,
     received_type_test,
     sent_message_test,
     simple_badarg_test,
     simple_chat_message_test,
     has_chat_state_notifications_test,
     has_delivery_receipts_test,
     is_muc_invitation_test,
     is_direct_invitation_test
    ].

groups() ->
    [{mod_message_carbons_proper_tests, [parallel], all_tests()}].

private_message_test(_) ->
    property(private_message_test, ?FORALL(Msg, private_carbon_message(),
          false == mod_carboncopy:should_forward(Msg, alice(), received))).

no_copy_type_test(_) ->
    property(no_copy_type_test, ?FORALL(Msg, no_copy_message(),
          false == mod_carboncopy:should_forward(Msg, alice(), received))).

empty_message_test(_) ->
    property(empty_message_test, ?FORALL(Msg, non_chat_message(),
          false == mod_carboncopy:should_forward(Msg, alice(), received))).

received_type_test(_) ->
    property(received_type_test, ?FORALL(Msg, received_message(),
          false == mod_carboncopy:should_forward(Msg, alice(), received))).

sent_message_test(_) ->
    property(sent_message_test, ?FORALL(Msg, sent_message(),
          false == mod_carboncopy:should_forward(Msg, alice(), received))).

simple_badarg_test(_) ->
    property(simple_badarg_test, ?FORALL(Msg, badarg_message(),
          false == mod_carboncopy:should_forward(Msg, alice(), received))).

simple_chat_message_test(_) ->
    property(simple_chat_message_test, ?FORALL(Msg, simple_chat_message(),
          true == mod_carboncopy:should_forward(Msg, alice(), received))).

has_chat_state_notifications_test(_) ->
    property(has_chat_state_notifications_test, ?FORALL(Msg, chat_state_notification(),
          true == mod_carboncopy:should_forward(Msg, alice(), received))).

has_delivery_receipts_test(_) ->
    property(has_delivery_receipts_test, ?FORALL(Msg, delivery_receipt(),
          true == mod_carboncopy:should_forward(Msg, alice(), received))).

is_muc_invitation_test(_) ->
    property(is_muc_invitation_test, ?FORALL(Msg, muc_invitation(),
          true == mod_carboncopy:should_forward(Msg, alice(), received))).

is_direct_invitation_test(_) ->
    property(is_direct_invitation_test, ?FORALL(Msg, direct_invitation(),
          true == mod_carboncopy:should_forward(Msg, alice(), received))).

property(Name, Prop) ->
    Props = proper:conjunction([{Name, Prop}]),
    true = proper:quickcheck(Props, [verbose, long_result, {numtests, 50}]).

alice() ->
    jid:make_noprep(<<"alice">>, <<"localhost">>, <<>>).

%%
%% Generators
%%

non_chat_message() ->
    xmlel("message", [], []).

private_carbon_message() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("private", [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}], [])]).

no_copy_message() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("no-copy", [{<<"xmlns">>, <<"urn:xmpp:hints">>}], [])]).

received_message() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("received", [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}], [])]).

sent_message() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("sent", [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}], [])]).

simple_chat_message() ->
    xmlel("message", [{<<"type">>, <<"chat">>}], []).

chat_state_notification() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("someelement", [{<<"xmlns">>, <<"http://jabber.org/protocol/chatstates">>}], [])]).

delivery_receipt() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("received", [{<<"xmlns">>, <<"urn:xmpp:receipts">>}], [])]).

muc_invitation() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("x", [{<<"xmlns">>, <<"jabber:x:conference">>}],
                 [xmlel("invite", [<<"from">>], [<<"alice@localhost">>])])]).

direct_invitation() ->
    xmlel("message",
          [{<<"type">>, <<"chat">>}],
          [xmlel("x", [{<<"xmlns">>, <<"jabber:x:conference">>}], [])]).

badarg_message() ->
    xmlel("message", [{<<"type">>, <<"123">>}],[]).
