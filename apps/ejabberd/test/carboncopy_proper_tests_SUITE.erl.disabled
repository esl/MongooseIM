%%%----------------------------------------------------------------------
%%% File    : carboncopy_proper_tests_SUITE.erl
%%% Author  : Shambhu Prasad <shambhuprasad58@gmail.com>
%%% Purpose : Tests for mod_carboncopy.erl based on Proper API
%%% Created : 15 July 2014 by Shambhu Prasad <shambhuprasad58@gmail.com>
%%%-------------------------------------------------------------------

-module(carboncopy_proper_tests_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(xmlel_gen, [xmlel/3]).

all() ->
    [{group, mod_message_carbons_proper_tests}].

all_tests() ->
    [chat_type_test, private_message_test, no_copy_type_test, 
          received_type_test, sent_forwarded_type_test, sent_message_test, 
          simple_chat_message_test, simple_badarg_test].

groups() ->
    [{mod_message_carbons_proper_tests, [sequence], all_tests()}].

private_message_test(_) ->
	property(private_message_test, ?FORALL(Msg, private_carbon_message(),
          ignore == mod_carboncopy:classify_packet(Msg))).

chat_type_test(_) ->
	property(chat_type_test, ?FORALL(Msg, non_chat_message(),
          ignore == mod_carboncopy:classify_packet(Msg))).

no_copy_type_test(_) ->
	property(no_copy_type_test, ?FORALL(Msg, no_copy_message(),
          ignore == mod_carboncopy:classify_packet(Msg))).

received_type_test(_) ->
	property(received_type_test, ?FORALL(Msg, received_message(),
          ignore == mod_carboncopy:classify_packet(Msg))).

sent_forwarded_type_test(_) ->
	property(sent_forwarded_type_test, ?FORALL(Msg, sent_forwarded_message(),
          ignore == mod_carboncopy:classify_packet(Msg))).

sent_message_test(_) ->
	property(sent_message_test, ?FORALL(Msg, sent_message(),
          forward == mod_carboncopy:classify_packet(Msg))).

simple_chat_message_test(_) ->
	property(simple_chat_message_test, ?FORALL(Msg, simple_chat_message(),
          forward == mod_carboncopy:classify_packet(Msg))).

simple_badarg_test(_) ->
	property(simple_badarg_test, ?FORALL(Msg, badarg_message(),
          ignore == mod_carboncopy:classify_packet(Msg))).

property(Name, Prop) ->
    Props = proper:conjunction([{Name, Prop}]),
    true = proper:quickcheck(Props, [verbose, long_result, {numtests, 50}]).
	


%%
%% Generators
%%

non_chat_message() ->
	xmlel("message",[],[]).

private_carbon_message() ->
	xmlel("message", 
          [{<<"type">>,<<"chat">>}],
          [xmlel("private",[{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}],[])]).

no_copy_message() ->
	xmlel("message", 
          [{<<"type">>,<<"chat">>}],
          [xmlel("no-copy",[{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}],[])]).

received_message() ->
	xmlel("message", 
          [{<<"type">>,<<"chat">>}],
          [xmlel("received",[{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}],[])]).

sent_forwarded_message() ->
	xmlel("message", 
          [{<<"type">>,<<"chat">>}],
          [xmlel("sent",[{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}],[xmlel("forwarded",[],[])])]).

sent_message() ->
	xmlel("message", 
          [{<<"type">>,<<"chat">>}],
          [xmlel("sent",[{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}],[])]).

simple_chat_message() ->
	xmlel("message", [{<<"type">>,<<"chat">>}],[]).

badarg_message() ->
	xmlel("message", [{<<"type">>,<<"123">>}],[]).
