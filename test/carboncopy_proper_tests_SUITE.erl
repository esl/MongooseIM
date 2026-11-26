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
-include("jlib.hrl").
-include("mongoose_logger.hrl").
-include("session.hrl").

-import(xmlel_gen, [xmlel/3]).

all() ->
    [{group, mod_message_carbons_proper_tests}, {group, session_metadata}].

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
    [{mod_message_carbons_proper_tests, [parallel], all_tests()},
     {session_metadata, [], [unavailable_session_with_stale_carbons_is_skipped,
                            unset_presence_queues_cleanup_before_external_events]}].

init_per_group(session_metadata, Config) ->
    {ok, _} = application:ensure_all_started(jid),
    mongoose_config:set_opts(#{default_server_domain => <<"localhost">>, language => <<"en">>}),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(session_metadata, _Config) ->
    mongoose_config:erase_opts();
end_per_group(_, _Config) ->
    ok.

init_per_testcase(Name, Config)
  when Name =:= unavailable_session_with_stale_carbons_is_skipped;
       Name =:= unset_presence_queues_cleanup_before_external_events ->
    meck:new([ejabberd_sm, ejabberd_sm_backend, mongoose_router], [passthrough]),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(Name, _Config)
  when Name =:= unavailable_session_with_stale_carbons_is_skipped;
       Name =:= unset_presence_queues_cleanup_before_external_events ->
    meck:unload([ejabberd_sm, ejabberd_sm_backend, mongoose_router]);
end_per_testcase(_, _Config) ->
    ok.

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
    xmlel("message", #{}, []).

private_carbon_message() ->
    xmlel("message",
          #{<<"type">> => <<"chat">>},
          [xmlel("private", #{<<"xmlns">> => <<"urn:xmpp:carbons:2">>}, [])]).

no_copy_message() ->
    xmlel("message",
          #{<<"type">> => <<"chat">>},
          [xmlel("no-copy", #{<<"xmlns">> => <<"urn:xmpp:hints">>}, [])]).

received_message() ->
    xmlel("message",
          #{<<"type">> => <<"chat">>},
          [xmlel("received", #{<<"xmlns">> => <<"urn:xmpp:carbons:2">>}, [])]).

sent_message() ->
    xmlel("message",
          #{<<"type">> => <<"chat">>},
          [xmlel("sent", #{<<"xmlns">> => <<"urn:xmpp:carbons:2">>}, [])]).

simple_chat_message() ->
    xmlel("message", #{<<"type">> => <<"chat">>}, []).

chat_state_notification() ->
    xmlel("message",
          #{<<"type">> => <<"chat">>},
          [xmlel("someelement", #{<<"xmlns">> => <<"http://jabber.org/protocol/chatstates">>}, [])]).

delivery_receipt() ->
    xmlel("message",
          #{<<"type">> => <<"chat">>},
          [xmlel("received", #{<<"xmlns">> => <<"urn:xmpp:receipts">>}, [])]).

muc_invitation() ->
    xmlel("message", #{},
          [xmlel("x", #{<<"xmlns">> => <<"http://jabber.org/protocol/muc#user">>},
                 [xmlel("invite", #{<<"from">> => <<"crone1@shakespeare.lit/desktop">>}, [])])]).

direct_invitation() ->
    xmlel("message", #{},
          [xmlel("x", #{<<"xmlns">> => <<"jabber:x:conference">>}, [])]).

badarg_message() ->
    xmlel("message", #{<<"type">> =>  <<"123">>},[]).

%% Session metadata regressions

unavailable_session_with_stale_carbons_is_skipped(_) ->
    Sender = jid:from_binary(<<"alice@localhost/res2">>),
    Recipient = jid:from_binary(<<"bob@localhost/res1">>),
    Sessions = [session(<<"res1">>, undefined, #{cc => 2}),
                session(<<"res2">>, 0, #{cc => 2}),
                session(<<"res3">>, -1, #{cc => 2}),
                session(<<"res4">>, 0, #{})],
    meck:expect(ejabberd_sm, get_raw_sessions, fun(_) -> Sessions end),
    meck:expect(mongoose_router, route, fun(Acc) -> Acc end),
    Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content = <<"hello">>}]},
    Packet = #xmlel{name = <<"message">>, attrs = #{<<"type">> => <<"chat">>},
                    children = [Body]},
    Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => <<"localhost">>,
                             host_type => <<"localhost">>, from_jid => Sender,
                             to_jid => Recipient, element => Packet}),
    {ok, _} = mod_carboncopy:user_send_message(Acc, #{}, #{}),
    ?assertEqual(1, meck:num_calls(mongoose_router, route, '_')),
    Routed = meck:capture(first, mongoose_router, route, '_', 1),
    ?assertEqual(<<"alice@localhost/res3">>, jid:to_binary(mongoose_acc:to_jid(Routed))).

unset_presence_queues_cleanup_before_external_events(_) ->
    JID = jid:from_binary(<<"alice@localhost/res1">>),
    State0 = mongoose_c2s:create_data(#{host_type => <<"localhost">>, jid => JID}),
    State = mongoose_c2s:set_info(State0, #{cc => 2, retained => true}),
    SID = mongoose_c2s:get_sid(State),
    Acc0 = mongoose_acc:new(#{location => ?LOCATION, lserver => <<"localhost">>,
                              host_type => <<"localhost">>}),
    Acc = mongoose_acc:set(c2s, origin_sid, SID, Acc0),
    meck:expect(ejabberd_sm_backend, set_session, fun(_, _, _, _) -> ok end),
    {ok, UpdatedAcc} = mod_carboncopy:remove_connection(Acc, #{jid => JID}, #{}),
    PresenceAcc = mongoose_c2s_acc:to_acc(UpdatedAcc, set_priority, undefined),
    %% Apply the unavailable transition, then its priority cleanup event.
    {next_state, session_established, UnavailableState, [{next_event, cast, Cleanup}]} =
        mongoose_c2s:handle_state_after_packet(State, session_established, PresenceAcc),
    {keep_state, UpdatedState} =
        mongoose_c2s:handle_event(cast, Cleanup, session_established, UnavailableState),
    ?assertEqual(#{retained => true}, mongoose_c2s:get_info(UpdatedState)),
    ?assertEqual(1, meck:num_calls(ejabberd_sm_backend, set_session, '_')),
    Stored = meck:capture(first, ejabberd_sm_backend, set_session, '_', 4),
    ?assertEqual(#{retained => true}, Stored#session.info),
    ?assertEqual(undefined, Stored#session.priority),
    ?assertEqual(undefined, mongoose_c2s:get_session_priority(UpdatedState)).

session(Resource, Priority, Info) ->
    #session{sid = {1, self()}, usr = {<<"alice">>, <<"localhost">>, Resource},
             us = {<<"alice">>, <<"localhost">>}, priority = Priority, info = Info}.
