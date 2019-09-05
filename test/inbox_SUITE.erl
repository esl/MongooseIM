%%%-------------------------------------------------------------------
%%% @author nelson vides
%%% @copyright (C) 2019, Erlang Solutions
%%% @doc
%%%
%%% @end
%%% Created : 2019-09-05
%%%-------------------------------------------------------------------
-module(inbox_SUITE).
-author('nelson.vides@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_ns.hrl").

%% API
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2
        ]).

%% Test cases
-export([
         incoming_any_marker_should_not_affect_inbox/1,
         outgoing_any_marker_should_not_be_written_to_inbox/1,
         outgoing_reset_marker_should_reset_unread_counter/1,
         outgoing_other_marker_should_not_change_unread_counter/1,
         has_chat_marker_tests/1
        ]).

all() ->
    [
     {group, one2one},
     {group, muclight},
     {group, muc},
     has_chat_marker_tests
    ].

groups() ->
    [
     {one2one, [], check_marker_activity()},
     {muclight, [], check_marker_activity()},
     {muc, [], check_marker_activity()}
    ].

check_marker_activity() ->
    [
     incoming_any_marker_should_not_affect_inbox,
     outgoing_any_marker_should_not_be_written_to_inbox,
     outgoing_reset_marker_should_reset_unread_counter,
     outgoing_other_marker_should_not_change_unread_counter
    ].


%%%===================================================================
%%% Suite setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(stringprep),
    Config.

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
init_per_group(one2one, Config) ->
    [{module, mod_inbox_one2one}| Config];
init_per_group(muclight, Config) ->
    [{module, mod_inbox_muclight}| Config];
init_per_group(muc, Config) ->
    [{module, mod_inbox_muc}| Config].

end_per_group(_,_C) ->
    [].

%%%===================================================================
%%% Individual Test Cases
%%%===================================================================

has_chat_marker_tests(_) ->
    AllMarkers = chat_markers(),
    Funs = lists:flatten([
        [
         fun() -> ?assert(mod_inbox_utils:has_chat_marker(
                            marker(M, ?NS_CHAT_MARKERS), [M])) end,
         fun() -> ?assert(mod_inbox_utils:has_chat_marker(
                            marker(M, ?NS_CHAT_MARKERS), AllMarkers)) end,
         fun() -> ?assertNot(mod_inbox_utils:has_chat_marker(
                               marker(<<"non-valid-marker">>, ?NS_CHAT_MARKERS), AllMarkers)) end,
         fun() -> ?assertNot(mod_inbox_utils:has_chat_marker(
                               marker(M, <<"unknown-namespace">>), [M])) end,
         fun() -> ?assertNot(mod_inbox_utils:has_chat_marker(
                               marker(M, <<"unknown-namespace">>), AllMarkers)) end
        ] || M <- AllMarkers]),
    Ret = lists:map(fun(F) -> F() end, Funs),
    ?assert(lists:all(fun(X) -> X =:= ok end, Ret)),
    ?assertEqual(length(Funs), length(Ret)).

incoming_any_marker_should_not_affect_inbox(Config) ->
    Mod = ?config(module, Config),
    lists:foreach(
      fun(Mark) ->
              ct:log("Testing Marker ~p~n", [Mark]),
              test_mod_inbox_one2one(Mark, fun Mod:handle_incoming_message/4),
              ct:log("ok", [])
      end,
      chat_markers()
     ).

outgoing_any_marker_should_not_be_written_to_inbox(Config) ->
    Mod = ?config(module, Config),
    lists:foreach(
      fun(Mark) ->
              ct:log("Testing Marker ~p~n", [Mark]),
              test_mod_inbox_one2one(Mark, fun Mod:handle_outgoing_message/4),
              ct:log("ok", [])
      end,
      chat_markers()
     ).

outgoing_reset_marker_should_reset_unread_counter(Config) ->
    Mod = ?config(module, Config),
    test_mod_inbox_one2one(
      <<"displayed">>,
      fun(Host, User, Remote, Packet) ->
              Mod:handle_outgoing_message(Host, User, Remote, Packet),
              ?assert(meck:called(mod_inbox_utils, reset_unread_count, 3)),
              ?assertEqual(0, meck:num_calls(mod_inbox_utils, write_to_sender_inbox, 4))
      end).

outgoing_other_marker_should_not_change_unread_counter(Config) ->
    Mod = ?config(module, Config),
    test_mod_inbox_one2one(
      <<"received">>,
      fun(Host, User, Remote, Packet) ->
              Mod:handle_outgoing_message(Host, User, Remote, Packet),
              ?assertNot(meck:called(mod_inbox_utils, reset_unread_count, 3)),
              ?assertEqual(0, meck:num_calls(mod_inbox_utils, write_to_sender_inbox, 4))
      end).

test_mod_inbox_one2one(Mark, HandleMessage) ->
    %% given
    Marker = #xmlel{name = Mark,
                    attrs = [{<<"xmlns">>, ?NS_CHAT_MARKERS},
                             {<<"id">>, <<"fake-id">>}]},
    Packet = #xmlel{name = <<"message">>, children = [Marker]},
    Host = <<"localhost">>,
    User = jid:make(<<"alice">>, Host, <<"wonderland">>),
    Remote = jid:make(<<"bob">>, Host, <<"home">>),
    %% test funs
    Setup = fun setup_meck/0,
    Test = fun() -> HandleMessage(Host, User, Remote, Packet) end,
    Cleanup = fun unload_meck/0,
    %% when / then
    ?assertEqual(ok, run_one_eunit_test(Setup, Test, Cleanup)).

%%%===================================================================
%%% helper functions
%%%===================================================================
marker(MarkerKind, NS) ->
    Marker = #xmlel{name = MarkerKind, attrs = [{<<"xmlns">>, NS}]},
    #xmlel{name = <<"message">>, children = [Marker]}.

run_one_eunit_test(Setup, Test, Cleanup) ->
    try
        Setup(),
        Test()
    after
        Cleanup()
    end.

setup_meck() ->
    ResetMarkers = [<<"displayed">>],
    meck:new(mod_inbox_utils, [passthrough]),
    meck:new(gen_mod, [passthrough]),
    meck:expect(mod_inbox_utils, write_to_receiver_inbox, 4, marker_written),
    meck:expect(mod_inbox_utils, write_to_sender_inbox, 4, marker_written),
    meck:expect(mod_inbox_utils, get_reset_markers, 1, ResetMarkers),
    meck:expect(mod_inbox_utils, reset_unread_count, 3, ok),
    meck:expect(gen_mod, get_module_opt, 4,
                fun(_,_,_,undefined) -> "localhost";
                   (_,_,_,Def) -> Def
                end).

unload_meck() ->
    meck:unload([mod_inbox_utils, gen_mod]).

chat_markers() ->
    [<<"displayed">>, <<"acknowledged">>, <<"received">>].
