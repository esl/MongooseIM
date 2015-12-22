%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @end
%%%===================================================================

-module(pubsub_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          {group, pubsub_cycle_tests}
         ].

groups() -> [{pubsub_cycle_tests, [sequence],
              [
               create_delete_node_test
              ]
             }].

suite() ->
    escalus:suite().

-define (NODE_ADDR, <<"pubsub.localhost">>).
-define (NODE_NAME, <<"princely_musings">>).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config,{by_name, [alice, bob, geralt, carol]}),
    ok.

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config,{by_name, [alice, bob, geralt, carol]}),
    ok.

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(_TestName, Config) ->
    escalus:end_per_testcase(_TestName, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

create_delete_node_test(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
              {true, _RecvdStanza} = pubsub_tools:create_node(Alice, ?NODE_ADDR, ?NODE_NAME),
              pubsub_tools:delete_node(Alice, ?NODE_ADDR, ?NODE_NAME)
      end).
