%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features over s2s
%%% @end
%%%===================================================================

-module(pubsub_s2s_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([
         publish_test/1,
         publish_without_node_attr_test/1
        ]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(pubsub_tools, [
                       domain/0,
                       encode_group_name/2,
                       decode_group_name/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

suite() ->
    require_rpc_nodes([mim, fed]) ++ escalus:suite().

all() ->
    [{group, GN} || {GN, _, _} <- groups()].

groups() ->
    [{encode_group_name(BaseGroup, NodeTree), Opts, Cases} || {BaseGroup, Opts, Cases} <- base_groups(),
                                                               NodeTree <- [<<"dag">>, <<"tree">>]].

base_groups() ->
    G = [
         {basic, [parallel], basic_tests()}
        ],
    ct_helper:repeat_all_until_all_ok(G).

basic_tests() ->
    [
     publish_test,
     publish_without_node_attr_test
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config1 = s2s_helper:init_s2s(escalus:init_per_suite(Config)),
    escalus:create_users(Config1, escalus:get_users(users())).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    s2s_helper:end_s2s(Config),
    escalus:delete_users(Config, escalus:get_users(users())),
    escalus:end_per_suite(Config).

init_per_group(ComplexName, Config) ->
    DecodedGroupName = decode_group_name(ComplexName),
    ExtraOptions = extra_options_by_group_name(DecodedGroupName),
    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), required_modules(ExtraOptions)),
    s2s_helper:configure_s2s(both_plain, Config2).

extra_options_by_group_name(#{ node_tree := NodeTree }) ->
    [{nodetree, NodeTree},
     {plugins, [plugin_by_nodetree(NodeTree)]}].

plugin_by_nodetree(<<"dag">>) -> <<"dag">>;
plugin_by_nodetree(<<"tree">>) -> <<"flat">>.

end_per_group(_GroupName, Config) ->
    dynamic_modules:restore_modules(domain(), Config).

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(TestName, Config) ->
    escalus:end_per_testcase(TestName, Config).

users() ->
    [alice2, alice].

%%--------------------------------------------------------------------
%% Test cases for XEP-0060
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Main PubSub cases
%%--------------------------------------------------------------------
publish_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {alice2, 1}],
      fun(Alice, Alice2) ->
              Node = pubsub_tools:pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice2, <<"item2">>, Node, [{expected_error_type, <<"cancel">>}]),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

publish_without_node_attr_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {alice2, 1}],
      fun(Alice, Alice2) ->
              Node = pubsub_tools:pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish_without_node_attr(Alice2, <<"item2">>, Node, [{expected_error_type, <<"cancel">>}]),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

required_modules(ExtraOpts) ->
    [{mod_pubsub, [
                   {backend, mongoose_helper:mnesia_or_rdbms_backend()},
                   {host, "pubsub.@HOST@"}
                   | ExtraOpts
                  ]}].
