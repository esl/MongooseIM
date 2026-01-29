%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for mod_event_pusher_push and related backends.
%%% This suite tests the remove_domain functionality for dynamic domains support.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("mongoose.hrl").

-define(HOST_TYPE, <<"test_host_type">>).
-define(DOMAIN, <<"test.domain">>).
-define(OTHER_DOMAIN, <<"other.domain">>).

-import(config_parser_helper, [mod_config/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mnesia_backend},
     {group, rdbms_backend},
     supported_features_tests].

groups() ->
    [{mnesia_backend, [sequence], mnesia_tests()},
     {rdbms_backend, [sequence], rdbms_tests()}].

mnesia_tests() ->
    [mnesia_remove_domain_deletes_subscriptions,
     mnesia_remove_domain_keeps_other_domain_subscriptions,
     mnesia_remove_domain_empty_table].

rdbms_tests() ->
    [rdbms_remove_domain_calls_execute_successfully].

%%--------------------------------------------------------------------
%% Init/End per suite
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mongoose_config:set_opts(opts()),
    async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                {mongooseim_helper, start_link_loaded_hooks, []}]).

end_per_suite(Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts(),
    mnesia:stop(),
    mnesia:delete_schema([node()]).

%%--------------------------------------------------------------------
%% Init/End per group
%%--------------------------------------------------------------------

init_per_group(mnesia_backend, Config) ->
    [{backend, mnesia} | Config];
init_per_group(rdbms_backend, Config) ->
    meck:new(mongoose_rdbms, [passthrough]),
    [{backend, rdbms} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(rdbms_backend, _Config) ->
    meck:unload(mongoose_rdbms);
end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Init/End per testcase
%%--------------------------------------------------------------------

init_per_testcase(TestCase, Config) when TestCase =:= mnesia_remove_domain_deletes_subscriptions;
                                         TestCase =:= mnesia_remove_domain_keeps_other_domain_subscriptions;
                                         TestCase =:= mnesia_remove_domain_empty_table ->
    ok = mod_event_pusher_push_mnesia:init(?HOST_TYPE, #{}),
    Config;
init_per_testcase(rdbms_remove_domain_calls_execute_successfully, Config) ->
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(TestCase, _Config) when TestCase =:= mnesia_remove_domain_deletes_subscriptions;
                                         TestCase =:= mnesia_remove_domain_keeps_other_domain_subscriptions;
                                         TestCase =:= mnesia_remove_domain_empty_table ->
    mnesia:clear_table(push_subscription),
    ok;
end_per_testcase(rdbms_remove_domain_calls_execute_successfully, _Config) ->
    meck:reset(mongoose_rdbms),
    ok;
end_per_testcase(_, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test cases - Mnesia backend
%%--------------------------------------------------------------------

mnesia_remove_domain_deletes_subscriptions(_Config) ->
    %% GIVEN subscriptions for users in DOMAIN
    User1 = jid:make_noprep(<<"user1">>, ?DOMAIN, <<>>),
    User2 = jid:make_noprep(<<"user2">>, ?DOMAIN, <<>>),
    PubSubJID = jid:make_noprep(<<"pubsub">>, <<"push.server">>, <<>>),
    Node = <<"node1">>,
    Form = #{<<"token">> => <<"abc123">>},

    ok = mod_event_pusher_push_mnesia:enable(?HOST_TYPE, User1, PubSubJID, Node, Form),
    ok = mod_event_pusher_push_mnesia:enable(?HOST_TYPE, User2, PubSubJID, Node, Form),

    %% Verify subscriptions exist
    ?assertMatch({ok, [_]}, mod_event_pusher_push_mnesia:get_publish_services(?HOST_TYPE, User1)),
    ?assertMatch({ok, [_]}, mod_event_pusher_push_mnesia:get_publish_services(?HOST_TYPE, User2)),

    %% WHEN remove_domain is called
    ok = mod_event_pusher_push_mnesia:remove_domain(?HOST_TYPE, ?DOMAIN),

    %% THEN all subscriptions for the domain are deleted
    {ok, ServicesAfter1} = mod_event_pusher_push_mnesia:get_publish_services(?HOST_TYPE, User1),
    {ok, ServicesAfter2} = mod_event_pusher_push_mnesia:get_publish_services(?HOST_TYPE, User2),
    ?assertEqual([], ServicesAfter1),
    ?assertEqual([], ServicesAfter2).

mnesia_remove_domain_keeps_other_domain_subscriptions(_Config) ->
    %% GIVEN subscriptions for users in both DOMAIN and OTHER_DOMAIN
    User1 = jid:make_noprep(<<"user1">>, ?DOMAIN, <<>>),
    User2 = jid:make_noprep(<<"user2">>, ?OTHER_DOMAIN, <<>>),
    PubSubJID = jid:make_noprep(<<"pubsub">>, <<"push.server">>, <<>>),
    Node = <<"node1">>,
    Form = #{<<"token">> => <<"abc123">>},

    ok = mod_event_pusher_push_mnesia:enable(?HOST_TYPE, User1, PubSubJID, Node, Form),
    ok = mod_event_pusher_push_mnesia:enable(?HOST_TYPE, User2, PubSubJID, Node, Form),

    %% WHEN remove_domain is called for DOMAIN only
    ok = mod_event_pusher_push_mnesia:remove_domain(?HOST_TYPE, ?DOMAIN),

    %% THEN subscriptions for OTHER_DOMAIN are preserved
    ?assertEqual({ok, []}, mod_event_pusher_push_mnesia:get_publish_services(?HOST_TYPE, User1)),
    ?assertMatch({ok, [_]}, mod_event_pusher_push_mnesia:get_publish_services(?HOST_TYPE, User2)).

mnesia_remove_domain_empty_table(_Config) ->
    %% GIVEN empty table
    %% WHEN remove_domain is called
    ok = mod_event_pusher_push_mnesia:remove_domain(?HOST_TYPE, ?DOMAIN),
    %% THEN no error occurs
    ok.

%%--------------------------------------------------------------------
%% Test cases - RDBMS backend
%%--------------------------------------------------------------------

rdbms_remove_domain_calls_execute_successfully(_Config) ->
    %% GIVEN expected LIKE pattern for the domain
    LikePattern = <<"%@", ?DOMAIN/binary>>,
    meck:expect(mongoose_rdbms, execute_successfully,
                fun(?HOST_TYPE, event_pusher_push_remove_domain, [ActualPattern]) ->
                        ?assertEqual(LikePattern, ActualPattern),
                        ok
                end),

    %% WHEN remove_domain is called
    ok = mod_event_pusher_push_rdbms:remove_domain(?HOST_TYPE, ?DOMAIN),

    %% THEN mongoose_rdbms:execute_successfully was called exactly once
    ?assertEqual(1, meck:num_calls(mongoose_rdbms, execute_successfully,
                                    [?HOST_TYPE, event_pusher_push_remove_domain, [LikePattern]])).

%%--------------------------------------------------------------------
%% Test cases - supported_features
%%--------------------------------------------------------------------

supported_features_tests(_Config) ->
    %% Test that all event pusher modules report dynamic_domains support
    ?assertEqual([dynamic_domains], mod_event_pusher:supported_features()),
    ?assertEqual([dynamic_domains], mod_event_pusher_http:supported_features()),
    ?assertEqual([dynamic_domains], mod_event_pusher_push:supported_features()),
    ?assertEqual([dynamic_domains], mod_event_pusher_rabbit:supported_features()),
    ?assertEqual([dynamic_domains], mod_event_pusher_sns:supported_features()).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

opts() ->
    #{hosts => [?HOST_TYPE],
      host_types => [],
      instrumentation => config_parser_helper:default_config([instrumentation])}.
