-module(mnesia_migration_SUITE).

-compile(export_all).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").

%%% ==================================================================
%%% Macro
%%% ==================================================================

-define(RPC_MIGRATE(Act), mongoose_helper:successful_rpc(service_admin_extra_migration, migrate, [<<"mnesia">>, <<"rdbms">>, Act])).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    case (not ct_helper:is_ct_running()) orelse mongoose_helper:is_rdbms_enabled(ct:get_config({hosts, mim, domain})) of
        true ->
            tests();
        false ->
            {skip, require_rdbms}
    end.

tests() ->
    [
        {group, migration}
    ].

groups() ->
    G = 
        [
            {migration, [sequence],
                [
                    migrate_pubsub_nodes,
                    migrate_pubsub_subscriptions,
                    migrate_pubsub_affiliations,
                    migrate_pubsub_items,
                    migrate_users,
                    migrate_vcard_search,
                    migrate_vcard,
                    migrate_event_pusher_push_subscription,
                    migrate_rosterusers,
                    migrate_roster_version,
                    migrate_rostergroups,
                    migrate_last,
                    migrate_private_storage,
                    migrate_offline_message,
                    migrate_muc_light_rooms,
                    migrate_all
                ]
            }
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%% ==================================================================
%%% Test case for migrate pubsub_nodes @TODO
%%% ==================================================================

migrate_pubsub_nodes(Config) ->
    R = ?RPC_MIGRATE(<<"pubsub_nodes">>),
    ct:comment("TEST CASE ~p", [{?FUNCTION_NAME, R}]).

%%% ==================================================================
%%% Test case for migrate pubsub_subscriptions @TODO
%%% ==================================================================

migrate_pubsub_subscriptions(Config) ->
    ?RPC_MIGRATE(<<"pubsub_subscriptions">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate pubsub_affiliations @TODO
%%% ==================================================================

migrate_pubsub_affiliations(Config) ->
    ?RPC_MIGRATE(<<"pubsub_affiliations">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate pubsub_items @TODO
%%% ==================================================================

migrate_pubsub_items(Config) ->
    ?RPC_MIGRATE(<<"pubsub_items">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate users @TODO
%%% ==================================================================

migrate_users(Config) ->
    ?RPC_MIGRATE(<<"users">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate vcard_search @TODO
%%% ==================================================================

migrate_vcard_search(Config) ->
    ?RPC_MIGRATE(<<"vcard_search">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate vcard @TODO
%%% ==================================================================

migrate_vcard(Config) ->
    ?RPC_MIGRATE(<<"vcard">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate event_pusher_push_subscription @TODO
%%% ==================================================================

migrate_event_pusher_push_subscription(Config) ->
    ?RPC_MIGRATE(<<"event_pusher_push_subscription">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate rosterusers @TODO
%%% ==================================================================

migrate_rosterusers(Config) ->
    ?RPC_MIGRATE(<<"rosterusers">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate roster_version @TODO
%%% ==================================================================

migrate_roster_version(Config) ->
    ?RPC_MIGRATE(<<"roster_version">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate rostergroups @TODO
%%% ==================================================================

migrate_rostergroups(Config) ->
    ?RPC_MIGRATE(<<"rostergroups">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate last @TODO
%%% ==================================================================

migrate_last(Config) ->
    ?RPC_MIGRATE(<<"last">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate private_storage @TODO
%%% ==================================================================

migrate_private_storage(Config) ->
    ?RPC_MIGRATE(<<"private_storage">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate offline_message @TODO
%%% ==================================================================

migrate_offline_message(Config) ->
    ?RPC_MIGRATE(<<"offline_message">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate muc_light_rooms @TODO
%%% ==================================================================

migrate_muc_light_rooms(Config) ->
    ?RPC_MIGRATE(<<"muc_light_rooms">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).

%%% ==================================================================
%%% Test case for migrate all @TODO
%%% ==================================================================

migrate_all(Config) ->
    ?RPC_MIGRATE(<<"all">>),
    ct:comment("TEST CASE ~p", [?FUNCTION_NAME]).
