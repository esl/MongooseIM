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
-define(JID, jid:binary_to_bare(<<"migration_user@localhost/test">>)).
-define(NODE_NAME, <<"migration_node">>).
-define(USERNAME, <<"migration_user">>).
-define(NODE_HOST, ct:get_config({hosts, mim, domain})).
-define(BASE_NODE, {pubsub_node, {?NODE_HOST, ?NODE_NAME}, undefined, [], <<"flat">>, [?JID], []}).
-define(BASE_ITEM(Nidx), {pubsub_item, {?NODE_NAME, Nidx}, {erlang:timestamp(), ?JID}, {erlang:timestamp(),?JID}, ?JID, [{xmlcdata, []}]}).
-define(BASE_VCARD, {xmlel,<<"vCard">>,
           [{<<"xmlns">>,<<"vcard-temp">>}],
           [{xmlel,<<"NICKNAME">>,[],[{xmlcdata, ?USERNAME}]},
            {xmlel,<<"TEL">>,[],
                   [{xmlel,<<"HOME">>,[],[]},
                    {xmlel,<<"VOICE">>,[],[]},
                    {xmlel,<<"NUMBER">>,[],
                           [{xmlcdata,<<"+00000000000">>}]}]}]}).
-define(BASE_SEARCH_VCARD, {vcard_search, {<<"migration_user">>, <<"localhost">>}, <<?USERNAME/binary, "@localhost/test">>, ?USERNAME, <<"fn">>, <<"lfn">>, <<"family">>,
                                          <<"lfamily">>, <<"given">>, <<"lgiven">>, <<"middle">>, <<"lmiddle">>, <<"nickname">>, <<"lnickname">>, <<"bday">>, <<"lbday">>,
                                          <<"ctry">>, <<"lctry">>, <<"locality">>, <<"llocality">>, <<"email">>, <<"lemail">>, <<"orgname">>, <<"lorgname">>, <<"orgunit">>,
                                          <<"lorgunit">>}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    case (not ct_helper:is_ct_running()) orelse mongoose_helper:is_rdbms_enabled(?NODE_HOST) of
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
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = mongoose_helper:successful_rpc(mod_pubsub_db_mnesia, start, []),
    ok = mongoose_helper:successful_rpc(ejabberd_auth_internal, start, [?NODE_HOST]),
    ok = mongoose_helper:successful_rpc(mod_vcard_mnesia, init, [<<>>, <<>>]),
    ok = mongoose_helper:successful_rpc(mod_event_pusher_push_mnesia, init, [<<>>, <<>>]),
    {ok, _} = application:ensure_all_started(jid),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    ok = mongoose_helper:successful_rpc(ejabberd_auth_internal, stop, [?NODE_HOST]),
    _ = mnesia:stop(),
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
%%% Test case for migrate pubsub_nodes
%%% ==================================================================

migrate_pubsub_nodes(_Config) ->
    Nidx = create_migration_node(),
    {ok, _} = ?RPC_MIGRATE(<<"pubsub_nodes">>),
    SqlData = #{table => <<"pubsub_nodes">>, where => <<"name='", ?NODE_NAME/binary, "'">>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"nidx">>}) of
        {selected, [{Nidx}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'pubsub_nodes' is successful for Nidx: ~p", [Nidx]);
        Any ->
            ct:fail("Unexpected result of 'pubsub_nodes' migration ~p~n", [Any])
    end.

%%% ==================================================================
%%% Test case for migrate pubsub_subscriptions
%%% ==================================================================

migrate_pubsub_subscriptions(_Config) ->
    Nidx = create_migration_node(),
    ok = mongoose_helper:successful_rpc(mod_pubsub_db_mnesia, transaction, [#{name => add_subscription, args => [Nidx, ?JID, 'subscribed', <<"0000-0000-0000000">>, []]}]),
    {ok, _} = ?RPC_MIGRATE(<<"pubsub_subscriptions">>),
    SqlData = #{table => <<"pubsub_subscriptions">>, where => <<"nidx=", (list_to_binary(integer_to_list(Nidx)))/binary>>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"nidx">>}) of
        {selected, [{Nidx}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'pubsub_subscriptions' is successful for Nidx: ~p", [Nidx]);
        Any ->
            ct:fail("Unexpected result of 'pubsub_subscriptions' migration ~p~n", [Any])
    end.

%%% ==================================================================
%%% Test case for migrate pubsub_affiliations
%%% ==================================================================

migrate_pubsub_affiliations(_Config) ->
    Nidx = create_migration_node(),
    {ok, _} = ?RPC_MIGRATE(<<"pubsub_affiliations">>),
    SqlData = #{table => <<"pubsub_affiliations">>, where => <<"nidx=", (list_to_binary(integer_to_list(Nidx)))/binary>>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"nidx">>}) of
        {selected, [{Nidx}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'pubsub_affiliations' is successful for Nidx: ~p", [Nidx]);
        Any ->
            ct:fail("Unexpected result of 'pubsub_affiliations' migration ~p~n", [Any])
    end.

%%% ==================================================================
%%% Test case for migrate pubsub_items
%%% ==================================================================

migrate_pubsub_items(_Config) ->
    Nidx = create_migration_node(),
    ok = mongoose_helper:successful_rpc(mod_pubsub_db_mnesia, transaction, [#{name => add_item, args => [Nidx, ?JID, ?BASE_ITEM(Nidx)]}]),
    {ok, _} = ?RPC_MIGRATE(<<"pubsub_items">>),
    SqlData = #{table => <<"pubsub_items">>, where => <<"nidx=", (list_to_binary(integer_to_list(Nidx)))/binary>>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"nidx">>}) of
        {selected, [{Nidx}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'pubsub_items' is successful for Nidx: ~p", [Nidx]);
        Any ->
            ct:fail("Unexpected result of 'pubsub_items' migration ~p~n", [Any])
    end.

%%% ==================================================================
%%% Test case for migrate users
%%% ==================================================================

migrate_users(_Config) ->
    ok = mongoose_helper:successful_rpc(ejabberd_auth_internal, set_password, [?USERNAME, <<"localhost">>, <<"migration_pass">>]),
    {ok, _} = ?RPC_MIGRATE(<<"users">>),
    SqlData = #{table => <<"users">>, where => <<"username='", ?USERNAME/binary, "'">>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"username">>}) of
        {selected, [{?USERNAME}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'users' is successful for Username: ~p", [?USERNAME]);
        Any ->
            ct:fail("Unexpected result of 'users' migration ~p~n", [Any])
    end.

%%% ==================================================================
%%% Test case for migrate vcard_search
%%% ==================================================================

migrate_vcard_search(_Config) ->
    ok = set_vcard(),
    {ok, _} = ?RPC_MIGRATE(<<"vcard_search">>),
    SqlData = #{table => <<"vcard_search">>, where => <<"username='", ?USERNAME/binary, "'">>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"username">>}) of
        {selected, [{?USERNAME}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'vcard_search' is successful for Username: ~p", [?USERNAME]);
        Any ->
            ct:fail("Unexpected result of 'vcard_search' migration ~p~n", [Any])
    end.

%%% ==================================================================
%%% Test case for migrate vcard
%%% ==================================================================

migrate_vcard(Config) ->
    ok = set_vcard(),
    {ok, _} = ?RPC_MIGRATE(<<"vcard">>),
    SqlData = #{table => <<"vcard">>, where => <<"username='", ?USERNAME/binary, "'">>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"username">>}) of
        {selected, [{?USERNAME}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'vcard' is successful for Username: ~p", [?USERNAME]);
        Any ->
            ct:fail("Unexpected result of 'vcard' migration ~p~n", [Any])
    end.

%%% ==================================================================
%%% Test case for migrate event_pusher_push_subscription
%%% ==================================================================

migrate_event_pusher_push_subscription(Config) ->
    PubsubNode = <<"migration_pub_sub_node">>,
    ok = mongoose_helper:successful_rpc(mod_event_pusher_push_mnesia, enable, [?JID, ?JID, PubsubNode, [{<<"name">>, <<"value">>}]]),
    {ok, _} = ?RPC_MIGRATE(<<"event_pusher_push_subscription">>),
    SqlData = #{table => <<"event_pusher_push_subscription">>, where => <<"node='", PubsubNode/binary, "'">>},
    case sql_to_rdbms(SqlData#{act => <<"SELECT">>, column => <<"node">>}) of
        {selected, [{PubsubNode}]} ->
            {updated, 1} = sql_to_rdbms(SqlData#{act => <<"DELETE">>, column => <<"">>}),
            _ = clear_tables(),
            ct:comment("Migration of 'event_pusher_push_subscription' is successful for Node: ~p", [PubsubNode]);
        Any ->
            ct:fail("Unexpected result of 'event_pusher_push_subscription' migration ~p~n", [Any])
    end.

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

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

sql_query(Query) ->
    slow_rpc(mongoose_rdbms, sql_query, [?NODE_HOST, Query]).

slow_rpc(M, F, A) ->
    Node = ct:get_config({hosts, mim, node}),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    Res = escalus_rpc:call(Node, M, F, A, timer:seconds(30), Cookie),
    case Res of
        {badrpc, timeout} ->
            {badrpc, {timeout, M, F}};
        _ ->
            Res
    end.

sql_to_rdbms(#{act := Act, column := Column, table := Table, where := Where}) ->
    SelectQuery = <<Act/binary, " ", Column/binary, " FROM ", Table/binary, " WHERE ", Where/binary>>,
    SelectResult = sql_query(SelectQuery).

clear_tables() ->
    Tables = [pubsub_state, pubsub_item, pubsub_node,
              pubsub_subscription, pubsub_subnode,
              passwd, reg_users_counter,
              vcard, vcard_search, push_subscription],
    _ = [mongoose_helper:successful_rpc(mod_pubsub_db_mnesia, clear_table, [T]) || T <- Tables].

create_migration_node() ->
    {ok, Nidx} = mongoose_helper:successful_rpc(mod_pubsub_db_mnesia, transaction,[#{name => set_node, args => [?BASE_NODE]}]),
    ok = mongoose_helper:successful_rpc(mod_pubsub_db_mnesia, transaction,[#{name => create_node, args => [Nidx, ?JID]}]),
    Nidx.

set_vcard() ->
    mongoose_helper:successful_rpc(mod_vcard_mnesia, set_vcard, [?USERNAME, <<"localhost">>, ?BASE_VCARD, ?BASE_SEARCH_VCARD]).    
