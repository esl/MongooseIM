%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_rdbms.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub RDBMS backend
%%% Created : 2 Nov 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_rdbms).
-author('piotr.nosek@erlang-solutions.com').
-author('michal.piotrowski@erlang-solutions.com').

-behaviour(mod_pubsub_db).

-include("pubsub.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

-export([init/2, stop/0]).
% Funs execution
-export([transaction/2, dirty/2]).
% Direct #pubsub_state access
-export([get_state/2,
         get_states/1, get_states_by_lus/1, get_states_by_bare/1,
         get_states_by_bare_and_full/1, get_idxs_of_own_nodes_with_pending_subs/1]).
% Node management
-export([
         create_node/2,
         del_node/1,
         set_node/1,
         find_node_by_id/1,
         find_nodes_by_key/1,
         find_node_by_name/2,
         delete_node/1,
         get_subnodes/2,
         get_parentnodes_tree/2,
         get_subnodes_tree/2
        ]).
% Affiliations
-export([
         set_affiliation/3,
         get_affiliation/2
        ]).
% Subscriptions
-export([
         add_subscription/5,
         set_subscription_opts/4,
         get_node_subscriptions/1,
         get_node_entity_subscriptions/2,
         delete_subscription/3,
         delete_all_subscriptions/2,
         update_subscription/4
        ]).
% Item ids in state
-export([
         add_item/3,
         remove_items/3,
         remove_all_items/1
        ]).
% Whole items
-export([
         get_items/2,
         get_item/2,
         set_item/1,
         del_item/2,
         del_items/2
        ]).

%% GDPR related
-export([
         get_user_payloads/2,
         get_user_nodes/2,
         get_user_subscriptions/2,
         delete_user_subscriptions/1,
         find_nodes_by_affiliated_user/1
        ]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

init(_HostType, _Opts) ->
    % -------------------- State building ----------------------------
    mongoose_rdbms:prepare(pubsub_get_item_rows_id, pubsub_items, [nidx],
        <<"SELECT nidx, created_luser, created_lserver, itemid "
          "FROM pubsub_items WHERE nidx = ?">>),
    mongoose_rdbms:prepare(pubsub_get_affiliation_rows_id, pubsub_affiliations, [nidx],
        <<"SELECT nidx, luser, lserver, aff FROM pubsub_affiliations WHERE nidx = ?">>),
    mongoose_rdbms:prepare(pubsub_get_subscriptions_rows_id, pubsub_subscriptions, [nidx],
        <<"SELECT nidx, luser, lserver, lresource, type, sub_id "
          "FROM pubsub_subscriptions WHERE nidx = ?">>),
    mongoose_rdbms:prepare(pubsub_get_item_rows, pubsub_items, [created_luser, created_lserver],
        <<"SELECT nidx, created_luser, created_lserver, itemid FROM pubsub_items"
          " WHERE created_luser = ? AND created_lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_get_affiliation_rows, pubsub_affiliations, [luser, lserver],
        <<"SELECT nidx, luser, lserver, aff FROM pubsub_affiliations "
          "WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_get_subscriptions_rows, pubsub_subscriptions, [luser, lserver],
        <<"SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions "
          "WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_get_subscriptions_rows_resource, pubsub_subscriptions,
        [luser, lserver, lresource],
        <<"SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions "
          "WHERE luser = ? AND lserver = ? AND lresource = ?">>),
    mongoose_rdbms:prepare(pubsub_get_idxs_of_own_nodes_with_pending_subs, pubsub_affiliations,
        [aff, luser, lserver, 'pubsub_subscriptions.type'],
        <<"SELECT DISTINCT s.nidx FROM pubsub_affiliations AS a "
          "INNER JOIN pubsub_subscriptions s ON a.nidx = s.nidx "
          "WHERE a.aff = ? AND a.luser = ? AND a.lserver = ? AND s.type = ?">>),

    % ------------------- Affiliations --------------------------------
    mongoose_rdbms:prepare(pubsub_get_affiliation, pubsub_affiliations, [nidx, luser, lserver],
        <<"SELECT aff FROM pubsub_affiliations WHERE nidx = ? AND luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_affiliation, pubsub_affiliations, [nidx, luser, lserver],
        <<"DELETE FROM pubsub_affiliations WHERE nidx = ? AND luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_all_affiliations, pubsub_affiliations, [nidx],
        <<"DELETE FROM pubsub_affiliations WHERE nidx = ?">>),

    % ------------------- Subscriptions --------------------------------
    mongoose_rdbms:prepare(pubsub_insert_subscription, pubsub_subscriptions,
        [nidx, luser, lserver, lresource, type, sub_id, options],
        <<"INSERT INTO pubsub_subscriptions (nidx, luser, lserver, lresource, "
            "type, sub_id, options) VALUES (?, ?, ?, ?, ?, ?, ?)">>),
    mongoose_rdbms:prepare(pubsub_update_subscription_opts, pubsub_subscriptions,
        [options, nidx, luser, lserver, lresource, sub_id],
        <<"UPDATE pubsub_subscriptions SET options = ? WHERE nidx = ? "
          "AND luser = ? AND lserver = ? AND lresource = ? AND sub_id = ?">>),
    mongoose_rdbms:prepare(pubsub_get_node_subs, pubsub_subscriptions, [nidx],
        <<"SELECT luser, lserver, lresource, type, sub_id, options "
          "FROM pubsub_subscriptions WHERE nidx = ?">>),
    mongoose_rdbms:prepare(pubsub_get_node_entity_subs, pubsub_subscriptions,
        [nidx, luser, lserver, lresource],
        <<"SELECT type, sub_id, options FROM pubsub_subscriptions "
          "WHERE nidx = ? AND luser = ? AND lserver = ? AND lresource = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_subscription, pubsub_subscriptions,
        [nidx, luser, lserver, lresource, sub_id],
        <<"DELETE FROM pubsub_subscriptions WHERE nidx = ? "
          "AND luser = ? AND lserver = ? AND lresource = ? AND sub_id = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_all_subscriptions, pubsub_subscriptions,
        [nidx, luser, lserver, lresource],
        <<"DELETE FROM pubsub_subscriptions"" WHERE nidx = ? "
          "AND luser = ? AND lserver = ? AND lresource = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_all_subscriptions_id, pubsub_subscriptions, [nidx],
        <<"DELETE FROM pubsub_subscriptions WHERE nidx = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_user_subscriptions,
        pubsub_subscriptions, [luser, lserver],
        <<"DELETE FROM pubsub_subscriptions WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_update_subscription, pubsub_subscriptions,
                           [type, nidx, luser, lserver, lresource, sub_id],
        <<"UPDATE pubsub_subscriptions SET type = ? WHERE nidx = ? "
          "AND luser = ? AND lserver = ? AND lresource = ? AND sub_id = ?">>),

    % ------------------- Items --------------------------------
    mongoose_rdbms:prepare(pubsub_get_entity_items, pubsub_items,
        [nidx, created_luser, created_lserver],
        <<"SELECT itemid FROM pubsub_items WHERE nidx = ? "
          "AND created_luser = ? AND created_lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_item, pubsub_items,
        [nidx, created_luser, created_lserver, itemid],
        <<"DELETE FROM pubsub_items WHERE nidx = ? AND created_luser = ? ",
          "AND created_lserver = ? AND itemid = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_all_items, pubsub_items, [nidx],
        <<"DELETE FROM pubsub_items WHERE nidx = ?">>),
    ItemColumns = item_columns(),
    mongoose_rdbms:prepare(pubsub_get_item, pubsub_items, [nidx, itemid],
        <<"SELECT ", ItemColumns/binary, " FROM pubsub_items WHERE nidx = ? AND itemid = ?">>),
    mongoose_rdbms:prepare(pubsub_get_items, pubsub_items, [nidx],
        <<"SELECT ", ItemColumns/binary, " FROM pubsub_items WHERE nidx = ? "
          "ORDER BY modified_at DESC">>),
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(),
    mongoose_rdbms:prepare(pubsub_get_items_limit, pubsub_items,
        rdbms_queries:add_limit_arg(limit, [nidx]),
        <<"SELECT ", LimitMSSQL/binary, " ", ItemColumns/binary,
          " FROM pubsub_items WHERE nidx = ? ORDER BY modified_at DESC ", LimitSQL/binary>>),
    mongoose_rdbms:prepare(pubsub_del_item, pubsub_items, [nidx, itemid],
        <<"DELETE FROM pubsub_items WHERE nidx = ? AND itemid = ?">>),

    % ------------------- Nodes --------------------------------
    mongoose_rdbms:prepare(pubsub_insert_node, pubsub_nodes, [p_key, name, type, owners, options],
        <<"INSERT INTO pubsub_nodes (p_key, name, type, owners, options) VALUES (?, ?, ?, ?, ?)">>),
    mongoose_rdbms:prepare(pubsub_insert_parent, pubsub_node_collections, [name, parent_name],
        <<"INSERT INTO pubsub_node_collections (name, parent_name) VALUES (?, ?)">>),
    mongoose_rdbms:prepare(pubsub_update_pubsub_node, pubsub_nodes, [type, owners, options, nidx],
        <<"UPDATE pubsub_nodes SET type = ?, owners = ?, options = ? WHERE nidx = ?">>),
    PubsubNodeFields = pubsub_node_fields(),
    mongoose_rdbms:prepare(pubsub_select_node_by_key_and_name, pubsub_nodes, [p_key, name],
        <<"SELECT ", PubsubNodeFields/binary, " from pubsub_nodes WHERE p_key = ? AND name = ?">>),
    mongoose_rdbms:prepare(pubsub_select_node_by_id, pubsub_nodes, [nidx],
        <<"SELECT ", PubsubNodeFields/binary, " from pubsub_nodes WHERE nidx = ?">>),
    mongoose_rdbms:prepare(pubsub_select_nodes_by_key, pubsub_nodes, [p_key],
        <<"SELECT ", PubsubNodeFields/binary, " from pubsub_nodes WHERE p_key = ?">>),
    mongoose_rdbms:prepare(pubsub_select_nodes_in_list_with_key, pubsub_nodes, [p_key, names],
        <<"SELECT ", PubsubNodeFields/binary, " from pubsub_nodes "
          "WHERE p_key = ? AND name IN (?)">>),
    PubsubNodeFieldsPrefixed = pubsub_node_fields_pn(),
    mongoose_rdbms:prepare(pubsub_select_nodes_by_affiliated_user,
        pubsub_affiliations, [luser, lserver],
        <<"SELECT aff, ", PubsubNodeFieldsPrefixed/binary, " FROM pubsub_affiliations AS pa "
          "INNER JOIN pubsub_nodes AS pn ON pa.nidx = pn.nidx WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_select_top_level_nodes, pubsub_nodes, [p_key],
        <<"SELECT ", PubsubNodeFieldsPrefixed/binary, " from pubsub_nodes as pn "
          "LEFT JOIN pubsub_node_collections as collection ON pn.name = collection.name "
          "WHERE p_key = ? AND collection.parent_name IS NULL">>),
    mongoose_rdbms:prepare(pubsub_select_subnodes, pubsub_nodes,
        ['pubsub_node_collections.parent_name', p_key],
        <<"SELECT ", PubsubNodeFieldsPrefixed/binary, " from pubsub_nodes as pn "
          "INNER JOIN pubsub_node_collections as collection ON pn.name = collection.name AND "
          "collection.parent_name = ? WHERE p_key = ?">>),
    mongoose_rdbms:prepare(pubsub_select_node_with_parents, pubsub_nodes,
        [p_key, name],
        <<"SELECT pn.nidx, pn.name, collection.parent_name from pubsub_nodes as pn "
          "LEFT JOIN pubsub_node_collections as collection ON pn.name = collection.name "
          "WHERE pn.p_key = ? AND pn.name = ?">>),
    mongoose_rdbms:prepare(pubsub_select_node_with_children, pubsub_nodes,
        [p_key, name],
        <<"SELECT pn.nidx, pn.name, collection.name from pubsub_nodes as pn "
          "LEFT JOIN pubsub_node_collections as collection ON pn.name = collection.parent_name "
          "WHERE pn.p_key = ? AND pn.name = ?">>),
    mongoose_rdbms:prepare(pubsub_delete_node, pubsub_nodes, [p_key, name],
        <<"DELETE from pubsub_nodes WHERE p_key = ? AND name = ?">>),
    mongoose_rdbms:prepare(pubsub_del_parents, pubsub_node_collections, [name],
        <<"DELETE FROM pubsub_node_collections WHERE name = ?">>),

    % ------------------- GDPR --------------------------------
    mongoose_rdbms:prepare(pubsub_get_user_items, pubsub_items, [created_luser, created_lserver],
        <<"SELECT name, itemid, payload FROM pubsub_items INNER JOIN pubsub_nodes "
          "ON pubsub_items.nidx = pubsub_nodes.nidx WHERE created_luser = ? "
          "AND created_lserver = ?">>),
    mongoose_rdbms:prepare(pubsub_get_user_subscriptions, pubsub_subscriptions, [luser, lserver],
        <<"SELECT name FROM pubsub_subscriptions INNER JOIN pubsub_nodes "
          "ON pubsub_subscriptions.nidx = pubsub_nodes.nidx WHERE luser = ? AND lserver = ?">>),
    prepare_select_nodes_by_owner(),

    rdbms_queries:prepare_upsert(global, pubsub_affiliation_upsert, pubsub_affiliations,
                                 [<<"nidx">>, <<"luser">>, <<"lserver">>, <<"aff">>],
                                 [<<"aff">>],
                                 [<<"nidx">>, <<"luser">>, <<"lserver">>]),
    ItemInsertFields = [<<"nidx">>, <<"itemid">>,
                        <<"created_luser">>, <<"created_lserver">>, <<"created_at">>,
                        <<"modified_luser">>, <<"modified_lserver">>, <<"modified_lresource">>, <<"modified_at">>,
                        <<"publisher">>, <<"payload">>],
    ItemUpdateFields = [<<"modified_luser">>, <<"modified_lserver">>, <<"modified_lresource">>, <<"modified_at">>,
                        <<"publisher">>, <<"payload">>],
    rdbms_queries:prepare_upsert(global, pubsub_item_upsert, pubsub_items,
                                 ItemInsertFields,
                                 ItemUpdateFields,
                                 [<<"nidx">>, <<"itemid">>]),
    ok.

-spec stop() -> ok.
stop() ->
    ok.

%% ------------------------ Queries execution --------------------
prepare_select_nodes_by_owner() ->
    case {mongoose_rdbms:db_engine(global), mongoose_rdbms:db_type()} of
        {mysql, _} ->
            mongoose_rdbms:prepare(pubsub_select_nodes_by_owner, pubsub_nodes, [owners],
                <<"SELECT name, type FROM pubsub_nodes WHERE owners = convert(?, JSON);">>);
        {pgsql, _} ->
            mongoose_rdbms:prepare(pubsub_select_nodes_by_owner, pubsub_nodes, [owners],
                <<"SELECT name, type FROM pubsub_nodes WHERE owners ::json->>0 like ? "
                  "AND JSON_ARRAY_LENGTH(owners) = 1">>);
        {odbc, mssql} ->
            mongoose_rdbms:prepare(pubsub_select_nodes_by_owner, pubsub_nodes, [owners],
                <<"SELECT name, type FROM pubsub_nodes WHERE cast(owners as nvarchar(max)) = ?;">>)
    end.
% -------------------- State building ----------------------------
-spec execute_get_item_rows_id(Nidx :: mod_pubsub:nodeIdx()) ->
    mongoose_rdbms:query_result().
execute_get_item_rows_id(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_item_rows_id, [Nidx]).

-spec execute_get_affiliation_rows_id(Nidx :: mod_pubsub:nodeIdx()) ->
    mongoose_rdbms:query_result().
execute_get_affiliation_rows_id(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_affiliation_rows_id, [Nidx]).

-spec execute_get_subscriptions_rows_id(Nidx :: mod_pubsub:nodeIdx()) ->
    mongoose_rdbms:query_result().
execute_get_subscriptions_rows_id(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_subscriptions_rows_id, [Nidx]).

-spec execute_get_item_rows(LU :: jid:luser(), LS :: jid:lserver()) ->
    mongoose_rdbms:query_result().
execute_get_item_rows(LU, LS) ->
     mongoose_rdbms:execute_successfully(LS, pubsub_get_item_rows, [LU, LS]).

-spec execute_get_affiliation_rows(LU :: jid:luser(), LS :: jid:lserver()) ->
    mongoose_rdbms:query_result().
execute_get_affiliation_rows(LU, LS) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_affiliation_rows, [LU, LS]).

-spec execute_get_subscriptions_rows(LU :: jid:luser(), LS :: jid:lserver()) ->
    mongoose_rdbms:query_result().
execute_get_subscriptions_rows(LU, LS) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_subscriptions_rows, [LU, LS]).

-spec execute_get_subscriptions_rows_resource(LU :: jid:luser(), LS :: jid:lserver(),
                                              LR :: jid:lresource()) -> mongoose_rdbms:query_result().
execute_get_subscriptions_rows_resource(LU, LS, LR) ->
    mongoose_rdbms:execute_successfully(global,
        pubsub_get_subscriptions_rows_resource, [LU, LS, LR]).

-spec execute_get_idxs_of_own_nodes_with_pending_subs(LS :: jid:lserver(),
                                                      Aff :: integer(),
                                                      LU :: jid:luser(),
                                                      Sub :: integer()) ->
                                                          mongoose_rdbms:query_result().
execute_get_idxs_of_own_nodes_with_pending_subs(LS, Aff, LU, Sub) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_get_idxs_of_own_nodes_with_pending_subs,
                                        [Aff, LU, LS, Sub]).

% ------------------- Affiliations --------------------------------
-spec execute_get_affiliation(Nidx :: mod_pubsub:nodeIdx(), LU :: jid:luser(),
                              LS :: jid:lserver()) -> mongoose_rdbms:query_result().
execute_get_affiliation(Nidx, LU, LS) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_get_affiliation, [Nidx, LU, LS]).

-spec execute_delete_affiliation(Nidx :: mod_pubsub:nodeIdx(), LU :: jid:luser(),
                                 LS :: jid:lserver()) -> mongoose_rdbms:query_result().
execute_delete_affiliation(Nidx, LU, LS) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_delete_affiliation, [Nidx, LU, LS]).

-spec execute_delete_all_affiliations(Nidx :: mod_pubsub:nodeIdx()) ->
    mongoose_rdbms:query_result().
execute_delete_all_affiliations(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_delete_all_affiliations, [Nidx]).

% ------------------- Subscriptions --------------------------------
-spec execute_insert_subscription(Nidx :: mod_pubsub:nodeIdx(),
                                  LU :: jid:luser(),
                                  LS :: jid:lserver(),
                                  LR :: jid:lresource(),
                                  Sub :: integer(),
                                  SubId :: mod_pubsub:subId(),
                                  EncodedOpts :: iodata()) ->
                                      mongoose_rdbms:query_result().
execute_insert_subscription(Nidx, LU, LS, LR, Sub, SubId, EncodedOpts) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_insert_subscription,
        [Nidx, LU, LS, LR, Sub, SubId, EncodedOpts]).

-spec execute_update_subscription_opts(EncodedOpts :: iodata(),
                                       Nidx :: mod_pubsub:nodeIdx(),
                                       LU :: jid:luser(),
                                       LS :: jid:lserver(),
                                       LR :: jid:lresource(),
                                       SubId :: mod_pubsub:subId()) ->
                                           mongoose_rdbms:query_result().
execute_update_subscription_opts(EncodedOpts, Nidx, LU, LS, LR, SubId) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_update_subscription_opts,
        [EncodedOpts, Nidx, LU, LS, LR, SubId]).

-spec execute_get_node_subs(Nidx :: mod_pubsub:nodeIdx()) -> mongoose_rdbms:query_result().
execute_get_node_subs(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_node_subs, [Nidx]).

-spec execute_get_node_entity_subs(LS :: jid:lserver(),
                                   Nidx :: mod_pubsub:nodeIdx(),
                                   LU :: jid:luser(),
                                   LR :: jid:lresource()) ->
                                       mongoose_rdbms:query_result().
execute_get_node_entity_subs(LS, Nidx, LU, LR) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_get_node_entity_subs, [Nidx, LU, LS, LR]).

-spec execute_delete_subscription(LS :: jid:lserver(),
                                  Nidx :: mod_pubsub:nodeIdx(),
                                  LU :: jid:luser(),
                                  LR :: jid:lresource(),
                                  SubId :: mod_pubsub:subId()) ->
                                      mongoose_rdbms:query_result().
execute_delete_subscription(LS, Nidx, LU, LR, SubId) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_delete_subscription, [Nidx, LU, LS, LR, SubId]).

-spec execute_delete_all_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                       LU :: jid:luser(),
                                       LS :: jid:lserver(),
                                       LR :: jid:lresource()) ->
                                           mongoose_rdbms:query_result().
execute_delete_all_subscriptions(Nidx, LU, LS, LR) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_delete_all_subscriptions, [Nidx, LU, LS, LR]).

-spec execute_delete_all_subscriptions_id(Nidx :: mod_pubsub:nodeIdx()) ->
    mongoose_rdbms:query_result().
execute_delete_all_subscriptions_id(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_delete_all_subscriptions_id, [Nidx]).

-spec execute_delete_user_subscriptions(LS :: jid:lserver(), LU :: jid:luser()) ->
    mongoose_rdbms:query_result().
execute_delete_user_subscriptions(LS, LU) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_delete_user_subscriptions, [LU, LS]).

-spec execute_update_subscription(Subscription :: integer(),
                                  Nidx :: mod_pubsub:nodeIdx(),
                                  LU :: jid:luser(),
                                  LS :: jid:lserver(),
                                  LR :: jid:lresource(),
                                  SubId :: mod_pubsub:subId()) ->
                                      mongoose_rdbms:query_result().
execute_update_subscription(Subscription, Nidx, LU, LS, LR, SubId) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_update_subscription,
        [Subscription, Nidx, LU, LS, LR, SubId]).

% ------------------- Items --------------------------------
-spec execute_get_entity_items(LS :: jid:lserver(),
                               Nidx :: mod_pubsub:nodeIdx(),
                               LU :: jid:lserver()) ->
                                   mongoose_rdbms:query_result().
execute_get_entity_items(LS, Nidx, LU) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_get_entity_items, [Nidx, LU, LS]).

-spec execute_delete_item(LS :: jid:lserver(),
                          Nidx :: mod_pubsub:nodeIdx(),
                          LU :: jid:luser(),
                          ItemId :: mod_pubsub:itemId()) ->
                              mongoose_rdbms:query_result().
execute_delete_item(LS, Nidx, LU, ItemId) ->
    mongoose_rdbms:execute_successfully(LS, pubsub_delete_item, [Nidx, LU, LS, ItemId]).

-spec execute_delete_all_items(Nidx :: mod_pubsub:nodeIdx()) -> mongoose_rdbms:query_result().
execute_delete_all_items(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_delete_all_items, [Nidx]).

-spec execute_get_item(Nidx :: mod_pubsub:nodeIdx(),
                       ItemId :: mod_pubsub:itemId()) ->
                           mongoose_rdbms:query_result().
execute_get_item(Nidx, ItemId) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_item, [Nidx, ItemId]).

-spec execute_get_items(Nidx :: mod_pubsub:nodeIdx()) -> mongoose_rdbms:query_result().
execute_get_items(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_items, [Nidx]).

-spec execute_get_items(Nidx :: mod_pubsub:nodeIdx(), Limit :: pos_integer()) ->
          mongoose_rdbms:query_result().
execute_get_items(Nidx, Limit) ->
    Args = rdbms_queries:add_limit_arg(Limit, [Nidx]),
    mongoose_rdbms:execute_successfully(global, pubsub_get_items_limit, Args).

-spec execute_del_item(Nidx :: mod_pubsub:nodeIdx(),
                       ItemId :: mod_pubsub:itemId()) ->
                           mongoose_rdbms:query_result().
execute_del_item(Nidx, ItemId) ->
    mongoose_rdbms:execute_successfully(global, pubsub_del_item, [Nidx, ItemId]).

% ------------------- Nodes --------------------------------
-spec execute_insert_pubsub_node(binary(), binary(), binary(), iodata(), iodata()) ->
          mongoose_rdbms:query_result().
execute_insert_pubsub_node(Key, Name, Type, Owners, Options) ->
    mongoose_rdbms:execute_successfully(global, pubsub_insert_node,
                                        [Key, Name, Type, Owners, Options]).

-spec execute_insert_parent(binary(), binary()) -> mongoose_rdbms:query_result().
execute_insert_parent(Name, ParentName) ->
    mongoose_rdbms:execute_successfully(global, pubsub_insert_parent,
                                        [Name, ParentName]).

-spec execute_update_pubsub_node(Type :: binary(),
                                 OwnersJid :: iodata(),
                                 Opts :: iodata(),
                                 Nidx :: mod_pubsub:nodeIdx()) ->
                                     mongoose_rdbms:query_result().
execute_update_pubsub_node(Type, OwnersJid, Opts, Nidx) ->
        mongoose_rdbms:execute_successfully(global, pubsub_update_pubsub_node,
            [Type, OwnersJid, Opts, Nidx]).

-spec execute_select_node_by_key_and_name(Key :: binary() | jid:ljid(),
                                          Node :: mod_pubsub:nodeId()) ->
                                              mongoose_rdbms:query_result().
execute_select_node_by_key_and_name(Key, Node) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_node_by_key_and_name,
        [Key, Node]).

-spec execute_select_node_by_id(Nidx :: mod_pubsub:nodeIdx()) -> mongoose_rdbms:query_result().
execute_select_node_by_id(Nidx) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_node_by_id, [Nidx]).

-spec execute_select_nodes_by_key(Key :: binary() | jid:ljid()) ->
    mongoose_rdbms:query_result().
execute_select_nodes_by_key(Key) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_nodes_by_key, [Key]).

-spec execute_select_nodes_by_affiliated_user(LU :: jid:luser(), LS :: jid:lserver()) ->
    mongoose_rdbms:query_result().
execute_select_nodes_by_affiliated_user(LU, LS) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_nodes_by_affiliated_user, [LU, LS]).

-spec execute_select_subnodes(Key :: binary(),
                              Node :: mod_pubsub:nodeId() | <<>>) ->
                                  mongoose_rdbms:query_result().
execute_select_subnodes(Key, <<>>) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_top_level_nodes, [Key]);
execute_select_subnodes(Key, Node) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_subnodes, [Node, Key]).

-spec execute_select_node_with_parents(Key :: binary(), Node :: mod_pubsub:nodeId()) ->
          mongoose_rdbms:query_result().
execute_select_node_with_parents(Key, Name) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_node_with_parents, [Key, Name]).

-spec execute_select_node_with_children(Key :: binary(), Node :: mod_pubsub:nodeId()) ->
          mongoose_rdbms:query_result().
execute_select_node_with_children(Key, Name) ->
    mongoose_rdbms:execute_successfully(global, pubsub_select_node_with_children, [Key, Name]).

-spec execute_delete_node(Key :: binary(), Node :: mod_pubsub:nodeId()) ->
    mongoose_rdbms:query_result().
execute_delete_node(Key, Node) ->
    mongoose_rdbms:execute_successfully(global, pubsub_delete_node, [Key, Node]).

-spec execute_del_parents(Name :: mod_pubsub:nodeId()) -> mongoose_rdbms:query_result().
execute_del_parents(Name) ->
    mongoose_rdbms:execute_successfully(global, pubsub_del_parents, [Name]).

% ------------------- GDPR --------------------------------
-spec execute_get_user_items(LU :: jid:luser(), LS :: jid:lserver()) ->
    mongoose_rdbms:query_result().
execute_get_user_items(LU, LS) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_user_items, [LU, LS]).

-spec execute_select_nodes_by_owner(LJID :: binary()) -> mongoose_rdbms:query_result().
execute_select_nodes_by_owner(LJID) ->
    case mongoose_rdbms:db_engine(global) of
        pgsql ->
            mongoose_rdbms:execute_successfully(global,
                pubsub_select_nodes_by_owner, [LJID]);
        _ ->
            mongoose_rdbms:execute_successfully(global,
                pubsub_select_nodes_by_owner, [iolist_to_binary(["[\"", LJID, "\"]"])])
    end.

-spec execute_get_user_subscriptions(LU :: jid:luser(), LS :: jid:lserver()) ->
    mongoose_rdbms:query_result().
execute_get_user_subscriptions(LU, LS) ->
    mongoose_rdbms:execute_successfully(global, pubsub_get_user_subscriptions, [LU, LS]).

%% ------------------------ Fun execution ------------------------

transaction(Fun, ErrorDebug) ->
    case mongoose_rdbms:sql_transaction(global, mod_pubsub_db:extra_debug_fun(Fun)) of
        {atomic, Result} ->
            Result;
        {aborted, ReasonData} ->
            mod_pubsub_db:db_error(ReasonData, ErrorDebug, transaction_failed)
    end.

dirty(Fun, ErrorDebug) ->
    try mongoose_rdbms:sql_dirty(global, mod_pubsub_db:extra_debug_fun(Fun)) of
        Result ->
            Result
    catch
        _C:ReasonData ->
            mod_pubsub_db:db_error(ReasonData, ErrorDebug, dirty_failed)
    end.

%% ------------------------ Direct #pubsub_state access ------------------------
%% TODO: Functions for direct #pubsub_access are currently inefficient for RDBMS
%%       - refactor them or remove as many of them as possible from the API at some point
-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, LJID) ->
    {ok, ItemIds} = get_entity_items(Nidx, LJID),
    {ok, Affiliation} = get_affiliation(Nidx, LJID),
    {ok, Subscriptions} = get_node_entity_subscriptions(Nidx, LJID),
    {ok, #pubsub_state{
            stateid = {LJID, Nidx},
            items = ItemIds,
            affiliation = Affiliation,
            subscriptions = Subscriptions
           }}.

-spec get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states(Nidx) ->
    {selected, ItemRows} = execute_get_item_rows_id(Nidx),
    {selected, AffiliationRows} = execute_get_affiliation_rows_id(Nidx),
    {selected, SubRows} = execute_get_subscriptions_rows_id(Nidx),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_lus(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_lus({ LU, LS, _ }) ->
    {selected, ItemRows} = execute_get_item_rows(LU, LS),
    {selected, AffiliationRows} = execute_get_affiliation_rows(LU, LS),
    {selected, SubRows} = execute_get_subscriptions_rows(LU, LS),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_bare(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare({ LU, LS, _ }) ->
    {selected, ItemRows} = execute_get_item_rows(LU, LS),
    {selected, AffiliationRows} = execute_get_affiliation_rows(LU, LS),
    {selected, SubRows} = execute_get_subscriptions_rows_resource(LU, LS, <<>>),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_bare_and_full(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare_and_full({ LU, LS, LR } = LJID) ->
    {ok, StatesBare} = get_states_by_bare(LJID),
    {selected, SubRows} = execute_get_subscriptions_rows_resource(LU, LS, LR),
    StatesFull = build_states([], [], SubRows),
    {ok, StatesFull ++ StatesBare}.

-spec get_idxs_of_own_nodes_with_pending_subs(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:nodeIdx()]}.
get_idxs_of_own_nodes_with_pending_subs({ LU, LS, _ }) ->
    {selected, Rows} =
        execute_get_idxs_of_own_nodes_with_pending_subs(LS, aff2int(owner), LU, sub2int(pending)),
    {ok, [ mongoose_rdbms:result_to_integer(Nidx) || {Nidx} <- Rows ]}.

%% ------------------------ Direct #pubsub_item access ------------------------

-spec get_items(Nidx :: mod_pubsub:nodeIdx(), gen_pubsub_node:get_item_options()) ->
    {ok, {[mod_pubsub:pubsubItem()], none}}.
get_items(Nidx, Opts) ->
    MaxItems = maps:get(max_items, Opts, undefined),
    ItemIds = maps:get(item_ids, Opts, undefined),
    Rows = get_item_rows(Nidx, MaxItems, ItemIds),
    Result = [item_to_record(Row) || Row <- Rows],
    {ok, {Result, none}}.

-spec get_item_rows(Nidx :: mod_pubsub:nodeIdx(),
                    MaxItems :: undefined | non_neg_integer(),
                    ItemIds :: undefined | mod_pubsub:itemId()) -> [tuple()].
get_item_rows(Nidx, undefined, undefined) ->
    {selected, Rows} = execute_get_items(Nidx),
    Rows;
get_item_rows(Nidx, MaxItems, undefined) ->
    {selected, Rows} = execute_get_items(Nidx, MaxItems),
    Rows;
get_item_rows(Nidx, MaxItems, ItemIds) ->
    %% Returned items have same order as ItemIds
    get_item_rows_acc(Nidx, MaxItems, ItemIds, []).

get_item_rows_acc(_Nidx, _MaxItems, [], AccRows) -> AccRows;
get_item_rows_acc(Nidx, MaxItems, [ItemId | ItemIds], AccRows) ->
    case execute_get_item(Nidx, ItemId) of
        {selected, []} ->
            get_item_rows_acc(Nidx, MaxItems, ItemIds, AccRows);
        {selected, [Item]} when MaxItems =:= undefined;
                                length(AccRows) < MaxItems ->
            get_item_rows_acc(Nidx, MaxItems, ItemIds, [Item | AccRows]);
        {selected, [_]} ->
            AccRows
    end.

-spec get_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) ->
    {ok, mod_pubsub:pubsubItem()} | {error, item_not_found}.
get_item(Nidx, ItemId) ->
    case execute_get_item(Nidx, ItemId) of
        {selected, []} ->
            {error, item_not_found};
        {selected, [Item]} ->
            {ok, item_to_record(Item)}
    end.

-spec set_item(Item :: mod_pubsub:pubsubItem()) -> ok.
set_item(#pubsub_item{itemid = {ItemId, NodeIdx},
                      creation = {CreatedAtNow, {CreatedLUser, CreatedLServer, _}},
                      modification = {ModifiedAtNow, {ModifiedLUser, ModifiedLServer, ModifiedLResource}},
                      publisher = PublisherIn,
                      payload = Payload}) ->
    PayloadWrapped = #xmlel{name = <<"item">>, children = Payload},
    PayloadXML = exml:to_binary(PayloadWrapped),
    CreatedAt = CreatedAtNow,
    ModifiedAt = ModifiedAtNow,
    Publisher = null_or_bin_jid(PublisherIn),
    InsertParams = [NodeIdx, ItemId, CreatedLUser, CreatedLServer, CreatedAt,
                    ModifiedLUser, ModifiedLServer, ModifiedLResource, ModifiedAt,
                    Publisher, PayloadXML],
    UpdateParams = [ModifiedLUser, ModifiedLServer, ModifiedLResource, ModifiedAt,
                    Publisher, PayloadXML],
    UniqueKeyValues  = [NodeIdx, ItemId],
    {updated, _} = rdbms_queries:execute_upsert(global, pubsub_item_upsert,
                                                InsertParams, UpdateParams, UniqueKeyValues),
    ok.

-spec del_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) -> ok.
del_item(Nidx, ItemId) ->
    {updated, _} = execute_del_item(Nidx, ItemId),
    ok.

-spec del_items(Nidx :: mod_pubsub:nodeIdx(), [ItemId :: mod_pubsub:itemId()]) -> ok.
del_items(_, []) ->
    ok;
del_items(Nidx, ItemIds) ->
    [{updated, _} = execute_del_item(Nidx, Item) || Item <- ItemIds],
    ok.

% ------------------- Node management --------------------------------

-spec create_node(Nidx :: mod_pubsub:nodeIdx(), Owner :: jid:ljid()) -> ok.
create_node(Nidx, LJID) ->
    set_affiliation(Nidx, LJID, owner).

-spec del_node(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
del_node(Nidx) ->
    {ok, States} = get_states(Nidx),
    {updated, _} = execute_delete_all_subscriptions_id(Nidx),
    {updated, _} = execute_delete_all_items(Nidx),
    {updated, _} = execute_delete_all_affiliations(Nidx),
    {ok, States}.

-spec set_node(Node :: mod_pubsub:pubsubNode()) -> {ok, mod_pubsub:nodeIdx()}.
set_node(#pubsub_node{nodeid = {Key, Name}, id = undefined, type = Type,
                      owners = Owners, options = Opts, parents = Parents}) ->
    ExtKey = encode_key(Key),
    ExtOwners = jiffy:encode([jid:to_binary(Owner) || Owner <- Owners]),
    ExtOpts = jiffy:encode({Opts}),
    {updated, 1} = execute_insert_pubsub_node(ExtKey, Name, Type, ExtOwners, ExtOpts),
    {selected, [Row]} = execute_select_node_by_key_and_name(ExtKey, Name),
    #pubsub_node{id = Nidx} = decode_pubsub_node_row(Row),
    set_parents(Name, Parents),
    {ok, Nidx};

set_node(#pubsub_node{nodeid = {_, Name}, id = Nidx, type = Type,
                      owners = Owners, options = Opts, parents = Parents}) ->
    OwnersJid = [jid:to_binary(Owner) || Owner <- Owners],
    execute_update_pubsub_node(Type, jiffy:encode(OwnersJid), jiffy:encode({Opts}), Nidx),
    execute_del_parents(Name),
    set_parents(Name, Parents),
    {ok, Nidx}.

-spec set_parents(mod_pubsub:nodeId(), [mod_pubsub:nodeId()]) -> ok.
set_parents(Name, Parents) ->
    [execute_insert_parent(Name, ParentName) || ParentName <- Parents],
    ok.

-spec find_node_by_id(Nidx :: mod_pubsub:nodeIdx()) ->
    {error, not_found} | {ok, mod_pubsub:pubsubNode()}.
find_node_by_id(Nidx) ->
    case execute_select_node_by_id(Nidx) of
        {selected, []} ->
            {error, not_found};
        {selected, [Row]} ->
            {ok, decode_pubsub_node_row(Row)}
    end.

-spec find_node_by_name(Key :: mod_pubsub:hostPubsub() | jid:ljid(),
                        Node :: mod_pubsub:nodeId()) ->
    mod_pubsub:pubsubNode() | false.
find_node_by_name(Key, Node) ->
    case execute_select_node_by_key_and_name(encode_key(Key), Node) of
        {selected, [Row]} ->
            decode_pubsub_node_row(Row);
        {selected, []} ->
            false
    end.

decode_pubsub_node_row({Nidx, KeySQL, Name, Type, Owners, Options}) ->
    Key = decode_key(KeySQL),
    {DecodedOpts} = jiffy:decode(Options),
    DecodedOptions = [maybe_option_value_to_atom(key_to_existing_atom(Item)) ||
                      Item <- DecodedOpts],
    DecodedOwners = [jid:to_lower(jid:from_binary(Owner)) ||
                     Owner <- jiffy:decode(Owners)],
    #pubsub_node{nodeid = {Key, Name},
                 id = mongoose_rdbms:result_to_integer(Nidx),
                 type = Type,
                 owners = DecodedOwners,
                 options = DecodedOptions}.

maybe_option_value_to_atom({access_model, Value}) ->
    {access_model, binary_to_existing_atom(Value, utf8)};
maybe_option_value_to_atom({publish_model, Value}) ->
    {publish_model, binary_to_existing_atom(Value, utf8)};
maybe_option_value_to_atom({notification_type, Value}) ->
    {notification_type, binary_to_existing_atom(Value, utf8)};
maybe_option_value_to_atom({node_type, Value}) ->
    {node_type, binary_to_existing_atom(Value, utf8)};
maybe_option_value_to_atom({send_last_published_item, Value}) ->
    {send_last_published_item, binary_to_existing_atom(Value, utf8)};
maybe_option_value_to_atom(Other) ->
    Other.

-spec find_nodes_by_key(Key :: mod_pubsub:hostPubsub() | jid:ljid()) ->
    [mod_pubsub:pubsubNode()].
find_nodes_by_key(Key) ->
    {selected, Rows} =
        execute_select_nodes_by_key(encode_key(Key)),
    [decode_pubsub_node_row(Row) || Row <- Rows].

-spec delete_node(Node :: mod_pubsub:pubsubNode()) -> ok.
delete_node(#pubsub_node{nodeid = {Key, Node}}) ->
    {updated, _} = execute_delete_node(encode_key(Key), Node),
    ok.

-spec get_subnodes(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId() | <<>>) ->
    [mod_pubsub:pubsubNode()].
get_subnodes(Key, Node) ->
    {selected, Rows} = execute_select_subnodes(encode_key(Key), Node),
    [decode_pubsub_node_row(Row) || Row <- Rows].

-spec get_parentnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
get_parentnodes_tree(Key, Node) ->
    find_nodes_with_parents(Key, [Node], 0, []).

find_nodes_with_parents(_, [], _, Acc) ->
    Acc;
find_nodes_with_parents(_, _, 100, Acc) ->
    ?LOG_WARNING(#{what => pubsub_max_depth_reached, pubsub_nodes => Acc}),
    Acc;
find_nodes_with_parents(Key, Nodes, Depth, Acc) ->
    ExtKey = encode_key(Key),
    Rows = lists:flatmap(fun(Name) ->
                                 {selected, Rs} = execute_select_node_with_parents(ExtKey, Name),
                                 Rs
                         end, Nodes),
    Map = lists:foldl(fun update_nodes_map/2, #{}, Rows),
    MapTransformer = fun(Nidx, #{name := NodeName,
                                 next_level := Parents}, {ParentsAcc, NodesAcc}) ->
                             NewParents = [Parents | ParentsAcc],
                             Node = #pubsub_node{id = Nidx,
                                                 nodeid = {Key, NodeName},
                                                 parents = Parents},
                             NewNodes = [Node | NodesAcc],
                             {NewParents, NewNodes}
                     end,

    {Parents, NewNodes} = maps:fold(MapTransformer, {[], []}, Map),
    NewAcc = [{Depth, NewNodes} | Acc],
    find_nodes_with_parents(Key, lists:flatten(Parents), Depth + 1, NewAcc).

update_nodes_map({NidxSQL, NodeName, NextLevelNodeName}, Map) ->
    Nidx = mongoose_rdbms:result_to_integer(NidxSQL),
    case maps:get(Nidx, Map, undefined) of
        undefined ->
            NewNode = #{name => NodeName,
                        next_level => maybe_add_parent_to_list(NextLevelNodeName, [])},
            Map#{Nidx => NewNode};
        #{next_level := NextLevelNodes} = Node ->
            UpdatedNode = Node#{next_level := maybe_add_parent_to_list(NextLevelNodeName, NextLevelNodes)},
            Map#{Nidx := UpdatedNode}
    end.

maybe_add_parent_to_list(null, List) ->
    List;
maybe_add_parent_to_list(ParentName, List) ->
    [ParentName | List].

-spec get_subnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
get_subnodes_tree(Key, Node) ->
    find_subnodes(Key, [Node], 0, []).

find_subnodes(_Key, [], _, Acc) ->
    Acc;
find_subnodes(_, _, 100, Acc) ->
    ?LOG_WARNING(#{what => pubsub_max_depth_reached, pubsub_nodes => Acc}),
    Acc;
find_subnodes(Key, Nodes, Depth, Acc) ->
    ExtKey = encode_key(Key),
    Rows = lists:flatmap(fun(Name) ->
                                 {selected, Rs} = execute_select_node_with_children(ExtKey, Name),
                                 Rs
                         end, Nodes),
    Map = lists:foldl(fun update_nodes_map/2, #{}, Rows),
    MapTransformer = fun(Nidx, #{name := NodeName,
                                 next_level := Subnodes}, {SubnodesAcc, NodesAcc}) ->
                             NewSubnodes = [Subnodes | SubnodesAcc],
                             Node = #pubsub_node{id = Nidx,
                                                 nodeid = {Key, NodeName}},
                             NewNodes = [Node | NodesAcc],
                             {NewSubnodes, NewNodes}
                     end,
    {Subnodes, NewNodes} = maps:fold(MapTransformer, {[], []}, Map),
    NewAcc = [{Depth, NewNodes} | Acc],
    find_subnodes(Key, lists:flatten(Subnodes), Depth + 1, NewAcc).

% ------------------- Affiliations --------------------------------

-spec set_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LJID :: jid:ljid(),
                      Affiliation :: mod_pubsub:affiliation()) -> ok.
set_affiliation(Nidx, { LU, LS, _ } = LJID, none) ->
    BareLJID = jid:to_bare(LJID),
    case get_node_entity_subscriptions(Nidx, BareLJID) of
        {ok, []} ->
            del_state(Nidx, BareLJID);
        _ ->
            delete_affiliation_wo_subs_check(Nidx, LU, LS)
    end;
set_affiliation(Nidx, { LU, LS, _ }, Affiliation) ->
    Aff = aff2int(Affiliation),
    InsertParams = [Nidx, LU, LS, Aff],
    UpdateParams = [Aff],
    UniqueKeyValues  = [Nidx, LU, LS],
    {updated, _} = rdbms_queries:execute_upsert(global, pubsub_affiliation_upsert,
                                                InsertParams, UpdateParams, UniqueKeyValues),
    ok.

-spec get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LJID :: jid:ljid()) ->
    {ok, mod_pubsub:affiliation()}.
get_affiliation(Nidx, { LU, LS, _ }) ->
    case execute_get_affiliation(Nidx, LU, LS) of
        {selected, [{AffInt}]} ->
            {ok, sql2aff(AffInt)};
        {selected, []} ->
            {ok, none}
    end.

% ------------------- Subscriptions --------------------------------

-spec add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                       LJID :: jid:ljid(),
                       Sub :: mod_pubsub:subscription(),
                       SubId :: mod_pubsub:subId(),
                       SubOpts :: mod_pubsub:subOptions()) -> ok.
add_subscription(Nidx, { LU, LS, LR }, Sub, SubId, SubOpts) ->
    EncodedOpts = jiffy:encode({SubOpts}),
    {updated, _} = execute_insert_subscription(Nidx, LU, LS, LR, sub2int(Sub), SubId, EncodedOpts),
    ok.

-spec set_subscription_opts(Nidx :: mod_pubsub:nodeIdx(),
                            LJID :: jid:ljid(),
                            SubId :: mod_pubsub:subId(),
                            Opts :: mod_pubsub:subOptions()) -> ok.
set_subscription_opts(Nidx, { LU, LS, LR }, SubId, Opts) ->
    EncodedOpts = jiffy:encode({Opts}),
    {updated, _} = execute_update_subscription_opts(EncodedOpts, Nidx, LU, LS, LR, SubId),
    ok.

-spec get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(),
           Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           SubOpts :: mod_pubsub:subOptions()}]}.
get_node_subscriptions(Nidx) ->
    {selected, QueryResult} = execute_get_node_subs(Nidx),
    {ok, [{{LU, LS, LR}, sql2sub(SubInt), SubId, sql_to_sub_opts(SubOpts)}
          || {LU, LS, LR, SubInt, SubId, SubOpts} <- QueryResult ]}.

-spec get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                    LJID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           SubOpts :: mod_pubsub:subOptions()}]}.
get_node_entity_subscriptions(Nidx, { LU, LS, LR }) ->
    {selected, QueryResult} = execute_get_node_entity_subs(LS, Nidx, LU, LR),
    {ok, [{sql2sub(SubInt), SubId, sql_to_sub_opts(SubOpts)}
          || {SubInt, SubId, SubOpts} <- QueryResult ]}.

-spec delete_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LJID :: jid:ljid(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
delete_subscription(Nidx, { LU, LS, LR }, SubId) ->
    {updated, _} = execute_delete_subscription(LS, Nidx, LU, LR, SubId),
    ok.

-spec delete_all_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                               LJID :: jid:ljid()) ->
    ok.
delete_all_subscriptions(Nidx, { LU, LS, LR } = LJID) ->
    case get_affiliation(Nidx, LJID) of
        {ok, none} ->
            del_state(Nidx, LJID);
        _ ->
            delete_all_subscriptions_wo_aff_check(Nidx, LU, LS, LR)
    end,
    ok.

-spec update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LJID :: jid:ljid(),
                          Subscription :: mod_pubsub:subscription(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
update_subscription(Nidx, { LU, LS, LR }, Subscription, SubId) ->
    {updated, _} = execute_update_subscription(sub2int(Subscription), Nidx, LU, LS, LR, SubId),
    ok.

% ------------------- Items --------------------------------

-spec add_item(Nidx :: mod_pubsub:nodeIdx(),
               LJID :: jid:ljid(),
               Item :: mod_pubsub:pubsubItem()) ->
    ok.
add_item(_Nidx, _, Item) ->
    set_item(Item),
    ok.

%% TODO: Make public at some point
-spec get_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                       LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:itemId()]}.
get_entity_items(Nidx, { LU, LS, _ }) ->
    {selected, ItemIds} = execute_get_entity_items(LS, Nidx, LU),
    {ok, [ ItemId || {ItemId} <- ItemIds]}.

-spec remove_items(Nidx :: mod_pubsub:nodeIdx(),
                   LJID :: jid:ljid(),
                   ItemIds :: [mod_pubsub:itemId()]) ->
    ok.
remove_items(Nidx, { LU, LS, _ }, ItemIds) ->
    lists:foreach(fun(ItemId) ->
        {updated, _} = execute_delete_item(LS, Nidx, LU, ItemId)
    end, ItemIds).

-spec remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.
remove_all_items(Nidx) ->
    {updated, _} = execute_delete_all_items(Nidx),
    ok.

% ------------------- GDPR-related --------------------------------

get_user_payloads(LUser, LServer) ->
    case execute_get_user_items(LUser, LServer) of
        {selected, Items} ->
            [[NodeName, ItemId, strip_payload(PayloadDB)] || {NodeName, ItemId, PayloadDB} <- Items]
    end.

get_user_nodes(LUser, LServer) ->
    LJID = jid:to_binary({LUser, LServer, <<>>}),
    {selected, Nodes} = execute_select_nodes_by_owner(LJID),
    lists:map(fun tuple_to_list/1, Nodes).

get_user_subscriptions(LUser, LServer) ->
    {selected, Nodes} = execute_get_user_subscriptions(LUser, LServer),
    lists:map(fun tuple_to_list/1, Nodes).

strip_payload(PayloadDB) ->
    PayloadXML = mongoose_rdbms:unescape_binary(global, PayloadDB),
    {ok, #xmlel{children = Payload}} = exml:parse(PayloadXML),
    exml:to_binary(Payload).

-spec delete_user_subscriptions(jid:ljid()) -> ok.
delete_user_subscriptions({ LU, LS, _ }) ->
    {updated, _} = execute_delete_user_subscriptions(LS, LU),
    ok.

find_nodes_by_affiliated_user({ LU, LS, _ }) ->
    {selected, NodesWithAffs} = execute_select_nodes_by_affiliated_user(LU, LS),
    lists:map(fun decode_pubsub_node_with_aff_row/1, NodesWithAffs).

decode_pubsub_node_with_aff_row(Row) ->
    [Aff | NodeRow] = tuple_to_list(Row),
    {decode_pubsub_node_row(list_to_tuple(NodeRow)), sql2aff(Aff)}.

%%====================================================================
%% Helpers
%%====================================================================

-spec item_columns() -> binary().
item_columns() ->
     <<"nidx, itemid, created_luser, created_lserver, created_at, "
       "modified_luser, modified_lserver, modified_lresource, modified_at, "
       "publisher, payload">>.

-spec pubsub_node_fields() -> binary().
pubsub_node_fields() ->
    <<"nidx, p_key, name, type, owners, options">>.

-spec pubsub_node_fields_pn() -> binary().
pubsub_node_fields_pn() ->
    <<"pn.nidx, pn.p_key, pn.name, pn.type, pn.owners, pn.options">>.

-spec del_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid()) -> ok.
del_state(Nidx, {LU, LS, LR}) ->
    delete_all_subscriptions_wo_aff_check(Nidx, LU, LS, LR),
    delete_affiliation_wo_subs_check(Nidx, LU, LS),
    ok.

-spec delete_all_subscriptions_wo_aff_check(Nidx :: mod_pubsub:nodeIdx(),
                                            LU :: jid:luser(),
                                            LS :: jid:lserver(),
                                            LR :: jid:lresource()) -> ok.
delete_all_subscriptions_wo_aff_check(Nidx, LU, LS, LR) ->
    {updated, _} = execute_delete_all_subscriptions(Nidx, LU, LS, LR),
    ok.

-spec delete_affiliation_wo_subs_check(Nidx :: mod_pubsub:nodeIdx(),
                                       LU :: jid:luser(),
                                       LS :: jid:lserver()) -> ok.
delete_affiliation_wo_subs_check(Nidx, LU, LS) ->
    {updated, _} = execute_delete_affiliation(Nidx, LU, LS),
    ok.

-type item_row() :: { NidxSql :: integer() | binary(),
                      LU :: binary(),
                      LS :: binary(),
                      ItemId :: binary() }.
-type aff_row() :: { NidxSql :: integer() | binary(),
                     LU :: binary(),
                     LS :: binary(),
                     AffInt :: integer() }.
-type sub_row() :: { NidxSql :: integer() | binary(),
                     LU :: binary(),
                     LS :: binary(),
                     LR :: binary(),
                     TypeInt :: integer(),
                     SubId :: binary() }.

-spec build_states(ItemRows :: [item_row()], AffRows :: [aff_row()], SubRows :: [sub_row()]) ->
    [mod_pubsub:pubsubState()].
build_states(ItemRows, AffRows, SubRows) ->
    Result1 = item_rows_to_states(ItemRows, #{}),
    Result2 = aff_rows_to_states(AffRows, Result1),
    maps:values(sub_rows_to_states(SubRows, Result2)).

item_rows_to_states([], Acc) ->
    Acc;
item_rows_to_states([{ NidxSql, LU, LS, ItemId } | RRows], Acc) ->
    Nidx = mongoose_rdbms:result_to_integer(NidxSql),
    LJID = { LU, LS, <<>> },
    PS = maps:get({LJID, Nidx}, Acc, #pubsub_state{ stateid = {LJID, Nidx} }),
    #pubsub_state{ items = Items0 } = PS,
    NAcc = Acc#{ {LJID, Nidx} => PS#pubsub_state{ items = [ItemId | Items0] } },
    item_rows_to_states(RRows, NAcc).

aff_rows_to_states([], Acc) ->
    Acc;
aff_rows_to_states([{ NidxSql, LU, LS, AffInt } | RRows], Acc) ->
    Nidx = mongoose_rdbms:result_to_integer(NidxSql),
    LJID = { LU, LS, <<>> },
    PS = maps:get({LJID, Nidx}, Acc, #pubsub_state{ stateid = {LJID, Nidx} }),
    NAcc = Acc#{ {LJID, Nidx} => PS#pubsub_state{ affiliation = sql2aff(AffInt) } },
    aff_rows_to_states(RRows, NAcc).

sub_rows_to_states([], Acc) ->
    Acc;
sub_rows_to_states([{ NidxSql, LU, LS, LR, TypeInt, SubId } | RRows], Acc) ->
    Nidx = mongoose_rdbms:result_to_integer(NidxSql),
    LJID = { LU, LS, LR },
    PS = maps:get({LJID, Nidx}, Acc, #pubsub_state{ stateid = {LJID, Nidx} }),
    #pubsub_state{ subscriptions = Subs0 } = PS,
    NAcc = Acc#{ {LJID, Nidx} => PS#pubsub_state{
                                subscriptions = [{sql2sub(TypeInt), SubId} | Subs0] } },
    sub_rows_to_states(RRows, NAcc).

-spec aff2int(mod_pubsub:affiliation()) -> integer().
aff2int(none) -> 0;
aff2int(owner) -> 1;
aff2int(publisher) -> 2;
aff2int(publish_only) -> 3;
aff2int(member) -> 4;
aff2int(outcast) -> 5.

-spec sql2aff(integer() | binary()) -> mod_pubsub:affiliation().
sql2aff(SqlInt) ->
    int2aff(mongoose_rdbms:result_to_integer(SqlInt)).

-spec int2aff(integer()) -> mod_pubsub:affiliation().
int2aff(0) -> none;
int2aff(1) -> owner;
int2aff(2) -> publisher;
int2aff(3) -> publish_only;
int2aff(4) -> member;
int2aff(5) -> outcast.

-spec sub2int(mod_pubsub:subscription()) -> integer().
sub2int(none) -> 0;
sub2int(pending) -> 1;
sub2int(subscribed) -> 3.

-spec sql2sub(integer() | binary()) -> mod_pubsub:subscription().
sql2sub(SqlInt) ->
    int2sub(mongoose_rdbms:result_to_integer(SqlInt)).

-spec int2sub(integer()) -> mod_pubsub:subscription().
int2sub(0) -> none;
int2sub(1) -> pending;
int2sub(3) -> subscribed.

sql_to_sub_opts(SqlOpts) ->
    {Opts} = jiffy:decode(SqlOpts),
    lists:map(fun key_to_existing_atom/1, Opts).

item_to_record({NodeIdx, ItemId, CreatedLUser, CreatedLServer, CreatedAt,
                ModifiedLUser, ModifiedLServer, ModifiedLResource, ModifiedAt,
                PublisherIn, PayloadDB}) ->
    PayloadXML = mongoose_rdbms:unescape_binary(global, PayloadDB),
    {ok, #xmlel{children = Payload}} = exml:parse(PayloadXML),
    ItemAndNodeId = {ItemId, mongoose_rdbms:result_to_integer(NodeIdx)},
    Creation = {mongoose_rdbms:result_to_integer(CreatedAt),
                {CreatedLUser, CreatedLServer, <<>>}},
    Modification = {mongoose_rdbms:result_to_integer(ModifiedAt),
                    {ModifiedLUser, ModifiedLServer, ModifiedLResource}},
    Publisher = decode_publisher(PublisherIn),
    #pubsub_item{itemid = ItemAndNodeId,
                 creation = Creation,
                 modification = Modification,
                 publisher = Publisher,
                 payload = Payload}.

decode_publisher(null) ->
    undefined;
decode_publisher(Binary) ->
    %% Silently returns `error` if parsing fails.
    jid:from_binary(Binary).

encode_key(Key) when is_binary(Key) ->
    Key;
encode_key({_, _, _} = JID) ->
    jid:to_binary(JID).

decode_key(KeySQL) ->
    case jid:from_binary(KeySQL) of
        #jid{luser = <<>>, lserver = Host, lresource = <<>>} ->
            Host;
        #jid{luser = LUser, lserver = LServer, lresource = LResource} ->
            {LUser, LServer, LResource}
    end.

null_or_bin_jid(undefined) ->
    null;
null_or_bin_jid(Jid) ->
    jid:to_binary(Jid).

key_to_existing_atom({Key, Value}) when is_atom(Key)->
    {Key, Value};
key_to_existing_atom({Key, Value}) ->
    {binary_to_existing_atom(Key, utf8), Value}.

