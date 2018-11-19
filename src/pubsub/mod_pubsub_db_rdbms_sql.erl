%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_rdbms_sql.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : SQL queries for PubSub RDBMS backend
%%% Created : 15 Nov 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_rdbms_sql).
-author('piotr.nosek@erlang-solutions.com').

% State building
-export([
         get_item_rows/1,
         get_affiliation_rows/1,
         get_subscriptions_rows/1,
         get_item_rows/2,
         get_affiliation_rows/2,
         get_subscriptions_rows/2,
         get_subscriptions_rows/3,
         get_idxs_of_own_nodes_with_pending_subs/2,
         delete_node_entity_items/3
        ]).

% Affiliations
-export([
         upsert_affiliation/4,
         get_affiliation/3,
         delete_affiliation/3
        ]).

% Subscriptions
-export([
         insert_subscription/6,
         get_node_subs/1,
         get_node_entity_subs/4,
         delete_subscription/5,
         delete_all_subscriptions/4,
         update_subscription/6
        ]).

% Items
-export([
         insert_item/4,
         get_entity_items/3,
         delete_item/4,
         delete_all_items/1
        ]).

%%====================================================================
%% SQL queries
%%====================================================================

% -------------------- State building ----------------------------

-spec get_item_rows(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
get_item_rows(Nidx) ->
    ["SELECT nidx, luser, lserver, itemid FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx)].

-spec get_affiliation_rows(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
get_affiliation_rows(Nidx) ->
    ["SELECT nidx, luser, lserver, aff FROM pubsub_affiliations"
     " WHERE nidx = ", esc_int(Nidx)].

-spec get_subscriptions_rows(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
get_subscriptions_rows(Nidx) ->
    ["SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions"
     " WHERE nidx = ", esc_int(Nidx)].

-spec get_item_rows(LU :: jid:luser(),
                    LS :: jid:lserver()) -> iolist().
get_item_rows(LU, LS) ->
    ["SELECT nidx, luser, lserver, itemid FROM pubsub_items"
     " WHERE luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS)].

-spec get_affiliation_rows(LU :: jid:luser(),
                           LS :: jid:lserver()) -> iolist().
get_affiliation_rows(LU, LS) ->
    ["SELECT nidx, luser, lserver, aff FROM pubsub_affiliations"
     " WHERE luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS)].

-spec get_subscriptions_rows(LU :: jid:luser(),
                             LS :: jid:lserver()) -> iolist().
get_subscriptions_rows(LU, LS) ->
    ["SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions"
     " WHERE luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS)].

-spec get_subscriptions_rows(LU :: jid:luser(),
                             LS :: jid:lserver(),
                             LR :: jid:lresource()) -> iolist().
get_subscriptions_rows(LU, LS, LR) ->
    ["SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions"
     " WHERE luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS),
     " AND lresource = ", esc_string(LR)].

-spec get_idxs_of_own_nodes_with_pending_subs(LU :: jid:luser(),
                                              LS :: jid:lserver()) -> iolist().
get_idxs_of_own_nodes_with_pending_subs(LU, LS) ->
    ["SELECT DISTINCT s.nidx"
     " FROM pubsub_affiliations AS a INNER JOIN pubsub_subscriptions s ON a.nidx = s.nidx"
     " WHERE a.aff = ", esc_int(mod_pubsub_db_rdbms:aff2int(owner)),
     " AND a.luser = ", esc_string(LU),
     " AND a.lserver = ", esc_string(LS),
     " AND s.type = ", esc_int(mod_pubsub_db_rdbms:sub2int(pending))].

-spec delete_node_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                               LU :: jid:luser(),
                               LS :: jid:lserver()) -> iolist().
delete_node_entity_items(Nidx, LU, LS) ->
    ["DELETE FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS)].

% ------------------- Affiliations --------------------------------

-spec upsert_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                         LU :: jid:luser(),
                         LS :: jid:lserver(),
                         AffInt :: integer()) -> iolist().
upsert_affiliation(Nidx, LU, LS, AffInt) ->
    case {mongoose_rdbms:db_engine(global), mongoose_rdbms_type:get()} of
        {mysql, _} -> upsert_affiliation_mysql(Nidx, LU, LS, AffInt);
        {pgsql, _} -> upsert_affiliation_pgsql(Nidx, LU, LS, AffInt);
        {odbc, mssql} -> upsert_affiliation_mssql(Nidx, LU, LS, AffInt);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

upsert_affiliation_mysql(Nidx, LU, LS, AffInt) ->
    EscAffInt = esc_int(AffInt),
    ["INSERT INTO pubsub_affiliations (nidx, luser, lserver, aff)"
    " VALUES (", esc_int(Nidx), ", ",
                 esc_string(LU), ", ",
                 esc_string(LS), ", ",
                 EscAffInt, ") ON DUPLICATE KEY",
    " UPDATE aff = ", EscAffInt].

upsert_affiliation_pgsql(Nidx, LU, LS, AffInt) ->
    EscAffInt = esc_int(AffInt),
    ["INSERT INTO pubsub_affiliations (nidx, luser, lserver, aff)"
    " VALUES (", esc_int(Nidx), ", ",
                 esc_string(LU), ", ",
                 esc_string(LS), ", ",
                 EscAffInt, ") ON CONFLICT (nidx, luser, lserver) DO",
    " UPDATE SET aff = ", EscAffInt].

upsert_affiliation_mssql(Nidx, LU, LS, AffInt) ->
    EscNidx = esc_int(Nidx),
    EscLU = esc_string(LU),
    EscLS = esc_string(LS),
    EscAffInt = esc_int(AffInt),
    ["MERGE INTO pubsub_affiliations with (SERIALIZABLE) AS target"
     " USING (SELECT ", EscNidx, " AS nidx,"
                   " ", EscLU, " AS luser,"
                   " ", EscLS, " AS lserver,"
                   " ", EscAffInt, " AS aff)"
     " AS source (nidx, luser, lserver, aff)"
         " ON (target.nidx = source.nidx"
         " AND target.luser = source.luser"
         " AND target.lserver = source.lserver)"
     " WHEN MATCHED THEN UPDATE"
         " SET aff = ", EscAffInt,
     " WHEN NOT MATCHED THEN INSERT"
         " (nidx, luser, lserver, aff)"
         " VALUES (", EscNidx, ", ", EscLU, ", ", EscLS, ", ", EscAffInt, ");"].

-spec get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LU :: jid:luser(),
                      LS :: jid:lserver()) -> iolist().
get_affiliation(Nidx, LU, LS) ->
    ["SELECT aff"
     " FROM pubsub_affiliations"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS) ].

-spec delete_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                         LU :: jid:luser(),
                         LS :: jid:lserver()) -> iolist().
delete_affiliation(Nidx, LU, LS) ->
    ["DELETE FROM pubsub_affiliations"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS) ].

% ------------------- Subscriptions --------------------------------

-spec insert_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LU :: jid:luser(),
                          LS :: jid:lserver(),
                          LR :: jid:lresource(),
                          SubInt :: integer(),
                          SubId :: binary()) -> iolist().
insert_subscription(Nidx, LU, LS, LR, SubInt, SubId) ->
    ["INSERT INTO pubsub_subscriptions (nidx, luser, lserver, lresource, type, sub_id)"
     " VALUES (", esc_int(Nidx), ", ",
     esc_string(LU), ", ",
     esc_string(LS), ", ",
     esc_string(LR), ", ",
     esc_int(SubInt), ", ",
     esc_string(SubId), ")"].

-spec get_node_subs(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
get_node_subs(Nidx) ->
    ["SELECT luser, lserver, lresource, type, sub_id"
     " FROM pubsub_subscriptions"
     " WHERE nidx = ", esc_int(Nidx)].

-spec get_node_entity_subs(Nidx :: mod_pubsub:nodeIdx(),
                           LU :: jid:luser(),
                           LS :: jid:lserver(),
                           LR :: jid:lresource()) -> iolist().
get_node_entity_subs(Nidx, LU, LS, LR) ->
    ["SELECT type, sub_id"
     " FROM pubsub_subscriptions"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS),
     " AND lresource = ", esc_string(LR)].

-spec delete_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LU :: jid:luser(),
                          LS :: jid:lserver(),
                          LR :: jid:lresource(),
                          SubId :: binary()) -> iolist().
delete_subscription(Nidx, LU, LS, LR, SubId) ->
    ["DELETE FROM pubsub_subscriptions"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS),
     " AND lresource = ", esc_string(LR),
     " AND sub_id = ", esc_string(SubId)].

-spec delete_all_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                               LU :: jid:luser(),
                               LS :: jid:lserver(),
                               LR :: jid:lresource()) -> iolist().
delete_all_subscriptions(Nidx, LU, LS, LR) ->
    ["DELETE FROM pubsub_subscriptions"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS),
     " AND lresource = ", esc_string(LR)].

-spec update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LU :: jid:luser(),
                          LS :: jid:lserver(),
                          LR :: jid:lresource(),
                          SubInt :: integer(),
                          SubId :: binary()) -> iolist().
update_subscription(Nidx, LU, LS, LR, SubInt, SubId) ->
    ["UPDATE pubsub_subscriptions",
     " SET type = ", esc_int(SubInt),
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS),
     " AND lresource = ", esc_string(LR),
     " AND sub_id = ", esc_string(SubId)].

% ------------------- Items --------------------------------

-spec insert_item(Nidx :: mod_pubsub:nodeIdx(),
                  LU :: jid:luser(),
                  LS :: jid:lserver(),
                  ItemId :: mod_pubsub:itemId()) -> iolist().
insert_item(Nidx, LU, LS, ItemId) ->
    ["INSERT INTO pubsub_items (nidx, luser, lserver, itemid)"
     " VALUES (", esc_int(Nidx), ", ",
     esc_string(LU), ", ",
     esc_string(LS), ", ",
     esc_string(ItemId), ")"].

-spec get_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                       LU :: jid:luser(),
                       LS :: jid:lserver()) -> iolist().
get_entity_items(Nidx, LU, LS) ->
    ["SELECT itemid"
     " FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS) ].

-spec delete_item(Nidx :: mod_pubsub:nodeIdx(),
                  LU :: jid:luser(),
                  LS :: jid:lserver(),
                  ItemId :: mod_pubsub:itemId()) -> iolist().
delete_item(Nidx, LU, LS, ItemId) ->
    ["DELETE FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS),
     " AND itemid = ", esc_string(ItemId) ].

-spec delete_all_items(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
delete_all_items(Nidx) ->
    ["DELETE FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx)].

%%====================================================================
%% Helpers
%%====================================================================

esc_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

esc_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

