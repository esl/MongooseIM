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
         delete_affiliation/3,
         delete_all_affiliations/1
        ]).

% Subscriptions
-export([
         insert_subscription/6,
         get_node_subs/1,
         get_node_entity_subs/4,
         delete_subscription/5,
         delete_all_subscriptions/4,
         delete_all_subscriptions/1,
         update_subscription/6
        ]).

% Items
-export([
         get_entity_items/3,
         delete_item/4,
         delete_all_items/1,
         upsert_item/11,
         get_items/2,
         get_item/2,
         del_item/2,
         del_items/2
        ]).

%%====================================================================
%% SQL queries
%%====================================================================

% -------------------- State building ----------------------------

-spec get_item_rows(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
get_item_rows(Nidx) ->
    ["SELECT nidx, created_luser, created_lserver, itemid FROM pubsub_items"
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
    ["SELECT nidx, created_luser, created_lserver, itemid FROM pubsub_items"
     " WHERE created_luser = ", esc_string(LU),
     " AND created_lserver = ", esc_string(LS)].

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
     " AND created_luser = ", esc_string(LU),
     " AND created_lserver = ", esc_string(LS)].

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

-spec delete_all_affiliations(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
delete_all_affiliations(Nidx) ->
    ["DELETE FROM pubsub_affiliations"
     " WHERE nidx = ", esc_int(Nidx)].

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

-spec delete_all_subscriptions(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
delete_all_subscriptions(Nidx) ->
    ["DELETE FROM pubsub_subscriptions"
     " WHERE nidx = ", esc_int(Nidx)].

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

-spec get_items(Nidx :: mod_pubsub:nodeIdx(), gen_pubsub_node:get_item_options()) ->
    iolist().
get_items(Nidx, Opts) ->
    MaxItems = maps:get(max_items, Opts, undefined),
    {MySQLOrPgSQLLimit, MSSQLLimit} = maybe_result_limit(MaxItems),
    ["SELECT ", MSSQLLimit, item_columns(), " ",
     "FROM pubsub_items "
     "WHERE nidx=", esc_int(Nidx),
     maybe_item_ids_filter(maps:get(item_ids, Opts, undefined)),
     " ORDER BY modified_at DESC",
     MySQLOrPgSQLLimit].

-spec get_item(mod_pubsub:nodeIdx(), mod_pubsub:itemId()) -> iolist().
get_item(Nidx, ItemId) ->
    ["SELECT ", item_columns(), " "
     "FROM pubsub_items "
     "WHERE nidx=", esc_int(Nidx),
     " AND itemid=", esc_string(ItemId)].

item_columns() ->
     "nidx, itemid, created_luser, created_lserver, created_at, "
     "modified_luser, modified_lserver, modified_lresource, modified_at, "
     "publisher, payload".

maybe_item_ids_filter(undefined) ->
    [];
maybe_item_ids_filter(ItemIds) ->
    EscapedIds = [esc_string(ItemId) || ItemId <- ItemIds],
    Ids = rdbms_queries:join(EscapedIds, ","),
    [" AND itemid IN (", Ids, ")"].

maybe_result_limit(undefined) ->
    {[], []};
maybe_result_limit(Limit) ->
    case {mongoose_rdbms:db_engine(global), mongoose_rdbms_type:get()} of
        {MySQLorPgSQL, _} when MySQLorPgSQL =:= mysql; MySQLorPgSQL =:= pgsql ->
            {[" LIMIT ", esc_int(Limit)], []};
        {odbc, mssql} ->
            {[], [" TOP ", esc_int(Limit), " "]};
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

-spec del_item(mod_pubsub:nodeIdx(), mod_pubsub:itemId()) -> iolist().
del_item(Nidx, ItemId) ->
    ["DELETE FROM pubsub_items "
     "WHERE nidx=", esc_int(Nidx),
      " AND itemid=", esc_string(ItemId)].

-spec del_items(mod_pubsub:nodeIdx(), [mod_pubsub:itemId()]) -> iolist().
del_items(Nidx, ItemIds) ->
    EscapedIds = [esc_string(ItemId) || ItemId <- ItemIds],
    Ids = rdbms_queries:join(EscapedIds, ", "),
    ["DELETE FROM pubsub_items "
     "WHERE nidx=", esc_int(Nidx),
      " AND itemid IN (", Ids,")"].

-spec upsert_item(NodeIdx :: mod_pubsub:nodeIdx(),
                  ItemId :: mod_pubsub:itemId(),
                  CreatedLUser :: jid:luser(),
                  CreatedLServer :: jid:lserver(),
                  CreatedAt :: erlang:timestamp(),
                  ModifiedLUser :: jid:luser(),
                  ModifiedLServer :: jid:lserver(),
                  ModifiedLResource :: jid:lresource(),
                  ModifiedAt :: erlang:timestamp(),
                  Publisher :: undefined | jid:jid(),
                  Payload :: exml:element()) -> iolist().
upsert_item(NodeIdx, ItemId, CreatedLUser, CreatedLServer, CreatedAt,
            ModifiedLUser, ModifiedLServer, ModifiedLResource, ModifiedAt,
            Publisher, Payload) ->
    PayloadXML = exml:to_binary(Payload),
    EscNodeIdx = esc_int(NodeIdx),
    EscItemId = esc_string(ItemId),
    EscCreatedLUser = esc_string(CreatedLUser),
    EscCreatedLServer = esc_string(CreatedLServer),
    EscCreatedAt = esc_int(usec:from_now(CreatedAt)),
    EscModifiedLUser = esc_string(ModifiedLUser),
    EscModifiedLServer = esc_string(ModifiedLServer),
    EscModifiedLResource = esc_string(ModifiedLResource),
    EscModifiedAt = esc_int(usec:from_now(ModifiedAt)),
    EscPublisher = null_or_bin_jid(Publisher),
    EscPayload = esc_string(PayloadXML),

    case {mongoose_rdbms:db_engine(global), mongoose_rdbms_type:get()} of
        {mysql, _} ->
            upsert_item_mysql(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                              EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                              EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload);
        {pgsql, _} ->
            upsert_item_pgsql(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                              EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                              EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload);
        {odbc, mssql} ->
            MSSQLPayload = mongoose_rdbms:use_escaped_binary(mongoose_rdbms:escape_binary(global, PayloadXML)),
            upsert_item_mssql(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                              EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                              EscModifiedLResource, EscModifiedAt, EscPublisher, MSSQLPayload);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

null_or_bin_jid(undefined) ->
    mongoose_rdbms:use_escaped_null(mongoose_rdbms:escape_null());
null_or_bin_jid(Jid) ->
    esc_string(jid:to_binary(Jid)).

upsert_item_mysql(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                  EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                  EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload) ->

    Insert = mysql_and_pgsql_item_insert(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                                         EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                                         EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload),
    OnConflict = mysql_item_conflict(EscModifiedLUser, EscModifiedLServer,
                                     EscModifiedLResource, EscModifiedAt,
                                     EscPublisher, EscPayload),
    [Insert, OnConflict].

upsert_item_pgsql(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                  EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                  EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload) ->

    Insert = mysql_and_pgsql_item_insert(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                                         EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                                         EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload),
    OnConflict = pgsql_item_conflict(EscModifiedLUser, EscModifiedLServer,
                                     EscModifiedLResource, EscModifiedAt,
                                     EscPublisher, EscPayload),
    [Insert, OnConflict].

upsert_item_mssql(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                  EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                  EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload) ->
    ["MERGE INTO pubsub_items with (SERIALIZABLE) as target"
     " USING (SELECT ", EscNodeIdx, " AS nidx, ",
                        EscItemId, " AS itemid)"
            " AS source (nidx, itemid)"
        " ON (target.nidx = source.nidx"
        " AND target.itemid = source.itemid)"
     " WHEN MATCHED THEN UPDATE"
       " SET ", update_fields_on_conflict(EscModifiedLUser, EscModifiedLServer,
                                          EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload),
     " WHEN NOT MATCHED THEN INSERT"
       " (nidx, itemid, created_luser, created_lserver, created_at, "
         "modified_luser, modified_lserver, modified_lresource, modified_at, "
         "publisher, payload)"
         " VALUES (",
          EscNodeIdx, ", ",
          EscItemId, ", ",
          EscCreatedLUser, ", ",
          EscCreatedLServer, ", ",
          EscCreatedAt, ", ",
          EscModifiedLUser, ", ",
          EscModifiedLServer, ", ",
          EscModifiedLResource, ", ",
          EscModifiedAt, ", ",
          EscPublisher, ", ",
          EscPayload,
          ");"].

mysql_and_pgsql_item_insert(EscNodeIdx, EscItemId, EscCreatedLUser, EscCreatedLServer,
                            EscCreatedAt, EscModifiedLUser, EscModifiedLServer,
                            EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload) ->
    ["INSERT INTO pubsub_items (nidx, itemid, created_luser, created_lserver, created_at, "
                               "modified_luser, modified_lserver, modified_lresource, modified_at, "
                               "publisher, payload)"
     " VALUES (",
     EscNodeIdx, ", ",
     EscItemId, ", ",
     EscCreatedLUser, ", ",
     EscCreatedLServer, ", ",
     EscCreatedAt, ", ",
     EscModifiedLUser, ", ",
     EscModifiedLServer, ", ",
     EscModifiedLResource, ", ",
     EscModifiedAt, ", ",
     EscPublisher, ", ",
     EscPayload,
     ")"].

pgsql_item_conflict(EscModifiedLUser, EscModifiedLServer, EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload) ->
    ["ON CONFLICT (nidx, itemid) DO UPDATE SET",
     update_fields_on_conflict(EscModifiedLUser, EscModifiedLServer,
                               EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload)].

mysql_item_conflict(EscModifiedLUser, EscModifiedLServer, EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload) ->
    ["ON DUPLICATE KEY UPDATE",
     update_fields_on_conflict(EscModifiedLUser, EscModifiedLServer,
                               EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload)].

update_fields_on_conflict(EscModifiedLUser, EscModifiedLServer,
                          EscModifiedLResource, EscModifiedAt, EscPublisher, EscPayload) ->
    [" modified_luser = ", EscModifiedLUser,
     ", modified_lserver = ", EscModifiedLServer,
     ", modified_lresource = ", EscModifiedLResource,
     ", modified_at = ", EscModifiedAt,
     ", publisher = ", EscPublisher,
     ", payload = ", EscPayload].

-spec get_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                       LU :: jid:luser(),
                       LS :: jid:lserver()) -> iolist().
get_entity_items(Nidx, LU, LS) ->
    ["SELECT itemid"
     " FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx),
     " AND created_luser = ", esc_string(LU),
     " AND created_lserver = ", esc_string(LS) ].

-spec delete_item(Nidx :: mod_pubsub:nodeIdx(),
                  LU :: jid:luser(),
                  LS :: jid:lserver(),
                  ItemId :: mod_pubsub:itemId()) -> iolist().
delete_item(Nidx, LU, LS, ItemId) ->
    ["DELETE FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx),
     " AND created_luser = ", esc_string(LU),
     " AND created_lserver = ", esc_string(LS),
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

