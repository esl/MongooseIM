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
         get_affiliation/3,
         delete_affiliation/3,
         delete_all_affiliations/1
        ]).

% Subscriptions
-export([
         insert_subscription/7,
         update_subscription_opts/6,
         get_node_subs/1,
         get_node_entity_subs/4,
         delete_subscription/5,
         delete_all_subscriptions/4,
         delete_all_subscriptions/1,
         delete_user_subscriptions/2,
         update_subscription/6
        ]).

% Items
-export([
         get_entity_items/3,
         delete_item/4,
         delete_all_items/1,
         get_items/2,
         get_item/2,
         del_item/2,
         del_items/2
        ]).

% Nodes

-export([insert_pubsub_node/5,
         update_pubsub_node/4,
         select_node_by_key_and_name/2,
         select_node_by_id/1,
         select_nodes_by_key/1,
         select_nodes_in_list_with_key/2,
         select_nodes_by_key_and_names_in_list_with_parents/2,
         select_nodes_by_key_and_names_in_list_with_children/2,
         select_nodes_by_affiliated_user/2,
         select_subnodes/2,
         delete_node/2,
         set_parents/2,
         del_parents/1]).

% GDPR
-export([get_user_items/2,
         select_nodes_by_owner/1,
         get_user_subscriptions/2]).

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
                          SubId :: binary(),
                          SubOpts :: binary()) -> iolist().
insert_subscription(Nidx, LU, LS, LR, SubInt, SubId, SubOpts) ->
    ["INSERT INTO pubsub_subscriptions (nidx, luser, lserver, lresource, type, sub_id, options)"
     " VALUES (", esc_int(Nidx), ", ",
                  esc_string(LU), ", ",
                  esc_string(LS), ", ",
                  esc_string(LR), ", ",
                  esc_int(SubInt), ", ",
                  esc_string(SubId), ", ",
                  esc_string(SubOpts), ")"].

-spec update_subscription_opts(Nidx :: mod_pubsub:nodeIdx(),
                               LU :: jid:luser(),
                               LS :: jid:lserver(),
                               LR :: jid:lresource(),
                               SubId :: binary(),
                               EncodedOpts :: binary()) -> iolist().
update_subscription_opts(Nidx, LU, LS, LR, SubId, EncodedOpts) ->
    ["UPDATE pubsub_subscriptions",
     " SET options = ", esc_string(EncodedOpts),
     " WHERE nidx = ", esc_int(Nidx),
     " AND luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS),
     " AND lresource = ", esc_string(LR),
     " AND sub_id = ", esc_string(SubId)].

-spec get_node_subs(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
get_node_subs(Nidx) ->
    ["SELECT luser, lserver, lresource, type, sub_id, options"
     " FROM pubsub_subscriptions"
     " WHERE nidx = ", esc_int(Nidx)].

-spec get_node_entity_subs(Nidx :: mod_pubsub:nodeIdx(),
                           LU :: jid:luser(),
                           LS :: jid:lserver(),
                           LR :: jid:lresource()) -> iolist().
get_node_entity_subs(Nidx, LU, LS, LR) ->
    ["SELECT type, sub_id, options"
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

-spec delete_user_subscriptions(LU :: jid:luser(), LS :: jid:lserver()) -> iolist().
delete_user_subscriptions(LU, LS) ->
    ["DELETE FROM pubsub_subscriptions"
     " WHERE luser = ", esc_string(LU),
     " AND lserver = ", esc_string(LS)].

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

-spec get_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                       LU :: jid:luser(),
                       LS :: jid:lserver()) -> iolist().
get_entity_items(Nidx, LU, LS) ->
    ["SELECT itemid"
     " FROM pubsub_items"
     " WHERE nidx = ", esc_int(Nidx),
     " AND created_luser = ", esc_string(LU),
     " AND created_lserver = ", esc_string(LS) ].

-spec get_user_items(LU :: jid:luser(), LS :: jid:lserver()) -> iolist().
get_user_items(LU, LS) ->
    ["SELECT name, itemid, payload"
    " FROM pubsub_items"
    " INNER JOIN pubsub_nodes"
    " ON pubsub_items.nidx = pubsub_nodes.nidx"
    " WHERE created_luser = ", esc_string(LU),
    " AND created_lserver = ", esc_string(LS) ].

-spec get_user_subscriptions(LU :: jid:luser(), LS :: jid:lserver()) -> iolist().
get_user_subscriptions(LU, LS) ->
    ["SELECT name"
    " FROM pubsub_subscriptions"
    " INNER JOIN pubsub_nodes"
    " ON pubsub_subscriptions.nidx = pubsub_nodes.nidx"
    " WHERE luser = ", esc_string(LU),
    " AND lserver = ", esc_string(LS) ].

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

insert_pubsub_node(Key, Name, Type, Owners, Options) ->
    RDBMSType = {mongoose_rdbms:db_engine(global), mongoose_rdbms_type:get()},
    EscKey = esc_string(Key),
    EscName = esc_string(Name),
    EscType = esc_string(Type),
    EscOwners = esc_string(Owners),
    EscOptions = esc_string(Options),
    sql_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions, RDBMSType).

update_pubsub_node(Nidx, Type, Owners, Options) ->
    EscNidx = esc_int(Nidx),
    EscType = esc_string(Type),
    EscOwners = esc_string(Owners),
    EscOptions = esc_string(Options),
    sql_node_update(EscNidx, EscType, EscOwners, EscOptions).

sql_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions, {odbc, mssql}) ->
    Query = ["INSERT INTO pubsub_nodes (p_key, name, type, owners, options) "
             "OUTPUT inserted.nidx "
             "VALUES (",
             EscKey, ", ",
             EscName, ", ",
             EscType, ", ",
             EscOwners, ", ",
             EscOptions, ");"],
    Res = mongoose_rdbms:sql_query(global, Query),
    convert_sql_nidx(Res);
sql_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions, {pgsql, _}) ->
    Query = [common_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions),
             [" RETURNING nidx;"]],
    Res = mongoose_rdbms:sql_query(global, Query),
    convert_sql_nidx(Res);
sql_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions, {mysql, _}) ->
    Queries = [common_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions),
               ["; SELECT last_insert_id();"]],
    %% When a list of qeries is passed, the firs encountered error will be returned.
    %% Otherwise last statement result is returned.
    Res = mongoose_rdbms:sql_query(global, Queries),
    convert_sql_nidx(Res).

common_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions) ->
    ["INSERT INTO pubsub_nodes (p_key, name, type, owners, options) VALUES (",
     EscKey, ", ",
     EscName, ", ",
     EscType, ", ",
     EscOwners, ", ",
     EscOptions, ")"].

sql_node_update(EscNidx, EscType, EscOwners, EscOptions) ->
    Query = [" UPDATE pubsub_nodes SET type = ", EscType, ", "
             " owners = ", EscOwners, ", "
             " options = ", EscOptions,
             " WHERE nidx = ", EscNidx, ";"],
    {updated, _} = mongoose_rdbms:sql_query(global, Query).

set_parents(Node, Parents) ->
    EscNode = esc_string(Node),
    ParentRows = [parent_row(EscNode, Parent) || Parent <- Parents],
    ["INSERT INTO pubsub_node_collections (name, parent_name) "
     "VALUES ", rdbms_queries:join(ParentRows, ",")].

del_parents(Node) ->
    ["DELETE FROM pubsub_node_collections ",
     "WHERE name = ", esc_string(Node)].

parent_row(EscNode, Parent) ->
    ["(", EscNode, ", ", esc_string(Parent),")"].

select_node_by_key_and_name(Key, Name) ->
    ["SELECT ", pubsub_node_fields(), " from pubsub_nodes "
     "WHERE p_key = ", esc_string(Key),
     " AND name = ", esc_string(Name)].

-spec select_node_by_id(mod_pubsub:nodeIdx()) -> iolist().
select_node_by_id(Nidx) ->
    ["SELECT ", pubsub_node_fields(), " from pubsub_nodes "
     "WHERE nidx = ", esc_int(Nidx)].

-spec select_nodes_by_key(Key :: binary()) -> iolist().
select_nodes_by_key(Key) ->
    ["SELECT ", pubsub_node_fields(), " from pubsub_nodes "
     "WHERE p_key = ", esc_string(Key)].

-spec select_nodes_by_owner(LJID :: binary()) -> iolist().
select_nodes_by_owner(LJID) ->
    %% TODO I wrote that code in tears in my eyes. Its super inefficient,
    %% there should be separate table for many-to-many relation and index
    case {mongoose_rdbms:db_engine(global), mongoose_rdbms_type:get()} of
        {mysql, _} ->
            ["SELECT name, type"
                " FROM pubsub_nodes"
                " WHERE owners = convert(", esc_string(iolist_to_binary(["[\"", LJID, "\"]"])), ", JSON);"
            ];
        {pgsql, _}  ->
            ["SELECT name, type"
            " FROM pubsub_nodes"
            " WHERE owners ::json->>0 like ", esc_string(LJID),
                " AND JSON_ARRAY_LENGTH(owners) = 1"
            ];
        {odbc, mssql} ->
            ["SELECT name, type"
            " FROM pubsub_nodes"
            " WHERE cast(owners as nvarchar(max)) = ", esc_string(iolist_to_binary(["[\"", LJID, "\"]"]))
            ]
    end.

-spec select_nodes_by_affiliated_user(LU :: jid:luser(), LS :: jid:lserver()) -> iolist().
select_nodes_by_affiliated_user(LU, LS) ->
    ["SELECT aff, ", pubsub_node_fields("pn"),
     " FROM pubsub_affiliations AS pa"
     " INNER JOIN pubsub_nodes AS pn ON pa.nidx = pn.nidx"
     " WHERE luser = ", esc_string(LU),
       " AND lserver = ", esc_string(LS)].

-spec select_nodes_in_list_with_key(Key :: binary(), Nodes :: [binary()]) -> iolist().
select_nodes_in_list_with_key(Key, Nodes) ->
    EscapedNames = [esc_string(Node) || Node <- Nodes],
    NodeNames = rdbms_queries:join(EscapedNames, ","),
    ["SELECT ", pubsub_node_fields(), " from pubsub_nodes "
     "WHERE p_key = ", esc_string(Key),
     " AND name IN (", NodeNames, ")"].

-spec select_nodes_by_key_and_names_in_list_with_parents(Key :: binary(), Nodes :: [binary()]) -> iolist().
select_nodes_by_key_and_names_in_list_with_parents(Key, Nodes) ->
    EscapedNames = [esc_string(Node) || Node <- Nodes],
    NodeNames = rdbms_queries:join(EscapedNames, ","),
    ["SELECT pn.nidx, pn.name, collection.parent_name from pubsub_nodes as pn "
     "LEFT JOIN pubsub_node_collections as collection ON "
     "pn.name = collection.name "
     "WHERE pn.p_key = ", esc_string(Key),
     " AND pn.name IN (", NodeNames, ")"].

-spec select_nodes_by_key_and_names_in_list_with_children(Key :: binary(), Nodes :: [binary()]) -> iolist().
select_nodes_by_key_and_names_in_list_with_children(Key, Nodes) ->
    EscapedNames = [esc_string(Node) || Node <- Nodes],
    NodeNames = rdbms_queries:join(EscapedNames, ","),
    ["SELECT pn.nidx, pn.name, collection.name from pubsub_nodes as pn "
     "LEFT JOIN pubsub_node_collections as collection ON "
     "pn.name = collection.parent_name "
     "WHERE pn.p_key = ", esc_string(Key),
     " AND pn.name IN (", NodeNames, ")"].

-spec select_subnodes(Key :: binary(), Node :: binary()) -> iolist().
%% This clause is to find top level nodes (without any parent)
select_subnodes(Key, <<>>) ->
    ["SELECT ", pubsub_node_fields("pn"), " from pubsub_nodes as pn "
     "LEFT JOIN pubsub_node_collections as collection ON "
     "pn.name = collection.name "
     "WHERE p_key = ", esc_string(Key),
     " AND collection.parent_name IS NULL"];
%% This clause is to find all children of node Node
select_subnodes(Key, Node) ->
    ["SELECT ", pubsub_node_fields("pn"), " from pubsub_nodes as pn "
     "INNER JOIN pubsub_node_collections as collection ON "
     "pn.name = collection.name AND "
     "collection.parent_name = ", esc_string(Node), " "
     "WHERE p_key = ", esc_string(Key)].

pubsub_node_fields() ->
    "nidx, p_key, name, type, owners, options".

pubsub_node_fields(Prefix) ->
    Names = ["nidx", "p_key", "name", "type", "owners", "options"],
    NamesWithPrefix = [ [Prefix, ".", Name] || Name <- Names],
    rdbms_queries:join(NamesWithPrefix, ", ").

-spec delete_node(Key :: binary(), Node :: binary()) -> iolist().
delete_node(Key, Node) ->
    ["DELETE from pubsub_nodes "
     "WHERE p_key = ", esc_string(Key),
     " AND name = ", esc_string(Node)].

%%====================================================================
%% Helpers
%%====================================================================

esc_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

esc_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

%% MSSQL and MYSQL
convert_sql_nidx({selected, [{Nidx}]}) ->
    {ok, mongoose_rdbms:result_to_integer(Nidx)};
%% PGSQL
convert_sql_nidx({updated, _, [{Nidx}]}) ->
    {ok, mongoose_rdbms:result_to_integer(Nidx)};
convert_sql_nidx(Res) ->
    {error, {bad_rdbms_response, Res}}.
