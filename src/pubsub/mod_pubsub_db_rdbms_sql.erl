%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_rdbms_sql.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : SQL queries for PubSub RDBMS backend
%%% Created : 15 Nov 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_rdbms_sql).
-author('piotr.nosek@erlang-solutions.com').

% Items
-export([
         get_items/2,
         get_item/2,
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
-export([select_nodes_by_owner/1]).

%%====================================================================
%% SQL queries
%%====================================================================

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

-spec del_items(mod_pubsub:nodeIdx(), [mod_pubsub:itemId()]) -> iolist().
del_items(Nidx, ItemIds) ->
    EscapedIds = [esc_string(ItemId) || ItemId <- ItemIds],
    Ids = rdbms_queries:join(EscapedIds, ", "),
    ["DELETE FROM pubsub_items "
     "WHERE nidx=", esc_int(Nidx),
      " AND itemid IN (", Ids,")"].

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
