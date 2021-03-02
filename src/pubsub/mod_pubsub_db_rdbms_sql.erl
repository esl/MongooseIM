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
         get_items/2
        ]).

% Nodes

-export([insert_pubsub_node/5,
         select_nodes_by_key_and_names_in_list_with_parents/2,
         select_nodes_by_key_and_names_in_list_with_children/2,
         set_parents/2]).


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

insert_pubsub_node(Key, Name, Type, Owners, Options) ->
    RDBMSType = {mongoose_rdbms:db_engine(global), mongoose_rdbms_type:get()},
    EscKey = esc_string(Key),
    EscName = esc_string(Name),
    EscType = esc_string(Type),
    EscOwners = esc_string(Owners),
    EscOptions = esc_string(Options),
    sql_node_insert(EscKey, EscName, EscType, EscOwners, EscOptions, RDBMSType).

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
    %% When a list of queries is passed, the first encountered error will be returned.
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

set_parents(Node, Parents) ->
    EscNode = esc_string(Node),
    ParentRows = [parent_row(EscNode, Parent) || Parent <- Parents],
    ["INSERT INTO pubsub_node_collections (name, parent_name) "
     "VALUES ", rdbms_queries:join(ParentRows, ",")].

parent_row(EscNode, Parent) ->
    ["(", EscNode, ", ", esc_string(Parent),")"].

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
