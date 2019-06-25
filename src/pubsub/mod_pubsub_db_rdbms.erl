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

-export([start/0, stop/0]).
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

% For SQL queries
-export([aff2int/1, sub2int/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() ->
    mod_pubsub_db_mnesia:start(),
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
    mod_pubsub_db_mnesia:stop().

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
    ItemRowsSQL = mod_pubsub_db_rdbms_sql:get_item_rows(Nidx),
    {selected, ItemRows} = mongoose_rdbms:sql_query_t(ItemRowsSQL),
    AffRowsSQL = mod_pubsub_db_rdbms_sql:get_affiliation_rows(Nidx),
    {selected, AffiliationRows} = mongoose_rdbms:sql_query_t(AffRowsSQL),
    SubRowsSQL = mod_pubsub_db_rdbms_sql:get_subscriptions_rows(Nidx),
    {selected, SubRows} = mongoose_rdbms:sql_query_t(SubRowsSQL),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_lus(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_lus({ LU, LS, _ }) ->
    {selected, ItemRows} = mongoose_rdbms:sql_query(
                             global, mod_pubsub_db_rdbms_sql:get_item_rows(LU, LS)),
    {selected, AffiliationRows} = mongoose_rdbms:sql_query(
                                    global, mod_pubsub_db_rdbms_sql:get_affiliation_rows(LU, LS)),
    {selected, SubRows} = mongoose_rdbms:sql_query(
                            global, mod_pubsub_db_rdbms_sql:get_subscriptions_rows(LU, LS)),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_bare(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare({ LU, LS, _ }) ->
    {selected, ItemRows} = mongoose_rdbms:sql_query(
                             global, mod_pubsub_db_rdbms_sql:get_item_rows(LU, LS)),
    {selected, AffiliationRows} = mongoose_rdbms:sql_query(
                                    global, mod_pubsub_db_rdbms_sql:get_affiliation_rows(LU, LS)),
    {selected, SubRows} = mongoose_rdbms:sql_query(
                            global, mod_pubsub_db_rdbms_sql:get_subscriptions_rows(LU, LS, <<>>)),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_bare_and_full(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare_and_full({ LU, LS, LR } = LJID) ->
    {ok, StatesBare} = get_states_by_bare(LJID),
    {selected, SubRows} = mongoose_rdbms:sql_query(
                            global, mod_pubsub_db_rdbms_sql:get_subscriptions_rows(LU, LS, LR)),
    StatesFull = build_states([], [], SubRows),
    {ok, StatesFull ++ StatesBare}.

-spec get_idxs_of_own_nodes_with_pending_subs(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:nodeIdx()]}.
get_idxs_of_own_nodes_with_pending_subs({ LU, LS, _ }) ->
    IdxsSQL = mod_pubsub_db_rdbms_sql:get_idxs_of_own_nodes_with_pending_subs(LU, LS),
    {selected, Rows} = mongoose_rdbms:sql_query_t(IdxsSQL),
    {ok, [ mongoose_rdbms:result_to_integer(Nidx) || {Nidx} <- Rows ]}.

%% ------------------------ Direct #pubsub_item access ------------------------

-spec get_items(Nidx :: mod_pubsub:nodeIdx(), gen_pubsub_node:get_item_options()) ->
    {ok, {[mod_pubsub:pubsubItem()], none}}.
get_items(Nidx, Opts) ->
    SQL = mod_pubsub_db_rdbms_sql:get_items(Nidx, Opts),
    {selected, Rows} = mongoose_rdbms:sql_query_t(SQL),
    Result = [item_to_record(Row) || Row <- Rows],
    {ok, {Result, none}}.


-spec get_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) ->
    {ok, mod_pubsub:pubsubItem()} | {error, item_not_found}.
get_item(Nidx, ItemId) ->
    SQL = mod_pubsub_db_rdbms_sql:get_item(Nidx, ItemId),
    case mongoose_rdbms:sql_query_t(SQL) of
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
    CreatedAt = usec:from_now(CreatedAtNow),
    ModifiedAt = usec:from_now(ModifiedAtNow),
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
    SQL = mod_pubsub_db_rdbms_sql:del_item(Nidx, ItemId),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

-spec del_items(Nidx :: mod_pubsub:nodeIdx(), [ItemId :: mod_pubsub:itemId()]) -> ok.
del_items(_, []) ->
    ok;
del_items(Nidx, ItemIds) ->
    SQL = mod_pubsub_db_rdbms_sql:del_items(Nidx, ItemIds),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

% ------------------- Node management --------------------------------

-spec create_node(Nidx :: mod_pubsub:nodeIdx(), Owner :: jid:ljid()) -> ok.
create_node(Nidx, LJID) ->
    set_affiliation(Nidx, LJID, owner).

-spec del_node(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
del_node(Nidx) ->
    {ok, States} = get_states(Nidx),
    DelAllSubsQ = mod_pubsub_db_rdbms_sql:delete_all_subscriptions(Nidx),
    {updated, _} = mongoose_rdbms:sql_query_t(DelAllSubsQ),
    DelAllItemsQ = mod_pubsub_db_rdbms_sql:delete_all_items(Nidx),
    {updated, _} = mongoose_rdbms:sql_query_t(DelAllItemsQ),
    DelAllAffsQ = mod_pubsub_db_rdbms_sql:delete_all_affiliations(Nidx),
    {updated, _} = mongoose_rdbms:sql_query_t(DelAllAffsQ),
    {ok, States}.

-spec set_node(Node :: mod_pubsub:pubsubNode()) -> {ok, mod_pubsub:nodeIdx()}.
set_node(#pubsub_node{nodeid = {Key, Name}, id = undefined, type = Type,
                      owners = Owners, options = Opts, parents = Parents}) ->
    OwnersJid = [jid:to_binary(Owner) || Owner <- Owners],
    {ok, Nidx} = mod_pubsub_db_rdbms_sql:insert_pubsub_node(encode_key(Key), Name, Type,
                                                     jiffy:encode(OwnersJid),
                                                     jiffy:encode({Opts})),
    maybe_set_parents(Name, Parents),
    {ok, Nidx};

set_node(#pubsub_node{nodeid = {_, Name}, id = Nidx, type = Type,
                      owners = Owners, options = Opts, parents = Parents}) ->
    OwnersJid = [jid:to_binary(Owner) || Owner <- Owners],
    mod_pubsub_db_rdbms_sql:update_pubsub_node(Nidx, Type,
                                               jiffy:encode(OwnersJid),
                                               jiffy:encode({Opts})),
    maybe_set_parents(Name, Parents),
    {ok, Nidx}.

maybe_set_parents(_Name, []) ->
    ok;
maybe_set_parents(Name, Parents) ->
    DelParentsSQL = mod_pubsub_db_rdbms_sql:del_parents(Name),
    {updated, _} = mongoose_rdbms:sql_query_t(DelParentsSQL),
    SetParentsSQL = mod_pubsub_db_rdbms_sql:set_parents(Name, Parents),
    {updated, _} = mongoose_rdbms:sql_query_t(SetParentsSQL).


-spec find_node_by_id(Nidx :: mod_pubsub:nodeIdx()) ->
    {error, not_found} | {ok, mod_pubsub:pubsubNode()}.
find_node_by_id(Nidx) ->
    SQL = mod_pubsub_db_rdbms_sql:select_node_by_id(Nidx),
    case mongoose_rdbms:sql_query_t(SQL) of
        {selected, []} ->
            {error, not_found};
        {selected, [Row]} ->
            {ok, decode_pubsub_node_row(Row)}
    end.

-spec find_node_by_name(Key :: mod_pubsub:hostPubsub() | jid:ljid(),
                        Node :: mod_pubsub:nodeId()) ->
    mod_pubsub:pubsubNode() | false.
find_node_by_name(Key, Node) ->
    SQL = mod_pubsub_db_rdbms_sql:select_node_by_key_and_name(encode_key(Key), Node),
    case mongoose_rdbms:sql_query_t(SQL) of
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
    SQL = mod_pubsub_db_rdbms_sql:select_nodes_by_key(encode_key(Key)),
    {selected, Rows} = mongoose_rdbms:sql_query_t(SQL),
    [decode_pubsub_node_row(Row) || Row <- Rows].


-spec delete_node(Node :: mod_pubsub:pubsubNode()) -> ok.
delete_node(#pubsub_node{nodeid = {Key, Node}}) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_node(encode_key(Key), Node),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

-spec get_subnodes(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId() | <<>>) ->
    [mod_pubsub:pubsubNode()].
get_subnodes(Key, Node) ->
    SQL = mod_pubsub_db_rdbms_sql:select_subnodes(encode_key(Key), Node),
    {selected, Rows} = mongoose_rdbms:sql_query_t(SQL),
    [decode_pubsub_node_row(Row) || Row <- Rows].

-spec get_parentnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
get_parentnodes_tree(Key, Node) ->
    find_nodes_with_parents(Key, [Node], 0, []).

find_nodes_with_parents(_, [], _, Acc) ->
    Acc;
find_nodes_with_parents(_, _, 100, Acc) ->
    ?WARNING_MSG("event=max_depth_reached, nodes=~p", [Acc]),
    Acc;
find_nodes_with_parents(Key, Nodes, Depth, Acc) ->
    SQL = mod_pubsub_db_rdbms_sql:select_nodes_by_key_and_names_in_list_with_parents(
            encode_key(Key), Nodes),
    {selected, Rows} = mongoose_rdbms:sql_query_t(SQL),
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
    ?WARNING_MSG("event=max_depth_reached, nodes=~p", [Acc]),
    Acc;
find_subnodes(Key, Nodes, Depth, Acc) ->
    SQL = mod_pubsub_db_rdbms_sql:select_nodes_by_key_and_names_in_list_with_children(
            encode_key(Key), Nodes),
    {selected, Rows} = mongoose_rdbms:sql_query_t(SQL),
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
    SQL = mod_pubsub_db_rdbms_sql:get_affiliation(Nidx, LU, LS),
    case mongoose_rdbms:sql_query_t(SQL) of
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
    SQL = mod_pubsub_db_rdbms_sql:insert_subscription(Nidx, LU, LS, LR, sub2int(Sub),
                                                      SubId, EncodedOpts),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

-spec set_subscription_opts(Nidx :: mod_pubsub:nodeIdx(),
                            LJID :: jid:ljid(),
                            SubId :: mod_pubsub:subId(),
                            Opts :: mod_pubsub:subOptions()) -> ok.
set_subscription_opts(Nidx, { LU, LS, LR }, SubId, Opts) ->
    EncodedOpts = jiffy:encode({Opts}),
    SQL = mod_pubsub_db_rdbms_sql:update_subscription_opts(Nidx, LU, LS, LR, SubId, EncodedOpts),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

-spec get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(),
           Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           SubOpts :: mod_pubsub:subOptions()}]}.
get_node_subscriptions(Nidx) ->
    SQL = mod_pubsub_db_rdbms_sql:get_node_subs(Nidx),
    {selected, QueryResult} = mongoose_rdbms:sql_query_t(SQL),
    {ok, [{{LU, LS, LR}, sql2sub(SubInt), SubId, sql_to_sub_opts(SubOpts)}
          || {LU, LS, LR, SubInt, SubId, SubOpts} <- QueryResult ]}.

-spec get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                    LJID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           SubOpts :: mod_pubsub:subOptions()}]}.
get_node_entity_subscriptions(Nidx, { LU, LS, LR }) ->
    SQL = mod_pubsub_db_rdbms_sql:get_node_entity_subs(Nidx, LU, LS, LR),
    {selected, QueryResult} = mongoose_rdbms:sql_query_t(SQL),
    {ok, [{sql2sub(SubInt), SubId, sql_to_sub_opts(SubOpts)}
          || {SubInt, SubId, SubOpts} <- QueryResult ]}.

-spec delete_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LJID :: jid:ljid(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
delete_subscription(Nidx, { LU, LS, LR }, SubId) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_subscription(Nidx, LU, LS, LR, SubId),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
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
    SQL = mod_pubsub_db_rdbms_sql:update_subscription(Nidx, LU, LS, LR,
                                                      sub2int(Subscription), SubId),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
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
    SQL = mod_pubsub_db_rdbms_sql:get_entity_items(Nidx, LU, LS),
    {selected, ItemIds} = mongoose_rdbms:sql_query_t(SQL),
    {ok, [ ItemId || {ItemId} <- ItemIds]}.

-spec remove_items(Nidx :: mod_pubsub:nodeIdx(),
                   LJID :: jid:ljid(),
                   ItemIds :: [mod_pubsub:itemId()]) ->
    ok.
remove_items(Nidx, { LU, LS, _ }, ItemIds) ->
    lists:foreach(fun(ItemId) ->
                          SQL = mod_pubsub_db_rdbms_sql:delete_item(Nidx, LU, LS, ItemId),
                          {updated, _} = mongoose_rdbms:sql_query_t(SQL)
                  end, ItemIds).

-spec remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.
remove_all_items(Nidx) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_all_items(Nidx),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

% ------------------- GDPR-related --------------------------------

get_user_payloads(LUser, LServer) ->
    SQL = mod_pubsub_db_rdbms_sql:get_user_items(LUser, LServer),
    case mongoose_rdbms:sql_query(global, SQL) of
        {selected, Items} ->
            [[NodeName, ItemId, strip_payload(PayloadDB)] || {NodeName, ItemId, PayloadDB} <- Items]
    end.

get_user_nodes(LUser, LServer) ->
    LJID = jid:to_binary({LUser, LServer, <<>>}),
    SQL = mod_pubsub_db_rdbms_sql:select_nodes_by_owner(LJID),
    {selected, Nodes} =  mongoose_rdbms:sql_query(global, SQL),
    lists:map(fun tuple_to_list/1, Nodes).

get_user_subscriptions(LUser, LServer) ->
    SQL = mod_pubsub_db_rdbms_sql:get_user_subscriptions(LUser, LServer),
    {selected, Nodes} =  mongoose_rdbms:sql_query(global, SQL),
    lists:map(fun tuple_to_list/1, Nodes).

strip_payload(PayloadDB) ->
    PayloadXML = mongoose_rdbms:unescape_binary(global, PayloadDB),
    {ok, #xmlel{children = Payload}} = exml:parse(PayloadXML),
    exml:to_binary(Payload).

-spec delete_user_subscriptions(jid:ljid()) -> ok.
delete_user_subscriptions({ LU, LS, _ }) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_user_subscriptions(LU, LS),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

find_nodes_by_affiliated_user({ LU, LS, _ }) ->
    SQL = mod_pubsub_db_rdbms_sql:select_nodes_by_affiliated_user(LU, LS),
    {selected, NodesWithAffs} = mongoose_rdbms:sql_query(global, SQL),
    lists:map(fun decode_pubsub_node_with_aff_row/1, NodesWithAffs).

decode_pubsub_node_with_aff_row(Row) ->
    [Aff | NodeRow] = tuple_to_list(Row),
    {decode_pubsub_node_row(list_to_tuple(NodeRow)), sql2aff(Aff)}.

%%====================================================================
%% Helpers
%%====================================================================

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
    SQL = mod_pubsub_db_rdbms_sql:delete_all_subscriptions(Nidx, LU, LS, LR),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
    ok.

-spec delete_affiliation_wo_subs_check(Nidx :: mod_pubsub:nodeIdx(),
                                       LU :: jid:luser(),
                                       LS :: jid:lserver()) -> ok.
delete_affiliation_wo_subs_check(Nidx, LU, LS) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_affiliation(Nidx, LU, LS),
    {updated, _} = mongoose_rdbms:sql_query_t(SQL),
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
    Creation = {usec:to_now(mongoose_rdbms:result_to_integer(CreatedAt)),
                {CreatedLUser, CreatedLServer, <<>>}},
    Modification = {usec:to_now(mongoose_rdbms:result_to_integer(ModifiedAt)),
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

