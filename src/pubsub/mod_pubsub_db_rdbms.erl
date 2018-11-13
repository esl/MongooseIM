%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_rdbms.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub RDBMS backend
%%% Created : 2 Nov 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_rdbms).
-author('piotr.nosek@erlang-solutions.com').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/0, stop/0]).
% Funs execution
-export([transaction/2, dirty/2]).
% Direct #pubsub_state access
-export([del_state/2, get_state/2,
         get_states/1, get_states_by_lus/1, get_states_by_bare/1,
         get_states_by_bare_and_full/1, get_idxs_of_own_nodes_with_pending_subs/1]).
% Node management
-export([
         create_node/2
        ]).
% Affiliations
-export([
         set_affiliation/3,
         get_affiliation/2
        ]).
% Subscriptions
-export([
         add_subscription/4,
         get_node_subscriptions/1,
         get_node_entity_subscriptions/2,
         delete_subscription/3,
         delete_all_subscriptions/2,
         update_subscription/4
        ]).
% Items
-export([
         add_item/3,
         remove_items/3,
         remove_all_items/1
        ]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() ->
    ok.

-spec stop() -> ok.
stop() ->
    ok.

%% ------------------------ Fun execution ------------------------

%% TODO: Replace these with RDBMS counterparts when this backend supports all
%% PubSub operations!

transaction(Fun, ErrorDebug) ->
    mod_pubsub_db_mnesia:transaction(Fun, ErrorDebug).

dirty(Fun, ErrorDebug) ->
    mod_pubsub_db_mnesia:dirty(Fun, ErrorDebug).

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
    {selected, ItemRows} = mongoose_rdbms:sql_query(global, sql_get_item_rows(Nidx)),
    {selected, AffiliationRows} = mongoose_rdbms:sql_query(global, sql_get_affiliation_rows(Nidx)),
    {selected, SubRows} = mongoose_rdbms:sql_query(global, sql_get_subscriptions_rows(Nidx)),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_lus(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_lus({ LU, LS, _ }) ->
    {selected, ItemRows} = mongoose_rdbms:sql_query(
                             global, sql_get_item_rows(LU, LS)),
    {selected, AffiliationRows} = mongoose_rdbms:sql_query(
                                    global, sql_get_affiliation_rows(LU, LS)),
    {selected, SubRows} = mongoose_rdbms:sql_query(
                            global, sql_get_subscriptions_rows(LU, LS)),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_bare(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare({ LU, LS, _ }) ->
    {selected, ItemRows} = mongoose_rdbms:sql_query(
                             global, sql_get_item_rows(LU, LS)),
    {selected, AffiliationRows} = mongoose_rdbms:sql_query(
                                    global, sql_get_affiliation_rows(LU, LS)),
    {selected, SubRows} = mongoose_rdbms:sql_query(
                            global, sql_get_subscriptions_rows(LU, LS, <<>>)),
    States = build_states(ItemRows, AffiliationRows, SubRows),
    {ok, States}.

-spec get_states_by_bare_and_full(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare_and_full({ LU, LS, LR } = LJID) ->
    {ok, StatesBare} = get_states_by_bare(LJID),
    {selected, SubRows} = mongoose_rdbms:sql_query(
                            global, sql_get_subscriptions_rows(LU, LS, LR)),
    StatesFull = build_states([], [], SubRows),
    {ok, StatesFull ++ StatesBare}.

-spec get_idxs_of_own_nodes_with_pending_subs(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:nodeIdx()]}.
get_idxs_of_own_nodes_with_pending_subs({ LU, LS, _ }) ->
    {selected, Rows} = mongoose_rdbms:sql_query(
                         global, sql_get_idxs_of_own_nodes_with_pending_subs(LU, LS)),
    {ok, [ Nidx || {Nidx} <- Rows ]}.

-spec del_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid()) -> ok.
del_state(Nidx, {LU, LS, _} = LJID) ->
    delete_all_subscriptions(Nidx, LJID),
    delete_affiliation_wo_subs_check(Nidx, LU, LS),
    {updated, _} = mongoose_rdbms:sql_query(global, sql_delete_node_entity_items(Nidx, LU, LS)),
    ok.

% ------------------- Node management --------------------------------

-spec create_node(Nidx :: mod_pubsub:nodeIdx(), Owner :: jid:ljid()) -> ok.
create_node(Nidx, LJID) ->
    set_affiliation(Nidx, LJID, owner).

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
set_affiliation(Nidx, { LU, LS, _ } = LJID, Affiliation) ->
    %% TODO: Replace with proper upsert!!!
    case get_affiliation(Nidx, LJID) of
        {ok, none} ->
            SQL = sql_insert_affiliation(Nidx, LU, LS, aff2int(Affiliation)),
            {updated, _} = mongoose_rdbms:sql_query(global, SQL);
        _ ->
            SQL = sql_update_affiliation(Nidx, LU, LS, aff2int(Affiliation)),
            {updated, _} = mongoose_rdbms:sql_query(global, SQL)
    end,
    ok.

-spec get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LJID :: jid:ljid()) ->
    {ok, mod_pubsub:affiliation()}.
get_affiliation(Nidx, { LU, LS, _ }) ->
    SQL = sql_get_affiliation(Nidx, LU, LS),
    case mongoose_rdbms:sql_query(global, SQL) of
        {selected, [{AffInt}]} ->
            {ok, int2aff(AffInt)};
        {selected, []} ->
            {ok, none}
    end.

% ------------------- Subscriptions --------------------------------

-spec add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                       LJID :: jid:ljid(),
                       Sub :: mod_pubsub:subscription(),
                       SubId :: mod_pubsub:subId()) -> ok.
add_subscription(Nidx, { LU, LS, LR }, Sub, SubId) ->
    SQL = sql_insert_subscription(Nidx, LU, LS, LR, sub2int(Sub), SubId),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-spec get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(), Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_subscriptions(Nidx) ->
    SQL = sql_get_node_subs(Nidx),
    {selected, QueryResult} = mongoose_rdbms:sql_query(global, SQL),
    {ok, [{{LU, LS, LR}, int2sub(SubInt), SubId}
          || {LU, LS, LR, SubInt, SubId} <- QueryResult ]}.

-spec get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                    LJID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_entity_subscriptions(Nidx, { LU, LS, LR }) ->
    SQL = sql_get_node_entity_subs(Nidx, LU, LS, LR),
    {selected, QueryResult} = mongoose_rdbms:sql_query(global, SQL),
    {ok, [{int2sub(SubInt), SubId} || {SubInt, SubId} <- QueryResult ]}.

-spec delete_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LJID :: jid:ljid(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
delete_subscription(Nidx, { LU, LS, LR }, SubId) ->
    SQL = sql_delete_subscription(Nidx, LU, LS, LR, SubId),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-spec delete_all_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                               LJID :: jid:ljid()) ->
    ok.
delete_all_subscriptions(Nidx, { LU, LS, LR } = LJID) ->
    case get_affiliation(Nidx, LJID) of
        {ok, none} ->
            del_state(Nidx, LJID);
        _ ->
            SQL = sql_delete_all_subscriptions(Nidx, LU, LS, LR),
            {updated, _} = mongoose_rdbms:sql_query(global, SQL)
    end,
    ok.

-spec update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LJID :: jid:ljid(),
                          Subscription :: mod_pubsub:subscription(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
update_subscription(Nidx, { LU, LS, LR }, Subscription, SubId) ->
    SQL = sql_update_subscription(Nidx, LU, LS, LR, sub2int(Subscription), SubId),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

% ------------------- Items --------------------------------

-spec add_item(Nidx :: mod_pubsub:nodeIdx(),
               LJID :: jid:ljid(),
               ItemId :: mod_pubsub:itemId()) ->
    ok.
add_item(Nidx, { LU, LS, _ }, ItemId) ->
    SQL = sql_insert_item(Nidx, LU, LS, ItemId),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

%% TODO: Make public at some point
-spec get_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                       LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:itemId()]}.
get_entity_items(Nidx, { LU, LS, _ }) ->
    SQL = sql_get_entity_items(Nidx, LU, LS),
    {selected, ItemIds} = mongoose_rdbms:sql_query(global, SQL),
    {ok, [ ItemId || {ItemId} <- ItemIds]}.

-spec remove_items(Nidx :: mod_pubsub:nodeIdx(),
                   LJID :: jid:ljid(),
                   ItemIds :: [mod_pubsub:itemId()]) ->
    ok.
remove_items(Nidx, { LU, LS, _ }, ItemIds) ->
    lists:foreach(fun(ItemId) ->
                          SQL = sql_delete_item(Nidx, LU, LS, ItemId),
                          {updated, _} = mongoose_rdbms:sql_query(global, SQL)
                  end, ItemIds).

-spec remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.
remove_all_items(Nidx) ->
    SQL = sql_delete_all_items(Nidx),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

%%====================================================================
%% SQL queries
%%====================================================================

% -------------------- State building ----------------------------

-spec sql_get_item_rows(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
sql_get_item_rows(Nidx) ->
    ["SELECT nidx, luser, lserver, itemid FROM pubsub_items"
    " WHERE nidx = ", esc_int(Nidx)].

-spec sql_get_affiliation_rows(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
sql_get_affiliation_rows(Nidx) ->
    ["SELECT nidx, luser, lserver, aff FROM pubsub_affiliations"
    " WHERE nidx = ", esc_int(Nidx)].

-spec sql_get_subscriptions_rows(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
sql_get_subscriptions_rows(Nidx) ->
    ["SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions"
    " WHERE nidx = ", esc_int(Nidx)].

-spec sql_get_item_rows(LU :: jid:luser(),
                        LS :: jid:lserver()) -> iolist().
sql_get_item_rows(LU, LS) ->
    ["SELECT nidx, luser, lserver, itemid FROM pubsub_items"
    " WHERE luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS)].

-spec sql_get_affiliation_rows(LU :: jid:luser(),
                               LS :: jid:lserver()) -> iolist().
sql_get_affiliation_rows(LU, LS) ->
    ["SELECT nidx, luser, lserver, aff FROM pubsub_affiliations"
    " WHERE luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS)].

-spec sql_get_subscriptions_rows(LU :: jid:luser(),
                                 LS :: jid:lserver()) -> iolist().
sql_get_subscriptions_rows(LU, LS) ->
    ["SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions"
    " WHERE luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS)].

-spec sql_get_subscriptions_rows(LU :: jid:luser(),
                                 LS :: jid:lserver(),
                                 LR :: jid:lresource()) -> iolist().
sql_get_subscriptions_rows(LU, LS, LR) ->
    ["SELECT nidx, luser, lserver, lresource, type, sub_id FROM pubsub_subscriptions"
    " WHERE luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS),
      " AND lresource = ", esc_string(LR)].

-spec sql_get_idxs_of_own_nodes_with_pending_subs(LU :: jid:luser(),
                                                  LS :: jid:lserver()) -> iolist().
sql_get_idxs_of_own_nodes_with_pending_subs(LU, LS) ->
    ["SELECT DISTINCT s.nidx"
    " FROM pubsub_affiliations AS a INNER JOIN pubsub_subscriptions s ON a.nidx = s.nidx"
    " WHERE a.aff = ", esc_int(aff2int(owner)),
      " AND a.luser = ", esc_string(LU),
      " AND a.lserver = ", esc_string(LS),
      " AND s.type = ", esc_int(sub2int(pending))].

-spec sql_delete_node_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                                   LU :: jid:luser(),
                                   LS :: jid:lserver()) -> iolist().
sql_delete_node_entity_items(Nidx, LU, LS) ->
    ["DELETE FROM pubsub_items"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS)].

% ------------------- Affiliations --------------------------------

%% Due to lack of many specs in mod_pubsub and its submodules,
%% sometimes it's very difficult to predict if we're going to
%% get jid() or ljid()... Will be clarified in the future.

-spec sql_insert_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                             LU :: jid:luser(),
                             LS :: jid:lserver(),
                             AffInt :: integer()) -> iolist().
sql_insert_affiliation(Nidx, LU, LS, AffInt) ->
    ["INSERT INTO pubsub_affiliations (nidx, luser, lserver, aff)"
    " VALUES (", esc_int(Nidx), ", ",
                 esc_string(LU), ", ",
                 esc_string(LS), ", ",
                 esc_int(AffInt), ")"].

-spec sql_update_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                             LU :: jid:luser(),
                             LS :: jid:lserver(),
                             AffInt :: integer()) -> iolist().
sql_update_affiliation(Nidx, LU, LS, AffInt) ->
    ["UPDATE pubsub_affiliations"
    " SET aff = ", esc_int(AffInt),
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS) ].

-spec sql_get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                          LU :: jid:luser(),
                          LS :: jid:lserver()) -> iolist().
sql_get_affiliation(Nidx, LU, LS) ->
    ["SELECT aff"
    " FROM pubsub_affiliations"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS) ].

-spec sql_delete_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                             LU :: jid:luser(),
                             LS :: jid:lserver()) -> iolist().
sql_delete_affiliation(Nidx, LU, LS) ->
    ["DELETE FROM pubsub_affiliations"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS) ].

% ------------------- Subscriptions --------------------------------

-spec sql_insert_subscription(Nidx :: mod_pubsub:nodeIdx(),
                              LU :: jid:luser(),
                              LS :: jid:lserver(),
                              LR :: jid:lresource(),
                              SubInt :: integer(),
                              SubId :: binary()) -> iolist().
sql_insert_subscription(Nidx, LU, LS, LR, SubInt, SubId) ->
    ["INSERT INTO pubsub_subscriptions (nidx, luser, lserver, lresource, type, sub_id)"
    " VALUES (", esc_int(Nidx), ", ",
                 esc_string(LU), ", ",
                 esc_string(LS), ", ",
                 esc_string(LR), ", ",
                 esc_int(SubInt), ", ",
                 esc_string(SubId), ")"].

-spec sql_get_node_subs(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
sql_get_node_subs(Nidx) ->
    ["SELECT luser, lserver, lresource, type, sub_id"
    " FROM pubsub_subscriptions"
    " WHERE nidx = ", esc_int(Nidx)].

-spec sql_get_node_entity_subs(Nidx :: mod_pubsub:nodeIdx(),
                               LU :: jid:luser(),
                               LS :: jid:lserver(),
                               LR :: jid:lresource()) -> iolist().
sql_get_node_entity_subs(Nidx, LU, LS, LR) ->
    ["SELECT type, sub_id"
    " FROM pubsub_subscriptions"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS),
      " AND lresource = ", esc_string(LR)].

-spec sql_delete_subscription(Nidx :: mod_pubsub:nodeIdx(),
                              LU :: jid:luser(),
                              LS :: jid:lserver(),
                              LR :: jid:lresource(),
                              SubId :: binary()) -> iolist().
sql_delete_subscription(Nidx, LU, LS, LR, SubId) ->
    ["DELETE FROM pubsub_subscriptions"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS),
      " AND lresource = ", esc_string(LR),
      " AND sub_id = ", esc_string(SubId)].

-spec sql_delete_all_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                   LU :: jid:luser(),
                                   LS :: jid:lserver(),
                                   LR :: jid:lresource()) -> iolist().
sql_delete_all_subscriptions(Nidx, LU, LS, LR) ->
    ["DELETE FROM pubsub_subscriptions"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS),
      " AND lresource = ", esc_string(LR)].

-spec sql_update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                              LU :: jid:luser(),
                              LS :: jid:lserver(),
                              LR :: jid:lresource(),
                              SubInt :: integer(),
                              SubId :: binary()) -> iolist().
sql_update_subscription(Nidx, LU, LS, LR, SubInt, SubId) ->
    ["UPDATE pubsub_subscriptions",
    " SET type = ", esc_int(SubInt),
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS),
      " AND lresource = ", esc_string(LR),
      " AND sub_id = ", esc_string(SubId)].

% ------------------- Items --------------------------------

-spec sql_insert_item(Nidx :: mod_pubsub:nodeIdx(),
                      LU :: jid:luser(),
                      LS :: jid:lserver(),
                      ItemId :: mod_pubsub:itemId()) -> iolist().
sql_insert_item(Nidx, LU, LS, ItemId) ->
    ["INSERT INTO pubsub_items (nidx, luser, lserver, itemid)"
    " VALUES (", esc_int(Nidx), ", ",
                 esc_string(LU), ", ",
                 esc_string(LS), ", ",
                 esc_string(ItemId), ")"].

-spec sql_get_entity_items(Nidx :: mod_pubsub:nodeIdx(),
                           LU :: jid:luser(),
                           LS :: jid:lserver()) -> iolist().
sql_get_entity_items(Nidx, LU, LS) ->
    ["SELECT itemid"
    " FROM pubsub_items"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS) ].

-spec sql_delete_item(Nidx :: mod_pubsub:nodeIdx(),
                      LU :: jid:luser(),
                      LS :: jid:lserver(),
                      ItemId :: mod_pubsub:itemId()) -> iolist().
sql_delete_item(Nidx, LU, LS, ItemId) ->
    ["DELETE FROM pubsub_items"
    " WHERE nidx = ", esc_int(Nidx),
      " AND luser = ", esc_string(LU),
      " AND lserver = ", esc_string(LS),
      " AND itemid = ", esc_string(ItemId) ].

-spec sql_delete_all_items(Nidx :: mod_pubsub:nodeIdx()) -> iolist().
sql_delete_all_items(Nidx) ->
    ["DELETE FROM pubsub_items"
    " WHERE nidx = ", esc_int(Nidx)].

%%====================================================================
%% Helpers
%%====================================================================

-spec delete_affiliation_wo_subs_check(Nidx :: mod_pubsub:nodeIdx(),
                                       LU :: jid:luser(),
                                       LS :: jid:lserver()) -> ok.
delete_affiliation_wo_subs_check(Nidx, LU, LS) ->
    SQL = sql_delete_affiliation(Nidx, LU, LS),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-type item_row() :: { Nidx :: mod_pubsub:nodeIdx(),
                      LU :: binary(),
                      LS :: binary(),
                      ItemId :: binary() }.
-type aff_row() :: { Nidx :: mod_pubsub:nodeIdx(),
                     LU :: binary(),
                     LS :: binary(),
                     AffInt :: integer() }.
-type sub_row() :: { Nidx :: mod_pubsub:nodeIdx(),
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
item_rows_to_states([{ Nidx, LU, LS, ItemId } | RRows], Acc) ->
    LJID = { LU, LS, <<>> },
    #pubsub_state{ items = Items0 }
    = PS = maps:get({LJID, Nidx}, Acc, #pubsub_state{ stateid = {LJID, Nidx} }),
    NAcc = Acc#{ {LJID, Nidx} => PS#pubsub_state{ items = [ItemId | Items0] } },
    item_rows_to_states(RRows, NAcc).

aff_rows_to_states([], Acc) ->
    Acc;
aff_rows_to_states([{ Nidx, LU, LS, AffInt } | RRows], Acc) ->
    LJID = { LU, LS, <<>> },
    PS = maps:get({LJID, Nidx}, Acc, #pubsub_state{ stateid = {LJID, Nidx} }),
    NAcc = Acc#{ {LJID, Nidx} => PS#pubsub_state{ affiliation = int2aff(AffInt) } },
    aff_rows_to_states(RRows, NAcc).

sub_rows_to_states([], Acc) ->
    Acc;
sub_rows_to_states([{ Nidx, LU, LS, LR, TypeInt, SubId } | RRows], Acc) ->
    LJID = { LU, LS, LR },
    #pubsub_state{ subscriptions = Subs0 }
    = PS = maps:get({LJID, Nidx}, Acc, #pubsub_state{ stateid = {LJID, Nidx} }),
    NAcc = Acc#{ {LJID, Nidx} => PS#pubsub_state{
                                subscriptions = [{int2sub(TypeInt), SubId} | Subs0] } },
    sub_rows_to_states(RRows, NAcc).

esc_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

esc_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

-spec aff2int(mod_pubsub:affiliation()) -> integer().
aff2int(none) -> 0;
aff2int(owner) -> 1;
aff2int(publisher) -> 2;
aff2int(publish_only) -> 3;
aff2int(member) -> 4;
aff2int(outcast) -> 5.

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
sub2int(unconfigured) -> 2;
sub2int(subscribed) -> 3.

-spec int2sub(integer()) -> mod_pubsub:subscription().
int2sub(0) -> none;
int2sub(1) -> pending;
int2sub(2) -> unconfigured;
int2sub(3) -> subscribed.

