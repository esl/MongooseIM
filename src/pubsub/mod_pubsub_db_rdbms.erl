%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_rdbms.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub RDBMS backend
%%% Created : 2 Nov 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_rdbms).
-author('piotr.nosek@erlang-solutions.com').
-author('michal.piotrowski@erlang-solutions.com').

-include("pubsub.hrl").
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
         del_node/1
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

% For SQL queries
-export([aff2int/1, sub2int/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() ->
    mod_pubsub_db_mnesia:start().

-spec stop() -> ok.
stop() ->
    mod_pubsub_db_mnesia:stop().

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
    ItemRowsSQL = mod_pubsub_db_rdbms_sql:get_item_rows(Nidx),
    {selected, ItemRows} = mongoose_rdbms:sql_query(global, ItemRowsSQL),
    AffRowsSQL = mod_pubsub_db_rdbms_sql:get_affiliation_rows(Nidx),
    {selected, AffiliationRows} = mongoose_rdbms:sql_query(global, AffRowsSQL),
    SubRowsSQL = mod_pubsub_db_rdbms_sql:get_subscriptions_rows(Nidx),
    {selected, SubRows} = mongoose_rdbms:sql_query(global, SubRowsSQL),
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
    {selected, Rows} = mongoose_rdbms:sql_query(global, IdxsSQL),
    {ok, [ mongoose_rdbms:result_to_integer(Nidx) || {Nidx} <- Rows ]}.

-spec del_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid()) -> ok.
del_state(Nidx, {LU, LS, LR}) ->
    delete_all_subscriptions_wo_aff_check(Nidx, LU, LS, LR),
    delete_affiliation_wo_subs_check(Nidx, LU, LS),
    ok.

%% ------------------------ Direct #pubsub_item access ------------------------

-spec get_items(Nidx :: mod_pubsub:nodeIdx(), gen_pubsub_node:get_item_options()) ->
    {ok, {[mod_pubsub:pubsubItem()], none}}.
get_items(Nidx, Opts) ->
    SQL = mod_pubsub_db_rdbms_sql:get_items(Nidx, Opts),
    {selected, Rows} = mongoose_rdbms:sql_query(global, SQL),
    Result = [item_to_record(Row) || Row <- Rows],
    {ok, {Result, none}}.


-spec get_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) ->
    {ok, mod_pubsub:pubsubItem()} | {error, item_not_found}.
get_item(Nidx, ItemId) ->
    SQL = mod_pubsub_db_rdbms_sql:get_item(Nidx, ItemId),
    case mongoose_rdbms:sql_query(global, SQL) of
        {selected, []} ->
            {error, item_not_found};
        {selected, [Item]} ->
            {ok, item_to_record(Item)}
    end.

-spec set_item(Item :: mod_pubsub:pubsubItem()) -> ok | abort.
set_item(#pubsub_item{itemid = {ItemId, NodeIdx},
                      creation = {CreatedAt, {CreatedLUser, CreatedLServer, _}},
                      modification = {ModifiedAt, {ModifiedLUser, ModifiedLServer, ModifiedLResource}},
                      publisher = Publisher,
                      payload = Payload}) ->
    PayloadXML = #xmlel{name = <<"item">>, children = Payload},
    SQL = mod_pubsub_db_rdbms_sql:upsert_item(NodeIdx, ItemId, CreatedLUser, CreatedLServer, CreatedAt,
                                              ModifiedLUser, ModifiedLServer, ModifiedLResource, ModifiedAt,
                                              Publisher, PayloadXML),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-spec del_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) -> ok.
del_item(Nidx, ItemId) ->
    SQL = mod_pubsub_db_rdbms_sql:del_item(Nidx, ItemId),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-spec del_items(Nidx :: mod_pubsub:nodeIdx(), [ItemId :: mod_pubsub:itemId()]) -> ok.
del_items(_, []) ->
    ok;
del_items(Nidx, ItemIds) ->
    SQL = mod_pubsub_db_rdbms_sql:del_items(Nidx, ItemIds),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
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
    {updated, _} = mongoose_rdbms:sql_query(global, DelAllSubsQ),
    DelAllItemsQ = mod_pubsub_db_rdbms_sql:delete_all_items(Nidx),
    {updated, _} = mongoose_rdbms:sql_query(global, DelAllItemsQ),
    DelAllAffsQ = mod_pubsub_db_rdbms_sql:delete_all_affiliations(Nidx),
    {updated, _} = mongoose_rdbms:sql_query(global, DelAllAffsQ),
    {ok, States}.


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
    SQL = mod_pubsub_db_rdbms_sql:upsert_affiliation(Nidx, LU, LS, aff2int(Affiliation)),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-spec get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LJID :: jid:ljid()) ->
    {ok, mod_pubsub:affiliation()}.
get_affiliation(Nidx, { LU, LS, _ }) ->
    SQL = mod_pubsub_db_rdbms_sql:get_affiliation(Nidx, LU, LS),
    case mongoose_rdbms:sql_query(global, SQL) of
        {selected, [{AffInt}]} ->
            {ok, sql2aff(AffInt)};
        {selected, []} ->
            {ok, none}
    end.

% ------------------- Subscriptions --------------------------------

-spec add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                       LJID :: jid:ljid(),
                       Sub :: mod_pubsub:subscription(),
                       SubId :: mod_pubsub:subId()) -> ok.
add_subscription(Nidx, { LU, LS, LR }, Sub, SubId) ->
    SQL = mod_pubsub_db_rdbms_sql:insert_subscription(Nidx, LU, LS, LR, sub2int(Sub), SubId),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-spec get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(), Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_subscriptions(Nidx) ->
    SQL = mod_pubsub_db_rdbms_sql:get_node_subs(Nidx),
    {selected, QueryResult} = mongoose_rdbms:sql_query(global, SQL),
    {ok, [{{LU, LS, LR}, sql2sub(SubInt), SubId}
          || {LU, LS, LR, SubInt, SubId} <- QueryResult ]}.

-spec get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                    LJID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_entity_subscriptions(Nidx, { LU, LS, LR }) ->
    SQL = mod_pubsub_db_rdbms_sql:get_node_entity_subs(Nidx, LU, LS, LR),
    {selected, QueryResult} = mongoose_rdbms:sql_query(global, SQL),
    {ok, [{sql2sub(SubInt), SubId} || {SubInt, SubId} <- QueryResult ]}.

-spec delete_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LJID :: jid:ljid(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
delete_subscription(Nidx, { LU, LS, LR }, SubId) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_subscription(Nidx, LU, LS, LR, SubId),
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
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
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
    {selected, ItemIds} = mongoose_rdbms:sql_query(global, SQL),
    {ok, [ ItemId || {ItemId} <- ItemIds]}.

-spec remove_items(Nidx :: mod_pubsub:nodeIdx(),
                   LJID :: jid:ljid(),
                   ItemIds :: [mod_pubsub:itemId()]) ->
    ok.
remove_items(Nidx, { LU, LS, _ }, ItemIds) ->
    lists:foreach(fun(ItemId) ->
                          SQL = mod_pubsub_db_rdbms_sql:delete_item(Nidx, LU, LS, ItemId),
                          {updated, _} = mongoose_rdbms:sql_query(global, SQL)
                  end, ItemIds).

-spec remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.
remove_all_items(Nidx) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_all_items(Nidx),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

-spec delete_all_subscriptions_wo_aff_check(Nidx :: mod_pubsub:nodeIdx(),
                                            LU :: jid:luser(),
                                            LS :: jid:lserver(),
                                            LR :: jid:lresource()) -> ok.
delete_all_subscriptions_wo_aff_check(Nidx, LU, LS, LR) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_all_subscriptions(Nidx, LU, LS, LR),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
    ok.

-spec delete_affiliation_wo_subs_check(Nidx :: mod_pubsub:nodeIdx(),
                                       LU :: jid:luser(),
                                       LS :: jid:lserver()) -> ok.
delete_affiliation_wo_subs_check(Nidx, LU, LS) ->
    SQL = mod_pubsub_db_rdbms_sql:delete_affiliation(Nidx, LU, LS),
    {updated, _} = mongoose_rdbms:sql_query(global, SQL),
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
sub2int(unconfigured) -> 2;
sub2int(subscribed) -> 3.

-spec sql2sub(integer() | binary()) -> mod_pubsub:subscription().
sql2sub(SqlInt) ->
    int2sub(mongoose_rdbms:result_to_integer(SqlInt)).

-spec int2sub(integer()) -> mod_pubsub:subscription().
int2sub(0) -> none;
int2sub(1) -> pending;
int2sub(2) -> unconfigured;
int2sub(3) -> subscribed.

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


%%====================================================================
%% Helpers
%%====================================================================
