%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_mnesia.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub Mnesia backend
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_mnesia).
-author('piotr.nosek@erlang-solutions.com').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/0, stop/0]).
% Funs execution
-export([transaction/2, dirty/2]).
% Direct #pubsub_state access
-export([del_state/2, get_state/2,
         get_states/1, get_states_by_lus/1, get_states_by_bare/1,
         get_states_by_bare_and_full/1, get_own_nodes_states/1]).
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

% Whole Items

-export([
         get_items/2,
         get_item/2,
         set_item/1,
         del_item/2,
         del_items/2
        ]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() ->
    mnesia:create_table(pubsub_state,
                        [{disc_copies, [node()]},
                         {type, ordered_set},
                         {attributes, record_info(fields, pubsub_state)}]),
    mnesia:add_table_copy(pubsub_state, node(), disc_copies),
    mnesia:create_table(pubsub_item,
                        [{disc_only_copies, [node()]},
                         {attributes, record_info(fields, pubsub_item)}]),
    mnesia:add_table_copy(pubsub_item, node(), disc_only_copies),
    ok.

-spec stop() -> ok.
stop() ->
    ok.

%% ------------------------ Fun execution ------------------------

-spec transaction(Fun :: fun(() -> {result | error, any()}),
                  ErrorDebug :: map()) ->
    {result | error, any()}.
transaction(Fun, ErrorDebug) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            mod_pubsub_db:transaction_error(Reason, ErrorDebug)
    end.

-spec dirty(Fun :: fun(() -> {result | error, any()}),
            ErrorDebug :: map()) ->
    {result | error, any()}.
dirty(Fun, ErrorDebug) ->
    try mnesia:sync_dirty(Fun, []) of
        Result ->
            Result
    catch
        C:R ->
            mod_pubsub_db:dirty_error(C, R, erlang:get_stacktrace(), ErrorDebug)
    end.

%% ------------------------ Direct #pubsub_state access ------------------------

-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                JID :: jid:jid()) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, JID) ->
    get_state(Nidx, JID, read).

-spec get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states(Nidx) ->
    States = case catch mnesia:match_object(
                          #pubsub_state{stateid = {'_', Nidx}, _ = '_'}) of
                 List when is_list(List) -> List;
                 _ -> []
             end,
    {ok, States}.

-spec get_states_by_lus(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_lus(#jid{ luser = LUser, lserver = LServer }) ->
    {ok, mnesia:match_object(#pubsub_state{stateid = {{LUser, LServer, '_'}, '_'}, _ = '_'})}.

-spec get_states_by_bare(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare(JID) ->
    LBare = jid:to_bare(jid:to_lower(JID)),
    {ok, mnesia:match_object(#pubsub_state{stateid = {LBare, '_'}, _ = '_'})}.

-spec get_states_by_bare_and_full(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare_and_full(JID) ->
    LJID = jid:to_lower(JID),
    LBare = jid:to_bare(LJID),
    {ok, mnesia:match_object(#pubsub_state{stateid = {LJID, '_'}, _ = '_'})
     ++ mnesia:match_object(#pubsub_state{stateid = {LBare, '_'}, _ = '_'})}.

-spec get_own_nodes_states(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_own_nodes_states(JID) ->
    LBare = jid:to_bare(jid:to_lower(JID)),
    MyStates = mnesia:match_object(#pubsub_state{stateid = {LBare, '_'},
                                                 affiliation = owner, _ = '_'}),
    NodeIdxs = [Nidx || #pubsub_state{stateid = {_, Nidx}} <- MyStates],
    OwnNodesStates =
    mnesia:foldl(fun (#pubsub_state{stateid = {_, Nidx}} = PubSubState, Acc) ->
                         case lists:member(Nidx, NodeIdxs) of
                             true -> [PubSubState | Acc];
                             false -> Acc
                         end
                 end,
                 [], pubsub_state),
    {ok, OwnNodesStates}.

-spec del_state(Nidx :: mod_pubsub:nodeIdx(),
                JIDorLJID :: jid:ljid() | jid:jid()) -> ok.
del_state(Nidx, #jid{} = JID) ->
    del_state(Nidx, jid:to_lower(JID));
del_state(Nidx, LJID) ->
    mnesia:delete({pubsub_state, {LJID, Nidx}}).

%% ------------------------ Affiliations ------------------------

-spec set_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      JID :: jid:jid(),
                      Affiliation :: mod_pubsub:affiliation()) -> ok.
set_affiliation(Nidx, JID, Affiliation) ->
    BareJID = jid:to_bare(JID),
    {ok, State} = get_state(Nidx, BareJID, write),
    case {Affiliation, State#pubsub_state.subscriptions} of
        {none, []} -> del_state(Nidx, BareJID);
        _ ->  mnesia:write(State#pubsub_state{ affiliation = Affiliation })
    end.

-spec get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      JID :: jid:jid()) ->
    {ok, mod_pubsub:affiliation()}.
get_affiliation(Nidx, JID) ->
    {ok, State} = get_state(Nidx, jid:to_bare(JID), read),
    {ok, State#pubsub_state.affiliation}.

%% ------------------------ Subscriptions ------------------------

-spec add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                       JID :: jid:jid(),
                       Sub :: mod_pubsub:subscription(),
                       SubId :: mod_pubsub:subId()) -> ok.
add_subscription(Nidx, JID, Sub, SubId) ->
    {ok, State} = get_state(Nidx, JID, write),
    NSubscriptions = [{Sub, SubId} | State#pubsub_state.subscriptions ],
    mnesia:write(State#pubsub_state{ subscriptions = NSubscriptions }).

-spec get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:jid(), Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_subscriptions(Nidx) ->
    {ok, States} = get_states(Nidx),
    {ok, states_to_subscriptions(States)}.

-spec get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                    JID :: jid:jid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_entity_subscriptions(Nidx, JID) ->
    {ok, State} = get_state(Nidx, JID, read),
    {ok, State#pubsub_state.subscriptions}.

-spec delete_subscription(
        Nidx :: mod_pubsub:nodeIdx(),
        JID :: jid:jid(),
        SubId :: mod_pubsub:subId()) ->
    ok.
delete_subscription(Nidx, JID, SubId) ->
    {ok, State} = get_state(Nidx, JID, write),
    NewSubs = lists:keydelete(SubId, 2, State#pubsub_state.subscriptions),
    case {State#pubsub_state.affiliation, NewSubs} of
        {none, []} -> del_state(Nidx, JID);
        _ -> mnesia:write(State#pubsub_state{subscriptions = NewSubs})
    end.

-spec delete_all_subscriptions(
        Nidx :: mod_pubsub:nodeIdx(),
        JID :: jid:jid()) ->
    ok.
delete_all_subscriptions(Nidx, JID) ->
    {ok, State} = get_state(Nidx, JID, write),
    case State#pubsub_state.affiliation of
        none -> del_state(Nidx, JID);
        _ -> mnesia:write(State#pubsub_state{subscriptions = []})
    end.

-spec update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          JID :: jid:jid(),
                          Subscription :: mod_pubsub:subscription(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
update_subscription(Nidx, JID, Subscription, SubId) ->
    {ok, State} = get_state(Nidx, JID, write),
    NewSubs = lists:keyreplace(SubId, 2, State#pubsub_state.subscriptions, {Subscription, SubId}),
    mnesia:write(State#pubsub_state{ subscriptions = NewSubs }).

%% ------------------------ Items ------------------------

-spec remove_items(Nidx :: mod_pubsub:nodeIdx(),
                   JID :: jid:jid(),
                   ItemIds :: [mod_pubsub:itemId()]) ->
    ok.
remove_items(Nidx, JID, ItemIds) ->
    {ok, State} = get_state(Nidx, jid:to_bare(JID), write),
    NewItems = State#pubsub_state.items -- ItemIds,
    mnesia:write(State#pubsub_state{ items = NewItems }).

-spec remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.
remove_all_items(Nidx) ->
    {ok, States} = get_states(Nidx),
    lists:foreach(fun(#pubsub_state{ items = [] }) ->
                          ok;
                     (#pubsub_state{} = S) ->
                          mnesia:write(S#pubsub_state{ items = [] })
                  end, States).

-spec add_item(Nidx :: mod_pubsub:nodeIdx(),
               JID :: jid:jid(),
               ItemId :: mod_pubsub:pubsubItem()) ->
    ok.
add_item(Nidx, JID, #pubsub_item{itemid = {ItemId, _}} = Item) ->
    set_item(Item),
    {ok, State} = get_state(Nidx, jid:to_bare(JID), write),
    NewItems = [ItemId | State#pubsub_state.items],
    mnesia:write(State#pubsub_state{ items = NewItems }).

%% ------------------------ Direct #pubsub_item access ------------------------

-spec get_items(Nidx :: mod_pubsub:nodeIdx(), gen_pubsub_node:get_item_options()) ->
    {ok, {[mod_pubsub:pubsubItem()], none}}.
get_items(Nidx, Opts) ->
    Items = mnesia:match_object(#pubsub_item{itemid = {'_', Nidx}, _ = '_'}),
    Sorted = lists:reverse(lists:keysort(#pubsub_item.modification, Items)),
    ItemsLimitedByIds = filter_items_by_item_ids(Sorted, maps:get(item_ids, Opts, undefined)),
    ItemsLimitedByMaxItems = limit_items(ItemsLimitedByIds, maps:get(max_items, Opts, undefined)),
    {ok, {ItemsLimitedByMaxItems, none}}.

filter_items_by_item_ids(Items, undefined) ->
    Items;
filter_items_by_item_ids(Items, ItemIds) ->
    lists:filter(fun (#pubsub_item{itemid = {ItemId, _}}) ->
                         lists:member(ItemId, ItemIds)
                 end, Items).

limit_items(Items, undefined) ->
    Items;
limit_items(Items, MaxItems) ->
    lists:sublist(Items, MaxItems).

-spec get_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) ->
    {ok, mod_pubsub:pubsubItem()} | {error, item_not_found}.
get_item(Nidx, ItemId) ->
    case mnesia:read({pubsub_item, {ItemId, Nidx}}) of
        [Item] when is_record(Item, pubsub_item) -> {ok, Item};
        _ -> {error, item_not_found}
    end.

-spec set_item(Item :: mod_pubsub:pubsubItem()) -> ok | abort.
set_item(Item) ->
    mnesia:write(Item).

-spec del_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) -> ok.
del_item(Nidx, ItemId) ->
    mnesia:delete({pubsub_item, {ItemId, Nidx}}).

-spec del_items(Nidx :: mod_pubsub:nodeIdx(), [ItemId :: mod_pubsub:itemId()]) -> ok.
del_items(Nidx, ItemIds) ->
    lists:foreach(fun (ItemId) -> del_item(Nidx, ItemId) end,
                  ItemIds).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                JID :: jid:jid(),
                LockKind :: write | read) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, JID, LockKind) ->
    StateId = {jid:to_lower(JID), Nidx},
    case catch mnesia:read(pubsub_state, StateId, LockKind) of
        [#pubsub_state{} = State] -> {ok, State};
        _ -> {ok, #pubsub_state{stateid = StateId}}
    end.

-spec states_to_subscriptions([mod_pubsub:pubsubState()]) ->
    [{jid:ljid(), mod_pubsub:subscription(), mod_pubsub:subId()}].
states_to_subscriptions([]) ->
    [];
states_to_subscriptions([#pubsub_state{ subscriptions = [] } | RStates]) ->
    states_to_subscriptions(RStates);
states_to_subscriptions([#pubsub_state{ stateid = {J, _}, subscriptions = Subs } | RStates]) ->
    add_jid_to_subs(Subs, J, RStates).

-spec add_jid_to_subs(Subs :: [{mod_pubsub:subscription(), mod_pubsub:subId()}],
                      LJID :: jid:ljid(),
                      RStates :: [mod_pubsub:pubsubState()]) ->
    [{jid:ljid(), mod_pubsub:subscription(), mod_pubsub:subId()}].
add_jid_to_subs([], _J, RStates) ->
    states_to_subscriptions(RStates);
add_jid_to_subs([{S, SubId} | RSubs], J, RStates) ->
    [ {J, S, SubId} | add_jid_to_subs(RSubs, J, RStates) ].

