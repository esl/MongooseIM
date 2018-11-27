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
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, stop/0]).
% Funs execution
-export([transaction/2, dirty/2]).
% Direct #pubsub_state access
-export([del_node/1, get_state/2,
         get_states/1, get_states_by_lus/1, get_states_by_bare/1,
         get_states_by_bare_and_full/1, get_idxs_of_own_nodes_with_pending_subs/1]).
% Node management
-export([
         create_node/2,
         set_node/1,
         find_node_by_id/1,
         find_nodes_by_key/1,
         find_node_by_name/2,
         delete_node/2,
         get_subnodes/2,
         get_parentnodes/2,
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
    mnesia:create_table(pubsub_node,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, pubsub_node)}]),
    mnesia:add_table_index(pubsub_node, id),
    ok.

-spec stop() -> ok.
stop() ->
    ok.

%% ------------------------ Fun execution ------------------------

-spec transaction(Fun :: fun(() -> {result | error, any()}),
                  ErrorDebug :: map()) ->
    {result | error, any()}.
transaction(Fun, ErrorDebug) ->
    case mnesia:transaction(extra_debug_fun(Fun)) of
        {atomic, Result} ->
            Result;
        {aborted, ReasonData} ->
            mod_pubsub_db:db_error(ReasonData, ErrorDebug, transaction_failed)
    end.

-spec dirty(Fun :: fun(() -> {result | error, any()}),
            ErrorDebug :: map()) ->
    {result | error, any()}.
dirty(Fun, ErrorDebug) ->
    try mnesia:sync_dirty(extra_debug_fun(Fun), []) of
        Result ->
            Result
    catch
        _C:ReasonData ->
            mod_pubsub_db:db_error(ReasonData, ErrorDebug, dirty_failed)
    end.

%% transaction and sync_dirty return very truncated error data so we add extra
%% try to gather stack trace etc.
extra_debug_fun(Fun) ->
    fun() ->
            try Fun() of
                Res -> Res
            catch
                C:R ->
                    throw(#{
                      class => C,
                      reason => R,
                      stacktrace => erlang:get_stacktrace()})
            end
    end.

%% ------------------------ Direct #pubsub_state access ------------------------

-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, LJID) ->
    get_state(Nidx, LJID, read).

-spec get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states(Nidx) ->
    States = case catch mnesia:match_object(
                          #pubsub_state{stateid = {'_', Nidx}, _ = '_'}) of
                 List when is_list(List) -> List;
                 _ -> []
             end,
    {ok, States}.

-spec get_states_by_lus(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_lus({ LUser, LServer, _ }) ->
    {ok, mnesia:match_object(#pubsub_state{stateid = {{LUser, LServer, '_'}, '_'}, _ = '_'})}.

-spec get_states_by_bare(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare(LJID) ->
    LBare = jid:to_bare(LJID),
    {ok, mnesia:match_object(#pubsub_state{stateid = {LBare, '_'}, _ = '_'})}.

-spec get_states_by_bare_and_full(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare_and_full(LJID) ->
    LBare = jid:to_bare(LJID),
    {ok, mnesia:match_object(#pubsub_state{stateid = {LJID, '_'}, _ = '_'})
     ++ mnesia:match_object(#pubsub_state{stateid = {LBare, '_'}, _ = '_'})}.

-spec get_idxs_of_own_nodes_with_pending_subs(LJID :: jid:ljid()) ->
    {ok, [mod_pubsub:nodeIdx()]}.
get_idxs_of_own_nodes_with_pending_subs(LJID) ->
    LBare = jid:to_bare(LJID),
    MyStates = mnesia:match_object(#pubsub_state{stateid = {LBare, '_'},
                                                 affiliation = owner, _ = '_'}),
    NodeIdxs = [Nidx || #pubsub_state{stateid = {_, Nidx}} <- MyStates],
    ResultNidxs = mnesia:foldl(pa:bind(fun get_idxs_with_pending_subs/3, NodeIdxs),
                               [], pubsub_state),
    {ok, ResultNidxs}.

%% ------------------------ Node management ------------------------

-spec create_node(Nidx :: mod_pubsub:nodeIdx(),
                  Owner :: jid:ljid()) ->
    ok.
create_node(Nidx, Owner) ->
    set_affiliation(Nidx, Owner, owner).

-spec del_node(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
del_node(Nidx) ->
    {ok, States} = get_states(Nidx),
    lists:foreach(fun (#pubsub_state{stateid = {LJID, _}, items = Items}) ->
                          del_items(Nidx, Items),
                          del_state(Nidx, LJID)
                  end, States),
    {ok, States}.

-spec set_node(mod_pubsub:pubsubNode()) -> ok.
set_node(Node) when is_record(Node, pubsub_node) ->
    mnesia:write(Node).


-spec find_node_by_id(Nidx :: mod_pubsub:nodeIdx()) ->
    {error, not_found} | {ok, mod_pubsub:pubsubNode()}.
find_node_by_id(Nidx) ->
    case mnesia:index_read(pubsub_node, Nidx, #pubsub_node.id) of
        [#pubsub_node{} = Record] -> {ok, Record};
        [] ->
            {error, not_found}
    end.

-spec find_node_by_name(
        Key :: mod_pubsub:hostPubsub() | jid:ljid(),
        Node :: mod_pubsub:nodeId()) ->
    mod_pubsub:pubsubNode() | false.
find_node_by_name(Key, Node) ->
    case mnesia:read(pubsub_node, oid(Key, Node), read) of
        [] -> false;
        [NodeRec] -> NodeRec
    end.

-spec find_nodes_by_key(Key :: mod_pubsub:hostPubsub() | jid:ljid()) ->
    [mod_pubsub:pubsubNode()].
find_nodes_by_key(Key) ->
    mnesia:match_object(#pubsub_node{nodeid = {Key, '_'}, _ = '_'}).

-spec find_nodes_by_id_and_pred(Key :: mod_pubsub:hostPubsub() | jid:ljid(),
                                Nodes :: [mod_pubsub:nodeId()],
                                Pred :: fun((mod_pubsub:nodeId(), mod_pubsub:pubsubNode()) -> boolean())) ->
    [mod_pubsub:pubsubNode()].
find_nodes_by_id_and_pred(Key, Nodes, Pred) ->
    Q = qlc:q([N
                || #pubsub_node{nodeid = {NHost, _}} = N
                    <- mnesia:table(pubsub_node),
                    Node <- Nodes, Key == NHost, Pred(Node, N)]),
    qlc:e(Q).

oid(Key, Name) -> {Key, Name}.


-spec delete_node(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) -> ok.
delete_node(Key, Node) ->
    mnesia:delete({pubsub_node, oid(Key, Node)}).


-spec get_subnodes(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId() | <<>>) ->
    [mod_pubsub:pubsubNode()].
get_subnodes(Key, <<>>) ->
    Q = qlc:q([N
               || #pubsub_node{nodeid = {NKey, _},
                               parents = []} = N
                  <- mnesia:table(pubsub_node),
                  Key == NKey]),
    qlc:e(Q);
get_subnodes(Key, Node) ->
    Q = qlc:q([N
               || #pubsub_node{nodeid = {NKey, _},
                               parents = Parents} =
                  N
                  <- mnesia:table(pubsub_node),
                    Key == NKey, lists:member(Node, Parents)]),
    qlc:e(Q).

-spec get_parentnodes(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [mod_pubsub:pubsubNode()] | {error, not_found}.
get_parentnodes(Key, Node) ->
    case find_node_by_name(Key, Node) of
        false ->
            {error, not_found};
        #pubsub_node{parents = []} ->
            [];
        #pubsub_node{parents = Parents} ->
            Q = qlc:q([N
                       || #pubsub_node{nodeid = {NHost, NNode}} = N
                          <- mnesia:table(pubsub_node),
                          Parent <- Parents, Key == NHost, Parent == NNode]),
            qlc:e(Q)
    end.

-spec get_parentnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
get_parentnodes_tree(Key, Node) ->
    Pred = fun (NID, #pubsub_node{nodeid = {_, NNode}}) ->
            NID == NNode
    end,
    Tr = fun (#pubsub_node{parents = Parents}) -> Parents
    end,
    traversal_helper(Pred, Tr, Key, [Node]).

-spec get_subnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
get_subnodes_tree(Key, Node) ->
    Pred = fun (NID, #pubsub_node{parents = Parents}) ->
            lists:member(NID, Parents)
    end,
    Tr = fun (#pubsub_node{nodeid = {_, N}}) -> [N] end,
    traversal_helper(Pred, Tr, 1, Key, [Node],
        [{0, [find_node_by_name(Key, Node)]}]).


-spec traversal_helper(
        Pred    :: fun(),
        Transform :: fun(),
        Host    :: mod_pubsub:hostPubsub(),
        Nodes :: [mod_pubsub:nodeId(), ...])
        -> [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
traversal_helper(Pred, Tr, Host, Nodes) ->
    traversal_helper(Pred, Tr, 0, Host, Nodes, []).

traversal_helper(_Pred, _Tr, _Depth, _Host, [], Acc) ->
    Acc;
traversal_helper(Pred, Tr, Depth, Host, Nodes, Acc) ->
    NodeRecs = find_nodes_by_id_and_pred(Host, Nodes, Pred),
    IDs = lists:flatmap(Tr, NodeRecs),
    traversal_helper(Pred, Tr, Depth + 1, Host, IDs, [{Depth, NodeRecs} | Acc]).

%% ------------------------ Affiliations ------------------------

-spec set_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LJID :: jid:ljid(),
                      Affiliation :: mod_pubsub:affiliation()) -> ok.
set_affiliation(Nidx, LJID, Affiliation) ->
    BareLJID = jid:to_bare(LJID),
    {ok, State} = get_state(Nidx, BareLJID, write),
    case {Affiliation, State#pubsub_state.subscriptions} of
        {none, []} -> del_state(Nidx, BareLJID);
        _ ->  mnesia:write(State#pubsub_state{ affiliation = Affiliation })
    end.

-spec get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LJID :: jid:ljid()) ->
    {ok, mod_pubsub:affiliation()}.
get_affiliation(Nidx, LJID) ->
    {ok, State} = get_state(Nidx, jid:to_bare(LJID), read),
    {ok, State#pubsub_state.affiliation}.

%% ------------------------ Subscriptions ------------------------

-spec add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                       LJID :: jid:ljid(),
                       Sub :: mod_pubsub:subscription(),
                       SubId :: mod_pubsub:subId()) -> ok.
add_subscription(Nidx, LJID, Sub, SubId) ->
    {ok, State} = get_state(Nidx, LJID, write),
    NSubscriptions = [{Sub, SubId} | State#pubsub_state.subscriptions ],
    mnesia:write(State#pubsub_state{ subscriptions = NSubscriptions }).

-spec get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(), Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_subscriptions(Nidx) ->
    {ok, States} = get_states(Nidx),
    {ok, states_to_subscriptions(States)}.

-spec get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                    LJID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.
get_node_entity_subscriptions(Nidx, LJID) ->
    {ok, State} = get_state(Nidx, LJID, read),
    {ok, State#pubsub_state.subscriptions}.

-spec delete_subscription(
        Nidx :: mod_pubsub:nodeIdx(),
        LJID :: jid:ljid(),
        SubId :: mod_pubsub:subId()) ->
    ok.
delete_subscription(Nidx, LJID, SubId) ->
    {ok, State} = get_state(Nidx, LJID, write),
    NewSubs = lists:keydelete(SubId, 2, State#pubsub_state.subscriptions),
    case {State#pubsub_state.affiliation, NewSubs} of
        {none, []} -> del_state(Nidx, LJID);
        _ -> mnesia:write(State#pubsub_state{subscriptions = NewSubs})
    end.

-spec delete_all_subscriptions(
        Nidx :: mod_pubsub:nodeIdx(),
        LJID :: jid:ljid()) ->
    ok.
delete_all_subscriptions(Nidx, LJID) ->
    {ok, State} = get_state(Nidx, LJID, write),
    case State#pubsub_state.affiliation of
        none -> del_state(Nidx, LJID);
        _ -> mnesia:write(State#pubsub_state{subscriptions = []})
    end.

-spec update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                          LJID :: jid:ljid(),
                          Subscription :: mod_pubsub:subscription(),
                          SubId :: mod_pubsub:subId()) ->
    ok.
update_subscription(Nidx, LJID, Subscription, SubId) ->
    {ok, State} = get_state(Nidx, LJID, write),
    NewSubs = lists:keyreplace(SubId, 2, State#pubsub_state.subscriptions, {Subscription, SubId}),
    mnesia:write(State#pubsub_state{ subscriptions = NewSubs }).

%% ------------------------ Items ------------------------

-spec remove_items(Nidx :: mod_pubsub:nodeIdx(),
                   LJID :: jid:ljid(),
                   ItemIds :: [mod_pubsub:itemId()]) ->
    ok.
remove_items(Nidx, LJID, ItemIds) ->
    {ok, State} = get_state(Nidx, jid:to_bare(LJID), write),
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
               JID :: jid:ljid(),
               Item :: mod_pubsub:pubsubItem()) ->
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

-spec del_state(Nidx :: mod_pubsub:nodeIdx(),
               LJID :: jid:ljid()) -> ok.
del_state(Nidx, LJID) ->
    mnesia:delete({pubsub_state, {LJID, Nidx}}).

-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid(),
                LockKind :: write | read) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, LJID, LockKind) ->
    StateId = {LJID, Nidx},
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

-spec get_idxs_with_pending_subs(NodeIdxs :: [mod_pubsub:nodeIdx()],
                                 PubsubState :: mod_pubsub:pubsubState(),
                                 Acc :: [mod_pubsub:nodeIdx()]) ->
    [mod_pubsub:nodeIdx()].
get_idxs_with_pending_subs(NodeIdxs,
                           #pubsub_state{stateid = {_, Nidx}, subscriptions = Subs},
                           Acc) ->
    case lists:member(Nidx, NodeIdxs)
         andalso lists:any(fun is_pending_sub/1, Subs) of
        true -> [Nidx | Acc];
        false -> Acc
    end.

is_pending_sub({pending, _}) -> true;
is_pending_sub(pending) -> true;
is_pending_sub(_) -> false.

