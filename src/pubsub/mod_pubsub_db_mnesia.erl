%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_mnesia.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub Mnesia backend
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_mnesia).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(mod_pubsub_db).

-include("pubsub.hrl").
-include("jlib.hrl").
-include("mongoose_logger.hrl").
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

% Whole Items
-export([
         get_items/2,
         get_item/2,
         set_item/1,
         del_item/2,
         del_items/2
        ]).

% GDPR
-export([
         get_user_payloads/2,
         get_user_nodes/2,
         get_user_subscriptions/2,
         delete_user_subscriptions/1,
         find_nodes_by_affiliated_user/1
        ]).

%%====================================================================
%% Internal records definitions
%%====================================================================

-record(pubsub_subscription, {
          subid :: mod_pubsub:subId(),
          options = [] :: mod_pubsub:subOptions()
         }).

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
    mnesia:create_table(pubsub_subscription,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, pubsub_subscription)},
                         {type, set}]),
    mnesia:add_table_copy(pubsub_subscription, node(), disc_copies),
    CreateSubnodeTableResult = mnesia:create_table(pubsub_subnode,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, pubsub_subnode)},
                         {type, bag}]),
    mnesia:add_table_copy(pubsub_subnode, node(), disc_copies),
    maybe_fill_subnode_table(CreateSubnodeTableResult),
    pubsub_index:init(),
    ok.

-spec stop() -> ok.
stop() ->
    ok.

%% If pubsub_subnode table was missing, than fill it with data
maybe_fill_subnode_table({atomic, ok}) ->
    F = fun(#pubsub_node{} = Node, Acc) ->
                set_subnodes(Node, []),
                Acc
        end,
    mnesia:transaction(fun() -> mnesia:foldl(F, ok, pubsub_node) end);
maybe_fill_subnode_table(_Other) ->
    ok.

%% ------------------------ Fun execution ------------------------

-spec transaction(Fun :: fun(() -> {result | error, any()}),
                  ErrorDebug :: map()) ->
    {result | error, any()}.
transaction(Fun, ErrorDebug) ->
    transaction(Fun, ErrorDebug, 3).

transaction(Fun, ErrorDebug, Retries) ->
    case mnesia:transaction(mod_pubsub_db:extra_debug_fun(Fun)) of
        {atomic, Result} ->
            Result;
        {aborted, ReasonData} when Retries > 0 ->
            ?WARNING_MSG("event=transaction_retry retries=~p reason=~p debug=~p",
                         [Retries, ReasonData, ErrorDebug]),
            timer:sleep(100),
            transaction(Fun, ErrorDebug, Retries - 1);
        {aborted, ReasonData} ->
            ?WARNING_MSG("event=transaction_failed reason=~p debug=~p",
                         [ReasonData, ErrorDebug]),
            mod_pubsub_db:db_error(ReasonData, ErrorDebug, transaction_failed)
    end.

-spec dirty(Fun :: fun(() -> {result | error, any()}),
            ErrorDebug :: map()) ->
    {result | error, any()}.
dirty(Fun, ErrorDebug) ->
    try mnesia:sync_dirty(mod_pubsub_db:extra_debug_fun(Fun), []) of
        Result ->
            Result
    catch
        _C:ReasonData ->
            mod_pubsub_db:db_error(ReasonData, ErrorDebug, dirty_failed)
    end.

%% ------------------------ GDPR-related ------------------------

-spec get_user_payloads(LUser :: jid:luser(), LServer :: jid:lserver()) ->
     [NodeNameItemIDAndPayload :: [binary()]].
get_user_payloads(LUser, LServer) ->
    {atomic, Recs} = mnesia:transaction(fun() -> get_user_payloads_t(LUser, LServer) end),
    Recs.

get_user_payloads_t(LUser, LServer) ->
    BareUserMatchSpec = {'_', {LUser, LServer, '_'}},
    Items = mnesia:match_object(#pubsub_item{creation = BareUserMatchSpec, _ = '_'}),
    [[node_name(Nidx), ItemId, << <<(exml:to_binary(P))/binary>> || P <- Payload >>] ||
        #pubsub_item{itemid = {ItemId, Nidx}, payload = Payload} <- Items].

-spec get_user_nodes(LUser :: jid:luser(), LServer :: jid:lserver()) ->
     [NodeNameAndType :: [binary()]].
get_user_nodes(LUser, LServer) ->
    LJID = {LUser, LServer, <<>>},
    {atomic, Recs} = mnesia:transaction(fun() ->
        Nodes = mnesia:match_object(#pubsub_node{owners = [LJID], _ = '_'}),
        [[NodeName, Type] || #pubsub_node{nodeid = {_, NodeName}, type = Type} <- Nodes]
      end),
    Recs.

-spec get_user_subscriptions(LUser :: jid:luser(), LServer :: jid:lserver()) ->
     [NodeName :: [binary()]].
get_user_subscriptions(LUser, LServer) ->
    {atomic, Recs} = mnesia:transaction(fun() -> get_user_subscriptions_t(LUser, LServer) end),
    Recs.

get_user_subscriptions_t(LUser, LServer) ->
    UserMatchSpec = {LUser, LServer, '_'},
    SubscriptionStates
    = mnesia:match_object(#pubsub_state{stateid = {UserMatchSpec, '_'},
                                        subscriptions = [{subscribed, '_'}], _ = '_'}),
    [ [node_name(Nidx)] || #pubsub_state{stateid = {_, Nidx}} <- SubscriptionStates].

node_name(Nidx) ->
    case find_node_by_id(Nidx) of
        {ok, #pubsub_node{ nodeid = {_, NodeName} }} -> NodeName;
        _ -> <<>>
    end.

-spec find_nodes_by_affiliated_user(JID :: jid:ljid()) ->
    [{mod_pubsub:pubsubNode(), mod_pubsub:affiliation()}].
find_nodes_by_affiliated_user(LJID) ->
    {ok, States} = get_states_by_lus(LJID),
    lists:map(fun(#pubsub_state{ stateid = {_, Nidx}, affiliation = Aff }) ->
                      {ok, Node} = find_node_by_id(Nidx),
                      {Node, Aff}
              end, States).

%% ------------------------ Direct #pubsub_state access ------------------------

-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, LJID) ->
    get_state(Nidx, LJID, read).

-spec get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states(Nidx) ->
    States = mnesia:match_object(#pubsub_state{stateid = {'_', Nidx}, _ = '_'}),
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
                          del_state_by_idx_and_ljid(Nidx, LJID)
                  end, States),
    {ok, States}.

-spec set_node(mod_pubsub:pubsubNode()) -> {ok, mod_pubsub:nodeIdx()}.
set_node(#pubsub_node{id = undefined} = Node) ->
    CreateNode = Node#pubsub_node{id = pubsub_index:new(node)},
    OldParents = [],
    set_node(CreateNode, OldParents);
set_node(Node) ->
    OldParents = get_parentnodes_names(Node),
    set_node(Node, OldParents).

set_node(#pubsub_node{id = Nidx} = Node, OldParents) ->
    mnesia:write(Node),
    set_subnodes(Node, OldParents),
    {ok, Nidx}.

get_parentnodes_names(#pubsub_node{nodeid = NodeId}) ->
    case mnesia:read(pubsub_node, NodeId, write) of
        [] ->
            [];
        [#pubsub_node{parents = Parents}] ->
            Parents
    end.

set_subnodes(#pubsub_node{parents = Parents}, Parents) ->
    ok;
set_subnodes(#pubsub_node{nodeid = NodeId, parents = NewParents}, OldParents) ->
    set_subnodes(NodeId, NewParents, OldParents).

set_subnodes({Key, Node}, NewParents, OldParents) ->
    OldParentsSet = sets:from_list(OldParents),
    NewParentsSet = sets:from_list(NewParents),
    Deleted = sets:to_list(sets:subtract(OldParentsSet, NewParentsSet)),
    Added   = sets:to_list(sets:subtract(NewParentsSet, OldParentsSet)),
    DeletedObjects = names_to_subnode_records(Key, Node, Deleted),
    AddedObjects   = names_to_subnode_records(Key, Node, Added),
    lists:foreach(fun(Object) -> mnesia:delete_object(Object) end, DeletedObjects),
    lists:foreach(fun(Object) -> mnesia:write(Object) end, AddedObjects),
    ok.

names_to_subnode_records(Key, Node, Parents) ->
    [#pubsub_subnode{nodeid = {Key, Parent}, subnode = Node} || Parent <- Parents].

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

oid(Key, Name) -> {Key, Name}.


-spec delete_node(Node :: mod_pubsub:pubsubNode()) -> ok.
delete_node(#pubsub_node{nodeid = NodeId, parents = Parents}) ->
    set_subnodes(NodeId, [], Parents),
    mnesia:delete({pubsub_node, NodeId}).


-spec get_subnodes(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId() | <<>>) ->
    [mod_pubsub:pubsubNode()].
get_subnodes(Key, <<>>) ->
    get_nodes_without_parents(Key);
get_subnodes(Key, Node) ->
    Subnodes = get_subnodes_names(Key, Node),
    find_nodes_by_names(Key, Subnodes).

%% Like get_subnodes, but returns node names instead of node records.
get_subnodes_names(Key, Node) ->
    MnesiaRecords = mnesia:read({pubsub_subnode, {Key, Node}}),
    [Subnode || #pubsub_subnode{subnode = Subnode} <- MnesiaRecords].

%% Warning: this function is full table scan and can return a lot of records
get_nodes_without_parents(Key) ->
    mnesia:match_object(#pubsub_node{nodeid = {Key, '_'}, parents = [], _ = '_'}).

%% Return a list of {Depth, Nodes} where Nodes are parent nodes of nodes of the lower level.
%% So, we start with {0, [NodeRecord]}, where NodeRecord is node that corresponds to Node argument.
%% The next level is {1, Nodes1}, where Nodes1 are parent nodes of Node.
%% The next level can be {2, Nodes2}, where Nodes2 are parents of Nodes1 (without duplicates).
%%
%% Each node can be returned only ones by the function.
-spec get_parentnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
get_parentnodes_tree(Key, Node) ->
    case find_node_by_name(Key, Node) of
        false ->
            [ {0, []} ]; %% node not found case

        #pubsub_node{parents = []} = Record ->
            [ {0, [Record]} ];

        #pubsub_node{parents = Parents} = Record ->
            Depth = 1,
            %% To avoid accidental cyclic issues, let's maintain the list of known nodes
            %% which we don't expand again
            KnownNodesSet = sets:from_list([Node]),
            extract_parents(Key, Node, Parents, Depth, KnownNodesSet) ++ [ {0, [Record]} ]
    end.

%% Each call extract Parents on the level and recurse to the next level.
%% KnownNodesSet are nodes to be filtered out.
extract_parents(Key, InitialNode, Parents, Depth, KnownNodesSet) ->
    ParentRecords = find_nodes_by_names(Key, Parents),
    KnownNodesSet1 = sets:union(KnownNodesSet, sets:from_list(Parents)),
    %% Names of parents of parents
    PPNames = lists:usort(lists:flatmap(fun(#pubsub_node{parents = PP}) -> PP end, ParentRecords)),
    CyclicNames = [Name || Name <- PPNames, sets:is_element(Name, KnownNodesSet1)],
    case CyclicNames of
        [] -> [];
        _ -> ?WARNING_MSG("event=cyclic_nodes_detected node=~p cyclic_names=~p", [InitialNode, CyclicNames])
    end,
    %% PPNames is ordset, so we don't need to worry about having duplicates in it.
    %% CyclicNames is usually an empty list.
    PPNamesToGet = PPNames -- CyclicNames,
    case PPNamesToGet of
        [] -> [];
        _ -> extract_parents(Key, InitialNode, PPNamesToGet, Depth + 1, KnownNodesSet1)
    end ++ [ {Depth, ParentRecords} ].

find_nodes_by_names(Key, Nodes) ->
    %% Contains false for missing nodes
    MaybeRecords = [find_node_by_name(Key, Node) || Node <- Nodes],
    %% Filter out false-s
    [Record || Record = #pubsub_node{} <- MaybeRecords].


-spec get_subnodes_tree(Key :: mod_pubsub:hostPubsub() | jid:ljid(), Node :: mod_pubsub:nodeId()) ->
    [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(), ...]}].
get_subnodes_tree(Key, Node) ->
    case find_node_by_name(Key, Node) of
        false ->
            [ {1, []}, {0, [false]} ]; %% node not found case

        #pubsub_node{} = Record ->
            Subnodes = get_subnodes_names(Key, Node),
            Depth = 1,
            %% To avoid accidental cyclic issues, let's maintain the list of known nodes
            %% which we don't expand again
            KnownNodesSet = sets:from_list([Node]),
            extract_subnodes(Key, Node, Subnodes, Depth, KnownNodesSet) ++ [ {0, [Record]} ]
    end.

%% Each call extract Subnodes on the level and recurse to the next level.
%% KnownNodesSet are nodes to be filtered out.
extract_subnodes(Key, InitialNode, Subnodes, Depth, KnownNodesSet) ->
    SubnodesRecords = find_nodes_by_names(Key, Subnodes),
    KnownNodesSet1 = sets:union(KnownNodesSet, sets:from_list(Subnodes)),
    %% Names of subnodes of subnodes
    SSNames = lists:usort(lists:flatmap(fun(Subnode) -> get_subnodes_names(Key, Subnode) end, Subnodes)),
    CyclicNames = [Name || Name <- SSNames, sets:is_element(Name, KnownNodesSet1)],
    case CyclicNames of
        [] -> [];
        _ -> ?WARNING_MSG("event=cyclic_nodes_detected node=~p cyclic_names=~p", [InitialNode, CyclicNames])
    end,
    SSNamesToGet = SSNames -- CyclicNames,
    case SSNamesToGet of
        [] -> [ {Depth + 1, []} ];
        _ -> extract_subnodes(Key, InitialNode, SSNamesToGet, Depth + 1, KnownNodesSet1)
    end ++ [ {Depth, SubnodesRecords} ].

%% ------------------------ Affiliations ------------------------

-spec set_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                      LJID :: jid:ljid(),
                      Affiliation :: mod_pubsub:affiliation()) -> ok.
set_affiliation(Nidx, LJID, Affiliation) ->
    BareLJID = jid:to_bare(LJID),
    {ok, State} = get_state(Nidx, BareLJID, write),
    case {Affiliation, State#pubsub_state.subscriptions} of
        {none, []} -> del_state_by_idx_and_ljid(Nidx, BareLJID);
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
                       SubId :: mod_pubsub:subId(),
                       SubOpts :: mod_pubsub:subOptions()) -> ok.
add_subscription(Nidx, LJID, Sub, SubId, SubOpts) ->
    {ok, State} = get_state(Nidx, LJID, write),
    NSubscriptions = [{Sub, SubId} | State#pubsub_state.subscriptions ],
    mnesia:write(State#pubsub_state{ subscriptions = NSubscriptions }),
    set_subscription_opts(Nidx, LJID, SubId, SubOpts).

-spec set_subscription_opts(Nidx :: mod_pubsub:nodeIdx(),
                            JID :: jid:ljid(),
                            SubId :: mod_pubsub:subId(),
                            Opts :: mod_pubsub:subOptions()) -> ok.
set_subscription_opts(_Nidx, _JID, SubId, Opts) ->
    mnesia:write(#pubsub_subscription{ subid = SubId, options = Opts }).

-spec get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(),
           Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           Opts :: mod_pubsub:subOptions()}]}.
get_node_subscriptions(Nidx) ->
    {ok, States} = get_states(Nidx),
    {ok, states_to_subscriptions(States)}.

-spec get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                    LJID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(),
           SubId :: mod_pubsub:subId(),
           Opts :: mod_pubsub:subOptions()}]}.
get_node_entity_subscriptions(Nidx, LJID) ->
    {ok, State} = get_state(Nidx, LJID, read),
    {ok, add_opts_to_subs(State#pubsub_state.subscriptions)}.

-spec delete_subscription(
        Nidx :: mod_pubsub:nodeIdx(),
        LJID :: jid:ljid(),
        SubId :: mod_pubsub:subId()) ->
    ok.
delete_subscription(Nidx, LJID, SubId) ->
    {ok, State} = get_state(Nidx, LJID, write),
    NewSubs = lists:keydelete(SubId, 2, State#pubsub_state.subscriptions),
    mnesia:delete({pubsub_subscription, SubId}),
    case {State#pubsub_state.affiliation, NewSubs} of
        {none, []} -> del_state_by_idx_and_ljid(Nidx, LJID);
        _ -> mnesia:write(State#pubsub_state{subscriptions = NewSubs})
    end.

-spec delete_all_subscriptions(
        Nidx :: mod_pubsub:nodeIdx(),
        LJID :: jid:ljid()) ->
    ok.
delete_all_subscriptions(Nidx, LJID) ->
    {ok, State} = get_state(Nidx, LJID, write),
    delete_all_subscriptions_by_state(State).

-spec delete_user_subscriptions(jid:ljid()) -> ok.
delete_user_subscriptions(LJID) ->
    {ok, States} = get_states_by_lus(LJID),
    lists:foreach(fun delete_all_subscriptions_by_state/1, States).

-spec delete_all_subscriptions_by_state(mod_pubsub:pubsubState()) -> ok.
delete_all_subscriptions_by_state(State) ->
    lists:foreach(fun({_, SubId}) -> mnesia:delete({pubsub_subscription, SubId}) end,
                  State#pubsub_state.subscriptions),
    case State#pubsub_state.affiliation of
        none -> del_state(State);
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

-spec set_item(Item :: mod_pubsub:pubsubItem()) -> ok.
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

-spec del_state_by_idx_and_ljid(Nidx :: mod_pubsub:nodeIdx(),
                                LJID :: jid:ljid()) -> ok.
del_state_by_idx_and_ljid(Nidx, LJID) ->
    {ok, State} = get_state(Nidx, LJID, write),
    del_state(State).

-spec del_state(mod_pubsub:pubsubState()) -> ok.
del_state(#pubsub_state{ stateid = {LJID, Nidx}, subscriptions = Subs }) ->
    lists:foreach(fun({_, SubId}) -> mnesia:delete({pubsub_subscription, SubId}) end, Subs),
    mnesia:delete({pubsub_state, {LJID, Nidx}}).

-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                LJID :: jid:ljid(),
                LockKind :: write | read) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, LJID, LockKind) ->
    StateId = {LJID, Nidx},
    case mnesia:read(pubsub_state, StateId, LockKind) of
        [#pubsub_state{} = State] -> {ok, State};
        _ -> {ok, #pubsub_state{stateid = StateId}}
    end.

-spec states_to_subscriptions([mod_pubsub:pubsubState()]) ->
    [{jid:ljid(), mod_pubsub:subscription(), mod_pubsub:subId(), mod_pubsub:subOptions()}].
states_to_subscriptions([]) ->
    [];
states_to_subscriptions([#pubsub_state{ subscriptions = [] } | RStates]) ->
    states_to_subscriptions(RStates);
states_to_subscriptions([#pubsub_state{ stateid = {J, _}, subscriptions = Subs } | RStates]) ->
    add_jid_and_opts_to_subs(Subs, J, RStates).

-spec add_jid_and_opts_to_subs(Subs :: [{mod_pubsub:subscription(), mod_pubsub:subId()}],
                               LJID :: jid:ljid(),
                               RStates :: [mod_pubsub:pubsubState()]) ->
    [{jid:ljid(), mod_pubsub:subscription(), mod_pubsub:subId(), mod_pubsub:subOptions()}].
add_jid_and_opts_to_subs([], _J, RStates) ->
    states_to_subscriptions(RStates);
add_jid_and_opts_to_subs([{S, SubId} | RSubs], J, RStates) ->
    Opts = read_sub_options(SubId),
    [ {J, S, SubId, Opts} | add_jid_and_opts_to_subs(RSubs, J, RStates) ].

-spec add_opts_to_subs(Subs :: [{mod_pubsub:subscription(), mod_pubsub:subId()}]) ->
    [{mod_pubsub:subscription(), mod_pubsub:subId(), mod_pubsub:subOptions()}].
add_opts_to_subs([]) ->
    [];
add_opts_to_subs([{S, SubId} | RSubs]) ->
    Opts = read_sub_options(SubId),
    [ {S, SubId, Opts} | add_opts_to_subs(RSubs) ].

-spec read_sub_options(SubId :: mod_pubsub:subId()) -> mod_pubsub:subOptions().
read_sub_options(SubId) ->
    case mnesia:read({pubsub_subscription, SubId}) of
        [] -> [];
        [#pubsub_subscription{ options = Opts0 }] -> Opts0
    end.

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
is_pending_sub(_) -> false.

