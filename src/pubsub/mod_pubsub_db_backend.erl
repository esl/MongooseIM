-module(mod_pubsub_db_backend).

-behaviour(mod_pubsub_db).

-export([init/2, stop/0]).
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

-define(MAIN_MODULE, mod_pubsub_db).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

init(HostType, Opts) ->
    TrackedFuns = [create_node, del_node, get_state, get_states,
                   get_states_by_lus, get_states_by_bare,
                   get_items, get_item, set_item, add_item,
                   del_item, del_items,
                   set_node, find_node_by_id, find_nodes_by_key,
                   find_node_by_name, delete_node, get_subnodes,
                   get_subnodes_tree, get_parentnodes_tree],
    mongoose_backend:init(global, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec stop() -> ok.
stop() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).


%% ------------------------ Fun execution ------------------------

transaction(Fun, ErrorDebug) ->
    Args = [Fun, ErrorDebug],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

dirty(Fun, ErrorDebug) ->
    Args = [Fun, ErrorDebug],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% ------------------------ Direct #pubsub_state access ------------------------
get_state(Nidx, LJID) ->
    Args = [Nidx, LJID],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_states(Nidx) ->
    Args = [Nidx],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_states_by_lus(LUS) ->
    Args = [LUS],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_states_by_bare(LUS) ->
    Args = [LUS],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_states_by_bare_and_full(LJID) ->
    Args = [LJID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_idxs_of_own_nodes_with_pending_subs(LJID) ->
    Args = [LJID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% ------------------------ Direct #pubsub_item access ------------------------

get_items(Nidx, Opts) ->
    Args = [Nidx, Opts],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_item(Nidx, ItemId) ->
    Args = [Nidx, ItemId],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

set_item(Item) ->
    Args = [Item],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

del_item(Nidx, ItemId) ->
    Args = [Nidx, ItemId],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

del_items(Nidx, ItemIds) ->
    Args = [Nidx, ItemIds],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

% ------------------- Node management --------------------------------

create_node(Nidx, LJID) ->
    Args = [Nidx, LJID],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

del_node(Nidx) ->
    Args = [Nidx],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

set_node(Node) ->
    Args = [Node],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

find_node_by_id(Nidx) ->
    Args = [Nidx],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

find_node_by_name(Key, Node) ->
    Args = [Key, Node],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

find_nodes_by_key(Key) ->
    Args = [Key],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

delete_node(Node) ->
    Args = [Node],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_subnodes(Key, Node) ->
    Args = [Key, Node],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_parentnodes_tree(Key, Node) ->
    Args = [Key, Node],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_subnodes_tree(Key, Node) ->
    Args = [Key, Node],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

% ------------------- Affiliations --------------------------------

set_affiliation(Nidx, LJID, Affiliation) ->
    Args = [Nidx, LJID, Affiliation],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_affiliation(Nidx, LJID) ->
    Args = [Nidx, LJID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

% ------------------- Subscriptions --------------------------------

add_subscription(Nidx, LJID, Sub, SubId, SubOpts) ->
    Args = [Nidx, LJID, Sub, SubId, SubOpts],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

set_subscription_opts(Nidx, LJID, SubId, Opts) ->
    Args = [Nidx, LJID, SubId, Opts],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_node_subscriptions(Nidx) ->
    Args = [Nidx],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_node_entity_subscriptions(Nidx, LJID) ->
    Args = [Nidx, LJID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

delete_subscription(Nidx, LJID, SubId) ->
    Args = [Nidx, LJID, SubId],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

delete_all_subscriptions(Nidx, LJID) ->
    Args = [Nidx, LJID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

update_subscription(Nidx, LJID, Subscription, SubId) ->
    Args = [Nidx, LJID, Subscription, SubId],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

% ------------------- Items --------------------------------

add_item(Nidx, LJID, Item) ->
    Args = [Nidx, LJID, Item],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

remove_items(Nidx, LJID, ItemIds) ->
    Args = [Nidx, LJID, ItemIds],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

remove_all_items(Nidx) ->
    Args = [Nidx],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

% ------------------- GDPR-related --------------------------------

get_user_payloads(LUser, LServer) ->
    Args = [LUser, LServer],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_user_nodes(LUser, LServer) ->
    Args = [LUser, LServer],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_user_subscriptions(LUser, LServer) ->
    Args = [LUser, LServer],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

delete_user_subscriptions(LJID) ->
    Args = [LJID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

find_nodes_by_affiliated_user(LJID) ->
    Args = [LJID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
