-module(mod_pubsub_backend).

-include("mod_pubsub.hrl").

-export([start/2, stop/1, set_node/2, get_node/2, get_nodes/2, delete_node/2, delete_nodes/2,
         set_subscription/2, delete_subscription/3, get_subscriptions/2, get_subscriptions/3,
         set_item/2, get_last_item/2, get_last_items/2]).

-callback start(mongooseim:host_type()) -> ok.
-callback stop(mongooseim:host_type()) -> ok.
-callback set_node(mongooseim:host_type(), mod_pubsub:pubsub_node()) -> ok.
-callback get_node(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:pubsub_node() | undefined.
-callback get_nodes(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:node_key()].
-callback delete_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
-callback delete_nodes(mongooseim:host_type(), jid:jid()) -> ok.
-callback set_subscription(mongooseim:host_type(), mod_pubsub:subscription()) -> ok.
-callback delete_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) -> ok.
-callback get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key()) ->
    [mod_pubsub:subscription()].
-callback get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    [mod_pubsub:subscription()].
-callback set_item(mongooseim:host_type(), mod_pubsub:item()) -> ok.
-callback get_last_item(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:item() | undefined.
-callback get_last_items(mongooseim:host_type(), jid:jid()) ->
    [mod_pubsub:item()].

%% API: start/stop

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MODULE, tracked_funs(), Opts),
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType]).

%% API: nodes

-spec set_node(mongooseim:host_type(), mod_pubsub:pubsub_node()) -> ok.
set_node(HostType, Node) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Node]).

-spec get_node(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:pubsub_node() | undefined.
get_node(HostType, NodeKey) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

-spec get_nodes(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:node_key()].
get_nodes(HostType, ServiceJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, ServiceJid]).

-spec delete_nodes(mongooseim:host_type(), jid:jid()) -> ok.
delete_nodes(HostType, ServiceJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, ServiceJid]).

-spec delete_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
delete_node(HostType, NodeKey) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

%% API: items

-spec set_subscription(mongooseim:host_type(), mod_pubsub:subscription()) -> ok.
set_subscription(HostType, Subscription) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Subscription]).

-spec get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key()) ->
    [mod_pubsub:subscription()].
get_subscriptions(HostType, NodeKey) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

-spec delete_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) -> ok.
delete_subscription(HostType, NodeKey, SubscriberJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey, SubscriberJid]).

-spec get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    [mod_pubsub:subscription()].
get_subscriptions(HostType, NodeKey, SubscriberJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey, SubscriberJid]).

set_item(HostType, Item) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Item]).

-spec get_last_item(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:item() | undefined.
get_last_item(HostType, NodeKey) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

-spec get_last_items(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:item()].
get_last_items(HostType, ServiceJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, ServiceJid]).

%% Helpers

tracked_funs() ->
    [set_node, delete_node, delete_nodes, set_subscription, delete_subscription, set_item].
