-module(mod_pubsub_backend).

-include("mod_pubsub.hrl").

-export([start/2, stop/1, set_node/2, get_node/2, get_nodes/2, delete_node/2,
         remove_user/2, remove_domain/2,
         set_subscription/2, delete_subscription/3, get_subscriptions/2,
         get_user_subscriptions/2, get_subscription/3,
         set_item/2, delete_item/3, get_items/3, get_user_items/2,
         get_last_item/2, get_last_items/2]).

-callback start(mongooseim:host_type()) -> ok.
-callback stop(mongooseim:host_type()) -> ok.
-callback set_node(mongooseim:host_type(), mod_pubsub:pubsub_node()) -> ok.
-callback get_node(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:pubsub_node() | undefined.
-callback get_nodes(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:pubsub_node()].
-callback delete_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
-callback remove_user(mongooseim:host_type(), jid:jid()) -> ok.
-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
-callback set_subscription(mongooseim:host_type(), mod_pubsub:subscription()) -> ok.
-callback delete_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    ok | not_found.
-callback get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key()) ->
    [mod_pubsub:subscription()].
-callback get_user_subscriptions(mongooseim:host_type(), jid:jid()) ->
    [mod_pubsub:subscription()].
-callback get_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    mod_pubsub:subscription() | undefined.
-callback set_item(mongooseim:host_type(), mod_pubsub:item()) -> ok.
-callback delete_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id()) ->
    ok | not_found.
-callback get_items(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:get_items_opts()) ->
    [mod_pubsub:item()].
-callback get_user_items(mongooseim:host_type(), jid:jid()) ->
    [mod_pubsub:item()].
-callback get_last_item(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:item() | undefined.
-callback get_last_items(mongooseim:host_type(), jid:jid()) ->
    [mod_pubsub:item()].

%% Currently unused, but implemented for the sake of completeness, and for debugging/inspection
-ignore_xref([get_last_item/2, get_subscription/3]).

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
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Node]).

-spec get_node(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:pubsub_node() | undefined.
get_node(HostType, NodeKey) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

-spec get_nodes(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:pubsub_node()].
get_nodes(HostType, ServiceJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, ServiceJid]).

-spec delete_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
delete_node(HostType, NodeKey) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

-spec remove_user(mongooseim:host_type(), jid:jid()) -> ok.
remove_user(HostType, ServiceJid) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, ServiceJid]).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, Domain) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Domain]).

%% API: items

-spec set_subscription(mongooseim:host_type(), mod_pubsub:subscription()) -> ok.
set_subscription(HostType, Subscription) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Subscription]).

-spec get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key()) ->
    [mod_pubsub:subscription()].
get_subscriptions(HostType, NodeKey) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

-spec get_user_subscriptions(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:subscription()].
get_user_subscriptions(HostType, Jid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

-spec delete_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
          ok | not_found.
delete_subscription(HostType, NodeKey, SubscriberJid) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME,
                                  [HostType, NodeKey, SubscriberJid]).

-spec get_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    mod_pubsub:subscription() | undefined.
get_subscription(HostType, NodeKey, SubscriberJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey, SubscriberJid]).

set_item(HostType, Item) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Item]).

-spec delete_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id()) ->
          ok | not_found.
delete_item(HostType, NodeKey, ItemId) ->
    mongoose_backend:call_tracked(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey, ItemId]).

-spec get_items(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:get_items_opts()) ->
    [mod_pubsub:item()].
get_items(HostType, NodeKey, Opts) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey, Opts]).

-spec get_user_items(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:item()].
get_user_items(HostType, Jid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, Jid]).

-spec get_last_item(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:item() | undefined.
get_last_item(HostType, NodeKey) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, NodeKey]).

-spec get_last_items(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:item()].
get_last_items(HostType, ServiceJid) ->
    mongoose_backend:call(HostType, ?MODULE, ?FUNCTION_NAME, [HostType, ServiceJid]).

%% Helpers

tracked_funs() ->
    [set_node, delete_node, remove_user, remove_domain, set_subscription, delete_subscription,
     set_item, delete_item].
