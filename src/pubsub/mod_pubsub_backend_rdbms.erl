-module(mod_pubsub_backend_rdbms).
-moduledoc "RDBMS backend for PubSub (XEP-0060)".

-behaviour(mod_pubsub_backend).

-include_lib("exml/include/exml.hrl").
-include("mod_pubsub.hrl").

-export([start/1, stop/1, set_node/2, get_node/2, get_nodes/2, delete_node/2, delete_nodes/2,
         set_subscription/2, delete_subscription/3, get_subscriptions/2, get_subscription/3,
         set_item/2, get_item/3, get_items/2, get_last_item/2, get_last_items/2]).

-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    NodeFields = [~"service_jid", ~"node_id", ~"access_model"],
    SubscriptionFields = [~"service_jid", ~"node_id", ~"subscriber_jid"],
    KeyFields = [~"service_jid", ~"node_id", ~"item_id"],
    UpdateFields = [~"publisher_jid", ~"payload", ~"created_at"],
    rdbms_queries:prepare_upsert(HostType, pubsub_node_upsert, pubsub_node,
                                 NodeFields, [~"access_model"], [~"service_jid", ~"node_id"]),
    mongoose_rdbms:prepare(pubsub_get_node, pubsub_node,
                           [service_jid, node_id], sql(get_node)),
    mongoose_rdbms:prepare(pubsub_delete_node, pubsub_node,
                           [service_jid, node_id], sql(delete_node)),
    mongoose_rdbms:prepare(pubsub_delete_nodes, pubsub_node,
                           [service_jid], sql(delete_nodes)),
    mongoose_rdbms:prepare(pubsub_get_nodes, pubsub_node,
                           [service_jid], sql(get_nodes)),
    rdbms_queries:prepare_upsert(HostType, pubsub_subscription_upsert, pubsub_subscription,
                                 SubscriptionFields, [], SubscriptionFields),
    mongoose_rdbms:prepare(pubsub_delete_subscription, pubsub_subscription,
                           [service_jid, node_id, subscriber_jid],
                           sql(delete_subscription)),
    mongoose_rdbms:prepare(pubsub_get_subscriptions, pubsub_subscription,
                           [service_jid, node_id], sql(get_subscriptions)),
    mongoose_rdbms:prepare(pubsub_get_subscriptions_for_jid, pubsub_subscription,
                           [service_jid, node_id, subscriber_jid],
                           sql(get_subscriptions_for_jid)),
    rdbms_queries:prepare_upsert(HostType, pubsub_item_upsert, pubsub_item,
                                 KeyFields ++ UpdateFields, UpdateFields, KeyFields),
    mongoose_rdbms:prepare(pubsub_get_item, pubsub_item,
                           [service_jid, node_id, item_id], sql(get_item)),
    mongoose_rdbms:prepare(pubsub_get_items, pubsub_item,
                           [service_jid, node_id], sql(get_items)),
    mongoose_rdbms:prepare(pubsub_get_last_item, pubsub_item,
                           [service_jid, node_id], sql(get_last_item)),
    mongoose_rdbms:prepare(pubsub_get_last_items, pubsub_item,
                           [service_jid], sql(get_last_items)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec set_node(mongooseim:host_type(), mod_pubsub:pubsub_node()) -> ok.
set_node(HostType, #pubsub_node{node_key = {ServiceJid, NodeId},
                                config = #{access_model := AccessModel}}) ->
    Values = [jid:to_binary(ServiceJid), NodeId, atom_to_binary(AccessModel)],
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_node_upsert, Values,
                                                [atom_to_binary(AccessModel)]),
    ok.

-spec get_node(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:pubsub_node() | undefined.
get_node(HostType, {ServiceJid, NodeId} = NodeKey) ->
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_node,
                                             [jid:to_binary(ServiceJid), NodeId]) of
        {selected, []} ->
            undefined;
        {selected, [{AccessModelBin}]} ->
            #pubsub_node{node_key = NodeKey,
                         config = #{access_model => binary_to_existing_atom(AccessModelBin)}}
    end.

-spec get_nodes(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:node_key()].
get_nodes(HostType, ServiceJid) ->
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_nodes,
                                                           [jid:to_binary(ServiceJid)]),
    [{ServiceJid, NodeId} || {NodeId} <- Rows].

-spec delete_nodes(mongooseim:host_type(), jid:jid()) -> ok.
delete_nodes(HostType, ServiceJid) ->
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, pubsub_delete_nodes,
                                                       [jid:to_binary(ServiceJid)]),
    ok.

-spec delete_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
delete_node(HostType, {ServiceJid, NodeId}) ->
    Args = [jid:to_binary(ServiceJid), NodeId],
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, pubsub_delete_node, Args),
    ok.

-spec set_subscription(mongooseim:host_type(), mod_pubsub:subscription()) -> ok.
set_subscription(HostType, #subscription{node_key = {ServiceJid, NodeId},
                                         jid = SubscriberJid}) ->
    Values = [jid:to_binary(ServiceJid), NodeId, jid:to_binary(SubscriberJid)],
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_subscription_upsert,
                                                Values, []),
    ok.

-spec get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key()) ->
    [mod_pubsub:subscription()].
get_subscriptions(HostType, {ServiceJid, NodeId} = NodeKey) ->
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_subscriptions,
                                                           [jid:to_binary(ServiceJid), NodeId]),
    [row_to_subscription(NodeKey, SubscriberJidBin) || {SubscriberJidBin} <- Rows].

-spec get_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    mod_pubsub:subscription() | undefined.
get_subscription(HostType, {ServiceJid, NodeId} = NodeKey, SubscriberJid) ->
    Args = [jid:to_binary(ServiceJid), NodeId, jid:to_binary(SubscriberJid)],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_subscriptions_for_jid,
                                                           Args),
    case Rows of
        [{SubscriberJidBin}] -> row_to_subscription(NodeKey, SubscriberJidBin);
        [] -> undefined
    end.

-spec delete_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) -> ok | not_found.
delete_subscription(HostType, {ServiceJid, NodeId}, SubscriberJid) ->
    Args = [jid:to_binary(ServiceJid), NodeId, jid:to_binary(SubscriberJid)],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_delete_subscription, Args) of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

-spec set_item(mongooseim:host_type(), mod_pubsub:item()) -> ok.
set_item(HostType, #item{node_key = {ServiceJid, NodeId},
                         id = ItemId,
                         publisher_jid = PublisherJid,
                         payload = Payload}) ->
    CreatedAt = os:system_time(microsecond),
    PublisherJidBin = jid:to_binary(PublisherJid),
    PayloadBin = encode_payload(Payload),
    KeyValues = [jid:to_binary(ServiceJid), NodeId, ItemId],
    UpdateValues = [PublisherJidBin, PayloadBin, CreatedAt],
    InsertValues = KeyValues ++ UpdateValues,
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_item_upsert,
                                                InsertValues, UpdateValues),
    ok.

-spec get_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id()) ->
    mod_pubsub:item() | undefined.
get_item(HostType, {ServiceJid, NodeId} = NodeKey, ItemId) ->
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_item,
                                             [jid:to_binary(ServiceJid), NodeId, ItemId]) of
        {selected, []} ->
            undefined;
        {selected, [{PublisherJidBin, PayloadBin}]} ->
            row_to_item(HostType, NodeKey, ItemId, PublisherJidBin, PayloadBin)
    end.

-spec get_items(mongooseim:host_type(), mod_pubsub:node_key()) -> [mod_pubsub:item()].
get_items(HostType, {ServiceJid, NodeId} = NodeKey) ->
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_items,
                                                           [jid:to_binary(ServiceJid), NodeId]),
    [row_to_item(HostType, NodeKey, ItemId, PublisherJidBin, PayloadBin)
     || {ItemId, PublisherJidBin, PayloadBin} <- Rows].

-spec get_last_item(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:item() | undefined.
get_last_item(HostType, {ServiceJid, NodeId} = NodeKey) ->
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_last_item,
                                             [jid:to_binary(ServiceJid), NodeId]) of
        {selected, []} ->
            undefined;
        {selected, [{ItemId, PublisherJidBin, PayloadBin}]} ->
            row_to_item(HostType, NodeKey, ItemId, PublisherJidBin, PayloadBin)
    end.

-spec get_last_items(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:item()].
get_last_items(HostType, ServiceJid) ->
    ServiceJidBin = jid:to_binary(ServiceJid),
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_last_items,
                                                           [ServiceJidBin]),
    [row_to_item(HostType, {ServiceJid, NodeId}, ItemId, PublisherJidBin, PayloadBin)
     || {NodeId, ItemId, PublisherJidBin, PayloadBin} <- Rows].

-spec encode_payload(mod_pubsub:item_payload()) -> binary().
encode_payload(Payload) ->
    exml:to_binary(Payload).

-spec decode_payload(mongooseim:host_type(), binary()) -> mod_pubsub:item_payload().
decode_payload(HostType, PayloadBin) ->
    {ok, Payload} = exml:parse(mongoose_rdbms:unescape_binary(HostType, PayloadBin)),
    Payload.

-spec row_to_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id(),
                  binary(), binary()) -> mod_pubsub:item().
row_to_item(HostType, NodeKey, ItemId, PublisherJidBin, PayloadBin) ->
    Payload = decode_payload(HostType, PayloadBin),
    #item{node_key = NodeKey,
          id = ItemId,
          publisher_jid = jid:from_binary(PublisherJidBin),
          payload = Payload}.

-spec row_to_subscription(mod_pubsub:node_key(), binary()) -> mod_pubsub:subscription().
row_to_subscription(NodeKey, SubscriberJidBin) ->
    #subscription{node_key = NodeKey, jid = jid:from_binary(SubscriberJidBin)}.

%% SQL queries

sql(get_item) ->
    ~"""
     SELECT publisher_jid, payload
     FROM pubsub_item
     WHERE service_jid = ? AND node_id = ? AND item_id = ?
     """;
sql(get_items) ->
    ~"""
     SELECT item_id, publisher_jid, payload
     FROM pubsub_item
     WHERE service_jid = ? AND node_id = ?
     ORDER BY created_at
     """;
sql(get_last_item) ->
    ~"""
     SELECT item_id, publisher_jid, payload
     FROM pubsub_item WHERE service_jid = ? AND node_id = ?
     ORDER BY created_at DESC LIMIT 1
     """;
sql(get_nodes) ->
    ~"""
     SELECT node_id
     FROM pubsub_node
     WHERE service_jid = ?
     """;
sql(get_subscriptions) ->
    ~"""
     SELECT subscriber_jid
     FROM pubsub_subscription
     WHERE service_jid = ? AND node_id = ?
     """;
sql(get_subscriptions_for_jid) ->
    ~"""
     SELECT subscriber_jid
     FROM pubsub_subscription
     WHERE service_jid = ? AND node_id = ? AND subscriber_jid = ?
     """;
sql(delete_subscription) ->
    ~"""
     DELETE FROM pubsub_subscription
     WHERE service_jid = ? AND node_id = ? AND subscriber_jid = ?
     """;
sql(get_node) ->
    ~"""
     SELECT access_model
     FROM pubsub_node
     WHERE service_jid = ? AND node_id = ?
     """;
sql(delete_node) ->
    ~"""
     DELETE FROM pubsub_node
     WHERE service_jid = ? AND node_id = ?
     """;
sql(delete_nodes) ->
    ~"""
     DELETE FROM pubsub_node
     WHERE service_jid = ?
     """;
sql(get_last_items) ->
    ~"""
     SELECT n.node_id, i.item_id, i.publisher_jid, i.payload
     FROM pubsub_node AS n
     CROSS JOIN LATERAL (
         SELECT item_id, publisher_jid, payload
         FROM pubsub_item
         WHERE service_jid = n.service_jid AND node_id = n.node_id
         ORDER BY created_at DESC
         LIMIT 1
     ) AS i
     WHERE n.service_jid = ?
     """.
