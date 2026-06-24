-module(mod_pubsub_backend_rdbms).
-moduledoc "RDBMS backend for PubSub (XEP-0060)".

-behaviour(mod_pubsub_backend).

-include_lib("exml/include/exml.hrl").
-include("jlib.hrl").
-include("mod_pubsub.hrl").

-export([start/1, stop/1, set_node/2, get_node/2, get_nodes/2, delete_node/2, delete_nodes/2,
         set_subscription/2, delete_subscription/3, get_subscriptions/2, get_subscription/3,
         set_item/2, delete_item/3, get_item/3, get_items/2, get_last_item/2, get_last_items/2]).

-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    NodeJid = [service_domain, service_user],
    NodeKey = NodeJid ++ [node_id],
    SubscriberJid = [subscriber_domain, subscriber_user, subscriber_resource],
    SubscriptionKey = NodeKey ++ SubscriberJid,
    ItemKey = NodeKey ++ [item_id],
    PublisherJid = [publisher_domain, publisher_user, publisher_resource],
    ItemUpdateFields = PublisherJid ++ [payload, published_at],
    prepare_upsert(HostType, pubsub_node_upsert, pubsub_node, [access_model], NodeKey),
    mongoose_rdbms:prepare(pubsub_get_node, pubsub_node, NodeKey, sql(get_node)),
    mongoose_rdbms:prepare(pubsub_delete_node, pubsub_node, NodeKey, sql(delete_node)),
    mongoose_rdbms:prepare(pubsub_delete_nodes, pubsub_node, NodeJid, sql(delete_nodes)),
    mongoose_rdbms:prepare(pubsub_get_nodes, pubsub_node, NodeJid, sql(get_nodes)),
    prepare_upsert(HostType, pubsub_subscription_upsert, pubsub_subscription, [], SubscriptionKey),
    mongoose_rdbms:prepare(pubsub_delete_subscription, pubsub_subscription,
                           SubscriptionKey, sql(delete_subscription)),
    mongoose_rdbms:prepare(pubsub_get_subscriptions, pubsub_subscription,
                           NodeKey, sql(get_subscriptions)),
    mongoose_rdbms:prepare(pubsub_get_subscriptions_for_jid, pubsub_subscription,
                           SubscriptionKey, sql(get_subscriptions_for_jid)),
    prepare_upsert(HostType, pubsub_item_upsert, pubsub_item, ItemUpdateFields, ItemKey),
    mongoose_rdbms:prepare(pubsub_get_item, pubsub_item, ItemKey, sql(get_item)),
    mongoose_rdbms:prepare(pubsub_get_items, pubsub_item, NodeKey, sql(get_items)),
    mongoose_rdbms:prepare(pubsub_get_last_item, pubsub_item, NodeKey, sql(get_last_item)),
    mongoose_rdbms:prepare(pubsub_get_last_items, pubsub_item, NodeJid, sql(get_last_items)),
    mongoose_rdbms:prepare(pubsub_delete_item, pubsub_item, ItemKey, sql(delete_item)),
    ok.

-spec prepare_upsert(mongooseim:host_type(), mongoose_rdbms:query_name(), atom(), [atom()],
                     [atom()]) -> ok.
prepare_upsert(HostType, QueryName, TableName, UpdateFields, KeyFields) ->
    UpdateBin = lists:map(fun atom_to_binary/1, UpdateFields),
    KeyBin = lists:map(fun atom_to_binary/1, KeyFields),
    rdbms_queries:prepare_upsert(HostType, QueryName, TableName,
                                 KeyBin ++ UpdateBin, UpdateBin, KeyBin),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec set_node(mongooseim:host_type(), mod_pubsub:pubsub_node()) -> ok.
set_node(HostType, #pubsub_node{node_key = {ServiceJid, NodeId},
                                config = #{access_model := AccessModel}}) ->
    Values = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, atom_to_binary(AccessModel)],
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_node_upsert, Values,
                                                [atom_to_binary(AccessModel)]),
    ok.

-spec get_node(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:pubsub_node() | undefined.
get_node(HostType, {ServiceJid, NodeId} = NodeKey) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_node, Args) of
        {selected, []} ->
            undefined;
        {selected, [{AccessModelBin}]} ->
            #pubsub_node{node_key = NodeKey,
                         config = #{access_model => binary_to_existing_atom(AccessModelBin)}}
    end.

-spec get_nodes(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:pubsub_node()].
get_nodes(HostType, ServiceJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_nodes, Args),
    [#pubsub_node{node_key = {ServiceJid, NodeId},
                  config = #{access_model => binary_to_existing_atom(AccessModelBin)}}
     || {NodeId, AccessModelBin} <- Rows].

-spec delete_nodes(mongooseim:host_type(), jid:jid()) -> ok.
delete_nodes(HostType, ServiceJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser],
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, pubsub_delete_nodes, Args),
    ok.

-spec delete_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
delete_node(HostType, {ServiceJid, NodeId}) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, pubsub_delete_node, Args),
    ok.

-spec set_subscription(mongooseim:host_type(), mod_pubsub:subscription()) -> ok.
set_subscription(HostType, #subscription{node_key = {ServiceJid, NodeId}, jid = SubscriberJid}) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId,
            SubscriberJid#jid.lserver, SubscriberJid#jid.luser, SubscriberJid#jid.lresource],
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_subscription_upsert, Args, []),
    ok.

-spec get_subscriptions(mongooseim:host_type(), mod_pubsub:node_key()) ->
    [mod_pubsub:subscription()].
get_subscriptions(HostType, {ServiceJid, NodeId} = NodeKey) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_subscriptions, Args),
    [row_to_subscription(NodeKey, SubscriberDomain, SubscriberUser, SubscriberResource)
     || {SubscriberDomain, SubscriberUser, SubscriberResource} <- Rows].

-spec get_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    mod_pubsub:subscription() | undefined.
get_subscription(HostType, {ServiceJid, NodeId} = NodeKey, SubscriberJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId,
            SubscriberJid#jid.lserver, SubscriberJid#jid.luser, SubscriberJid#jid.lresource],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_subscriptions_for_jid,
                                                           Args),
    case Rows of
        [{SubscriberDomain, SubscriberUser, SubscriberResource}] ->
            row_to_subscription(NodeKey, SubscriberDomain, SubscriberUser, SubscriberResource);
        [] -> undefined
    end.

-spec delete_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) -> ok | not_found.
delete_subscription(HostType, {ServiceJid, NodeId}, SubscriberJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId,
            SubscriberJid#jid.lserver, SubscriberJid#jid.luser, SubscriberJid#jid.lresource],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_delete_subscription, Args) of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

-spec set_item(mongooseim:host_type(), mod_pubsub:item()) -> ok.
set_item(HostType, #item{node_key = {ServiceJid, NodeId},
                         id = ItemId,
                         publisher_jid = PublisherJid,
                         payload = Payload}) ->
    PublishedAt = os:system_time(microsecond),
    PayloadBin = encode_payload(Payload),
    KeyValues = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, ItemId],
    UpdateValues = [PublisherJid#jid.lserver, PublisherJid#jid.luser,
                    PublisherJid#jid.lresource, PayloadBin, PublishedAt],
    InsertValues = KeyValues ++ UpdateValues,
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_item_upsert,
                                                InsertValues, UpdateValues),
    ok.

-spec delete_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id()) ->
          ok | not_found.
delete_item(HostType, {ServiceJid, NodeId}, ItemId) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, ItemId],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_delete_item, Args) of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

-spec get_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id()) ->
    mod_pubsub:item() | undefined.
get_item(HostType, {ServiceJid, NodeId} = NodeKey, ItemId) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, ItemId],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_item, Args) of
        {selected, []} ->
            undefined;
        {selected, [{PublisherDomain, PublisherUser, PublisherResource, PayloadBin, PublishedAt}]} ->
            row_to_item(HostType, NodeKey, ItemId, PublisherDomain, PublisherUser,
                        PublisherResource, PayloadBin, PublishedAt)
    end.

-spec get_items(mongooseim:host_type(), mod_pubsub:node_key()) -> [mod_pubsub:item()].
get_items(HostType, {ServiceJid, NodeId} = NodeKey) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_items, Args),
    [row_to_item(HostType, NodeKey, ItemId, PublisherDomain, PublisherUser, PublisherResource,
                 PayloadBin, PublishedAt)
     || {ItemId, PublisherDomain, PublisherUser, PublisherResource, PayloadBin, PublishedAt}
            <- Rows].

-spec get_last_item(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:item() | undefined.
get_last_item(HostType, {ServiceJid, NodeId} = NodeKey) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_last_item, Args) of
        {selected, []} ->
            undefined;
        {selected, [{ItemId, PublisherDomain, PublisherUser, PublisherResource, PayloadBin,
                     PublishedAt}]} ->
            row_to_item(HostType, NodeKey, ItemId, PublisherDomain, PublisherUser,
                        PublisherResource, PayloadBin, PublishedAt)
    end.

-spec get_last_items(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:item()].
get_last_items(HostType, ServiceJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_last_items, Args),
    [row_to_item(HostType, {ServiceJid, NodeId}, ItemId, PublisherDomain, PublisherUser,
                 PublisherResource, PayloadBin, PublishedAt)
     || {NodeId, ItemId, PublisherDomain, PublisherUser, PublisherResource, PayloadBin,
         PublishedAt} <- Rows].

-spec encode_payload(mod_pubsub:item_payload()) -> binary().
encode_payload(Payload) ->
    exml:to_binary(Payload).

-spec decode_payload(mongooseim:host_type(), binary()) -> mod_pubsub:item_payload().
decode_payload(HostType, PayloadBin) ->
    {ok, Payload} = exml:parse(mongoose_rdbms:unescape_binary(HostType, PayloadBin)),
    Payload.

-spec row_to_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id(),
                  jid:lserver(), jid:luser(), jid:lresource(), binary(), integer()) ->
    mod_pubsub:item().
row_to_item(HostType, NodeKey, ItemId, PublisherDomain, PublisherUser, PublisherResource,
            PayloadBin, PublishedAt) ->
    Payload = decode_payload(HostType, PayloadBin),
    #item{node_key = NodeKey,
          id = ItemId,
          publisher_jid = jid:make_noprep(PublisherUser, PublisherDomain, PublisherResource),
          payload = Payload,
          published_at = PublishedAt}.

-spec row_to_subscription(mod_pubsub:node_key(), jid:lserver(), jid:luser(), jid:lresource()) ->
    mod_pubsub:subscription().
row_to_subscription(NodeKey, SubscriberDomain, SubscriberUser, SubscriberResource) ->
    #subscription{node_key = NodeKey,
                  jid = jid:make_noprep(SubscriberUser, SubscriberDomain, SubscriberResource)}.

%% SQL queries

sql(get_item) ->
    ~"""
     SELECT publisher_domain, publisher_user, publisher_resource, payload, published_at
     FROM pubsub_item
     WHERE service_domain = ? AND service_user = ? AND node_id = ? AND item_id = ?
     """;
sql(get_items) ->
    ~"""
     SELECT item_id, publisher_domain, publisher_user, publisher_resource, payload, published_at
     FROM pubsub_item
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
     ORDER BY published_at
     """;
sql(get_last_item) ->
    ~"""
     SELECT item_id, publisher_domain, publisher_user, publisher_resource, payload, published_at
     FROM pubsub_item
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
     ORDER BY published_at DESC LIMIT 1
     """;
sql(get_nodes) ->
    ~"""
     SELECT node_id, access_model
     FROM pubsub_node
     WHERE service_domain = ? AND service_user = ?
     """;
sql(get_subscriptions) ->
    ~"""
     SELECT subscriber_domain, subscriber_user, subscriber_resource
     FROM pubsub_subscription
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
     """;
sql(get_subscriptions_for_jid) ->
    ~"""
     SELECT subscriber_domain, subscriber_user, subscriber_resource
     FROM pubsub_subscription
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
       AND subscriber_domain = ? AND subscriber_user = ? AND subscriber_resource = ?
     """;
sql(delete_subscription) ->
    ~"""
     DELETE FROM pubsub_subscription
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
       AND subscriber_domain = ? AND subscriber_user = ? AND subscriber_resource = ?
     """;
sql(get_node) ->
    ~"""
     SELECT access_model
     FROM pubsub_node
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
     """;
sql(delete_node) ->
    ~"""
     DELETE FROM pubsub_node
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
     """;
sql(delete_nodes) ->
    ~"""
     DELETE FROM pubsub_node
     WHERE service_domain = ? AND service_user = ?
     """;
sql(get_last_items) ->
    ~"""
     SELECT n.node_id, i.item_id, i.publisher_domain, i.publisher_user, i.publisher_resource,
            i.payload, i.published_at
     FROM pubsub_node AS n
     CROSS JOIN LATERAL (
         SELECT item_id, publisher_domain, publisher_user, publisher_resource, payload, published_at
         FROM pubsub_item
         WHERE service_domain = n.service_domain
           AND service_user = n.service_user
           AND node_id = n.node_id
         ORDER BY published_at DESC
         LIMIT 1
     ) AS i
     WHERE n.service_domain = ? AND n.service_user = ?
     """;
sql(delete_item) ->
    ~"""
     DELETE FROM pubsub_item
     WHERE service_domain = ? AND service_user = ? AND node_id = ? AND item_id = ?
     """.
