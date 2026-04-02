-module(mod_pubsub_backend_rdbms).
-moduledoc "RDBMS backend for PubSub (XEP-0060)".

-behaviour(mod_pubsub_backend).

-include_lib("exml/include/exml.hrl").
-include("mod_pubsub.hrl").

-export([start/1, stop/1, set_node/2, get_nodes/2, delete_nodes/2,
         set_item/2, get_last_item/2, get_last_items/2]).

-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    NodeFields = [~"service_jid", ~"node_id"],
    KeyFields = [~"service_jid", ~"node_id", ~"item_id"],
    UpdateFields = [~"publisher_jid", ~"payload", ~"created_at"],
    rdbms_queries:prepare_upsert(HostType, pubsub_node_upsert, pubsub_node,
                                 NodeFields, [], NodeFields),
    mongoose_rdbms:prepare(pubsub_delete_nodes, pubsub_node,
                           [service_jid], sql(delete_nodes)),
    mongoose_rdbms:prepare(pubsub_get_nodes, pubsub_node,
                           [service_jid], sql(get_nodes)),
    rdbms_queries:prepare_upsert(HostType, pubsub_item_upsert, pubsub_item,
                                 KeyFields ++ UpdateFields, UpdateFields, KeyFields),
    mongoose_rdbms:prepare(pubsub_get_last_item, pubsub_item,
                           [service_jid, node_id], sql(get_last_item)),
    mongoose_rdbms:prepare(pubsub_get_last_items, pubsub_item,
                           [service_jid], sql(get_last_items)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec set_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
set_node(HostType, {ServiceJid, NodeId}) ->
    Values = [jid:to_binary(ServiceJid), NodeId],
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_node_upsert, Values, []),
    ok.

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
    exml:to_binary(#xmlel{name = ~"item", children = Payload}).

-spec decode_payload(mongooseim:host_type(), binary()) -> mod_pubsub:item_payload().
decode_payload(HostType, PayloadBin) ->
    {ok, #xmlel{children = Payload}} =
        exml:parse(mongoose_rdbms:unescape_binary(HostType, PayloadBin)),
    Payload.

-spec row_to_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id(),
                  binary(), binary()) -> mod_pubsub:item().
row_to_item(HostType, NodeKey, ItemId, PublisherJidBin, PayloadBin) ->
    Payload = decode_payload(HostType, PayloadBin),
    #item{node_key = NodeKey,
          id = ItemId,
          publisher_jid = jid:from_binary(PublisherJidBin),
          payload = Payload}.

%% SQL queries

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
