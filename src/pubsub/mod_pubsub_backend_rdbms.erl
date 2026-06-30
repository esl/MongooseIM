-module(mod_pubsub_backend_rdbms).
-moduledoc "RDBMS backend for PubSub (XEP-0060)".

-behaviour(mod_pubsub_backend).

-include_lib("exml/include/exml.hrl").
-include("jlib.hrl").
-include("mod_pubsub.hrl").

-export([start/1, stop/1, set_node/2, get_node/2, get_nodes/2, delete_node/2,
         remove_user/2, remove_domain/2,
         set_subscription/2, delete_subscription/3, get_subscriptions/2,
         get_user_subscriptions/2, get_subscription/3,
         set_item/3, delete_item/3, get_items/3, get_user_items/2,
         get_last_item/2, get_last_items/2]).

-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    NodeJid = [service_domain, service_user],
    NodeKey = NodeJid ++ [node_id],
    SubscriberBareJid = [subscriber_domain, subscriber_user],
    SubscriptionKey = NodeKey ++ SubscriberBareJid ++ [subscriber_resource],
    ItemKey = NodeKey ++ [item_id],
    PublisherFullJid = [publisher_domain, publisher_user, publisher_resource],
    ItemUpdateFields = PublisherFullJid ++ [payload, published_at],
    prepare_upsert(HostType, pubsub_node_upsert, pubsub_node, [access_model, max_items], NodeKey),
    mongoose_rdbms:prepare(pubsub_get_node, pubsub_node, NodeKey, sql(get_node)),
    mongoose_rdbms:prepare(pubsub_delete_node, pubsub_node, NodeKey, sql(delete_node)),
    mongoose_rdbms:prepare(pubsub_delete_nodes, pubsub_node, NodeJid, sql(delete_nodes)),
    mongoose_rdbms:prepare(pubsub_delete_domain_nodes, pubsub_node,
                           [service_domain], sql(delete_domain_nodes)),
    mongoose_rdbms:prepare(pubsub_get_nodes, pubsub_node, NodeJid, sql(get_nodes)),
    prepare_upsert(HostType, pubsub_subscription_upsert, pubsub_subscription, [], SubscriptionKey),
    mongoose_rdbms:prepare(pubsub_delete_subscription, pubsub_subscription,
                           SubscriptionKey, sql(delete_subscription)),
    mongoose_rdbms:prepare(pubsub_delete_user_subscriptions, pubsub_subscription,
                           SubscriberBareJid, sql(delete_user_subscriptions)),
    mongoose_rdbms:prepare(pubsub_delete_domain_subscriptions, pubsub_subscription,
                           [subscriber_domain], sql(delete_domain_subscriptions)),
    mongoose_rdbms:prepare(pubsub_get_subscriptions, pubsub_subscription,
                           NodeKey, sql(get_subscriptions)),
    mongoose_rdbms:prepare(pubsub_get_user_subscriptions, pubsub_subscription,
                           SubscriberBareJid, sql(get_user_subscriptions)),
    mongoose_rdbms:prepare(pubsub_get_subscription, pubsub_subscription,
                           SubscriptionKey, sql(get_subscription)),
    prepare_upsert(HostType, pubsub_item_upsert, pubsub_item, ItemUpdateFields, ItemKey),
    mongoose_rdbms:prepare(pubsub_get_item, pubsub_item, ItemKey, sql(get_item)),
    mongoose_rdbms:prepare(pubsub_get_items, pubsub_item, NodeKey, sql(get_items)),
    mongoose_rdbms:prepare(pubsub_get_items_limit, pubsub_item,
                           NodeKey ++ [limit], sql(get_items_limit)),
    mongoose_rdbms:prepare(pubsub_get_user_items, pubsub_item, NodeJid, sql(get_user_items)),
    mongoose_rdbms:prepare(pubsub_get_last_item, pubsub_item, NodeKey, sql(get_last_item)),
    mongoose_rdbms:prepare(pubsub_get_last_items, pubsub_item, NodeJid, sql(get_last_items)),
    mongoose_rdbms:prepare(pubsub_delete_item, pubsub_item, ItemKey, sql(delete_item)),
    mongoose_rdbms:prepare(pubsub_get_extra_item_published_at, pubsub_item,
                           NodeKey ++ [offset], sql(get_extra_item_published_at)),
    mongoose_rdbms:prepare(pubsub_delete_old_items, pubsub_item,
                           NodeKey ++ [published_at], sql(delete_old_items)),
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
set_node(HostType, Node) ->
    F = fun() -> set_node_t(HostType, Node) end,
    {atomic, ok} = mongoose_rdbms:sql_transaction(HostType, F),
    ok.

-spec set_node_t(mongooseim:host_type(), mod_pubsub:pubsub_node()) -> ok.
set_node_t(HostType, #pubsub_node{node_key = NodeKey = {ServiceJid, NodeId},
                                  config = #{access_model := AccessModel,
                                             max_items := MaxItems}}) ->
    MaxItemsValue = max_items_node_to_sql(MaxItems),
    Values = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId,
              atom_to_binary(AccessModel), MaxItemsValue],
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_node_upsert, Values,
                                                [atom_to_binary(AccessModel), MaxItemsValue]),
    delete_old_items(HostType, NodeKey, MaxItems),
    ok.

-spec get_node(mongooseim:host_type(), mod_pubsub:node_key()) ->
    mod_pubsub:pubsub_node() | undefined.
get_node(HostType, {ServiceJid, NodeId} = NodeKey) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_node, Args) of
        {selected, []} ->
            undefined;
        {selected, [{AccessModelBin, MaxItems}]} ->
            row_to_node(NodeKey, AccessModelBin, MaxItems)
    end.

-spec get_nodes(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:pubsub_node()].
get_nodes(HostType, ServiceJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_nodes, Args),
    [row_to_node({ServiceJid, NodeId}, AccessModelBin, MaxItems)
     || {NodeId, AccessModelBin, MaxItems} <- Rows].

-spec delete_node(mongooseim:host_type(), mod_pubsub:node_key()) -> ok.
delete_node(HostType, {ServiceJid, NodeId}) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, pubsub_delete_node, Args),
    ok.

-spec remove_user(mongooseim:host_type(), jid:jid()) -> ok.
remove_user(HostType, ServiceJid) ->
    F = fun() -> remove_user_t(HostType, ServiceJid) end,
    {atomic, ok} = mongoose_rdbms:sql_transaction(HostType, F),
    ok.

-spec remove_user_t(mongooseim:host_type(), jid:jid()) -> ok.
remove_user_t(HostType, #jid{lserver = Domain, luser = User}) ->
    {updated, _} =
        mongoose_rdbms:execute_successfully(HostType, pubsub_delete_nodes, [Domain, User]),
    {updated, _} =
        mongoose_rdbms:execute_successfully(HostType, pubsub_delete_user_subscriptions,
                                            [Domain, User]),
    ok.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, Domain) ->
    F = fun() -> remove_domain_t(HostType, Domain) end,
    {atomic, ok} = mongoose_rdbms:sql_transaction(HostType, F),
    ok.

-spec remove_domain_t(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain_t(HostType, Domain) ->
    {updated, _} =
        mongoose_rdbms:execute_successfully(HostType, pubsub_delete_domain_nodes, [Domain]),
    {updated, _} =
        mongoose_rdbms:execute_successfully(HostType, pubsub_delete_domain_subscriptions, [Domain]),
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

-spec get_user_subscriptions(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:subscription()].
get_user_subscriptions(HostType, #jid{lserver = SubscriberDomain, luser = SubscriberUser}) ->
    Args = [SubscriberDomain, SubscriberUser],
    {selected, Rows} =
        mongoose_rdbms:execute_successfully(HostType, pubsub_get_user_subscriptions, Args),
    [row_to_subscription({jid:make_noprep(ServiceUser, ServiceDomain, <<>>), NodeId},
                         SubscriberDomain, SubscriberUser, SubscriberResource)
     || {ServiceDomain, ServiceUser, NodeId, SubscriberResource} <- Rows].

-spec get_subscription(mongooseim:host_type(), mod_pubsub:node_key(), jid:jid()) ->
    mod_pubsub:subscription() | undefined.
get_subscription(HostType, {ServiceJid, NodeId} = NodeKey, SubscriberJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId,
            SubscriberJid#jid.lserver, SubscriberJid#jid.luser, SubscriberJid#jid.lresource],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_subscription,
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

-spec set_item(mongooseim:host_type(), mod_pubsub:item(), max | pos_integer()) -> ok.
set_item(HostType, Item, MaxItems) ->
    F = fun() -> set_item_t(HostType, Item, MaxItems) end,
    {atomic, ok} = mongoose_rdbms:sql_transaction(HostType, F),
    ok.

-spec set_item_t(mongooseim:host_type(), mod_pubsub:item(), max | pos_integer()) -> ok.
set_item_t(HostType, #item{node_key = NodeKey = {ServiceJid, NodeId},
                           id = ItemId,
                           publisher_jid = PublisherJid,
                           payload = Payload},
           MaxItems) ->
    PublishedAt = os:system_time(microsecond),
    PayloadBin = encode_payload(Payload),
    KeyValues = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, ItemId],
    UpdateValues = [PublisherJid#jid.lserver, PublisherJid#jid.luser,
                    PublisherJid#jid.lresource, PayloadBin, PublishedAt],
    InsertValues = KeyValues ++ UpdateValues,
    {updated, _} = rdbms_queries:execute_upsert(HostType, pubsub_item_upsert,
                                                InsertValues, UpdateValues),
    delete_old_items(HostType, NodeKey, MaxItems),
    ok.

-spec delete_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id()) ->
          ok | not_found.
delete_item(HostType, {ServiceJid, NodeId}, ItemId) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, ItemId],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_delete_item, Args) of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

-spec get_items(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:get_items_opts()) ->
          [mod_pubsub:item()].
get_items(HostType, NodeKey, #{item_ids := ItemIds}) ->
    lists:flatmap(fun(ItemId) -> get_item(HostType, NodeKey, ItemId) end, ItemIds);
get_items(HostType, {ServiceJid, NodeId}, Opts) ->
    Rows = get_item_rows(HostType, {ServiceJid, NodeId}, Opts),
    [row_to_item(HostType, {ServiceJid, NodeId}, ItemId, PublisherDomain, PublisherUser,
                 PublisherResource, PayloadBin, PublishedAt)
     || {ItemId, PublisherDomain, PublisherUser, PublisherResource, PayloadBin, PublishedAt}
            <- Rows].

-spec get_item(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:item_id()) ->
          [mod_pubsub:item()].
get_item(HostType, {ServiceJid, NodeId} = NodeKey, ItemId) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, ItemId],
    {selected, Results} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_item, Args),
    case Results of
        [] ->
            [];
        [{PublisherDomain, PublisherUser, PublisherResource, PayloadBin, PublishedAt}] ->
            [row_to_item(HostType, NodeKey, ItemId, PublisherDomain, PublisherUser,
                         PublisherResource, PayloadBin, PublishedAt)]
    end.

-spec get_item_rows(mongooseim:host_type(), mod_pubsub:node_key(), mod_pubsub:get_items_opts()) ->
          [{mod_pubsub:item_id(), jid:lserver(), jid:luser(), jid:lresource(), binary(),
            integer()}].
get_item_rows(HostType, {ServiceJid, NodeId}, #{max_items := MaxItems}) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, MaxItems],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_items_limit, Args),
    lists:reverse(Rows);
get_item_rows(HostType, {ServiceJid, NodeId}, #{}) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_items, Args),
    Rows.

-spec get_user_items(mongooseim:host_type(), jid:jid()) -> [mod_pubsub:item()].
get_user_items(HostType, ServiceJid) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser],
    {selected, Rows} = mongoose_rdbms:execute_successfully(HostType, pubsub_get_user_items, Args),
    [row_to_item(HostType, {ServiceJid, NodeId}, ItemId, PublisherDomain, PublisherUser,
                 PublisherResource, PayloadBin, PublishedAt)
     || {NodeId, ItemId, PublisherDomain, PublisherUser, PublisherResource, PayloadBin, PublishedAt}
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

-spec row_to_node(mod_pubsub:node_key(), binary(), null | non_neg_integer()) ->
    mod_pubsub:pubsub_node().
row_to_node(NodeKey, AccessModelBin, MaxItems) ->
    #pubsub_node{node_key = NodeKey,
                 config = #{access_model => binary_to_existing_atom(AccessModelBin),
                            max_items => max_items_node_from_sql(MaxItems)}}.

-spec max_items_node_to_sql(mod_pubsub:max_items_node()) -> null | non_neg_integer().
max_items_node_to_sql(max) ->
    null;
max_items_node_to_sql(MaxItems) ->
    MaxItems.

-spec max_items_node_from_sql(null | non_neg_integer()) -> mod_pubsub:max_items_node().
max_items_node_from_sql(null) ->
    max;
max_items_node_from_sql(MaxItems) ->
    MaxItems.

-spec delete_old_items(mongooseim:host_type(), mod_pubsub:node_key(),
                       mod_pubsub:max_items_node()) -> ok | no_limit | no_extra_items.
delete_old_items(HostType, NodeKey, MaxItems) ->
    maybe
        {ok, Limit} ?= get_effective_max_items_limit(HostType, MaxItems),
        {ok, PublishedAt} ?= get_extra_item_published_at(HostType, NodeKey, Limit),
        delete_items_published_at_or_before(HostType, NodeKey, PublishedAt)
    end.

-spec get_effective_max_items_limit(mongooseim:host_type(), mod_pubsub:max_items_node()) ->
          {ok, non_neg_integer()} | no_limit.
get_effective_max_items_limit(HostType, MaxItems) ->
    case min(MaxItems, gen_mod:get_module_opt(HostType, mod_pubsub, max_items_per_node)) of
        Limit when is_integer(Limit) -> {ok, Limit};
        _ -> no_limit % both were atoms: 'max', 'infinity'
    end.

-spec get_extra_item_published_at(mongooseim:host_type(), mod_pubsub:node_key(),
                                  non_neg_integer()) -> {ok, integer()} | no_extra_items.
get_extra_item_published_at(HostType, {ServiceJid, NodeId}, Limit) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, Limit],
    case mongoose_rdbms:execute_successfully(HostType, pubsub_get_extra_item_published_at,
                                             Args) of
        {selected, [{PublishedAt}]} -> {ok, PublishedAt};
        {selected, []} -> no_extra_items
    end.

-spec delete_items_published_at_or_before(mongooseim:host_type(), mod_pubsub:node_key(),
                                          integer()) -> ok.
delete_items_published_at_or_before(HostType, {ServiceJid, NodeId}, PublishedAt) ->
    Args = [ServiceJid#jid.lserver, ServiceJid#jid.luser, NodeId, PublishedAt],
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, pubsub_delete_old_items,
                                                       Args),
    ok.

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
sql(get_items_limit) ->
    <<(sql(get_items))/binary, " DESC LIMIT ?">>;
sql(get_user_items) ->
    ~"""
     SELECT node_id, item_id, publisher_domain, publisher_user, publisher_resource,
            payload, published_at
     FROM pubsub_item
     WHERE service_domain = ? AND service_user = ?
     ORDER BY node_id, published_at
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
     SELECT node_id, access_model, max_items
     FROM pubsub_node
     WHERE service_domain = ? AND service_user = ?
     """;
sql(get_subscriptions) ->
    ~"""
     SELECT subscriber_domain, subscriber_user, subscriber_resource
     FROM pubsub_subscription
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
     """;
sql(get_user_subscriptions) ->
    ~"""
     SELECT service_domain, service_user, node_id, subscriber_resource
     FROM pubsub_subscription
     WHERE subscriber_domain = ? AND subscriber_user = ?
     ORDER BY service_domain, service_user, node_id
     """;
sql(get_subscription) ->
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
     SELECT access_model, max_items
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
sql(delete_domain_nodes) ->
    ~"""
     DELETE FROM pubsub_node
     WHERE service_domain = ?
     """;
sql(delete_user_subscriptions) ->
    ~"""
     DELETE FROM pubsub_subscription
     WHERE subscriber_domain = ? AND subscriber_user = ?
     """;
sql(delete_domain_subscriptions) ->
    ~"""
     DELETE FROM pubsub_subscription
     WHERE subscriber_domain = ?
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
     """;
sql(get_extra_item_published_at) ->
    ~"""
     SELECT published_at
     FROM pubsub_item
     WHERE service_domain = ? AND service_user = ? AND node_id = ?
     ORDER BY published_at DESC
     LIMIT 1 OFFSET ?
     """;
sql(delete_old_items) ->
    ~"""
     DELETE FROM pubsub_item
     WHERE service_domain = ? AND service_user = ? AND node_id = ? AND published_at <= ?
     """.
