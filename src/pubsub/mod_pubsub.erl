-module(mod_pubsub).
-behaviour(gen_mod).

-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

-export([user_send_iq/3,
         caps_recognised/3,
         roster_out_subscription/3,
         remove_user/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-include("mod_pubsub.hrl").

-type item() :: #item{}.
-type pubsub_node() :: #pubsub_node{}.
-type subscription() :: #subscription{}.
-type node_key() :: {jid:jid(), node_id()}.
-type node_id() :: binary().
-type item_id() :: binary().
-type subscription_id() :: binary().
-type item_payload() :: [exml:child()].
-type access_model() :: open | presence.
-type node_config() :: #{access_model => access_model()}.
-type error_reason() :: generic_error_reason() | {generic_error_reason(), binary()}.
-type generic_error_reason() ::
        bad_request | conflict | forbidden | not_acceptable | not_authorized |
        unexpected_request | item_not_found.
-type error_result() :: {error, error_reason()}.
-type result(T) :: {ok, T} | error_result().
-type ok_result() :: ok | error_result().
-type node_config_field() :: {access_model, access_model()}.


-type iq_request() :: #{acc := mongoose_acc:t(),
                        iq := jlib:iq(),
                        from_jid := jid:jid(),
                        service_jid := jid:jid(),
                        c2s_data := mongoose_c2s:data()}.

-type iq_action() ::
        #{action := error, reason := error_reason()}
      | #{action := create, node_id := node_id(), config := node_config(),
          result => ok}
      | #{action := delete, node_id := node_id(),
          result => ok}
      | #{action := subscribe, node_id := node_id(), subscriber_jid := jid:jid(),
          result => subscription_id()}
      | #{action := unsubscribe, node_id := node_id(), subscriber_jid := jid:jid(),
          result => ok}
      | #{action := get_items, node_id := node_id(),
          result => [item()]}
      | #{action := get_item, node_id := node_id(), item_id := item_id(),
          result => [item()]}
      | #{action := publish, node_id := node_id(), item_id := item_id(), payload := item_payload(),
          config => node_config(),
          result => item_id()}.

-export_type([item/0, pubsub_node/0, subscription/0, node_key/0, node_id/0,
              item_id/0, subscription_id/0, item_payload/0, access_model/0, node_config/0]).

%% gen_mod callbacks

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_pubsub_backend:start(HostType, Opts).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mod_pubsub_backend:stop(HostType).

-spec supported_features() -> [gen_mod:module_feature()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{~"backend" => #option{type = atom,
                                        validate = {module, mod_pubsub_backend}}},
        defaults = #{~"backend" => rdbms}
    }.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{user_send_iq, HostType, fun ?MODULE:user_send_iq/3, #{}, 50},
     {caps_recognised, HostType, fun ?MODULE:caps_recognised/3, #{}, 50},
     {roster_out_subscription, HostType, fun ?MODULE:roster_out_subscription/3, #{}, 50},
     {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {anonymous_purge, HostType, fun ?MODULE:remove_user/3, #{}, 50}].

%% Hook handlers

-spec user_send_iq(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
          mongoose_c2s_hooks:result().
user_send_iq(Acc, _Args = #{c2s_data := C2SData}, _Extra) ->
    {From, To, Stanza} = mongoose_acc:packet(Acc),
    maybe
        IQ = #iq{sub_el = SubEl, xmlns = NS} ?= jlib:iq_query_info(Stanza),
        #xmlel{name = ~"pubsub", children = Children} ?= SubEl,
        #{} ?= Action = pubsub_action(NS, jlib:remove_cdata(Children)),
        Request = #{acc => Acc, iq => IQ, from_jid => From, service_jid => To, c2s_data => C2SData},
        ?LOG_DEBUG(#{what => pubsub_iq_action, request => Request, action => Action}),
        reply(Request, make_reply(try_action(Request, Action))),
        {stop, Acc}
    else
        _ -> {ok, Acc}
    end.

-spec caps_recognised(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
caps_recognised(Acc, #{c2s_data := C2SData, features := Features}, _Extra) ->
    SubscriberJid = mongoose_acc:from_jid(Acc),
    [send_last_items(host_type(OwnerJid), jid:to_bare(OwnerJid), SubscriberJid, Features)
     || OwnerJid <- subscriptions(s_to, C2SData)],
    {ok, Acc}.

-spec roster_out_subscription(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
roster_out_subscription(Acc, #{to := SubscriberJid, from := OwnerJid, type := subscribed}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    RecipientJids = [jid:to_bare(SubscriberJid)],
    lists:foreach(fun(Item = #item{node_key = NodeKey}) ->
                          FilteredJids = filter_recipients(RecipientJids, NodeKey),
                          broadcast_item(HostType, OwnerJid, FilteredJids, Item)
                  end, mod_pubsub_backend:get_last_items(HostType, OwnerJid)),
    {ok, Acc};
roster_out_subscription(Acc, _, _) ->
    {ok, Acc}.

-spec remove_user(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
remove_user(Acc, #{jid := ServiceJid}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mod_pubsub_backend:delete_nodes(HostType, ServiceJid),
    {ok, Acc}.

%% Internal functions

-spec try_action(iq_request(), iq_action()) -> iq_action().
try_action(Request, Action) ->
    maybe
        {error, Reason} ?= perform_action(Request, Action),
        #{action => error, reason => Reason}
    end.

-spec perform_action(iq_request(), iq_action()) -> iq_action() | error_result().
perform_action(#{}, Action = #{action := error}) ->
    Action;
perform_action(#{acc := Acc, service_jid := ServiceJid} = Request,
               Action = #{action := create, node_id := NodeId, config := #{access_model := AccessModel}}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        Node = #pubsub_node{node_key = {ServiceJid, NodeId}, access_model = AccessModel},
        mod_pubsub_backend:set_node(HostType, Node),
        Action#{result => ok}
    end;
perform_action(#{acc := Acc, service_jid := ServiceJid} = Request,
               Action = #{action := delete, node_id := NodeId}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        {ok, _} ?= get_node(HostType, NodeKey),
        mod_pubsub_backend:delete_node(HostType, NodeKey),
        Action#{result => ok}
    end;
perform_action(#{service_jid := ServiceJid} = Request,
               Action = #{action := subscribe, node_id := NodeId, subscriber_jid := SubscriberJid}) ->
    maybe
        NodeKey = {ServiceJid, NodeId},
        {ok, #subscription{id = SubscriptionId}} ?= subscribe(Request, SubscriberJid, NodeKey),
        Action#{result => SubscriptionId}
    end;
perform_action(#{service_jid := ServiceJid} = Request,
               Action = #{action := unsubscribe, node_id := NodeId, subscriber_jid := SubscriberJid}) ->
    maybe
        NodeKey = {ServiceJid, NodeId},
        ok ?= unsubscribe(Request, SubscriberJid, NodeKey),
        Action#{result => ok}
    end;
perform_action(#{acc := Acc, service_jid := ServiceJid} = Request,
               Action = #{action := get_items, node_id := NodeId}) ->
    maybe
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        {ok, Node} ?= get_node(HostType, NodeKey),
        ok ?= assert_retrieve_permission(Node, Request),
        Items = mod_pubsub_backend:get_items(HostType, NodeKey),
        Action#{result => Items}
    end;
perform_action(#{acc := Acc, service_jid := ServiceJid} = Request,
               Action = #{action := get_item, node_id := NodeId, item_id := ItemId}) ->
    maybe
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        {ok, Node} ?= get_node(HostType, NodeKey),
        ok ?= assert_retrieve_permission(Node, Request),
        {ok, Item} ?= get_item(HostType, NodeKey, ItemId),
        Action#{result => [Item]}
    end;
perform_action(#{acc := Acc, from_jid := PublisherJid, service_jid := ServiceJid} = Request,
               Action = #{action := publish, node_id := NodeId, item_id := ItemId, payload := Payload}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        Node = get_or_create_node(HostType, NodeKey, maps:get(config, Action, #{})),
        Item = #item{node_key = NodeKey, id = ItemId, publisher_jid = PublisherJid, payload = Payload},
        mod_pubsub_backend:set_item(HostType, Item),
        broadcast_item(HostType, jid:to_bare(PublisherJid), recipient_jids(Node, Request), Item),
        Action#{result => ItemId}
    end.

-spec make_reply(iq_action()) -> {result | error, [exml:child()]}.
make_reply(#{action := error, reason := {GenericReason, PubSubReason}}) ->
    {error, [error_el(GenericReason), mod_pubsub_xml:pubsub_error_el(PubSubReason)]};
make_reply(#{action := error, reason := Reason}) ->
    {error, [error_el(Reason)]};
make_reply(#{action := _, result := ok}) ->
    {result, []};
make_reply(#{action := subscribe, node_id := NodeId, subscriber_jid := SubscriberJid,
             result := SubscriptionId}) ->
    {result, [mod_pubsub_xml:pubsub_el(
                [mod_pubsub_xml:subscription_el(SubscriberJid, NodeId, SubscriptionId, ~"subscribed")])]};
make_reply(#{action := Get, node_id := NodeId, result := Items}) when Get =:= get_item;
                                                                      Get =:= get_items ->
    {result, [mod_pubsub_xml:pubsub_el(
                [mod_pubsub_xml:items_el(NodeId, [mod_pubsub_xml:item_el(Item) || Item <- Items])])]};
make_reply(#{action := publish, result := ItemId}) ->
    {result, [mod_pubsub_xml:pubsub_el([mod_pubsub_xml:published_item_el(ItemId)])]}.

-spec error_el(generic_error_reason()) -> exml:element().
error_el(bad_request) -> mongoose_xmpp_errors:bad_request();
error_el(conflict) -> mongoose_xmpp_errors:conflict();
error_el(forbidden) -> mongoose_xmpp_errors:forbidden();
error_el(not_acceptable) -> mongoose_xmpp_errors:not_acceptable();
error_el(not_authorized) -> mongoose_xmpp_errors:not_authorized();
error_el(unexpected_request) -> mongoose_xmpp_errors:unexpected_request_cancel();
error_el(item_not_found) -> mongoose_xmpp_errors:item_not_found().

-spec subscribe(iq_request(), jid:jid(), node_key()) -> result(subscription()).
subscribe(#{acc := Acc} = Request, SubscriberJid, NodeKey) ->
    HostType = mongoose_acc:host_type(Acc),
    maybe
        {ok, Node = #pubsub_node{}} ?= get_node(HostType, NodeKey),
        ok ?= assert_subscribe_permission(Node, Request),
        SubscriptionId = make_subid(),
        Subscription = #subscription{node_key = NodeKey, jid = SubscriberJid, id = SubscriptionId},
        mod_pubsub_backend:set_subscription(mongoose_acc:host_type(Acc), Subscription),
        {ok, Subscription}
    end.

-spec unsubscribe(iq_request(), jid:jid(), node_key()) -> ok_result().
unsubscribe(#{acc := Acc}, SubscriberJid, NodeKey) ->
    HostType = mongoose_acc:host_type(Acc),
    maybe
        {ok, #pubsub_node{}} ?= get_node(HostType, NodeKey),
        delete_subscriptions(HostType, NodeKey, SubscriberJid)
    end.

-spec get_node(mongooseim:host_type(), node_key()) -> result(pubsub_node()).
get_node(HostType, NodeKey) ->
    case mod_pubsub_backend:get_node(HostType, NodeKey) of
        undefined -> {error, item_not_found};
        Node -> {ok, Node}
    end.

-spec get_item(mongooseim:host_type(), node_key(), item_id()) -> result(item()).
get_item(HostType, NodeKey, ItemId) ->
    case mod_pubsub_backend:get_item(HostType, NodeKey, ItemId) of
        undefined -> {error, item_not_found};
        Item -> {ok, Item}
    end.

-spec assert_retrieve_permission(pubsub_node(), iq_request()) -> ok_result().
assert_retrieve_permission(Node, Request) ->
    case is_owner(Request) of
        true -> ok;
        false -> assert_subscribe_permission(Node, Request)
    end.

-spec assert_subscribe_permission(pubsub_node(), iq_request()) -> ok_result().
assert_subscribe_permission(#pubsub_node{access_model = open}, _) ->
    ok;
assert_subscribe_permission(#pubsub_node{access_model = presence,
                                         node_key = {OwnerJid, _}}, Request) ->
    assert_subscribed_to(OwnerJid, Request).

-spec assert_owner(iq_request()) -> ok_result().
assert_owner(Request) ->
    case is_owner(Request) of
        true -> ok;
        false -> {error, forbidden}
    end.

-spec assert_subscribed_to(jid:jid(), iq_request()) -> ok_result().
assert_subscribed_to(OwnerJid, #{c2s_data := C2SData}) ->
    case is_subscribed_to(OwnerJid, C2SData) of
        true -> ok;
        false -> {error, {not_authorized, ~"presence-subscription-required"}}
    end.

-spec delete_subscriptions(mongooseim:host_type(), node_key(), jid:jid()) -> ok_result().
delete_subscriptions(HostType, NodeKey, SubscriberJid) ->
    case mod_pubsub_backend:get_subscriptions(HostType, NodeKey, SubscriberJid) of
        [] ->
            {error, {unexpected_request, ~"not-subscribed"}};
        _ ->
            mod_pubsub_backend:delete_subscription(HostType, NodeKey, SubscriberJid)
    end.

-spec is_owner(iq_request()) -> boolean().
is_owner(#{from_jid := #jid{luser = LUser, lserver = LServer},
           service_jid := #jid{luser = LUser, lserver = LServer}}) ->
    true;
is_owner(#{}) ->
    false.

-spec recipient_jids(pubsub_node(), iq_request()) -> [jid:jid()].
recipient_jids(Node, #{acc := Acc, c2s_data := C2SData}) ->
    Subs = lists:usort(implicit_subscribers(Node, C2SData) ++ explicit_subscribers(Node, Acc)),
    lists:foldl(fun drop_bare_jid_shadowed_by_full_jid/2, [], Subs).

-spec drop_bare_jid_shadowed_by_full_jid(jid:jid(), [jid:jid()]) -> [jid:jid()].
drop_bare_jid_shadowed_by_full_jid(#jid{luser = U, lserver = S} = FullJid,
                                   [#jid{luser = U, lserver = S, lresource = ~""} | Results]) ->
    [FullJid | Results];
drop_bare_jid_shadowed_by_full_jid(Jid, Results) ->
    [Jid | Results].

-spec implicit_subscribers(pubsub_node(), mongoose_c2s:data()) -> [jid:jid()].
implicit_subscribers(#pubsub_node{node_key = NodeKey = {OwnerJid, _}}, C2SData) ->
    filter_recipients([OwnerJid | subscriptions(s_from, C2SData)], NodeKey).

%% XEP-0163 4.1 limits notifications for explicit subscriptions to the 'open' model
-spec explicit_subscribers(pubsub_node(), mongoose_acc:t()) -> [jid:jid()].
explicit_subscribers(#pubsub_node{access_model = open, node_key = NodeKey}, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    [Jid || #subscription{jid = Jid} <- mod_pubsub_backend:get_subscriptions(HostType, NodeKey)];
explicit_subscribers(_Node, _Acc) ->
    [].

-spec get_or_create_node(mongooseim:host_type(), node_key(), node_config()) -> pubsub_node().
get_or_create_node(HostType, NodeKey, Config) ->
    case mod_pubsub_backend:get_node(HostType, NodeKey) of
        undefined ->
            AccessModel = maps:get(access_model, Config, presence),
            Node = #pubsub_node{node_key = NodeKey, access_model = AccessModel},
            mod_pubsub_backend:set_node(HostType, Node),
            Node;
        Node ->
            Node
    end.

-spec reply(iq_request(), {result | error, [exml:child()]}) -> ok.
reply(#{acc := Acc, iq := IQ, from_jid := FromJid, service_jid := ServiceJid}, {IQType, Els}) ->
    Reply = jlib:iq_to_xml(IQ#iq{type = IQType, sub_el = Els}),
    ejabberd_router:route(ServiceJid, FromJid, Acc, Reply),
    ok.

-spec pubsub_action(binary(), [exml:element()]) -> iq_action() | no_action.
%% TODO: support XEP-0060 instant nodes, i.e. <create/> without a node attribute.
pubsub_action(?NS_PUBSUB, [#xmlel{name = ~"create", attrs = #{~"node" := NodeId}} | Rest]) ->
    case parse_create_config(Rest) of
        {ok, Config} ->
            #{action => create, node_id => NodeId, config => Config};
        {error, Reason} ->
            #{action => error, reason => Reason}
    end;
pubsub_action(?NS_PUBSUB, [#xmlel{name = ~"subscribe",
                                  attrs = #{~"node" := NodeId, ~"jid" := SubscriberJidBin}}]) ->
    #{action => subscribe, node_id => NodeId, subscriber_jid => jid:from_binary(SubscriberJidBin)};
pubsub_action(?NS_PUBSUB, [#xmlel{name = ~"unsubscribe",
                                  attrs = #{~"node" := NodeId, ~"jid" := SubscriberJidBin}}]) ->
    #{action => unsubscribe, node_id => NodeId, subscriber_jid => jid:from_binary(SubscriberJidBin)};
pubsub_action(?NS_PUBSUB, [#xmlel{name = ~"items", attrs = #{~"node" := NodeId},
                                  children = [#xmlel{name = ~"item", attrs = #{~"id" := ItemId}}]}]) ->
    #{action => get_item, node_id => NodeId, item_id => ItemId};
pubsub_action(?NS_PUBSUB, [#xmlel{name = ~"items", attrs = #{~"node" := NodeId}}]) ->
    #{action => get_items, node_id => NodeId};
pubsub_action(?NS_PUBSUB, [El = #xmlel{name = ~"publish", attrs = #{~"node" := NodeId}} | Rest]) ->
    maybe
        {ok, Config} ?= parse_publish_options(Rest),
        {ok, ItemOpts} ?= parse_item(El#xmlel.children),
        ItemOpts#{action => publish, node_id => NodeId, config => Config}
    else
        {error, Reason} ->
            #{action => error, reason => Reason}
    end;
pubsub_action(?NS_PUBSUB_OWNER, [#xmlel{name = ~"delete", attrs = #{~"node" := NodeId}}]) ->
    #{action => delete, node_id => NodeId};
pubsub_action(_, _) ->
    no_action.

-spec parse_create_config([exml:element()]) -> result(node_config()).
parse_create_config([ConfigureEl = #xmlel{name = ~"configure"}]) ->
    parse_form_config(ConfigureEl, ?NS_PUBSUB_NODE_CONFIG, #{access_model => presence}, true);
parse_create_config([]) ->
    {ok, #{access_model => presence}};
parse_create_config(_) ->
    {error, bad_request}.

-spec parse_publish_options([exml:element()]) -> result(node_config()).
parse_publish_options([PublishOptionsEl = #xmlel{name = ~"publish-options"}]) ->
    case mongoose_data_forms:find_and_parse_form(PublishOptionsEl) of
        #{type := ~"submit", kvs := KVs, ns := ?NS_PUBSUB_PUB_OPTIONS} ->
            case parse_node_config(KVs) of
                {ok, Config} -> {ok, Config};
                {error, _} -> {error, {conflict, ~"precondition-not-met"}}
            end;
        _ ->
            {error, bad_request}
    end;
parse_publish_options([]) ->
    {ok, #{}};
parse_publish_options(_) ->
    {error, bad_request}.

-spec parse_item([exml:element()]) -> result(#{item_id := item_id(), payload := item_payload()}).
parse_item([#xmlel{attrs = #{~"id" := ItemId}, children = Payload}]) ->
    {ok, #{item_id => ItemId, payload => Payload}};
parse_item([]) ->
    {error, {bad_request, ~"item-required"}};
parse_item(_) ->
    {error, {bad_request, ~"invalid-payload"}}.

-spec parse_form_config(exml:element(), binary(), node_config(), boolean()) ->
    result(node_config()).
parse_form_config(FormEl, NS, DefaultConfig, AllowCancel) ->
    case mongoose_data_forms:find_and_parse_form(FormEl) of
        #{type := ~"submit", kvs := KVs, ns := NS} ->
            parse_node_config(KVs);
        #{type := ~"cancel"} when AllowCancel ->
            {ok, DefaultConfig};
        _ ->
            {error, bad_request}
    end.

-spec parse_node_config(mongoose_data_forms:kv_map()) -> result(node_config()).
parse_node_config(KVs) ->
    parse_node_config_fields(maps:to_list(KVs), []).

-spec parse_node_config_fields([{binary(), [binary()]}], [node_config_field()]) ->
          result(node_config()).
parse_node_config_fields([{Key, Values} | Rest], ParsedKVs) ->
    case parse_node_config_field(Key, Values) of
        {ok, ParsedKV} ->
            parse_node_config_fields(Rest, [ParsedKV | ParsedKVs]);
        {error, _} = Error ->
            Error
    end;
parse_node_config_fields([], ParsedKVs) ->
    {ok, maps:from_list(ParsedKVs)}.

-spec parse_node_config_field(binary(), [binary()]) -> result(node_config_field()).
parse_node_config_field(~"pubsub#access_model", Values) ->
    maybe
        {ok, AccessModel} ?= parse_access_model(Values),
        {ok, {access_model, AccessModel}}
    end;
parse_node_config_field(_, _) ->
    {error, bad_request}.

-spec parse_access_model([binary()]) -> result(access_model()).
parse_access_model([~"open"]) -> {ok, open};
parse_access_model([~"presence"]) -> {ok, presence};
parse_access_model([~"authorize"]) -> {error, {not_acceptable, ~"unsupported-access-model"}};
parse_access_model(_) -> {error, bad_request}.

-spec make_subid() -> subscription_id().
make_subid() ->
    mongoose_bin:gen_from_timestamp().

-spec send_last_items(mongooseim:host_type(), jid:jid(), jid:jid(), [mod_caps:feature()]) -> ok.
send_last_items(HostType, OwnerJid = #jid{lresource = ~""},
                SubscriberJid = #jid{lresource = <<_, _/binary>>}, Features) ->
    [route_notification(HostType, OwnerJid, SubscriberJid, mod_pubsub_xml:notification_message_el(Item))
     || Item <- mod_pubsub_backend:get_last_items(HostType, OwnerJid),
        lists:member(notify_feature(Item#item.node_key), Features)],
    ok.

-spec filter_recipients([jid:jid()], node_key()) -> [jid:jid()].
filter_recipients(RecipientJids, NodeKey) ->
    Feature = notify_feature(NodeKey),
    [RecipientJid#jid{lresource = LRes}
     || RecipientJid <- RecipientJids,
        {LRes, Features} <- resources_with_features(RecipientJid),
        lists:member(Feature, Features)].

-spec broadcast_item(mongooseim:host_type(), jid:jid(), [jid:jid()], item()) -> ok.
broadcast_item(_HostType, _FromJid, [], _Item) ->
    ok;
broadcast_item(HostType, FromJid, ToFullJids, Item) ->
    Notification = mod_pubsub_xml:notification_message_el(Item),
    lists:foreach(fun(ToJid) ->
                          route_notification(HostType, FromJid, ToJid, Notification)
                  end, ToFullJids).

-spec notify_feature(node_key()) -> mod_caps:feature().
notify_feature({_, NodeId}) ->
    <<NodeId/binary, "+notify">>.

-spec subscriptions(s_from | s_to, mongoose_c2s:data()) -> [jid:jid()].
subscriptions(Type, C2SData) ->
    case mongoose_c2s:get_mod_state(C2SData, mod_presence) of
        {ok, State} ->
            maps:keys(mod_presence:get(State, Type));
        _ ->
            []
    end.

-spec is_subscribed_to(jid:jid(), mongoose_c2s:data()) -> boolean().
is_subscribed_to(OwnerJid = #jid{lresource = ~""}, C2SData) ->
    case mongoose_c2s:get_mod_state(C2SData, mod_presence) of
        {ok, State} ->
            mod_presence:is_subscribed(State, OwnerJid, to);
        _ ->
            false
    end.

-spec resources_with_features(jid:jid()) -> [{jid:lresource(), [mod_caps:feature()]}].
resources_with_features(Jid = #jid{lserver = LServer}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} -> resources_with_features(HostType, Jid);
        {error, not_found} -> []
    end.

-spec resources_with_features(mongooseim:host_type(), jid:jid()) ->
    [{jid:lresource(), [mod_caps:feature()]}].
resources_with_features(HostType, Jid = #jid{lresource = ~""}) ->
    mod_caps:get_resources_with_features(HostType, Jid);
resources_with_features(HostType, Jid = #jid{lresource = LResource}) ->
    [{LResource, mod_caps:get_features(HostType, Jid)}].

-spec route_notification(mongooseim:host_type(), jid:jid(), jid:jid(), exml:element()) -> any().
route_notification(HostType, FromJid = #jid{lserver = LServer}, ToJid, Packet) ->
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             host_type => HostType,
                             lserver => LServer,
                             element => Packet,
                             from_jid => FromJid,
                             to_jid => ToJid}),
    ejabberd_router:route(FromJid, ToJid, Acc).

-spec host_type(jid:jid()) -> mongooseim:host_type().
host_type(#jid{lserver = LServer}) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    HostType.
