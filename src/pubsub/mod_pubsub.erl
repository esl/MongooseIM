-module(mod_pubsub).
-behaviour(gen_mod).

-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

-export([user_send_iq/3,
         disco_sm_identity/3,
         disco_sm_features/3,
         disco_sm_items/3,
         caps_recognised/3,
         roster_out_subscription/3,
         remove_user/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_ns.hrl").
-include("mongoose_config_spec.hrl").
-include("mod_pubsub.hrl").

-type item() :: #item{}.
-type pubsub_node() :: #pubsub_node{}.
-type subscription() :: #subscription{}.
-type node_key() :: {jid:jid(), node_id()}.
-type node_id() :: binary().
-type item_id() :: binary().
-type item_ids() :: [item_id()].
-type item_payload() :: exml:element().
-type access_model() :: open | presence.
-type node_config() :: #{access_model => access_model()}.
-type error_reason() :: generic_error_reason() | {generic_error_reason(), binary()}.
-type generic_error_reason() ::
        bad_request | conflict | forbidden | not_acceptable | not_authorized |
        unexpected_request | item_not_found.
-type error_result() :: {error, error_reason()}.
-type result(T) :: {ok, T} | error_result().
-type ok_result() :: ok | error_result().


-type iq_request() :: #{acc := mongoose_acc:t(),
                        iq := jlib:iq(),
                        from_jid := jid:jid(),
                        service_jid := jid:jid(),
                        c2s_data := mongoose_c2s:data()}.

-type iq_action() ::
        #{action := error, reason := error_reason()}
      | #{action := defer}
      | #{action := create, node_id := node_id(), config := node_config(),
          result => ok}
      | #{action := get_configuration, node_id := node_id(),
          result => pubsub_node()}
      | #{action := configure, node_id := node_id(), config := node_config(),
          result => ok}
      | #{action := delete, node_id := node_id(),
          result => ok}
      | #{action := subscribe, node_id := node_id(), subscriber_jid := jid:jid(),
          result => ok}
      | #{action := unsubscribe, node_id := node_id(), subscriber_jid := jid:jid(),
          result => ok}
      | #{action := get_items, node_id := node_id(),
          item_ids => item_ids(),
          result => [item()]}
      | #{action := publish, node_id := node_id(), item_id := item_id(), payload := item_payload(),
          config => node_config(),
          result => item_id()}.

-export_type([item/0, pubsub_node/0, subscription/0, node_key/0, node_id/0,
              item_id/0, item_payload/0, access_model/0, node_config/0]).

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
     {disco_sm_identity, HostType, fun ?MODULE:disco_sm_identity/3, #{}, 75},
     {disco_sm_features, HostType, fun ?MODULE:disco_sm_features/3, #{}, 75},
     {disco_sm_items, HostType, fun ?MODULE:disco_sm_items/3, #{}, 75},
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
        true ?= is_user_jid(To), % Only PEP is handled now
        IQ = #iq{} ?= jlib:iq_query_info(Stanza),
        #{} ?= Action = mod_pubsub_xml:iq_action(IQ),
        PreparedAcc = prepare_acc(Action, C2SData, Acc),
        Request = #{acc => PreparedAcc, iq => IQ, from_jid => From, service_jid => To, c2s_data => C2SData},
        ?LOG_DEBUG(#{what => pubsub_iq_action, request => Request, action => Action}),
        handle_action(Action, Request)
    else
        _ -> {ok, Acc}
    end.

-spec prepare_acc(iq_action(), mongoose_c2s:data(), mongoose_acc:t()) -> mongoose_acc:t().
prepare_acc(#{action := defer}, C2SData, Acc) ->
    mod_presence:put_state_in_acc(C2SData, Acc);
prepare_acc(#{}, _C2SData, Acc) ->
    Acc.

-spec handle_action(iq_action(), iq_request()) -> mongoose_c2s_hooks:result().
handle_action(#{action := defer}, #{acc := Acc}) ->
    {ok, Acc};
handle_action(Action, Request = #{acc := Acc}) ->
    reply(Request, mod_pubsub_xml:make_reply(try_action(Request, Action))),
    {stop, Acc}.

-spec is_user_jid(jid:jid()) -> boolean().
is_user_jid(#jid{luser = ~""}) -> false;
is_user_jid(#jid{}) -> true.

-spec caps_recognised(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
caps_recognised(Acc, #{c2s_data := C2SData, features := Features}, _Extra) ->
    SubscriberJid = mongoose_acc:from_jid(Acc),
    [send_last_items(host_type(OwnerJid), jid:to_bare(OwnerJid), SubscriberJid, Features)
     || OwnerJid <- subscriptions(s_to, C2SData)],
    {ok, Acc}.

-spec disco_sm_identity(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_disco:identity_acc().
disco_sm_identity(Acc = #{to_jid := ToJid = #jid{lresource = ~""}, node := ~""}, _, _) ->
    case ejabberd_auth:does_user_exist(ToJid) of
        true -> {ok, mongoose_disco:add_identities([pep_identity()], Acc)};
        false -> {ok, Acc}
    end;
disco_sm_identity(Acc, _, _) ->
    {ok, Acc}.

-spec disco_sm_features(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_disco:feature_acc().
disco_sm_features(Acc = #{to_jid := ToJid = #jid{lresource = ~""}, node := ~""}, _, _) ->
    case ejabberd_auth:does_user_exist(ToJid) of
        true -> {ok, mongoose_disco:add_features(pep_disco_features(), Acc)};
        false -> {ok, Acc}
    end;
disco_sm_features(Acc, _, _) ->
    {ok, Acc}.

-spec disco_sm_items(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_disco:item_acc().
disco_sm_items(Acc = #{host_type := HostType, from_jid := FromJid,
                       to_jid := ToJid = #jid{lresource = ~""}, node := ~""}, _, _) ->
    case ejabberd_auth:does_user_exist(ToJid) of
        true -> {ok, mongoose_disco:add_items(disco_items(HostType, ToJid, FromJid, Acc), Acc)};
        false -> {ok, Acc}
    end;
disco_sm_items(Acc, _, _) ->
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
               Action = #{action := create, node_id := NodeId, config := Config}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        ok ?= assert_node_does_not_exist(HostType, NodeKey),
        Node = #pubsub_node{node_key = NodeKey, config = Config},
        mod_pubsub_backend:set_node(HostType, Node),
        Action#{result => ok}
    end;
perform_action(#{acc := Acc, service_jid := ServiceJid} = Request,
               Action = #{action := get_configuration, node_id := NodeId}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        {ok, Node} ?= get_node(HostType, NodeKey),
        Action#{result => Node}
    end;
perform_action(#{acc := Acc, service_jid := ServiceJid} = Request,
               Action = #{action := configure, node_id := NodeId, config := Config}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        {ok, Node} ?= get_node(HostType, NodeKey),
        mod_pubsub_backend:set_node(HostType, apply_node_config(Node, Config)),
        Action#{result => ok}
    end;
perform_action(#{acc := Acc, service_jid := ServiceJid} = Request,
               Action = #{action := delete, node_id := NodeId}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        {ok, Node} ?= get_node(HostType, NodeKey),
        Recipients = recipient_jids(Node, Request),
        mod_pubsub_backend:delete_node(HostType, NodeKey),
        broadcast_node_deletion(HostType, ServiceJid, Recipients, NodeId),
        Action#{result => ok}
    end;
perform_action(#{service_jid := ServiceJid} = Request,
               Action = #{action := subscribe, node_id := NodeId, subscriber_jid := SubscriberJid}) ->
    maybe
        NodeKey = {ServiceJid, NodeId},
        ok ?= subscribe(Request, SubscriberJid, NodeKey),
        Action#{result => ok}
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
        Items = get_items(HostType, NodeKey, Action),
        Action#{result => Items}
    end;
perform_action(#{acc := Acc, from_jid := PublisherJid, service_jid := ServiceJid} = Request,
               Action = #{action := publish, node_id := NodeId, item_id := ItemId, payload := Payload}) ->
    maybe
        ok ?= assert_owner(Request),
        HostType = mongoose_acc:host_type(Acc),
        NodeKey = {ServiceJid, NodeId},
        {ok, Node} ?= get_or_create_node(HostType, NodeKey, maps:get(config, Action, #{})),
        Item = #item{node_key = NodeKey, id = ItemId, publisher_jid = PublisherJid, payload = Payload},
        mod_pubsub_backend:set_item(HostType, Item),
        broadcast_item(HostType, jid:to_bare(PublisherJid), recipient_jids(Node, Request), Item),
        Action#{result => ItemId}
    end.

-spec subscribe(iq_request(), jid:jid(), node_key()) -> ok_result().
subscribe(#{acc := Acc} = Request, SubscriberJid, NodeKey) ->
    HostType = mongoose_acc:host_type(Acc),
    maybe
        ok ?= assert_subscriber_jid(Request, SubscriberJid),
        {ok, Node = #pubsub_node{}} ?= get_node(HostType, NodeKey),
        ok ?= assert_subscribe_permission(Node, Request),
        Subscription = #subscription{node_key = NodeKey, jid = SubscriberJid},
        mod_pubsub_backend:set_subscription(mongoose_acc:host_type(Acc), Subscription)
    end.

-spec unsubscribe(iq_request(), jid:jid(), node_key()) -> ok_result().
unsubscribe(#{acc := Acc} = Request, SubscriberJid, NodeKey) ->
    HostType = mongoose_acc:host_type(Acc),
    maybe
        {ok, #pubsub_node{}} ?= get_node(HostType, NodeKey),
        ok ?= assert_can_unsubscribe(Request, SubscriberJid),
        delete_subscription(HostType, NodeKey, SubscriberJid)
    end.

-spec assert_node_does_not_exist(mongooseim:host_type(), node_key()) -> ok_result().
assert_node_does_not_exist(HostType, NodeKey) ->
    case mod_pubsub_backend:get_node(HostType, NodeKey) of
        undefined -> ok;
        _ -> {error, conflict}
    end.

-spec get_node(mongooseim:host_type(), node_key()) -> result(pubsub_node()).
get_node(HostType, NodeKey) ->
    case mod_pubsub_backend:get_node(HostType, NodeKey) of
        undefined -> {error, item_not_found};
        Node -> {ok, Node}
    end.

-spec get_item(mongooseim:host_type(), node_key(), item_id()) -> [item()].
get_item(HostType, NodeKey, ItemId) ->
    case mod_pubsub_backend:get_item(HostType, NodeKey, ItemId) of
        undefined -> [];
        Item -> [Item]
    end.

-spec get_items(mongooseim:host_type(), node_key(), iq_action()) -> [item()].
get_items(HostType, NodeKey, #{item_ids := ItemIds}) ->
    lists:flatmap(fun(ItemId) -> get_item(HostType, NodeKey, ItemId) end, ItemIds);
get_items(HostType, NodeKey, #{}) ->
    mod_pubsub_backend:get_items(HostType, NodeKey).

-spec assert_retrieve_permission(pubsub_node(), iq_request()) -> ok_result().
assert_retrieve_permission(Node, Request) ->
    case is_owner(Request) of
        true -> ok;
        false -> assert_subscribe_permission(Node, Request)
    end.

-spec assert_subscribe_permission(pubsub_node(), iq_request()) -> ok_result().
assert_subscribe_permission(#pubsub_node{node_key = {OwnerJid, _}, config = Config}, Request) ->
    case maps:get(access_model, Config) of
        open -> ok;
        presence -> assert_subscribed_to(OwnerJid, Request)
    end.

-spec assert_subscriber_jid(iq_request(), jid:jid()) -> ok_result().
assert_subscriber_jid(#{from_jid := FromJid}, SubscriberJid) ->
    case jid:are_bare_equal(FromJid, SubscriberJid) of
        true -> ok;
        false -> {error, {bad_request, ~"invalid-jid"}}
    end.

-spec assert_can_unsubscribe(iq_request(), jid:jid()) -> ok_result().
assert_can_unsubscribe(#{from_jid := FromJid}, SubscriberJid) ->
    case jid:are_bare_equal(FromJid, SubscriberJid) of
        true -> ok;
        false -> {error, forbidden}
    end.

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

-spec delete_subscription(mongooseim:host_type(), node_key(), jid:jid()) -> ok_result().
delete_subscription(HostType, NodeKey, SubscriberJid) ->
    case mod_pubsub_backend:delete_subscription(HostType, NodeKey, SubscriberJid) of
        ok -> ok;
        not_found -> {error, {unexpected_request, ~"not-subscribed"}}
    end.

-spec is_owner(iq_request()) -> boolean().
is_owner(#{from_jid := FromJid, service_jid := ServiceJid}) ->
    jid:are_bare_equal(FromJid, ServiceJid).

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
explicit_subscribers(#pubsub_node{node_key = NodeKey, config = Config}, Acc) ->
    case maps:get(access_model, Config) of
        open ->
            HostType = mongoose_acc:host_type(Acc),
            [Jid || #subscription{jid = Jid} <- mod_pubsub_backend:get_subscriptions(HostType, NodeKey)];
        presence ->
            []
    end.

-spec get_or_create_node(mongooseim:host_type(), node_key(), node_config()) -> result(pubsub_node()).
get_or_create_node(HostType, NodeKey, Config) ->
    case mod_pubsub_backend:get_node(HostType, NodeKey) of
        undefined ->
            Node = #pubsub_node{node_key = NodeKey, config = default_node_config()},
            mod_pubsub_backend:set_node(HostType, apply_node_config(Node, Config)),
            {ok, Node};
        Node = #pubsub_node{config = ExistingConfig} ->
            case maps:with(maps:keys(Config), ExistingConfig) of
                Config -> {ok, Node};
                _ -> {error, {conflict, ~"precondition-not-met"}}
            end
    end.

-spec default_node_config() -> node_config().
default_node_config() ->
    #{access_model => presence}.

-spec reply(iq_request(), {result | error, [exml:child()]}) -> ok.
reply(#{acc := Acc, iq := IQ, from_jid := FromJid, service_jid := ServiceJid}, {IQType, Els}) ->
    Reply = jlib:iq_to_xml(IQ#iq{type = IQType, sub_el = Els}),
    ejabberd_router:route(ServiceJid, FromJid, Acc, Reply),
    ok.

-spec apply_node_config(pubsub_node(), node_config()) -> pubsub_node().
apply_node_config(Node, Config) ->
    Node#pubsub_node{config = maps:merge(Node#pubsub_node.config, Config)}.

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
broadcast_item(HostType, FromJid, ToJids, Item) ->
    Notification = mod_pubsub_xml:notification_message_el(Item),
    broadcast_notification(HostType, FromJid, ToJids, Notification).

-spec broadcast_node_deletion(mongooseim:host_type(), jid:jid(), [jid:jid()], node_id()) -> ok.
broadcast_node_deletion(_HostType, _FromJid, [], _Item) ->
    ok;
broadcast_node_deletion(HostType, FromJid, ToJids, NodeId) ->
    Notification = mod_pubsub_xml:deletion_notification_message_el(NodeId),
    broadcast_notification(HostType, FromJid, ToJids, Notification).

-spec broadcast_notification(mongooseim:host_type(), jid:jid(), [jid:jid()], exml:element()) -> ok.
broadcast_notification(HostType, FromJid, ToJids, Notification) ->
    lists:foreach(fun(ToJid) ->
                          route_notification(HostType, FromJid, ToJid, Notification)
                  end, ToJids).

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

-spec disco_items(mongooseim:host_type(), jid:jid(), jid:jid(), mongoose_disco:item_acc()) ->
    [mongoose_disco:item()].
disco_items(HostType, ServiceJid, FromJid, Acc) ->
    [#{jid => jid:to_binary(ServiceJid), node => NodeId}
     || Node = #pubsub_node{node_key = {_, NodeId}} <- mod_pubsub_backend:get_nodes(HostType, ServiceJid),
        can_discover_node(Node, FromJid, Acc)].

-spec can_discover_node(pubsub_node(), jid:jid(), mongoose_disco:item_acc()) -> boolean().
can_discover_node(#pubsub_node{node_key = {OwnerJid, _},
                               config = #{access_model := AccessModel}}, FromJid, Acc) ->
    jid:are_bare_equal(OwnerJid, FromJid)
        orelse AccessModel =:= open
        orelse has_presence_subscription(OwnerJid, Acc).

-spec has_presence_subscription(jid:jid(), mongoose_disco:item_acc()) -> boolean().
has_presence_subscription(OwnerJid, #{acc := Acc}) ->
    case mongoose_acc:get(mod_presence, presences_state, undefined, Acc) of
        undefined -> false;
        PresencesState -> mod_presence:is_subscribed(PresencesState, jid:to_bare(OwnerJid), to)
    end;
has_presence_subscription(_, _) ->
    false.

-spec pep_identity() -> mongoose_disco:identity().
pep_identity() ->
    #{category => ~"pubsub", type => ~"pep"}.

-spec pep_disco_features() -> [mongoose_disco:feature()].
pep_disco_features() ->
    [?NS_PUBSUB | [pubsub_feature(Feature) || Feature <- pep_features()]].

-spec pep_features() -> [binary()].
pep_features() ->
    [~"access-open",
     ~"access-presence",
     ~"auto-create",
     ~"config-node",
     ~"create-and-configure",
     ~"create-nodes",
     ~"delete-nodes",
     ~"filtered-notifications",
     ~"item-ids",
     ~"persistent-items",
     ~"publish",
     ~"publish-options",
     ~"retrieve-items",
     ~"subscribe"].

-spec pubsub_feature(binary()) -> mongoose_disco:feature().
pubsub_feature(Feature) ->
    <<(?NS_PUBSUB)/binary, "#", Feature/binary>>.
