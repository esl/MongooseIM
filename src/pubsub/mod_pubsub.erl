-module(mod_pubsub).
-behaviour(gen_mod).

-xep([{xep, 60}, {version, "1.25.0"}, {status, partial}]).
-xep([{xep, 163}, {version, "1.2.2"}, {status, partial}]).

-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

-export([user_send_iq/3,
         iq_sm/5,
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
                        service_jid := jid:jid()}.

-type iq_action() ::
        #{action := error, reason := error_reason()}
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
start(HostType, #{iqdisc := IQDisc} = Opts) ->
    mod_pubsub_backend:start(HostType, Opts),
    add_iq_handlers(HostType, IQDisc).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    delete_iq_handlers(HostType),
    mod_pubsub_backend:stop(HostType).

-spec supported_features() -> [gen_mod:module_feature()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{~"backend" => #option{type = atom,
                                        validate = {module, mod_pubsub_backend}},
                  ~"iqdisc" => mongoose_config_spec:iqdisc()},
        defaults = #{~"backend" => rdbms,
                     ~"iqdisc" => no_queue}
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

-spec add_iq_handlers(mongooseim:host_type(), gen_iq_handler:execution_type()) -> ok.
add_iq_handlers(HostType, IQDisc) ->
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_PUBSUB, ejabberd_sm,
                                             fun ?MODULE:iq_sm/5, #{}, IQDisc),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_PUBSUB_OWNER, ejabberd_sm,
                                             fun ?MODULE:iq_sm/5, #{}, IQDisc),
    ok.

-spec delete_iq_handlers(mongooseim:host_type()) -> ok.
delete_iq_handlers(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_PUBSUB, ejabberd_sm),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_PUBSUB_OWNER, ejabberd_sm),
    ok.

-spec user_send_iq(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
          mongoose_c2s_hooks:result().
user_send_iq(Acc, _Args = #{c2s_data := C2SData}, _Extra) ->
    {_From, To, Stanza} = mongoose_acc:packet(Acc),
    maybe
        true ?= is_user_jid(To), % Only PEP is handled now
        IQ = #iq{} ?= jlib:iq_query_info(Stanza),
        true ?= mod_pubsub_xml:is_iq_relevant(IQ),
        Acc1 = mod_presence:put_state_in_acc(C2SData, Acc),
        {ok, Acc1}
    else
        _ -> {ok, Acc}
    end.

-spec iq_sm(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
iq_sm(Acc, From, To, IQ, _Extra) ->
    Action = handle_iq(Acc, From, To, IQ),
    Reply = mod_pubsub_xml:make_reply(Action),
    make_iq_response(Acc, IQ, Reply).

-spec handle_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq()) -> iq_action().
handle_iq(Acc, From, To, IQ) ->
    Action = mod_pubsub_xml:iq_action(IQ),
    Request = #{acc => Acc, iq => IQ, from_jid => From, service_jid => To},
    ?LOG_DEBUG(#{what => pubsub_iq_action, request => Request, action => Action}),
    handle_action_result(perform_action(Request, Action)).

-spec is_user_jid(jid:jid()) -> boolean().
is_user_jid(#jid{luser = ~""}) -> false;
is_user_jid(#jid{}) -> true.

-spec caps_recognised(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
caps_recognised(Acc, #{c2s_data := C2SData, features := Features}, _Extra) ->
    SubscriberJid = mongoose_acc:from_jid(Acc),
    Acc1 = mod_presence:put_state_in_acc(C2SData, Acc),
    [send_last_items(host_type(OwnerJid), jid:to_bare(OwnerJid), SubscriberJid, Features)
     || OwnerJid <- subscriptions(s_to, Acc1)],
    {ok, Acc}.

-spec disco_sm_identity(DiscoAcc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, DiscoAcc} when
      DiscoAcc :: mongoose_disco:identity_acc().
disco_sm_identity(DiscoAcc = #{to_jid := ToJid = #jid{lresource = ~""}, node := ~""}, _, _) ->
    case ejabberd_auth:does_user_exist(ToJid) of
        true -> {ok, mongoose_disco:add_identities([pep_identity()], DiscoAcc)};
        false -> {ok, DiscoAcc}
    end;
disco_sm_identity(DiscoAcc = #{host_type := HostType, from_jid := FromJid,
                               to_jid := ToJid = #jid{lresource = ~""}, node := NodeId,
                               presence_subscribed := PresenceSubscribed},
                  _, _) ->
    maybe
        Node = #pubsub_node{} ?= mod_pubsub_backend:get_node(HostType, {ToJid, NodeId}),
        true ?= can_discover_node(Node, FromJid, PresenceSubscribed),
        {ok, mongoose_disco:add_identities([pep_node_identity()], DiscoAcc)}
    else
        _ -> {ok, DiscoAcc}
    end;
disco_sm_identity(DiscoAcc, _, _) ->
    {ok, DiscoAcc}.

-spec disco_sm_features(DiscoAcc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, DiscoAcc} when
      DiscoAcc :: mongoose_disco:feature_acc().
disco_sm_features(DiscoAcc = #{to_jid := ToJid = #jid{lresource = ~""}, node := ~""}, _, _) ->
    case ejabberd_auth:does_user_exist(ToJid) of
        true -> {ok, mongoose_disco:add_features(pep_disco_features(), DiscoAcc)};
        false -> {ok, DiscoAcc}
    end;
disco_sm_features(DiscoAcc = #{host_type := HostType, from_jid := FromJid,
                               to_jid := ToJid = #jid{lresource = ~""}, node := NodeId,
                               presence_subscribed := PresenceSubscribed},
                  _, _) ->
    maybe
        Node = #pubsub_node{} ?= mod_pubsub_backend:get_node(HostType, {ToJid, NodeId}),
        true ?= can_discover_node(Node, FromJid, PresenceSubscribed),
        {ok, mongoose_disco:add_features(pep_node_disco_features(), DiscoAcc)}
    else
        _ -> {ok, DiscoAcc}
    end;
disco_sm_features(DiscoAcc, _, _) ->
    {ok, DiscoAcc}.

-spec disco_sm_items(DiscoAcc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, DiscoAcc} when
      DiscoAcc :: mongoose_disco:item_acc().
disco_sm_items(DiscoAcc = #{host_type := HostType, from_jid := FromJid,
                            to_jid := ToJid = #jid{lresource = ~""}, node := ~""}, _, _) ->
    case ejabberd_auth:does_user_exist(ToJid) of
        true -> {ok, mongoose_disco:add_items(disco_items(HostType, ToJid, FromJid, DiscoAcc), DiscoAcc)};
        false -> {ok, DiscoAcc}
    end;
disco_sm_items(DiscoAcc, _, _) ->
    {ok, DiscoAcc}.

-spec roster_out_subscription(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
roster_out_subscription(Acc, #{to := SubscriberJid, from := OwnerJid, type := subscribed}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    RecipientJids = [jid:to_bare(SubscriberJid)],
    lists:foreach(fun(Item = #item{node_key = NodeKey}) ->
                          FilteredJids = filter_recipients(RecipientJids, NodeKey),
                          broadcast_item(HostType, OwnerJid, FilteredJids, Item, true)
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

-spec handle_action_result(iq_action() | error_result()) -> iq_action().
handle_action_result({error, Reason}) ->
    #{action => error, reason => Reason};
handle_action_result(Action) ->
    Action.

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
        Node = #pubsub_node{node_key = NodeKey},
        mod_pubsub_backend:set_node(HostType, apply_node_config(Node, Config)),
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
        broadcast_item(HostType, jid:to_bare(PublisherJid), recipient_jids(Node, Request), Item, false),
        Action#{result => ItemId}
    end.

-spec subscribe(iq_request(), jid:jid(), node_key()) -> ok_result().
subscribe(#{acc := Acc} = Request, SubscriberJid, NodeKey) ->
    HostType = mongoose_acc:host_type(Acc),
    maybe
        ok ?= assert_subscriber_jid(Request, SubscriberJid),
        {ok, Node = #pubsub_node{}} ?= get_node(HostType, NodeKey),
        ok ?= assert_subscribe_permission(Node, Request),
        case mod_pubsub_backend:get_subscription(HostType, NodeKey, SubscriberJid) of
            undefined ->
                Subscription = #subscription{node_key = NodeKey, jid = SubscriberJid},
                mod_pubsub_backend:set_subscription(HostType, Subscription),
                route_last_item_if_exists(HostType, NodeKey, SubscriberJid);
            _Subscription ->
                ok % already subscribed
        end
    end.

-spec route_last_item_if_exists(mongooseim:host_type(), node_key(), jid:jid()) -> ok.
route_last_item_if_exists(HostType, NodeKey = {ServiceJid, _}, SubscriberJid) ->
    case mod_pubsub_backend:get_last_item(HostType, NodeKey) of
        undefined ->
            ok;
        Item ->
            Notification = mod_pubsub_xml:notification_message_el(Item, true),
            route_notification(HostType, ServiceJid, SubscriberJid, Notification)
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
assert_subscribed_to(OwnerJid, #{acc := Acc}) ->
    case is_subscribed_to_presence(OwnerJid, Acc) of
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
recipient_jids(Node, #{acc := Acc}) ->
    Subs = lists:usort(implicit_subscribers(Node, Acc) ++ explicit_subscribers(Node, Acc)),
    lists:foldl(fun drop_bare_jid_shadowed_by_full_jid/2, [], Subs).

-spec drop_bare_jid_shadowed_by_full_jid(jid:jid(), [jid:jid()]) -> [jid:jid()].
drop_bare_jid_shadowed_by_full_jid(#jid{luser = U, lserver = S} = FullJid,
                                   [#jid{luser = U, lserver = S, lresource = ~""} | Results]) ->
    [FullJid | Results];
drop_bare_jid_shadowed_by_full_jid(Jid, Results) ->
    [Jid | Results].

-spec implicit_subscribers(pubsub_node(), mongoose_acc:t()) -> [jid:jid()].
implicit_subscribers(#pubsub_node{node_key = NodeKey = {OwnerJid, _}}, Acc) ->
    filter_recipients([OwnerJid | subscriptions(s_from, Acc)], NodeKey).

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
            Node = #pubsub_node{node_key = NodeKey},
            mod_pubsub_backend:set_node(HostType, apply_node_config(Node, Config)),
            {ok, Node};
        Node = #pubsub_node{config = ExistingConfig} ->
            case maps:with(maps:keys(Config), ExistingConfig) of
                Config -> {ok, Node};
                _ -> {error, {conflict, ~"precondition-not-met"}}
            end
    end.

-spec make_iq_response(mongoose_acc:t(), jlib:iq(), {result | error, [exml:child()]}) ->
    {mongoose_acc:t(), jlib:iq()}.
make_iq_response(Acc, IQ, {IQType, Els}) ->
    {Acc, IQ#iq{type = IQType, sub_el = Els}}.

-spec apply_node_config(pubsub_node(), node_config()) -> pubsub_node().
apply_node_config(Node, Config) ->
    Node#pubsub_node{config = maps:merge(Node#pubsub_node.config, Config)}.

-spec send_last_items(mongooseim:host_type(), jid:jid(), jid:jid(), [mod_caps:feature()]) -> ok.
send_last_items(HostType, OwnerJid = #jid{lresource = ~""},
                SubscriberJid = #jid{lresource = <<_, _/binary>>}, Features) ->
    [route_notification(HostType, OwnerJid, SubscriberJid, mod_pubsub_xml:notification_message_el(Item, true))
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

-spec broadcast_item(mongooseim:host_type(), jid:jid(), [jid:jid()], item(), boolean()) -> ok.
broadcast_item(_HostType, _FromJid, [], _Item, _Delayed) ->
    ok;
broadcast_item(HostType, FromJid, ToJids, Item, Delayed) ->
    Notification = mod_pubsub_xml:notification_message_el(Item, Delayed),
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

-spec subscriptions(s_from | s_to, mongoose_acc:t()) -> [jid:jid()].
subscriptions(Type, Acc) ->
    case mod_presence:get_state_from_acc(Acc) of
        undefined -> [];
        State -> maps:keys(mod_presence:get(State, Type))
    end.

-spec resources_with_features(jid:jid()) -> [{jid:lresource(), [mod_caps:feature()]}].
resources_with_features(Jid = #jid{lserver = LServer}) ->
    maybe
        {ok, HostType} ?= mongoose_domain_api:get_domain_host_type(LServer),
        true ?= gen_mod:is_loaded(HostType, mod_caps),
        resources_with_features(HostType, Jid)
    else
        _ -> []
    end.

-spec resources_with_features(mongooseim:host_type(), jid:jid()) ->
    [{jid:lresource(), [mod_caps:feature()]}].
resources_with_features(HostType, Jid = #jid{lresource = ~""}) ->
    mod_caps:get_resources_with_features(HostType, Jid);
resources_with_features(HostType, Jid = #jid{lresource = LResource}) ->
    [{LResource, mod_caps:get_features(HostType, Jid)}].

-spec route_notification(mongooseim:host_type(), jid:jid(), jid:jid(), exml:element()) -> ok.
route_notification(_HostType, FromJid, ToJid, Packet) ->
    Acc = mongoose_acc:new(FromJid, ToJid, Packet, ?LOCATION),
    mongoose_router:route(Acc),
    ok.

-spec host_type(jid:jid()) -> mongooseim:host_type().
host_type(#jid{lserver = LServer}) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    HostType.

-spec disco_items(mongooseim:host_type(), jid:jid(), jid:jid(), mongoose_disco:item_acc()) ->
    [mongoose_disco:item()].
disco_items(HostType, ServiceJid, FromJid, DiscoAcc) ->
    [#{jid => jid:to_binary(ServiceJid), node => NodeId}
     || Node = #pubsub_node{node_key = {_, NodeId}} <- mod_pubsub_backend:get_nodes(HostType, ServiceJid),
        can_discover_node(Node, FromJid, maps:get(presence_subscribed, DiscoAcc, false))].

-spec can_discover_node(pubsub_node(), jid:jid(), boolean()) -> boolean().
can_discover_node(#pubsub_node{node_key = {OwnerJid, _},
                               config = #{access_model := AccessModel}}, FromJid, PresenceSubscribed) ->
    jid:are_bare_equal(OwnerJid, FromJid) orelse AccessModel =:= open orelse PresenceSubscribed.

-spec is_subscribed_to_presence(jid:jid(), mongoose_acc:t()) -> boolean().
is_subscribed_to_presence(OwnerJid = #jid{lresource = ~""}, Acc) ->
    case mod_presence:get_state_from_acc(Acc) of
        undefined -> false;
        PresencesState -> mod_presence:is_subscribed(PresencesState, OwnerJid, to)
    end.

-spec pep_identity() -> mongoose_disco:identity().
pep_identity() ->
    #{category => ~"pubsub", type => ~"pep"}.

-spec pep_node_identity() -> mongoose_disco:identity().
pep_node_identity() ->
    #{category => ~"pubsub", type => ~"leaf"}.

-spec pep_disco_features() -> [mongoose_disco:feature()].
pep_disco_features() ->
    [?NS_PUBSUB | [pubsub_feature(Feature) || Feature <- pep_features()]].

-spec pep_node_disco_features() -> [mongoose_disco:feature()].
pep_node_disco_features() ->
    [?NS_PUBSUB].

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
     ~"last-published",
     ~"persistent-items",
     ~"publish",
     ~"publish-options",
     ~"retrieve-items",
     ~"subscribe"].

-spec pubsub_feature(binary()) -> mongoose_disco:feature().
pubsub_feature(Feature) ->
    <<(?NS_PUBSUB)/binary, "#", Feature/binary>>.
