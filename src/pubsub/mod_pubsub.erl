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
-type access_model() :: open | presence | authorize.
-type node_config() :: #{access_model => access_model()}.
-type error_reason() :: bad_request | {bad_request, binary()}.

-type iq_request() :: #{acc := mongoose_acc:t(),
                        iq := jlib:iq(),
                        from_jid := jid:jid(),
                        service_jid := jid:jid(),
                        c2s_data := mongoose_c2s:data()}.

-type iq_action() :: #{action := error,
                       reason := error_reason()}
                   | #{action := create,
                       node_id := node_id(),
                       config := node_config()}
                    | #{action := publish,
                       node_id => node_id(), item_id => item_id(), payload => item_payload()}.

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
        reply(Request, perform_action(Request, Action)),
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
    lists:foreach(fun(Item) ->
                          filter_and_broadcast(HostType, OwnerJid, RecipientJids, Item)
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

-spec perform_action(iq_request(), iq_action()) -> {result | error, [exml:child()]}.
perform_action(#{}, #{action := error, reason := {bad_request, Reason}}) ->
    {error, [mongoose_xmpp_errors:bad_request(), pubsub_error_el(Reason)]};
perform_action(#{}, #{action := error, reason := bad_request}) ->
    {error, [mongoose_xmpp_errors:bad_request()]};
perform_action(#{acc := Acc, service_jid := ServiceJid},
               #{action := create, node_id := NodeId, config := #{access_model := AccessModel}}) ->
    HostType = mongoose_acc:host_type(Acc),
    Node = #pubsub_node{node_key = {ServiceJid, NodeId}, access_model = AccessModel},
    mod_pubsub_backend:set_node(HostType, Node),
    {result, []};
perform_action(#{acc := Acc, from_jid := PublisherJid, service_jid := ServiceJid,
                 c2s_data := C2SData},
               #{action := publish, node_id := NodeId, item_id := ItemId, payload := Payload}) ->
    HostType = mongoose_acc:host_type(Acc),
    NodeKey = {ServiceJid, NodeId},
    Node = get_or_create_node(HostType, NodeKey),
    Item = #item{node_key = NodeKey, id = ItemId, publisher_jid = PublisherJid, payload = Payload},
    mod_pubsub_backend:set_item(HostType, Item),
    filter_and_broadcast(HostType, jid:to_bare(PublisherJid), subscribers(HostType, Node, C2SData), Item),
    ReplyPubsubEl = #xmlel{name = ~"pubsub",
                           attrs = #{~"xmlns" => ?NS_PUBSUB},
                           children = [#xmlel{name = ~"item", attrs = #{~"id" => ItemId}}]},
    {result, [ReplyPubsubEl]}.

-spec subscribers(pubsub_node(), mongoose_c2s:data()) -> [jid:jid()].
subscribers(_HostType, #pubsub_node{access_model = presence}, C2SData) ->
    subscriptions(s_from, C2SData);
subscribers(HostType, #pubsub_node{node_key = NodeKey, access_model = open}, _C2SData) ->
    Subscriptions = mod_pubsub_backend:get_subscriptions(HostType, NodeKey),
    lists:uniq([Jid || #subscription{jid = Jid} <- Subscriptions]).

-spec get_or_create_node(mongooseim:host_type(), node_key()) -> pubsub_node().
get_or_create_node(HostType, NodeKey) ->
    case mod_pubsub_backend:get_node(HostType, NodeKey) of
        undefined ->
            Node = #pubsub_node{node_key = NodeKey, access_model = presence},
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

-spec pubsub_error_el(binary()) -> exml:element().
pubsub_error_el(Reason) ->
    #xmlel{name = Reason, attrs = #{~"xmlns" => ?NS_PUBSUB_ERRORS}}.

-spec pubsub_action(binary(), [exml:element()]) -> iq_action() | no_action.
%% TODO: support XEP-0060 instant nodes, i.e. <create/> without a node attribute.
pubsub_action(?NS_PUBSUB, [#xmlel{name = ~"create", attrs = #{~"node" := NodeId}} | Rest]) ->
    case parse_create_config(Rest) of
        {ok, Config} ->
            #{action => create, node_id => NodeId, config => Config};
        {error, Reason} ->
            #{action => error, reason => Reason}
    end;
pubsub_action(?NS_PUBSUB, [El = #xmlel{name = ~"publish", attrs = #{~"node" := NodeId}}]) ->
    case exml_query:subelements(El, ~"item") of
        [#xmlel{attrs = #{~"id" := ItemId}, children = Payload}] ->
            #{action => publish, node_id => NodeId, item_id => ItemId, payload => Payload};
        [] ->
            #{action => error, reason => {bad_request, ~"item-required"}};
        [_] ->
            #{action => error, reason => {bad_request, ~"invalid-payload"}}
    end;
pubsub_action(_, _) ->
    no_action.

-spec parse_create_config([exml:element()]) -> {ok, node_config()} | {error, error_reason()}.
parse_create_config([ConfigureEl = #xmlel{name = ~"configure"}]) ->
    case mongoose_data_forms:find_and_parse_form(ConfigureEl) of
        #{type := ~"submit", kvs := KVs} ->
            parse_node_config(KVs);
        #{type := ~"cancel"} ->
            {ok, #{access_model => presence}};
        {error, _} ->
            {error, {bad_request, ~"invalid-form"}};
        _ ->
            {error, {bad_request, ~"invalid-form-type"}}
    end;
parse_create_config([]) ->
    {ok, #{access_model => presence}};
parse_create_config(_) ->
    {error, bad_request}.

-spec parse_node_config(mongoose_data_forms:kv_map()) -> {ok, node_config()} | {error, binary()}.
parse_node_config(KVs) ->
    try
        {ok, maps:from_list([parse_node_config_field(Key, Values) || Key := Values <- KVs])}
    catch
        throw:Reason ->
            {error, Reason}
    end.

-spec parse_node_config_field(binary(), [binary()]) -> {access_model, access_model()}.
parse_node_config_field(~"pubsub#access_model", Values) ->
    {access_model, parse_access_model(Values)};
parse_node_config_field(_, _) ->
    throw(~"invalid-node-config").

-spec parse_access_model([binary()]) -> access_model().
parse_access_model([~"open"]) -> open;
parse_access_model([~"presence"]) -> presence;
parse_access_model([~"authorize"]) -> authorize;
parse_access_model(_) -> throw(~"invalid-access-model").

-spec send_last_items(mongooseim:host_type(), jid:jid(), jid:jid(), [mod_caps:feature()]) -> ok.
send_last_items(HostType, OwnerJid = #jid{lresource = ~""},
                SubscriberJid = #jid{lresource = <<_, _/binary>>}, Features) ->
    [route_notification(HostType, OwnerJid, SubscriberJid, notification_message(Item))
     || Item <- mod_pubsub_backend:get_last_items(HostType, OwnerJid),
        lists:member(notify_feature(Item), Features)],
    ok.

-spec filter_and_broadcast(mongooseim:host_type(), jid:jid(), [jid:jid()], item()) -> ok.
filter_and_broadcast(HostType, OwnerJid = #jid{lresource = ~""}, RecipientJids, Item) ->
    Feature = notify_feature(Item),
    ToFullJids = [RecipientJid#jid{lresource = LRes}
                  || RecipientJid <- RecipientJids,
                     {LRes, Features} <- resources_with_features(RecipientJid),
                     lists:member(Feature, Features)],
    broadcast_item(HostType, OwnerJid, ToFullJids, Item).

-spec broadcast_item(mongooseim:host_type(), jid:jid(), [jid:jid()], item()) -> ok.
broadcast_item(_HostType, _FromJid, [], _Item) ->
    ok;
broadcast_item(HostType, FromJid, ToFullJids, Item) ->
    Notification = notification_message(Item),
    lists:foreach(fun(ToJid) ->
                          route_notification(HostType, FromJid, ToJid, Notification)
                  end, ToFullJids).

-spec notify_feature(item()) -> mod_caps:feature().
notify_feature(#item{node_key = {_, NodeId}}) ->
    <<NodeId/binary, "+notify">>.

-spec subscriptions(s_from | s_to, mongoose_c2s:data()) -> [jid:jid()].
subscriptions(Type, C2SData) ->
    case mongoose_c2s:get_mod_state(C2SData, mod_presence) of
        {ok, State} ->
            maps:keys(mod_presence:get(State, Type));
        _ ->
            []
    end.

-spec resources_with_features(jid:jid()) -> [{jid:lresource(), [mod_caps:feature()]}].
resources_with_features(Jid = #jid{lserver = LServer, lresource = ~""}) ->
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

-spec notification_message(item()) -> exml:element().
notification_message(#item{node_key = {_, NodeId}, id = ItemId, payload = Payload}) ->
    #xmlel{name = ~"message",
           attrs = #{~"type" => ~"headline"},
           children = [#xmlel{name = ~"event",
                              attrs = #{~"xmlns" => ?NS_PUBSUB_EVENT},
                              children = [#xmlel{name = ~"items",
                                                 attrs = #{~"node" => NodeId},
                                                 children = [#xmlel{name = ~"item",
                                                                    attrs = #{~"id" => ItemId},
                                                                    children = Payload}]}]}]}.

-spec host_type(jid:jid()) -> mongooseim:host_type().
host_type(#jid{lserver = LServer}) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    HostType.
