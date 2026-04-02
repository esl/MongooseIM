-module(mod_pubsub).
-behaviour(gen_mod).

-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

-export([user_send_iq/3,
         roster_out_subscription/3,
         remove_user/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-include("mod_pubsub.hrl").

-type item() :: #item{}.
-type node_key() :: {jid:jid(), node_id()}.
-type node_id() :: binary().
-type item_id() :: binary().
-type item_payload() :: [exml:child()].

-type iq_request() :: #{acc := mongoose_acc:t(),
                        iq := jlib:iq(),
                        from_jid := jid:jid(),
                        service_jid := jid:jid(),
                        c2s_data := mongoose_c2s:data()}.

-type iq_action() :: #{action := error,
                       reason := {atom(), binary()}}
                   | #{action := publish,
                       node_id => node_id(), item_id => item_id(), payload => item_payload()}.

-export_type([item/0, node_key/0, node_id/0, item_id/0, item_payload/0]).

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{user_send_iq, HostType, fun ?MODULE:user_send_iq/3, #{}, 50},
     {roster_out_subscription, HostType, fun ?MODULE:roster_out_subscription/3, #{}, 50},
     {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {anonymous_purge, HostType, fun ?MODULE:remove_user/3, #{}, 50}].

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
        items = #{<<"backend">> => #option{type = atom,
                                           validate = {module, mod_pubsub_backend}}},
        defaults = #{<<"backend">> => rdbms}
    }.

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

-spec perform_action(iq_request(), iq_action()) -> {result | error, [exml:child()]}.
perform_action(#{iq := IQ}, #{action := error, reason := {bad_request, _Reason}}) ->
    {error, [mongoose_xmpp_errors:bad_request(), IQ#iq.sub_el]};
perform_action(#{acc := Acc, from_jid := PublisherJid, service_jid := ServiceJid} = Request,
               #{action := publish, node_id := NodeId, item_id := ItemId, payload := Payload}) ->
    NodeKey = {ServiceJid, NodeId},
    Item = #item{node_key = NodeKey, id = ItemId, publisher_jid = PublisherJid, payload = Payload},
    HostType = mongoose_acc:host_type(Acc),
    mod_pubsub_backend:set_node(HostType, NodeKey),
    mod_pubsub_backend:set_item(HostType, Item),
    broadcast_item(Request, Item),
    ReplyPubsubEl = #xmlel{name = ~"pubsub",
                           attrs = #{~"xmlns" => ?NS_PUBSUB},
                           children = [#xmlel{name = ~"item", attrs = #{~"id" => ItemId}}]},
    {result, [ReplyPubsubEl]}.

-spec reply(iq_request(), {result | error, [exml:child()]}) -> ok.
reply(#{acc := Acc, iq := IQ, from_jid := FromJid, service_jid := ServiceJid}, {IQType, Els}) ->
    Reply = jlib:iq_to_xml(IQ#iq{type = IQType, sub_el = Els}),
    ejabberd_router:route(ServiceJid, FromJid, Acc, Reply),
    ok.

-spec pubsub_action(binary(), [exml:element()]) -> iq_action() | no_action.
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

-spec roster_out_subscription(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
roster_out_subscription(Acc, #{to := SubscriberJid, from := ApproverJid, type := subscribed}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    LastItems = mod_pubsub_backend:get_last_items(HostType, ApproverJid),
    [send_notification(Acc, ApproverJid, SubscriberJid, Item) || Item <- LastItems],
    {ok, Acc};
roster_out_subscription(Acc, _, _) ->
    {ok, Acc}.

-spec remove_user(Acc, gen_hook:hook_params(), gen_hook:extra()) -> {ok, Acc} when
      Acc :: mongoose_acc:t().
remove_user(Acc, #{jid := ServiceJid}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mod_pubsub_backend:delete_nodes(HostType, ServiceJid),
    {ok, Acc}.

send_notification(Acc, FromJid, RecipientJid, Item) ->
    Feature = notify_feature(Item),
    ToFullJids = [RecipientJid#jid{lresource = LRes}
                  || LRes <- resources_to_notify(RecipientJid, Feature)],
    broadcast_item(mongoose_acc:host_type(Acc), FromJid, ToFullJids, Item).

-spec broadcast_item(iq_request(), item()) -> ok.
broadcast_item(#{acc := Acc, c2s_data := C2SData, from_jid := FromJid}, Item) ->
    Feature = notify_feature(Item),
    ToFullJids = [RecipientJid#jid{lresource = LRes}
                  || RecipientJid <- presence_subscribers(C2SData),
                     LRes <- resources_to_notify(RecipientJid, Feature)],
    broadcast_item(mongoose_acc:host_type(Acc), FromJid, ToFullJids, Item).

-spec broadcast_item(mongooseim:host_type(), jid:jid(), [jid:jid()], item()) -> ok.
broadcast_item(_HostType, _FromBareJid, [], _Item) ->
    ok;
broadcast_item(HostType, FromJid, ToFullJids, Item) ->
    FromJidBare = jid:to_bare(FromJid),
    Notification = notification_message(Item),
    lists:foreach(fun(ToJid) ->
                          route_notification(HostType, FromJidBare, ToJid, Notification)
                  end, ToFullJids).

resources_to_notify(RecipientJid, Feature) ->
    [LRes || {LRes, Features} <- resources_with_features(RecipientJid),
             lists:member(Feature, Features)].

notify_feature(#item{node_key = {_, NodeId}}) ->
    <<NodeId/binary, "+notify">>.

-spec presence_subscribers(mongoose_c2s:data()) -> [jid:jid()].
presence_subscribers(C2SData) ->
    case mongoose_c2s:get_mod_state(C2SData, mod_presence) of
        {ok, State} ->
            maps:keys(mod_presence:get(State, s_from));
        _ ->
            []
    end.

-spec resources_with_features(jid:jid()) -> [{jid:lresource(), [binary()]}].
resources_with_features(Jid = #jid{lserver = LServer}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} -> resources_with_features(HostType, Jid);
        {error, not_found} -> []
    end.

-spec resources_with_features(mongooseim:host_type(), jid:jid()) -> [{jid:lresource(), [binary()]}].
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
