-module(mod_pubsub_xml).

-export([pubsub_error_el/1,
         pubsub_el/1,
         subscription_el/4,
         items_el/2,
         item_el/1,
         published_item_el/1,
         notification_message_el/1]).

-include_lib("exml/include/exml.hrl").
-include("mongoose_ns.hrl").
-include("mod_pubsub.hrl").

-spec pubsub_error_el(binary()) -> exml:element().
pubsub_error_el(Reason) ->
    #xmlel{name = Reason, attrs = #{~"xmlns" => ?NS_PUBSUB_ERRORS}}.

-spec pubsub_el([exml:child()]) -> exml:element().
pubsub_el(Children) ->
    #xmlel{name = ~"pubsub", attrs = #{~"xmlns" => ?NS_PUBSUB}, children = Children}.

-spec subscription_el(jid:jid(), mod_pubsub:node_id(), mod_pubsub:subscription_id(), binary()) ->
    exml:element().
subscription_el(SubscriberJid, NodeId, SubscriptionId, Subscription) ->
    #xmlel{name = ~"subscription",
           attrs = #{~"jid" => jid:to_binary(SubscriberJid),
                     ~"node" => NodeId,
                     ~"subid" => SubscriptionId,
                     ~"subscription" => Subscription}}.

-spec items_el(mod_pubsub:node_id(), [exml:child()]) -> exml:element().
items_el(NodeId, Children) ->
    #xmlel{name = ~"items", attrs = #{~"node" => NodeId}, children = Children}.

-spec item_el(mod_pubsub:item()) -> exml:element().
item_el(#item{id = ItemId, payload = Payload}) ->
    #xmlel{name = ~"item", attrs = #{~"id" => ItemId}, children = Payload}.

-spec published_item_el(mod_pubsub:item_id()) -> exml:element().
published_item_el(ItemId) ->
    #xmlel{name = ~"item", attrs = #{~"id" => ItemId}}.

-spec notification_message_el(mod_pubsub:item()) -> exml:element().
notification_message_el(#item{node_key = {_, NodeId}, id = ItemId, payload = Payload}) ->
    #xmlel{name = ~"message",
           attrs = #{~"type" => ~"headline"},
           children = [#xmlel{name = ~"event",
                              attrs = #{~"xmlns" => ?NS_PUBSUB_EVENT},
                              children = [items_el(NodeId, [#xmlel{name = ~"item",
                                                                    attrs = #{~"id" => ItemId},
                                                                    children = Payload}])]}]}.
