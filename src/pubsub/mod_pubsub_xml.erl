-module(mod_pubsub_xml).
-moduledoc "XML element parsing and construction for mod_pubsub".

%% XML Parsing
-export([iq_action/1,
         is_iq_relevant/1]).

%% XML Construction
-export([make_reply/1,
         deletion_notification_message_el/1,
         retraction_notification_message_el/2,
         notification_message_el/2]).

-include_lib("exml/include/exml.hrl").
-include("jlib.hrl").
-include("mongoose_ns.hrl").
-include("mod_pubsub.hrl").

-type node_config_field() :: {access_model, mod_pubsub:access_model()}.

%% XML Parsing

-spec is_iq_relevant(jlib:iq()) -> boolean().
is_iq_relevant(#iq{xmlns = NS}) ->
    NS =:= ?NS_PUBSUB orelse NS =:= ?NS_PUBSUB_OWNER.

-spec iq_action(jlib:iq()) -> mod_pubsub:iq_action() | mod_pubsub:error_result().
iq_action(#iq{type = IQType, xmlns = NS, sub_el = #xmlel{name = ~"pubsub", children = Children}}) ->
    iq_action(IQType, NS, jlib:remove_cdata(Children));
iq_action(_) ->
    {error, bad_request}.

-spec iq_action(get | set, binary(), [exml:element()]) ->
          mod_pubsub:iq_action() | mod_pubsub:error_result().
iq_action(set, ?NS_PUBSUB, Children) ->
    iq_pubsub_set(Children);
iq_action(get, ?NS_PUBSUB, Children) ->
    iq_pubsub_get(Children);
iq_action(set, ?NS_PUBSUB_OWNER, Children) ->
    iq_pubsub_owner_set(Children);
iq_action(get, ?NS_PUBSUB_OWNER, Children) ->
    iq_pubsub_owner_get(Children).

-spec iq_pubsub_set([exml:element()]) -> mod_pubsub:iq_action() | mod_pubsub:error_result().
iq_pubsub_set([#xmlel{name = ~"create", attrs = #{~"node" := NodeId}} | Rest]) ->
    maybe
        {ok, Config} ?= parse_create_config(Rest),
        #{action => create, node_id => NodeId, config => Config}
    end;
iq_pubsub_set([#xmlel{name = ~"create"}]) ->
    {error, {not_acceptable, ~"nodeid-required"}};
iq_pubsub_set([#xmlel{name = ~"subscribe",
                      attrs = #{~"node" := NodeId, ~"jid" := SubscriberJidBin}}]) ->
    #{action => subscribe, node_id => NodeId, subscriber_jid => jid:from_binary(SubscriberJidBin)};
iq_pubsub_set([#xmlel{name = ~"unsubscribe",
                      attrs = #{~"node" := NodeId, ~"jid" := SubscriberJidBin}}]) ->
    #{action => unsubscribe, node_id => NodeId, subscriber_jid => jid:from_binary(SubscriberJidBin)};
iq_pubsub_set([#xmlel{name = ~"retract", attrs = #{~"node" := NodeId} = Attrs,
                      children = Children}]) ->
    maybe
        {ok, ItemId} ?= parse_retract_item(Children),
        {ok, Notify} ?= parse_flag(~"notify", Attrs, false),
        #{action => retract, node_id => NodeId, item_id => ItemId, notify => Notify}
    end;
iq_pubsub_set([#xmlel{name = ~"retract"}]) ->
    {error, {bad_request, ~"nodeid-required"}};
iq_pubsub_set([El = #xmlel{name = ~"publish", attrs = #{~"node" := NodeId}} | Rest]) ->
    maybe
        {ok, Config} ?= parse_publish_options(Rest),
        {ok, ItemOpts} ?= parse_item(El#xmlel.children),
        ItemOpts#{action => publish, node_id => NodeId, config => Config}
    end;
iq_pubsub_set(_) ->
    {error, bad_request}.

-spec parse_retract_item([exml:child()]) -> mod_pubsub:result(mod_pubsub:item_id()).
parse_retract_item([#xmlel{name = ~"item", attrs = #{~"id" := ItemId}, children = []}]) ->
    {ok, ItemId};
parse_retract_item([#xmlel{name = ~"item", attrs = #{~"id" := _ItemId}}]) ->
    {error, bad_request}; % XEP-0060 7.2.1 the <item/> element MUST be empty
parse_retract_item([#xmlel{name = ~"item"}]) ->
    {error, {bad_request, ~"item-required"}}; % XEP-0060 7.2.3.4 ItemID Required
parse_retract_item([]) ->
    {error, {bad_request, ~"item-required"}}; % XEP-0060 7.2.3.4 Item Required
parse_retract_item(_) ->
    {error, bad_request}.

-spec iq_pubsub_get([exml:element()]) -> mod_pubsub:iq_action() | mod_pubsub:error_result().
iq_pubsub_get([#xmlel{name = ~"items", attrs = #{~"node" := NodeId} = Attrs,
                      children = Children}]) ->
    maybe
        {ok, Opts} ?= parse_max_items(Attrs),
        get_items_action(NodeId, Opts, Children)
    end;
iq_pubsub_get(_) ->
    {error, bad_request}.

-spec get_items_action(mod_pubsub:node_id(), #{max_items => mod_pubsub:max_items()} | #{},
                       [exml:child()]) ->
    mod_pubsub:iq_action() | mod_pubsub:error_result().
get_items_action(NodeId, Opts, []) ->
    Opts#{action => get_items, node_id => NodeId};
get_items_action(_NodeId, #{max_items := _}, _Children) ->
    {error, bad_request};
get_items_action(NodeId, #{}, Children) ->
    case parse_item_ids(Children) of
        ItemIds when length(ItemIds) =:= length(Children) ->
            #{action => get_items, node_id => NodeId, item_ids => ItemIds};
        _ ->
            {error, bad_request}
    end.

-spec iq_pubsub_owner_set([exml:element()]) -> mod_pubsub:iq_action() | mod_pubsub:error_result().
iq_pubsub_owner_set([#xmlel{name = ~"delete", attrs = #{~"node" := NodeId}}]) ->
    #{action => delete, node_id => NodeId};
iq_pubsub_owner_set([ConfigureEl = #xmlel{name = ~"configure",
                                          attrs = #{~"node" := NodeId}}]) ->
    maybe
        {ok, Config} ?= parse_configure_config(ConfigureEl),
        #{action => configure, node_id => NodeId, config => Config}
    end;
iq_pubsub_owner_set([#xmlel{name = Name}]) when Name =:= ~"configure" ->
    {error, {bad_request, ~"nodeid-required"}};
iq_pubsub_owner_set(_) ->
    {error, bad_request}.

-spec iq_pubsub_owner_get([exml:element()]) -> mod_pubsub:iq_action() | mod_pubsub:error_result().
iq_pubsub_owner_get([#xmlel{name = ~"configure", attrs = #{~"node" := NodeId}}]) ->
    #{action => get_configuration, node_id => NodeId};
iq_pubsub_owner_get([#xmlel{name = Name}]) when Name =:= ~"configure" ->
    {error, {bad_request, ~"nodeid-required"}};
iq_pubsub_owner_get(_) ->
    {error, bad_request}.

-spec parse_create_config([exml:element()]) -> mod_pubsub:result(mod_pubsub:node_config()).
parse_create_config([ConfigureEl = #xmlel{name = ~"configure"}]) ->
    parse_configure_config(ConfigureEl);
parse_create_config([]) ->
    {ok, #{access_model => presence}};
parse_create_config(_) ->
    {error, bad_request}.

-spec parse_configure_config(exml:element()) -> mod_pubsub:result(mod_pubsub:node_config()).
parse_configure_config(ConfigureEl) ->
    parse_form_config(ConfigureEl, ?NS_PUBSUB_NODE_CONFIG).

-spec parse_publish_options([exml:element()]) -> mod_pubsub:result(mod_pubsub:node_config()).
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

-spec parse_item([exml:child()]) ->
    mod_pubsub:result(#{item_id := mod_pubsub:item_id(), payload := mod_pubsub:item_payload()}).
parse_item([Item = #xmlel{name = ~"item", children = Children}]) ->
    maybe
        {ok, PayloadEl} ?= parse_payload(Children),
        {ok, #{item_id => get_or_generate_item_id(Item), payload => PayloadEl}}
    end;
parse_item([]) ->
    {error, {bad_request, ~"item-required"}};
parse_item(_) ->
    {error, bad_request}.

get_or_generate_item_id(#xmlel{attrs = #{~"id" := ItemId}}) ->
    ItemId;
get_or_generate_item_id(_) ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

-spec parse_payload([exml:child()]) -> {ok, exml:element()} | mod_pubsub:error_result().
parse_payload([#xmlel{} = PayloadEl]) ->
    {ok, PayloadEl};
parse_payload([]) ->
    {error, {bad_request, ~"payload-required"}};
parse_payload(_) ->
    {error, {bad_request, ~"invalid-payload"}}.

-spec parse_item_ids([exml:child()]) -> [mod_pubsub:item_id()].
parse_item_ids(Elements) ->
    [ItemId || #xmlel{name = ~"item", attrs = #{~"id" := ItemId}} <- Elements].

-spec parse_max_items(exml:attrs()) -> {ok, #{max_items => mod_pubsub:max_items()}} |
                                       {ok, #{}} |
                                       mod_pubsub:error_result().
parse_max_items(#{~"max_items" := MaxItemsBin}) ->
    try binary_to_pos_integer(MaxItemsBin) of
        MaxItems -> {ok, #{max_items => MaxItems}}
    catch
        _:_ -> {error, bad_request}
    end;
parse_max_items(#{}) ->
    {ok, #{}}.

-spec binary_to_pos_integer(binary()) -> pos_integer().
binary_to_pos_integer(BinValue) ->
    IntValue = binary_to_integer(BinValue),
    true = IntValue > 0,
    IntValue.

-spec parse_flag(binary(), exml:attrs(), boolean()) -> {ok, boolean()} | mod_pubsub:error_result().
parse_flag(Key, Attrs, Default) ->
    case Attrs of
        #{Key := Value} when Value =:= ~"0"; Value =:= ~"false" -> {ok, false};
        #{Key := Value} when Value =:= ~"1"; Value =:= ~"true" -> {ok, true};
        #{Key := _} -> {error, bad_request};
        #{} -> {ok, Default}
    end.

-spec parse_form_config(exml:element(), binary()) -> mod_pubsub:result(mod_pubsub:node_config()).
parse_form_config(FormEl, NS) ->
    case mongoose_data_forms:find_and_parse_form(FormEl) of
        #{type := ~"submit", kvs := KVs, ns := NS} ->
            parse_node_config(KVs);
        _ ->
            {error, bad_request}
    end.

-spec parse_node_config(mongoose_data_forms:kv_map()) ->
          mod_pubsub:result(mod_pubsub:node_config()).
parse_node_config(KVs) ->
    parse_node_config_fields(maps:to_list(KVs), []).

-spec parse_node_config_fields([{binary(), [binary()]}], [node_config_field()]) ->
          mod_pubsub:result(mod_pubsub:node_config()).
parse_node_config_fields([{Key, Values} | Rest], ParsedKVs) ->
    maybe
        {ok, ParsedKV} ?= parse_node_config_field(Key, Values),
        parse_node_config_fields(Rest, [ParsedKV | ParsedKVs])
    end;
parse_node_config_fields([], ParsedKVs) ->
    {ok, maps:from_list(ParsedKVs)}.

-spec parse_node_config_field(binary(), [binary()]) -> mod_pubsub:result(node_config_field()).
parse_node_config_field(~"pubsub#access_model", Values) ->
    maybe
        {ok, AccessModel} ?= parse_access_model(Values),
        {ok, {access_model, AccessModel}}
    end;
parse_node_config_field(_, _) ->
    {error, bad_request}.

-spec parse_access_model([binary()]) -> mod_pubsub:result(mod_pubsub:access_model()).
parse_access_model([~"open"]) -> {ok, open};
parse_access_model([~"presence"]) -> {ok, presence};
parse_access_model([~"authorize"]) -> {error, {not_acceptable, ~"unsupported-access-model"}};
parse_access_model(_) -> {error, bad_request}.

%% XML Construction

-spec make_reply(mod_pubsub:iq_action() | mod_pubsub:error_result()) ->
          {result | error, [exml:child()]}.
make_reply({error, {GenericReason, PubSubReason}}) ->
    {error, [exml:append_children(error_el(GenericReason), [pubsub_error_el(PubSubReason)])]};
make_reply({error, Reason}) ->
    {error, [error_el(Reason)]};
make_reply(#{action := subscribe, node_id := NodeId, subscriber_jid := SubscriberJid, result := ok}) ->
    {result, [pubsub_el([subscription_el(SubscriberJid, NodeId, ~"subscribed")])]};
make_reply(#{action := _, result := ok}) ->
    {result, []};
make_reply(#{action := get_configuration, result := Node}) ->
    {result, [pubsub_owner_el([configure_el(Node)])]};
make_reply(#{action := get_items, node_id := NodeId, result := Items}) ->
    {result, [pubsub_el([items_el(NodeId, [item_el(Item) || Item <- Items])])]};
make_reply(#{action := publish, node_id := NodeId, result := ItemId}) ->
    {result, [pubsub_el([publish_el(NodeId, [published_item_el(ItemId)])])]}.

-spec error_el(mod_pubsub:generic_error_reason()) -> exml:element().
error_el(bad_request) -> mongoose_xmpp_errors:bad_request();
error_el(conflict) -> mongoose_xmpp_errors:conflict();
error_el(forbidden) -> mongoose_xmpp_errors:forbidden();
error_el(not_acceptable) -> mongoose_xmpp_errors:not_acceptable();
error_el(not_authorized) -> mongoose_xmpp_errors:not_authorized();
error_el(unexpected_request) -> mongoose_xmpp_errors:unexpected_request_cancel();
error_el(item_not_found) -> mongoose_xmpp_errors:item_not_found().

-spec pubsub_error_el(binary()) -> exml:element().
pubsub_error_el(Reason) ->
    #xmlel{name = Reason, attrs = #{~"xmlns" => ?NS_PUBSUB_ERRORS}}.

-spec pubsub_el([exml:child()]) -> exml:element().
pubsub_el(Children) ->
    #xmlel{name = ~"pubsub", attrs = #{~"xmlns" => ?NS_PUBSUB}, children = Children}.

-spec pubsub_owner_el([exml:child()]) -> exml:element().
pubsub_owner_el(Children) ->
    #xmlel{name = ~"pubsub", attrs = #{~"xmlns" => ?NS_PUBSUB_OWNER}, children = Children}.

-spec configure_el(mod_pubsub:pubsub_node()) -> exml:element().
configure_el(Node = #pubsub_node{node_key = {_, NodeId}}) ->
    Fields = configure_fields(Node),
    Form = mongoose_data_forms:form(#{type => ~"form", ns => ?NS_PUBSUB_NODE_CONFIG,
                                      fields => Fields}),
    #xmlel{name = ~"configure", attrs = #{~"node" => NodeId}, children = [Form]}.

-spec configure_fields(mod_pubsub:pubsub_node()) -> [mongoose_data_forms:field()].
configure_fields(#pubsub_node{config = #{access_model := AccessModel}}) ->
    [#{var => ~"pubsub#access_model",
       type => ~"list-single",
       values => [atom_to_binary(AccessModel)],
       options => [~"open", ~"presence"]}].

-spec subscription_el(jid:jid(), mod_pubsub:node_id(), binary()) -> exml:element().
subscription_el(SubscriberJid, NodeId, Subscription) ->
    #xmlel{name = ~"subscription",
           attrs = #{~"jid" => jid:to_binary(SubscriberJid),
                     ~"node" => NodeId,
                     ~"subscription" => Subscription}}.

-spec items_el(mod_pubsub:node_id(), [exml:child()]) -> exml:element().
items_el(NodeId, Children) ->
    #xmlel{name = ~"items", attrs = #{~"node" => NodeId}, children = Children}.

-spec publish_el(mod_pubsub:node_id(), [exml:child()]) -> exml:element().
publish_el(NodeId, Children) ->
    #xmlel{name = ~"publish", attrs = #{~"node" => NodeId}, children = Children}.

-spec item_el(mod_pubsub:item()) -> exml:element().
item_el(#item{id = ItemId, payload = Payload}) ->
    #xmlel{name = ~"item", attrs = #{~"id" => ItemId}, children = [Payload]}.

-spec published_item_el(mod_pubsub:item_id()) -> exml:element().
published_item_el(ItemId) ->
    #xmlel{name = ~"item", attrs = #{~"id" => ItemId}}.

-spec retract_item_el(mod_pubsub:item_id()) -> exml:element().
retract_item_el(ItemId) ->
    #xmlel{name = ~"retract", attrs = #{~"id" => ItemId}}.

-spec deletion_notification_message_el(mod_pubsub:node_id()) -> exml:element().
deletion_notification_message_el(NodeId) ->
    event_message_el(#xmlel{name = ~"delete", attrs = #{~"node" => NodeId}}).

-spec retraction_notification_message_el(mod_pubsub:node_id(), mod_pubsub:item_id()) ->
          exml:element().
retraction_notification_message_el(NodeId, ItemId) ->
    event_message_el(items_el(NodeId, [retract_item_el(ItemId)])).

-spec notification_message_el(mod_pubsub:item(), boolean()) -> exml:element().
notification_message_el(#item{node_key = {_, NodeId}} = Item, Delayed) ->
    DelayEls = delay_elements(Item, Delayed),
    event_message_el(items_el(NodeId, [item_el(Item)]), DelayEls).

-spec event_message_el(exml:element()) -> exml:element().
event_message_el(EventEl) ->
    event_message_el(EventEl, []).

-spec event_message_el(exml:element(), [exml:element()]) -> exml:element().
event_message_el(EventEl, ExtraElements) ->
    #xmlel{name = ~"message",
           attrs = #{~"type" => ~"headline"},
           children = [#xmlel{name = ~"event",
                              attrs = #{~"xmlns" => ?NS_PUBSUB_EVENT},
                              children = [EventEl]} | ExtraElements]}.

-spec delay_elements(mod_pubsub:item(), boolean()) -> [exml:element()].
delay_elements(#item{node_key = {ServiceJid, _}, published_at = PublishedAt}, true) ->
    [delay_el(ServiceJid, PublishedAt)];
delay_elements(_Item, false) ->
    [].

-spec delay_el(jid:jid(), integer()) -> exml:element().
delay_el(ServiceJid, PublishedAt) ->
    Timestamp = calendar:system_time_to_rfc3339(PublishedAt, [{offset, "Z"}, {unit, microsecond}]),
    jlib:timestamp_to_xml(Timestamp, ServiceJid, undefined).
