-module(mod_pubsub_xml).
-moduledoc("XML element parsing and construction for mod_pubsub").

%% XML Parsing
-export([iq_action/1]).

%% XML Construction
-export([pubsub_error_el/1,
         pubsub_el/1,
         pubsub_owner_el/1,
         configure_el/1,
         subscription_el/4,
         items_el/2,
         item_el/1,
         published_item_el/1,
         notification_message_el/1]).

-include_lib("exml/include/exml.hrl").
-include("jlib.hrl").
-include("mongoose_ns.hrl").
-include("mod_pubsub.hrl").

-type error_reason() :: generic_error_reason() | {generic_error_reason(), binary()}.
-type generic_error_reason() ::
        bad_request | conflict | forbidden | not_acceptable | not_authorized |
        unexpected_request | item_not_found.
-type result(T) :: {ok, T} | {error, error_reason()}.
-type iq_action() :: map().
-type node_config_field() :: {access_model, mod_pubsub:access_model()}.

%% XML Parsing

-spec iq_action(jlib:iq()) -> iq_action() | no_action.
iq_action(#iq{type = IQType, xmlns = NS, sub_el = #xmlel{name = ~"pubsub", children = Children}}) ->
    iq_action(IQType, NS, jlib:remove_cdata(Children));
iq_action(_) ->
    no_action.

-spec iq_action(get | set, binary(), [exml:element()]) -> iq_action() | no_action.
iq_action(set, ?NS_PUBSUB, Children) ->
    iq_pubsub_set(Children);
iq_action(get, ?NS_PUBSUB, Children) ->
    iq_pubsub_get(Children);
iq_action(set, ?NS_PUBSUB_OWNER, Children) ->
    iq_pubsub_owner_set(Children);
iq_action(get, ?NS_PUBSUB_OWNER, Children) ->
    iq_pubsub_owner_get(Children);
iq_action(_, _, _) ->
    no_action.

-spec iq_pubsub_set([exml:element()]) -> iq_action().
iq_pubsub_set([#xmlel{name = ~"create", attrs = #{~"node" := NodeId}} | Rest]) ->
    case parse_create_config(Rest) of
        {ok, Config} ->
            #{action => create, node_id => NodeId, config => Config};
        {error, Reason} ->
            #{action => error, reason => Reason}
    end;
iq_pubsub_set([#xmlel{name = ~"create"}]) ->
    #{action => error, reason => {not_acceptable, ~"nodeid-required"}};
iq_pubsub_set([#xmlel{name = ~"subscribe",
                      attrs = #{~"node" := NodeId, ~"jid" := SubscriberJidBin}}]) ->
    #{action => subscribe, node_id => NodeId, subscriber_jid => jid:from_binary(SubscriberJidBin)};
iq_pubsub_set([#xmlel{name = ~"unsubscribe",
                      attrs = #{~"node" := NodeId, ~"jid" := SubscriberJidBin}}]) ->
    #{action => unsubscribe, node_id => NodeId, subscriber_jid => jid:from_binary(SubscriberJidBin)};
iq_pubsub_set([El = #xmlel{name = ~"publish", attrs = #{~"node" := NodeId}} | Rest]) ->
    maybe
        {ok, Config} ?= parse_publish_options(Rest),
        {ok, ItemOpts} ?= parse_item(El#xmlel.children),
        ItemOpts#{action => publish, node_id => NodeId, config => Config}
    else
        {error, Reason} ->
            #{action => error, reason => Reason}
    end;
iq_pubsub_set(_) ->
    #{action => error, reason => bad_request}.

-spec iq_pubsub_owner_set([exml:element()]) -> iq_action().
iq_pubsub_owner_set([#xmlel{name = ~"delete", attrs = #{~"node" := NodeId}}]) ->
    #{action => delete, node_id => NodeId};
iq_pubsub_owner_set([ConfigureEl = #xmlel{name = ~"configure",
                                          attrs = #{~"node" := NodeId}}]) ->
    case parse_configure_config(ConfigureEl) of
        {ok, Config} ->
            #{action => configure, node_id => NodeId, config => Config};
        {error, Reason} ->
            #{action => error, reason => Reason}
    end;
iq_pubsub_owner_set([#xmlel{name = Name}]) when Name =:= ~"configure" ->
    #{action => error, reason => {bad_request, ~"nodeid-required"}};
iq_pubsub_owner_set(_) ->
    #{action => error, reason => bad_request}.

-spec iq_pubsub_get([exml:element()]) -> iq_action().
iq_pubsub_get([#xmlel{name = ~"items", attrs = #{~"node" := NodeId},
                      children = [#xmlel{name = ~"item", attrs = #{~"id" := ItemId}}]}]) ->
    #{action => get_item, node_id => NodeId, item_id => ItemId};
iq_pubsub_get([#xmlel{name = ~"items", attrs = #{~"node" := NodeId}}]) ->
    #{action => get_items, node_id => NodeId};
iq_pubsub_get(_) ->
    #{action => error, reason => bad_request}.

-spec iq_pubsub_owner_get([exml:element()]) -> iq_action().
iq_pubsub_owner_get([#xmlel{name = ~"configure", attrs = #{~"node" := NodeId}}]) ->
    #{action => get_configuration, node_id => NodeId};
iq_pubsub_owner_get([#xmlel{name = Name}]) when Name =:= ~"configure" ->
    #{action => error, reason => {bad_request, ~"nodeid-required"}};
iq_pubsub_owner_get(_) ->
    #{action => error, reason => bad_request}.

-spec parse_create_config([exml:element()]) -> result(mod_pubsub:node_config()).
parse_create_config([ConfigureEl = #xmlel{name = ~"configure"}]) ->
    parse_form_config(ConfigureEl, ?NS_PUBSUB_NODE_CONFIG, #{access_model => presence}, true);
parse_create_config([]) ->
    {ok, #{access_model => presence}};
parse_create_config(_) ->
    {error, bad_request}.

-spec parse_configure_config(exml:element()) -> result(mod_pubsub:node_config()).
parse_configure_config(ConfigureEl) ->
    parse_form_config(ConfigureEl, ?NS_PUBSUB_NODE_CONFIG, #{}, false).

-spec parse_publish_options([exml:element()]) -> result(mod_pubsub:node_config()).
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
    result(#{item_id := mod_pubsub:item_id(), payload := mod_pubsub:item_payload()}).
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

parse_payload([#xmlel{} = PayloadEl]) ->
    {ok, PayloadEl};
parse_payload([]) ->
    {error, {bad_request, ~"payload-required"}};
parse_payload(_) ->
    {error, {bad_request, ~"invalid-payload"}}.

-spec parse_form_config(exml:element(), binary(), mod_pubsub:node_config(), boolean()) ->
    result(mod_pubsub:node_config()).
parse_form_config(FormEl, NS, DefaultConfig, AllowCancel) ->
    case mongoose_data_forms:find_and_parse_form(FormEl) of
        #{type := ~"submit", kvs := KVs, ns := NS} ->
            parse_node_config(KVs);
        #{type := ~"cancel"} when AllowCancel ->
            {ok, DefaultConfig};
        _ ->
            {error, bad_request}
    end.

-spec parse_node_config(mongoose_data_forms:kv_map()) -> result(mod_pubsub:node_config()).
parse_node_config(KVs) ->
    parse_node_config_fields(maps:to_list(KVs), []).

-spec parse_node_config_fields([{binary(), [binary()]}], [node_config_field()]) ->
          result(mod_pubsub:node_config()).
parse_node_config_fields([{Key, Values} | Rest], ParsedKVs) ->
    maybe
        {ok, ParsedKV} ?= parse_node_config_field(Key, Values),
        parse_node_config_fields(Rest, [ParsedKV | ParsedKVs])
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

-spec parse_access_model([binary()]) -> result(mod_pubsub:access_model()).
parse_access_model([~"open"]) -> {ok, open};
parse_access_model([~"presence"]) -> {ok, presence};
parse_access_model([~"authorize"]) -> {error, {not_acceptable, ~"unsupported-access-model"}};
parse_access_model(_) -> {error, bad_request}.

%% XML Construction

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
configure_fields(#pubsub_node{access_model = AccessModel}) ->
    [#{var => ~"pubsub#access_model",
       type => ~"list-single",
       values => [atom_to_binary(AccessModel)],
       options => [~"open", ~"presence"]}].

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
    #xmlel{name = ~"item", attrs = #{~"id" => ItemId}, children = [Payload]}.

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
                                                                    children = [Payload]}])]}]}.
