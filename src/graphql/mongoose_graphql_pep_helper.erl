-module(mongoose_graphql_pep_helper).

-export([config_to_map/1, make_config/1, get_items_opts/2, make_items/1]).

-include("mod_pubsub.hrl").

-spec config_to_map(null | map()) -> mod_pubsub:node_config().
config_to_map(null) ->
    #{};
config_to_map(Options) ->
    #{config_key(Key) => Value || Key := Value <- Options, Value =/= null}.

-spec make_config(mod_pubsub:node_config()) -> map().
make_config(#{access_model := AccessModel, max_items := MaxItems}) ->
    #{~"accessModel" => AccessModel, ~"maxItems" => MaxItems}.

-spec get_items_opts(null | [mod_pubsub:item_id()], null | pos_integer()) ->
    {ok, mod_pubsub:get_items_opts()} | {bad_request, string()}.
get_items_opts(null, null) ->
    {ok, #{}};
get_items_opts(ItemIds, null) ->
    {ok, #{item_ids => ItemIds}};
get_items_opts(null, MaxItems) ->
    {ok, #{max_items => MaxItems}};
get_items_opts(_ItemIds, _MaxItems) ->
    {bad_request, "itemIds and maxItems cannot be used together"}.

-spec make_items([mod_pubsub:item()]) -> [{ok, map()}].
make_items(Items) ->
    [{ok, #{~"itemId" => ItemId, ~"payload" => Payload}}
     || #item{id = ItemId, payload = Payload} <- Items].

config_key(~"accessModel") -> access_model;
config_key(~"maxItems") -> max_items.
