-module(mongoose_graphql_pep_helper).

-export([config_to_map/1, make_config/1]).

-spec config_to_map(null | map()) -> mod_pubsub:node_config().
config_to_map(null) ->
    #{};
config_to_map(Options) ->
    #{config_key(Key) => Value || Key := Value <- Options, Value =/= null}.

-spec make_config(mod_pubsub:node_config()) -> map().
make_config(#{access_model := AccessModel, max_items := MaxItems}) ->
    #{~"accessModel" => AccessModel, ~"maxItems" => MaxItems}.

config_key(~"accessModel") -> access_model;
config_key(~"maxItems") -> max_items.
