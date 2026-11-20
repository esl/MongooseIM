-module(mongoose_graphql_pep_helper).

-export([config_to_map/1]).

-spec config_to_map(null | map()) -> mod_pubsub:node_config().
config_to_map(null) ->
    #{};
config_to_map(Options) ->
    #{config_key(Key) => Value || Key := Value <- Options, Value =/= null}.

config_key(~"accessModel") -> access_model;
config_key(~"maxItems") -> max_items.
