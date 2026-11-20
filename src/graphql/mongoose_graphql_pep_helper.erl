-module(mongoose_graphql_pep_helper).

-export([publish_options_to_config/1]).

-spec publish_options_to_config(null | map()) -> mod_pubsub:node_config().
publish_options_to_config(null) ->
    #{};
publish_options_to_config(Options) ->
    #{config_key(Key) => Value || Key := Value <- Options, Value =/= null}.

config_key(~"accessModel") -> access_model;
config_key(~"maxItems") -> max_items.
