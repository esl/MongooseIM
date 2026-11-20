-module(mongoose_graphql_pep_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [make_error/2, null_to_default/2]).
-import(mongoose_graphql_pep_helper, [config_to_map/1]).

execute(#{user := OwnerJid}, _Obj, ~"createNode", #{~"node" := NodeId, ~"config" := Config}) ->
    case mod_pubsub_api:create_node(OwnerJid, NodeId, config_to_map(Config)) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid), node => NodeId})
    end;
execute(#{user := OwnerJid}, _Obj, ~"deleteNode", #{~"node" := NodeId}) ->
    case mod_pubsub_api:delete_node(OwnerJid, NodeId) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid), node => NodeId})
    end;
execute(#{user := OwnerJid}, _Obj, ~"configure", #{~"node" := NodeId, ~"config" := Config}) ->
    case mod_pubsub_api:configure(OwnerJid, NodeId, config_to_map(Config)) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid), node => NodeId})
    end;
execute(#{user := OwnerJid}, _Obj, ~"publishItem",
        #{~"node" := NodeId, ~"itemId" := ItemId, ~"payload" := Payload,
          ~"publishOptions" := PublishOptions}) ->
    Config = config_to_map(PublishOptions),
    case mod_pubsub_api:publish_item(OwnerJid, NodeId, ItemId, Payload, Config) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid),
                                     node => NodeId, itemId => ItemId})
    end;
execute(#{user := OwnerJid}, _Obj, ~"retractItem",
        #{~"node" := NodeId, ~"itemId" := ItemId, ~"notify" := Notify}) ->
    case mod_pubsub_api:retract_item(OwnerJid, NodeId, ItemId, null_to_default(Notify, false)) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid),
                                     node => NodeId, itemId => ItemId})
    end.
