-module(mongoose_graphql_pep_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

execute(_Ctx, _Obj, ~"publishItem",
        #{~"owner" := OwnerJid, ~"node" := NodeId, ~"itemId" := ItemId,  ~"payload" := Payload}) ->
    case mod_pubsub_api:publish_item(OwnerJid, NodeId, ItemId, Payload) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid),
                                     node => NodeId, itemId => ItemId})
    end;
execute(_Ctx, _Obj, ~"retractItem",
        #{~"owner" := OwnerJid, ~"node" := NodeId, ~"itemId" := ItemId, ~"notify" := Notify}) ->
    case mod_pubsub_api:retract_item(OwnerJid, NodeId, ItemId, Notify) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid),
                                     node => NodeId, itemId => ItemId})
    end.
