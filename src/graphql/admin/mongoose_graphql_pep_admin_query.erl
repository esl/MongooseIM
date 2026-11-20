-module(mongoose_graphql_pep_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).
-import(mongoose_graphql_pep_helper, [make_config/1]).

execute(_Ctx, _Obj, ~"getConfiguration", #{~"owner" := OwnerJid, ~"node" := NodeId}) ->
    case mod_pubsub_api:get_configuration(OwnerJid, NodeId) of
        {ok, Config} -> {ok, make_config(Config)};
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid), node => NodeId})
    end;
execute(_Ctx, _Obj, ~"getItem",
        #{~"owner" := OwnerJid, ~"node" := NodeId, ~"itemId" := ItemId}) ->
    case mod_pubsub_api:get_item(OwnerJid, NodeId, ItemId) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid),
                                     node => NodeId, itemId => ItemId})
    end.
