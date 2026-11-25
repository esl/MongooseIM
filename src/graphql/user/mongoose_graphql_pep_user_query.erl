-module(mongoose_graphql_pep_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).
-import(mongoose_graphql_pep_helper, [make_config/1, get_items_opts/2, make_items/1]).

execute(#{user := OwnerJid}, _Obj, ~"getNodes", _Args) ->
    case mod_pubsub_api:get_nodes(OwnerJid) of
        {ok, NodeIds} -> {ok, [{ok, NodeId} || NodeId <- NodeIds]};
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid)})
    end;
execute(#{user := OwnerJid}, _Obj, ~"getConfiguration", #{~"node" := NodeId}) ->
    case mod_pubsub_api:get_configuration(OwnerJid, NodeId) of
        {ok, Config} -> {ok, make_config(Config)};
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid), node => NodeId})
    end;
execute(#{user := OwnerJid}, _Obj, ~"getItems",
        #{~"node" := NodeId, ~"itemIds" := ItemIds, ~"maxItems" := MaxItems}) ->
    maybe
        {ok, Opts} ?= get_items_opts(ItemIds, MaxItems),
        {ok, Items} ?= mod_pubsub_api:get_items(OwnerJid, NodeId, Opts),
        {ok, make_items(Items)}
    else
        Error -> make_error(Error, #{owner => jid:to_binary(OwnerJid), node => NodeId})
    end.
