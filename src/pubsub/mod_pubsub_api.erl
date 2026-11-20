%% @doc Provide an interface for frontends (like graphql or ctl) to manage PEP nodes and items.
-module(mod_pubsub_api).

-export([create_node/3, delete_node/2, get_configuration/2, configure/3,
         get_items/3, publish_item/5, retract_item/4]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_pubsub.hrl").

-type error_code() :: domain_not_found | user_not_found |
                      mod_pubsub:generic_error_reason().
-type result(T) :: {ok, T} | {error_code(), iolist()}.

-spec create_node(jid:jid(), mod_pubsub:node_id(), mod_pubsub:node_config()) -> result(true).
create_node(OwnerJid, NodeId, Config) ->
    M = #{owner => jid:to_bare(OwnerJid), node_id => NodeId, config => Config},
    fold(M, [fun check_user/1, fun do_create_node/1]).

-spec delete_node(jid:jid(), mod_pubsub:node_id()) -> result(true).
delete_node(OwnerJid, NodeId) ->
    M = #{owner => jid:to_bare(OwnerJid), node_id => NodeId},
    fold(M, [fun check_user/1, fun do_delete_node/1]).

-spec get_configuration(jid:jid(), mod_pubsub:node_id()) -> result(mod_pubsub:node_config()).
get_configuration(OwnerJid, NodeId) ->
    M = #{owner => jid:to_bare(OwnerJid), node_id => NodeId},
    fold(M, [fun check_user/1, fun do_get_configuration/1]).

-spec configure(jid:jid(), mod_pubsub:node_id(), mod_pubsub:node_config()) -> result(true).
configure(OwnerJid, NodeId, Config) ->
    M = #{owner => jid:to_bare(OwnerJid), node_id => NodeId, config => Config},
    fold(M, [fun check_user/1, fun do_configure/1]).

-spec get_items(jid:jid(), mod_pubsub:node_id(), mod_pubsub:get_items_opts()) ->
          result([mod_pubsub:item()]).
get_items(OwnerJid, NodeId, Opts) ->
    M = #{owner => jid:to_bare(OwnerJid), node_id => NodeId, opts => Opts},
    fold(M, [fun check_user/1, fun do_get_items/1]).

-spec publish_item(jid:jid(), mod_pubsub:node_id(), mod_pubsub:item_id(),
                   mod_pubsub:item_payload(), mod_pubsub:node_config()) ->
          result(mod_pubsub:item_id()).
publish_item(OwnerJid, NodeId, ItemId, Payload, Config) ->
    M = #{owner => jid:to_bare(OwnerJid), node_id => NodeId, item_id => ItemId,
          payload => Payload, config => Config},
    fold(M, [fun check_user/1, fun do_publish_item/1]).

-spec retract_item(jid:jid(), mod_pubsub:node_id(), mod_pubsub:item_id(), boolean()) ->
          result(true).
retract_item(OwnerJid, NodeId, ItemId, Notify) ->
    M = #{owner => jid:to_bare(OwnerJid), node_id => NodeId, item_id => ItemId,
          notify => Notify},
    fold(M, [fun check_user/1, fun do_retract_item/1]).

%% Internal: steps used in fold/2

check_user(M = #{owner := OwnerJid = #jid{lserver = LServer}}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case ejabberd_auth:does_user_exist(HostType, OwnerJid, stored) of
                true -> M#{host_type => HostType};
                false -> {user_not_found, "Given user does not exist"}
            end;
        {error, not_found} ->
            {domain_not_found, "User's domain does not exist"}
    end.

do_create_node(M = #{node_id := NodeId, config := Config}) ->
    Action = #{action => create, node_id => NodeId, config => Config},
    case perform_action(M, Action) of
        #{result := ok} -> {ok, true};
        {error, Reason} -> format_error(Reason)
    end.

do_delete_node(M = #{node_id := NodeId}) ->
    Action = #{action => delete, node_id => NodeId},
    case perform_action(M, Action) of
        #{result := ok} -> {ok, true};
        {error, Reason} -> format_error(Reason)
    end.

do_get_configuration(M = #{node_id := NodeId}) ->
    Action = #{action => get_configuration, node_id => NodeId},
    case perform_action(M, Action) of
        #{result := #pubsub_node{config = Config}} -> {ok, Config};
        {error, Reason} -> format_error(Reason)
    end.

do_configure(M = #{node_id := NodeId, config := Config}) ->
    Action = #{action => configure, node_id => NodeId, config => Config},
    case perform_action(M, Action) of
        #{result := ok} -> {ok, true};
        {error, Reason} -> format_error(Reason)
    end.

do_get_items(M = #{node_id := NodeId, opts := Opts}) ->
    Action = #{action => get_items, node_id => NodeId, opts => Opts},
    case perform_action(M, Action) of
        #{result := Items} -> {ok, Items};
        {error, Reason} -> format_error(Reason)
    end.

do_publish_item(M = #{node_id := NodeId, item_id := ItemId, payload := Payload, config := Config}) ->
    Action = #{action => publish, node_id => NodeId, item_id => ItemId, payload => Payload,
               config => Config},
    case perform_action(M, Action) of
        #{result := PublishedItemId} -> {ok, PublishedItemId};
        {error, Reason} -> format_error(Reason)
    end.

do_retract_item(M = #{node_id := NodeId, item_id := ItemId, notify := Notify}) ->
    Action = #{action => retract, node_id => NodeId, item_id => ItemId, notify => Notify},
    case perform_action(M, Action) of
        #{result := ok} -> {ok, true};
        {error, Reason} -> format_error(Reason)
    end.

%% Helpers

perform_action(#{owner := OwnerJid = #jid{lserver = LServer}, host_type := HostType}, Action) ->
    %% No C2S presence state: notify explicit subscribers and eligible owner resources only.
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             host_type => HostType,
                             lserver => LServer}),
    Request = #{acc => Acc, from_jid => OwnerJid, service_jid => OwnerJid},
    mod_pubsub:perform_action(Request, Action).

format_error(item_not_found) ->
    {item_not_found, "PEP node or item not found"};
format_error({Code, Detail}) ->
    {Code, io_lib:format("PEP operation failed: ~p (~ts)", [Code, Detail])};
format_error(Code) ->
    {Code, io_lib:format("PEP operation failed: ~p", [Code])}.

fold({_, _} = Result, _) ->
    Result;
fold(M, [Step | Rest]) when is_map(M) ->
    fold(Step(M), Rest).
