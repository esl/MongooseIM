%% @doc Provide an interface for frontends (like graphql or ctl) to manage PEP nodes and items.
-module(mod_pubsub_api).

-export([get_nodes/1, create_node/3, delete_node/2, get_configuration/2, configure/3,
         get_items/3, publish_item/5, retract_item/4]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_pubsub.hrl").

-type error_code() :: domain_not_found | user_not_found |
                      mod_pubsub:generic_error_reason().
-type result(T) :: {ok, T} | {error_code(), iolist()}.

-spec get_nodes(jid:jid()) -> result([mod_pubsub:node_id()]).
get_nodes(OwnerJid) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Nodes = mod_pubsub_backend:get_nodes(HostType, jid:to_bare(OwnerJid)),
        {ok, [NodeId || #pubsub_node{node_key = {_, NodeId}} <- Nodes]}
    end.

-spec create_node(jid:jid(), mod_pubsub:node_id(), mod_pubsub:node_config()) -> result(true).
create_node(OwnerJid, NodeId, Config) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Action = #{action => create, node_id => NodeId, config => Config},
        #{result := ok} ?= perform_action(HostType, OwnerJid, Action),
        {ok, true}
    end.

-spec delete_node(jid:jid(), mod_pubsub:node_id()) -> result(true).
delete_node(OwnerJid, NodeId) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Action = #{action => delete, node_id => NodeId},
        #{result := ok} ?= perform_action(HostType, OwnerJid, Action),
        {ok, true}
    end.

-spec get_configuration(jid:jid(), mod_pubsub:node_id()) -> result(mod_pubsub:node_config()).
get_configuration(OwnerJid, NodeId) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Action = #{action => get_configuration, node_id => NodeId},
        #{result := #pubsub_node{config = Config}} ?= perform_action(HostType, OwnerJid, Action),
        {ok, Config}
    end.

-spec configure(jid:jid(), mod_pubsub:node_id(), mod_pubsub:node_config()) -> result(true).
configure(OwnerJid, NodeId, Config) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Action = #{action => configure, node_id => NodeId, config => Config},
        #{result := ok} ?= perform_action(HostType, OwnerJid, Action),
        {ok, true}
    end.

-spec get_items(jid:jid(), mod_pubsub:node_id(), mod_pubsub:get_items_opts()) ->
          result([mod_pubsub:item()]).
get_items(OwnerJid, NodeId, Opts) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Action = #{action => get_items, node_id => NodeId, opts => Opts},
        #{result := Items} ?= perform_action(HostType, OwnerJid, Action),
        {ok, Items}
    end.

-spec publish_item(jid:jid(), mod_pubsub:node_id(), mod_pubsub:item_id(),
                   mod_pubsub:item_payload(), mod_pubsub:node_config()) ->
          result(mod_pubsub:item_id()).
publish_item(OwnerJid, NodeId, ItemId, Payload, Config) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Action = #{action => publish, node_id => NodeId, item_id => ItemId, payload => Payload,
                   config => Config},
        #{result := PublishedItemId} ?= perform_action(HostType, OwnerJid, Action),
        {ok, PublishedItemId}
    end.

-spec retract_item(jid:jid(), mod_pubsub:node_id(), mod_pubsub:item_id(), boolean()) ->
          result(true).
retract_item(OwnerJid, NodeId, ItemId, Notify) ->
    maybe
        {ok, HostType} ?= check_user(OwnerJid),
        Action = #{action => retract, node_id => NodeId, item_id => ItemId, notify => Notify},
        #{result := ok} ?= perform_action(HostType, OwnerJid, Action),
        {ok, true}
    end.

%% Helpers

check_user(OwnerJid = #jid{lserver = LServer}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case ejabberd_auth:does_user_exist(HostType, jid:to_bare(OwnerJid), stored) of
                true -> {ok, HostType};
                false -> {user_not_found, "Given user does not exist"}
            end;
        {error, not_found} ->
            {domain_not_found, "User's domain does not exist"}
    end.

perform_action(HostType, OwnerJid = #jid{lserver = LServer}, Action) ->
    %% No C2S presence state: notify explicit subscribers and eligible owner resources only.
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             host_type => HostType,
                             lserver => LServer}),
    BareOwnerJid = jid:to_bare(OwnerJid),
    Request = #{acc => Acc, from_jid => BareOwnerJid, service_jid => BareOwnerJid},
    case mod_pubsub:perform_action(Request, Action) of
        #{result := _} = Result -> Result;
        {error, Reason} -> format_error(Reason)
    end.

format_error(item_not_found) ->
    {item_not_found, "PEP node or item not found"};
format_error({Code, Detail}) ->
    {Code, io_lib:format("PEP operation failed: ~p (~ts)", [Code, Detail])};
format_error(Code) ->
    {Code, io_lib:format("PEP operation failed: ~p", [Code])}.
