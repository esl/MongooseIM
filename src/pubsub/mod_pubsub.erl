%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the core of the PubSub
%%% extension. It relies on PubSub plugins for a large part of its functions.
%%%
%%% @headerfile "pubsub.hrl"
%%%
%%% @reference See <a href="http://www.xmpp.org/extensions/xep-0060.html">XEP-0060: Pubsub</a> for
%%% the latest version of the PubSub specification.
%%% This module uses version 1.12 of the specification as a base.
%%% Most of the specification is implemented.
%%% Functions concerning configuration should be rewritten.
%%%
%%% Support for subscription-options and multi-subscribe features was
%%% added by Brian Cully (bjc AT kublai.com). For information on
%%% subscription-options and multi-subscribe see XEP-0060 sections 6.1.6,
%%% 6.2.3.1, 6.2.3.5, and 6.3. For information on subscription leases see
%%% XEP-0060 section 12.18.

-module(mod_pubsub).
-behaviour(gen_mod).
-behaviour(gen_server).
-behaviour(mongoose_packet_handler).
-behaviour(mongoose_module_metrics).
-author('christophe.romain@process-one.net').

-xep([{xep, 60}, {version, "1.25.0"}]).
-xep([{xep, 163}, {version, "1.2.2"}]).
-xep([{xep, 248}, {version, "0.3.0"}]).
-xep([{xep, 277}, {version, "0.6.5"}]).

%% https://xmpp.org/extensions/xep-0384.html#server-side
-xep([{xep, 384}, {version, "0.8.3"}]).

-xep([{xep, 402}, {version, "1.2.0"}]).

-include("mongoose.hrl").
-include("adhoc.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").
-include("mongoose_config_spec.hrl").
-include("session.hrl").

-define(STDTREE, nodetree_tree).
-define(STDNODE, <<"flat">>).
-define(STDNODE_MODULE, node_flat).
-define(PEPNODE, <<"pep">>).
-define(PUSHNODE, <<"push">>).

%% exports for hooks
-export([presence_probe/3,
         caps_recognised/3,
         in_subscription/3,
         out_subscription/3,
         on_user_offline/3,
         remove_user/3,
         disco_local_features/3,
         disco_sm_identity/3,
         disco_sm_features/3,
         disco_sm_items/3,
         handle_pep_authorization_response/3,
         foreign_event/3]).

%% exported iq handlers
-export([iq_sm/4]).

%% exports for console debug manual use
-export([create_node/5, create_node/7, delete_node/3,
         subscribe_node/5, unsubscribe_node/5, publish_item/6,
         delete_item/4, send_items/7, get_items/2, get_item/3,
         get_cached_item/2, tree_action/3, node_action/4]).

%% general helpers for plugins
-export([subscription_to_string/1, affiliation_to_string/1,
         string_to_subscription/1, string_to_affiliation/1,
         extended_error/2, service_jid/1, tree/1, plugin/1,
         plugin_call/3, serverhost/1, host_to_host_type/1,
         host/2]).

%% API and gen_server callbacks
-export([start_link/2, start/2, stop/1, deps/2, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, instrumentation/1]).

%% Config callbacks
-export([config_spec/0, process_pep_mapping/1]).

-export([default_host/0]).

-export([get_personal_data/3]).

%% packet handler export
-export([process_packet/5]).

-export([config_metrics/1]).

%% Private export for wpool worker callbacks
-export([handle_msg/1]).

-define(MOD_PUBSUB_DB_BACKEND, mod_pubsub_db_backend).
-ignore_xref([
    {?MOD_PUBSUB_DB_BACKEND, transaction, 2},
    {?MOD_PUBSUB_DB_BACKEND, get_user_nodes, 2},
    {?MOD_PUBSUB_DB_BACKEND, get_user_payloads, 2},
    {?MOD_PUBSUB_DB_BACKEND, get_user_subscriptions, 2},
    {?MOD_PUBSUB_DB_BACKEND, start, 0},
    {?MOD_PUBSUB_DB_BACKEND, set_subscription_opts, 4},
    {?MOD_PUBSUB_DB_BACKEND, stop, 0},
    affiliation_to_string/1,
    create_node/7,
    default_host/0,
    delete_item/4,
    delete_node/3,
    get_cached_item/2,
    get_item/3,
    get_items/2,
    host/2,
    iq_sm/4,
    node_action/4,
    node_call/4,
    plugin/2,
    plugin/1,
    publish_item/6,
    send_items/7,
    serverhost/1,
    start_link/2,
    string_to_affiliation/1,
    string_to_subscription/1,
    subscribe_node/5,
    subscription_to_string/1,
    tree_action/3,
    unsubscribe_node/5,
    handle_msg/1
]).

-type plugin_name() :: binary().

-export_type([
              host/0,
              hostPubsub/0,
              hostPEP/0,
              %%
              nodeIdx/0,
              nodeId/0,
              itemId/0,
              subId/0,
              payload/0,
              %%
              nodeOption/0,
              nodeOptions/0,
              subOption/0,
              subOptions/0,
              %%
              affiliation/0,
              subscription/0,
              accessModel/0,
              publishModel/0
             ]).

%% -type payload() defined here because the -type exml:element() is not accessible
%% from pubsub.hrl
-type(payload() :: [] | [exml:element(), ...]).
-type(publishOptions() :: undefined | exml:element()).

-export_type([
              pubsubNode/0,
              pubsubState/0,
              pubsubItem/0,
              pubsubLastItem/0,
              publishOptions/0
             ]).

-type(pubsubNode() ::
        #pubsub_node{
           nodeid  :: {Host::mod_pubsub:host(), Node::mod_pubsub:nodeId()},
           id      :: Nidx::mod_pubsub:nodeIdx(),
           parents :: [Node::mod_pubsub:nodeId()],
           type    :: Type::binary(),
           owners  :: [Owner::jid:ljid(), ...],
           options :: Opts::mod_pubsub:nodeOptions()
          }
        ).

-type(pubsubState() ::
        #pubsub_state{
           stateid       :: {Entity::jid:ljid(), Nidx::mod_pubsub:nodeIdx()},
           items         :: [ItemId::mod_pubsub:itemId()],
           affiliation   :: Affs::mod_pubsub:affiliation(),
           subscriptions :: [{Sub::mod_pubsub:subscription(), SubId::mod_pubsub:subId()}]
          }
        ).

-type(pubsubItem() ::
        #pubsub_item{
           itemid       :: {ItemId::mod_pubsub:itemId(), Nidx::mod_pubsub:nodeIdx()},
           creation     :: {integer(), jid:ljid()},
           modification :: {integer(), jid:ljid()},
           payload      :: mod_pubsub:payload()
          }
        ).

-type(pubsubLastItem() ::
        #pubsub_last_item{
           nodeid   :: mod_pubsub:nodeIdx(),
           itemid   :: mod_pubsub:itemId(),
           creation :: {integer(), jid:ljid()},
           payload  :: mod_pubsub:payload()
          }
        ).

-record(state,
        {
          server_host,
          host,
          access,
          pep_mapping             = #{},
          ignore_pep_from_offline = true,
          last_item_cache         = false,
          max_items_node          = ?MAXITEMS,
          max_subscriptions_node  = undefined,
          default_node_config     = [],
          nodetree                = ?STDTREE,
          plugins                 = [?STDNODE] :: [plugin_name()]
        }).

-type(state() ::
        #state{
           server_host             :: binary(),
           host                    :: mod_pubsub:hostPubsub(),
           access                  :: atom(),
           pep_mapping             :: map(),
           ignore_pep_from_offline :: boolean(),
           last_item_cache         :: mnesia | rdbms | false,
           max_items_node          :: non_neg_integer(),
           max_subscriptions_node  :: non_neg_integer()|undefined,
           default_node_config     :: [{atom(), binary()|boolean()|integer()|atom()}],
           nodetree                :: module(),
           plugins                 :: [binary(), ...]
          }

        ).

%%====================================================================
%% API
%%====================================================================

%% @doc: Starts the server.
-spec start_link(mongooseim:domain_name(), gen_mod:module_opts()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

deps(_Host, _Opts) ->
    [{mod_caps, #{cache_size => 1000, cache_life_time => timer:hours(24) div 1000}, optional}].

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
                 transient, 1000, worker, [?MODULE]},
    start_pool(Host, Opts),
    ejabberd_sup:start_child(ChildSpec).

start_pool(HostType, #{wpool := WpoolOpts}) ->
    {ok, _} = mongoose_wpool:start(generic, HostType, pubsub_notify, maps:to_list(WpoolOpts)).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    mongoose_wpool:stop(generic, Host, pubsub_notify),
    ejabberd_sup:stop_child(Proc).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"host">> => #option{type = string,
                                       validate = subdomain_template,
                                       process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_pubsub_db}},
                 <<"access_createnode">> => #option{type = atom,
                                                    validate = access_rule},
                 <<"max_items_node">> => #option{type = integer,
                                                 validate = non_negative},
                 <<"max_subscriptions_node">> => #option{type = integer,
                                                         validate = non_negative},
                 <<"nodetree">> => #option{type = binary,
                                           validate = {module, nodetree},
                                           process = fun tree_mod/1},
                 <<"ignore_pep_from_offline">> => #option{type = boolean},
                 <<"last_item_cache">> => #option{type = atom,
                                                  validate = {enum, [mnesia, rdbms, false]}},
                 <<"plugins">> => #list{items = #option{type = binary,
                                                        validate = {module, node}}},
                 <<"pep_mapping">> => #list{items = pep_mapping_config_spec(),
                                            format_items = map},
                 <<"default_node_config">> => default_node_config_spec(),
                 <<"item_publisher">> => #option{type = boolean},
                 <<"sync_broadcast">> => #option{type = boolean},
                 <<"wpool">> => wpool_spec()
                },
       defaults = #{<<"iqdisc">> => one_queue,
                    <<"host">> => default_host(),
                    <<"backend">> => mnesia,
                    <<"access_createnode">> => all,
                    <<"max_items_node">> => ?MAXITEMS,
                    <<"nodetree">> => ?STDTREE,
                    <<"ignore_pep_from_offline">> => true,
                    <<"last_item_cache">> => false,
                    <<"plugins">> => [?STDNODE],
                    <<"pep_mapping">> => #{},
                    <<"default_node_config">> => [],
                    <<"item_publisher">> => false,
                    <<"sync_broadcast">> => false}
      }.

wpool_spec() ->
    Wpool = mongoose_config_spec:wpool(#{}),
    Wpool#section{include = always}.

pep_mapping_config_spec() ->
    #section{
       items = #{<<"namespace">> => #option{type = binary,
                                            validate = non_empty},
                 <<"node">> => #option{type = binary,
                                       validate = non_empty}},
       required = all,
       process = fun ?MODULE:process_pep_mapping/1
      }.

default_node_config_spec() ->
    #section{
       items = #{<<"access_model">> => #option{type = atom,
                                               validate = non_empty},
                 <<"deliver_notifications">> => #option{type = boolean},
                 <<"deliver_payloads">> => #option{type = boolean},
                 <<"max_items">> => #option{type = integer,
                                            validate = non_negative},
                 <<"max_payload_size">> => #option{type = integer,
                                                   validate = non_negative},
                 <<"node_type">> => #option{type = atom,
                                            validate = non_empty},
                 <<"notification_type">> => #option{type = atom,
                                                    validate = non_empty},
                 <<"notify_config">> => #option{type = boolean},
                 <<"notify_delete">> => #option{type = boolean},
                 <<"notify_retract">> => #option{type = boolean},
                 <<"persist_items">> => #option{type = boolean},
                 <<"presence_based_delivery">> => #option{type = boolean},
                 <<"publish_model">> => #option{type = atom,
                                                validate = non_empty},
                 <<"purge_offline">> => #option{type = boolean},
                 <<"roster_groups_allowed">> => #list{items = #option{type = binary,
                                                                      validate = non_empty}},
                 <<"send_last_published_item">> => #option{type = atom,
                                                           validate = non_empty},
                 <<"subscribe">> => #option{type = boolean}
                },
       format_items = list
      }.

process_pep_mapping(#{namespace := NameSpace, node := Node}) ->
    {NameSpace, Node}.

-spec default_host() -> mongoose_subdomain_utils:subdomain_pattern().
default_host() ->
    mongoose_subdomain_utils:make_subdomain_pattern(<<"pubsub.@HOST@">>).

%% State is an extra data, required for processing
-spec process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(), El :: exml:element(),
                     #{state := #state{}}) -> mongoose_acc:t().
process_packet(Acc, From, To, El, #{state := State}) ->
    #state{server_host = ServerHost, access = Access, plugins = Plugins} = State,
    do_route(Acc, ServerHost, Access, Plugins, To#jid.lserver, From, To, El).

%%====================================================================
%% GDPR callback
%%====================================================================

-spec get_personal_data(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: gdpr:personal_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
get_personal_data(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, _) ->
    Payloads = mod_pubsub_db_backend:get_user_payloads(LUser, LServer),
    Nodes = mod_pubsub_db_backend:get_user_nodes(LUser, LServer),
    Subscriptions = mod_pubsub_db_backend:get_user_subscriptions(LUser, LServer),

    NewAcc = [{pubsub_payloads, ["node_name", "item_id", "payload"], Payloads},
              {pubsub_nodes, ["node_name", "type"], Nodes},
              {pubsub_subscriptions, ["node_name"], Subscriptions} | Acc],
    {ok, NewAcc}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([binary() | gen_mod:module_opts(), ...]) -> {'ok', state()}.
init([ServerHost, Opts = #{host := SubdomainPattern}]) ->
    ?LOG_DEBUG(#{what => pubsub_init, server => ServerHost, opts => Opts}),
    Host = mongoose_subdomain_utils:get_fqdn(SubdomainPattern, ServerHost),

    init_backend(ServerHost, Opts),
    Plugins = init_plugins(Host, ServerHost, Opts),

    gen_hook:add_handlers(hooks(ServerHost)),
    case lists:member(?PEPNODE, Plugins) of
        true ->
            gen_hook:add_handlers(pep_hooks(ServerHost)),
            add_pep_iq_handlers(ServerHost, Opts);
        false ->
            ok
    end,
    State = init_state(ServerHost, Opts, Plugins),

    %% Pass State as extra into ?MODULE:process_packet/5 function
    PacketHandler = mongoose_packet_handler:new(?MODULE, #{state => State}),
    %% TODO: Conversion of this module is not done, it doesn't support dynamic
    %%       domains yet. Only subdomain registration is done properly.
    mongoose_domain_api:register_subdomain(ServerHost, SubdomainPattern, PacketHandler),
    {ok, State}.

init_backend(ServerHost, Opts) ->
    mod_pubsub_db_backend:init(ServerHost, Opts),
    maybe_start_cache_module(ServerHost, Opts).

hooks(ServerHost) ->
    [{disco_local_features, ServerHost, fun ?MODULE:disco_local_features/3, #{}, 75},
     {sm_remove_connection, ServerHost, fun ?MODULE:on_user_offline/3, #{}, 75},
     {presence_probe, ServerHost, fun ?MODULE:presence_probe/3, #{}, 80},
     {roster_in_subscription, ServerHost, fun ?MODULE:in_subscription/3, #{}, 50},
     {roster_out_subscription, ServerHost, fun ?MODULE:out_subscription/3, #{}, 50},
     {remove_user, ServerHost, fun ?MODULE:remove_user/3, #{}, 50},
     {anonymous_purge, ServerHost, fun ?MODULE:remove_user/3, #{}, 50},
     {get_personal_data, ServerHost, fun ?MODULE:get_personal_data/3, #{}, 50}].

pep_hooks(ServerHost) ->
    [
     {caps_recognised, ServerHost, fun ?MODULE:caps_recognised/3, #{}, 80},
     {disco_sm_identity, ServerHost, fun ?MODULE:disco_sm_identity/3, #{}, 75},
     {disco_sm_features, ServerHost, fun ?MODULE:disco_sm_features/3, #{}, 75},
     {disco_sm_items, ServerHost, fun ?MODULE:disco_sm_items/3, #{}, 75},
     {filter_local_packet, ServerHost, fun ?MODULE:handle_pep_authorization_response/3, #{}, 1},
     {foreign_event, ServerHost, fun ?MODULE:foreign_event/3, #{}, 100}
    ].

add_pep_iq_handlers(ServerHost, #{iqdisc := IQDisc}) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB, ?MODULE, iq_sm, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB_OWNER,
                                  ?MODULE, iq_sm, IQDisc).

delete_pep_iq_handlers(ServerHost) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ServerHost, ?NS_PUBSUB_OWNER).

%% Plugins is a subset of configured plugins which are able to start
%% TODO Evaluate if we should just use plugins from the config instead
init_state(ServerHost, #{last_item_cache := LastItemCache, max_items_node := MaxItemsNode,
                         pep_mapping := PepMapping, ignore_pep_from_offline := PepOffline,
                         access_createnode := Access}, Plugins) ->
    HostType = host_to_host_type(ServerHost),
    NodeTree = tree(HostType),
    Host = host(HostType, ServerHost),
    #state{host = Host, server_host = ServerHost,
           access = Access, pep_mapping = PepMapping,
           ignore_pep_from_offline = PepOffline,
           last_item_cache = LastItemCache,
           max_items_node = MaxItemsNode, nodetree = NodeTree,
           plugins = Plugins }.

%% @doc Call the init/1 function for each plugin declared in the config file.
%% The default plugin module is implicit.
%% <p>The Erlang code for the plugin is located in a module called
%% <em>node_plugin</em>. The 'node_' prefix is mandatory.</p>
%% <p>The modules are initialized in alphabetical order and the list is checked
%% and sorted to ensure that each module is initialized only once.</p>
%% <p>See {@link node_hometree:init/1} for an example implementation.</p>
init_plugins(Host, ServerHost, Opts = #{nodetree := TreePlugin, plugins := Plugins}) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(ServerHost),
    ?LOG_DEBUG(#{what => pubsub_tree_plugin, tree_plugin => TreePlugin}),
    gen_pubsub_nodetree:init(TreePlugin, HostType, Opts),
    PluginsOK = lists:foldl(pa:bind(fun init_plugin/5, Host, ServerHost, Opts), [], Plugins),
    lists:reverse(PluginsOK).

init_plugin(Host, ServerHost, Opts, Name, Acc) ->
    Plugin = plugin(Name),
    case catch apply(Plugin, init, [Host, ServerHost, Opts]) of
        {'EXIT', Error} ->
            ?LOG_ERROR(#{what => pubsub_plugin_init_failed, plugin => Plugin,
                server => ServerHost, sub_host => Host, opts => Opts, error => Error}),
            Acc;
        _ ->
            ?LOG_DEBUG(#{what => pubsub_init_plugin, plugin_name => Name}),
            [Name | Acc]
    end.

terminate_plugins(Host, ServerHost, Plugins, TreePlugin) ->
    lists:foreach(
      fun (Name) ->
              ?LOG_DEBUG(#{what => pubsub_terminate_plugin, plugin_name => Name}),
              Plugin = plugin(Name),
              gen_pubsub_node:terminate(Plugin, Host, ServerHost)
      end,
      Plugins),
    gen_pubsub_nodetree:terminate(TreePlugin, Host, ServerHost),
    ok.

notify_worker(HostType, HashKey, Request) ->
    mongoose_wpool:cast(generic, HostType, pubsub_notify, HashKey,
                        {?MODULE, handle_msg, [Request]}).

handle_msg({send_last_pubsub_items, Host, Recipient, Plugins}) ->
    send_last_pubsub_items(Host, Recipient, Plugins);
handle_msg({send_last_pep_items, Host, IgnorePepFromOffline, RecipientJID, RecipientPid, Features}) ->
    send_last_pep_items(Host, IgnorePepFromOffline, RecipientJID, RecipientPid, Features);
handle_msg({send_last_items_from_owner, Host, NodeOwner, RecipientInfo}) ->
    send_last_items_from_owner(Host, NodeOwner, RecipientInfo).

send_last_pubsub_items(Host, Recipient, Plugins) ->
    F = fun(PluginType) -> send_last_pubsub_items_for_plugin(Host, PluginType, Recipient) end,
    lists:foreach(F, Plugins).

send_last_pubsub_items_for_plugin(Host, PluginType, Recipient) ->
    JIDs = [Recipient, jid:to_lower(Recipient), jid:to_bare(Recipient)],
    Subs = get_subscriptions_for_send_last(Host, PluginType, JIDs),
    lists:foreach(
      fun({#pubsub_node{nodeid={_, Node}, id=Nidx, options=Options}, _, _, SubJID}) ->
              send_items(Host, Node, Nidx, PluginType, Options, SubJID, last)
      end,
      lists:usort(Subs)).

send_last_pep_items(Host, IgnorePepFromOffline, RecipientJID, RecipientPid, Features) ->
    RecipientLJID = jid:to_lower(RecipientJID),
    [send_last_item_to_jid(NodeOwnerJID, NodeRec, RecipientLJID) ||
        NodeOwnerJID <- get_contacts_for_sending_last_item(RecipientPid, IgnorePepFromOffline),
        NodeRec = #pubsub_node{nodeid = {_, Node}} <- get_nodes_for_sending_last_item(Host, NodeOwnerJID),
        lists:member(<<Node/binary, "+notify">>, Features)],
    ok.

get_contacts_for_sending_last_item(RecipientPid, IgnorePepFromOffline) ->
    case catch mod_presence:get_subscribed(RecipientPid) of
        Contacts when is_list(Contacts) ->
            [Contact || Contact = #jid{luser = U, lserver = S} <- Contacts,
                        user_resources(U, S) /= [] orelse not IgnorePepFromOffline];
        _ ->
            []
    end.

send_last_items_from_owner(Host, NodeOwner, _Recipient = {U, S, Resources}) ->
    [send_last_item_to_jid(NodeOwner, NodeRec, {U, S, R}) ||
        NodeRec = #pubsub_node{nodeid = {_, Node}} <- get_nodes_for_sending_last_item(Host, NodeOwner),
        R <- Resources,
        not should_drop_pep_message(Node, jid:make(U, S, R))
    ],
    ok.

should_drop_pep_message(Node, RecipientJid) ->
    case ejabberd_sm:get_session_pid(RecipientJid) of
        none -> true;
        Pid ->
            Event = {filter_pep_recipient, RecipientJid, <<Node/binary, "+notify">>},
            try mongoose_c2s:call(Pid, ?MODULE, Event)
            catch exit:timeout -> true
            end
    end.

get_nodes_for_sending_last_item(Host, NodeOwnerJID) ->
    lists:filter(fun(#pubsub_node{options = Options}) ->
                         match_option(Options, send_last_published_item, on_sub_and_presence)
                 end,
                 get_nodes_owned_by(Host, NodeOwnerJID)).

get_nodes_owned_by(Host, OwnerJID) ->
    OwnerBLJID = jid:to_bare(jid:to_lower(OwnerJID)),
    tree_action(Host, get_nodes, [OwnerBLJID, OwnerJID]).

send_last_item_to_jid(NodeOwner, #pubsub_node{nodeid = {_, Node}, type = NodeType,
                                              id = Nidx, options = NodeOptions}, RecipientJID) ->
    NodeOwnerBLJID = jid:to_bare(jid:to_lower(NodeOwner)),
    case is_subscribed(RecipientJID, NodeOwnerBLJID, NodeOptions) of
        true -> send_items(NodeOwnerBLJID, Node, Nidx, NodeType, NodeOptions, RecipientJID, last);
        false -> ok
    end.

is_subscribed(Recipient, NodeOwner, NodeOptions) ->
    case get_option(NodeOptions, access_model) of
        open -> true;
        presence -> true;
        whitelist -> false; % subscribers are added manually
        authorize -> false; % likewise
        roster ->
            Grps = get_option(NodeOptions, roster_groups_allowed, []),
            {OU, OS, _} = NodeOwner,
            element(2, get_roster_info(OU, OS, Recipient, Grps))
    end.

%% -------
%% disco hooks handling functions
%%

-spec identities(jid:lserver(), ejabberd:lang()) -> [mongoose_disco:identity()].
identities(Host, Lang) ->
    pubsub_identity(Lang) ++ node_identity(Host, ?PEPNODE) ++ node_identity(Host, ?PUSHNODE).

-spec pubsub_identity(ejabberd:lang()) -> [mongoose_disco:identity()].
pubsub_identity(Lang) ->
    [#{category => <<"pubsub">>,
       type => <<"service">>,
       name => service_translations:do(Lang, <<"Publish-Subscribe">>)}].

-spec node_identity(jid:lserver(), binary()) -> [mongoose_disco:identity()].
node_identity(Host, Type) ->
    case lists:member(Type, plugins(Host)) of
        true -> [#{category => <<"pubsub">>, type => Type}];
        false -> []
    end.


-spec disco_local_features(mongoose_disco:feature_acc(),
                           map(),
                           map()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{to_jid := #jid{lserver = LServer}, node := <<>>}, _, _) ->
    Features = [?NS_PUBSUB | [feature(F) || F <- features(LServer, <<>>)]],
    {ok, mongoose_disco:add_features(Features, Acc)};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

-spec disco_sm_identity(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:identity_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_sm_identity(Acc = #{from_jid := From, to_jid := To}, _, _) ->
    Identities = disco_identity(jid:to_lower(jid:to_bare(To)), From),
    {ok, mongoose_disco:add_identities(Identities, Acc)}.

disco_identity(error, _From) ->
    [];
disco_identity(_Host, _From) ->
    [pep_identity()].

pep_identity() ->
    #{category => <<"pubsub">>, type => <<"pep">>}.

-spec disco_sm_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_sm_features(Acc = #{from_jid := From, to_jid := To}, _, _) ->
    Features = disco_features(jid:to_lower(jid:to_bare(To)), From),
    {ok, mongoose_disco:add_features(Features, Acc)}.

-spec disco_features(error | jid:simple_jid(), jid:jid()) -> [mongoose_disco:feature()].
disco_features(error, _From) ->
    [];
disco_features(_Host, _From) ->
    [?NS_PUBSUB | [feature(F) || F <- plugin_features(<<"pep">>)]].

-spec disco_sm_items(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:item_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_sm_items(Acc = #{from_jid := From, to_jid := To}, _, _) ->
    Items = disco_items(jid:to_lower(jid:to_bare(To)), From),
    {ok, mongoose_disco:add_items(Items, Acc)}.

-spec disco_items(mod_pubsub:host(), jid:jid()) -> [mongoose_disco:item()].
disco_items(Host, From) ->
    Action = fun (#pubsub_node{nodeid = {_, Node},
                               options = Options, type = Type, id = Nidx, owners = Owners},
                  Acc) ->
                     case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) of
                         {result, _} ->
                             [disco_item(Node, Host, Options) | Acc];
                         _ ->
                             Acc
                     end
             end,
    NodeBloc = fun() ->
                       {result,
                        lists:foldl(Action, [], tree_call(Host, get_nodes, [Host, From]))}
               end,
    ErrorDebug = #{
      action => disco_items,
      pubsub_host => Host,
      from => From
     },
    case mod_pubsub_db_backend:dirty(NodeBloc, ErrorDebug) of
        {result, Items} -> Items;
        _ -> []
    end.

disco_item(Node, Host, Options) ->
    Item = #{node => Node,
             jid => jid:to_binary(Host)},
    case get_option(Options, title) of
        false -> Item;
        [Title] -> Item#{name => Title}
    end.

%% -------
%% callback that prevents routing subscribe authorizations back to the sender
%%

-spec handle_pep_authorization_response(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_hooks:filter_packet_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
handle_pep_authorization_response({From, To, Acc, #xmlel{ name = Name } = Packet}, _, _) ->
    Type = mongoose_acc:stanza_type(Acc),
    {ok, handle_pep_authorization_response(Name, Type, From, To, Acc, Packet)}.

handle_pep_authorization_response(_, <<"error">>, From, To, Acc, Packet) ->
    {From, To, Acc, Packet};
handle_pep_authorization_response(<<"message">>, _, From, To, Acc, Packet)
  when From#jid.luser == To#jid.luser, From#jid.lserver == To#jid.lserver ->
        case find_authorization_response(Packet) of
            none ->
                {From, To, Acc, Packet};
            invalid ->
                {From, To, Acc, Packet};
            KVs ->
                handle_authorization_response(Acc, jid:to_lower(To), From, To, Packet, KVs),
                drop
        end;
handle_pep_authorization_response(_, _, From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.

%% -------
%% callback for foreign_event calls, to distribute pep messages from the node owner c2s process
%%
-spec foreign_event(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
      mongoose_c2s_hooks:result().
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := {call, From},
                     event_tag := ?MODULE,
                     event_content := {get_pep_recipients, Feature}}, _Extra) ->
    Reply = mongoose_hooks:get_pep_recipients(StateData, Feature),
    Acc1 = mongoose_c2s_acc:to_acc(Acc, actions, [{reply, From, Reply}]),
    {stop, Acc1};
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := {call, From},
                     event_tag := ?MODULE,
                     event_content := {filter_pep_recipient, To, Feature}}, _Extra) ->
    Reply = mongoose_hooks:filter_pep_recipient(StateData, Feature, To),
    Acc1 = mongoose_c2s_acc:to_acc(Acc, actions, [{reply, From, Reply}]),
    {stop, Acc1};
foreign_event(Acc, _Params, _Extra) ->
    {ok, Acc}.

%% -------
%% presence hooks handling functions
%%

-spec caps_recognised(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{from := jid:jid(), pid := pid(), features := [binary()]},
    Extra :: gen_hook:extra().
caps_recognised(Acc, #{from := #jid{ luser = U, lserver = S } = JID, pid := Pid, features := Features}, _) ->
    Host = host(S, S),
    IgnorePepFromOffline = gen_mod:get_module_opt(S, ?MODULE, ignore_pep_from_offline),
    notify_worker(S, U, {send_last_pep_items, Host, IgnorePepFromOffline, JID, Pid, Features}),
    {ok, Acc}.

-spec presence_probe(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{from := jid:jid(), to := jid:jid()},
    Extra :: gen_hook:extra().
presence_probe(Acc, #{from := #jid{luser = U, lserver = S, lresource = _R} = JID, to := JID}, _) ->
    %% Get subdomain
    Host = host(S, S),
    Plugins = plugins(S),
    notify_worker(S, U, {send_last_pubsub_items, Host, _Recipient = JID, Plugins}),
    {ok, Acc};
presence_probe(Acc, _, _) ->
    {ok, Acc}.

%% -------
%% subscription hooks handling functions
%%

-spec out_subscription(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{to := jid:jid(),
                from := jid:jid(),
                type := mod_roster:sub_presence()},
    Extra :: gen_hook:extra().
out_subscription(Acc,
                 #{to := #jid{luser = PUser, lserver = PServer, lresource = PResource},
                   from := #jid{luser = LUser, lserver = LServer} = FromJID,
                   type := subscribed},
                 _) ->
    PResources = case PResource of
                     <<>> -> user_resources(PUser, PServer);
                     _ -> [PResource]
                 end,
    Host = host(LServer, LServer),
    notify_worker(LServer, LUser, {send_last_items_from_owner, Host, FromJID,
                                   {PUser, PServer, PResources}}),
    {ok, Acc};
out_subscription(Acc, _, _) ->
    {ok, Acc}.

-spec in_subscription(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{to := jid:jid(),
                from := jid:jid(),
                type := mod_roster:sub_presence()},
    Extra :: gen_hook:extra().
in_subscription(Acc, #{to := ToJID, from := OwnerJID, type := unsubscribed}, _) ->
    unsubscribe_user(ToJID, OwnerJID),
    {ok, Acc};
in_subscription(Acc, _, _) ->
    {ok, Acc}.

unsubscribe_user(Entity, Owner) ->
    ServerHosts = lists:usort(lists:foldl(
                                fun(UserHost, Acc) ->
                                        case gen_mod:is_loaded(UserHost, mod_pubsub) of
                                            true -> [UserHost|Acc];
                                            false -> Acc
                                        end
                                end, [], [Entity#jid.lserver, Owner#jid.lserver])),
    [unsubscribe_user(ServerHost, Entity, Owner) || ServerHost <- ServerHosts],
    ok.

unsubscribe_user(Host, Entity, Owner) ->
    BJID = jid:to_lower(jid:to_bare(Owner)),
    lists:foreach(fun (PType) ->
                         unsubscribe_user_per_plugin(Host, Entity, BJID, PType)
                  end, plugins(Host)).

unsubscribe_user_per_plugin(Host, Entity, BJID, PType) ->
    {result, Subs} = node_action(Host, PType, get_entity_subscriptions, [Host, Entity]),
    lists:foreach(fun({#pubsub_node{options = Options, owners = Owners, id = Nidx},
                       subscribed, _, JID}) ->
                          Unsubscribe = match_option(Options, access_model, presence)
                          andalso lists:member(BJID, Owners),
                          case Unsubscribe of
                              true ->
                                  node_action(Host, PType,
                                              unsubscribe_node, [Nidx, Entity, JID, all]);
                              false ->
                                  ok
                          end;
                     (_) ->
                          ok
                  end, Subs).

%% -------
%% user remove hook handling function
%%
-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: #{host_type := mongooseim:host_type()}.
remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, _) ->
    lists:foreach(fun(PType) ->
                          remove_user_per_plugin_safe(LUser, LServer, plugin(PType))
                  end, plugins(LServer)),
    {ok, Acc}.

remove_user_per_plugin_safe(LUser, LServer, Plugin) ->
    try
        plugin_call(Plugin, remove_user, [LUser, LServer])
    catch
        Class:Reason:StackTrace ->
            ?LOG_WARNING(#{what => pubsub_delete_user_failed, user => LUser,
                server => LServer, class => Class, reason => Reason,
                stacktrace => StackTrace})
    end.

handle_call(server_host, _From, State) ->
    {reply, State#state.server_host, State};
handle_call(plugins, _From, State) ->
    {reply, State#state.plugins, State};
handle_call(pep_mapping, _From, State) ->
    {reply, State#state.pep_mapping, State};
handle_call(nodetree, _From, State) ->
    {reply, State#state.nodetree, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, #state{host = Host, server_host = ServerHost,
                          nodetree = TreePlugin, plugins = Plugins}) ->
    SubdomainPattern = gen_mod:get_module_opt(ServerHost, ?MODULE, host),
    mongoose_domain_api:unregister_subdomain(ServerHost, SubdomainPattern),
    case lists:member(?PEPNODE, Plugins) of
        true ->
            gen_hook:delete_handlers(pep_hooks(ServerHost)),
            delete_pep_iq_handlers(ServerHost);
        false -> ok
    end,
    gen_hook:delete_handlers(hooks(ServerHost)),
    case whereis(gen_mod:get_module_proc(ServerHost, ?LOOPNAME)) of
        undefined ->
            ?LOG_ERROR(#{what => pubsub_process_is_dead,
                text => <<"process is dead, pubsub was broken">>,
                process => ?LOOPNAME});
        Pid ->
            Pid ! stop
    end,
    terminate_plugins(Host, ServerHost, Plugins, TreePlugin),
    mod_pubsub_db_backend:stop().

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec do_route(
        Acc :: mongoose_acc:t(),
        ServerHost :: binary(),
          Access     :: atom(),
          Plugins    :: [binary(), ...],
          Host       :: mod_pubsub:hostPubsub(),
          From       ::jid:jid(),
          To         ::jid:jid(),
          Packet     :: exml:element())
        -> mongoose_acc:t().

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_route(Acc, ServerHost, Access, Plugins, Host, From,
         #jid{luser = <<>>, lresource = <<>>} = To,
         #xmlel{ name = <<"iq">> } = Packet) ->
    case jlib:iq_query_info(Packet) of
        #iq{type = get, xmlns = ?NS_DISCO_INFO, sub_el = SubEl, lang = Lang} = IQ ->
            #xmlel{attrs = QAttrs} = SubEl,
            Node = exml_query:attr(SubEl, <<"node">>, <<>>),
            InfoXML = mongoose_disco:get_info(ServerHost, ?MODULE, <<>>, <<>>),
            Res = case iq_disco_info(ServerHost, Host, Node, From, Lang) of
                      {result, IQRes} ->
                          jlib:iq_to_xml(IQ#iq{type = result,
                                               sub_el =
                                               [#xmlel{name = <<"query">>,
                                                       attrs = QAttrs,
                                                       children = IQRes ++ InfoXML}]});
                      {error, Error} ->
                          make_error_reply(Packet, Error)
                  end,
            ejabberd_router:route(To, From, Acc, Res);
        #iq{type = get, xmlns = ?NS_DISCO_ITEMS, sub_el = SubEl} = IQ ->
            #xmlel{attrs = QAttrs} = SubEl,
            Node = exml_query:attr(SubEl, <<"node">>, <<>>),
            Res = case iq_disco_items(Host, Node, From, jlib:rsm_decode(IQ)) of
                      {result, IQRes} ->
                          jlib:iq_to_xml(IQ#iq{type = result,
                                               sub_el =
                                               [#xmlel{name = <<"query">>,
                                                       attrs = QAttrs,
                                                       children = IQRes}]});
                      {error, Error} ->
                          make_error_reply(Packet, Error)
                  end,
            ejabberd_router:route(To, From, Acc, Res);
        #iq{type = IQType, xmlns = ?NS_PUBSUB, lang = Lang, sub_el = SubEl} = IQ ->
            Res = case iq_pubsub(Host, ServerHost, From, IQType,
                                 SubEl, Lang, Access, Plugins)
                  of
                      {result, IQRes} ->
                          jlib:iq_to_xml(IQ#iq{type = result, sub_el = IQRes});
                      {error, Error} ->
                          make_error_reply(Packet, Error)
                  end,
            ejabberd_router:route(To, From, Acc, Res);
        #iq{type = IQType, xmlns = ?NS_PUBSUB_OWNER, lang = Lang, sub_el = SubEl} = IQ ->
            Res = case iq_pubsub_owner(Host, ServerHost, From,
                                       IQType, SubEl, Lang)
                  of
                      {result, IQRes} ->
                          jlib:iq_to_xml(IQ#iq{type = result, sub_el = IQRes});
                      {error, {Error, NewPayload}} ->
                          make_error_reply(Packet#xmlel{ children = NewPayload }, Error);
                      {error, Error} ->
                          make_error_reply(Packet, Error)
                  end,
            ejabberd_router:route(To, From, Acc, Res);
        #iq{type = get, xmlns = (?NS_VCARD) = XMLNS, lang = Lang, sub_el = _SubEl} = IQ ->
            Res = IQ#iq{type = result,
                        sub_el =
                        [#xmlel{name = <<"vCard">>,
                                attrs = #{<<"xmlns">> => XMLNS},
                                children = iq_get_vcard(Lang)}]},
            ejabberd_router:route(To, From, Acc, jlib:iq_to_xml(Res));
        #iq{type = set, xmlns = ?NS_COMMANDS} = IQ ->
            Res = case iq_command(Host, ServerHost, From, IQ, Access, Plugins) of
                      {error, Error} ->
                          make_error_reply(Packet, Error);
                      {result, IQRes} ->
                          jlib:iq_to_xml(IQ#iq{type = result, sub_el = IQRes})
                  end,
            ejabberd_router:route(To, From, Acc, Res);
        #iq{} ->
            Err = make_error_reply(Packet, mongoose_xmpp_errors:feature_not_implemented()),
            ejabberd_router:route(To, From, Acc, Err);
        _ ->
            Acc
    end;
do_route(Acc, _ServerHost, _Access, _Plugins, Host, From,
         #jid{luser = <<>>, lresource = <<>>} = To,
         #xmlel{ name = <<"message">> } = Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"error">> ->
            ok;
        _ ->
            case find_authorization_response(Packet) of
                none ->
                    Acc;
                invalid ->
                    Err = make_error_reply(Packet, mongoose_xmpp_errors:bad_request()),
                    ejabberd_router:route(To, From, Acc, Err);
                XFields ->
                    handle_authorization_response(Acc, Host, From, To, Packet, XFields)
            end
    end;
do_route(Acc, _ServerHost, _Access, _Plugins, _Host, _From,
         #jid{luser = <<>>, lresource = <<>>} = _To, _Packet) ->
    Acc;
do_route(Acc, _ServerHost, _Access, _Plugins, _Host, From, To, Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"error">> ->
            Acc;
        <<"result">> ->
            Acc;
        _ ->
            Err = make_error_reply(Packet, mongoose_xmpp_errors:item_not_found()),
            ejabberd_router:route(To, From, Acc, Err)
    end.

command_disco_info(_Host, ?NS_COMMANDS, _From) ->
    IdentityEl = #xmlel{name = <<"identity">>,
                        attrs = #{<<"category">> => <<"automation">>,
                                  <<"type">> => <<"command-list">>}},
    {result, [IdentityEl]};
command_disco_info(_Host, ?NS_PUBSUB_GET_PENDING, _From) ->
    IdentityEl = #xmlel{name = <<"identity">>,
                        attrs = #{<<"category">> => <<"automation">>,
                                  <<"type">> => <<"command-node">>}},
    FeaturesEl = #xmlel{name = <<"feature">>,
                        attrs = #{<<"var">> => ?NS_COMMANDS}},
    {result, [IdentityEl, FeaturesEl]}.

node_disco_info(Host, Node, From) ->
    node_disco_info(Host, Node, From, true, true).

node_disco_info(Host, Node, _From, _Identity, _Features) ->
    Action = fun (#pubsub_node{type = Type, options = Options}) ->
                     NodeType = case get_option(Options, node_type) of
                                    collection -> <<"collection">>;
                                    _ -> <<"leaf">>
                                end,
                     I = #xmlel{name = <<"identity">>,
                                attrs = #{<<"category">> => <<"pubsub">>,
                                          <<"type">> => NodeType}},
                     F = [#xmlel{name = <<"feature">>,
                                 attrs = #{<<"var">> => ?NS_PUBSUB}}
                          | [#xmlel{name = <<"feature">>,
                                    attrs = #{<<"var">> => feature(F)}}
                             || F <- plugin_features(Type)]],
                     {result, [I | F]}
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, Result}} -> {result, Result};
        Other -> Other
    end.

iq_disco_info(ServerHost, Host, SNode, From, Lang) ->
    [Node | _] = case SNode of
                     <<>> -> [<<>>];
                     _ -> mongoose_bin:tokens(SNode, <<"!">>)
                 end,
    case Node of
        <<>> ->
            Identities = identities(ServerHost, Lang),
            Features = [?NS_DISCO_INFO,
                        ?NS_DISCO_ITEMS,
                        ?NS_PUBSUB,
                        ?NS_COMMANDS,
                        ?NS_VCARD] ++ [feature(F) || F <- features(ServerHost, Node)],
            {result, mongoose_disco:identities_to_xml(Identities) ++
                 mongoose_disco:features_to_xml(Features)};
        ?NS_COMMANDS ->
            command_disco_info(Host, Node, From);
        ?NS_PUBSUB_GET_PENDING ->
            command_disco_info(Host, Node, From);
        _ ->
            node_disco_info(Host, Node, From)
    end.

-spec iq_disco_items(
        Host   :: mod_pubsub:host(),
          Node   :: <<>> | mod_pubsub:nodeId(),
          From   ::jid:jid(),
          Rsm    :: none | jlib:rsm_in())
        -> {result, [exml:element()]} | {error, term()}.
iq_disco_items(Host, <<>>, From, _RSM) ->
    {result,
     lists:map(fun (#pubsub_node{nodeid = {_, SubNode}, options = Options}) ->
                       Attrs1 = node_attr(SubNode),
                       Attrs2 = case get_option(Options, title) of
                                    false ->
                                        Attrs1#{<<"jid">> => Host};
                                    [Title] ->
                                        Attrs1#{<<"jid">> => Host,
                                                <<"name">> => Title}
                               end,
                       #xmlel{name = <<"item">>, attrs = Attrs2}
               end,
               tree_action(Host, get_subnodes, [Host, <<>>, From]))};
iq_disco_items(Host, ?NS_COMMANDS, _From, _RSM) ->
    {result, [#xmlel{name = <<"item">>,
                     attrs = #{<<"jid">> => Host,
                               <<"node">> => ?NS_PUBSUB_GET_PENDING,
                               <<"name">> => <<"Get Pending">>}}]};
iq_disco_items(_Host, ?NS_PUBSUB_GET_PENDING, _From, _RSM) ->
    {result, []};
iq_disco_items(Host, Item, From, RSM) ->
    case mongoose_bin:tokens(Item, <<"!">>) of
        [_Node, _ItemId] ->
            {result, []};
        [Node] ->
            Action = fun (PubSubNode) ->
                             iq_disco_items_transaction(Host, From, Node, RSM, PubSubNode)
                     end,
            case dirty(Host, Node, Action, ?FUNCTION_NAME) of
                {result, {_, Result}} -> {result, Result};
                Other -> Other
            end
    end.

iq_disco_items_transaction(Host, From, Node, RSM,
                           #pubsub_node{id = Nidx, type = Type, options = Options, owners = Owners}) ->
    {NodeItems, RsmOut} = case get_allowed_items_call(Host, Nidx,
                                                      From, Type, Options, Owners, RSM)
                          of
                              {result, R} -> R;
                              _ -> {[], none}
                          end,
    Nodes = lists:map(fun (#pubsub_node{nodeid = {_, SubNode}, options = SubOptions}) ->
                              Attrs1 = node_attr(SubNode),
                              Attrs2 = case get_option(SubOptions, title) of
                                           false ->
                                               Attrs1#{<<"jid">> => Host};
                                           [Title] ->
                                               Attrs1#{<<"jid">> => Host,
                                                       <<"name">> => Title}
                                       end,
                              #xmlel{name = <<"item">>, attrs = Attrs2}
                      end,
                      tree_call(Host, get_subnodes, [Host, Node, From])),
    Items = lists:map(fun (#pubsub_item{itemid = {RN, _}}) ->
                              {result, Name} = node_call(Type, get_item_name,
                                                         [Host, Node, RN]),
                              #xmlel{name = <<"item">>,
                                     attrs = #{<<"jid">> => Host, <<"name">> => Name}}
                      end,
                      NodeItems),
    {result, Nodes ++ Items ++ jlib:rsm_encode(RsmOut)}.

-spec iq_sm(From ::jid:jid(),
            To   ::jid:jid(),
            Acc :: mongoose_acc:t(),
            IQ   :: jlib:iq())
        -> {mongoose_acc:t(), jlib:iq()}.
iq_sm(From, To, Acc, #iq{type = Type, sub_el = SubEl, xmlns = XMLNS, lang = Lang} = IQ) ->
    ServerHost = To#jid.lserver,
    LOwner = jid:to_lower(jid:to_bare(To)),
    Res = case XMLNS of
              ?NS_PUBSUB ->
                  iq_pubsub(LOwner, ServerHost, From, Type, SubEl, Lang);
              ?NS_PUBSUB_OWNER ->
                  iq_pubsub_owner(LOwner, ServerHost, From, Type, SubEl, Lang)
          end,
    case Res of
        {result, IQRes} -> {Acc, IQ#iq{type = result, sub_el = IQRes}};
        {error, Error} -> {Acc, make_error_reply(IQ, Error)}
    end.

iq_get_vcard(Lang) ->
    Desc = <<(service_translations:do(Lang, <<"ejabberd Publish-Subscribe module">>))/binary,
             "\nCopyright (c) 2004-2015 ProcessOne">>,
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"ejabberd/mod_pubsub">>}]},
     #xmlel{name = <<"URL">>,
            children = [#xmlcdata{content = ?MONGOOSE_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content = Desc}]}].

-spec iq_pubsub(Host :: mod_pubsub:host(),
                ServerHost :: binary(),
                From ::jid:jid(),
                IQType :: get | set,
                QueryEl :: exml:element(),
                Lang :: binary()) -> {result, [exml:element()]} | {error, exml:element()}.
iq_pubsub(Host, ServerHost, From, IQType, QueryEl, Lang) ->
    iq_pubsub(Host, ServerHost, From, IQType, QueryEl, Lang, all, plugins(ServerHost)).

-spec iq_pubsub(Host :: mod_pubsub:host(),
                ServerHost :: binary(),
                From ::jid:jid(),
                IQType :: 'get' | 'set',
                QueryEl :: exml:element(),
                Lang :: binary(),
                Access :: atom(),
                Plugins :: [binary(), ...]) -> {result, [exml:element()]} | {error, exml:element()}.
iq_pubsub(Host, ServerHost, From, IQType, #xmlel{children = SubEls} = QueryEl,
          Lang, Access, Plugins) ->
    case jlib:remove_cdata(SubEls) of
        [#xmlel{name = Name} = ActionEl | _] ->
            Node = exml_query:attr(ActionEl, <<"node">>, <<>>),
            ActionExtraArgs = #{server_host => ServerHost,
                                plugins => Plugins,
                                access => Access,
                                action_el => ActionEl,
                                query_el => QueryEl,
                                lang => Lang},
            mongoose_instrument:span(event_name(IQType, Name), #{host_type => ServerHost}, fun iq_pubsub_action/6,
                                     [IQType, Name, Host, Node, From, ActionExtraArgs],
                                     fun(Time, Result) -> iq_action_result_to_measurements(Time, Result, From) end);
        Other ->
            ?LOG_INFO(#{what => pubsub_bad_request, exml_packet => Other}),
            {error, mongoose_xmpp_errors:bad_request()}
    end.

iq_pubsub_action(IQType, Name, Host, Node, From, ExtraArgs) ->
    case {IQType, Name} of
        {set, <<"create">>} ->
            iq_pubsub_set_create(Host, Node, From, ExtraArgs);
        {set, <<"publish">>} ->
            iq_pubsub_set_publish(Host, Node, From, ExtraArgs);
        {set, <<"retract">>} ->
            iq_pubsub_set_retract(Host, Node, From, ExtraArgs);
        {set, <<"subscribe">>} ->
            iq_pubsub_set_subscribe(Host, Node, From, ExtraArgs);
        {set, <<"unsubscribe">>} ->
            iq_pubsub_set_unsubscribe(Host, Node, From, ExtraArgs);
        {get, <<"items">>} ->
            iq_pubsub_get_items(Host, Node, From, ExtraArgs);
        {get, <<"subscriptions">>} ->
            get_subscriptions(Host, Node, From, ExtraArgs);
        {get, <<"affiliations">>} ->
            get_affiliations(Host, Node, From, ExtraArgs);
        {get, <<"options">>} ->
            iq_pubsub_get_options(Host, Node, From, ExtraArgs);
        {set, <<"options">>} ->
            iq_pubsub_set_options(Host, Node, ExtraArgs);
          _ ->
            {error, mongoose_xmpp_errors:feature_not_implemented()}
    end.

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    Measurements = #{count => spiral, errors => spiral, time => histogram},
    [{event_name(IQType, Name), #{host_type => HostType}, #{metrics => Measurements}} ||
        {IQType, Name} <- all_metrics()].

all_metrics() ->
    [{set, create},
     {set, publish},
     {set, retract},
     {set, subscribe},
     {set, unsubscribe},
     {get, items},
     {get, options},
     {set, options},
     {get, configure},
     {set, configure},
     {get, default},
     {set, delete},
     {set, purge},
     {get, subscriptions},
     {set, subscriptions},
     {get, affiliations},
     {set, affiliations}].

iq_action_to_metric_name(<<"create">>) -> create;
iq_action_to_metric_name(<<"publish">>) -> publish;
iq_action_to_metric_name(<<"retract">>) -> retract;
iq_action_to_metric_name(<<"subscribe">>) -> subscribe;
iq_action_to_metric_name(<<"unsubscribe">>) -> unsubscribe;
iq_action_to_metric_name(<<"items">>) -> items;
iq_action_to_metric_name(<<"options">>) -> options;
iq_action_to_metric_name(<<"configure">>) -> configure;
iq_action_to_metric_name(<<"default">>) -> default;
iq_action_to_metric_name(<<"delete">>) -> delete;
iq_action_to_metric_name(<<"purge">>) -> purge;
iq_action_to_metric_name(<<"subscriptions">>) -> subscriptions;
iq_action_to_metric_name(<<"affiliations">>) -> affiliations.

event_name(IQType, Name) when is_binary(Name) ->
    NameAtom = iq_action_to_metric_name(Name),
    event_name(IQType, NameAtom);
event_name(set, create) ->
    mod_pubsub_set_create;
event_name(set, publish) ->
    mod_pubsub_set_publish;
event_name(set, retract) ->
    mod_pubsub_set_retract;
event_name(set, subscribe) ->
    mod_pubsub_set_subscribe;
event_name(set, unsubscribe) ->
    mod_pubsub_set_unsubscribe;
event_name(get, items) ->
    mod_pubsub_get_items;
event_name(get, options) ->
    mod_pubsub_get_options;
event_name(set, options) ->
    mod_pubsub_set_options;
event_name(get, configure) ->
    mod_pubsub_get_configure;
event_name(set, configure) ->
    mod_pubsub_set_configure;
event_name(get, default) ->
    mod_pubsub_get_default;
event_name(set, delete) ->
    mod_pubsub_set_delete;
event_name(set, purge) ->
    mod_pubsub_set_purge;
event_name(get, subscriptions) ->
    mod_pubsub_get_subscriptions;
event_name(set, subscriptions) ->
    mod_pubsub_set_subscriptions;
event_name(get, affiliations) ->
    mod_pubsub_get_affiliations;
event_name(set, affiliations) ->
    mod_pubsub_set_affiliations.

iq_action_result_to_measurements(_Time, {error, _}, From) ->
    #{errors => 1, jid => From};
iq_action_result_to_measurements(Time, _Result, From) ->
    #{time => Time, count => 1, jid => From}.

iq_pubsub_set_create(Host, Node, From,
                     #{server_host := ServerHost, access := Access, plugins := Plugins,
                       action_el := CreateEl, query_el := QueryEl}) ->
    Config = exml_query:subelement(QueryEl, <<"configure">>),
    Type = exml_query:attr(CreateEl, <<"type">>, hd(Plugins)),
    case lists:member(Type, Plugins) of
        false ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"create-nodes">>)};
        true ->
            create_node(Host, ServerHost, Node, From, Type, Access, Config)
    end.

iq_pubsub_set_publish(_Host, <<>>, _From, _ExtraArgs) ->
    {error, extended_error(mongoose_xmpp_errors:bad_request(), <<"nodeid-required">>)};
iq_pubsub_set_publish(Host, Node, From, #{server_host := ServerHost, access := Access,
                                          action_el := ActionEl, query_el := QueryEl}) ->
    case jlib:remove_cdata(ActionEl#xmlel.children) of
        [#xmlel{name = <<"item">>, children = Payload} = Element] ->
            ItemId = exml_query:attr(Element, <<"id">>, <<>>),
            PublishOptions = exml_query:subelement(QueryEl, <<"publish-options">>),
            publish_item(Host, ServerHost, Node, From, ItemId,
                         Payload, Access, PublishOptions);
        [] ->
            {error, extended_error(mongoose_xmpp_errors:bad_request(), <<"item-required">>)};
        _ ->
            {error, extended_error(mongoose_xmpp_errors:bad_request(), <<"invalid-payload">>)}
    end.

iq_pubsub_set_retract(Host, Node, From,
                      #{action_el := #xmlel{children = RetractSubEls} = Element}) ->
    ForceNotify = case exml_query:attr(Element, <<"notify">>) of
                      <<"1">> -> true;
                      <<"true">> -> true;
                      _ -> false
                  end,
    case jlib:remove_cdata(RetractSubEls) of
        [#xmlel{name = <<"item">>} = Element1] ->
            ItemId = exml_query:attr(Element1, <<"id">>, <<>>),
            delete_item(Host, Node, From, ItemId, ForceNotify);
        _ ->
            {error,
             extended_error(mongoose_xmpp_errors:bad_request(), <<"item-required">>)}
    end.

iq_pubsub_set_subscribe(Host, Node, From, #{query_el := QueryEl,
                                            action_el := #xmlel{} = El}) ->
    ConfigXForm = case exml_query:subelement(QueryEl, <<"options">>) of
                      undefined -> undefined;
                      Options -> mongoose_data_forms:find_form(Options)
                  end,
    JID = exml_query:attr(El, <<"jid">>, <<>>),
    subscribe_node(Host, Node, From, JID, ConfigXForm).

iq_pubsub_set_unsubscribe(Host, Node, From, #{action_el := #xmlel{} = El}) ->
    JID = exml_query:attr(El, <<"jid">>, <<>>),
    SubId = exml_query:attr(El, <<"subid">>, <<>>),
    unsubscribe_node(Host, Node, From, JID, SubId).

iq_pubsub_get_items(Host, Node, From,
                    #{query_el := QueryEl,
                      action_el := #xmlel{children = GetItemsSubEls}= El}) ->
    MaxItems = exml_query:attr(El, <<"max_items">>, <<>>),
    SubId = exml_query:attr(El, <<"subid">>, <<>>),
    ItemIds = extract_item_ids(GetItemsSubEls),
    get_items(Host, Node, From, SubId, MaxItems, ItemIds, jlib:rsm_decode(QueryEl)).

extract_item_ids(GetItemsSubEls) ->
    case lists:foldl(fun extract_item_id/2, [], GetItemsSubEls) of
        [] ->
            undefined;
        List ->
            List
    end.

extract_item_id(#xmlel{name = <<"item">>} = Item, Acc) ->
  case exml_query:attr(Item, <<"id">>) of
      undefined -> Acc;
      ItemId -> [ItemId | Acc]
    end;
extract_item_id(_, Acc) -> Acc.


iq_pubsub_get_options(Host, Node, Lang, #{action_el := #xmlel{} = El}) ->
    SubId = exml_query:attr(El, <<"subid">>, <<>>),
    JID = exml_query:attr(El, <<"jid">>, <<>>),
    get_options(Host, Node, JID, SubId, Lang).

iq_pubsub_set_options(Host, Node, #{action_el := #xmlel{} = ActionEl}) ->
    XForm = mongoose_data_forms:find_form(ActionEl),
    SubId = exml_query:attr(ActionEl, <<"subid">>, <<>>),
    JID = exml_query:attr(ActionEl, <<"jid">>, <<>>),
    set_options(Host, Node, JID, SubId, XForm).

-spec iq_pubsub_owner(
        Host       :: mod_pubsub:host(),
          ServerHost :: binary(),
          From       ::jid:jid(),
          IQType     :: 'get' | 'set',
          SubEl      :: exml:element(),
          Lang       :: binary())
        -> {result, [exml:element()]}
           | {error, exml:element() | [exml:element()] | {exml:element(), [exml:element()]}}.
iq_pubsub_owner(Host, ServerHost, From, IQType, SubEl, Lang) ->
    #xmlel{children = SubEls} = SubEl,
    Action = jlib:remove_cdata(SubEls),
    case Action of
        [#xmlel{name = Name} = ActionEl] ->
            Node = exml_query:attr(ActionEl, <<"node">>, <<>>),
            ActionExtraArgs = #{server_host => ServerHost,
                                action_el => ActionEl,
                                lang => Lang},
            mongoose_instrument:span(event_name(IQType, Name), #{host_type => ServerHost}, fun iq_pubsub_owner_action/6,
                                     [IQType, Name, Host, From, Node, ActionExtraArgs],
                                     fun(Time, Result) -> iq_action_result_to_measurements(Time, Result, From) end);
        _ ->
            ?LOG_INFO(#{what => pubsub_too_many_actions, exml_packet => Action}),
            {error, mongoose_xmpp_errors:bad_request()}
    end.

iq_pubsub_owner_action(IQType, Name, Host, From, Node, ExtraParams) ->
    case {IQType, Name} of
        {get, <<"configure">>} ->
            get_configure(Host, Node, From, ExtraParams);
        {set, <<"configure">>} ->
            set_configure(Host, Node, From, ExtraParams);
        {get, <<"default">>} ->
            get_default(Host, Node, From, ExtraParams);
        {set, <<"delete">>} ->
            delete_node(Host, Node, From);
        {set, <<"purge">>} ->
            purge_node(Host, Node, From);
        {get, <<"subscriptions">>} ->
            get_subscriptions(Host, Node, From);
        {set, <<"subscriptions">>} ->
            set_subscriptions(Host, Node, From, ExtraParams);
        {get, <<"affiliations">>} ->
            get_affiliations(Host, Node, From);
        {set, <<"affiliations">>} ->
            set_affiliations(Host, Node, From, ExtraParams);
        _ ->
            {error, mongoose_xmpp_errors:feature_not_implemented()}
    end.

iq_command(Host, ServerHost, From, IQ, Access, Plugins) ->
    case adhoc:parse_request(IQ) of
        Req when is_record(Req, adhoc_request) ->
            case adhoc_request(Host, ServerHost, From, Req, Access, Plugins) of
                Resp when is_record(Resp, xmlel) ->
                    {result, [Resp]};
                Error ->
                    Error
            end;
        Err -> Err
    end.

%% @doc <p>Processes an Ad Hoc Command.</p>
adhoc_request(Host, _ServerHost, Owner,
              Request = #adhoc_request{node = ?NS_PUBSUB_GET_PENDING,
                                       action = <<"execute">>,
                                       xdata = false},
              _Access, Plugins) ->
    send_pending_node_form(Request, Host, Owner, Plugins);
adhoc_request(Host, _ServerHost, Owner,
              Request = #adhoc_request{node = ?NS_PUBSUB_GET_PENDING,
                                       action = <<"execute">>, xdata = XData},
              _Access, _Plugins) ->
    ParseOptions = adhoc_get_pending_parse_options(Host, XData),
    case ParseOptions of
        {result, XForm} ->
            case lists:keysearch(node, 1, XForm) of
                {value, {_, Node}} -> send_pending_auth_events(Request, Host, Node, Owner);
                false -> {error, extended_error(mongoose_xmpp_errors:bad_request(), <<"bad-payload">>)}
            end;
        Error -> Error
    end;
adhoc_request(_Host, _ServerHost, _Owner,
              #adhoc_request{action = <<"cancel">>} = Request, _Access,
              _Plugins) ->
    adhoc:produce_response(Request, canceled);
adhoc_request(Host, ServerHost, Owner,
              #adhoc_request{action = <<>>} = R, Access, Plugins) ->
    adhoc_request(Host, ServerHost, Owner,
                  R#adhoc_request{action = <<"execute">>}, Access,
                  Plugins);
adhoc_request(_Host, _ServerHost, _Owner, Other, _Access, _Plugins) ->
    ?LOG_DEBUG(#{what => pubsub_adhoc_request_error,
        text => <<"Couldn't process ad hoc command">>, command => Other}),
    {error, mongoose_xmpp_errors:item_not_found()}.

%% @doc <p>Sends the process pending subscriptions XForm for Host to Owner.</p>
send_pending_node_form(Request, Host, Owner, Plugins) ->
    Filter = fun (Type) ->
                     lists:member(<<"get-pending">>, plugin_features(Type))
             end,
    case lists:filter(Filter, Plugins) of
        [] ->
            {error, mongoose_xmpp_errors:feature_not_implemented()};
        Ps ->
            Options = get_pending_nodes(Host, Owner, Ps),
            Field = #{type => <<"list-single">>, var => <<"pubsub#node">>,
                      options => lists:usort(Options)},
            Form = mongoose_data_forms:form(#{fields => [Field]}),
            adhoc:produce_response(Request, executing, <<"execute">>, [Form])
    end.

get_pending_nodes(Host, Owner, Plugins) ->
    Tr = fun (Type) ->
                 case node_call(Type, get_pending_nodes, [Host, Owner]) of
                     {result, Nodes} -> Nodes;
                     _ -> []
                 end
         end,
    Action = fun() -> {result, lists:flatmap(Tr, Plugins)} end,
    ErrorDebug = #{
      action => get_pending_nodes,
      pubsub_host => Host,
      owner => Owner,
      plugins => Plugins
     },
    case mod_pubsub_db_backend:dirty(Action, ErrorDebug) of
        {result, Res} -> Res;
        Err -> Err
    end.

adhoc_get_pending_parse_options(Host, XEl) ->
    case mongoose_data_forms:parse_form_fields(XEl) of
        #{type := <<"submit">>, kvs := KVs} ->
            case set_xoption(Host, maps:to_list(KVs), []) of
                NewOpts when is_list(NewOpts) -> {result, NewOpts};
                Err -> Err
            end;
        #{} ->
            {error, mongoose_xmpp_errors:bad_request(<<"en">>, <<"Invalid form type">>)}
    end.

%% @doc <p>Send a subscription approval form to Owner for all pending
%% subscriptions on Host and Node.</p>
send_pending_auth_events(Request, Host, Node, Owner) ->
    ?LOG_DEBUG(#{what => pubsub_sending_pending_auth_events,
        owner => jid:to_binary(Owner), sub_host => Host, pubsub_node => Node}),
    Action = fun(PubSubNode) ->
                     get_node_subscriptions_transaction(Owner, PubSubNode)
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {N, Subs}} ->
            lists:foreach(fun
                              ({J, pending, _SubId, _}) -> send_authorization_request(N, jid:make(J));
                              (_) -> ok
                         end,
                          Subs),
            adhoc:produce_response(Request, undefined);
        Err ->
            Err
    end.

get_node_subscriptions_transaction(Owner, #pubsub_node{id = Nidx, type = Type}) ->
    case lists:member(<<"get-pending">>, plugin_features(Type)) of
        true ->
            case node_call(Type, get_affiliation, [Nidx, Owner]) of
                {result, owner} -> node_call(Type, get_node_subscriptions, [Nidx]);
                _ -> {error, mongoose_xmpp_errors:forbidden()}
            end;
        false ->
            {error, mongoose_xmpp_errors:feature_not_implemented()}
    end.

%%% authorization handling

send_authorization_request(#pubsub_node{nodeid = {Host, Node}, owners = Owners},
                           Subscriber) ->
    Lang = <<"en">>,
    Title = service_translations:do(Lang, <<"PubSub subscriber request">>),
    Instructions = service_translations:do(Lang, <<"Choose whether to approve this entity's "
                                               "subscription.">>),
    Fields = [#{var => <<"pubsub#node">>,
                type => <<"text-single">>,
                label => service_translations:do(Lang, <<"Node ID">>),
                values => [Node]},
              #{var => <<"pubsub#subscriber_jid">>,
                type => <<"jid-single">>,
                label => service_translations:do(Lang, <<"Subscriber Address">>),
                values => [jid:to_binary(Subscriber)]},
              #{var => <<"pubsub#allow">>,
                type => <<"boolean">>,
                label => service_translations:do(Lang, <<"Allow this Jabber ID to subscribe to "
                                                     "this pubsub node?">>),
                values => [<<"false">>]}],
    Form = mongoose_data_forms:form(#{title => Title, instructions => Instructions,
                                      ns => ?NS_PUBSUB_SUB_AUTH, fields => Fields}),
    Stanza = #xmlel{name = <<"message">>,
                    attrs = #{<<"id">> => mongoose_bin:gen_from_crypto()},
                    children = [Form]},
    lists:foreach(fun(Owner) ->
                          ejabberd_router:route(service_jid(Host), jid:make(Owner), Stanza)
                  end, Owners).

find_authorization_response(El) ->
    case mongoose_data_forms:find_form(El) of
        undefined ->
            none;
        Form ->
            case mongoose_data_forms:parse_form_fields(Form) of
                #{type := <<"submit">>, ns := ?NS_PUBSUB_SUB_AUTH, kvs := KVs} ->
                    KVs;
                _ ->
                    invalid
            end
    end.

%% @doc Send a message to JID with the supplied Subscription
send_authorization_approval(Host, JID, SNode, Subscription) ->
    Attrs1 = node_attr(SNode),
    Attrs2 = case Subscription of
                %  {S, SID} ->
                %      Attrs1#{<<"subscription">> => subscription_to_string(S),
                %              <<"subid">> => SID};
                 S ->
                     Attrs1#{<<"subscription">> => subscription_to_string(S)}
             end,
    Stanza = event_stanza(<<"subscription">>,
                          Attrs2#{<<"jid">> => jid:to_binary(JID)}),
    ejabberd_router:route(service_jid(Host), JID, Stanza).

handle_authorization_response(Acc, Host, From, To, Packet, XFields) ->
    case XFields of
        #{<<"pubsub#node">> := [Node],
          <<"pubsub#subscriber_jid">> := [SSubscriber],
          <<"pubsub#allow">> := [SAllow]} ->
            FromLJID = jid:to_lower(jid:to_bare(From)),
            Subscriber = jid:from_binary(SSubscriber),
            Allow = string_allow_to_boolean(SAllow),
            Action = fun (PubSubNode) ->
                             handle_authorization_response_transaction(Host, FromLJID, Subscriber,
                                                                       Allow, Node, PubSubNode)
                     end,
            case dirty(Host, Node, Action, ?FUNCTION_NAME) of
                {error, Error} ->
                    Err = make_error_reply(Packet, Error),
                    ejabberd_router:route(To, From, Acc, Err);
                {result, {_, _NewSubscription}} ->
                    %% XXX: notify about subscription state change, section 12.11
                    Acc;
                _ ->
                    Err = make_error_reply(Packet, mongoose_xmpp_errors:internal_server_error()),
                    ejabberd_router:route(To, From, Acc, Err)
            end;
        _ ->
            Err = make_error_reply(Packet, mongoose_xmpp_errors:not_acceptable()),
            ejabberd_router:route(To, From, Acc, Err)
    end.

string_allow_to_boolean(<<"1">>) -> true;
string_allow_to_boolean(<<"true">>) -> true;
string_allow_to_boolean(_) -> false.

handle_authorization_response_transaction(Host, FromLJID, Subscriber, Allow, Node,
                                          #pubsub_node{type = Type, id = Nidx, owners = Owners}) ->
    case lists:member(FromLJID, Owners) of
        true ->
            {result, Subs} = node_call(Type, get_subscriptions, [Nidx, Subscriber]),
            update_auth(Host, Node, Type, Nidx, Subscriber, Allow, Subs);
        false ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

update_auth(Host, Node, Type, Nidx, Subscriber, Allow, Subs) ->
    Sub = lists:filter(fun
                          ({pending, _, _}) -> true;
                          (_) -> false
                     end,
                      Subs),
    case Sub of
        [{pending, SubId, _}] ->
            NewSub = case Allow of
                         true -> subscribed;
                         false -> none
                     end,
            node_call(Type, set_subscriptions, [Nidx, Subscriber, NewSub, SubId]),
            send_authorization_approval(Host, Subscriber, Node, NewSub),
            {result, ok};
        _ ->
            {error, mongoose_xmpp_errors:unexpected_request()}
    end.

-define(XFIELD(Type, Label, Var, Val),
        #{type => Type,
          label => service_translations:do(Lang, Label),
          var => Var,
          values => [Val]}).

-define(BOOLXFIELD(Label, Var, Val),
        ?XFIELD(<<"boolean">>, Label, Var,
                case Val of
                    true -> <<"1">>;
                    _ -> <<"0">>
                end)).

-define(STRINGXFIELD(Label, Var, Val),
        ?XFIELD(<<"text-single">>, Label, Var, Val)).

-define(STRINGMXFIELD(Label, Var, Vals),
        #{type => <<"text-multi">>,
          label => service_translations:do(Lang, Label),
          var => Var,
          values => Vals}).

-define(XFIELDOPT(Type, Label, Var, Val, Opts),
        #{type => Type,
          label => service_translations:do(Lang, Label),
          var => Var,
          options => Opts,
          values => [Val]}).

-define(LISTXFIELD(Label, Var, Val, Opts),
        ?XFIELDOPT(<<"list-single">>, Label, Var, Val, Opts)).

-define(LISTMXFIELD(Label, Var, Vals, Opts),
        #{type => <<"list-multi">>,
          label => service_translations:do(Lang, Label),
          var => Var,
          options => Opts,
          values => Vals}).

%% @doc <p>Create new pubsub nodes</p>
%%<p>In addition to method-specific error conditions, there are several general reasons
%%   why the node creation request might fail:</p>
%%<ul>
%%<li>The service does not support node creation.</li>
%%<li>Only entities that are registered with the service are allowed to create nodes
%%    but the requesting entity is not registered.</li>
%%<li>The requesting entity does not have sufficient privileges to create nodes.</li>
%%<li>The requested Node already exists.</li>
%%<li>The request did not include a Node and "instant nodes" are not supported.</li>
%%</ul>
%%<p>Note: node creation is a particular case, error return code is evaluated at many places:</p>
%%<ul>
%%<li>iq_pubsub checks if service supports node creation (type exists)</li>
%%<li>create_node checks if instant nodes are supported</li>
%%<li>create_node asks node plugin if entity have sufficient privilege</li>
%%<li>nodetree create_node checks if nodeid already exists</li>
%%<li>node plugin create_node just sets default affiliation/subscription</li>
%%</ul>
%% @end

create_node(Host, ServerHost, Node, Owner, Type) ->
    create_node(Host, ServerHost, Node, Owner, Type, all, undefined).

-spec create_node(Host, ServerHost, Node, Owner, Type, Access, Configuration) -> R when
      Host          :: mod_pubsub:host(),
      ServerHost    :: binary(),
      Node          :: <<>> | mod_pubsub:nodeId(),
      Owner         :: jid:jid(),
      Type          :: binary(),
      Access        :: atom(),
      Configuration :: exml:element() | undefined,
      R             :: {result, [exml:element(), ...]}
                     | {error, exml:element()}.
create_node(Host, ServerHost, <<>>, Owner, Type, Access, Configuration) ->
    case lists:member(<<"instant-nodes">>, plugin_features(Type)) of
        true ->
            Node = mongoose_bin:gen_from_crypto(),
            case create_node(Host, ServerHost, Node, Owner, Type, Access, Configuration) of
                {result, _} ->
                    {result, [#xmlel{name = <<"pubsub">>,
                                     attrs = #{<<"xmlns">> => ?NS_PUBSUB},
                                     children = [#xmlel{name = <<"create">>,
                                                        attrs = node_attr(Node)}]}]};
                Error ->
                    Error
            end;
        false ->
            {error, extended_error(mongoose_xmpp_errors:not_acceptable(), <<"nodeid-required">>)}
    end;
create_node(Host, ServerHost, Node, Owner, GivenType, Access, Configuration) ->
    Type = select_type(ServerHost, Host, Node, GivenType),
    case parse_create_node_options(Host, Type, Configuration) of
        {result, NodeOptions} ->
            CreateNode = fun () ->
                                 create_node_transaction(Host, ServerHost, Node, Owner,
                                                         Type, Access, NodeOptions)
                         end,
            ErrorDebug = #{
              action => create_node,
              pubsub_host => Host,
              owner => Owner,
              node_name => Node },
            case mod_pubsub_db_backend:transaction(CreateNode, ErrorDebug) of
                {result, {Nidx, SubsByDepth, {Result, broadcast}}} ->
                    broadcast_created_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth),
                    create_node_reply(Node, Result);
                {result, {_Nidx, _SubsByDepth, Result}} ->
                    create_node_reply(Node, Result);
                Error ->
                    %% in case we change transaction to sync_dirty...
                    %%  node_call(Type, delete_node, [Host, Node]),
                    %%  tree_call(Host, delete_node, [Host, Node]),
                    Error
            end;
        Error ->
            ?LOG_INFO(#{what => pubsub_bad_node_configuration,
                        pubsub_node => Node, configuration => Configuration}),
            Error
    end.

parse_create_node_options(Host, Type, undefined) ->
    {result, node_options(Host, Type)};
parse_create_node_options(Host, Type, Configuration) ->
    case mongoose_data_forms:find_and_parse_form(Configuration) of
        #{type := <<"submit">>, kvs := KVs} ->
            case set_xoption(Host, maps:to_list(KVs), node_options(Host, Type)) of
                NewOpts when is_list(NewOpts) -> {result, NewOpts};
                Err -> Err
            end;
        #{} ->
            {error, mongoose_xmpp_errors:bad_request(<<"en">>, <<"Invalid form type">>)};
        {error, _} ->
            {result, node_options(Host, Type)}
    end.

create_node_transaction(Host, ServerHost, Node, Owner, Type, Access, NodeOptions) ->
    Parent = get_parent(Type, Node),
    case node_call(Type, create_node_permission,
                   [Host, ServerHost, Node, Parent, Owner, Access]) of
        {result, true} ->
            create_node_authorized_transaction(Host, Node, Parent, Owner, Type, NodeOptions);
        _ ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

get_parent(Type, Node) ->
    case node_call(Type, node_to_path, [Node]) of
        {result, [Node]} ->
            <<>>;
        {result, Path} ->
            element(2, node_call(Type, path_to_node, [lists:sublist(Path, length(Path)-1)]))
    end.

create_node_authorized_transaction(Host, Node, Parent, Owner, Type, NodeOptions) ->
    Parents = case Parent of
                  <<>> -> [];
                  _ -> [Parent]
              end,
    case tree_call(Host, create_node, [Host, Node, Type, Owner, NodeOptions, Parents]) of
        {ok, Nidx} ->
            SubsByDepth = get_node_subs_by_depth(Host, Node, Owner),
            case node_call(Type, create_node, [Nidx, Owner]) of
                {result, Result} -> {result, {Nidx, SubsByDepth, Result}};
                Error -> Error
            end;
        {error, {virtual, Nidx}} ->
            case node_call(Type, create_node, [Nidx, Owner]) of
                {result, Result} -> {result, {Nidx, [], Result}};
                Error -> Error
            end;
        Error ->
            Error
    end.

create_node_reply(Node, default) ->
    {result, create_node_make_reply(Node)};
create_node_reply(_Node, Result) ->
    {result, Result}.

create_node_make_reply(Node) ->
    [#xmlel{name = <<"pubsub">>,
            attrs = #{<<"xmlns">> => ?NS_PUBSUB},
            children = [#xmlel{name = <<"create">>,
                               attrs = node_attr(Node)}]}].

%% @doc <p>Delete specified node and all childs.</p>
%%<p>There are several reasons why the node deletion request might fail:</p>
%%<ul>
%%<li>The requesting entity does not have sufficient privileges to delete the node.</li>
%%<li>The node is the root collection node, which cannot be deleted.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
-spec delete_node(
        Host  :: mod_pubsub:host(),
          Node  :: mod_pubsub:nodeId(),
          Owner :: jid:jid())
        -> {result, [exml:element(), ...]} | {error, exml:element()}.
delete_node(_Host, <<>>, _Owner) ->
    {error, mongoose_xmpp_errors:not_allowed()};
delete_node(Host, Node, Owner) ->
    Action = fun (PubSubNode) -> delete_node_transaction(Host, Owner, Node, PubSubNode) end,
    case transaction(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, {SubsByDepth, {Result, broadcast, Removed}}}} ->
            lists:foreach(fun ({RNode, _RSubs}) ->
                                  {RH, RN} = RNode#pubsub_node.nodeid,
                                  RNidx = RNode#pubsub_node.id,
                                  RType = RNode#pubsub_node.type,
                                  ROptions = RNode#pubsub_node.options,
                                  broadcast_removed_node(RH, RN, RNidx,
                                                         RType, ROptions, SubsByDepth)
                          end,
                          Removed),
            case Result of
                default -> {result, []};
                _ -> {result, Result}
            end;
        {result, {_, {_, {Result, _}}}} ->
            case Result of
                default -> {result, []};
                _ -> {result, Result}
            end;
        Error ->
            Error
    end.

delete_node_transaction(Host, Owner, Node, #pubsub_node{type = Type, id = Nidx}) ->
    case node_call(Type, get_affiliation, [Nidx, Owner]) of
        {result, owner} ->
            SubsByDepth = get_node_subs_by_depth(Host, Node, service_jid(Host)),
            Removed = tree_call(Host, delete_node, [Host, Node]),
            case node_call(Type, delete_node, [Removed]) of
                {result, Res} -> {result, {SubsByDepth, Res}};
                Error -> Error
            end;
        _ ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

%% @see node_hometree:subscribe_node/5
%% @doc <p>Accepts or rejects subcription requests on a PubSub node.</p>
%%<p>There are several reasons why the subscription request might fail:</p>
%%<ul>
%%<li>The bare JID portions of the JIDs do not match.</li>
%%<li>The node has an access model of "presence" and the requesting entity
%%    is not subscribed to the owner's presence.</li>
%%<li>The node has an access model of "roster" and the requesting entity
%%    is not in one of the authorized roster groups.</li>
%%<li>The node has an access model of "whitelist"
%%    and the requesting entity is not on the whitelist.</li>
%%<li>The service requires payment for subscriptions to the node.</li>
%%<li>The requesting entity is anonymous and the service
%%    does not allow anonymous entities to subscribe.</li>
%%<li>The requesting entity has a pending subscription.</li>
%%<li>The requesting entity is blocked from subscribing
%%    (e.g., because having an affiliation of outcast).</li>
%%<li>The node does not support subscriptions.</li>
%%<li>The node does not exist.</li>
%%</ul>
-spec subscribe_node(
        Host          :: mod_pubsub:host(),
        Node          :: mod_pubsub:nodeId(),
        From          ::jid:jid(),
        JID           :: binary(),
        ConfigurationXForm :: exml:element() | undefined)
        -> {result, [exml:element(), ...]} | {error, exml:element()}.
subscribe_node(Host, Node, From, JID, ConfigurationXForm) ->
    SubOpts = case pubsub_form_utils:parse_sub_xform(ConfigurationXForm) of
                  {ok, GoodSubOpts} -> GoodSubOpts;
                  _ -> invalid
              end,
    Subscriber = string_to_ljid(JID),
    Action = fun (PubSubNode) ->
                     subscribe_node_transaction(Host, SubOpts, From, Subscriber, PubSubNode)
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {TNode, {Result, subscribed, SubId, send_last}}} ->
            Nidx = TNode#pubsub_node.id,
            Type = TNode#pubsub_node.type,
            Options = TNode#pubsub_node.options,
            send_items(Host, Node, Nidx, Type, Options, Subscriber, last),
            case Result of
                default -> {result, subscribe_node_reply(Subscriber, Node, {subscribed, SubId})};
                _ -> {result, Result}
            end;
        {result, {_TNode, {default, subscribed, SubId}}} ->
            {result, subscribe_node_reply(Subscriber, Node, {subscribed, SubId})};
        {result, {_TNode, {Result, subscribed, _SubId}}} ->
            {result, Result};
        {result, {TNode, {default, pending, _SubId}}} ->
            send_authorization_request(TNode, Subscriber),
            {result, subscribe_node_reply(Subscriber, Node, pending)};
        {result, {TNode, {Result, pending}}} ->
            send_authorization_request(TNode, Subscriber),
            {result, Result};
        {result, {_, Result}} ->
            {result, Result};
        Error -> Error
    end.

subscribe_node_transaction(Host, SubOpts, From, Subscriber, PubSubNode) ->
    Features = plugin_features(PubSubNode#pubsub_node.type),
    subscribe_node_transaction_step1(Host, SubOpts, From, Subscriber, PubSubNode, Features).

subscribe_node_transaction_step1(Host, SubOpts, From, Subscriber, PubSubNode, Features) ->
    case lists:member(<<"subscribe">>, Features) of
        false ->
            {error, unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                                      <<"subscribe">>)};
        true ->
            subscribe_node_transaction_step2(Host, SubOpts, From, Subscriber, PubSubNode, Features)
    end.

subscribe_node_transaction_step2(Host, SubOpts, From, Subscriber, PubSubNode, Features) ->
    case get_option(PubSubNode#pubsub_node.options, subscribe) of
        false ->
            {error, unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                                      <<"subscribe">>)};
        true ->
            subscribe_node_transaction_step3(Host, SubOpts, From, Subscriber, PubSubNode, Features)
    end.

subscribe_node_transaction_step3(Host, SubOpts, From, Subscriber, PubSubNode, Features) ->
    case {SubOpts /= [], lists:member(<<"subscription-options">>, Features)} of
        {true, false} ->
           {error,
            unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                              <<"subscription-options">>)};
        _ ->
            subscribe_node_transaction_step4(Host, SubOpts, From, Subscriber, PubSubNode)
    end.

subscribe_node_transaction_step4(_Host, invalid, _From, _Subscriber, _PubSubNode) ->
    {error, extended_error(mongoose_xmpp_errors:bad_request(), <<"invalid-options">>)};
subscribe_node_transaction_step4(Host, SubOpts, From, Subscriber,
                                 #pubsub_node{options = Options, type = Type,
                                              id = Nidx, owners = Owners}) ->
    case check_subs_limit(Host, Type, Nidx) of
        true ->
           {error, extended_error(mongoose_xmpp_errors:not_allowed(), <<"closed-node">>)};
        false ->
            AccessModel = get_option(Options, access_model),
            SendLast = get_option(Options, send_last_published_item),
            AllowedGroups = get_option(Options, roster_groups_allowed, []),

            {PS, RG} = get_presence_and_roster_permissions(Host, Subscriber,
                                                           Owners, AccessModel, AllowedGroups),
            node_call(Type, subscribe_node,
                      [Nidx, From, Subscriber, AccessModel,
                       SendLast, PS, RG, SubOpts])
    end.

check_subs_limit(Host, Type, Nidx) ->
    case get_max_subscriptions_node(Host) of
        Max when is_integer(Max) ->
            case node_call(Type, get_node_subscriptions, [Nidx]) of
                {result, NodeSubs} -> count_subscribed(NodeSubs) >= Max;
                _ -> false
            end;
        _ ->
            false
    end.

count_subscribed(NodeSubs) ->
    lists:foldl(
      fun ({_JID, subscribed, _SubId, _Opts}, Acc) -> Acc+1;
          (_, Acc) -> Acc
      end, 0, NodeSubs).

subscribe_node_reply(Subscriber, Node, {subscribed, SubId}) ->
    SubAttrs = #{<<"subscription">> => subscription_to_string(subscribed),
                 <<"subid">> => SubId, <<"node">> => Node},
    subscribe_node_reply(Subscriber, SubAttrs);
subscribe_node_reply(Subscriber, Node, Subscription) ->
    SubAttrs = #{<<"subscription">> => subscription_to_string(Subscription),
                 <<"node">> => Node},
    subscribe_node_reply(Subscriber, SubAttrs).

subscribe_node_reply(Subscriber, SubAttrs) ->
    [#xmlel{name = <<"pubsub">>,
            attrs = #{<<"xmlns">> => ?NS_PUBSUB},
            children = [#xmlel{name = <<"subscription">>,
                               attrs = SubAttrs#{<<"jid">> => jid:to_binary(Subscriber)}}]}].

%% @doc <p>Unsubscribe <tt>JID</tt> from the <tt>Node</tt>.</p>
%%<p>There are several reasons why the unsubscribe request might fail:</p>
%%<ul>
%%<li>The requesting entity has multiple subscriptions to the node
%%    but does not specify a subscription ID.</li>
%%<li>The request does not specify an existing subscriber.</li>
%%<li>The requesting entity does not have sufficient privileges
%%    to unsubscribe the specified JID.</li>
%%<li>The node does not exist.</li>
%%<li>The request specifies a subscription ID that is not valid or current.</li>
%%</ul>
-spec unsubscribe_node(
        Host  :: mod_pubsub:host(),
          Node  :: mod_pubsub:nodeId(),
          From  ::jid:jid(),
          JID   :: binary() | jid:ljid(),
          SubId :: mod_pubsub:subId())
        -> {result, []} | {error, exml:element()}.
unsubscribe_node(Host, Node, From, JID, SubId) when is_binary(JID) ->
    unsubscribe_node(Host, Node, From, string_to_ljid(JID), SubId);
unsubscribe_node(Host, Node, From, Subscriber, SubId) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
                     node_call(Type, unsubscribe_node, [Nidx, From, Subscriber, SubId])
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, default}} -> {result, []};
 %      {result, {_, Result}} -> {result, Result};
        Error -> Error
    end.

%% @doc <p>Publish item to a PubSub node.</p>
%% <p>The permission to publish an item must be verified by the plugin implementation.</p>
%%<p>There are several reasons why the publish request might fail:</p>
%%<ul>
%%<li>The requesting entity does not have sufficient privileges to publish.</li>
%%<li>The node does not support item publication.</li>
%%<li>The node does not exist.</li>
%%<li>The payload size exceeds a service-defined limit.</li>
%%<li>The item contains more than one payload element or the namespace of the root payload element
%%    does not match the configured namespace for the node.</li>
%%<li>The request does not match the node configuration.</li>
%%</ul>
-spec publish_item(
          Host       :: mod_pubsub:host(),
          ServerHost :: binary(),
          Node       :: mod_pubsub:nodeId(),
          Publisher  ::jid:jid(),
          ItemId     :: <<>> | mod_pubsub:itemId(),
          Payload    :: mod_pubsub:payload())
        -> {result, [exml:element(), ...]} | {error, exml:element()}.
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload) ->
    publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, all).
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, Access) ->
    publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, Access, undefined).
publish_item(Host, ServerHost, Node, Publisher, <<>>, Payload, Access, PublishOptions) ->
    publish_item(Host, ServerHost, Node, Publisher, uniqid(), Payload, Access, PublishOptions);
publish_item(Host, ServerHost, Node, Publisher, ItemId, Payload, Access, PublishOptions) ->
    ItemPublisher = config(serverhost(Host), item_publisher),
    Action =
        fun (#pubsub_node{options = Options, type = Type, id = Nidx}) ->
                Features = plugin_features(Type),
                PublishFeature = lists:member(<<"publish">>, Features),
                PubOptsFeature = lists:member(<<"publish-options">>, Features),
                PublishModel = get_option(Options, publish_model),
                DeliverPayloads = get_option(Options, deliver_payloads),
                PersistItems = get_option(Options, persist_items),
                MaxItems = max_items(Host, Options),
                PayloadCount = payload_xmlelements(Payload),
                PayloadSize = byte_size(term_to_binary(Payload)) - 2,
                PayloadMaxSize = get_option(Options, max_payload_size),

                Errors = [ %% [{Condition :: boolean(), Reason :: term()}]
                    {not PublishFeature,
                     unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                                       <<"publish">>)},
                    {not PubOptsFeature andalso PublishOptions /= undefined,
                     unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                                       <<"publish-options">>)},
                    {PayloadSize > PayloadMaxSize,
                     extended_error(mongoose_xmpp_errors:not_acceptable(), <<"payload-too-big">>)},
                    {(PayloadCount == 0) and (Payload == []),
                     extended_error(mongoose_xmpp_errors:bad_request(), <<"payload-required">>)},
                    {(PayloadCount > 1) or (PayloadCount == 0),
                     extended_error(mongoose_xmpp_errors:bad_request(), <<"invalid-payload">>)},
                    {(DeliverPayloads == false) and (PersistItems == false) and (PayloadSize > 0),
                     extended_error(mongoose_xmpp_errors:bad_request(), <<"item-forbidden">>)},
                    {((DeliverPayloads == true) or (PersistItems == true)) and (PayloadSize == 0),
                     extended_error(mongoose_xmpp_errors:bad_request(), <<"item-required">>)},
                    {PubOptsFeature andalso check_publish_options(Type, PublishOptions, Options),
                     extended_error(mongoose_xmpp_errors:conflict(), <<"precondition-not-met">>)}
                ],

                case lists:keyfind(true, 1, Errors) of
                    {true, Reason} ->
                        {error, Reason};
                    false ->
                        node_call(Type, publish_item,
                                  [ServerHost, Nidx, Publisher, PublishModel, MaxItems, ItemId,
                                   ItemPublisher, Payload, PublishOptions])
                end
        end,
    Reply = [#xmlel{name = <<"pubsub">>,
                    attrs = #{<<"xmlns">> => ?NS_PUBSUB},
                    children = [#xmlel{name = <<"publish">>, attrs = node_attr(Node),
                                       children = [#xmlel{name = <<"item">>,
                                                          attrs = item_attr(ItemId)}]}]}],
    ErrorItemNotFound = mongoose_xmpp_errors:item_not_found(),
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {TNode, {Result, Broadcast, Removed}}} ->
            Nidx = TNode#pubsub_node.id,
            Type = TNode#pubsub_node.type,
            Options = TNode#pubsub_node.options,
            BrPayload = case Broadcast of
                            broadcast -> Payload;
                            PluginPayload -> PluginPayload
                        end,
            set_cached_item(Host, Nidx, ItemId, Publisher, BrPayload),
            case get_option(Options, deliver_notifications) of
                true ->
                    broadcast_publish_item(Host, Node, Nidx, Type, Options, ItemId,
                                           Publisher, BrPayload, Removed, ItemPublisher);
                false ->
                    ok
            end,
            case Result of
                default -> {result, Reply};
                _ -> {result, Result}
            end;
        {result, {TNode, {default, Removed}}} ->
            Nidx = TNode#pubsub_node.id,
            Type = TNode#pubsub_node.type,
            Options = TNode#pubsub_node.options,
            broadcast_retract_items(Host, Publisher, Node, Nidx, Type, Options, Removed),
            set_cached_item(Host, Nidx, ItemId, Publisher, Payload),
            {result, Reply};
        {result, {TNode, {Result, Removed}}} ->
            Nidx = TNode#pubsub_node.id,
            Type = TNode#pubsub_node.type,
            Options = TNode#pubsub_node.options,
            broadcast_retract_items(Host, Publisher, Node, Nidx, Type, Options, Removed),
            set_cached_item(Host, Nidx, ItemId, Publisher, Payload),
            {result, Result};
        {result, {_, default}} ->
            {result, Reply};
        {result, {_, Result}} ->
            {result, Result};
        {error, ErrorItemNotFound} ->
            Type = select_type(ServerHost, Host, Node),
            autocreate_if_supported_and_publish(Host, ServerHost, Node, Publisher,
                                                Type, Access, ItemId, Payload, PublishOptions);
        Error ->
            Error
    end.

autocreate_if_supported_and_publish(Host, ServerHost, Node, Publisher,
                                    Type, Access, ItemId, Payload, PublishOptions) ->
    ErrorItemNotFound = mongoose_xmpp_errors:item_not_found(),
    case lists:member(<<"auto-create">>, plugin_features(Type)) of
        true ->
            case create_node(Host, ServerHost, Node, Publisher, Type, Access, PublishOptions) of
                {result,
                 [#xmlel{name = <<"pubsub">>,
                         attrs = #{<<"xmlns">> := ?NS_PUBSUB},
                         children = [#xmlel{name = <<"create">>,
                                            attrs = #{<<"node">> := NewNode}}]}]} ->
                    publish_item(Host, ServerHost, NewNode, Publisher, ItemId, Payload);
                _ ->
                    {error, ErrorItemNotFound}
            end;
        false ->
            {error, ErrorItemNotFound}
    end.

%% @doc <p>Delete item from a PubSub node.</p>
%% <p>The permission to delete an item must be verified by the plugin implementation.</p>
%%<p>There are several reasons why the item retraction request might fail:</p>
%%<ul>
%%<li>The publisher does not have sufficient privileges to delete the requested item.</li>
%%<li>The node or item does not exist.</li>
%%<li>The request does not specify a node.</li>
%%<li>The request does not include an <item/> element
%%    or the <item/> element does not specify an ItemId.</li>
%%<li>The node does not support persistent items.</li>
%%<li>The service does not support the deletion of items.</li>
%%</ul>
-spec delete_item(
        Host      :: mod_pubsub:host(),
          Node      :: mod_pubsub:nodeId(),
          Publisher ::jid:jid(),
          ItemId    :: mod_pubsub:itemId())
        -> {result, []} | {error, exml:element()}.
delete_item(Host, Node, Publisher, ItemId) ->
    delete_item(Host, Node, Publisher, ItemId, false).
delete_item(_, <<>>, _, _, _) ->
    {error, extended_error(mongoose_xmpp_errors:bad_request(), <<"node-required">>)};
delete_item(Host, Node, Publisher, ItemId, ForceNotify) ->
    Action = fun(PubSubNode) -> delete_item_transaction(Publisher, ItemId, PubSubNode) end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {TNode, {Result, broadcast}}} ->
            Nidx = TNode#pubsub_node.id,
            Type = TNode#pubsub_node.type,
            Options = TNode#pubsub_node.options,
            broadcast_retract_items(Host, Publisher, Node, Nidx, Type, Options, [ItemId], ForceNotify),
            case get_cached_item(Host, Nidx) of
                #pubsub_item{itemid = {ItemId, Nidx}} -> unset_cached_item(Host, Nidx);
                _ -> ok
            end,
            case Result of
                default -> {result, []};
                _ -> {result, Result}
            end;
        {result, {_, default}} ->
            {result, []};
        {result, {_, Result}} ->
            {result, Result};
        Error ->
            Error
    end.

delete_item_transaction(Publisher, ItemId,
                        #pubsub_node{options = Options, type = Type, id = Nidx}) ->
    Features = plugin_features(Type),
    case lists:member(<<"persistent-items">>, Features) of
        true ->
            case lists:member(<<"delete-items">>, Features) of
                true ->
                    PublishModel = get_option(Options, publish_model),
                    node_call(Type, delete_item, [Nidx, Publisher, PublishModel, ItemId]);
                false ->
                    {error,
                     unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                                       <<"delete-items">>)}
            end;
        false ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"persistent-items">>)}
    end.

%% @doc <p>Delete all items of specified node owned by JID.</p>
%%<p>There are several reasons why the node purge request might fail:</p>
%%<ul>
%%<li>The node or service does not support node purging.</li>
%%<li>The requesting entity does not have sufficient privileges to purge the node.</li>
%%<li>The node is not configured to persist items.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
-spec purge_node(
        Host  :: mod_pubsub:host(),
          Node  :: mod_pubsub:nodeId(),
          Owner :: jid:jid())
        -> {result, []} | {error, exml:element()}.
purge_node(Host, Node, Owner) ->
    Action = fun (PubSubNode) -> purge_node_transaction(Owner, PubSubNode) end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {TNode, {Result, broadcast}}} ->
            Nidx = TNode#pubsub_node.id,
            Type = TNode#pubsub_node.type,
            Options = TNode#pubsub_node.options,
            broadcast_purge_node(Host, Node, Nidx, Type, Options),
            unset_cached_item(Host, Nidx),
            case Result of
                default -> {result, []};
                _ -> {result, Result}
            end;
        {result, {_, default}} ->
            {result, []};
        {result, {_, Result}} ->
            {result, Result};
        Error ->
            Error
    end.

purge_node_transaction(Owner, #pubsub_node{options = Options, type = Type, id = Nidx}) ->
    Features = plugin_features(Type),
    case {lists:member(<<"purge-nodes">>, Features),
          lists:member(<<"persistent-items">>, Features),
          get_option(Options, persist_items)} of
        {false, _, _} ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"purge-nodes">>)};
        {_, false, _} ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"persistent-items">>)};
        {_, _, false} ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"persistent-items">>)};
        _ -> node_call(Type, purge_node, [Nidx, Owner])
    end.

%% @doc <p>Return the items of a given node.</p>
%% <p>The number of items to return is limited by MaxItems.</p>
%% <p>The permission are not checked in this function.</p>
%% @todo We probably need to check that the user doing the query has the right
%% to read the items.
-spec get_items(Host :: mod_pubsub:host(),
                Node :: mod_pubsub:nodeId(),
                From ::jid:jid(),
                SubId :: mod_pubsub:subId(),
                SMaxItems :: binary(),
                ItemIds :: [mod_pubsub:itemId()],
                Rsm :: none | jlib:rsm_in()) -> {result, [exml:element(), ...]} | {error, exml:element()}.
get_items(Host, Node, From, SubId, <<>>, ItemIds, RSM) ->
    MaxItems = case get_max_items_node(Host) of
                   undefined -> ?MAXITEMS;
                   Max -> Max
               end,
    get_items_with_limit(Host, Node, From, SubId, ItemIds, RSM, MaxItems);
get_items(Host, Node, From, SubId, SMaxItems, ItemIds, RSM) ->
    MaxItems = case catch binary_to_integer(SMaxItems) of
                   {'EXIT', _} -> {error, mongoose_xmpp_errors:bad_request()};
                   Val -> Val
               end,
    get_items_with_limit(Host, Node, From, SubId, ItemIds, RSM, MaxItems).

get_items_with_limit(_Host, _Node, _From, _SubId, _ItemIds, _RSM, {error, _} = Err) ->
    Err;
get_items_with_limit(Host, Node, From, SubId, ItemIds, RSM, MaxItems) ->
    Action = fun (PubSubNode) ->
                     get_items_transaction(Host, From, RSM, SubId, PubSubNode, MaxItems, ItemIds)
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, {Items, RsmOut}}} ->
            {result,
             [#xmlel{name = <<"pubsub">>,
                     attrs = #{<<"xmlns">> => ?NS_PUBSUB},
                     children =
                     [#xmlel{name = <<"items">>, attrs = node_attr(Node),
                             children = items_els(Items)}
                      | jlib:rsm_encode(RsmOut)]}]};
        Error ->
            Error
    end.

get_items_transaction(Host, From, RSM, SubId,
                      #pubsub_node{options = Options, type = Type, id = Nidx, owners = Owners},
                      MaxItems, ItemIds) ->
    Features = plugin_features(Type),
    case {lists:member(<<"retrieve-items">>, Features),
          lists:member(<<"persistent-items">>, Features)} of
        {false, _} ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"retrieve-items">>)};
        {_, false} ->
           {error,
            unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                              <<"persistent-items">>)};
        _ ->
            AccessModel = get_option(Options, access_model),
            AllowedGroups = get_option(Options, roster_groups_allowed, []),
            {PS, RG} = get_presence_and_roster_permissions(Host, From, Owners,
                                                           AccessModel, AllowedGroups),
            Opts = #{access_model => AccessModel,
                     presence_permission => PS,
                     roster_permission => RG,
                     rsm => RSM,
                     max_items => MaxItems,
                     item_ids => ItemIds,
                     subscription_id => SubId},

            node_call(Type, get_items_if_authorised, [Nidx, From, Opts])
    end.

get_items(Host, Node) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
                     node_call(Type, get_items, [Nidx, service_jid(Host), #{}])
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, {Items, _}}} -> Items;
        Error -> Error
    end.

get_item(Host, Node, ItemId) ->
    Action = fun (#pubsub_node{type = Type, id = Nidx}) ->
                     node_call(Type, get_item, [Nidx, ItemId])
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, Items}} -> Items;
        Error -> Error
    end.

get_allowed_items_call(Host, Nidx, From, Type, Options, Owners) ->
    case get_allowed_items_call(Host, Nidx, From, Type, Options, Owners, none) of
        {result, {Items, _RSM}} -> {result, Items};
        Error -> Error
    end.
get_allowed_items_call(Host, Nidx, From, Type, Options, Owners, RSM) ->
    AccessModel = get_option(Options, access_model),
    AllowedGroups = get_option(Options, roster_groups_allowed, []),
    {PS, RG} = get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups),
    Opts = #{access_model => AccessModel,
             presence_permission => PS,
             roster_permission => RG,
             rsm => RSM},
    node_call(Type, get_items_if_authorised, [Nidx, From, Opts]).

get_last_item(Host, Type, Nidx, LJID) ->
    case get_cached_item(Host, Nidx) of
        false ->
            case node_action(Host, Type, get_items, [Nidx, LJID, #{}]) of
                {result, {[LastItem|_], _}} -> LastItem;
                _ -> undefined
            end;
        LastItem ->
            LastItem
    end.

get_last_items(Host, Type, Nidx, LJID, Number) ->
    case node_action(Host, Type, get_items, [Nidx, LJID, #{}]) of
        {result, {Items, _}} -> lists:sublist(Items, Number);
        _ -> []
    end.

%% @doc <p>Resend the items of a node to the user.</p>
%% @todo use cache-last-item feature
send_items(Host, Node, Nidx, Type, Options, LJID, last) ->
    case get_last_item(Host, Type, Nidx, LJID) of
        undefined ->
            ok;
        LastItem ->
            Stanza = items_event_stanza(Node, [LastItem]),
            dispatch_items(Host, LJID, Options, Stanza)
    end;
send_items(Host, Node, Nidx, Type, Options, LJID, Number) when Number > 0 ->
    Stanza = items_event_stanza(Node, get_last_items(Host, Type, Nidx, Number, LJID)),
    dispatch_items(Host, LJID, Options, Stanza);
send_items(Host, Node, _Nidx, _Type, Options, LJID, _) ->
    Stanza = items_event_stanza(Node, []),
    dispatch_items(Host, LJID, Options, Stanza).

dispatch_items({_, FromS, _} = From, To, Options, Stanza) ->
    NotificationType = get_option(Options, notification_type, headline),
    Message = add_message_type(Stanza, NotificationType),
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(FromS),
    FromJid = jid:make(From),
    ToJid = jid:make(To),
    AccParams = #{host_type => HostType, lserver => FromS, location => ?LOCATION,
                  element => Message, from_jid => FromJid, to_jid => ToJid},
    Acc = mongoose_acc:new(AccParams),
    ejabberd_router:route(FromJid, ToJid, Acc);
dispatch_items(From, To, Options, Stanza) ->
    NotificationType = get_option(Options, notification_type, headline),
    Message = add_message_type(Stanza, NotificationType),
    ejabberd_router:route(service_jid(From), jid:make(To), Message).

%% @doc <p>Return the list of affiliations as an XMPP response.</p>
-spec get_affiliations(
        Host    :: mod_pubsub:host(),
        Node    :: mod_pubsub:nodeId(),
        JID     :: jid:jid(),
        Plugins :: #{plugins := [binary()]})
        -> {result, [exml:element()]} | {error, exml:element()}.
get_affiliations(Host, Node, JID, #{plugins := Plugins}) when is_list(Plugins) ->
    Result = lists:foldl(
               fun(Type, {Status, Acc}) ->
                       Features = plugin_features(Type),
                       case lists:member(<<"retrieve-affiliations">>, Features) of
                           true ->
                               {result, Affs} = node_action(Host, Type, get_entity_affiliations,
                                                            [Host, JID]),
                               {Status, [Affs | Acc]};
                           false ->
                               {{error,
                                 unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                                                   <<"retrieve-affiliations">>)},
                                Acc}
                       end
               end, {ok, []}, Plugins),
    case Result of
        {ok, Affs} ->
            Entities = lists:flatmap(
                         fun ({_, none}) ->
                                 [];
                             ({#pubsub_node{nodeid = {_, NodeId}}, Aff}) when
                                   Node == <<>> orelse Node == NodeId ->
                                 Attrs = node_attr(NodeId),
                                 [#xmlel{name = <<"affiliation">>,
                                         attrs = Attrs#{<<"affiliation">> => affiliation_to_string(Aff)}}];
                             (_) ->
                                 []
                         end,
                         lists:usort(lists:flatten(Affs))),
            {result,
             [#xmlel{name = <<"pubsub">>,
                     attrs = #{<<"xmlns">> => ?NS_PUBSUB},
                     children = [#xmlel{name = <<"affiliations">>,
                                        children = Entities}]}]};
        {Error, _} ->
            Error
    end.

-spec get_affiliations(Host :: mod_pubsub:host(), Node :: mod_pubsub:nodeId(), JID :: jid:jid()) ->
    {result, [exml:element(), ...]} | {error, exml:element()}.
get_affiliations(Host, Node, JID) ->
    Action = fun (PubSubNode) -> get_affiliations_transaction(JID, PubSubNode) end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, []}} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        {result, {_, Affs}} ->
            Entities =
            lists:flatmap(fun({_, none}) ->
                                  [];
                             ({AJID, Aff}) ->
                                  [#xmlel{
                                      name = <<"affiliation">>,
                                      attrs = #{<<"jid">> => jid:to_binary(AJID),
                                                <<"affiliation">> => affiliation_to_string(Aff)}}]
                          end, Affs),
            {result,
             [#xmlel{name = <<"pubsub">>,
                     attrs = #{<<"xmlns">> => ?NS_PUBSUB_OWNER},
                     children = [#xmlel{name = <<"affiliations">>,
                                        attrs = node_attr(Node), children = Entities}]}]};
        Error ->
            Error
    end.

get_affiliations_transaction(JID, #pubsub_node{type = Type, id = Nidx}) ->
    Features = plugin_features(Type),
    case lists:member(<<"modify-affiliations">>, Features) of
        true ->
            case node_call(Type, get_affiliation, [Nidx, JID]) of
                {result, owner} ->
                    node_call(Type, get_node_affiliations, [Nidx]);
                _ ->
                    {error, mongoose_xmpp_errors:forbidden()}
            end;
        false ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"modify-affiliations">>)}
    end.

-spec set_affiliations(
        Host        :: mod_pubsub:host(),
        Node        :: mod_pubsub:nodeId(),
        From        ::jid:jid(),
        EntitiesEls :: #{action_el := exml:element()})
        -> {result, []} | {error, exml:element() | {exml:element(), [exml:element()]}} | {error, exml:element()}.
set_affiliations(Host, Node, From, #{action_el := ActionEl} ) ->
    EntitiesEls = jlib:remove_cdata(ActionEl#xmlel.children),
    Owner = jid:to_lower(jid:to_bare(From)),
    Entities = lists:foldl(fun
                               (_, error) ->
                                  error;
                               (#xmlel{name = <<"affiliation">>} = El, Acc) ->
                                   JID = jid:from_binary(exml_query:attr(El, <<"jid">>, <<>>)),
                                   Affiliation = string_to_affiliation(
                                                   exml_query:attr(El, <<"affiliation">>, <<>>)),
                                   case (JID == error) or (Affiliation == false) of
                                       true -> error;
                                       false -> [{jid:to_lower(JID), Affiliation} | Acc]
                                   end;
                               (_, _) ->
                                   error
                          end,
                           [], EntitiesEls),
    case Entities of
        error ->
            {error, mongoose_xmpp_errors:bad_request()};
        _ ->
            Action = fun (PubSubNode) ->
                             set_affiliations_transaction(Host, Owner, PubSubNode, Entities)
                     end,
            case dirty(Host, Node, Action, ?FUNCTION_NAME) of
                {result, {_, Result}} -> {result, Result};
                Other -> Other
            end
    end.

set_affiliations_transaction(Host, Owner,
                             #pubsub_node{owners = Owners, nodeid = {_, NodeId}} = N, Entities) ->
    case lists:member(Owner, Owners) of
        true ->
            % It is a very simple check, as XEP doesn't state any
            % other invalid affiliation transitions
            OwnersDryRun = lists:foldl(
                             fun({JID, owner}, Acc) ->
                                     sets:add_element(jid:to_bare(JID), Acc);
                                ({JID, _}, Acc) ->
                                     sets:del_element(jid:to_bare(JID), Acc)
                             end, sets:from_list(Owners), Entities),
            case sets:size(OwnersDryRun) of
                0 ->
                    OwnersPayload = [ #xmlel{ name = <<"affiliation">>,
                                              attrs = #{<<"jid">> => jid:to_binary(Unchanged),
                                                        <<"affiliation">> => <<"owner">>} }
                                      || Unchanged <- Owners ],
                    AffiliationsPayload = #xmlel{ name = <<"affiliations">>,
                                                  attrs = #{<<"node">> => NodeId},
                                                  children = OwnersPayload },
                    NewPubSubPayload = #xmlel{ name = <<"pubsub">>,
                                               attrs = #{<<"xmlns">> => ?NS_PUBSUB_OWNER},
                                               children = [AffiliationsPayload] },
                    {error, {mongoose_xmpp_errors:not_acceptable(), [NewPubSubPayload]}};
                _ ->
                    set_validated_affiliations_transaction(Host, N, Owners, Entities),
                    {result, []}
            end;
        _ ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

set_validated_affiliations_transaction(Host, #pubsub_node{ type = Type, id = Nidx } = N,
                                       Owners, Entities) ->
    lists:foreach(fun ({JID, owner}) ->
                          node_call(Type, set_affiliation, [Nidx, JID, owner]),
                          NewOwner = jid:to_bare(JID),
                          NewOwners = [NewOwner | Owners],
                          tree_call(Host,
                                    set_node,
                                    [N#pubsub_node{owners = NewOwners}]);
                      ({JID, none}) ->
                          node_call(Type, set_affiliation, [Nidx, JID, none]),
                          OldOwner = jid:to_bare(JID),
                          case lists:member(OldOwner, Owners) of
                              true ->
                                  NewOwners = Owners -- [OldOwner],
                                  tree_call(Host,
                                            set_node,
                                            [N#pubsub_node{owners = NewOwners}]);
                              _ ->
                                  ok
                          end;
                      ({JID, Affiliation}) ->
                          node_call(Type, set_affiliation, [Nidx, JID, Affiliation])
                  end,
                  Entities).

get_options(Host, Node, JID, SubId, Lang) ->
    Action = fun(PubSubNode) ->
                     get_options_transaction(Node, JID, SubId, Lang, PubSubNode)
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_Node, XForm}} -> {result, [XForm]};
        Error -> Error
    end.

get_options_transaction(Node, JID, SubId, Lang, #pubsub_node{type = Type, id = Nidx}) ->
    case lists:member(<<"subscription-options">>, plugin_features(Type)) of
        true ->
            get_sub_options_xml(JID, Lang, Node, Nidx, SubId, Type);
        false ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"subscription-options">>)}
    end.

% TODO: Support Lang at some point again
get_sub_options_xml(JID, _Lang, Node, Nidx, RequestedSubId, Type) ->
    Subscriber = string_to_ljid(JID),
    {result, Subs} = node_call(Type, get_subscriptions, [Nidx, Subscriber]),
    SubscribedSubs = [{Id, Opts} || {Sub, Id, Opts} <- Subs, Sub == subscribed],

    case {RequestedSubId, SubscribedSubs} of
        {_, []} ->
            {error, extended_error(mongoose_xmpp_errors:not_acceptable(), <<"not-subscribed">>)};
        {<<>>, [{TheOnlySID, Opts}]} ->
            make_and_wrap_sub_xform(Opts, Node, Subscriber, TheOnlySID);
        {<<>>, _} ->
            {error, extended_error(mongoose_xmpp_errors:not_acceptable(), <<"subid-required">>)};
        {_, _} ->
            case lists:keyfind(RequestedSubId, 1, SubscribedSubs) of
                {_, Opts} ->
                    make_and_wrap_sub_xform(Opts, Node, Subscriber, RequestedSubId);
                _ ->
                    {error, extended_error(mongoose_xmpp_errors:not_acceptable(),
                                           <<"invalid-subid">>)}
            end
    end.

make_and_wrap_sub_xform(Options, Node, Subscriber, SubId) ->
    {ok, XForm} = pubsub_form_utils:make_sub_xform(Options),
    Attrs = node_attr(Node),
    OptionsEl = #xmlel{name = <<"options">>,
                       attrs = Attrs#{<<"jid">> => jid:to_binary(Subscriber),
                                      <<"subid">> => SubId},
                       children = [XForm]},
    PubSubEl = #xmlel{name = <<"pubsub">>,
                      attrs = #{<<"xmlns">> => ?NS_PUBSUB},
                      children = [OptionsEl]},
    {result, PubSubEl}.

set_options(Host, Node, JID, SubId, ConfigXForm) ->
    Action = fun(PubSubNode) ->
                     ok = set_options_transaction(JID, SubId, ConfigXForm, PubSubNode),
                     {result, []}
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_Node, Result}} -> {result, Result};
        Error -> Error
    end.

set_options_transaction(JID, SubId, ConfigXForm, #pubsub_node{type = Type, id = Nidx}) ->
    case lists:member(<<"subscription-options">>, plugin_features(Type)) of
        true ->
            validate_and_set_options_helper(ConfigXForm, JID, Nidx, SubId, Type);
        false ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"subscription-options">>)}
    end.

validate_and_set_options_helper(ConfigXForm, JID, Nidx, SubId, Type) ->
    SubOpts = pubsub_form_utils:parse_sub_xform(ConfigXForm),
    set_options_helper(SubOpts, JID, Nidx, SubId, Type).

set_options_helper({error, Reason}, JID, Nidx, RequestedSubId, _Type) ->
    % TODO: Make smarter logging (better details formatting)
    ?LOG_DEBUG(#{what => pubsub_invalid_subscription_options, jid => JID,
        nidx => Nidx, sub_id => RequestedSubId, reason => Reason}),
    {error, extended_error(mongoose_xmpp_errors:bad_request(), <<"invalid-options">>)};
set_options_helper({ok, []}, _JID, _Nidx, _RequestedSubId, _Type) ->
    {result, []};
set_options_helper({ok, SubOpts}, JID, Nidx, RequestedSubId, Type) ->
    Subscriber = string_to_ljid(JID),
    {result, Subs} = node_call(Type, get_subscriptions, [Nidx, Subscriber]),
    SubIds = [Id || {Sub, Id, _Opts} <- Subs, Sub == subscribed],
    case {RequestedSubId, SubIds} of
        {_, []} ->
            {error, extended_error(mongoose_xmpp_errors:not_acceptable(), <<"not-subscribed">>)};
        {<<>>, [TheOnlySID]} ->
            mod_pubsub_db_backend:set_subscription_opts(Nidx, Subscriber, TheOnlySID, SubOpts);
        {<<>>, _} ->
            {error, extended_error(mongoose_xmpp_errors:not_acceptable(), <<"subid-required">>)};
        {_, _} ->
            case lists:member(RequestedSubId, SubIds) of
                true ->
                    mod_pubsub_db_backend:set_subscription_opts(Nidx, Subscriber, RequestedSubId,
                                                                SubOpts);
                false ->
                    {error, extended_error(mongoose_xmpp_errors:not_acceptable(),
                                           <<"invalid-subid">>)}
            end
    end.

%% @doc <p>Return the list of subscriptions as an XMPP response.</p>
-spec get_subscriptions(Host, Node, JID, Plugins) -> {error, Reason} | {result, Response} when
    Host :: host(),
    Node :: pubsubNode(),
    JID :: jid:jid(),
    Plugins :: map(),
    Reason :: any(),
    Response :: [exml:element()].
get_subscriptions(Host, Node, JID, #{plugins := Plugins}) when is_list(Plugins) ->
    Result = lists:foldl(fun (Type, {Status, Acc}) ->
                                 Features = plugin_features(Type),
                                 case lists:member(<<"retrieve-subscriptions">>, Features) of
                                     true ->
                                         Subscriber = jid:to_bare(JID),
                                         {result, Subs} = node_action(Host, Type,
                                                                      get_entity_subscriptions,
                                                                      [Host, Subscriber]),
                                         {Status, [Subs | Acc]};
                                     false ->
                                         {{error,
                                           unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                                                             <<"retrieve-subscriptions">>)},
                                          Acc}
                                 end
                         end,
                         {ok, []}, Plugins),
    case Result of
        {ok, Subs} ->
            Entities = lists:flatmap(fun(Sub) -> subscription_to_xmlel(Sub, Node) end,
                                     lists:usort(lists:flatten(Subs))),
            {result,
             [#xmlel{name = <<"pubsub">>,
                     attrs = #{<<"xmlns">> => ?NS_PUBSUB},
                     children = [#xmlel{name = <<"subscriptions">>,
                                        children = Entities}]}]};
        {Error, _} ->
            Error
    end.

%% 2-element tuples are not used by any node type probably
subscription_to_xmlel({_, none}, _Node) ->
    [];
subscription_to_xmlel({#pubsub_node{nodeid = {_, SubsNode}}, Sub}, <<>>) ->
    Attrs = node_attr(SubsNode),
    [#xmlel{name = <<"subscription">>,
            attrs = Attrs#{<<"subscription">> => subscription_to_string(Sub)}}];
subscription_to_xmlel({#pubsub_node{nodeid = {_, SubsNode}}, Sub}, SubsNode) ->
    [#xmlel{name = <<"subscription">>,
            attrs = #{<<"subscription">> => subscription_to_string(Sub)}}];
subscription_to_xmlel({#pubsub_node{nodeid = {_, _}}, _}, _) ->
    [];
%% no idea how to trigger this one
subscription_to_xmlel({_, none, _}, _Node) ->
    [];
%% sometimes used by node_pep
subscription_to_xmlel({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubId, SubJID}, <<>>) ->
    Attrs = node_attr(SubsNode),
    [#xmlel{name = <<"subscription">>,
            attrs = Attrs#{<<"jid">> => jid:to_binary(SubJID),
                           <<"subid">> => SubId,
                           <<"subscription">> => subscription_to_string(Sub)}}];
subscription_to_xmlel({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubId, SubJID}, SubsNode) ->
    [#xmlel{name = <<"subscription">>,
            attrs =#{<<"jid">> => jid:to_binary(SubJID),
                     <<"subid">> => SubId,
                     <<"subscription">> => subscription_to_string(Sub)}}];
subscription_to_xmlel({#pubsub_node{nodeid = {_, _}}, _, _, _}, _Node) ->
    [];
%% used by node_flat (therefore by dag, hometree and push as well)
subscription_to_xmlel({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubJID}, <<>>) ->
    Attrs = node_attr(SubsNode),
    [#xmlel{name = <<"subscription">>,
            attrs = Attrs#{<<"jid">> => jid:to_binary(SubJID),
                           <<"subscription">> => subscription_to_string(Sub)}}];
subscription_to_xmlel({#pubsub_node{nodeid = {_, SubsNode}}, Sub, SubJID}, SubsNode) ->
    [#xmlel{name = <<"subscription">>,
            attrs = #{<<"jid">> => jid:to_binary(SubJID),
                      <<"subscription">> => subscription_to_string(Sub)}}];
subscription_to_xmlel({#pubsub_node{nodeid = {_, _}}, _, _}, _Node) ->
    [].

get_subscriptions(Host, Node, JID) ->
    Action = fun (PubSubNode) -> get_subscriptions_transaction(JID, PubSubNode) end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, Subs}} ->
            Entities =
            lists:flatmap(fun({_, pending, _, _}) ->
                                  [];
                             ({AJID, Sub, SubId, _}) ->
                                  [#xmlel{name = <<"subscription">>,
                                          attrs =#{
                                            <<"jid">> => jid:to_binary(AJID),
                                            <<"subscription">> => subscription_to_string(Sub),
                                            <<"subid">> => SubId}}]
                          end, Subs),
            {result,
             [#xmlel{name = <<"pubsub">>,
                     attrs = #{<<"xmlns">> => ?NS_PUBSUB_OWNER},
                     children = [#xmlel{name = <<"subscriptions">>,
                                        attrs = node_attr(Node),
                                        children = Entities}]}]};
        Error ->
            Error
    end.

get_subscriptions_transaction(JID, #pubsub_node{type = Type, id = Nidx}) ->
    Features = plugin_features(Type),
    case lists:member(<<"manage-subscriptions">>, Features) of
        true ->
            case node_call(Type, get_affiliation, [Nidx, JID]) of
                {result, owner} ->
                    node_call(Type, get_node_subscriptions, [Nidx]);
                _ ->
                    {error, mongoose_xmpp_errors:forbidden()}
            end;
        false ->
            {error,
             unsupported_error(mongoose_xmpp_errors:feature_not_implemented(),
                               <<"manage-subscriptions">>)}
    end.

get_subscriptions_for_send_last(Host, PType, [JID, LJID, BJID]) ->
    {result, Subs} = node_action(Host, PType,
                                 get_entity_subscriptions,
                                 [Host, JID]),
    [{Node, Sub, SubId, SubJID}
     || {Node, Sub, SubId, SubJID} <- Subs,
        Sub =:= subscribed, (SubJID == LJID) or (SubJID == BJID),
        match_option(Node, send_last_published_item, on_sub_and_presence)];
get_subscriptions_for_send_last(_Host, _PType, _JIDs) ->
    [].

set_subscriptions(Host, Node, From, #{action_el := ActionEl} ) ->
    EntitiesEls = jlib:remove_cdata(ActionEl#xmlel.children),
    Owner = jid:to_lower(jid:to_bare(From)),
    Entities = lists:foldl(fun(_, error) ->
                                   error;
                              (#xmlel{name = <<"subscription">>} = El, Acc) ->
                                   JID = jid:from_binary(exml_query:attr(El, <<"jid">>, <<>>)),
                                   Sub = string_to_subscription(
                                           exml_query:attr(El, <<"subscription">>, <<>>)),
                                   SubId = exml_query:attr(El, <<"subid">>, <<>>),
                                   case (JID == error) or (Sub == false) of
                                       true -> error;
                                       false -> [{jid:to_lower(JID), Sub, SubId} | Acc]
                                   end;
                              (_, _) ->
                                   error
                           end, [], EntitiesEls),
    case Entities of
        error ->
            {error, mongoose_xmpp_errors:bad_request()};
        _ ->
            Action = fun (PubSubNode) ->
                             set_subscriptions_transaction(Host, Owner, Node, PubSubNode, Entities)
                     end,
            case dirty(Host, Node, Action, ?FUNCTION_NAME) of
                {result, {_, Result}} -> {result, Result};
                Other -> Other
            end
    end.

set_subscriptions_transaction(Host, Owner, Node,
                              #pubsub_node{type = Type, id = Nidx, owners = Owners}, Entities) ->
    case lists:member(Owner, Owners) of
        true ->
            Result =
            lists:foldl(fun(Entity, Acc) ->
                                set_subscription_transaction(Host, Node, Nidx, Type, Entity, Acc)
                        end,
                        [], Entities),
            case Result of
                [] -> {result, []};
                _ -> {error, mongoose_xmpp_errors:not_acceptable()}
            end;
        _ ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

set_subscription_transaction(Host, Node, Nidx, Type, {JID, Sub, SubId}, Acc) ->
    case node_call(Type, set_subscriptions, [Nidx, JID, Sub, SubId]) of
        {error, Err} -> [{error, Err} | Acc];
        _ -> notify_subscription_change(Host, Node, JID, Sub), Acc
    end.

notify_subscription_change(Host, Node, JID, Sub) ->
    Attrs = node_attr(Node),
    SubscriptionEl = #xmlel{name = <<"subscription">>,
                            attrs = Attrs#{<<"jid">> => jid:to_binary(JID),
                                           <<"subscription">> => subscription_to_string(Sub)}},
    PubSubEl = #xmlel{name = <<"pubsub">>,
                      attrs = #{<<"xmlns">> => ?NS_PUBSUB},
                      children = [SubscriptionEl]},
    Stanza = #xmlel{name = <<"message">>, children = [PubSubEl]},
    ejabberd_router:route(service_jid(Host), jid:make(JID), Stanza).

-spec get_presence_and_roster_permissions(Host :: mod_pubsub:host(),
                                          From :: jid:jid() | jid:ljid(),
                                          Owners :: [jid:ljid(), ...],
                                          AccessModel :: mod_pubsub:accessModel(),
                                          AllowedGroups :: [binary()]) ->
    {PresenceSubscription :: boolean(), RosterGroup :: boolean()}.
get_presence_and_roster_permissions(Host, From, Owners, AccessModel, AllowedGroups)
      when (AccessModel == presence) orelse (AccessModel == roster) ->
    case Host of
        {User, Server, _} ->
            get_roster_info(User, Server, From, AllowedGroups);
        _ ->
            [{OUser, OServer, _} | _] = Owners,
            get_roster_info(OUser, OServer, From, AllowedGroups)
    end;
get_presence_and_roster_permissions(_Host, _From, _Owners, _AccessModel, _AllowedGroups) ->
    {true, true}.

get_roster_info(_, _, {<<>>, <<>>, _}, _) ->
    {false, false};
get_roster_info(OwnerUser, OwnerServer, {SubscriberUser, SubscriberServer, _}, AllowedGroups) ->
    LJID = {SubscriberUser, SubscriberServer, <<>>},
    {Subscription, Groups} = mongoose_hooks:roster_get_jid_info(
                               OwnerServer,
                               jid:make_bare(OwnerUser, OwnerServer), LJID),
    PresenceSubscription = Subscription == both orelse
        Subscription == from orelse
        {OwnerUser, OwnerServer} == {SubscriberUser, SubscriberServer},
    RosterGroup = lists:any(fun (Group) ->
                                    lists:member(Group, AllowedGroups)
                            end,
                            Groups),
    {PresenceSubscription, RosterGroup};
get_roster_info(OwnerUser, OwnerServer, JID, AllowedGroups) ->
    get_roster_info(OwnerUser, OwnerServer, jid:to_lower(JID), AllowedGroups).

string_to_affiliation(<<"owner">>) -> owner;
string_to_affiliation(<<"publisher">>) -> publisher;
string_to_affiliation(<<"publish-only">>) -> publish_only;
string_to_affiliation(<<"member">>) -> member;
string_to_affiliation(<<"outcast">>) -> outcast;
string_to_affiliation(<<"none">>) -> none;
string_to_affiliation(_) -> false.

string_to_subscription(<<"subscribed">>) -> subscribed;
string_to_subscription(<<"pending">>) -> pending;
string_to_subscription(<<"none">>) -> none;
string_to_subscription(_) -> false.

affiliation_to_string(owner) -> <<"owner">>;
affiliation_to_string(publisher) -> <<"publisher">>;
affiliation_to_string(publish_only) -> <<"publish-only">>;
affiliation_to_string(member) -> <<"member">>;
affiliation_to_string(outcast) -> <<"outcast">>;
affiliation_to_string(_) -> <<"none">>.

subscription_to_string(subscribed) -> <<"subscribed">>;
subscription_to_string(pending) -> <<"pending">>;
subscription_to_string(_) -> <<"none">>.

-spec service_jid(mod_pubsub:host()) -> jid:jid().
service_jid({U, S, _}) ->
    jid:make_bare(U, S);
service_jid(Host) ->
    jid:make_bare(<<>>, Host).

%% @doc <p>Check if a notification must be delivered or not based on
%% node and subscription options.</p>
-spec is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) -> boolean() when
    LJID :: jid:ljid(),
    NotifyType :: items | nodes,
    Depth :: integer(),
    NodeOptions :: [{atom(), term()}],
    SubOptions :: [{atom(), term()}].
is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) ->
    sub_to_deliver(LJID, NotifyType, Depth, SubOptions)
        andalso node_to_deliver(LJID, NodeOptions).

sub_to_deliver(_LJID, NotifyType, Depth, SubOptions) ->
    lists:all(fun (Option) ->
                      sub_option_can_deliver(NotifyType, Depth, Option)
              end,
              SubOptions).

node_to_deliver(LJID, NodeOptions) ->
    presence_can_deliver(LJID, get_option(NodeOptions, presence_based_delivery)).

sub_option_can_deliver(items, _, {subscription_type, nodes}) -> false;
sub_option_can_deliver(nodes, _, {subscription_type, items}) -> false;
sub_option_can_deliver(_, _, {subscription_depth, all}) -> true;
sub_option_can_deliver(_, Depth, {subscription_depth, D}) -> Depth =< D;
sub_option_can_deliver(_, _, {deliver, false}) -> false;
sub_option_can_deliver(_, _, {expire, When}) -> timestamp() < When;
sub_option_can_deliver(_, _, _) -> true.

-spec presence_can_deliver(Entity :: jid:ljid(), PresenceBasedDelivery :: boolean()) -> boolean().
presence_can_deliver(_, false) ->
    true;
presence_can_deliver({User, Server, <<>>}, true) ->
    ejabberd_sm:get_user_present_resources(jid:make_noprep(User, Server, <<>>)) =/= [];
presence_can_deliver({User, Server, Resource}, true) ->
    JID = jid:make_noprep(User, Server, Resource),
    case ejabberd_sm:get_session(JID) of
        #session{priority = SPriority} when SPriority /= undefined -> true;
        _ -> false
    end.

-spec state_can_deliver(
        Entity::jid:ljid(),
          SubOptions :: mod_pubsub:subOptions() | [])
        -> [jid:ljid()].
state_can_deliver({U, S, R}, []) -> [{U, S, R}];
state_can_deliver({U, S, R}, SubOptions) ->
    case lists:keysearch(show_values, 1, SubOptions) of
        %% If not in suboptions, item can be delivered, case doesn't apply
        false -> [{U, S, R}];
        %% If in a suboptions ...
        {_, {_, ShowValues}} ->
            Resources = case R of
                            %% If the subscriber JID is a bare one, get all its resources
                            <<>> -> user_resources(U, S);
                            %% If the subscriber JID is a full one, use its resource
                            R -> [R]
                        end,
            lists:foldl(fun (Resource, Acc) ->
                                get_resource_state({U, S, Resource}, ShowValues, Acc)
                        end,
                        [], Resources)
    end.

-spec get_resource_state(
        Entity     :: jid:ljid(),
          ShowValues :: [binary()],
          JIDs       :: [jid:ljid()])
        -> [jid:ljid()].
get_resource_state({U, S, R}, ShowValues, JIDs) ->
    case ejabberd_sm:get_session_pid(jid:make_noprep(U, S, R)) of
        none ->
            %% If no PID, item can be delivered
            lists:append([{U, S, R}], JIDs);
        Pid ->
            Show = case mod_presence:get_presence(Pid) of
                       {_, _, <<"available">>, _} -> <<"online">>;
                       {_, _, State, _} -> State
                   end,
            case lists:member(Show, ShowValues) of
                %% If yes, item can be delivered
                true -> lists:append([{U, S, R}], JIDs);
                %% If no, item can't be delivered
                false -> JIDs
            end
    end.

-spec payload_xmlelements(
        Payload :: mod_pubsub:payload())
        -> Count :: non_neg_integer().
payload_xmlelements(Payload) ->
    payload_xmlelements(Payload, 0).

payload_xmlelements([], Count) -> Count;
payload_xmlelements([#xmlel{} | Tail], Count) ->
    payload_xmlelements(Tail, Count + 1);
payload_xmlelements([_ | Tail], Count) ->
    payload_xmlelements(Tail, Count).

items_event_stanza(Node, Items) ->
    MoreEls =
    case Items of
        [LastItem] ->
            {ModifNow, ModifUSR} = LastItem#pubsub_item.modification,
            Sec = erlang:convert_time_unit(ModifNow, microsecond, second),
            TString = calendar:system_time_to_rfc3339(Sec, [{offset, "Z"}]),
            [#xmlel{name = <<"delay">>,
                    attrs = #{<<"xmlns">> => ?NS_DELAY,
                              <<"from">> => jid:to_binary(ModifUSR),
                              <<"stamp">> => list_to_binary(TString)},
                    children = [#xmlcdata{content =  <<>>}]}];
        _ ->
            []
    end,
    Attrs = node_attr(Node),
    event_stanza_with_els([#xmlel{name = <<"items">>,
                                  attrs = Attrs#{<<"type">> => <<"headline">>},
                                  children = items_els(Items)}],
                          MoreEls).

event_stanza(Els) ->
    event_stanza_with_els(Els, []).
event_stanza_with_els(Els, MoreEls) ->
    #xmlel{name = <<"message">>,
           children = [#xmlel{name = <<"event">>,
                              attrs = #{<<"xmlns">> => ?NS_PUBSUB_EVENT},
                              children = Els}
                       | MoreEls]}.

event_stanza(Event, EvAttr) ->
    event_stanza_with_els([#xmlel{name = Event, attrs = EvAttr}], []).

%%%%%% broadcast functions

broadcast_publish_item(Host, Node, Nidx, Type, NodeOptions,
                       ItemId, From, Payload, Removed, ItemPublisher) ->
    case get_collection_subscriptions(Host, Node) of
        SubsByDepth when is_list(SubsByDepth) ->
            Content = case get_option(NodeOptions, deliver_payloads) of
                          true -> Payload;
                          false -> []
                      end,
            ItemAttr = case ItemPublisher of
                           true  -> item_attr(ItemId, From);
                           false -> item_attr(ItemId)
                       end,
            Stanza = event_stanza(
                       [#xmlel{name = <<"items">>, attrs = node_attr(Node),
                               children = [#xmlel{name = <<"item">>, attrs = ItemAttr,
                                                  children = Content}]}]),
            broadcast_step(Host, fun() ->
                broadcast_stanza(Host, From, Node, Nidx, Type,
                                 NodeOptions, SubsByDepth, items, Stanza, true),
                broadcast_auto_retract_notification(Host, From, Node, Nidx, Type,
                                                    NodeOptions, SubsByDepth, Removed)
                end),
            {result, true};
        _ ->
            {result, false}
    end.

broadcast_auto_retract_notification(_Host, _ReplyToJID, _Node, _Nidx, _Type, _NodeOptions,
                                    _SubsByDepth, []) ->
    ok;
broadcast_auto_retract_notification(Host, ReplyToJID, Node, Nidx, Type, NodeOptions,
                                    SubsByDepth, Removed) ->
    case get_option(NodeOptions, notify_retract) of
        true ->
            RetractEls = [#xmlel{name = <<"retract">>, attrs = item_attr(RId)} || RId <- Removed],
            RetractStanza = event_stanza([#xmlel{name = <<"items">>, attrs = node_attr(Node),
                                                 children = RetractEls}]),
            broadcast_stanza(Host, ReplyToJID, Node, Nidx, Type, NodeOptions, SubsByDepth,
                             items, RetractStanza, true);
        _ ->
            ok
    end.

broadcast_retract_items(Host, ReplyToJID, Node, Nidx, Type, NodeOptions, ItemIds) ->
    broadcast_retract_items(Host, ReplyToJID, Node, Nidx, Type, NodeOptions, ItemIds, false).
broadcast_retract_items(_Host, _ReplyToJID, _Node, _Nidx, _Type, _NodeOptions, [], _ForceNotify) ->
    {result, false};
broadcast_retract_items(Host, ReplyToJID, Node, Nidx, Type, NodeOptions, ItemIds, ForceNotify) ->
    case (get_option(NodeOptions, notify_retract) or ForceNotify) of
        true ->
            case get_collection_subscriptions(Host, Node) of
                SubsByDepth when is_list(SubsByDepth) ->
                    Stanza = event_stanza(
                               [#xmlel{name = <<"items">>, attrs = node_attr(Node),
                                       children = [#xmlel{name = <<"retract">>,
                                                          attrs = item_attr(ItemId)}
                                                   || ItemId <- ItemIds]}]),
                    broadcast_step(Host, fun() ->
                        broadcast_stanza(Host, ReplyToJID, Node, Nidx, Type,
                                         NodeOptions, SubsByDepth, items, Stanza, true)
                        end),
                    {result, true};
                _ ->
                    {result, false}
            end;
        _ ->
            {result, false}
    end.

broadcast_purge_node(Host, Node, Nidx, Type, NodeOptions) ->
    case get_option(NodeOptions, notify_retract) of
        true ->
            case get_collection_subscriptions(Host, Node) of
                SubsByDepth when is_list(SubsByDepth) ->
                    Stanza = event_stanza(
                               [#xmlel{name = <<"purge">>, attrs = node_attr(Node)}]),
                    broadcast_step(Host, fun() ->
                        broadcast_stanza(Host, Node, Nidx, Type,
                                         NodeOptions, SubsByDepth, nodes, Stanza, false)
                        end),
                    {result, true};
                _ ->
                    {result, false}
            end;
        _ ->
            {result, false}
    end.

broadcast_removed_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth) ->
    case get_option(NodeOptions, notify_delete) of
        true ->
            case SubsByDepth of
                [] ->
                    {result, false};
                _ ->
                    Stanza = event_stanza(
                               [#xmlel{name = <<"delete">>, attrs = node_attr(Node)}]),
                    broadcast_step(Host, fun() ->
                        broadcast_stanza(Host, Node, Nidx, Type,
                                         NodeOptions, SubsByDepth, nodes, Stanza, false)
                        end),
                    {result, true}
            end;
        _ ->
            {result, false}
    end.

broadcast_created_node(_, _, _, _, _, []) ->
    {result, false};
broadcast_created_node(Host, Node, Nidx, Type, NodeOptions, SubsByDepth) ->
    Stanza = event_stanza([#xmlel{name = <<"create">>, attrs = node_attr(Node)}]),
    broadcast_step(Host, fun() ->
        broadcast_stanza(Host, Node, Nidx, Type, NodeOptions, SubsByDepth, nodes, Stanza, true)
        end),
    {result, true}.

broadcast_config_notification(Host, Node, Nidx, Type, NodeOptions, Lang) ->
    case get_option(NodeOptions, notify_config) of
        true ->
            case get_collection_subscriptions(Host, Node) of
                SubsByDepth when is_list(SubsByDepth) ->
                    Content = payload_by_option(Type, NodeOptions, Lang),
                    Stanza = event_stanza([#xmlel{name = <<"configuration">>,
                                                  attrs = node_attr(Node), children = Content}]),
                    broadcast_step(Host, fun() ->
                        broadcast_stanza(Host, Node, Nidx, Type,
                                         NodeOptions, SubsByDepth, nodes, Stanza, false)
                        end),
                    {result, true};
                _ ->
                    {result, false}
            end;
        _ ->
            {result, false}
    end.

payload_by_option(_Type, NodeOptions, Lang) ->
    case get_option(NodeOptions, deliver_payloads) of
        true ->
            [configure_form(<<"result">>, NodeOptions, Lang, [])];
        false ->
            []
    end.

get_collection_subscriptions(Host, Node) ->
    Action = fun() ->
                     {result, get_node_subs_by_depth(Host, Node, service_jid(Host))}
             end,
    ErrorDebug = #{
      pubsub_host => Host,
      action => get_collection_subscriptions,
      node_name => Node
     },
    case mod_pubsub_db_backend:dirty(Action, ErrorDebug) of
        {result, CollSubs} -> CollSubs;
        _ -> []
    end.

get_node_subs_by_depth(Host, Node, From) ->
    ParentTree = tree_call(Host, get_parentnodes_tree, [Host, Node, From]),
    [{Depth, [{N, get_node_subs(N)} || N <- Nodes]} || {Depth, Nodes} <- ParentTree].

get_node_subs(#pubsub_node{type = Type, id = Nidx}) ->
    case node_call(Type, get_node_subscriptions, [Nidx]) of
        {result, Subs} ->
            % TODO: Replace with proper DB/plugin call with sub type filter
            [{JID, SubID, Opts} || {JID, SubType, SubID, Opts} <- Subs, SubType == subscribed];
        Other ->
            Other
    end.

%% Execute broadcasting step in a new process
%% F contains one or more broadcast_stanza calls, executed sequentially
broadcast_step(Host, F) ->
    case gen_mod:get_module_opt(serverhost(Host), ?MODULE, sync_broadcast) of
        true ->
            F();
        false ->
            proc_lib:spawn(F)
    end.

broadcast_stanza(Host, Node, _Nidx, _Type, NodeOptions,
                 SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    NotificationType = get_option(NodeOptions, notification_type, headline),
    %% Option below is not standard, but useful
    BroadcastAll = get_option(NodeOptions, broadcast_all_resources),
    From = service_jid(Host),
    Stanza = add_message_type(BaseStanza, NotificationType),
    %% Handles explicit subscriptions
    SubIDsByJID = subscribed_nodes_by_jid(NotifyType, SubsByDepth),
    lists:foreach(fun ({LJID, SubNodeName, SubIDs}) ->
                          LJIDs = case BroadcastAll of
                                      true ->
                                          {U, S, _} = LJID,
                                          [{U, S, R} || R <- user_resources(U, S)];
                                      false ->
                                          [LJID]
                                  end,
                          StanzaToSend = maybe_add_shim_headers(Stanza, SHIM, SubIDs,
                                                                Node, SubNodeName),

                          lists:foreach(fun(To) ->
                                                ejabberd_router:route(From, jid:make(To),
                                                                      StanzaToSend)
                                        end, LJIDs)
                  end, SubIDsByJID).

broadcast_stanza({LUser, LServer, LResource}, ReplyToJID, Node, Nidx, Type, NodeOptions,
                 SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza({LUser, LServer, LResource}, Node, Nidx, Type, NodeOptions,
                     SubsByDepth, NotifyType, BaseStanza, SHIM),
    %% Handles implicit presence subscriptions
    SenderResource = user_resource(LUser, LServer, LResource),
    case ejabberd_sm:get_session_pid(jid:make(LUser, LServer, SenderResource)) of
        C2SPid when is_pid(C2SPid) ->
            NotificationType = get_option(NodeOptions, notification_type, headline),
            Stanza = add_message_type(BaseStanza, NotificationType),
            %% set the from address on the notification to the bare JID of the account owner
            %% Also, add "replyto" if entity has presence subscription to the account owner
            %% See XEP-0163 1.1 section 4.3.1
            ReplyTo = extended_headers([jid:to_binary(ReplyToJID)]),
            Feature = <<((Node))/binary, "+notify">>,
            Recipients = mongoose_c2s:call(C2SPid, ?MODULE, {get_pep_recipients, Feature}),
            Packet = add_extended_headers(Stanza, ReplyTo),
            From = jid:make_bare(LUser, LServer),
            lists:foreach(fun(USR) -> ejabberd_router:route(From, jid:make(USR), Packet) end,
                          lists:usort(Recipients));
        _ ->
            ?LOG_DEBUG(#{what => pubsub_no_session,
                text => <<"User has no session; cannot deliver stanza to contacts">>,
                user => LUser, server => LServer, exml_packet => BaseStanza})
    end;
broadcast_stanza(Host, _ReplyToJID, Node, Nidx, Type, NodeOptions,
                 SubsByDepth, NotifyType, BaseStanza, SHIM) ->
    broadcast_stanza(Host, Node, Nidx, Type, NodeOptions, SubsByDepth,
                     NotifyType, BaseStanza, SHIM).

subscribed_nodes_by_jid(NotifyType, SubsByDepth) ->
    DepthsToDeliver =
    fun({Depth, SubsByNode}, Acc1) ->
            lists:foldl(fun({Node, Subs}, Acc2) ->
                                nodes_to_deliver(NotifyType, Depth, Node, Subs, Acc2)
                        end, Acc1, SubsByNode)
    end,
    {_, JIDSubs} = lists:foldl(DepthsToDeliver, {[], []}, SubsByDepth),
    JIDSubs.

nodes_to_deliver(NotifyType, Depth, Node, Subs, Acc) ->
    NodeName = case Node#pubsub_node.nodeid of
                   {_, N} -> N;
                   Other -> Other
               end,
    NodeOptions = Node#pubsub_node.options,
    lists:foldl(fun({LJID, SubID, SubOptions}, InnerAcc) ->
                        case is_to_deliver(LJID, NotifyType, Depth, NodeOptions, SubOptions) of
                            true ->
                                JIDsToDeliver = state_can_deliver(LJID, SubOptions),
                                process_jids_to_deliver(NodeName, SubID, JIDsToDeliver, InnerAcc);
                            false ->
                                InnerAcc
                        end
                end, Acc, Subs).

process_jids_to_deliver(NodeName, SubID, JIDsToDeliver, {JIDs, Recipients}) ->
    lists:foldl(
      fun(JIDToDeliver, {JIDsAcc, RecipientsAcc}) ->
              process_jid_to_deliver(JIDs, SubID, NodeName,
                                     JIDToDeliver, {JIDsAcc, RecipientsAcc})
      end, {JIDs, Recipients}, JIDsToDeliver).

process_jid_to_deliver(JIDs, SubID, NodeName, JIDToDeliver, {JIDsAcc, RecipientsAcc}) ->
    case lists:member(JIDToDeliver, JIDs) of
        %% check if the JIDs co-accumulator contains the Subscription Jid,
        false ->
            %%  - if not,
            %%  - add the Jid to JIDs list co-accumulator ;
            %%  - create a tuple of the Jid, Nidx, and SubID (as list),
            %%    and add the tuple to the Recipients list co-accumulator
            {[JIDToDeliver | JIDsAcc],
             [{JIDToDeliver, NodeName, [SubID]}
              | RecipientsAcc]};
        true ->
            %% - if the JIDs co-accumulator contains the Jid
            %%   get the tuple containing the Jid from the Recipient list co-accumulator
            {_, {JIDToDeliver, NodeName1, SubIDs}} =
                lists:keysearch(JIDToDeliver, 1, RecipientsAcc),
            %%   delete the tuple from the Recipients list
            % v1 : Recipients1 = lists:keydelete(LJID, 1, Recipients),
            % v2 : Recipients1 = lists:keyreplace(LJID, 1, Recipients,
            %                                     {LJID, Nidx1, [SubID | SubIDs]}),
            %%   add the SubID to the SubIDs list in the tuple,
            %%   and add the tuple back to the Recipients list co-accumulator
            % v1.1 : {JIDs, lists:append(Recipients1,
            %                            [{LJID, Nidx1, lists:append(SubIDs, [SubID])}])}
            % v1.2 : {JIDs, [{LJID, Nidx1, [SubID | SubIDs]} | Recipients1]}
            % v2: {JIDs, Recipients1}
            {JIDsAcc,
             lists:keyreplace(JIDToDeliver, 1,
                              RecipientsAcc,
                              {JIDToDeliver, NodeName1,
                               [SubID | SubIDs]})}
    end.

user_resources(User, Server) ->
    JID = jid:make_bare(User, Server),
    ejabberd_sm:get_user_resources(JID).

user_resource(User, Server, <<>>) ->
    case user_resources(User, Server) of
        [R | _] -> R;
        _ -> <<>>
    end;
user_resource(_, _, Resource) ->
    Resource.

%%%%%%% Configuration handling

get_configure(Host, Node, From, #{server_host := ServerHost, lang := Lang}) ->
    Action = fun(PubSubNode) ->
                     get_configure_transaction(ServerHost, Node, From, Lang, PubSubNode)
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_, Result}} -> {result, Result};
        Other -> Other
    end.

get_configure_transaction(ServerHost, Node, From, Lang,
                          #pubsub_node{options = Options, type = Type, id = Nidx}) ->
    case node_call(Type, get_affiliation, [Nidx, From]) of
        {result, owner} ->
            Groups = mongoose_hooks:roster_groups(ServerHost),
            XEl = configure_form(<<"form">>, Options, Lang, Groups),
            ConfigureEl = #xmlel{name = <<"configure">>,
                                 attrs = node_attr(Node),
                                 children = [XEl]},
            {result,
             [#xmlel{name = <<"pubsub">>,
                     attrs = #{<<"xmlns">> => ?NS_PUBSUB_OWNER},
                     children = [ConfigureEl]}]};
        _ ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

get_default(Host, Node, _From, #{lang := Lang}) ->
    Type = select_type(Host, Node),
    Options = node_options(Host, Type),
    XEl = configure_form(<<"form">>, Options, Lang, []),
    DefaultEl = #xmlel{name = <<"default">>, children = [XEl]},
    {result,
     [#xmlel{name = <<"pubsub">>,
             attrs = #{<<"xmlns">> => ?NS_PUBSUB_OWNER},
             children = [DefaultEl]}]}.

match_option(Node, Var, Val) when is_record(Node, pubsub_node) ->
    match_option(Node#pubsub_node.options, Var, Val);
match_option(Options, Var, Val) when is_list(Options) ->
    get_option(Options, Var) == Val;
match_option(_, _, _) ->
    false.

get_option([], _) -> false;
get_option(Options, Var) -> get_option(Options, Var, false).

get_option(Options, Var, Def) ->
    case lists:keysearch(Var, 1, Options) of
        {value, {_Val, Ret}} -> Ret;
        _ -> Def
    end.

-spec check_publish_options(binary(), undefined | exml:element(), mod_pubsub:nodeOptions()) ->
    boolean().
check_publish_options(Type, PublishOptions, Options) ->
    ParsedPublishOptions = parse_publish_options(PublishOptions),
    ConvertedOptions = convert_options(Options),
    case node_call(Type, check_publish_options, [ParsedPublishOptions, ConvertedOptions]) of
        {error, _} ->
            true;
        {result, Result} ->
            Result
    end.

-spec parse_publish_options(undefined | exml:element()) -> invalid_form | #{binary() => [binary()]}.
parse_publish_options(undefined) ->
    #{};
parse_publish_options(PublishOptions) ->
    case mongoose_data_forms:find_and_parse_form(PublishOptions) of
        #{type := <<"submit">>, kvs := KVs, ns := ?NS_PUBSUB_PUB_OPTIONS} ->
            KVs;
        _ ->
            invalid_form
    end.

-spec convert_options(mod_pubsub:nodeOptions()) -> #{binary() => [binary()]}.
convert_options(Options) ->
    ConvertedOptions = lists:map(fun({Key, Value}) ->
                                     {atom_to_binary(Key), convert_option_value(Value)}
                                 end, Options),
    maps:from_list(ConvertedOptions).

-spec convert_option_value(binary() | [binary()] | atom() | non_neg_integer()) -> [binary()].
convert_option_value(true) ->
    [<<"1">>];
convert_option_value(false) ->
    [<<"0">>];
convert_option_value(Element) when is_atom(Element) ->
    [atom_to_binary(Element)];
convert_option_value(Element) when is_integer(Element) ->
    [integer_to_binary(Element)];
convert_option_value(List) when is_list(List) ->
    List.

node_options(Host, Type) ->
    ConfiguredOpts = lists:keysort(1, config(serverhost(Host), default_node_config)),
    DefaultOpts = lists:keysort(1, node_plugin_options(Type)),
    lists:keymerge(1, ConfiguredOpts, DefaultOpts).

node_plugin_options(Type) ->
    Module = plugin(Type),
    case catch gen_pubsub_node:options(Module) of
        {'EXIT', {undef, _}} ->
            DefaultModule = plugin(?STDNODE),
            gen_pubsub_node:options(DefaultModule);
        Result ->
            Result
    end.

%% @doc <p>Return the maximum number of items for a given node.</p>
%% <p>Unlimited means that there is no limit in the number of items that can
%% be stored.</p>
%% @todo In practice, the current data structure means that we cannot manage
%% millions of items on a given node. This should be addressed in a new
%% version.
-spec max_items(Host, Options) -> MaxItems when
    Host :: host(),
    Options :: [Option],
    Option :: {Key :: atom(), Value :: term()},
    MaxItems :: integer() | unlimited.
max_items(Host, Options) ->
    case get_option(Options, persist_items) of
        true ->
            case get_option(Options, max_items) of
                I when is_integer(I), I < 0 -> 0;
                I when is_integer(I) -> I;
                _ -> ?MAXITEMS
            end;
        false ->
            %% Don't publish if it means sending the item without a way to retrieve it
            case get_option(Options, send_last_published_item) == never
                 orelse is_last_item_cache_enabled(Host) of
                true -> 0;
                false -> 1
            end
    end.

-define(BOOL_CONFIG_FIELD(Label, Var),
        ?BOOLXFIELD(Label,
                    <<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
                    (get_option(Options, Var)))).

-define(STRING_CONFIG_FIELD(Label, Var),
        ?STRINGXFIELD(Label,
                      <<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
                      (get_option(Options, Var, <<>>)))).

-define(INTEGER_CONFIG_FIELD(Label, Var),
        ?STRINGXFIELD(Label,
                      <<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
                      (integer_to_binary(get_option(Options, Var))))).

-define(JLIST_CONFIG_FIELD(Label, Var, Opts),
        ?LISTXFIELD(Label,
                    <<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
                    (jid:to_binary(get_option(Options, Var))),
                    [jid:to_binary(O) || O <- Opts])).

-define(ALIST_CONFIG_FIELD(Label, Var, Opts),
        ?LISTXFIELD(Label,
                    <<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
                    (atom_to_binary(get_option(Options, Var), latin1)),
                    [atom_to_binary(O, latin1) || O <- Opts])).

-define(LISTM_CONFIG_FIELD(Label, Var, Opts),
        ?LISTMXFIELD(Label,
                     <<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
                     (get_option(Options, Var)), Opts)).

-define(NLIST_CONFIG_FIELD(Label, Var),
        ?STRINGMXFIELD(Label,
                       <<"pubsub#", (atom_to_binary(Var, latin1))/binary>>,
                       get_option(Options, Var, []))).

configure_form(Type, Options, Lang, Groups) ->
    Fields = get_configure_xfields(Options, Lang, Groups),
    mongoose_data_forms:form(#{type => Type, ns => ?NS_PUBSUB_NODE_CONFIG, fields => Fields}).

get_configure_xfields(Options, Lang, Groups) ->
    [?BOOL_CONFIG_FIELD(<<"Deliver payloads with event notifications">>,
                        deliver_payloads),
     ?BOOL_CONFIG_FIELD(<<"Deliver event notifications">>,
                        deliver_notifications),
     ?BOOL_CONFIG_FIELD(<<"Notify subscribers when the node configuration changes">>,
                        notify_config),
     ?BOOL_CONFIG_FIELD(<<"Notify subscribers when the node is deleted">>,
                        notify_delete),
     ?BOOL_CONFIG_FIELD(<<"Notify subscribers when items are removed from the node">>,
                        notify_retract),
     ?BOOL_CONFIG_FIELD(<<"Persist items to storage">>,
                        persist_items),
     ?STRING_CONFIG_FIELD(<<"A friendly name for the node">>,
                          title),
     ?STRINGXFIELD(<<"Max # of items to persist">>, <<"pubsub#max_items">>,
                   (integer_to_binary(get_option(Options, max_items, <<>>)))),
     ?BOOL_CONFIG_FIELD(<<"Whether to allow subscriptions">>,
                        subscribe),
     ?ALIST_CONFIG_FIELD(<<"Specify the access model">>,
                         access_model, [open, authorize, presence, roster, whitelist]),
     ?LISTM_CONFIG_FIELD(<<"Roster groups allowed to subscribe">>,
                         roster_groups_allowed, Groups),
     ?ALIST_CONFIG_FIELD(<<"Specify the publisher model">>,
                         publish_model, [publishers, subscribers, open]),
     ?BOOL_CONFIG_FIELD(<<"Purge all items when the relevant publisher goes offline">>,
                        purge_offline),
     ?ALIST_CONFIG_FIELD(<<"Specify the event message type">>,
                         notification_type, [headline, normal]),
     ?INTEGER_CONFIG_FIELD(<<"Max payload size in bytes">>,
                           max_payload_size),
     ?ALIST_CONFIG_FIELD(<<"When to send the last published item">>,
                         send_last_published_item, [never, on_sub, on_sub_and_presence]),
     ?BOOL_CONFIG_FIELD(<<"Only deliver notifications to available users">>,
                        presence_based_delivery),
     ?STRING_CONFIG_FIELD(<<"Specify the type of payload data to be provided at this node">>,
                          type),
     ?NLIST_CONFIG_FIELD(<<"The collections with which a node is affiliated">>,
                         collection)].

%%<p>There are several reasons why the node configuration request might fail:</p>
%%<ul>
%%<li>The service does not support node configuration.</li>
%%<li>The requesting entity does not have sufficient privileges to configure the node.</li>
%%<li>The request did not specify a node.</li>
%%<li>The node has no configuration options.</li>
%%<li>The specified node does not exist.</li>
%%</ul>
set_configure(Host, Node, From, #{action_el := ActionEl, lang := Lang}) ->
    case mongoose_data_forms:find_and_parse_form(ActionEl) of
        #{type := <<"cancel">>} ->
            {result, []};
        #{type := <<"submit">>, kvs := KVs} ->
            set_configure_submit(Host, Node, From, KVs, Lang);
        {error, Msg} ->
            {error, mongoose_xmpp_errors:bad_request(Lang, Msg)};
        _ ->
            {error, mongoose_xmpp_errors:bad_request(Lang, <<"Invalid form type">>)}
    end.

set_configure_submit(Host, Node, User, KVs, Lang) ->
    Action = fun(NodeRec) ->
                     set_configure_transaction(Host, User, KVs, NodeRec)
             end,
    case transaction(Host, Node, Action, ?FUNCTION_NAME) of
        {result, {_OldNode, TNode}} ->
            Nidx = TNode#pubsub_node.id,
            Type = TNode#pubsub_node.type,
            Options = TNode#pubsub_node.options,
            broadcast_config_notification(Host, Node, Nidx, Type, Options, Lang),
            {result, []};
        Other ->
            Other
    end.

set_configure_transaction(Host, User, KVs, #pubsub_node{ type = Type, id = Nidx } = NodeRec) ->
    case node_call(Type, get_affiliation, [Nidx, User]) of
        {result, owner} ->
            set_configure_valid_transaction(Host, NodeRec, KVs);
        _ ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

set_configure_valid_transaction(Host, #pubsub_node{ type = Type, options = Options } = NodeRec,
                                KVs) ->
    OldOpts = case Options of
                  [] -> node_options(Host, Type);
                  _ -> Options
              end,
    case set_xoption(Host, maps:to_list(KVs), OldOpts) of
        NewOpts when is_list(NewOpts) ->
            NewNode = NodeRec#pubsub_node{options = NewOpts},
            case tree_call(Host, set_node, [NewNode]) of
                {ok, _} -> {result, NewNode};
                Err -> Err
            end;
        Error ->
            Error
    end.

add_opt(Key, Value, Opts) ->
    [{Key, Value} | lists:keydelete(Key, 1, Opts)].

-define(SET_BOOL_XOPT(Opt, Val),
        BoolVal = case Val of
                      <<"0">> -> false;
                      <<"1">> -> true;
                      <<"false">> -> false;
                      <<"true">> -> true;
                      _ -> error
                  end,
        case BoolVal of
            error -> {error, mongoose_xmpp_errors:not_acceptable()};
            _ -> set_xoption(Host, Opts, add_opt(Opt, BoolVal, NewOpts))
        end).

-define(SET_STRING_XOPT(Opt, Val),
        set_xoption(Host, Opts, add_opt(Opt, Val, NewOpts))).

-define(SET_INTEGER_OR_MAX_XOPT(Opt, Val, Min, Max),
        case Val of
            <<"max">> ->
                set_xoption(Host, Opts, add_opt(Opt, Max, NewOpts));
            _ ->
                ?SET_INTEGER_XOPT(Opt, Val, Min, Max)
        end).

-define(SET_INTEGER_XOPT(Opt, Val, Min, Max),
        case catch binary_to_integer(Val) of
            IVal when is_integer(IVal), IVal >= Min ->
                if (Max =:= undefined) orelse (IVal =< Max) ->
                        set_xoption(Host, Opts, add_opt(Opt, IVal, NewOpts));
                   true ->
                        {error, mongoose_xmpp_errors:not_acceptable()}
                end;
            _ ->
                {error, mongoose_xmpp_errors:not_acceptable()}
        end).

-define(SET_ALIST_XOPT(Opt, Val, Vals),
        case lists:member(Val, [atom_to_binary(V, latin1) || V <- Vals]) of
            true ->
                set_xoption(Host, Opts, add_opt(Opt, binary_to_atom(Val, utf8), NewOpts));
            false ->
                {error, mongoose_xmpp_errors:not_acceptable()}
        end).

-define(SET_LIST_XOPT(Opt, Val),
        set_xoption(Host, Opts, add_opt(Opt, Val, NewOpts))).

set_xoption(_Host, [], NewOpts) -> NewOpts;
set_xoption(Host, [{<<"pubsub#roster_groups_allowed">>, Value} | Opts], NewOpts) ->
    ?SET_LIST_XOPT(roster_groups_allowed, Value);
set_xoption(Host, [{<<"pubsub#deliver_payloads">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_payloads, Val);
set_xoption(Host, [{<<"pubsub#deliver_notifications">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(deliver_notifications, Val);
set_xoption(Host, [{<<"pubsub#notify_config">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_config, Val);
set_xoption(Host, [{<<"pubsub#notify_delete">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_delete, Val);
set_xoption(Host, [{<<"pubsub#notify_retract">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(notify_retract, Val);
set_xoption(Host, [{<<"pubsub#persist_items">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(persist_items, Val);
set_xoption(Host, [{<<"pubsub#max_items">>, [Val]} | Opts], NewOpts) ->
    MaxItems = get_max_items_node(Host),
    ?SET_INTEGER_OR_MAX_XOPT(max_items, Val, 0, MaxItems);
set_xoption(Host, [{<<"pubsub#subscribe">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(subscribe, Val);
set_xoption(Host, [{<<"pubsub#access_model">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(access_model, Val, [open, authorize, presence, roster, whitelist]);
set_xoption(Host, [{<<"pubsub#publish_model">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(publish_model, Val, [publishers, subscribers, open]);
set_xoption(Host, [{<<"pubsub#notification_type">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(notification_type, Val, [headline, normal]);
set_xoption(Host, [{<<"pubsub#node_type">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(node_type, Val, [leaf, collection]);
set_xoption(Host, [{<<"pubsub#max_payload_size">>, [Val]} | Opts], NewOpts) ->
    ?SET_INTEGER_XOPT(max_payload_size, Val, 0, (?MAX_PAYLOAD_SIZE));
set_xoption(Host, [{<<"pubsub#send_last_published_item">>, [Val]} | Opts], NewOpts) ->
    ?SET_ALIST_XOPT(send_last_published_item, Val, [never, on_sub, on_sub_and_presence]);
set_xoption(Host, [{<<"pubsub#presence_based_delivery">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(presence_based_delivery, Val);
set_xoption(Host, [{<<"pubsub#purge_offline">>, [Val]} | Opts], NewOpts) ->
    ?SET_BOOL_XOPT(purge_offline, Val);
set_xoption(Host, [{<<"pubsub#title">>, Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(title, Value);
set_xoption(Host, [{<<"pubsub#type">>, Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(type, Value);
set_xoption(Host, [{<<"pubsub#body_xslt">>, Value} | Opts], NewOpts) ->
    ?SET_STRING_XOPT(body_xslt, Value);
set_xoption(Host, [{<<"pubsub#collection">>, Value} | Opts], NewOpts) ->
    ?SET_LIST_XOPT(collection, Value);
set_xoption(Host, [{<<"pubsub#node">>, [Value]} | Opts], NewOpts) ->
    ?SET_LIST_XOPT(node, Value);
set_xoption(Host, [_ | Opts], NewOpts) ->
    set_xoption(Host, Opts, NewOpts).

get_max_items_node({_, ServerHost, _}) ->
    get_max_items_node(ServerHost);
get_max_items_node(Host) ->
    config(serverhost(Host), max_items_node).

get_max_subscriptions_node({_, ServerHost, _}) ->
    get_max_subscriptions_node(ServerHost);
get_max_subscriptions_node(Host) ->
    config(serverhost(Host), max_subscriptions_node).

%%%% last item cache handling
maybe_start_cache_module(ServerHost, #{last_item_cache := Cache} = Opts) ->
    case Cache of
        false -> ok;
        _Backend -> mod_pubsub_cache_backend:start(ServerHost, Opts)
    end.

is_last_item_cache_enabled(Host) ->
    cache_backend(Host) =/= false.

cache_backend(Host) ->
    gen_mod:get_module_opt(serverhost(Host), mod_pubsub, last_item_cache).

set_cached_item({_, ServerHost, _}, Nidx, ItemId, Publisher, Payload) ->
    set_cached_item(ServerHost, Nidx, ItemId, Publisher, Payload);
set_cached_item(Host, Nidx, ItemId, Publisher, Payload) ->
    is_last_item_cache_enabled(Host) andalso
        mod_pubsub_cache_backend:upsert_last_item(serverhost(Host), Nidx, ItemId, Publisher, Payload).

unset_cached_item({_, ServerHost, _}, Nidx) ->
    unset_cached_item(ServerHost, Nidx);
unset_cached_item(Host, Nidx) ->
    is_last_item_cache_enabled(Host) andalso
        mod_pubsub_cache_backend:delete_last_item(serverhost(Host), Nidx).

-spec get_cached_item(ServerHost :: mod_pubsub:host(),
                      Nidx :: mod_pubsub:nodeIdx()) -> false | mod_pubsub:pubsubItem().
get_cached_item({_, ServerHost, _}, Nidx) ->
    get_cached_item(ServerHost, Nidx);
get_cached_item(Host, Nidx) ->
    is_last_item_cache_enabled(Host) andalso
        case mod_pubsub_cache_backend:get_last_item(serverhost(Host), Nidx) of
            {ok, #pubsub_last_item{itemid = ItemId, creation = Creation, payload = Payload}} ->
                    #pubsub_item{itemid = {ItemId, Nidx},
                                 payload = Payload, creation = Creation,
                                 modification = Creation};
                _ ->
                   false
        end.

%%%% plugin handling

-spec host(HostType :: mongooseim:host_type(), ServerHost :: mongooseim:domain_name()) -> host().
host(HostType, ServerHost) ->
    SubdomainPattern = config(HostType, host),
    mongoose_subdomain_utils:get_fqdn(SubdomainPattern, ServerHost).

-spec serverhost(host()) -> host().
serverhost({_U, Server, _R})->
    Server;
serverhost(Host) ->
    case config(Host, host) of
        undefined ->
            [_, ServerHost] = binary:split(Host, <<".">>),
            ServerHost;
        _ ->
            Host
    end.

-spec host_to_host_type(mod_pubsub:host()) -> mongooseim:host_type().
host_to_host_type(Host) ->
    SH = serverhost(Host),
    {ok, HT} = mongoose_domain_api:get_host_type(SH),
    HT.

-spec tree(HostType :: mongooseim:host_type() | host()) -> module() | nodetree_virtual.
tree(HostType) ->
    try gen_mod:get_module_opt(HostType, ?MODULE, nodetree)
    catch error:{badkey, _} ->
        %todo remove when pubsub supports dynamic domains
        HT = host_to_host_type(HostType),
        gen_mod:get_module_opt(HT, ?MODULE, nodetree)
    end.

tree_mod(<<"virtual">>) ->
    nodetree_virtual;   % special case, virtual does not use any backend
tree_mod(Name) ->
    binary_to_atom(<<"nodetree_", Name/binary>>, utf8).

-spec plugin(Name :: plugin_name()) -> module().
plugin(Name) ->
    binary_to_atom(<<"node_", Name/binary>>, utf8).

-spec plugins(ServerHost :: mongooseim:domain_name()) -> [plugin_name()].
plugins(ServerHost) ->
    Proc = gen_mod:get_module_proc(ServerHost, ?PROCNAME),
    %% TODO This call could be replaced with persistent terms
    gen_server:call(Proc, plugins).

config(ServerHost, Key) ->
    config(ServerHost, Key, undefined).
config(ServerHost, Key, Default) ->
    gen_mod:get_module_opt(ServerHost, ?MODULE, Key, Default).

select_type(Host, Node) ->
    select_type(serverhost(Host), Host, Node).

select_type(ServerHost, Host, Node) ->
    select_type(ServerHost, Host, Node, hd(plugins(ServerHost))).

select_type(ServerHost, Host, Node, Type) ->
    SelectedType = case Host of
                       {_User, _Server, _Resource} ->
                           case config(ServerHost, pep_mapping) of
                               undefined -> ?PEPNODE;
                               Mapping -> maps:get(Node, Mapping, ?PEPNODE)
                           end;
                       _ ->
                           Type
                   end,
    ConfiguredTypes = plugins(ServerHost),
    case lists:member(SelectedType, ConfiguredTypes) of
        true -> SelectedType;
        false -> hd(ConfiguredTypes)
    end.

feature(Feature) -> <<(?NS_PUBSUB)/binary, "#", Feature/binary>>.

features() ->
    [% see plugin "access-authorize",   % OPTIONAL
     <<"access-open">>,   % OPTIONAL this relates to access_model option in node_hometree
     <<"access-presence">>,   % OPTIONAL this relates to access_model option in node_pep
     <<"access-whitelist">>,   % OPTIONAL
     <<"collections">>,   % RECOMMENDED
     <<"config-node">>,   % RECOMMENDED
     <<"create-and-configure">>,   % RECOMMENDED
     <<"item-ids">>,   % RECOMMENDED
     <<"last-published">>,   % RECOMMENDED
     <<"member-affiliation">>,   % RECOMMENDED
     <<"presence-notifications">>,   % OPTIONAL
     <<"presence-subscribe">>,   % RECOMMENDED
     <<"publisher-affiliation">>,   % RECOMMENDED
     <<"publish-only-affiliation">>,   % OPTIONAL
     <<"retrieve-default">>,
     <<"rsm">>,   % RECOMMENDED
     <<"shim">>].   % RECOMMENDED
% see plugin "retrieve-items",   % RECOMMENDED
% see plugin "retrieve-subscriptions",   % RECOMMENDED
% see plugin "subscribe",   % REQUIRED
% see plugin "subscription-options",   % OPTIONAL
% see plugin "subscription-notifications"   % OPTIONAL

plugin_features(Type) ->
    Module = plugin(Type),
    case catch gen_pubsub_node:features(Module) of
        {'EXIT', {undef, _}} -> [];
        Result -> Result
    end.

features(Host, <<>>) ->
    lists:usort(lists:foldl(fun (Plugin, Acc) ->
                                    Acc ++ plugin_features(Plugin)
                            end,
                            features(), plugins(Host)));
features(Host, Node) when is_binary(Node) ->
    Action = fun (#pubsub_node{type = Type}) ->
                     {result, plugin_features(Type)}
             end,
    case dirty(Host, Node, Action, ?FUNCTION_NAME) of
        {result, Features} -> lists:usort(features() ++ Features);
        _ -> features()
    end.

%% @doc <p>node tree plugin call.</p>
tree_call(HostType, Function, Args) ->
    ?LOG_DEBUG(#{what => pubsub_tree_call, action_function => Function,
                 args => Args, host_type => HostType}),
    apply(tree(HostType), Function, Args).

tree_action(HostType, Function, Args) ->
    ?LOG_DEBUG(#{what => pubsub_tree_action,
        action_function => Function, args => Args}),
    Fun = fun () -> tree_call(HostType, Function, Args) end,
    ErrorDebug = #{
      action => tree_action,
      host_type => HostType,
      function => Function,
      args => Args
     },
    catch mod_pubsub_db_backend:dirty(Fun, ErrorDebug).

%% @doc <p>node plugin call.</p>
node_call(Type, Function, Args) ->
    ?LOG_DEBUG(#{what => pubsub_node_call, node_type => Type,
        action_function => Function, args => Args}),
    PluginModule = plugin(Type),
    plugin_call(PluginModule, Function, Args).

-spec plugin_call(module(), atom(), [term()]) -> {result, any()} | {error, any()}.
plugin_call(PluginModule, Function, Args) ->
    CallModule = maybe_default_node(PluginModule, Function, Args),
    case apply(CallModule, Function, Args) of
        {result, Result} ->
            {result, Result};
        {error, Error} ->
            {error, Error};
        {'EXIT', Reason} ->
            {error, Reason};
        Result ->
            {result, Result} %% any other return value is forced as result
    end.

maybe_default_node(PluginModule, Function, Args) ->
    case erlang:function_exported(PluginModule, Function, length(Args)) of
        true ->
            PluginModule;
        _ ->
           case gen_pubsub_node:based_on(PluginModule) of
               none ->
                   ?LOG_ERROR(#{what => pubsub_undefined_function,
                       node_plugin => PluginModule, action_function => Function}),
                   exit(udefined_node_plugin_function);
               BaseModule ->
                   maybe_default_node(BaseModule, Function, Args)
           end
    end.

node_action(Host, Type, Function, Args) ->
    ?LOG_DEBUG(#{what => pubsub_node_action, sub_host => Host,
        node_type => Type, action_function => Function, args => Args}),
    ErrorDebug = #{
        action => {node_action, Function},
        pubsub_host => Host,
        node_type => Type,
        args => Args
     },
    mod_pubsub_db_backend:dirty(fun() ->
                                        node_call(Type, Function, Args)
                                end, ErrorDebug).

dirty(Host, Node, Action, ActionName) ->
    ErrorDebug = #{
      pubsub_host => Host,
      node_name => Node,
      action => ActionName },
    mod_pubsub_db_backend:dirty(db_call_fun(Host, Node, Action), ErrorDebug).

transaction(Host, Node, Action, ActionName) ->
    ErrorDebug = #{
      pubsub_host => Host,
      node_name => Node,
      action => ActionName
     },
    mod_pubsub_db_backend:transaction(db_call_fun(Host, Node, Action), ErrorDebug).

db_call_fun(Host, Node, Action) ->
    fun () ->
            case tree_call(Host, get_node, [Host, Node]) of
                #pubsub_node{} = N ->
                    case Action(N) of
                        {result, Result} -> {result, {N, Result}};
                        {atomic, {result, Result}} -> {result, {N, Result}};
                        Other -> Other
                    end;
                Error -> Error
            end
    end.

%%%% helpers

%% Add pubsub-specific error element
extended_error(#xmlel{} = Error, Ext) when is_binary(Ext) ->
    extend_error(Error, Ext, #{<<"xmlns">> => ?NS_PUBSUB_ERRORS}).

unsupported_error(Error, Feature) ->
    %% Give a uniq identifier
    extend_error(Error, <<"unsupported">>,
                 #{<<"xmlns">> => ?NS_PUBSUB_ERRORS, <<"feature">> => Feature}).

extend_error(#xmlel{children = SubEls} = El, Ext, ExtAttrs) ->
    El#xmlel{children = lists:reverse([#xmlel{name = Ext, attrs = ExtAttrs} | SubEls])}.

string_to_ljid(JID) ->
    case jid:from_binary(JID) of
        error ->
            {<<>>, <<>>, <<>>};
        J ->
            case jid:to_lower(J) of
                error -> {<<>>, <<>>, <<>>};
                J1 -> J1
            end
    end.

-spec uniqid() -> mod_pubsub:itemId().
uniqid() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

node_attr(Node) -> #{<<"node">> => Node}.

item_attr([])     -> #{};
item_attr(ItemId) -> #{<<"id">> => ItemId}.

item_attr(ItemId, undefined) -> item_attr(ItemId);
item_attr([], Publisher)     -> #{<<"publisher">> => jid:to_binary(jid:to_lower(Publisher))};
item_attr(ItemId, Publisher) -> #{<<"id">> => ItemId,
                                  <<"publisher">> => jid:to_binary(jid:to_lower(Publisher))}.

items_els(Items) ->
    [#xmlel{name = <<"item">>, attrs = item_attr(ItemId, Publisher), children = Payload}
     || #pubsub_item{itemid = {ItemId, _}, publisher = Publisher, payload = Payload } <- Items].

-spec add_message_type(Message :: exml:element(), Type :: atom()) -> exml:element().
add_message_type(Message, normal) -> Message;
add_message_type(#xmlel{name = <<"message">>, attrs = Attrs, children = Els}, Type) ->
    #xmlel{name = <<"message">>,
           attrs = Attrs#{<<"type">> => atom_to_binary(Type, utf8)},
           children = Els};
add_message_type(XmlEl, _Type) ->
    XmlEl.

maybe_add_shim_headers(Stanza, false, _SubIDs, _OriginNode, _SubNode) ->
    Stanza;
maybe_add_shim_headers(Stanza, true, SubIDs, OriginNode, SubNode) ->
    Headers1 = case SubIDs of
                   [_OnlyOneSubID] ->
                       [];
                   _ ->
                       subid_shim(SubIDs)
               end,
    Headers2 = case SubNode of
                   OriginNode ->
                       Headers1;
                   _ ->
                       [collection_shim(SubNode) | Headers1]
               end,
    add_headers(Stanza, <<"headers">>, ?NS_SHIM, Headers2).

add_extended_headers(Stanza, HeaderEls) ->
    add_headers(Stanza, <<"addresses">>, ?NS_ADDRESS, HeaderEls).

add_headers(#xmlel{name = Name, attrs = Attrs, children = Els}, HeaderName, HeaderNS, HeaderEls) ->
    HeaderEl = #xmlel{name = HeaderName,
                      attrs = #{<<"xmlns">> => HeaderNS},
                      children = HeaderEls},
    #xmlel{name = Name, attrs = Attrs,
           children = lists:append(Els, [HeaderEl])}.

subid_shim(SubIds) ->
    [#xmlel{ name = <<"header">>,
             attrs = #{<<"name">> => <<"SubId">>},
             children = [#xmlcdata{ content = SubId }]}
     || SubId <- SubIds].

collection_shim(CollectionNode) ->
    #xmlel{ name = <<"header">>,
            attrs = #{<<"name">> => <<"Collection">>},
            children = [#xmlcdata{ content = CollectionNode }] }.

%% The argument is a list of Jids because this function could be used
%% with the 'pubsub#replyto' (type=jid-multi) node configuration.
extended_headers(Jids) ->
    [#xmlel{name = <<"address">>,
            attrs = #{<<"type">> => <<"replyto">>, <<"jid">> => Jid}}
     || Jid <- Jids].

-spec on_user_offline(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
on_user_offline(Acc,
                #{jid := #jid{luser = User, lserver = Server, lresource = Resource}},
                #{host_type := HostType}) ->
    case user_resources(User, Server) of
        [] -> purge_offline(HostType, {User, Server, Resource});
        _ -> true
    end,
    {ok, Acc}.

purge_offline(HT, {_, LServer, _} = LJID) ->
    Host = host(HT, LServer),
    Plugins = plugins(LServer),
    Affs = lists:foldl(
             fun (PluginType, Acc) ->
                     check_plugin_features_and_acc_affs(Host, PluginType, LJID, Acc)
             end, [], Plugins),
    lists:foreach(
      fun ({Node, Affiliation}) ->
              Options = Node#pubsub_node.options,
              IsPublisherOrOwner = lists:member(Affiliation, [owner, publisher, publish_only]),
              OpenNode = get_option(Options, publish_model) == open,
              ShouldPurge = get_option(Options, purge_offline)
              andalso get_option(Options, persist_items),
              case (IsPublisherOrOwner or OpenNode) and ShouldPurge of
                  true -> purge_offline(Host, LJID, Node);
                  false -> ok
              end
      end, lists:usort(lists:flatten(Affs))).

check_plugin_features_and_acc_affs(Host, PluginType, LJID, AffsAcc) ->
    Features = plugin_features(PluginType),
    case lists:member(<<"retract-items">>, Features)
         andalso lists:member(<<"persistent-items">>, Features)
         andalso lists:member(<<"retrieve-affiliations">>, Features) of
        true ->
            {result, Affs} = node_action(Host, PluginType, get_entity_affiliations, [Host, LJID]),
            [Affs | AffsAcc];
        false ->
            ?LOG_DEBUG(#{what => pubsub_plugin_features_check_error,
                text => <<"Cannot purge items on offline">>,
                plugin => PluginType, user => jid:to_binary(LJID)}),
            AffsAcc
    end.

purge_offline(Host, {User, Server, _} = LJID, #pubsub_node{ id = Nidx, type = Type } = Node) ->
    case node_action(Host, Type, get_items, [Nidx, service_jid(Host), #{}]) of
        {result, {[], _}} ->
            ok;
        {result, {Items, _}} ->
            lists:foreach(fun(#pubsub_item{itemid = {ItemId, _}, modification = {_, {U, S, _}}})
                                when (U == User) and (S == Server) ->
                                  purge_item_of_offline_user(Host, LJID, Node, ItemId, U, S);
                             (_) ->
                                  true
                          end, Items);
        Error ->
            Error
    end.

purge_item_of_offline_user(Host, LJID,
                           #pubsub_node{ id = Nidx, nodeid = {_, NodeId},
                                         options = Options, type = Type }, ItemId, U, S) ->
    PublishModel = get_option(Options, publish_model),
    ForceNotify = get_option(Options, notify_retract),
    case node_action(Host, Type, delete_item, [Nidx, {U, S, <<>>}, PublishModel, ItemId]) of
        {result, {_, broadcast}} ->
            broadcast_retract_items(Host, LJID, NodeId, Nidx, Type, Options, [ItemId], ForceNotify),
            case get_cached_item(Host, Nidx) of
                #pubsub_item{itemid = {ItemId, Nidx}} -> unset_cached_item(Host, Nidx);
                _ -> ok
            end;
        {result, _} ->
            ok;
        Error ->
            Error
    end.

timestamp() ->
    os:system_time(microsecond).

make_error_reply(#iq{ sub_el = SubEl } = IQ, #xmlel{} = ErrorEl) ->
    IQ#iq{type = error, sub_el = [ErrorEl, SubEl]};
make_error_reply(#iq{ sub_el = SubEl } = IQ, Error) ->
    ?LOG_ERROR(#{what => pubsub_crash, reason => Error}),
    IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:internal_server_error(), SubEl]};
make_error_reply(Packet, #xmlel{} = ErrorEl) ->
    jlib:make_error_reply(Packet, ErrorEl);
make_error_reply(Packet, Error) ->
    ?LOG_ERROR(#{what => pubsub_crash, reason => Error}),
    jlib:make_error_reply(Packet, mongoose_xmpp_errors:internal_server_error()).

config_metrics(Host) ->
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, [backend]).
