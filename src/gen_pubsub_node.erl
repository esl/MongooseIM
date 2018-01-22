%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @private
%%% @doc <p>The module <strong>{@module}</strong> defines the PubSub node
%%% plugin behaviour. This behaviour is used to check that a PubSub plugin
%%% respects the current ejabberd PubSub plugin API.</p>

-module(gen_pubsub_node).

-include("jlib.hrl").

-type(host() :: mod_pubsub:host()).
-type(nodeId() :: mod_pubsub:nodeId()).
-type(nodeIdx() :: mod_pubsub:nodeIdx()).
-type(itemId() :: mod_pubsub:itemId()).
-type(pubsubNode() :: mod_pubsub:pubsubNode()).
-type(pubsubState() :: mod_pubsub:pubsubState()).
-type(pubsubItem() :: mod_pubsub:pubsubItem()).
-type(subOptions() :: mod_pubsub:subOptions()).
-type(affiliation() :: mod_pubsub:affiliation()).
-type(subscription() :: mod_pubsub:subscription()).
-type(subId() :: mod_pubsub:subId()).
-type(accessModel() :: mod_pubsub:accessModel()).
-type(publishModel() :: mod_pubsub:publishModel()).
-type(payload() :: mod_pubsub:payload()).
-type(publishOptions() :: mod_pubsub:publishOptions()).

-export([
         init/4,
         terminate/3,
         options/1,
         features/1,
         create_node_permission/7,
         create_node/3,
         delete_node/2,
         purge_node/3,
         subscribe_node/9,
         unsubscribe_node/5,
         publish_item/10,
         delete_item/5,
         remove_extra_items/4,
         get_node_affiliations/2,
         get_entity_affiliations/3,
         get_affiliation/3,
         set_affiliation/4,
         get_node_subscriptions/2,
         get_entity_subscriptions/3,
         get_subscriptions/3,
         get_pending_nodes/3,
         get_states/2,
         get_state/3,
         set_state/2,
         get_items/8,
         get_items/4,
         get_item/8,
         get_item/3,
         set_item/2,
         get_item_name/4,
         node_to_path/2,
         path_to_node/2
        ]).

%% --------------------------------------------------------
%% Callbacks
%% --------------------------------------------------------

-callback init(Host :: binary(), ServerHost :: binary(), Opts :: [any()]) -> atom().

-callback terminate(Host :: host(), ServerHost :: binary()) -> atom().

-callback options() -> [{atom(), any()}].

-callback features() -> [binary()].

-callback create_node_permission(Host :: host(),
                                 ServerHost :: binary(),
                                 Node :: nodeId(),
                                 ParentNode :: nodeId(),
                                 Owner :: jid:jid(),
                                 Access :: atom()) ->
    {result, boolean()}.

-callback create_node(NodeIdx :: nodeIdx(), Owner :: jid:jid()) -> {result, {default, broadcast}}.

-callback delete_node(Nodes :: [pubsubNode(), ...]) ->
    {result,
        {default, broadcast,
            [{pubsubNode(),
                    [{jid:ljid(), [{subscription(), subId()}]}, ...]}, ...]
            }
        }
    |
    {result,
        {[],
            [{pubsubNode(),
                    [{jid:ljid(), [{subscription(), subId()}]}, ...]}, ...]
            }
        }.

-callback purge_node(NodeIdx :: nodeIdx(), Owner :: jid:jid()) ->
    {result, {default, broadcast}} | {error, exml:element()}.

-callback subscribe_node(NodeIdx :: nodeIdx(),
        Sender :: jid:jid(),
        Subscriber :: jid:jid(),
        AccessModel :: accessModel(),
        SendLast :: 'never' | 'on_sub' | 'on_sub_and_presence',
        PresenceSubscription :: boolean(),
        RosterGroup :: boolean(),
        Options :: subOptions()) ->
    {result, {default, subscribed, subId()}} |
    {result, {default, subscribed, subId(), send_last}} |
    {result, {default, pending, subId()}} |
    {error, exml:element()}.

-callback unsubscribe_node(NodeIdx :: nodeIdx(),
        Sender :: jid:jid(),
        Subscriber :: jid:jid(),
        SubId :: subId()) ->
    {result, default} | {error, exml:element()}.

-callback publish_item(ServerHost :: jid:server(),
        NodeId :: nodeIdx(),
        Publisher :: jid:jid(),
        PublishModel :: publishModel(),
        MaxItems :: non_neg_integer(),
        ItemId :: <<>> | itemId(),
        ItemPublisher :: boolean(),
        Payload :: payload(),
        PublishOptions :: publishOptions()) ->
    {result, {default, broadcast, [itemId()]}} | {error, exml:element()}.

-callback delete_item(NodeIdx :: nodeIdx(),
        Publisher :: jid:jid(),
        PublishModel :: publishModel(),
        ItemId :: <<>> | itemId()) ->
    {result, {default, broadcast}} | {error, exml:element()}.

-callback remove_extra_items(NodeIdx :: nodeIdx(),
        MaxItems :: unlimited | non_neg_integer(),
        ItemIds :: [itemId()]) ->
    {result, {[itemId()], [itemId()]}}.

-callback get_node_affiliations(NodeIdx :: nodeIdx()) -> {result, [{jid:ljid(), affiliation()}]}.

-callback get_entity_affiliations(Host :: host(), Owner :: jid:jid()) ->
    {result, [{pubsubNode(), affiliation()}]}.

-callback get_affiliation(NodeIdx :: nodeIdx(), Owner :: jid:jid()) -> {result, affiliation()}.

-callback set_affiliation(NodeIdx :: nodeIdx(), Owner :: jid:jid(), Affiliation :: affiliation()) ->
    ok | {error, exml:element()}.

-callback get_node_subscriptions(NodeIdx :: nodeIdx()) ->
    {result,
        [{jid:ljid(), subscription(), subId()}] |
        [{jid:ljid(), none}, ...]
        }.

-callback get_entity_subscriptions(Host :: host(), Key :: jid:jid()) ->
    {result, [{pubsubNode(), subscription(), subId(), jid:ljid()}]}.

-callback get_subscriptions(NodeIdx :: nodeIdx(), Owner :: jid:jid()) ->
    {result, [{subscription(), subId()}]}.

-callback get_pending_nodes(Host :: host(), Owner :: jid:jid()) -> {result, [nodeId()]}.

-callback get_states(NodeIdx::nodeIdx()) -> {result, [pubsubState()]}.

-callback get_state(NodeIdx :: nodeIdx(), Key :: jid:ljid()) -> pubsubState().

-callback set_state(State::pubsubState()) -> ok | {error, exml:element()}.

-callback get_items(NodeIdx :: nodeIdx(),
        JID :: jid:jid(),
        AccessModel :: accessModel(),
        PresenceSubscription :: boolean(),
        RosterGroup :: boolean(),
        SubId :: subId(),
        RSM :: none | jlib:rsm_in()) ->
    {result, {[pubsubItem()], none | jlib:rsm_out()}} | {error, exml:element()}.

-callback get_items(NodeIdx :: nodeIdx(), From :: jid:jid(), RSM :: none | jlib:rsm_in()) ->
    {result, {[pubsubItem()], none | jlib:rsm_out()}}.

-callback get_item(NodeIdx :: nodeIdx(),
        ItemId :: itemId(),
        JID :: jid:jid(),
        AccessModel :: accessModel(),
        PresenceSubscription :: boolean(),
        RosterGroup :: boolean(),
        SubId :: subId()) ->
    {result, pubsubItem()} | {error, exml:element()}.

-callback get_item(NodeIdx :: nodeIdx(), ItemId :: itemId()) ->
    {result, pubsubItem()} | {error, exml:element()}.

-callback set_item(Item :: pubsubItem()) -> ok.

-callback get_item_name(Host :: host(), ServerHost :: binary(), Node :: nodeId()) -> itemId().

-callback node_to_path(Node :: nodeId()) -> [nodeId()].

-callback path_to_node(Path :: [nodeId()]) -> nodeId().

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------

init(Mod, Host, ServerHost, Opts) ->
    Mod:init(Host, ServerHost, Opts).

terminate(Mod, Host, ServerHost) ->
    Mod:terminate(Host, ServerHost).

options(Mod) ->
    Mod:options().

features(Mod) ->
    Mod:features().

create_node_permission(Mod, Host, ServerHost, Node, ParentNode, Owner, Access) ->
    Mod:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Mod, NodeIdx, Owner) ->
    Mod:create_node(NodeIdx, Owner).

delete_node(Mod, Nodes) ->
    Mod:delete_node(Nodes).

purge_node(Mod, NodeIdx, Owner) ->
    Mod:purge_node(NodeIdx, Owner).

subscribe_node(Mod, NodeIdx, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription,
               RosterGroup, Options) ->
    Mod:subscribe_node(NodeIdx, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription,
                       RosterGroup, Options).

unsubscribe_node(Mod, NodeIdx, Sender, Subscriber, SubId) ->
    Mod:unsubscribe_node(NodeIdx, Sender, Subscriber, SubId).

publish_item(Mod, ServerHost, NodeId, Publisher, PublishModel, MaxItems, ItemId,
             ItemPublisher, Payload, PublishOptions) ->
    Mod:publish_item(ServerHost, NodeId, Publisher, PublishModel, MaxItems, ItemId,
                     ItemPublisher, Payload, PublishOptions).

delete_item(Mod, NodeIdx, Publisher, PublishModel, ItemId) ->
    Mod:delete_item(NodeIdx, Publisher, PublishModel, ItemId).

remove_extra_items(Mod, NodeIdx, MaxItems, ItemIds) ->
    Mod:remove_extra_items(NodeIdx, MaxItems, ItemIds).

get_node_affiliations(Mod, NodeIdx) ->
    Mod:get_node_affiliations(NodeIdx).

get_entity_affiliations(Mod, Host, Owner) ->
    Mod:get_entity_affiliations(Host, Owner).

get_affiliation(Mod, NodeIdx, Owner) ->
    Mod:get_affiliation(NodeIdx, Owner).

set_affiliation(Mod, NodeIdx, Owner, Affiliation) ->
    Mod:set_affiliation(NodeIdx, Owner, Affiliation).

get_node_subscriptions(Mod, NodeIdx) ->
    Mod:get_node_subscriptions(NodeIdx).

get_entity_subscriptions(Mod, Host, Key) ->
    Mod:get_entity_subscriptions(Host, Key).

get_subscriptions(Mod, NodeIdx, Owner) ->
    Mod:get_subscriptions(NodeIdx, Owner).

get_pending_nodes(Mod, Host, Owner) ->
    Mod:get_pending_nodes(Host, Owner).

get_states(Mod, NodeIdx) ->
    Mod:get_states(NodeIdx).

get_state(Mod, NodeIdx, Key) ->
    Mod:get_state(NodeIdx, Key).

set_state(Mod, State) ->
    Mod:set_state(State).

get_items(Mod, NodeIdx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    Mod:get_items(NodeIdx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM).

get_items(Mod, NodeIdx, From, RSM) ->
    Mod:get_items(NodeIdx, From, RSM).

get_item(Mod, NodeIdx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    Mod:get_item(NodeIdx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

get_item(Mod, NodeIdx, ItemId) ->
    Mod:get_item(NodeIdx, ItemId).

set_item(Mod, Item) ->
    Mod:set_item(Item).

get_item_name(Mod, Host, ServerHost, Node) ->
    Mod:get_item_name(Host, ServerHost, Node).

node_to_path(Mod, Node) ->
    Mod:node_to_path(Node).

path_to_node(Mod, Path) ->
    Mod:path_to_node(Path).

