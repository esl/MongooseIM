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
-type(pubsubItem() :: mod_pubsub:pubsubItem()).
-type(subOptions() :: mod_pubsub:subOptions()).
-type(affiliation() :: mod_pubsub:affiliation()).
-type(subscription() :: mod_pubsub:subscription()).
-type(subId() :: mod_pubsub:subId()).
-type(accessModel() :: mod_pubsub:accessModel()).
-type(publishModel() :: mod_pubsub:publishModel()).
-type(payload() :: mod_pubsub:payload()).
-type(publishOptions() :: mod_pubsub:publishOptions()).
-type(get_authorised_item_options() :: #{access_model := accessModel(),
                                         presence_permission := boolean(),
                                         roster_permission := boolean(),
                                         rsm := none | jlib:rsm_in(),
                                         max_items => non_neg_integer(),
                                         item_ids => [itemId()],
                                         subscription_id => subId()}).
-type(get_item_options() :: #{rsm => none | jlib:rms_in(),
                              max_items => undefined | non_neg_integer(),
                              item_ids => undefined | [itemId()]}).

-export([based_on/1,
         terminate/3,
         options/1,
         features/1,
         node_to_path/2
        ]).

-export_type([get_item_options/0]).

%% --------------------------------------------------------
%% Callbacks
%% --------------------------------------------------------

%% @doc
%% This function is to call the base node module in case the target node doesn't
%% implement an optional callback
-callback based_on() -> node_flat | node_hometree | node_dag | node_push | node_pep | none.

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
        Subscriber :: jid:ljid(),
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
    {result, [{jid:ljid(), subscription(), subId(), subOptions()}]}.

-callback get_entity_subscriptions(Host :: host(), Key :: jid:jid()) ->
    {result, [{pubsubNode(), subscription(), subId(), jid:ljid()}]}.

-callback get_subscriptions(NodeIdx :: nodeIdx(), Owner :: jid:jid()) ->
    {result, [{subscription(), subId(), subOptions()}]}.

-callback get_pending_nodes(Host :: host(), Owner :: jid:jid()) -> {result, [nodeId()]}.

-callback get_items_if_authorised(NodeIdx :: nodeIdx(), JID :: jid:jid(), get_authorised_item_options()) ->
    {result, {[pubsubItem()], none | jlib:rsm_out()}} | {error, exml:element()}.

-callback get_items(NodeIdx :: nodeIdx(), From :: jid:jid(), get_item_options()) ->
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

-callback should_delete_when_owner_removed() -> boolean().

-callback remove_user(LUser :: jid:luser(), LServer :: jid:lserver()) -> any().

-optional_callbacks([create_node_permission/6,
                     create_node/2,
                     delete_node/1,
                     purge_node/2,
                     subscribe_node/8,
                     unsubscribe_node/4,
                     publish_item/9,
                     delete_item/4,
                     remove_extra_items/3,
                     get_node_affiliations/1,
                     get_entity_affiliations/2,
                     get_affiliation/2,
                     set_affiliation/3,
                     get_node_subscriptions/1,
                     get_entity_subscriptions/2,
                     get_subscriptions/2,
                     get_pending_nodes/2,
                     get_items_if_authorised/3,
                     get_items/3,
                     get_item/7,
                     get_item/2,
                     set_item/1,
                     get_item_name/3,
                     path_to_node/1,
                     should_delete_when_owner_removed/0,
                     remove_user/2]).

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------
based_on(Mod) ->
    Mod:based_on().

terminate(Mod, Host, ServerHost) ->
    Mod:terminate(Host, ServerHost).

options(Mod) ->
    Mod:options().

features(Mod) ->
    Mod:features().

node_to_path(Mod, Node) ->
    Mod:node_to_path(Node).

