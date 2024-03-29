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
%%% tree plugin behaviour. This behaviour is used to check that a PubSub
%%% node tree plugin respects the current ejabberd PubSub plugin API.</p>

-module(gen_pubsub_nodetree).

-include("jlib.hrl").

-export([init/3, terminate/3, set_node/2, get_node/3, get_node/2,
         get_nodes/3, get_parentnodes_tree/4,
         get_subnodes/4, create_node/7, delete_node/3]).

-ignore_xref([behaviour_info/1, create_node/7, delete_node/3, get_node/3,
              get_nodes/3, get_parentnodes_tree/4, get_subnodes/4, set_node/2]).

-type(host() :: mod_pubsub:host()).
-type(nodeId() :: mod_pubsub:nodeId()).
-type(nodeIdx() :: mod_pubsub:nodeIdx()).
-type(pubsubNode() :: mod_pubsub:pubsubNode()).
-type(nodeOptions() :: mod_pubsub:nodeOptions()).

%% ---------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------

-callback init(HostType :: mongooseim:host_type(), Opts :: [any()]) -> atom().

-callback terminate(Host :: host(), ServerHost :: binary()) -> atom().

-callback set_node(PubsubNode :: pubsubNode()) ->
    {ok, NodeIdx::nodeIdx()} | {error, exml:element()}.

-callback get_node(Host :: host(), NodeId :: nodeId()) -> pubsubNode() | {error, exml:element()}.

-callback get_node(NodeIdx :: nodeIdx()) -> pubsubNode() | {error, exml:element()}.

-callback get_nodes(Host :: host(), From :: jid:jid()) -> [pubsubNode()].

-callback get_parentnodes_tree(Host :: host(), NodeId :: nodeId(), From :: jid:jid()) ->
    [{0, [pubsubNode(), ...]}].

-callback get_subnodes(Host :: host(), NodeId :: nodeId(), From :: jid:jid()) -> [pubsubNode()].

-callback create_node(Host :: host(),
                      NodeId :: nodeId(),
                      Type :: binary(),
                      Owner :: jid:jid(),
                      Options :: nodeOptions(),
                      Parents :: [nodeId()]) ->
    {ok, NodeIdx::nodeIdx()} | {error, exml:element()} | {error, {virtual, {host(), nodeId()}}}.

-callback delete_node(Host :: host(), NodeId :: nodeId()) -> [pubsubNode()].

%% ---------------------------------------------------------------
%% API
%% ---------------------------------------------------------------

init(Mod, HostType, Opts) ->
    Mod:init(HostType, Opts).

terminate(Mod, Host, ServerHost) ->
    Mod:terminate(Host, ServerHost).

set_node(Mod, PubsubNode) ->
    Mod:set_node(PubsubNode).

get_node(Mod, Host, NodeId) ->
   Mod:get_node(Host, NodeId).

get_node(Mod, NodeIdx) ->
    Mod:get_node(NodeIdx).

get_nodes(Mod, Host, From) ->
    Mod:get_nodes(Host, From).

get_parentnodes_tree(Mod, Host, NodeId, From) ->
    Mod:get_parentnodes_tree(Host, NodeId, From).

get_subnodes(Mod, Host, NodeId, From) ->
   Mod:get_subnodes(Host, NodeId, From).

create_node(Mod, Host, NodeId, Type, Owner, Options, Parents) ->
    Mod:create_node(Host, NodeId, Type, Owner, Options, Parents).

delete_node(Mod, Host, NodeId) ->
    Mod:delete_node(Host, NodeId).

