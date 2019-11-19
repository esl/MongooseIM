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
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the default PubSub node tree plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node tree
%%% types.</p>
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_tree).
-behaviour(gen_pubsub_nodetree).
-author('christophe.romain@process-one.net').

-include_lib("stdlib/include/qlc.hrl").

-include("pubsub.hrl").
-include("jlib.hrl").

-export([init/3, terminate/2, set_node/1,
         get_node/2, get_node/1, get_nodes/2,
         get_parentnodes_tree/3,
         get_subnodes/3, create_node/6,
         delete_node/2]).

init(_Host, _ServerHost, _Options) ->
    ok.

terminate(_Host, _ServerHost) ->
    ok.

set_node(Node) ->
    mod_pubsub_db_backend:set_node(Node).

get_node(Host, Node) ->
    case catch mod_pubsub_db_backend:find_node_by_name(Host, Node) of
        #pubsub_node{} = Record -> Record;
        _ -> {error, mongoose_xmpp_errors:item_not_found()}
    end.

get_node(Nidx) ->
    case catch mod_pubsub_db_backend:find_node_by_id(Nidx) of
        {ok, Node} -> Node;
        _ -> {error, mongoose_xmpp_errors:item_not_found()}
    end.

get_nodes(Key, _From) ->
    mod_pubsub_db_backend:find_nodes_by_key(Key).

%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes_tree(Host, Node, _From) ->
    case catch mod_pubsub_db_backend:find_node_by_name(Host, Node) of
        #pubsub_node{} = Record -> [{0, [Record]}];
        _ -> []
    end.

get_subnodes(Host, Node, _From) ->
    mod_pubsub_db_backend:get_subnodes(Host, Node).

create_node(Host, NodeName, Type, Owner, Options, Parents) ->
    BJID = jid:to_lower(jid:to_bare(Owner)),
    case catch mod_pubsub_db_backend:find_node_by_name(Host, NodeName) of
        false ->
            case check_parent_and_its_owner_list(Host, Parents, BJID) of
                true ->
                    Node = #pubsub_node{nodeid = {Host, NodeName},
                                        parents = Parents,
                                        type = Type, owners = [BJID],
                                        options = Options},
                    set_node(Node);
                false ->
                    {error, mongoose_xmpp_errors:forbidden()}
            end;
        _ ->
            {error, mongoose_xmpp_errors:conflict()}
    end.

check_parent_and_its_owner_list({_U, _S, _R}, _Parents, _BJID) ->
    %% This is special case for PEP handling
    %% PEP does not uses hierarchy
    true;
check_parent_and_its_owner_list(_Host, [], _BJID) ->
    true;
check_parent_and_its_owner_list(Host, [Parent | _], BJID) ->
    case catch mod_pubsub_db_backend:find_node_by_name(Host, Parent) of
        #pubsub_node{owners = [{<<>>, Host, <<>>}]} ->
            true;
        #pubsub_node{owners = Owners} ->
            lists:member(BJID, Owners);
        _ ->
            false
    end;
check_parent_and_its_owner_list(_Host, _Parents, _BJID) ->
    false.

delete_node(Host, Node) ->
    SubNodesTree = mod_pubsub_db_backend:get_subnodes_tree(Host, Node),
    Removed = lists:flatten([Nodes || {_, Nodes} <- SubNodesTree]),
    lists:foreach(fun (NodeToDel) ->
                mod_pubsub_db_backend:delete_node(NodeToDel)
        end,
        Removed),
    Removed.

