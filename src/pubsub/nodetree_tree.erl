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
%%% @version {@vsn}, {@date} {@time}
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

-export([init/3, terminate/2, options/0, set_node/1,
         get_node/3, get_node/2, get_node/1, get_nodes/2,
         get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
         get_subnodes/3, get_subnodes_tree/3, create_node/6,
         delete_node/2]).

init(_Host, _ServerHost, _Options) ->
    mnesia:create_table(pubsub_node,
        [{disc_copies, [node()]},
            {attributes, record_info(fields, pubsub_node)}]),
    mnesia:add_table_index(pubsub_node, id),
    NodesFields = record_info(fields, pubsub_node),
    case mnesia:table_info(pubsub_node, attributes) of
        NodesFields -> ok;
        _ -> ok
    end,
    %% mnesia:transform_table(pubsub_state, ignore, StatesFields)
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{virtual_tree, false}].

set_node(Node) when is_record(Node, pubsub_node) ->
    mnesia:write(Node).

get_node(Host, Node, _From) ->
    get_node(Host, Node).

get_node(Host, Node) ->
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
        [Record] when is_record(Record, pubsub_node) -> Record;
        _ -> {error, mongoose_xmpp_errors:item_not_found()}
    end.

get_node(Nidx) ->
    case catch mnesia:index_read(pubsub_node, Nidx, #pubsub_node.id) of
        [Record] when is_record(Record, pubsub_node) -> Record;
        _ -> {error, mongoose_xmpp_errors:item_not_found()}
    end.

get_nodes(Host, _From) ->
    get_nodes(Host).

get_nodes(Host) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, _ = '_'}).

get_parentnodes(_Host, _Node, _From) ->
    [].

%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes_tree(Host, Node, _From) ->
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
        [Record] when is_record(Record, pubsub_node) -> [{0, [Record]}];
        _ -> []
    end.

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).

get_subnodes(Host, <<>>) ->
    Q = qlc:q([N
                || #pubsub_node{nodeid = {NHost, _},
                        parents = Parents} =
                    N
                    <- mnesia:table(pubsub_node),
                    Host == NHost, Parents == []]),
    qlc:e(Q);
get_subnodes(Host, Node) ->
    Q = qlc:q([N
                || #pubsub_node{nodeid = {NHost, _},
                        parents = Parents} =
                    N
                    <- mnesia:table(pubsub_node),
                    Host == NHost, lists:member(Node, Parents)]),
    qlc:e(Q).

get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).

get_subnodes_tree(Host, Node) ->
    case get_node(Host, Node) of
        {error, _} -> [];
        NodeRec -> get_subnodes_of_existing_tree(Host, Node, NodeRec)
    end.

get_subnodes_of_existing_tree(Host, Node, NodeRec) ->
    BasePlugin = binary_to_atom(<<"node_", (NodeRec#pubsub_node.type)/binary>>, utf8),
    BasePath = gen_pubsub_node:node_to_path(BasePlugin, Node),
    mnesia:foldl(fun (#pubsub_node{nodeid = {H, N}} = R, Acc) ->
                         Plugin = binary_to_atom(<<"node_", (R#pubsub_node.type)/binary>>, utf8),
                         Path = gen_pubsub_node:node_to_path(Plugin, N),
                         case lists:prefix(BasePath, Path) and (H == Host) of
                             true -> [R | Acc];
                             false -> Acc
                         end
                 end,
                 [], pubsub_node).

create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jid:to_lower(jid:to_bare(Owner)),
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
        [] ->
            case check_parent_and_its_owner_list(Host, Parents, BJID) of
                true ->
                    Nidx = pubsub_index:new(node),
                    mnesia:write(#pubsub_node{nodeid = {Host, Node},
                            id = Nidx, parents = Parents,
                            type = Type, owners = [BJID],
                            options = Options}),
                    {ok, Nidx};
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
    case catch mnesia:read({pubsub_node, {Host, Parent}}) of
        [#pubsub_node{owners = [{[], Host, []}]}] ->
            true;
        [#pubsub_node{owners = Owners}] ->
            lists:member(BJID, Owners);
        _ ->
            false
    end;
check_parent_and_its_owner_list(_Host, _Parents, _BJID) ->
    false.

delete_node(Host, Node) ->
    Removed = get_subnodes_tree(Host, Node),
    lists:foreach(fun (#pubsub_node{nodeid = {_, SubNode}, id = SubNidx}) ->
                pubsub_index:free(node, SubNidx),
                mnesia:delete({pubsub_node, {Host, SubNode}})
        end,
        Removed),
    Removed.
