%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Allocates unique ids for each node.
%%% @end
%%%-------------------------------------------------------------------
-module(ejabberd_node_id).
-export([start/0, node_id/0]).


-include("ejabberd.hrl").
-include("jlib.hrl").

%%====================================================================
%% API
%%====================================================================

-record(node, {name, id}).

start() ->
    mnesia:create_table(node,
            [{ram_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, node)}]),
    mnesia:add_table_copy(node, node(), ram_copies),
    register_node(node()),
    ok.

register_node(NodeName) ->
    {atomic, _} = mnesia:transaction(fun() ->
        case mnesia:read(node, NodeName) of
            [] ->
                mnesia:write(#node{name = NodeName, id = next_node_id()});
            [_] -> ok
        end
        end),
    ok.

%% @doc Return an integer node ID.
-spec node_id() -> non_neg_integer().
node_id() ->
    %% Save result into the process's memory space.
    case get(node_id) of
        undefined ->
            NodeId = select_node_id(node()),
            put(node_id, NodeId),
            NodeId;
        NodeId ->
            NodeId
    end.


next_node_id() ->
    max_node_id() + 1.

max_node_id() ->
    mnesia:foldl(fun(#node{id=Id}, Max) -> max(Id, Max) end, 0, node).

select_node_id(NodeName) ->
    case mnesia:dirty_read(node, NodeName) of
        [#node{id=Id}] -> {ok, Id};
        [] -> {error, not_found}
    end.
