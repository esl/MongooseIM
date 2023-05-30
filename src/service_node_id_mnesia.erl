-module(service_node_id_mnesia).
-behaviour(service_node_id_backend).

-export([init/1]).

-record(node, {name :: atom(),
               id :: service_node_id:nodeid() }).

init(_Opts) ->
    mnesia:create_table(node,
        [{ram_copies, [node()]}, {type, set},
         {attributes, record_info(fields, node)}]),
    mnesia:add_table_copy(node, node(), ram_copies),
    mnesia:add_table_index(node, id),
    register_node(node()),
    [#node{id = Id}] = mnesia:dirty_read(node, node()),
    {ok, Id}.

-spec register_node(atom()) -> ok.
register_node(NodeName) ->
    {atomic, _} = mnesia:transaction(fun() ->
        case mnesia:read(node, NodeName) of
            [] ->
                mnesia:write(#node{name = NodeName, id = next_node_id()});
            [_] -> ok
        end
        end),
    ok.

-spec next_node_id() -> service_node_id:nodeid().
next_node_id() ->
    max_node_id() + 1.

-spec max_node_id() -> service_node_id:nodeid().
max_node_id() ->
    mnesia:foldl(fun(#node{id=Id}, Max) -> max(Id, Max) end, 0, node).
