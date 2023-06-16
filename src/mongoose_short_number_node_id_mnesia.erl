-module(mongoose_short_number_node_id_mnesia).

-export([init/0]).

-record(node, {name :: atom(),
               id :: mongoose_short_number_node_id:node_id() }).

init() ->
    mongoose_lib:create_mnesia_table(node,
        [{ram_copies, [node()]}, {type, set},
         {attributes, record_info(fields, node)}]),
    mnesia:add_table_index(node, id),
    register_node(node()),
    [#node{id = Id}] = mnesia:dirty_read(node, node()),
    mongoose_short_number_node_id:set_node_id(Id),
    ok.

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

-spec next_node_id() -> mongoose_short_number_node_id:node_id().
next_node_id() ->
    max_node_id() + 1.

-spec max_node_id() -> mongoose_short_number_node_id:node_id().
max_node_id() ->
    mnesia:foldl(fun(#node{id=Id}, Max) -> max(Id, Max) end, 0, node).
