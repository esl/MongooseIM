-module(mongoose_node_num_mnesia).

-export([init/0]).

-record(node_num, {name :: atom(),
                   num :: mongoose_node_num:node_num() }).

init() ->
    mongoose_mnesia:create_table(node_num,
        [{ram_copies, [node()]}, {type, set},
         {attributes, record_info(fields, node_num)}]),
    mnesia:add_table_index(node_num, num),
    register_node(node()),
    [#node_num{num = Num}] = mnesia:dirty_read(node_num, node()),
    mongoose_node_num:set_node_num(Num),
    ok.

-spec register_node(atom()) -> ok.
register_node(NodeName) ->
    {atomic, _} = mnesia:transaction(fun() ->
        case mnesia:read(node_num, NodeName) of
            [] ->
                mnesia:write(#node_num{name = NodeName, num = next_node_num()});
            [_] -> ok
        end
        end),
    ok.

-spec next_node_num() -> mongoose_node_num:node_num().
next_node_num() ->
    max_node_num() + 1.

-spec max_node_num() -> mongoose_node_num:node_num().
max_node_num() ->
    mnesia:foldl(fun(#node_num{num = Num}, Max) -> max(Num, Max) end, 0, node_num).
