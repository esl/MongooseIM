%%% @doc Allocates unique ids for each node.
-module(service_node_id).
-export([start/1, stop/0, config_spec/0]).
-export([node_id/0, node_id_to_name/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-type nodeid() :: non_neg_integer().
-record(node, {name :: atom(),
               id :: nodeid() }).

start(_Opts) ->
    mnesia:create_table(node,
            [{ram_copies, [node()]}, {type, set},
             {attributes, record_info(fields, node)}]),
    mnesia:add_table_copy(node, node(), ram_copies),
    mnesia:add_table_index(node, id),
    register_node(node()),
    ok.

stop() ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{}.

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

%% @doc Return an integer node ID.
-spec node_id() -> nodeid().
node_id() ->
    [#node{id=Id}] = mnesia:dirty_read(node, node()),
    Id.

node_id_to_name(ID) ->
    case mnesia:dirty_index_read(node, ID, #node.id) of
        [] ->
            {error, unknown_id};
        [#node{name = Name}] ->
            {ok, Name}
    end.

-spec next_node_id() -> nodeid().
next_node_id() ->
    max_node_id() + 1.

-spec max_node_id() -> nodeid().
max_node_id() ->
    mnesia:foldl(fun(#node{id=Id}, Max) -> max(Id, Max) end, 0, node).
