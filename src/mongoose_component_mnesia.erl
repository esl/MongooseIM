-module(mongoose_component_mnesia).
-export([init/1,
         node_cleanup/1]).

-include("external_component.hrl").

init(_) ->
    update_tables(),
    %% add distributed service_component routes
    mongoose_mnesia:create_table(external_component,
                        [{attributes, record_info(fields, external_component)},
                         {type, bag}, {ram_copies, [node()]}]),

update_tables() ->
    case catch mnesia:table_info(external_component, attributes) of
        [domain, handler, node] ->
            mnesia:delete_table(external_component);
        [domain, handler, node, is_hidden] ->
            ok;
        {'EXIT', _} ->
            ok
    end.

node_cleanup(Node) ->
    Entries = mnesia:dirty_match_object(external_component,
                                        #external_component{node = Node, _ = '_'}),
    [mnesia:dirty_delete_object(external_component, Entry) || Entry <- Entries],
    ok.
