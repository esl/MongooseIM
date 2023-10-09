-module(mongoose_component_mnesia).
-behaviour(mongoose_component_backend).

-export([init/1,
         node_cleanup/1,
         register_components/1,
         unregister_components/1,
         lookup_component/1,
         lookup_component/2,
         get_all_components/1]).

-include("external_component.hrl").

init(_) ->
    update_tables(),
    %% add distributed service_component routes
    mongoose_mnesia:create_table(external_component,
                                 [{type, bag},
                                  {ram_copies, [node()]},
                                  {attributes,
                                   record_info(fields, external_component)}]).

update_tables() ->
    %% delete old schema
    case catch mnesia:table_info(external_componenst, local_content) of
        true ->
            mnesia:delete_table(external_component);
        _ ->
            ok
    end.

node_cleanup(Node) ->
    Entries = mnesia:dirty_match_object(external_component,
                                        #external_component{node = Node, _ = '_'}),
    [mnesia:dirty_delete_object(external_component, Entry) || Entry <- Entries],
    ok.

register_components(Components) ->
    F = fun() ->
            lists:foreach(fun mnesia:write/1, Components)
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> error({mnesia_aborted_write, Reason})
    end.

unregister_components(Components) ->
    F = fun() ->
            lists:foreach(fun do_unregister_component/1, Components)
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

do_unregister_component(Component) ->
    ok = mnesia:delete_object(external_component, Component, write).

lookup_component(Domain) ->
    mnesia:dirty_read(external_component, Domain).

lookup_component(Domain, Node) ->
    mnesia:dirty_match_object(external_component,
                              #external_component{domain = Domain, node = Node, _ = '_'}).

get_all_components(all) ->
    mnesia:dirty_all_keys(external_component);
get_all_components(only_public) ->
    MatchNonHidden = {#external_component{ domain = '$1', is_hidden = false, _ = '_' }, [], ['$1']},
    mnesia:dirty_select(external_component, [MatchNonHidden]).
