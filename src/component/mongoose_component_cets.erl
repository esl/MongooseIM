-module(mongoose_component_cets).
-behaviour(mongoose_component_backend).

-export([init/1,
         node_cleanup/1,
         register_components/1,
         unregister_components/1,
         lookup_component/1,
         lookup_component/2,
         get_all_components/1]).

-include("external_component.hrl").
-define(TABLE, cets_external_component).

init(_) ->
    cets:start(?TABLE, #{type => bag, keypos => 2}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE).

node_cleanup(Node) ->
    Components = ets:match_object(?TABLE, #external_component{node = Node, _ = '_'}),
    unregister_components(Components).

register_components(Components) ->
    cets:insert_many(?TABLE, Components),
    ok.

unregister_components(Components) ->
    cets:delete_objects(?TABLE, Components),
    ok.

lookup_component(Domain) ->
    ets:lookup(?TABLE, Domain).

lookup_component(Domain, Node) ->
    ets:match_object(?TABLE, #external_component{domain = Domain, node = Node, _ = '_'}).

get_all_components(all) ->
    MatchAll = {#external_component{ domain = '$1', _ = '_' }, [], ['$1']},
    ets:select(?TABLE, [MatchAll]);
get_all_components(only_public) ->
    MatchNonHidden = {#external_component{ domain = '$1', is_hidden = false, _ = '_' }, [], ['$1']},
    ets:select(?TABLE, [MatchNonHidden]).
