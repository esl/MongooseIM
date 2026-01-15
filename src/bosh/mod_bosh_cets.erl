-module(mod_bosh_cets).

-behaviour(mod_bosh_backend).

%% mod_bosh_backend callbacks
-export([start/1,
         create_session/2,
         delete_session/2,
         get_session/2,
         get_sessions/1,
         node_cleanup/2]).

-include("mod_bosh.hrl").

-spec start(mongooseim:host_type()) -> any().
start(HostType) ->
    cets:start(table_name(HostType), #{keypos => 2}),
    cets_discovery:add_table(mongoose_cets_discovery, table_name(HostType)).

%% Session key (sid) is unique, so we don't expect conflicts
%% So, the confict resolution could be avoided
-spec create_session(mongooseim:host_type(), mod_bosh:session()) -> ok.
create_session(HostType, #bosh_session{} = Session) ->
    cets:insert(table_name(HostType), Session).

-spec delete_session(mongooseim:host_type(), mod_bosh:sid()) -> any().
delete_session(HostType, Sid) ->
    cets:delete(table_name(HostType), Sid).

-spec get_session(mongooseim:host_type(), mod_bosh:sid()) -> [mod_bosh:session()].
get_session(HostType, Sid) ->
    ets:lookup(table_name(HostType), Sid).

-spec get_sessions(mongooseim:host_type()) -> [mod_bosh:session()].
get_sessions(HostType) ->
    ets:tab2list(table_name(HostType)).

-spec node_cleanup(mongooseim:host_type(), atom()) -> any().
node_cleanup(HostType, Node) ->
    Guard = {'==', {node, '$2'}, Node},
    R = {'_', '$1', '$2'},
    Table = table_name(HostType),
    cets:ping_all(Table),
    Keys = ets:select(Table, [{R, [Guard], ['$1']}]),
    cets:delete_many(Table, Keys).

%% Helpers

-spec table_name(mongooseim:host_type()) -> cets:table_name().
table_name(HostType) ->
    binary_to_atom(<<"mod_bosh_cets_", HostType/binary>>).
