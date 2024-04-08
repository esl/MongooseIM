-module(mod_bosh_cets).

-behaviour(mod_bosh_backend).

%% mod_bosh_backend callbacks
-export([start/0,
         create_session/1,
         delete_session/1,
         get_session/1,
         get_sessions/0,
         node_cleanup/1]).

-include("mod_bosh.hrl").

-define(TABLE, cets_bosh).

-spec start() -> any().
start() ->
    cets:start(?TABLE, #{keypos => 2}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE).

%% Session key (sid) is unique, so we don't expect conflicts
%% So, the confict resolution could be avoided
-spec create_session(mod_bosh:session()) -> any().
create_session(Session) ->
    cets:insert(?TABLE, Session).

-spec delete_session(mod_bosh:sid()) -> any().
delete_session(Sid) ->
    cets:delete(?TABLE, Sid).

-spec get_session(mod_bosh:sid()) -> [mod_bosh:session()].
get_session(Sid) ->
    ets:lookup(?TABLE, Sid).

-spec get_sessions() -> [mod_bosh:session()].
get_sessions() ->
    ets:tab2list(?TABLE).

-spec node_cleanup(atom()) -> any().
node_cleanup(Node) ->
    Guard = {'==', {node, '$2'}, Node},
    R = {'_', '$1', '$2'},
    cets:ping_all(?TABLE),
    Keys = ets:select(?TABLE, [{R, [Guard], ['$1']}]),
    cets:delete_many(?TABLE, Keys).
