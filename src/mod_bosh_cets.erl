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
    cets:start(?TABLE, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE).

%% Session key (sid) is unique, so we don't expect conflicts
%% So, the confict resolution could be avoided
-spec create_session(mod_bosh:session()) -> any().
create_session(Session) ->
    cets:insert(?TABLE, session_to_tuple(Session)).

-spec delete_session(mod_bosh:sid()) -> any().
delete_session(Sid) ->
    cets:delete(?TABLE, Sid).

-spec get_session(mod_bosh:sid()) -> [mod_bosh:session()].
get_session(Sid) ->
    tuples_to_records(ets:lookup(?TABLE, Sid)).

-spec get_sessions() -> [mod_bosh:session()].
get_sessions() ->
    tuples_to_records(ets:tab2list(?TABLE)).

-spec node_cleanup(atom()) -> any().
node_cleanup(Node) ->
    Guard = {'==', {node, '$1'}, Node},
    R = {'_', '$1'},
    cets:sync(?TABLE),
    %% We don't need to replicate deletes
    %% We remove the local content here
    ets:select_delete(?TABLE, [{R, [Guard], [true]}]),
    ok.

%% Simple format conversion
session_to_tuple(#bosh_session{sid = Sid, socket = Pid}) when is_pid(Pid) ->
    {Sid, Pid}.

tuples_to_records(Tuples) ->
    [tuple_to_record(Tuple) || Tuple <- Tuples].

tuple_to_record({Sid, Pid}) ->
    #bosh_session{sid = Sid, socket = Pid}.
