%% This module contains the state of mongoose_domain_loader between calls.
-module(mongoose_loader_state).
-export([init/0, set/1, get/1, reset/0]).

-define(TABLE, ?MODULE).

%% Called by mongoose_domain_core
%% The loader state lives till the core process lives.
%% This means it survives service_domain_db restarts.
init() ->
    ets:new(?TABLE, [set, named_table, public]).

set(Val) ->
    ets:insert(?TABLE, {state, Val}).

get(Def) ->
    try
        ets:lookup_element(?MODULE, state, 2)
    catch _:_:_ -> Def
    end.

reset() ->
    ets:insert(?TABLE, {state, reset}).
