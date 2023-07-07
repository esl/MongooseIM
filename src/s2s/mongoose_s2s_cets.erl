-module(mongoose_s2s_cets).
-behaviour(mongoose_s2s_backend).

-export([init/1,
         get_s2s_out_pids/1,
         try_register/3,
         remove_connection/2,
         node_cleanup/1]).

-export([register_secret/2,
         get_shared_secret/1]).

-include("mongoose_logger.hrl").

-define(TABLE, cets_s2s_session).
-define(SECRET_TABLE, cets_s2s_secret).

init(_) ->
    cets:start(?TABLE, #{}),
    cets:start(?SECRET_TABLE, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE),
    cets_discovery:add_table(mongoose_cets_discovery, ?SECRET_TABLE).

%% Pid lists
get_s2s_out_pids(FromTo) ->
    R = {{FromTo, '$1'}},
    ets:select(?TABLE, [{R, [], ['$1']}]).

try_register(Pid, ShouldWriteF, FromTo) ->
    L = get_s2s_out_pids(FromTo),
    case ShouldWriteF(L) of
        true ->
            cets:insert(?TABLE, {{FromTo, Pid}}),
            true;
        false ->
            false
    end.

remove_connection(FromTo, Pid) ->
    cets:delete(?TABLE, {FromTo, Pid}),
    ok.

%% node_cleanup is called on each node in the cluster, when Node is down
node_cleanup(Node) ->
    KeyPattern = {'_', '$1'},
    R = {KeyPattern},
    Guard = {'==', {node, '$1'}, Node},
    ets:select_delete(?TABLE, [{R, [Guard], [true]}]).

%% Secrets
register_secret(HostType, Secret) ->
    cets:insert(?SECRET_TABLE, {HostType, Secret}),
    ok.

get_shared_secret(HostType) ->
    case ets:lookup(?SECRET_TABLE, HostType) of
        [{_HostType, Secret}] ->
            {ok, Secret};
        [] ->
            {error, not_found}
    end.
