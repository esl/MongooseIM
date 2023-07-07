-module(mongoose_s2s_cets).
-behaviour(mongoose_s2s_backend).

-export([init/1,
         get_s2s_out_pids/1,
         try_register/2,
         remove_connection/2,
         node_cleanup/1]).

-export([register_secret/2,
         get_shared_secret/1]).

%% Internal usage (export so the callback would survive multiple code reloads)
-export([handle_secret_conflict/2]).

-include("mongoose_logger.hrl").

-define(TABLE, cets_s2s_session).
-define(SECRET_TABLE, cets_s2s_secret).

init(_) ->
    cets:start(?TABLE, #{}),
    %% Non-random, non-node-specific keys
    %% This means that default merging would not work
    cets:start(?SECRET_TABLE, #{handle_conflict => fun ?MODULE:handle_secret_conflict/2}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE),
    cets_discovery:add_table(mongoose_cets_discovery, ?SECRET_TABLE).

%% Store the most recent value:
%% - first element of the tuple is the same and it is the key.
%% - second element is a timestamp, so comparing tuples works.
%% Even if we choose the wrong record - nothing bad would happen
%% (we still need to choose one).
%% Choosing the record with the highest timestamp is just a logical behaviour
%% (it also matches the logic of mongoose_s2s_lib:check_shared_secret/2, where updated secret
%% in the config is updated across all nodes in the cluster).
handle_secret_conflict(Rec1, Rec2) when Rec1 > Rec2 ->
    Rec1;
handle_secret_conflict(_Rec1, Rec2) ->
    Rec2.

%% Pid lists
-spec get_s2s_out_pids(ejabberd_s2s:fromto()) -> ejabberd_s2s:s2s_pids().
get_s2s_out_pids(FromTo) ->
    R = {{FromTo, '$1'}},
    ets:select(?TABLE, [{R, [], ['$1']}]).

-spec try_register(Pid :: pid(),
                   FromTo :: ejabberd_s2s:fromto()) -> boolean().
try_register(Pid, FromTo) ->
    Pids = get_s2s_out_pids(FromTo),
    case mongoose_s2s_lib:need_more_connections(FromTo, Pids) of
        true ->
            cets:insert(?TABLE, {{FromTo, Pid}}),
            true;
        false ->
            false
    end.

-spec remove_connection(FromTo :: ejabberd_s2s:fromto(), Pid :: pid()) -> ok.
remove_connection(FromTo, Pid) ->
    cets:delete(?TABLE, {FromTo, Pid}),
    ok.

%% node_cleanup is called on each node in the cluster, when Node is down
-spec node_cleanup(Node :: node()) -> ok.
node_cleanup(Node) ->
    KeyPattern = {'_', '$1'},
    R = {KeyPattern},
    Guard = {'==', {node, '$1'}, Node},
    ets:select_delete(?TABLE, [{R, [Guard], [true]}]),
    ok.

%% Secrets
-spec register_secret(HostType :: mongooseim:host_type(),
                      Secret :: ejabberd_s2s:base16_secret()) -> ok.
register_secret(HostType, Secret) ->
    %% We store timestamp so we could use it when merging two tables when clustering.
    %% Secrets is a very small table and get_shared_secret is called rarely,
    %% so having an extra field is not a problem.
    TS = erlang:system_time(microsecond),
    cets:insert(?SECRET_TABLE, {HostType, TS, Secret}),
    ok.

-spec get_shared_secret(mongooseim:host_type()) ->
    {ok, ejabberd_s2s:base16_secret()} | {error, not_found}.
get_shared_secret(HostType) ->
    case ets:lookup(?SECRET_TABLE, HostType) of
        [{_HostType, _TS, Secret}] ->
            {ok, Secret};
        [] ->
            {error, not_found}
    end.
