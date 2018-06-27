%%%-------------------------------------------------------------------
%%% @doc
%%% This is here because there are pool options which have to be given when calling
%%% the pool (selection strategy, timeout), while we want to set it once for the pool and not
%%% worry about them later, hence additional storage.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_wpool).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose.hrl").
%% API
-export([setup_env/0, get_pool_settings/1, delete_pool_settings/1, save_pool_settings/2]).

setup_env() ->
    wpool:start(),
    case ets:info(pool_settings_tab()) of
        undefined ->
            % we set heir here because the whole thing may be started by an ephemeral process
            ets:new(pool_settings_tab(), [named_table, public,
                {keypos, 2},
                {heir, whereis(wpool_sup), undefined}]);
        _ ->
            ok
    end.

get_pool_settings(PoolName) ->
    case ets:lookup(pool_settings_tab(), PoolName) of
        [PoolOpts] ->
            PoolOpts;
        [] ->
            undefined
    end.

save_pool_settings(PoolName, PoolSettings) ->
    ets:insert(pool_settings_tab(), PoolSettings#pool{name = PoolName}).

delete_pool_settings(PoolName) ->
    ets:delete(pool_settings_tab(), PoolName).

pool_settings_tab() ->
    list_to_atom("mongoose_worker_pool_settings").

