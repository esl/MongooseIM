%%%-------------------------------------------------------------------
%%% @doc Stores mam user ids in cache.
%%% This module is a proxy for `mod_mam_rdbms_user' (it should be started).
%%%
%%% There are 2 hooks for `mam_archive_id':
%%% `cached_archive_id/3' and `store_archive_id/3'.
%%%
%%% This module supports several hosts.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_cache_user).

-behaviour(mongoose_module_metrics).
-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1, supported_features/0]).

%% ejabberd handlers
-export([cached_archive_id/3,
         store_archive_id/3,
         remove_archive/3]).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    start_cache(HostType, Opts),
    gen_hook:add_handlers(hooks(HostType, Opts)),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)),
    stop_cache(HostType),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    Opts = gen_mod:get_module_opts(HostType, ?MODULE),
    hooks(HostType, Opts).

-spec hooks(mongooseim:host_type(), gen_mod:module_opts()) -> any().
hooks(HostType, Opts) ->
    PM = gen_mod:get_opt(pm, Opts, false),
    MUC = gen_mod:get_opt(muc, Opts, false),
    maybe_pm_hooks(PM, HostType) ++ maybe_muc_hooks(MUC, HostType).

maybe_pm_hooks(true, HostType) -> pm_hooks(HostType);
maybe_pm_hooks(false, _HostType) -> [].

maybe_muc_hooks(true, HostType) -> muc_hooks(HostType);
maybe_muc_hooks(false, _HostType) -> [].

pm_hooks(HostType) ->
    [{mam_archive_id, HostType, fun ?MODULE:cached_archive_id/3, #{}, 30},
     {mam_archive_id, HostType, fun ?MODULE:store_archive_id/3, #{}, 70},
     {mam_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 100}].

muc_hooks(HostType) ->
    [{mam_muc_archive_id, HostType, fun ?MODULE:cached_archive_id/3, #{}, 30},
     {mam_muc_archive_id, HostType, fun ?MODULE:store_archive_id/3, #{}, 70},
     {mam_muc_remove_archive, HostType, fun ?MODULE:remove_archive/3, #{}, 100}].

%%====================================================================
%% API
%%====================================================================
-spec cached_archive_id(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:archive_id() | undefined,
    Params :: #{owner := jid:jid()},
    Extra :: gen_hook:extra().
cached_archive_id(undefined, #{owner := ArcJid}, #{host_type := HostType}) ->
    case mongoose_user_cache:get_entry(HostType, ?MODULE, ArcJid) of
        #{id := ArchId} ->
            {ok, ArchId};
        _ ->
            put(mam_not_cached_flag, true),
            {ok, undefined}
    end.

-spec store_archive_id(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:archive_id() | undefined,
    Params :: #{owner := jid:jid()},
    Extra :: gen_hook:extra().
store_archive_id(ArchId, #{owner := ArcJid}, #{host_type := HostType}) ->
    case erase(mam_not_cached_flag) of
        undefined ->
            {ok, ArchId};
        true ->
            mongoose_user_cache:merge_entry(HostType, ?MODULE, ArcJid, #{id => ArchId}),
            {ok, ArchId}
    end.

-spec remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, owner => jid:jid(), room => jid:jid()},
    Extra :: gen_hook:extra().
remove_archive(Acc, #{owner := ArcJid}, #{host_type := HostType}) ->
    mongoose_user_cache:delete_user(HostType, ?MODULE, ArcJid),
    {ok, Acc};
remove_archive(Acc, #{room := ArcJid}, #{host_type := HostType}) ->
    mongoose_user_cache:delete_user(HostType, ?MODULE, ArcJid),
    {ok, Acc}.

%%====================================================================
%% internal
%%====================================================================
-spec start_cache(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_cache(HostType, Opts) ->
    mongoose_user_cache:start_new_cache(HostType, ?MODULE, Opts).

-spec stop_cache(mongooseim:host_type()) -> any().
stop_cache(HostType) ->
    mongoose_user_cache:stop_cache(HostType, ?MODULE).
