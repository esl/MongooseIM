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

%% gen_mod handlers
-export([start/2]).
-export([deps/2]).
-export([stop/1]).
-export([supported_features/0]).

%% ejabberd handlers
-export([cached_archive_id/3,
         store_archive_id/3,
         remove_archive/4]).

-ignore_xref([start/2, stop/1, deps/2, supported_features/0,
              cached_archive_id/3, store_archive_id/3, remove_archive/4]).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    start_cache(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType, Opts)),
    ok.

-spec deps(mongooseim:host_type(), proplists:proplist()) -> gen_mod:deps_list().
deps(_, Opts) ->
    case gen_mod:get_opt(cache_module, Opts, internal) of
        internal -> [];
        mod_cache_users -> [{mod_cache_users, hard}]
    end.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    stop_cache(HostType),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> [ejabberd_hooks:hook()].
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
    [{mam_archive_id, HostType, ?MODULE, cached_archive_id, 30},
     {mam_archive_id, HostType, ?MODULE, store_archive_id, 70},
     {mam_remove_archive, HostType, ?MODULE, remove_archive, 100}].

muc_hooks(HostType) ->
    [{mam_muc_archive_id, HostType, ?MODULE, cached_archive_id, 30},
     {mam_muc_archive_id, HostType, ?MODULE, store_archive_id, 70},
     {mam_muc_remove_archive, HostType, ?MODULE, remove_archive, 100}].

%%====================================================================
%% API
%%====================================================================
-spec cached_archive_id(undefined, mongooseim:host_type(), jid:jid()) ->
    mod_mam:archive_id().
cached_archive_id(undefined, HostType, ArcJid) ->
    CacheName = cache_name(HostType),
    case segmented_cache:get_entry(CacheName, key(ArcJid)) of
        #{id := UserId} -> UserId;
        not_found ->
            put(mam_not_cached_flag, true),
            undefined
    end.

-spec store_archive_id(mod_mam:archive_id(), mongooseim:host_type(), jid:jid())
        -> mod_mam:archive_id().
store_archive_id(UserId, HostType, ArcJid) ->
    case erase(mam_not_cached_flag) of
        undefined ->
            UserId;
        true ->
            CacheName = cache_name(HostType),
            segmented_cache:merge_entry(CacheName, key(ArcJid), #{id => UserId}),
            UserId
    end.

-spec remove_archive(Acc :: map(), HostType :: mongooseim:host_type(),
                     UserId :: mod_mam:archive_id(), ArcJid :: jid:jid()) -> map().
remove_archive(Acc, HostType, _UserID, ArcJid) ->
    CacheName = cache_name(HostType),
    segmented_cache:delete_entry(CacheName, key(ArcJid)),
    Acc.

%%====================================================================
%% internal
%%====================================================================
-spec cache_name(mongooseim:host_type()) -> atom().
cache_name(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, cache_module, internal) of
        internal -> cache_name(HostType, ?MODULE);
        CacheMod -> cache_name(HostType, CacheMod)
    end.

-spec cache_name(mongooseim:host_type(), module()) -> atom().
cache_name(HostType, Module) ->
    gen_mod:get_module_proc(HostType, Module).

-spec start_cache(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_cache(HostType, Opts) ->
    case gen_mod:get_opt(cache_module, Opts, internal) of
        internal -> mod_cache_users:start_new_cache(HostType, ?MODULE, Opts);
        mod_cache_users -> ok
    end.

-spec stop_cache(mongooseim:host_type()) -> any().
stop_cache(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, cache_module, internal) of
        internal -> ejabberd_sup:stop_child(cache_name(HostType, ?MODULE));
        mod_cache_users -> ok
    end.

-compile({inline, [key/1]}).
-spec key(jid:jid()) -> jid:simple_bare_jid().
key(Jid) ->
    jid:to_lus(Jid).
