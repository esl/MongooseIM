%% @doc Caches info about non-anonymous users using a queue of ets tables
-module(mod_cache_users).

-behaviour(gen_mod).

%% cache backend sharing
-export([start_new_cache/2]).

%% gen_mod API
-export([start/2]).
-export([stop/1]).
-export([config_spec/0]).
-export([supported_features/0]).

%% Hooks.
-export([does_cached_user_exist/4]).
-export([maybe_put_user_into_cache/4]).
-export([remove_user/3]).
-export([remove_domain/3]).

-ignore_xref([does_cached_user_exist/4, maybe_put_user_into_cache/4,
              remove_domain/3, remove_user/3]).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    start_cache(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    stop_cache(HostType),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    mongoose_config_spec:user_cache().

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [
     %% These hooks must run before and after the ejabberd_auth does_user_exist hook
     {does_user_exist, HostType, ?MODULE, does_cached_user_exist, 30},
     {does_user_exist, HostType, ?MODULE, maybe_put_user_into_cache, 70},
     %% It is important that these two handlers happen _after_ ejabberd_auth
     %% but _before_ all other modules
     {remove_user, HostType, ?MODULE, remove_user, 10},
     {remove_domain, HostType, ?MODULE, remove_domain, 20}
    ].

%%====================================================================
%% Cache sharing
%%====================================================================
-spec cache_name(mongooseim:host_type()) -> atom().
cache_name(HostType) ->
    gen_mod:get_module_proc(HostType, ?MODULE).

-spec start_new_cache(atom(), gen_mod:module_opts()) -> any().
start_new_cache(CacheName, Opts) ->
    CacheOpts = #{merger_fun => gen_mod:get_opt(merger_fun, Opts, fun maps:merge/2),
                  segment_num => gen_mod:get_opt(number_of_segments, Opts, 3),
                  strategy => gen_mod:get_opt(strategy, Opts, fifo),
                  ttl => gen_mod:get_opt(time_to_live, Opts, {hours, 8})
                 },
    Spec = #{id => CacheName, start => {segmented_cache, start_link, [CacheName, CacheOpts]},
             restart => permanent, shutdown => 5000,
             type => worker, modules => [segmented_cache]},
    {ok, _} = ejabberd_sup:start_child(Spec).

%%====================================================================
%% Hooks
%%====================================================================

-spec remove_user(Acc :: mongoose_acc:t(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver() | string()) -> mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    HostType = mongoose_acc:host_type(Acc),
    CacheName = cache_name(HostType),
    segmented_cache:delete_entry(CacheName, key(LUser, LServer)),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    CacheName = cache_name(HostType),
    segmented_cache:delete_pattern(CacheName, {{'_', Domain}, '_'}),
    Acc.

%%====================================================================
%% Helpers
%%====================================================================

-spec does_cached_user_exist(Status :: boolean(),
                             HostType :: mongooseim:host_type(),
                             Jid :: jid:jid(),
                             RequestType :: ejabberd_auth:exist_type()) ->
    boolean().
does_cached_user_exist(false, HostType, Jid, stored) ->
    CacheName = cache_name(HostType),
    case segmented_cache:is_member(CacheName, key(Jid)) of
        true -> {stop, true};
        false -> false
    end;
does_cached_user_exist(Status, _, _, _) ->
    Status.

-spec maybe_put_user_into_cache(Status :: boolean(),
                                HostType :: mongooseim:host_type(),
                                Jid :: jid:jid(),
                                RequestType :: ejabberd_auth:exist_type()) ->
    boolean().
maybe_put_user_into_cache(true, HostType, Jid, stored) ->
    CacheName = cache_name(HostType),
    segmented_cache:merge_entry(CacheName, key(Jid), #{});
maybe_put_user_into_cache(Status, _, _, _) ->
    Status.

-compile({inline, [key/1, key/2]}).
-spec key(jid:jid()) -> jid:simple_bare_jid().
key(Jid) ->
    jid:to_lus(Jid).

-spec key(jid:luser(), jid:lserver()) -> jid:simple_bare_jid().
key(LUser, LServer) ->
    {LUser, LServer}.

-spec start_cache(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_cache(HostType, Opts) ->
    CacheName = cache_name(HostType),
    start_new_cache(CacheName, Opts).

-spec stop_cache(mongooseim:host_type()) -> any().
stop_cache(HostType) ->
    ok = ejabberd_sup:stop_child(cache_name(HostType)).
