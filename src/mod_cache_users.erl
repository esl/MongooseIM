%% @doc Caches info about non-anonymous users using a queue of ets tables
-module(mod_cache_users).

-behaviour(gen_mod).

%% gen_mod API
-export([start/2, stop/1, config_spec/0, supported_features/0]).

%% Hooks.
-export([does_cached_user_exist/4, maybe_put_user_into_cache/4,
         remove_user/3, remove_domain/3]).

-ignore_xref([does_cached_user_exist/4, maybe_put_user_into_cache/4,
              remove_domain/3, remove_user/3]).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mongoose_user_cache:start_new_cache(HostType, ?MODULE, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    mongoose_user_cache:stop_cache(HostType, ?MODULE),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    mongoose_user_cache:config_spec().

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
%% Hooks
%%====================================================================

-spec remove_user(Acc :: mongoose_acc:t(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver() | string()) -> mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    HostType = mongoose_acc:host_type(Acc),
    Jid = jid:make_noprep(LUser, LServer, <<>>),
    mongoose_user_cache:delete_user(HostType, ?MODULE, Jid),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    mongoose_user_cache:delete_domain(HostType, ?MODULE, Domain),
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
    case mongoose_user_cache:is_member(HostType, ?MODULE, Jid) of
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
    mongoose_user_cache:merge_entry(HostType, ?MODULE, Jid, #{});
maybe_put_user_into_cache(Status, _, _, _) ->
    Status.
