%% @doc Caches info about non-anonymous users using a queue of ets tables
-module(mod_cache_users).

-include("mongoose_config_spec.hrl").

-behaviour(gen_mod).

%% gen_mod API
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

%% Hooks.
-export([does_cached_user_exist/3, maybe_put_user_into_cache/3,
         remove_user/3, remove_domain/3]).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mongoose_user_cache:start_new_cache(HostType, ?MODULE, Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mongoose_user_cache:stop_cache(HostType, ?MODULE),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = Items,
             defaults = Defaults} = Sec = mongoose_user_cache:config_spec(),
    Sec#section{items = maps:remove(<<"module">>, Items),
                defaults = maps:remove(<<"module">>, Defaults)}.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [
     %% These hooks must run before and after the ejabberd_auth does_user_exist hook
     {does_user_exist, HostType, fun ?MODULE:does_cached_user_exist/3, #{}, 30},
     {does_user_exist, HostType, fun ?MODULE:maybe_put_user_into_cache/3, #{}, 70},
     %% It is important that these two handlers happen _after_ ejabberd_auth
     %% but _before_ all other modules
     {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 10},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 20}
    ].

%%====================================================================
%% Hooks
%%====================================================================

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: #{host_type := mongooseim:host_type()}.
remove_user(Acc, #{jid := Jid}, #{host_type := HostType}) ->
    mongoose_user_cache:delete_user(HostType, ?MODULE, Jid),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok , Acc} when
      Acc :: mongoose_domain_api:remove_domain_acc(),
      Params :: #{domain := jid:lserver()},
      Extra :: #{host_type := mongooseim:host_type()}.
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    mongoose_user_cache:delete_domain(HostType, ?MODULE, Domain),
    {ok, Acc}.

-spec does_cached_user_exist(Acc, Params, Extra) -> {stop | ok , Acc} when
      Acc :: boolean(),
      Params :: #{jid := jid:jid(), request_type := ejabberd_auth:exist_type()},
      Extra :: #{host_type := mongooseim:host_type()}.
does_cached_user_exist(false,
                       #{jid := Jid, request_type := stored},
                       #{host_type := HostType}) ->
    case mongoose_user_cache:is_member(HostType, ?MODULE, Jid) of
        true -> {stop, true};
        false -> {ok, false}
    end;
does_cached_user_exist(Acc, _, _) ->
    {ok, Acc}.

-spec maybe_put_user_into_cache(Acc, Params, Extra) -> {ok , Acc} when
      Acc :: boolean(),
      Params :: #{jid := jid:jid(), request_type := ejabberd_auth:exist_type()},
      Extra :: #{host_type := mongooseim:host_type()}.
maybe_put_user_into_cache(true,
                          #{jid := Jid, request_type := stored},
                          #{host_type := HostType}) ->
    mongoose_user_cache:merge_entry(HostType, ?MODULE, Jid, #{}),
    {ok, true};
maybe_put_user_into_cache(Acc, _, _) ->
    {ok, Acc}.
