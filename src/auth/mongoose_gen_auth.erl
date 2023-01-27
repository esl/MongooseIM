%%% @doc Behaviour and API for authentication backends
-module(mongoose_gen_auth).

-export([start/2,
         stop/2,
         config_spec/1,
         supports_sasl_module/3,
         authorize/2,
         check_password/5,
         check_password/7,
         set_password/5,
         try_register/5,
         get_registered_users/4,
         get_registered_users_number/4,
         get_password/4,
         get_password_s/4,
         does_user_exist/4,
         supported_features/1,
         remove_user/4,
         remove_domain/3]).

-ignore_xref([behaviour_info/1]).

-import(mongoose_lib, [is_exported/3]).

-type t() :: module().
-export_type([t/0]).

%% Mandatory callbacks

-callback start(HostType :: mongooseim:host_type()) -> ok.

-callback stop(HostType :: mongooseim:host_type()) -> ok.

-callback config_spec() -> mongoose_config_spec:config_section().

-callback supports_sasl_module(HostType :: mongooseim:host_type(),
                               Module :: cyrsasl:sasl_module()) ->
    boolean().

-callback does_user_exist(HostType :: mongooseim:host_type(),
                          User :: jid:luser(),
                          Server :: jid:lserver()) ->
    boolean() | {error, atom()}.

%% credentials already contain host type
-callback authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                               | {error, any()}.

%% Optional callbacks

-callback try_register(HostType :: mongooseim:host_type(),
                       User :: jid:luser(),
                       Server :: jid:lserver(),
                       Password :: binary()) ->
    ok | {error, exists | not_allowed | term()}.

-callback get_registered_users(HostType :: mongooseim:host_type(),
                               Server :: jid:lserver(),
                               Opts :: list()) ->
    [jid:simple_bare_jid()].

-callback get_registered_users_number(HostType :: mongooseim:host_type(),
                                      Server :: jid:lserver(),
                                      Opts :: list()) ->
    non_neg_integer().

-callback get_password(HostType :: mongooseim:host_type(),
                       User :: jid:luser(),
                       Server :: jid:lserver()) ->
    ejabberd_auth:passterm() | false.

-callback get_password_s(HostType :: mongooseim:host_type(),
                         User :: jid:luser(),
                         Server :: jid:lserver()) ->
    binary().

-callback set_password(HostType :: mongooseim:host_type(),
                       User :: jid:luser(),
                       Server :: jid:lserver(),
                       Password :: binary()) ->
    ok | {error, not_allowed | invalid_jid | user_not_found}.

-callback remove_user(HostType :: mongooseim:host_type(),
                      User :: jid:luser(),
                      Server :: jid:lserver()) ->
    ok | {error, not_allowed}.

-callback remove_domain(HostType :: mongooseim:host_type(), Server :: jid:lserver()) ->
    ok | {error, term()}.

-callback supported_features() -> [Feature::atom()].

%% Implementation of check_password callbacks is required
%% for the corresponding check_password interfaces of ejabberd_auth module.
%%
%% With the help of ejabberd_auth:authorize_with_check_password/2 function,
%% these callbacks can be reused to simplify implementation of the M:authorize/1 interface.
-callback check_password(HostType :: mongooseim:host_type(),
                         LUser :: jid:luser(),
                         LServer :: jid:lserver(),
                         Password :: binary()) -> boolean().

-callback check_password(HostType :: mongooseim:host_type(),
                         LUser :: jid:luser(),
                         LServer :: jid:lserver(),
                         Password :: binary(),
                         Digest :: binary(),
                         DigestGen :: fun()) -> boolean().

%% See the API function definitions below for default values
-optional_callbacks([config_spec/0,
                     try_register/4,
                     get_registered_users/3,
                     get_registered_users_number/3,
                     get_password/3,
                     get_password_s/3,
                     set_password/4,
                     remove_user/3,
                     remove_domain/2,
                     supported_features/0,
                     check_password/4,
                     check_password/6]).

-include("mongoose_config_spec.hrl").

%% API

-spec start(ejabberd_auth:authmodule(), mongooseim:host_type()) -> ok.
start(Mod, HostType) ->
    Mod:start(HostType).

-spec stop(ejabberd_auth:authmodule(), mongooseim:host_type()) -> ok.
stop(Mod, HostType) ->
    Mod:stop(HostType).

-spec config_spec(ejabberd_auth:authmodule()) -> mongoose_config_spec:config_section().
config_spec(Mod) ->
    case is_exported(Mod, config_spec, 0) of
        true -> Mod:config_spec();
        false -> #section{}
    end.

-spec supports_sasl_module(ejabberd_auth:authmodule(), mongooseim:host_type(),
                           cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(Mod, HostType, SASLModule) ->
    Mod:supports_sasl_module(HostType, SASLModule).

-spec does_user_exist(ejabberd_auth:authmodule(), mongooseim:host_type(),
                      jid:luser(), jid:lserver()) ->
          boolean() | {error, atom()}.
does_user_exist(Mod, HostType, LUser, LServer) ->
    Mod:does_user_exist(HostType, LUser, LServer).

-spec authorize(ejabberd_auth:authmodule(), mongoose_credentials:t()) ->
          {ok, mongoose_credentials:t()} | {error, any()}.
authorize(Mod, Creds) ->
    Mod:authorize(Creds).

-spec try_register(ejabberd_auth:authmodule(), mongooseim:host_type(),
                   jid:luser(), jid:lserver(), binary()) ->
          ok | {error, exists | not_allowed | term()}.
try_register(Mod, HostType, LUser, LServer, Password) ->
    case is_exported(Mod, try_register, 4) of
        true -> Mod:try_register(HostType, LUser, LServer, Password);
        false -> {error, not_allowed}
    end.

-spec get_registered_users(ejabberd_auth:authmodule(), mongooseim:host_type(),
                           jid:lserver(), list()) ->
          [jid:simple_bare_jid()].
get_registered_users(Mod, HostType, LServer, Opts) ->
    case is_exported(Mod, get_registered_users, 3) of
        true -> Mod:get_registered_users(HostType, LServer, Opts);
        false -> []
    end.

-spec get_registered_users_number(ejabberd_auth:authmodule(), mongooseim:host_type(),
                                  jid:lserver(), list()) ->
          non_neg_integer().
get_registered_users_number(Mod, HostType, LServer, Opts) ->
    case is_exported(Mod, get_registered_users_number, 3) of
        true -> Mod:get_registered_users_number(HostType, LServer, Opts);
        false -> 0
    end.

-spec get_password(ejabberd_auth:authmodule(), mongooseim:host_type(),
                   jid:luser(), jid:lserver()) ->
          ejabberd_auth:passterm() | false.
get_password(Mod, HostType, LUser, LServer) ->
    case is_exported(Mod, get_password, 3) of
        true -> Mod:get_password(HostType, LUser, LServer);
        false -> false
    end.

-spec get_password_s(ejabberd_auth:authmodule(), mongooseim:host_type(),
                     jid:luser(), jid:lserver()) ->
          binary().
get_password_s(Mod, HostType, LUser, LServer) ->
    case is_exported(Mod, get_password_s, 3) of
        true -> Mod:get_password_s(HostType, LUser, LServer);
        false -> <<>>
    end.

-spec set_password(ejabberd_auth:authmodule(), mongooseim:host_type(),
                   jid:luser(), jid:lserver(), binary()) ->
          ok | {error, not_allowed | invalid_jid}.
set_password(Mod, HostType, LUser, LServer, Password) ->
    case is_exported(Mod, set_password, 4) of
        true -> Mod:set_password(HostType, LUser, LServer, Password);
        false -> {error, not_allowed}
    end.

-spec remove_user(ejabberd_auth:authmodule(), mongooseim:host_type(),
                  jid:luser(), jid:lserver()) ->
          ok | {error, not_allowed}.
remove_user(Mod, HostType, LUser, LServer) ->
    case is_exported(Mod, remove_user, 3) of
        true -> Mod:remove_user(HostType, LUser, LServer);
        false -> {error, not_allowed}
    end.

-spec remove_domain(ejabberd_auth:authmodule(), mongooseim:host_type(), jid:lserver()) ->
          ok | {error, term()}.
remove_domain(Mod, HostType, Domain) ->
    case is_exported(Mod, remove_domain, 2) of
        true -> Mod:remove_domain(HostType, Domain);
        false -> ok
    end.

-spec supported_features(ejabberd_auth:authmodule()) -> [atom()].
supported_features(Mod) ->
    case is_exported(Mod, supported_features, 0) of
        true -> Mod:supported_features();
        false -> []
    end.

-spec check_password(ejabberd_auth:authmodule(), mongooseim:host_type(),
                     jid:luser(), jid:lserver(), binary()) -> boolean().
check_password(Mod, HostType, LUser, LServer, Password) ->
    case is_exported(Mod, check_password, 4) of
        true -> Mod:check_password(HostType, LUser, LServer, Password);
        false -> false
    end.

-spec check_password(ejabberd_auth:authmodule(), mongooseim:host_type(),
                     jid:luser(), jid:lserver(), binary(), binary(), fun()) -> boolean().
check_password(Mod, HostType, LUser, LServer, Password, Digest, DigestGen) ->
    case is_exported(Mod, check_password, 6) of
        true -> Mod:check_password(HostType, LUser, LServer, Password, Digest, DigestGen);
        false -> false
    end.
