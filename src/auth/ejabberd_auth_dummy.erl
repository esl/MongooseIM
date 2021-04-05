-module(ejabberd_auth_dummy).

%% API
-export([start/1,
         stop/1,
         authorize/1,
         plain_password_required/0,
         remove_domain/1,
         supported_features/0]).

%% ejabberd_auth compatibility layer - not supported features
-behaviour(ejabberd_gen_auth).
-export([set_password/4,
         try_register/4,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         does_user_exist/2,
         remove_user/2,
         supports_sasl_module/2,
         scram_passwords/0]).

-include("mongoose.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(HostType :: binary()) -> ok.
start(_HostType) ->
    ok.

-spec stop(HostType :: binary()) -> ok.
stop(_HostType) ->
    ok.

authorize(Creds) ->
    timer:sleep(50 + rand:uniform(450)),
    {ok, mongoose_credentials:set(Creds, auth_module, ?MODULE)}.

plain_password_required() ->
    true.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(_HostType, _User, _Server, _Password) ->
    ok.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
try_register(_HostType, _User, _Server, _Password) ->
    ok.

dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Server) ->
    [].

get_vh_registered_users(_Server, _) ->
    [].

get_vh_registered_users_number(_Server) ->
    0.

get_vh_registered_users_number(_Server, _) ->
    0.

get_password(_User, _Server) ->
    <<>>.

get_password_s(_User, _Server) ->
    <<>>.

%% @spec (User, Server) -> true | false | {error, Error}
does_user_exist(_User, _Server) ->
    true.

%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
remove_user(_User, _Server) ->
    ok.

remove_domain(_Server) -> ok.

supported_features() -> [dynamic_domains].

supports_sasl_module(_, cyrsasl_plain) -> true;
supports_sasl_module(_, _) -> false.

scram_passwords() ->
    not_supported.
