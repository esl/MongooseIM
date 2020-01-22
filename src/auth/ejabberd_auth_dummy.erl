-module(ejabberd_auth_dummy).

%% API
-export([start/1,
         stop/1,
         check_password/3,
         check_password/5,
         authorize/1,
         plain_password_required/0]).

%% ejabberd_auth compatibility layer - not supported features
-behaviour(ejabberd_gen_auth).
-export([set_password/3,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         does_user_exist/2,
         remove_user/2,
         remove_user/3,
         supports_sasl_module/2,
         scram_passwords/0]).

-include("mongoose.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(Domain :: ejabberd:server()) -> ok.
start(_Domain) ->
    ok.

-spec stop(Domain:: ejabberd:server()) -> ok.
stop(_Domain) ->
    ok.

authorize(Creds) ->
    timer:sleep(50 + rand:uniform(450)),
    {ok, mongoose_credentials:set(Creds, auth_module, ?MODULE)}.

plain_password_required() ->
    true.

check_password(_User, _Server, _Password) ->
    true.

check_password(_User, _Server, _Password, _Digest, _DigestGen) ->
    ?DEBUG("no support for digest authentication", []),
    false.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(_User, _Server, _Password) ->
    ok.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
try_register(_User, _Server, _Password) ->
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

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
%% @doc Remove user if the provided password is correct.
remove_user(_User, _Server, _Password) ->
    %% in fact not_supported
    ok.

supports_sasl_module(_, cyrsasl_plain) -> true;
supports_sasl_module(_, _) -> false.

scram_passwords() ->
    not_supported.
