%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc demo PKI auth backend.
%%%
%%% it authorises all the certificates with Common Name (used as client's
%%% "username"), assuming that all of the certificates are valid.
%%%
%%% certificate verification can be configured for c2s listener.
%%%
%%% as we cannot track properly the list of valid user, does_user_exist/2
%%% function is stubbed to true (this one is used by MAM)
%%% @end
%%%=============================================================================
-module(ejabberd_auth_pki).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-include("jlib.hrl").

-behaviour(ejabberd_gen_auth).

%% ejabberd_gen_auth API
-export([start/1,
         stop/1,
         store_type/1,
         set_password/3,
         authorize/1,
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
         remove_user/3
]).

-export([check_password/3,
         check_password/5]).

%%-callback start(Host :: ejabberd:lserver()) -> ok.
start(_) -> ok.

%%-callback stop(Host :: ejabberd:lserver()) -> ok.
stop(_) -> ok.

%%-callback store_type(Host :: ejabberd:lserver()) -> scram | plain | external.
store_type(_) -> scram.

%%-callback set_password( User :: ejabberd:luser(),
%%                        Server :: ejabberd:lserver(),
%%                        Password :: binary()
%%                      ) -> ok | {error, not_allowed | invalid_jid}.
set_password(_, _, _) -> {error, not_allowed}.

%%-callback authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()} | {error, any()}.
authorize(Creds) ->
    Credentials = { get_credentials(Creds, requested_name),
                    get_credentials(Creds, xmpp_addresses),
                    get_credentials(Creds, common_name)},
    case do_authorize(Credentials) of
        {error, <<"not-authorized">>} ->
            {error, <<"not-authorized">>};
        UserName ->
            {ok, mongoose_credentials:extend(Creds, [{username, UserName},
                                                     {auth_module, ?MODULE}])}
    end.

do_authorize({undefined, [OneXmppAddr], _ }) ->
    %%  name from jid ?
    OneXmppAddr;
do_authorize({undefined, [], CommonName}) ->
    %% check if CommonName is correct jid
    CommonName;
do_authorize({RequestedName, [], CommonName}) ->
    %% check if RequestedName matches ComonName
    RequestedName;
do_authorize({RequestedName, XmppAddrList, _}) ->
    %% check if RequestedName matches any of XmppAddrList
    case lists:filter(fun(XmppAddr) -> XmppAddr == RequestedName end, XmppAddrList) of
        [OneAddr] ->
            get_username(OneAddr);
        _ ->
            {error, <<"not-authorized">>}
    end;
do_authorize(_) ->
    {error, <<"not-authorized">>}.


%%-callback try_register( User :: ejabberd:luser(),
%%                        Server :: ejabberd:lserver(),
%%                        Password :: binary()
%%                      ) -> ok | {error, exists | not_allowed | term()}.
try_register(_, _, _) -> {error, not_allowed}.

%%-callback dirty_get_registered_users() -> [ejabberd:simple_bare_jid()].
dirty_get_registered_users() -> [].

%%-callback get_vh_registered_users(Server :: ejabberd:lserver()) -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(_) -> [].

%%-callback get_vh_registered_users( Server :: ejabberd:lserver(),
%%                                   Opts :: list()
%%                                 ) -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(_, _) -> [].

%%-callback get_vh_registered_users_number(Server :: ejabberd:lserver()) -> integer().
get_vh_registered_users_number(_) -> 0.

%%-callback get_vh_registered_users_number( Server :: ejabberd:lserver(),
%%                                          Opts :: list()
%%                                        ) -> integer().
get_vh_registered_users_number(_, _) -> 0.

%%-callback get_password( User :: ejabberd:luser(),
%%                        Server :: ejabberd:lserver()
%%                      ) -> ejabberd_auth:passterm() | false.
get_password(_, _) -> false.

%%-callback get_password_s( User :: ejabberd:luser(),
%%                          Server :: ejabberd:lserver()
%%                        ) -> binary().
get_password_s(_, _) -> <<"">>.

%%-callback does_user_exist( User :: ejabberd:luser(),
%%                           Server :: ejabberd:lserver()
%%                         ) -> boolean() | {error, atom()}.
does_user_exist(_, _) -> true.

%%-callback remove_user( User :: ejabberd:luser(),
%%                       Server :: ejabberd:lserver()
%%                     ) -> ok | {error, not_allowed}.
remove_user(_, _) -> {error, not_allowed}.

%%-callback remove_user( User :: ejabberd:luser(),
%%                       Server :: ejabberd:lserver(),
%%                       Password :: binary()
%%                     ) -> ok | {error, not_exists | not_allowed | bad_request}.
remove_user(_, _, _) -> {error, not_allowed}.

check_password(_, _, _) -> false.

check_password(_, _, _, _, _) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_credentials(Cred, Key) ->
    mongoose_credentials:get(Cred, Key, undefined).

get_username(Jid) ->
    JidRecord = jid:binary_to_bare(Jid),
    JidRecord#jid.user.
