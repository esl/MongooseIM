-module(ldap_helper).

-behaviour(escalus_user_db).

% copied from ejabberd_auth_ldap
-record(state, {host = <<"">>          :: ejabberd:server(),
                eldap_id = <<"">>      :: binary(),
                bind_eldap_id = <<"">> :: binary(),
                servers = []           :: [ejabberd:server()],
                backups = []           :: [binary()],
                port = 389             :: inet:port_number(),
                tls_options = []       :: list(),
                dn = <<"">>            :: binary(),
                password = <<"">>      :: binary(),
                base = <<"">>          :: binary(),
                uids = []              :: [{binary()} | {binary(), binary()}],
                ufilter = <<"">>       :: binary(),
                sfilter = <<"">>       :: binary(),
                lfilter                :: {any(), any()},
                deref_aliases = never  :: never | searching | finding | always,
                dn_filter              :: binary(),
                dn_filter_attrs = []   :: [binary()]}).

%% API
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2]).

-type user_spec() :: escalus_users:user_spec().

-spec start(any()) -> ok.
start(_) ->
    ok.

-spec stop(any()) -> ok.
stop(_) ->
    ok.

-spec create_users(escalus:config(), [user_spec()]) -> escalus:config().
create_users(Config, Users) ->
    lists:foreach(fun create_user/1, Users),
    lists:keystore(escalus_users, 1, Config, {escalus_users, Users}).

-spec delete_users(escalus:config(), [user_spec()]) -> escalus:config().
delete_users(Config, Users) ->
    lists:foreach(fun delete_user/1, Users),
    Config.

create_user({_User, Spec}) ->
    {User, Server, Password} = get_usp(Spec),
    {ok, State} = escalus_ejabberd:rpc(eldap_utils, get_state, [Server, ejabberd_auth_ldap]),
    UserStr=binary_to_list(User),
    DN = "cn=" ++ UserStr ++ "," ++ binary_to_list(State#state.base),
    Attrs = [{"objectclass", ["inetOrgPerson"]},
             {"cn", [UserStr]},
             {"sn", [UserStr]},
             {"userPassword", [binary_to_list(Password)]},
             {"ou", ["shared_group"]},
             {"uid", [UserStr]}],
    escalus_ejabberd:rpc(eldap_pool, add, [State#state.eldap_id, DN, Attrs]).

delete_user({_Name, Spec}) ->
    {User, Server, _Password} = get_usp(Spec),
    escalus_ejabberd:rpc(ejabberd_auth_ldap, remove_user, [User, Server]).

get_usp(Spec) ->
    Username = proplists:get_value(username, Spec),
    Server = proplists:get_value(server, Spec),
    Password = proplists:get_value(password, Spec),
    {Username, Server, Password}.
