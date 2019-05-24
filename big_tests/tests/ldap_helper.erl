-module(ldap_helper).

-behaviour(escalus_user_db).

%% API
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2,
         get_ldap_base/1,
         call_ldap/3]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

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
    UserStr=binary_to_list(User),
    Base = get_ldap_base(Server),
    DN = "cn=" ++ UserStr ++ "," ++ binary_to_list(Base),
    Attrs = [{"objectclass", ["inetOrgPerson"]},
             {"cn", [UserStr]},
             {"sn", [UserStr]},
             {"userPassword", [binary_to_list(Password)]},
             {"ou", ["shared_group"]},
             {"uid", [UserStr]}],
    call_ldap(Server, add, [DN, Attrs]).

delete_user({_Name, Spec}) ->
    {User, Server, _Password} = get_usp(Spec),
    rpc(mim(), ejabberd_auth_ldap, remove_user, [User, Server]).

get_usp(Spec) ->
    Username = proplists:get_value(username, Spec),
    Server = proplists:get_value(server, Spec),
    Password = proplists:get_value(password, Spec),
    {Username, Server, Password}.

get_ldap_base(Server) ->
    list_to_binary(rpc(mim(), gen_mod, get_module_opt, [Server, mod_vcard, ldap_base, ""])).

call_ldap(Server, F, Args) ->
    rpc(mim(), mongoose_wpool, call, [ldap, Server, default, {F, Args}]).
