-module(auth_external_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

-define(AUTH_MOD, ejabberd_auth_external).

all() ->
    [{group, no_cache}].

groups() ->
    [{no_cache, [], all_tests()}].

all_tests() ->
    [try_register_ok,
     remove_user_ok,
     remove_user_with_pass_ok,
     set_password_ok,
     does_user_exist,
     get_password_returns_false_if_no_cache,
     get_password_s_returns_empty_bin_if_no_cache,
     store_type_external
    ].

init_per_suite(C) ->
    ok = stringprep:start(),
    C.

end_per_suite(C) ->
    C.

init_per_group(G, Config) ->
    setup_meck(G, Config),
    ejabberd_auth_external:start(domain()),
    Config.

end_per_group(G, Config) ->
    ejabberd_auth_external:stop(domain()),
    unload_meck(G),
    Config.

try_register_ok(_C) ->
    {U, P} = given_user_registered(),
    true = ?AUTH_MOD:check_password(U, domain(), P).

remove_user_ok(_C) ->
    {U, P} = given_user_registered(),
    ok = ?AUTH_MOD:remove_user(U, domain()),
    false = ?AUTH_MOD:check_password(U, domain(), P).

remove_user_with_pass_ok(_C) ->
    {U, P} = given_user_registered(),
    ok = ?AUTH_MOD:remove_user(U, domain(), P),
    false = ?AUTH_MOD:check_password(U, domain(), P).

set_password_ok(_C) ->
    {U, P} = given_user_registered(),
    NewP = random_binary(7),
    ok = ?AUTH_MOD:set_password(U, domain(), NewP),
    false = ?AUTH_MOD:check_password(U, domain(), P),
    true = ?AUTH_MOD:check_password(U, domain(), NewP).

does_user_exist(_C) ->
    {U, _P} = given_user_registered(),
    true = ?AUTH_MOD:does_user_exist(U, domain()).

get_password_returns_false_if_no_cache(_C) ->
    false = ?AUTH_MOD:get_password(random_binary(8), domain()).

get_password_s_returns_empty_bin_if_no_cache(_C) ->
    <<"">> = ?AUTH_MOD:get_password_s(random_binary(8), domain()).

store_type_external(_C) ->
    external = ?AUTH_MOD:store_type(domain()).

given_user_registered() ->
    {U, P} = UP = gen_user(),
    ok = ?AUTH_MOD:try_register(U, domain(), P),
    UP.


domain() ->
    <<"mim1.esl.com">>.


setup_meck(_G, Config) ->
    DataDir = ?config(data_dir, Config),
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_local_option,
                fun(auth_opts, _Host) ->
                        [{extauth_program, DataDir ++ "sample_external_auth.py"}]
                end),
    meck:expect(ejabberd_config, get_local_option,
                fun({extauth_instances, _Host}) -> undefined;
                   ({extauth_cache, _Host}) -> undefined
                end).



unload_meck(_G) ->
    meck:unload(ejabberd_config).

gen_user() ->
    U = random_binary(5),
    P = random_binary(6),
    {U, P}.

random_binary(S) ->
    base16:encode(crypto:rand_bytes(S)).

