-module(auth_external_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HOST_TYPE, <<"test host type">>).

all() ->
    [{group, no_cache}].

groups() ->
    [{no_cache, [], all_tests()}].

all_tests() ->
    [try_register_ok,
     remove_user_ok,
     set_password_ok,
     does_user_exist,
     get_password_returns_false_if_no_cache,
     get_password_s_returns_empty_bin_if_no_cache,
     supported_sasl_mechanisms
    ].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(jid),
    C.

end_per_suite(C) ->
    C.

init_per_group(_G, Config) ->
    set_opts(Config),
    ejabberd_auth_external:start(host_type()),
    Config.

end_per_group(_G, Config) ->
    ejabberd_auth_external:stop(host_type()),
    unset_opts(),
    Config.

try_register_ok(_C) ->
    {U, P} = given_user_registered(),
    true = ejabberd_auth_external:check_password(host_type(), U, domain(), P).

remove_user_ok(_C) ->
    {U, P} = given_user_registered(),
    ok = ejabberd_auth_external:remove_user(host_type(), U, domain()),
    false = ejabberd_auth_external:check_password(host_type(), U, domain(), P).

set_password_ok(_C) ->
    {U, P} = given_user_registered(),
    NewP = random_binary(7),
    ok = ejabberd_auth_external:set_password(host_type(), U, domain(), NewP),
    false = ejabberd_auth_external:check_password(host_type(), U, domain(), P),
    true = ejabberd_auth_external:check_password(host_type(), U, domain(), NewP).

does_user_exist(_C) ->
    {U, _P} = given_user_registered(),
    true = ejabberd_auth_external:does_user_exist(host_type(), U, domain()).

get_password_returns_false_if_no_cache(_C) ->
    false = ejabberd_auth_external:get_password(host_type(), random_binary(8), domain()).

get_password_s_returns_empty_bin_if_no_cache(_C) ->
    <<"">> = ejabberd_auth_external:get_password_s(host_type(), random_binary(8), domain()).

supported_sasl_mechanisms(_C) ->
    Modules = [cyrsasl_plain, cyrsasl_digest, cyrsasl_external,
               cyrsasl_scram_sha1, cyrsasl_scram_sha224, cyrsasl_scram_sha256,
               cyrsasl_scram_sha384, cyrsasl_scram_sha512],
    [true, false, false, false, false, false, false, false] =
        [ejabberd_auth_external:supports_sasl_module(domain(), Mod) || Mod <- Modules].

given_user_registered() ->
    {U, P} = UP = gen_user(),
    ok = ejabberd_auth_external:try_register(host_type(), U, domain(), P),
    UP.

set_opts(Config) ->
    DataDir = ?config(data_dir, Config),
    mongoose_config:set_opts(#{{auth, ?HOST_TYPE} =>
                                   #{external => #{program => DataDir ++ "sample_external_auth.py",
                                                   instances => 1}}}).

unset_opts() ->
    mongoose_config:erase_opts().

gen_user() ->
    U = random_binary(5),
    P = random_binary(6),
    {U, P}.

random_binary(S) ->
    base16:encode(crypto:strong_rand_bytes(S)).

domain() ->
    <<"mim1.esl.com">>.

host_type() ->
    ?HOST_TYPE.
