-module(auth_jwt_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(DOMAIN, <<"localhost">>).
-define(HOST_TYPE, <<"test host type">>).
-define(USERNAME, <<"10857839">>).
-define(WRONG_USERNAME, <<"alice">>).
-define(JWT_KEY, <<"testtesttest">>).
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, generic}, {group, public_key}].

groups() ->
    [
     {generic, [parallel], generic_tests()},
     {public_key, [], public_key_tests()}
    ].

generic_tests() ->
    [
     check_password_succeeds_for_correct_token,
     check_password_fails_for_wrong_token,
     check_password_fails_for_correct_token_but_wrong_username,
     authorize,
     does_user_exist,
     supported_sasl_mechanisms
    ].

public_key_tests() ->
    [
     check_password_succeeds_for_pubkey_signed_token
    ].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(jid),
    meck:new(ejabberd_auth, [no_link, passthrough]),
    meck:expect(ejabberd_auth, auth_modules_for_host_type,
                fun(_) -> [] end),
    Config.

end_per_suite(Config) ->
    unset_auth_opts(),
    meck:unload(ejabberd_auth),
    Config.

init_per_group(public_key, Config) ->
    Root = small_path_helper:repo_dir(Config),
    PrivkeyPath = filename:join([Root, "tools", "ssl", "mongooseim", "privkey.pem"]),
    PubkeyPath = filename:join([Root, "tools", "ssl", "mongooseim", "pubkey.pem"]),
    {ok, PrivKey} = file:read_file(PrivkeyPath),
    set_auth_opts({file, PubkeyPath}, "RS256", bookingNumber),
    ok = ejabberd_auth_jwt:start(?HOST_TYPE),
    [{priv_key, PrivKey} | Config];
init_per_group(_, Config) ->
    set_auth_opts({value, ?JWT_KEY}, "HS256", bookingNumber),
    ok = ejabberd_auth_jwt:start(?HOST_TYPE),
    Config.

end_per_group(_, Config) ->
    ok = ejabberd_auth_jwt:stop(?HOST_TYPE),
    Config.


init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

check_password_succeeds_for_correct_token(_Config) ->
    true = ejabberd_auth_jwt:check_password(?HOST_TYPE, ?USERNAME, ?DOMAIN,
                                            generate_token(hs256, 0, ?JWT_KEY)).

check_password_fails_for_wrong_token(_C) ->
    false = ejabberd_auth_jwt:check_password(?HOST_TYPE, ?USERNAME, ?DOMAIN,
                                             generate_token(hs256, 60, ?JWT_KEY)).

check_password_fails_for_correct_token_but_wrong_username(_C) ->
    false = ejabberd_auth_jwt:check_password(?HOST_TYPE, ?WRONG_USERNAME, ?DOMAIN,
                                             generate_token(hs256, 0, ?JWT_KEY)).

authorize(_C) ->
    Creds0 = mongoose_credentials:new(?DOMAIN, ?HOST_TYPE, #{}),
    Creds = mongoose_credentials:extend(Creds0, [{username, ?USERNAME},
                                                 {password, generate_token(hs256, 0, ?JWT_KEY)},
                                                 {digest, fake},
                                                 {digest_gen, fun(A) -> A end}]),
    {ok, Creds2} = ejabberd_auth_jwt:authorize(Creds),
    ejabberd_auth_jwt = mongoose_credentials:get(Creds2, auth_module).

does_user_exist(_Config) ->
    true = ejabberd_auth_jwt:does_user_exist(?HOST_TYPE, <<"madhatter">>, ?DOMAIN).

supported_sasl_mechanisms(_C) ->
    Modules = [cyrsasl_plain, cyrsasl_digest, cyrsasl_external,
               cyrsasl_scram_sha1, cyrsasl_scram_sha224, cyrsasl_scram_sha256,
               cyrsasl_scram_sha384, cyrsasl_scram_sha512],
    [true, false, false, false, false, false, false, false] =
        [ejabberd_auth_jwt:supports_sasl_module(?DOMAIN, Mod) || Mod <- Modules].

check_password_succeeds_for_pubkey_signed_token(C) ->
    Key = proplists:get_value(priv_key, C),
    true = ejabberd_auth_jwt:check_password(?HOST_TYPE, ?USERNAME, ?DOMAIN,
                                            generate_token(rs256, 0, Key)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

set_auth_opts(Secret, Algorithm, Key) ->
    mongoose_config:set_opts(#{{auth, ?HOST_TYPE} => #{jwt => #{secret => Secret,
                                                                algorithm => Algorithm,
                                                                username_key => Key}}}).

unset_auth_opts() ->
    mongoose_config:erase_opts().

generate_token(Alg, NbfDelta, Key) ->
    Now = erlang:system_time(second),
    Data = #{bookingNumber => ?USERNAME,
             exp => Now + 60,
             nbf => Now + NbfDelta,
             iat => Now},
    jwerl:sign(Data, Alg, Key).
