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
    Self = self(),
    ETSProcess = spawn(fun() -> ets_owner(Self) end),
    wait_for_ets(),
    meck_config(Config),
    meck:new(ejabberd_auth, [no_link, passthrough]),
    meck:expect(ejabberd_auth, auth_modules_for_host_type,
                fun(_) -> [] end),
    [{ets_process, ETSProcess} | Config].

end_per_suite(Config) ->
    meck_cleanup(),
    meck:unload(ejabberd_auth),
    stop_ets(proplists:get_value(ets_process, Config)),
    Config.

init_per_group(public_key, Config) ->
    Root = small_path_helper:repo_dir(Config),
    PrivkeyPath = filename:join([Root, "tools", "ssl", "mongooseim", "privkey.pem"]),
    PubkeyPath = filename:join([Root, "tools", "ssl", "mongooseim", "pubkey.pem"]),
    {ok, PrivKey} = file:read_file(PrivkeyPath),
    set_auth_opts(PubkeyPath, undefined, "RS256", bookingNumber),
    ok = ejabberd_auth_jwt:start(?HOST_TYPE),
    [{priv_key, PrivKey} | Config];
init_per_group(_, Config) ->
    set_auth_opts(undefined, ?JWT_KEY, "HS256", bookingNumber),
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
    Creds0 = mongoose_credentials:new(?DOMAIN, ?HOST_TYPE),
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

ets_owner(Parent) ->
    ets:new(jwt_meck, [public, named_table, set]),
    Parent ! ets_ready,
    receive stop -> ok end.

wait_for_ets() ->
    receive
        ets_ready -> ok
    after
        5000 -> ct:fail(cant_prepare_ets)
    end.

stop_ets(undefined) -> ok;
stop_ets(Pid) -> Pid ! stop.

set_auth_opts(SecretSource, Secret, Algorithm, Key) ->
    ets:insert(jwt_meck, {auth_opts, [
                                      {jwt_secret_source, SecretSource},
                                      {jwt_secret, Secret},
                                      {jwt_algorithm, Algorithm},
                                      {jwt_username_key, Key}
                                     ]}).

meck_config(_Config) ->
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_local_option,
                fun(Key, _Host) -> [{_, Data}] = ets:lookup(jwt_meck, Key), Data end),
    meck:expect(ejabberd_config, add_local_option,
                fun({Key, _}, Val) -> ets:insert(jwt_meck, {Key, Val}) end).

meck_cleanup() ->
    meck:validate(ejabberd_config),
    meck:unload(ejabberd_config),
    ets:delete(jwt_meck).

generate_token(Alg, NbfDelta, Key) ->
    Now = erlang:system_time(second),
    Data = #{bookingNumber => ?USERNAME,
             exp => Now + 60,
             nbf => Now + NbfDelta,
             iat => Now},
    jwerl:sign(Data, Alg, Key).
