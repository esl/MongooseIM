-module(auth_jwt_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/PKCS-FRAME.hrl").

-define(DOMAIN1, <<"localhost">>).
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
     set_password,
     try_register,
     get_password,
     is_user_exists,
     remove_user,
     get_vh_registered_users_number,
     get_vh_registered_users,
     store_type,
     dirty_get_registered_users
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
    application:ensure_all_started(stringprep),
    Self = self(),
    ETSProcess = spawn(fun() -> ets_owner(Self) end),
    wait_for_ets(),
    meck_config(Config),
    [{ets_process, ETSProcess} | Config].

end_per_suite(Config) ->
    meck_cleanup(),
    stop_ets(proplists:get_value(ets_process, Config)),
    Config.

init_per_group(public_key, Config) ->
    Root = filename:join(["..", "..", "..", ".."]),
    PrivkeyPath = filename:join([Root, "tools", "ssl", "fake_privkey.pem"]),
    PubkeyPath = filename:join([Root, "tools", "ssl", "fake_pubkey.pem"]),
    {ok, PrivKey} = file:read_file(PrivkeyPath),
    set_auth_opts(PubkeyPath, undefined, "RS256", bookingNumber),
    ok = ejabberd_auth_jwt:start(?DOMAIN1),
    [{priv_key, PrivKey} | Config];
init_per_group(_, Config) ->
    set_auth_opts(undefined, ?JWT_KEY, "HS256", bookingNumber),
    ok = ejabberd_auth_jwt:start(?DOMAIN1),
    Config.

end_per_group(_, Config) ->
    ok = ejabberd_auth_jwt:stop(?DOMAIN1),
    Config.


init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

check_password_succeeds_for_correct_token(_Config) ->
    true = ejabberd_auth_jwt:check_password(?USERNAME, ?DOMAIN1,
                                            generate_token(hs256, 0, ?JWT_KEY)).

check_password_fails_for_wrong_token(_C) ->
    false = ejabberd_auth_jwt:check_password(?USERNAME, ?DOMAIN1,
                                             generate_token(hs256, 60, ?JWT_KEY)).

check_password_fails_for_correct_token_but_wrong_username(_C) ->
    false = ejabberd_auth_jwt:check_password(?WRONG_USERNAME, ?DOMAIN1,
                                             generate_token(hs256, 0, ?JWT_KEY)).

authorize(_C) ->
    Creds0 = mongoose_credentials:new(?DOMAIN1),
    Creds = mongoose_credentials:extend(Creds0, [{username, ?USERNAME},
                                                 {password, generate_token(hs256, 0, ?JWT_KEY)},
                                                 {digest, fake},
                                                 {digest_gen, fun(A) -> A end}]),
    {ok, Creds2} = ejabberd_auth_jwt:authorize(Creds),
    ejabberd_auth_jwt = mongoose_credentials:get(Creds2, auth_module).

set_password(_Config) ->
    {error, not_allowed} = ejabberd_auth_jwt:set_password(?USERNAME, ?DOMAIN1, <<"mialakota">>).

try_register(_Config) ->
    {error, not_allowed} = ejabberd_auth_jwt:try_register(?USERNAME, ?DOMAIN1, <<"newpass">>).

% get_password + get_password_s
get_password(_Config) ->
    false = ejabberd_auth_jwt:get_password(<<"anaking">>, ?DOMAIN1),
    <<>> = ejabberd_auth_jwt:get_password_s(<<"anakin">>, ?DOMAIN1).

is_user_exists(_Config) ->
    true = ejabberd_auth_jwt:does_user_exist(<<"madhatter">>, ?DOMAIN1).

% remove_user/2,3
remove_user(_Config) ->
    ok = ejabberd_auth_jwt:remove_user(<<"toremove3">>, ?DOMAIN1),
    {error, not_allowed} = ejabberd_auth_jwt:remove_user(<<"toremove3">>,
                                                         ?DOMAIN1, <<"wrongpass">>).

get_vh_registered_users_number(_C) ->
    0 = ejabberd_auth_jwt:get_vh_registered_users_number(?DOMAIN1, []).

get_vh_registered_users(_C) ->
    [] = ejabberd_auth_jwt:get_vh_registered_users(?DOMAIN1, []).

store_type(_C) ->
    external = ejabberd_auth_jwt:store_type(?DOMAIN1).

dirty_get_registered_users(_C) ->
    [] = ejabberd_auth_jwt:dirty_get_registered_users().

check_password_succeeds_for_pubkey_signed_token(C) ->
    Key = proplists:get_value(priv_key, C),
    true = ejabberd_auth_jwt:check_password(?USERNAME, ?DOMAIN1, generate_token(rs256, 0, Key)).

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
    Now = p1_time_compat:system_time(seconds),
    Data = #{bookingNumber => ?USERNAME,
             exp => Now + 60,
             nbf => Now + NbfDelta,
             iat => Now},
    jwerl:sign(Data, #{alg => alg_to_bin(Alg), key => Key}).

alg_to_bin(hs256) -> <<"HS256">>;
alg_to_bin(rs256) -> <<"RS256">>.

