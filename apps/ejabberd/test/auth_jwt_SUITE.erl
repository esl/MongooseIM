-module(auth_jwt_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(DOMAIN1, <<"localhost">>).
-define(USERNAME, <<"10857839">>).
-define(JWT_KEY, <<"testtesttest">>).
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, all}].

groups() ->
    [
     {all, [parallel], all_tests()}
    ].

all_tests() ->
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

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = stringprep:start(),
    ok = ejabberd_auth_jwt:start(?DOMAIN1),
    case list_to_integer(erlang:system_info(otp_release)) of
        Release when Release < 18 ->
            {skip, "jwerl need recent crypto features"};
        _ ->
            Config
    end.

end_per_suite(Config) ->
    ok = ejabberd_auth_jwt:stop(?DOMAIN1),
    Config.

init_per_group(_, Config) ->
    meck_config(Config),
    Config.

end_per_group(_, Config) ->
    meck_cleanup(),
    Config.


init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

check_password_succeeds_for_correct_token(_Config) ->
    true = ejabberd_auth_jwt:check_password(?USERNAME, ?DOMAIN1, generate_token(0)).

check_password_fails_for_wrong_token(_C) ->
    false = ejabberd_auth_jwt:check_password(?USERNAME, ?DOMAIN1, generate_token(60)).

check_password_fails_for_correct_token_but_wrong_username(_C) ->
    false = ejabberd_auth_jwt:check_password(<<"alice">>, ?DOMAIN1, generate_token(0)).

authorize(_C) ->
    Creds0 = mongoose_credentials:new(?DOMAIN1),
    Creds = mongoose_credentials:extend(Creds0, [{username, ?USERNAME},
                                                 {password, generate_token(0)},
                                                 {digest, fake},
                                                 {digest_gen, fun(A) -> A end}]),
    {ok, Creds2} = ejabberd_auth_jwt:authorize(Creds),
    ejabberd_auth_jwt = mongoose_credentials:get(Creds2, auth_module).

set_password(_Config) ->
    {error, not_allowed} = ejabberd_auth_jwt:set_password(<<"alice">>, ?DOMAIN1, <<"mialakota">>).

try_register(_Config) ->
    {error, not_allowed} = ejabberd_auth_jwt:try_register(<<"nonexistent">>,
                                                          ?DOMAIN1, <<"newpass">>).

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

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

meck_config(_Config) ->
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_local_option,
                fun(auth_opts, _Host) ->
                        [
                         {jwt_key, ?JWT_KEY},
                         {token_user_key, bookingNumber}
                        ]
                end).

meck_cleanup() ->
    meck:validate(ejabberd_config),
    meck:unload(ejabberd_config).

generate_token(NbfDelta) ->
    Now = p1_time_compat:system_time(seconds),
    Data = #{bookingNumber => ?USERNAME,
             exp => Now + 60,
             nbf => Now + NbfDelta,
             iat => Now},
    jwerl:sign(Data, #{alg => <<"HS256">>, key => ?JWT_KEY}).
