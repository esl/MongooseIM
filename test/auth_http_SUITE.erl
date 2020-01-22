%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(auth_http_SUITE).
-compile(export_all).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").

-define(DOMAIN1, <<"localhost">>).
-define(DOMAIN2, <<"localhost2">>).
-define(AUTH_HOST, "http://localhost:12000").
-define(BASIC_AUTH, "softkitty:purrpurrpurr").
-define(CERT_PATH, "../../../../tools/ssl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, auth_requests_plain}, {group, auth_requests_scram}].

groups() ->
    [
     {cert_auth, cert_auth()},
     {auth_requests_plain, [sequence], all_tests()},
     {auth_requests_scram, [sequence], [{group,cert_auth} | all_tests()]}
    ].

all_tests() ->
    [
     check_password,
     set_password,
     try_register,
     get_password,
     is_user_exists,
     remove_user,
     supported_sasl_mechanisms
    ].

cert_auth() ->
    [
        cert_auth_fail,
        cert_auth_success,
        cert_auth_nonexistent
    ].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = stringprep:start(),
    meck_config(Config),
    mim_ct_rest:start(?BASIC_AUTH, Config),
    % Separate process needs to do this, because this one will terminate
    % so will supervisor and children and ETS tables
    mim_ct_rest:do(
      fun() ->
              mim_ct_sup:start_link(ejabberd_sup),
              mongoose_wpool:ensure_started(),
              % This would be started via outgoing_pools in normal case
              Pool = {http, host, auth,
                      [{strategy, random_worker}, {call_timeout, 5000}, {workers, 20}],
                      [{path_prefix, "/auth/"}, {http_opts, []}, {server, ?AUTH_HOST}]},
              Hosts = [?DOMAIN1, ?DOMAIN2],
              mongoose_wpool:start_configured_pools([Pool], Hosts),
              mongoose_wpool_http:init(),
              ejabberd_auth_http:start(?DOMAIN1),
              ejabberd_auth_http:start(?DOMAIN2)
      end),
    meck_cleanup(),
    Config.

end_per_suite(Config) ->
    ejabberd_auth_http:stop(?DOMAIN1),
    ejabberd_auth_http:stop(?DOMAIN2),
    ok = mim_ct_rest:stop(),
    Config.

init_per_group(cert_auth, Config) ->
    try
        {ok, Cert1} = file:read_file(?CERT_PATH ++ "/mongooseim/cert.pem"),
        {ok, Cert2} = file:read_file(?CERT_PATH ++ "/ca/cacert.pem"),
        [{'Certificate', DerBin, not_encrypted} | _] = public_key:pem_decode(Cert2),
        [{der_cert, DerBin}, {pem_cert1, Cert1}, {pem_cert2, Cert2} | Config]
    catch
        _:E ->
            {skip, {E, ?CERT_PATH, element(2, file:get_cwd())}}
    end;
init_per_group(GroupName, Config) ->
    Config2 = lists:keystore(scram_group, 1, Config,
                             {scram_group, GroupName == auth_requests_scram}),
    meck_config(Config2),
    mim_ct_rest:register(<<"alice">>, ?DOMAIN1, do_scram(<<"makota">>, Config2)),
    mim_ct_rest:register(<<"bob">>, ?DOMAIN1, do_scram(<<"niema5klepki">>, Config2)),
    meck_cleanup(),
    Config2.

end_per_group(cert_auth, Config) ->
    Config;
end_per_group(_GroupName, Config) ->
    mim_ct_rest:remove_user(<<"alice">>, ?DOMAIN1),
    mim_ct_rest:remove_user(<<"bob">>, ?DOMAIN1),
    Config.

init_per_testcase(remove_user, Config) ->
    meck_config(Config),
    mim_ct_rest:register(<<"toremove1">>, ?DOMAIN1, do_scram(<<"pass">>, Config)),
    mim_ct_rest:register(<<"toremove2">>, ?DOMAIN1, do_scram(<<"pass">>, Config)),
    Config;
init_per_testcase(cert_auth_fail, Config) ->
    meck_config(Config),
    Cert = proplists:get_value(pem_cert1, Config),
    mim_ct_rest:register(<<"cert_user">>, ?DOMAIN1, Cert),
    Config;
init_per_testcase(cert_auth_success, Config) ->
    meck_config(Config),
    Cert1 = proplists:get_value(pem_cert1, Config),
    Cert2 = proplists:get_value(pem_cert2, Config),
    SeveralCerts = <<Cert1/bitstring, Cert2/bitstring>>,
    mim_ct_rest:register(<<"cert_user">>, ?DOMAIN1, SeveralCerts),
    Config;
init_per_testcase(_CaseName, Config) ->
    meck_config(Config),
    Config.

end_per_testcase(try_register, Config) ->
    mim_ct_rest:remove_user(<<"nonexistent">>, ?DOMAIN1),
    meck_cleanup(),
    Config;
end_per_testcase(remove_user, Config) ->
    mim_ct_rest:remove_user(<<"toremove1">>, ?DOMAIN1),
    mim_ct_rest:remove_user(<<"toremove2">>, ?DOMAIN1),
    meck_cleanup(),
    Config;
end_per_testcase(cert_auth_fail, Config) ->
    mim_ct_rest:remove_user(<<"cert_user">>, ?DOMAIN1),
    meck_cleanup(),
    Config;
end_per_testcase(cert_auth_success, Config) ->
    mim_ct_rest:remove_user(<<"cert_user">>, ?DOMAIN1),
    meck_cleanup(),
    Config;
end_per_testcase(_CaseName, Config) ->
    meck_cleanup(),
    Config.

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

check_password(_Config) ->
    true = ejabberd_auth_http:check_password(<<"alice">>, ?DOMAIN1, <<"makota">>),
    false = ejabberd_auth_http:check_password(<<"alice">>, ?DOMAIN1, <<"niemakota">>),
    false = ejabberd_auth_http:check_password(<<"kate">>, ?DOMAIN1, <<"mapsa">>).

set_password(_Config) ->
    ok = ejabberd_auth_http:set_password(<<"alice">>, ?DOMAIN1, <<"mialakota">>),
    true = ejabberd_auth_http:check_password(<<"alice">>, ?DOMAIN1, <<"mialakota">>),
    ok = ejabberd_auth_http:set_password(<<"alice">>, ?DOMAIN1, <<"makota">>).

try_register(_Config) ->
    ok = ejabberd_auth_http:try_register(<<"nonexistent">>, ?DOMAIN1, <<"newpass">>),
    true = ejabberd_auth_http:check_password(<<"nonexistent">>, ?DOMAIN1, <<"newpass">>),
    {error, exists} = ejabberd_auth_http:try_register(<<"nonexistent">>, ?DOMAIN1, <<"anypass">>).

% get_password + get_password_s
get_password(_Config) ->
    case mongoose_scram:enabled(?DOMAIN1) of
        false ->
            <<"makota">> = ejabberd_auth_http:get_password(<<"alice">>, ?DOMAIN1),
            <<"makota">> = ejabberd_auth_http:get_password_s(<<"alice">>, ?DOMAIN1);
        true ->
            % tuple with SCRAM data
            {_, _, _, _} = ejabberd_auth_http:get_password(<<"alice">>, ?DOMAIN1),
            <<>> = ejabberd_auth_http:get_password_s(<<"alice">>, ?DOMAIN1)
    end,
    false = ejabberd_auth_http:get_password(<<"anakin">>, ?DOMAIN1),
    <<>> = ejabberd_auth_http:get_password_s(<<"anakin">>, ?DOMAIN1).

is_user_exists(_Config) ->
    true = ejabberd_auth_http:does_user_exist(<<"alice">>, ?DOMAIN1),
    false = ejabberd_auth_http:does_user_exist(<<"madhatter">>, ?DOMAIN1).

% remove_user/2,3
remove_user(_Config) ->
    true = ejabberd_auth_http:does_user_exist(<<"toremove1">>, ?DOMAIN1),
    ok = ejabberd_auth_http:remove_user(<<"toremove1">>, ?DOMAIN1),
    false = ejabberd_auth_http:does_user_exist(<<"toremove1">>, ?DOMAIN1),

    true = ejabberd_auth_http:does_user_exist(<<"toremove2">>, ?DOMAIN1),
    {error, not_allowed} = ejabberd_auth_http:remove_user(<<"toremove2">>, ?DOMAIN1, <<"wrongpass">>),
    true = ejabberd_auth_http:does_user_exist(<<"toremove2">>, ?DOMAIN1),
    ok = ejabberd_auth_http:remove_user(<<"toremove2">>, ?DOMAIN1, <<"pass">>),
    false = ejabberd_auth_http:does_user_exist(<<"toremove2">>, ?DOMAIN1),

    {error, not_exists} = ejabberd_auth_http:remove_user(<<"toremove3">>, ?DOMAIN1, <<"wrongpass">>).

supported_sasl_mechanisms(Config) ->
    Modules = [cyrsasl_plain, cyrsasl_digest, cyrsasl_scram, cyrsasl_external],
    DigestSupported = case lists:keyfind(scram_group, 1, Config) of
                          {_, true} -> false;
                          _ -> true
                      end,
    [true, DigestSupported, true, false] =
        [ejabberd_auth_http:supports_sasl_module(?DOMAIN1, Mod) || Mod <- Modules].

cert_auth_fail(Config) ->
    Creds = creds_with_cert(Config, <<"cert_user">>),
    {error, not_authorized} = ejabberd_auth_http:authorize(Creds).

cert_auth_success(Config) ->
    Creds = creds_with_cert(Config, <<"cert_user">>),
    {ok, _} = ejabberd_auth_http:authorize(Creds).

cert_auth_nonexistent(Config) ->
    Creds = creds_with_cert(Config, <<"nonexistent">>),
    {error, not_authorized} = ejabberd_auth_http:authorize(Creds).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
creds_with_cert(Config, Username) ->
    Cert = proplists:get_value(der_cert, Config),
    NewCreds = mongoose_credentials:new(?DOMAIN1),
    mongoose_credentials:extend(NewCreds, [{der_cert, Cert},
                                           {username, Username}]).

meck_config(Config) ->
    meck:unload(),
    ScramOpts = case lists:keyfind(scram_group, 1, Config) of
                    {_, true} -> [{password_format, scram}];
                    _ -> []
                end,
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
                fun(auth_opts, _Host) ->
                        [
                         {host, ?AUTH_HOST},
                         {path_prefix, "/auth/"},
                         {basic_auth, ?BASIC_AUTH}
                        ] ++ ScramOpts
                end).

meck_cleanup() ->
    meck:validate(ejabberd_config),
    meck:unload(ejabberd_config).

do_scram(Pass, Config) ->
    case lists:keyfind(scram_group, 1, Config) of
        {_, true} ->
            mongoose_scram:serialize(mongoose_scram:password_to_scram(Pass, mongoose_scram:iterations(?DOMAIN1)));
        _ ->
            Pass
    end.
