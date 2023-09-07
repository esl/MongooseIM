%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(login_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-import(domain_helper, [host_type/0, domain/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, login},
     {group, login_digest},
     {group, login_scram},
     {group, login_scram_store_plain},
     {group, login_specific_scram},
     {group, login_scram_tls},
     {group, messages},
     {group, access}
    ].

groups() ->
    [{login, [parallel], all_tests()},
     {login_digest, [sequence], digest_tests()},
     {login_scram, [parallel], scram_tests()},
     {login_scram_store_plain, [parallel], scram_tests()},
     {login_scram_tls, [parallel], scram_tests()},
     {login_specific_scram, [sequence], configure_specific_scram_test()},
     {messages, [sequence], [messages_story]},
     {access, [], access_tests()}].

scram_tests() ->
    [scram_failed_with_non_authorized,
     log_one,
     log_one_scram_sha1,
     log_one_scram_sha224,
     log_one_scram_sha256,
     log_one_scram_sha384,
     log_one_scram_sha512,
     log_one_scram_sha1_plus,
     log_one_scram_sha224_plus,
     log_one_scram_sha256_plus,
     log_one_scram_sha384_plus,
     log_one_scram_sha512_plus].

configure_specific_scram_test() ->
    [configure_sha1_log_with_sha1,
     configure_sha224_log_with_sha224,
     configure_sha256_log_with_sha256,
     configure_sha384_log_with_sha384,
     configure_sha512_log_with_sha512,
     configure_sha1_log_with_sha1_plus,
     configure_sha224_log_with_sha224_plus,
     configure_sha256_log_with_sha256_plus,
     configure_sha384_log_with_sha384_plus,
     configure_sha512_log_with_sha512_plus,
     configure_sha1_fail_log_with_sha224,
     configure_sha224_fail_log_with_sha256,
     configure_sha256_fail_log_with_sha384,
     configure_sha384_fail_log_with_sha512,
     configure_sha512_fail_log_with_sha1,
     configure_sha1_plus_fail_log_with_sha1,
     configure_sha224_plus_fail_log_with_sha224,
     configure_sha256_plus_fail_log_with_sha256,
     configure_sha384_plus_fail_log_with_sha384,
     configure_sha512_plus_fail_log_with_sha512].

all_tests() ->
    [log_one,
     log_non_existent_plain,
     log_one_scram_sha1,
     log_non_existent_scram,
     log_bad_user_fails
    ].

access_tests() ->
    [blocked_user,
     access_none_blocks_all_users,
     access_none_for_other_listener_has_no_effect].

digest_tests() ->
    [log_one_digest,
     log_non_existent_digest].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(login_digest = GroupName, ConfigIn) ->
    Config = backup_and_set_options(GroupName, ConfigIn),
    case mongoose_helper:supports_sasl_module(cyrsasl_digest) of
        false ->
            mongoose_helper:restore_config(Config),
            {skip, "digest password type not supported"};
        true ->
            escalus:create_users(Config, escalus:get_users([alice, bob]))
    end;
init_per_group(GroupName, ConfigIn)
  when GroupName == login_scram;
       GroupName == login_scram_store_plain ->
    Config = backup_and_set_options(GroupName, ConfigIn),
    case are_sasl_scram_modules_supported() of
        false ->
            mongoose_helper:restore_config(Config),
            {skip, "scram password type not supported"};
        true ->
            Config2 = escalus:create_users(Config, escalus:get_users([alice, bob, neustradamus])),
            assert_password_format(GroupName, Config2)
    end;
init_per_group(login_scram_tls = GroupName, ConfigIn) ->
    Config = backup_and_set_options(GroupName, ConfigIn),
    case are_sasl_scram_modules_supported() of
        false ->
            mongoose_helper:restore_config(Config),
            {skip, "scram password type not supported"};
        true ->
            Config1 = configure_c2s_listener(Config),
            Config2 = create_tls_users(Config1),
            assert_password_format(scram, Config2)
    end;
init_per_group(login_specific_scram = GroupName, ConfigIn) ->
    Config = backup_and_set_options(GroupName, ConfigIn),
    case are_sasl_scram_modules_supported() of
        false ->
            mongoose_helper:restore_config(Config),
            {skip, "scram password type not supported"};
        true ->
            escalus:create_users(Config, escalus:get_users([alice, bob, neustradamus]))
    end;
init_per_group(GroupName, ConfigIn) ->
    Config = backup_and_set_options(GroupName, ConfigIn),
    escalus:create_users(Config, escalus:get_users([alice, bob])).

backup_and_set_options(GroupName, Config) ->
    mongoose_helper:backup_and_set_config_option(Config, {auth, host_type()}, auth_opts(GroupName)).

auth_opts(login_digest) ->
    AuthOpts = mongoose_helper:auth_opts_with_password_format(plain),
    AuthOpts#{sasl_mechanisms => [cyrsasl_digest]};
auth_opts(login_scram_store_plain) ->
    mongoose_helper:auth_opts_with_password_format(plain);
auth_opts(_GroupName) ->
    mongoose_helper:auth_opts_with_password_format(scram).

end_per_group(login_digest, Config) ->
    mongoose_helper:restore_config(Config),
    escalus:delete_users(Config, escalus:get_users([alice, bob]));
end_per_group(GroupName, Config) when
    GroupName == login_scram; GroupName == login_specific_scram ->
    mongoose_helper:restore_config(Config),
    escalus:delete_users(Config, escalus:get_users([alice, bob, neustradamus]));
end_per_group(login_scram_tls, Config) ->
    mongoose_helper:restore_config(Config),
    restore_c2s(Config),
    delete_tls_users(Config);
end_per_group(_GroupName, Config) ->
    mongoose_helper:restore_config(Config),
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) when
      CaseName =:= log_one_scram_sha1; CaseName =:= log_non_existent_scram ->
    case mongoose_helper:supports_sasl_module(cyrsasl_scram_sha1) of
        false ->
            {skip, "scram password type not supported"};
        true ->
            escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(blocked_user = CaseName, Config) ->
    [{_, Spec}] = escalus_users:get_users([alice]),
    Config1 = set_acl_for_blocking(Config, Spec),
    escalus:init_per_testcase(CaseName, Config1);
init_per_testcase(access_none_blocks_all_users = CaseName, Config) ->
    Config1 = set_access_none(ct:get_config({hosts, mim, c2s_port}), Config),
    escalus:init_per_testcase(CaseName, Config1);
init_per_testcase(access_none_for_other_listener_has_no_effect = CaseName, Config) ->
    Config1 = set_access_none(ct:get_config({hosts, mim, c2s_tls_port}), Config),
    escalus:init_per_testcase(CaseName, Config1);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(blocked_user = CaseName, Config) ->
    unset_acl_for_blocking(Config),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(access_none_blocks_all_users = CaseName, Config) ->
    restore_c2s(Config),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(access_none_for_other_listener_has_no_effect = CaseName, Config) ->
    restore_c2s(Config),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

set_access_none(C2SPort, Config) ->
    [C2SListener] =
        mongoose_helper:get_listeners(mim(), #{port => C2SPort, module => mongoose_c2s_listener}),
    mongoose_helper:restart_listener(mim(), C2SListener#{access := none}),
    [{c2s_listener, C2SListener} | Config].

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

scram_failed_with_non_authorized(Config) ->
    ConnectionSteps = [start_stream, stream_features],
    UserSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _Features} = escalus_connection:start(UserSpec, ConnectionSteps),
    Username = escalus_utils:get_username(Alice),
    BadPayload = <<"n,,n=", Username/binary, ",r=9ZdW+o71OwOrDUx4J5+M+A==">>,
    AuthStanza = auth_stanza(<<"SCRAM-SHA-1">>, BadPayload),
    escalus_client:send(Alice, AuthStanza),
    _Challenge = escalus_client:wait_for_stanza(Alice),
    WrongProof = <<"c=biws,r=invalid_nonce,p=wrong_proof">>,
    Response = auth_response(WrongProof),
    escalus_client:send(Alice, Response),
    Failure = escalus_client:wait_for_stanza(Alice),
    ?assertMatch(#xmlel{name = <<"failure">>}, Failure),
    ?assertMatch(#xmlel{}, exml_query:subelement(Failure, <<"not-authorized">>)).

log_one(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Alice))

        end).

log_one_scram_plus(Config) ->
    escalus:fresh_story(Config, [{neustradamus, 1}], fun(Neustradamus) ->

        escalus_client:send(Neustradamus, escalus_stanza:chat_to(Neustradamus, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Neustradamus))

        end).

log_one_digest(Config) ->
    log_one([{escalus_auth_method, <<"DIGEST-MD5">>} | Config]).

log_one_scram_sha1(Config) ->
    log_one([{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config]).

log_one_scram_sha224(Config) ->
    log_one([{escalus_auth_method, <<"SCRAM-SHA-224">>} | Config]).

log_one_scram_sha256(Config) ->
    log_one([{escalus_auth_method, <<"SCRAM-SHA-256">>} | Config]).

 log_one_scram_sha384(Config) ->
    log_one([{escalus_auth_method, <<"SCRAM-SHA-384">>} | Config]).

log_one_scram_sha512(Config) ->
    log_one([{escalus_auth_method, <<"SCRAM-SHA-512">>} | Config]).

log_one_scram_sha1_plus(Config) ->
    log_one_scram_plus([{escalus_auth_method, <<"SCRAM-SHA-1-PLUS">>} | Config]).

log_one_scram_sha224_plus(Config) ->
    log_one_scram_plus([{escalus_auth_method, <<"SCRAM-SHA-224-PLUS">>} | Config]).

log_one_scram_sha256_plus(Config) ->
    log_one_scram_plus([{escalus_auth_method, <<"SCRAM-SHA-256-PLUS">>} | Config]).

log_one_scram_sha384_plus(Config) ->
    log_one_scram_plus([{escalus_auth_method, <<"SCRAM-SHA-384-PLUS">>} | Config]).

log_one_scram_sha512_plus(Config) ->
    log_one_scram_plus([{escalus_auth_method, <<"SCRAM-SHA-512-PLUS">>} | Config]).

configure_sha1_log_with_sha1(Config) ->
    configure_and_log_scram(Config, sha, <<"SCRAM-SHA-1">>).

configure_sha224_log_with_sha224(Config) ->
    configure_and_log_scram(Config, sha224, <<"SCRAM-SHA-224">>).

configure_sha256_log_with_sha256(Config) ->
    configure_and_log_scram(Config, sha256, <<"SCRAM-SHA-256">>).

configure_sha384_log_with_sha384(Config) ->
    configure_and_log_scram(Config, sha384, <<"SCRAM-SHA-384">>).

configure_sha512_log_with_sha512(Config) ->
    configure_and_log_scram(Config, sha512, <<"SCRAM-SHA-512">>).

configure_sha1_log_with_sha1_plus(Config) ->
    configure_and_log_scram_plus(Config, sha, <<"SCRAM-SHA-1-PLUS">>).

configure_sha224_log_with_sha224_plus(Config) ->
    configure_and_log_scram_plus(Config, sha224, <<"SCRAM-SHA-224-PLUS">>).

configure_sha256_log_with_sha256_plus(Config) ->
    configure_and_log_scram_plus(Config, sha256, <<"SCRAM-SHA-256-PLUS">>).

configure_sha384_log_with_sha384_plus(Config) ->
    configure_and_log_scram_plus(Config, sha384, <<"SCRAM-SHA-384-PLUS">>).

configure_sha512_log_with_sha512_plus(Config) ->
    configure_and_log_scram_plus(Config, sha512, <<"SCRAM-SHA-512-PLUS">>).

configure_sha1_fail_log_with_sha224(Config) ->
    configure_and_fail_log_scram(Config, sha, <<"SCRAM-SHA-224">>).

configure_sha224_fail_log_with_sha256(Config) ->
    configure_and_fail_log_scram(Config, sha224, <<"SCRAM-SHA-256">>).

configure_sha256_fail_log_with_sha384(Config) ->
    configure_and_fail_log_scram(Config, sha256, <<"SCRAM-SHA-384">>).

configure_sha384_fail_log_with_sha512(Config) ->
    configure_and_fail_log_scram(Config, sha384, <<"SCRAM-SHA-512">>).

configure_sha512_fail_log_with_sha1(Config) ->
    configure_and_fail_log_scram(Config, sha512, <<"SCRAM-SHA-1">>).

%%
%% configure_sha*_plus_fail_log_with_sha* tests are succeeding due to the fact that
%% escalus, when configured with fast_tls and login with scram, sets channel binding
%% flag to 'y'. This indicates that escalus supports channel binding but the server
%% does not. The server did advertise the SCRAM PLUS mechanism, so this flag is
%% incorrect and could be the result of the man-in-the-middle attack attempting to
%% downgrade the authentication mechanism. Because of that, the authentication should fail.
%%
configure_sha1_plus_fail_log_with_sha1(Config) ->
    configure_scram_plus_and_fail_log_scram(Config, sha, <<"SCRAM-SHA-1">>).

configure_sha224_plus_fail_log_with_sha224(Config) ->
    configure_scram_plus_and_fail_log_scram(Config, sha224, <<"SCRAM-SHA-224">>).

configure_sha256_plus_fail_log_with_sha256(Config) ->
    configure_scram_plus_and_fail_log_scram(Config, sha256, <<"SCRAM-SHA-256">>).

configure_sha384_plus_fail_log_with_sha384(Config) ->
    configure_scram_plus_and_fail_log_scram(Config, sha384, <<"SCRAM-SHA-384">>).

configure_sha512_plus_fail_log_with_sha512(Config) ->
    configure_scram_plus_and_fail_log_scram(Config, sha512, <<"SCRAM-SHA-512">>).

log_non_existent_plain(Config) ->
    {auth_failed, _, Xmlel} = log_non_existent(Config),
    #xmlel{name = <<"failure">>} = Xmlel,
    #xmlel{} = exml_query:subelement(Xmlel, <<"not-authorized">>).

log_non_existent_digest(Config) ->
    R = log_non_existent([{escalus_auth_method, <<"DIGEST-MD5">>} | Config]),
    {expected_challenge, _, _} = R.

log_non_existent_scram(Config) ->
    R = log_non_existent([{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config]),
    {expected_challenge, _, _} = R.

log_bad_user_fails(Config) ->
    Config1 = [{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config],
    [{kate, UserSpec}] = escalus_users:get_users([kate]),
    UserSpec1 = lists:keyreplace(username, 1, UserSpec, {username, <<" kate">>}),
    {error, {connection_step_failed, _, R}} = escalus_client:start(Config1, UserSpec1, <<"res">>),
    {expected_challenge, got, Xmlel} = R,
    #xmlel{name = <<"failure">>} = Xmlel.

log_non_existent(Config) ->
    [{kate, UserSpec}] = escalus_users:get_users([kate]),
    {error, {connection_step_failed, _, R}} = escalus_client:start(Config, UserSpec, <<"res">>),
    R.

blocked_user(_Config) ->
    [{_, Spec}] = escalus_users:get_users([alice]),
    try
        {ok, _Alice, _Spec2, _Features} = escalus_connection:start(Spec),
        ct:fail("Alice authenticated but shouldn't")
    catch
        error:{assertion_failed, assert, is_iq_result, Stanza, _Bin} ->
            <<"cancel">> = exml_query:path(Stanza, [{element, <<"error">>}, {attr, <<"type">>}])
    end.

access_none_blocks_all_users(Config) ->
    blocked_user(Config).

access_none_for_other_listener_has_no_effect(Config) ->
    log_one(Config).

messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        % Alice sends a message to Bob
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),

        % Bob gets the message
        escalus_assert:is_chat_message(<<"Hi!">>, escalus_client:wait_for_stanza(Bob))

    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

configure_c2s_listener(Config) ->
    C2SPort = ct:get_config({hosts, mim, c2s_port}),
    [C2SListener = #{tls := TLSOpts}] =
        mongoose_helper:get_listeners(mim(), #{port => C2SPort, module => mongoose_c2s_listener}),
    %% replace starttls with tls
    NewTLSOpts = TLSOpts#{mode := tls},
    mongoose_helper:restart_listener(mim(), C2SListener#{tls := NewTLSOpts}),
    [{c2s_listener, C2SListener} | Config].

create_tls_users(Config) ->
   Config1 = escalus:create_users(Config, escalus:get_users([alice, neustradamus])),
   Users = proplists:get_value(escalus_users, Config1, []),
   Users1 = prepare_user_for_ssl(Users, neustradamus),
   Users2 = prepare_user_for_ssl(Users1, alice),
   lists:keystore(escalus_users, 1, Config1, {escalus_users, Users2}).

prepare_user_for_ssl(Users, User) ->
   UserSpec = proplists:get_value(User, Users),
   UserSpec1 = lists:keydelete(starttls, 1, UserSpec),
   UserSpec2 = lists:keystore(ssl, 1, UserSpec1, {ssl, true}),
   UserSpec3 = lists:keystore(ssl_opts, 1, UserSpec2, {ssl_opts, [{verify, verify_none}]}),
   lists:keystore(User, 1, Users, {User, UserSpec3}).

delete_tls_users(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, neustradamus])).

assert_password_format(GroupName, Config) ->
    Users = proplists:get_value(escalus_users, Config),
    [verify_format(GroupName, User) || User <- Users],
    Config.

verify_format(GroupName, {_User, Props}) ->
    Username = escalus_utils:jid_to_lower(proplists:get_value(username, Props)),
    Server = proplists:get_value(server, Props),
    Password = proplists:get_value(password, Props),
    JID = mongoose_helper:make_jid(Username, Server),
    {SPassword, _} = rpc(mim(), ejabberd_auth, get_passterm_with_authmodule, [host_type(), JID]),
    do_verify_format(GroupName, Password, SPassword).


do_verify_format(GroupName, _P, #{iteration_count := _IC,
                                  sha    := #{salt := _, stored_key := _, server_key := _},
                                  sha224 := #{salt := _, stored_key := _, server_key := _},
                                  sha256 := #{salt := _, stored_key := _, server_key := _},
                                  sha384 := #{salt := _, stored_key := _, server_key := _},
                                  sha512 := #{salt := _, stored_key := _, server_key := _}}) when
                 GroupName == login_scram orelse GroupName == scram ->
    true;
do_verify_format({scram, Sha}, _Password, ScramMap = #{iteration_count := _IC}) ->
   maps:is_key(Sha, ScramMap);
do_verify_format(login_scram, _Password, SPassword) ->
    %% returned password is a tuple containing scram data
    {_, _, _, _} = SPassword;
do_verify_format(_, Password, SPassword) ->
    Password = SPassword.

set_acl_for_blocking(Config, Spec) ->
    User = proplists:get_value(username, Spec),
    LUser = jid:nodeprep(User),
    mongoose_helper:backup_and_set_config_option(Config, [{acl, host_type()}, blocked],
                                                 [#{user => LUser, match => current_domain}]).

unset_acl_for_blocking(Config) ->
    mongoose_helper:restore_config_option(Config, [{acl, host_type()}, blocked]).

configure_and_log_scram(Config, Sha, Mech) ->
    set_scram_sha(Config, Sha),
    log_one([{escalus_auth_method, Mech} | Config]).

configure_and_log_scram_plus(Config, Sha, Mech) ->
    set_scram_sha(Config, Sha),
    log_one_scram_plus([{escalus_auth_method, Mech} | Config]).

configure_and_fail_log_scram(Config, Sha, Mech) ->
    set_scram_sha(Config, Sha),
    {expected_challenge, _, _} = fail_log_one([{escalus_auth_method, Mech} | Config]).

configure_scram_plus_and_fail_log_scram(Config, Sha, Mech) ->
    set_scram_sha(Config, Sha),
    {expected_challenge, _, _} = fail_log_one_scram_plus([{escalus_auth_method, Mech} | Config]).

set_scram_sha(Config, Sha) ->
    NewAuthOpts = mongoose_helper:auth_opts_with_password_format({scram, [Sha]}),
    mongoose_helper:change_config_option(Config, {auth, host_type()}, NewAuthOpts),
    assert_password_format({scram, Sha}, Config).

fail_log_one(Config) ->
    [{alice, UserSpec}] = escalus_users:get_users([alice]),
    {error, {connection_step_failed, _, R}} = escalus_client:start(Config, UserSpec, <<"res">>),
    R.

fail_log_one_scram_plus(Config) ->
    [{neustradamus, UserSpec}] = escalus_users:get_users([neustradamus]),
    {error, {connection_step_failed, _, R}} = escalus_client:start(Config, UserSpec, <<"res">>),
    R.

are_sasl_scram_modules_supported() ->
    ScramModules = [cyrsasl_scram_sha1, cyrsasl_scram_sha224, cyrsasl_scram_sha256,
                    cyrsasl_scram_sha384, cyrsasl_scram_sha512],
    IsSupported = [mongoose_helper:supports_sasl_module(Module) || Module <- ScramModules],
    [true, true, true, true, true] == IsSupported.

restore_c2s(Config) ->
   C2SListener = proplists:get_value(c2s_listener, Config),
   mongoose_helper:restart_listener(mim(), C2SListener).

-define(NS_SASL, <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
auth_stanza(Mech, Payload) ->
    #xmlel{name = <<"auth">>,
           attrs = [{<<"xmlns">>, ?NS_SASL},
                    {<<"mechanism">>, Mech}],
           children = [#xmlcdata{content = base64:encode(Payload)}]}.

auth_response(Payload) ->
    #xmlel{name = <<"response">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = [#xmlcdata{content = base64:encode(Payload)}]}.
