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
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds

all() ->
    [
     {group, register},
     {group, bad_registration},
     {group, bad_cancelation},
     {group, registration_timeout},
     {group, change_account_details},
     {group, login},
     {group, login_scram},
     {group, login_scram_store_plain},
     {group, legacy_auth},
     {group, messages}
    ].

groups() ->
    [{register, [sequence], [register,
                             already_registered,
                             check_unregistered]},
     {bad_registration, [no_sequence], [null_password]},
     {bad_cancelation, [no_sequence], [bad_request_registration_cancelation,
                                       not_allowed_registration_cancelation]},
     {registration_timeout, [sequence], [registration_timeout,
                                         registration_failure_timeout]},
     {change_account_details, [no_sequence], [change_password,
                                              change_password_to_null]},
     {login, [parallel], all_tests()},
     {login_scram, [parallel], scram_tests()},
     {login_scram_store_plain, [parallel], scram_tests()},
     {legacy_auth, [parallel], [legacy_successful_plain,
                                legacy_unsuccessful_plain,
                                legacy_successful_digest,
                                legacy_blocked_user]},
     {messages, [sequence], [messages_story, message_zlib_limit]}].

scram_tests() ->
    [log_one, log_one_scram].

all_tests() ->
    [log_one,
     log_non_existent_plain,
     log_one_digest,
     log_non_existent_digest,
     log_one_scram,
     log_non_existent_scram,
     blocked_user
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
     escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(register, Config) ->
    [{escalus_user_db, xmpp} | skip_if_mod_register_not_enabled(Config)];
init_per_group(bad_registration, Config) ->
    [{escalus_user_db, xmpp} | Config];
init_per_group(bad_cancelation, Config) ->
    [{escalus_user_db, xmpp} | skip_if_mod_register_not_enabled(Config)];
init_per_group(registration_timeout, Config) ->
    case escalus_users:is_mod_register_enabled(Config) of
        true ->
            set_registration_timeout(Config);
        _ ->
            {skip, mod_register_disabled}
    end;
init_per_group(change_account_details, Config) ->
    skip_if_mod_register_not_enabled(Config);
init_per_group(GroupName, Config) when
      GroupName == login_scram; GroupName == login_scram_store_plain ->
    case get_store_type() of
        external ->
            {skip, "external store type requires plain password"};
        _ ->
            config_password_format(GroupName),
            Config2 = escalus:create_users(Config, escalus:get_users([alice, bob])),
            assert_password_format(GroupName, Config2)
    end;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(register, Config) ->
    escalus:delete_users(Config, escalus:get_users([admin, alice, bob]));
end_per_group(change_account_details, Config) ->
    ok;
end_per_group(bad_registration, _Config) ->
    ok;
end_per_group(bad_cancelation, _Config) ->
    ok;
end_per_group(registration_timeout, Config) ->
    Config1 = restore_registration_timeout(Config);
end_per_group(login_scram, Config) ->
    set_store_password(plain),
    escalus:delete_users(Config, escalus:get_users([alice, bob]));
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(DigestOrScram, Config) when
      DigestOrScram =:= log_one_digest; DigestOrScram =:= log_non_existent_digest;
      DigestOrScram =:= log_one_scram; DigestOrScram =:= log_non_existent_scram;
      DigestOrScram =:= legacy_successful_digest ->
    case get_store_type() of
        external ->
            {skip, "external store type requires plain password"};
        _ ->
            escalus:init_per_testcase(DigestOrScram, Config)
    end;
init_per_testcase(check_unregistered, Config) ->
    Config;
init_per_testcase(change_password, Config0) ->
    Config1 =  escalus:init_per_testcase(change_password, Config0),
    escalus:create_users(Config1, escalus:get_users([alice]));
init_per_testcase(change_password_to_null, Config0) ->
    Config1 =  escalus:init_per_testcase(change_password_to_null, Config0),
    escalus:create_users(Config1, escalus:get_users([alice]));
init_per_testcase(bad_request_registration_cancelation, Config0) ->
    Config1 =  escalus:init_per_testcase(bad_request_registration, Config0),
    escalus:create_users(Config1, escalus:get_users([alice]));
init_per_testcase(not_allowed_registration_cancelation, Config0) ->
    Config1 = escalus:init_per_testcase(not_allowed_registration_cancelation, Config0),
    Config2 = escalus:create_users(Config1, escalus:get_users([alice])),
    %% Use a configuration that will not allow inband cancelation (and
    %% registration).
    restart_mod_register_with_option(Config2, access, {access, none});
init_per_testcase(registration_failure_timeout, Config) ->
    ok = deny_everyone_registration(),
    escalus:init_per_testcase(registration_failure_timeout, Config);
init_per_testcase(message_zlib_limit, Config) ->
    Listeners = [Listener
                 || {Listener, _, _} <- escalus_ejabberd:rpc(ejabberd_config, get_local_option, [listen])],
    [{_U, Props}] = escalus_users:get_users([hacker]),
    Port = proplists:get_value(port, Props),
    case lists:keymember(Port, 1, Listeners) of
        true ->
            escalus:create_users(Config, escalus:get_users([hacker])),
            escalus:init_per_testcase(message_zlib_limit, Config);
        false ->
            {skip, port_not_configured_on_server}
    end;
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(change_password, Config) ->
    [{alice, Details}] = escalus_users:get_users([alice]),
    Alice = {alice, lists:keyreplace(password, 1, Details, {password, strong_pwd()})},
    {ok, result, Response} = escalus_users:delete_user(Config, Alice);
end_per_testcase(change_password_to_null, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice]));
end_per_testcase(message_zlib_limit, Config) ->
    escalus:delete_users(Config, escalus:get_users([hacker]));
end_per_testcase(check_unregistered, Config) ->
    Config;
end_per_testcase(bad_request_registration_cancelation, Config0) ->
    true = user_exists(alice, Config0),
    escalus:delete_users(Config0, escalus:get_users([alice]));
end_per_testcase(not_allowed_registration_cancelation, Config) ->
    restore_mod_register_options(Config),
    true = user_exists(alice, Config),
    escalus:delete_users(Config, escalus:get_users([alice]));
end_per_testcase(registration_timeout, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob]));
end_per_testcase(registration_failure_timeout, Config) ->
    ok = allow_everyone_registration();
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------


register(Config) ->
    [{Name1, UserSpec1}, {Name2, UserSpec2}] = escalus_users:get_users([alice, bob]),
    [{_, AdminSpec}] = escalus_users:get_users([admin]),
    [Username1, _Server1, _Pass1] = escalus_users:get_usp(Config, UserSpec1),
    [Username2, _Server2, _Pass2] = escalus_users:get_usp(Config, UserSpec2),
    [AdminU, AdminS, AdminP] = escalus_users:get_usp(Config, AdminSpec),

    escalus_ejabberd:rpc(ejabberd_auth, try_register, [AdminU, AdminS, AdminP]),

    escalus:story(Config, [{admin, 1}], fun(Admin) ->
            escalus:create_users(Config, escalus:get_users([Name1, Name2])),

            Predicates = [
                          fun(Stanza) ->
                                  Body = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
                                  escalus_pred:is_chat_message(Stanza)
                                  andalso
                                  re:run(Body, <<"registered">>, []) =/= nomatch
                                  andalso
                                  re:run(Body, Username, []) =/= nomatch
                          end
                          || Username <- [Username1, Username2]
                         ],
            escalus:assert_many(Predicates, escalus:wait_for_stanzas(Admin, 2))
        end).

already_registered(Config) ->

    %% This relies on Alice already being registered in test case
    %% `register' in the same group as this test (group `register').

    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        escalus:send(Alice, escalus_stanza:get_registration_fields()),

        Stanza = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_result, Stanza),
        true = has_registered_element(Stanza)

    end).

null_password(Config) ->
    [{alice, Details}] = escalus_users:get_users([alice]),
    Alice = {alice, lists:keyreplace(password, 1, Details, {password, <<>>})},
    {error, _, Response} = escalus_users:create_user(Config, Alice),
    escalus:assert(is_iq_error, Response),
    %% This error response means there was no character data,
    %% i.e. elements `<password\>' or `<password></password>' where
    %% indeed present.
    {username, Name} = lists:keyfind(username, 1, Details),
    {server, Server} = lists:keyfind(server, 1, Details),
    escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Response),
    false = escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Name, Server]).

check_unregistered(Config) ->
    escalus:delete_users(Config, escalus:get_users([admin, alice, bob])),
    [{_, UserSpec}| _] = escalus_users:get_users(all),
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    false = escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Username, Server]).

bad_request_registration_cancelation(Config) ->

    %% To quote XEP 0077, section 3.2, table 1 (unregister error
    %% cases): "The <remove/> element [is] not the only child element
    %% of the <query/> element."

    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        %% Alice sends bad cancelation request
        escalus:send(Alice, bad_cancelation_stanza()),

        %% Alice receives failure response
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Stanza),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], Stanza)

    end).

not_allowed_registration_cancelation(Config) ->

    %% To quote XEP 0077, section 3.2, table 1 (unregister error
    %% cases): "No sender is allowed to cancel registrations in-band."

    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        %% Alice sends cancelation request
        escalus:send(Alice, escalus_stanza:remove_account()),

        %% Alice receives failure response
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Stanza),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Stanza)

    end).

registration_timeout(Config) ->
    timer:sleep(timer:seconds(?REGISTRATION_TIMEOUT)),
    [Alice, Bob] = escalus_users:get_users([alice, bob]),

    %% The first user should be created successfully
    escalus_users:verify_creation(escalus_users:create_user(Config, Alice)),

    %% Creation of the second one should err because of not timing out yet
    {error, failed_to_register, Stanza} = escalus_users:create_user(Config, Bob),
    escalus:assert(is_iq_error, Stanza),
    %% Something else may be more acceptable for the assertion
    %% below... 2nd paragraph, section 3.1.1, XEP 0077: [...] a server
    %% MAY return a `<not-acceptable/>' stanza error if [...] an
    %% entity attempts to register a second identity after
    %% successfully completing the registration use case.
    escalus:assert(is_error, [<<"wait">>, <<"resource-constraint">>], Stanza),

    %% After timeout, the user should be registered successfully
    timer:sleep(erlang:round(?REGISTRATION_TIMEOUT * 1.5 * 1000)),
    escalus_users:verify_creation(escalus_users:create_user(Config, Bob)).

registration_failure_timeout(Config) ->
    timer:sleep(timer:seconds(?REGISTRATION_TIMEOUT + 1)),
    [Alice] = escalus_users:get_users([alice]),

    %% Registration of the first user should fail because of access denial
    {error,failed_to_register,R} = escalus_users:create_user(Config, Alice),
    escalus:assert(is_iq_error, R),
    escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], R),

    %% Registration of a second one should fail because requests were
    %% made in quick succession
    {error,failed_to_register,S} = escalus_users:create_user(Config, Alice),
    escalus:assert(is_iq_error, S),
    escalus:assert(is_error, [<<"wait">>, <<"resource-constraint">>], S).

change_password(Config) ->

    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        escalus:send(Alice,
            Q = escalus_stanza:iq_set(?NS_INBAND_REGISTER,
                [#xmlel{name = <<"username">>,
                        children = [#xmlcdata{content = <<"alice">>}]},
                 #xmlel{name = <<"password">>,
                        children = [#xmlcdata{content = strong_pwd()}]}])),

        R = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_result, [Q], R)

    end).

change_password_to_null(Config) ->

    %% Section 3.3, XEP 0077: If the user provides an empty password
    %% element or a password element that contains no XML character
    %% data (i.e., either <password/> or <password></password>), the
    %% server or service MUST NOT change the password to a null value,
    %% but instead MUST maintain the existing password.

    %% By the above, `end_per_testcase' should succeed. XEP 0077
    %% doesn't say how how an XMPP sever should respond, but since
    %% this is in IQ, it must: so we choose to require a `not-allowed'
    %% response.

    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        escalus:send(Alice,
            escalus_stanza:iq_set(?NS_INBAND_REGISTER,
                [#xmlel{name = <<"username">>,
                        children = [#xmlcdata{content = <<"alice">>}]},
                 #xmlel{name = <<"password">>,
                        children = [#xmlcdata{content = <<"">>}]}])),

        R = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_error, R),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], R)

    end).

log_one(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Alice))

        end).

log_one_digest(Config) ->
    log_one([{escalus_auth_method, <<"DIGEST-MD5">>} | Config]).

log_one_scram(Config) ->
    log_one([{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config]).


log_non_existent_plain(Config) ->
    {auth_failed, _, _} = log_non_existent(Config).

log_non_existent_digest(Config) ->
    R = log_non_existent([{escalus_auth_method, <<"DIGEST-MD5">>} | Config]),
    {expected_challenge, _, _} = R.

log_non_existent_scram(Config) ->
    R = log_non_existent([{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config]),
    {expected_challenge, _, _} = R.

log_non_existent(Config) ->
    [{kate, UserSpec}] = escalus_users:get_users([kate]),
    {error, {connection_step_failed, _, R}} = escalus_client:start(Config, UserSpec, <<"res">>),
    R.

blocked_user(_Config) ->
    [{_, Spec}] = escalus_users:get_users([alice]),
    set_acl_for_blocking(Spec),
    try
        {ok, _Alice, _Spec2, _Features} = escalus_connection:start(Spec),
        ct:fail("Alice authenticated but shouldn't")
    catch
        error:{assertion_failed, assert, is_iq_result, Stanza, _Bin} ->
            <<"cancel">> = exml_query:path(Stanza, [{element, <<"error">>}, {attr, <<"type">>}])
    after
        unset_acl_for_blocking(Spec)
    end,
    ok.

messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        % Alice sends a message to Bob
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),

        % Bob gets the message
        escalus_assert:is_chat_message(<<"Hi!">>, escalus_client:wait_for_stanza(Bob))

    end).

message_zlib_limit(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        [{_, Spec}] = escalus_users:get_users([hacker]),
        {ok, Hacker, _Spec2, _Features} = escalus_connection:start(Spec),

        ManySpaces = [ 32 || _N <- lists:seq(1, 10*1024) ],

        escalus:send(Hacker, escalus_stanza:chat_to(Alice, ManySpaces)),

        escalus:assert(is_stream_error, [<<"policy-violation">>, <<"XML stanza is too big">>],
                       escalus:wait_for_stanza(Hacker)),
        escalus:assert(is_stream_end, escalus:wait_for_stanza(Hacker))

    end).

legacy_successful_digest(Config) ->
    legacy_auth(Config, legacy_auth_digest).

legacy_successful_plain(Config) ->
    legacy_auth(Config, legacy_auth_plain).

legacy_unsuccessful_plain(ConfigIn) ->
    Config = escalus_fresh:create_users(ConfigIn, [alice]),
    Spec = escalus_users:get_userspec(Config, alice),
    NewSpec = lists:keyreplace(password, 1, Spec, {password, <<"wrong_pass">>}),
    try
        do_legacy_auth(NewSpec, legacy_auth_plain),
        ct:fail("Authenticated but shouldn't")
    catch
        error:{assertion_failed, assert, is_iq_result, _, _, _} ->
            ok
    end.

legacy_auth(ConfigIn, Function) ->
    %% given
    Config = escalus_fresh:create_users(ConfigIn, [alice]),
    Spec = escalus_users:get_userspec(Config, alice),
    do_legacy_auth(Spec, Function).

do_legacy_auth(Spec, Function) ->
    Steps = [
             %% when
             {legacy_stream_helper, start_stream_pre_xmpp_1_0},
             {legacy_stream_helper, Function}
            ],
    %% ok, now do the plan from above
    {ok, Conn, _, _} = escalus_connection:start(Spec, Steps),
    escalus_connection:stop(Conn).


legacy_blocked_user(ConfigIn) ->
    Config = escalus_fresh:create_users(ConfigIn, [alice]),
    Spec = escalus_users:get_userspec(Config, alice),
    set_acl_for_blocking(Spec),
    try
        do_legacy_auth(Spec, legacy_auth_plain),
        ct:fail("alice authenticated but shouldn't")
    catch
        error:{assertion_failed, assert, is_iq_result, _, Stanza, _Bin} ->
            <<"cancel">> = exml_query:path(Stanza,
                                           [{element, <<"error">>},
                                            {attr, <<"type">>}])
    after
        unset_acl_for_blocking(Spec)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

skip_if_mod_register_not_enabled(Config) ->
    case escalus_users:is_mod_register_enabled(Config) of
        true ->
            Config; % will create users inside test case
        _ ->
            {skip, mod_register_disabled}
    end.

strong_pwd() ->
    <<"Sup3r","c4li","fr4g1","l1571c","3xp1","4l1","d0c10u5">>.

set_registration_timeout(Config) ->
    Record = {local_config, registration_timeout, ?REGISTRATION_TIMEOUT},
    OldTimeout = escalus_ejabberd:rpc(ejabberd_config, get_local_option,
                                      [registration_timeout]),
    true = escalus_ejabberd:rpc(ets, insert, [local_config, Record]),
    [ {old_timeout, OldTimeout} | Config ].

restore_registration_timeout(Config) ->
    {old_timeout, OldTimeout} = proplists:lookup(old_timeout, Config),
    Record = {local_config, registration_timeout, OldTimeout},
    true = escalus_ejabberd:rpc(ets, insert, [local_config, Record]),
    proplists:delete(old_timeout, Config).

deny_everyone_registration() ->
    ok = change_registration_settings_for_everyone(deny).

allow_everyone_registration() ->
    ok = change_registration_settings_for_everyone(allow).

change_registration_settings_for_everyone(Rule)
  when allow =:= Rule; deny =:= Rule ->
    {atomic,ok} = escalus_ejabberd:rpc(ejabberd_config, add_global_option,
        [{access, register, global}, [{Rule, all}]]),
    ok.

get_client_details(Identifier) ->
    [{Identifier, Details}] = escalus_users:get_users([Identifier]),
    {username, Name} = lists:keyfind(username, 1, Details),
    {server, Server} = lists:keyfind(server, 1, Details),
    {string(Name), string(Server)}.

get_store_type() ->
    XMPPDomain = escalus_ejabberd:unify_str_arg(
                   ct:get_config({hosts, mim, domain})),
    escalus_ejabberd:rpc(ejabberd_auth, store_type,
                         [XMPPDomain]).

set_store_password(Type) ->
    XMPPDomain = escalus_ejabberd:unify_str_arg(
                   ct:get_config({hosts, mim, domain})),
    AuthOpts = escalus_ejabberd:rpc(ejabberd_config, get_local_option,
                                    [{auth_opts, XMPPDomain}]),
    NewAuthOpts = lists:keystore(password_format, 1, AuthOpts, {password_format, Type}),
    escalus_ejabberd:rpc(ejabberd_config, add_local_option,
                         [{auth_opts, XMPPDomain}, NewAuthOpts]).



config_password_format(login_scram) ->
    set_store_password(scram);
config_password_format(_) ->
    set_store_password(plain).

assert_password_format(GroupName, Config) ->
    Users = proplists:get_value(escalus_users, Config),
    [verify_format(GroupName, User) || User <- Users],
    Config.

verify_format(GroupName, {_User, Props}) ->
    Username = escalus_utils:jid_to_lower(proplists:get_value(username, Props)),
    Server = proplists:get_value(server, Props),
    Password = proplists:get_value(password, Props),

    SPassword = escalus_ejabberd:rpc(ejabberd_auth, get_password, [Username, Server]),
    do_verify_format(GroupName, Password, SPassword).

do_verify_format(login_scram, _Password, SPassword) ->
    %% returned password is a tuple containing scram data
    {_, _, _, _} = SPassword;
do_verify_format(_, Password, SPassword) ->
    Password = SPassword.

has_registered_element(Stanza) ->
        [#xmlel{name = <<"registered">>}] =:= exml_query:paths(Stanza,
            [{element, <<"query">>}, {element, <<"registered">>}]).

bad_cancelation_stanza() ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"query">>,
        attrs = [{<<"xmlns">>, <<"jabber:iq:register">>}],
        children = [#xmlel{name = <<"remove">>},
                    %% The <remove/> element is not the only child element of the
                    %% <query/> element.
                    #xmlel{name = <<"foo">>}]}]).

restart_mod_register_with_option(Config, Name, Value) ->
    Domain = ct:get_config({hosts, mim, domain}),
    ModuleOptions = escalus_ejabberd:rpc(gen_mod, loaded_modules_with_opts, [Domain]),
    {mod_register, OldRegisterOptions} = lists:keyfind(mod_register, 1, ModuleOptions),
    {atomic, ok} = dynamic_modules:stop(Domain, mod_register),
    NewRegisterOptions = lists:keystore(Name, 1, OldRegisterOptions, Value),
    ok = dynamic_modules:start(Domain, mod_register, NewRegisterOptions),
    [{old_mod_register_opts, OldRegisterOptions}|Config].

restore_mod_register_options(Config0) ->
    Domain = ct:get_config({hosts, mim, domain}),
    {value, {old_mod_register_opts, RegisterOpts}, Config1} =
        lists:keytake(old_mod_register_opts, 1, Config0),
    {atomic, ok} = dynamic_modules:stop(Domain, mod_register),
    ok = dynamic_modules:start(Domain, mod_register, RegisterOpts),
    Config1.

user_exists(Name, Config) ->
    {Name, Client} = escalus_users:get_user_by_name(Name),
    [Username, Server, _Pass] = escalus_users:get_usp(Config, Client),
    escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Username, Server]).

string(<<_/binary>> = Subject) ->
    erlang:binary_to_list(Subject).

set_acl_for_blocking(Spec) ->
    modify_acl_for_blocking(add, Spec).

unset_acl_for_blocking(Spec) ->
    modify_acl_for_blocking(delete, Spec).

modify_acl_for_blocking(Method, Spec) ->
    ct:print("Spec: ~p", [Spec]),
    Domain = ct:get_config({hosts, mim, domain}),
    User = proplists:get_value(username, Spec),
    Lower = escalus_utils:jid_to_lower(User),
    escalus_ejabberd:rpc(acl, Method, [Domain, blocked, {user, Lower}]).
