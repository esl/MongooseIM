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
-include_lib("common_test/include/ct.hrl").

-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(REGISTRATION_TIMEOUT, 2).  %% seconds

all() ->
    [
     {group, register},
     {group, registration_timeout},
     {group, login},
     {group, login_scram},
     {group, login_scram_store_plain},
     {group, legacy_auth},
     {group, messages}
    ].

groups() ->
    [{register, [sequence], [register,
                             check_unregistered,
			     bad_request_registration_cancelation]},
     {registration_timeout, [sequence], [registration_timeout]},
     {login, [sequence], all_tests()},
     {login_scram, [sequence], scram_tests()},
     {login_scram_store_plain, [sequence], scram_tests()},
     {legacy_auth, [sequence], [legacy_successful_plain,
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
    escalus:end_per_suite(Config).

init_per_group(register, Config) ->
    case escalus_users:is_mod_register_enabled(Config) of
        true ->
            Config; % will create users inside test case
        _ ->
            {skip, mod_register_disabled}
    end;
init_per_group(registration_timeout, Config) ->
    case escalus_users:is_mod_register_enabled(Config) of
        true ->
            set_registration_timeout(Config);
        _ ->
            {skip, mod_register_disabled}
    end;
init_per_group(GroupName, Config) when
      GroupName == login_scram; GroupName == login_scram_store_plain ->
    case get_auth_method() of
        external ->
            {skip, "external authentication requires plain password"};
        _ ->
            config_password_format(GroupName),
            Config2 = escalus:create_users(Config, {by_name, [alice, bob]}),
            assert_password_format(GroupName, Config2)
    end;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(register, _Config) ->
    ok;
end_per_group(registration_timeout, Config) ->
    Config1 = restore_registration_timeout(Config),
    escalus_users:delete_users(Config1, {by_name, [alice, bob]});
end_per_group(login_scram, Config) ->
    set_store_password(plain),
    escalus:delete_users(Config, {by_name, [alice, bob]});
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(DigestOrScram, Config) when
      DigestOrScram =:= log_one_digest; DigestOrScram =:= log_non_existent_digest;
      DigestOrScram =:= log_one_scram; DigestOrScram =:= log_non_existent_scram;
      DigestOrScram =:= legacy_successful_digest ->
    case get_auth_method() of
        external ->
            {skip, "external authentication requires plain password"};
        ldap ->
            {skip, "ldap authentication requires plain password"};
        _ ->
            escalus:init_per_testcase(DigestOrScram, Config)
    end;
init_per_testcase(check_unregistered, Config) ->
    Config;
init_per_testcase(message_zlib_limit, Config) ->
    Listeners = [Listener
                 || {Listener, _, _} <- escalus_ejabberd:rpc(ejabberd_config, get_local_option, [listen])],
    [{_U, Props}] = escalus_users:get_users({by_name, [hacker]}),
    Port = proplists:get_value(port, Props),
    case lists:keymember(Port, 1, Listeners) of
        true ->
            escalus:create_users(Config, {by_name, [hacker]}),
            escalus:init_per_testcase(message_zlib_limit, Config);
        false ->
            {skip, port_not_configured_on_server}
    end;
init_per_testcase(Name, Config)
  when Name == blocked_user; Name == legacy_blocked_user ->
    Domain = ct:get_config(ejabberd_domain),
    escalus_ejabberd:rpc(acl, add, [Domain, blocked, {user, <<"alice">>}]),
    escalus:init_per_testcase(Name, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(Name, Config)
  when Name == blocked_user; Name == legacy_blocked_user ->
    Domain = ct:get_config(ejabberd_domain),
    escalus_ejabberd:rpc(acl, delete, [Domain, blocked, {user, <<"alice">>}]),
    Config;
end_per_testcase(message_zlib_limit, Config) ->
    escalus:delete_users(Config, {by_name, [hacker]});
end_per_testcase(check_unregistered, Config) ->
    Config;
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

register(Config) ->
    [{Name1, UserSpec1}, {Name2, UserSpec2}] = escalus_users:get_users({by_name, [alice, bob]}),
    [{_, AdminSpec}] = escalus_users:get_users({by_name, [admin]}),
    [Username1, Server1, _Pass1] = escalus_users:get_usp(Config, UserSpec1),
    [Username2, Server2, _Pass2] = escalus_users:get_usp(Config, UserSpec2),
    [AdminU, AdminS, AdminP] = escalus_users:get_usp(Config, AdminSpec),

    ok = escalus_ejabberd:rpc(ejabberd_auth, try_register, [AdminU, AdminS, AdminP]),

    escalus:story(Config, [{admin, 1}], fun(Admin) ->
            escalus:create_users(Config, {by_name, [Name1, Name2]}),

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

check_unregistered(Config) ->
    escalus:delete_users(Config, {by_name, [admin, alice, bob]}),
    [{_, UserSpec}| _] = escalus_users:get_users(all),
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    false = escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Username, Server]).

bad_request_registration_cancelation(Config) ->

    %% To quote XEP 0077, section 3.2, table 1 (unregister error
    %% cases): "The <remove/> element [is] not the only child element
    %% of the <query/> element."

    escalus:create_users(Config, {by_name, [alice]}),

    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        %% Alice sends cancelation request
        escalus:send(Alice, bad_cancelation_stanza()),

        %% Alice receives failure response
        escalus:assert(is_iq_error,
                       escalus:wait_for_stanza(Alice))

    end).

registration_timeout(Config) ->
    [Alice, Bob] = escalus_users:get_users({by_name, [alice, bob]}),

    %% The first user should be created successfully
    escalus_users:verify_creation(escalus_users:create_user(Config, Alice)),

    %% Creation of the second one should err because of not timing out yet
    {error, failed_to_register, Reason} = escalus_users:create_user(Config, Bob),
    escalus:assert(is_iq_error, Reason),

    %% After timeout, the user should be registered successfully
    timer:sleep(erlang:round(?REGISTRATION_TIMEOUT * 1.5 * 1000)),
    escalus_users:verify_creation(escalus_users:create_user(Config, Bob)).

log_one(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

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
    [{kate, UserSpec}] = escalus_users:get_users({by_name, [kate]}),
    {error, {connection_step_failed, _, R}} = escalus_client:start(Config, UserSpec, <<"res">>),
    R.

blocked_user(_Config) ->
    [{_, Spec}] = escalus_users:get_users({by_name, [alice]}),
    try
        {ok, _Alice, _Spec2, _Features} = escalus_connection:start(Spec),
        ct:fail("Alice authenticated but shouldn't")
    catch
        error:{assertion_failed, assert, is_iq_result, Stanza, _Bin} ->
            <<"cancel">> = exml_query:path(Stanza, [{element, <<"error">>}, {attr, <<"type">>}])
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
        [{_, Spec}] = escalus_users:get_users({by_name, [hacker]}),
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
legacy_unsuccessful_plain(Config) ->
    Spec = escalus_users:get_userspec(Config, alice),
    NewSpec = lists:keyreplace(password, 1, Spec, {password, <<"wrong_pass">>}),
    Users = ?config(escalus_users, Config),
    NewUsers = lists:keyreplace(alice, 1, Users, {alice, NewSpec}),
    NewConfig = lists:keyreplace(escalus_users, 1, Config, {escalus_users, NewUsers}),
    try
        legacy_auth(NewConfig, legacy_auth_plain),
        ct:fail("Authenticated but shouldn't")
    catch
        error:{assertion_failed,assert,is_iq_result,_,_,_} ->
            ok
    end.

legacy_auth(Config, Function) ->
    %% given
    Spec = escalus_users:get_userspec(Config, alice),
    Steps = [
             %% when
             {legacy_stream_helper, start_stream_pre_xmpp_1_0},
             {legacy_stream_helper, Function}
            ],
    %% ok, now do the plan from above
    {ok, Conn, _, _} = escalus_connection:start(Spec, Steps),
    escalus_connection:stop(Conn).


legacy_blocked_user(Config) ->
    try
        legacy_auth(Config, legacy_auth_plain),
        ct:fail("alice authenticated but shouldn't")
    catch
        error:{assertion_failed, assert, is_iq_result, _, Stanza, _Bin} ->
            <<"cancel">> = exml_query:path(Stanza,
                                           [{element, <<"error">>},
                                            {attr, <<"type">>}])
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

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

get_auth_method() ->
    XMPPDomain = escalus_ejabberd:unify_str_arg(
                   ct:get_config(ejabberd_domain)),
    escalus_ejabberd:rpc(ejabberd_auth, store_type,
                         [XMPPDomain]).

set_store_password(Type) ->
    XMPPDomain = escalus_ejabberd:unify_str_arg(
                   ct:get_config(ejabberd_domain)),
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

bad_cancelation_stanza() ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"query">>,
        attrs = [{<<"xmlns">>, <<"jabber:iq:register">>}],
	children = [#xmlel{name = <<"remove">>},
	%% The <remove/> element was not the only child element of the
	%% <query/> element.
	#xmlel{name = <<"foo">>}]}]).
