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
     {group, messages}
    ].

groups() ->
    [{register, [sequence], [register,
                             check_unregistered]},
     {registration_timeout, [sequence], [registration_timeout]},
     {login, [sequence], [log_one,
                          log_one_digest]},
     {login_scram, [sequence], scram_tests()},
     {login_scram_store_plain, [sequence], scram_tests()},
     {messages, [sequence], [messages_story]}].

scram_tests() ->
    [log_one, log_one_scram].

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
            case GroupName of
                login_scram ->
                    set_store_password(scram);
                _ ->
                    set_store_password(plain)
            end,
            escalus:create_users(Config, {by_name, [alice, bob]})
    end;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(register, Config) ->
    ok;
end_per_group(registration_timeout, Config) ->
    Config1 = restore_registration_timeout(Config),
    escalus_users:delete_users(Config1, {by_name, [alice, bob]});
end_per_group(login_scram, Config) ->
    set_store_password(plain),
    escalus:delete_users(Config, {by_name, [alice, bob]});
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(log_one_digest, Config) ->
    case get_auth_method() of
        external ->
            {skip, "external authentication requires plain password"};
        ldap ->
            {skip, "ldap authentication requires plain password"};
        _ ->
            Conf1 = [ {escalus_auth_method, <<"DIGEST-MD5">>} | Config],
            escalus:init_per_testcase(log_one_digest, Conf1)
    end;
init_per_testcase(log_one_scram, Config) ->
    Conf1 = [{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config],
    escalus:init_per_testcase(log_one_digest, Conf1);
init_per_testcase(log_one_basic_digest, Config) ->
    Conf1 = [ {escalus_auth_method, digest} | Config],
    escalus:init_per_testcase(log_one_digest, Conf1);
init_per_testcase(log_one_basic_plain, Config) ->
    Conf1 = [ {escalus_auth_method, password} | Config],
    escalus:init_per_testcase(log_one_digest, Conf1);
init_per_testcase(check_unregistered, Config) ->
    Config;
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(check_unregistered, Config) ->
    Config;
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

register(Config) ->
    Users = escalus_config:get_config(escalus_users, Config),
    [{Name1, UserSpec1}, {Name2, UserSpec2} | _] = Users,
    {_, AdminSpec} = lists:keyfind(admin, 1, Users),
    [Username1, Server1, _Pass1] = escalus_users:get_usp(Config, UserSpec1),
    [Username2, Server2, _Pass2] = escalus_users:get_usp(Config, UserSpec2),
    [AdminU, AdminS, AdminP] = escalus_users:get_usp(Config, AdminSpec),

    {atomic, ok} = escalus_ejabberd:rpc(ejabberd_auth, try_register, [AdminU, AdminS, AdminP]),

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
            escalus:assert_many(Predicates, escalus:wait_for_stanzas(Admin, 2)),

            true = escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Username1, Server1]),
            true = escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Username2, Server2])
        end).

check_unregistered(Config) ->
    escalus:delete_users(Config, {by_name, [admin, alice, bob]}),
    [{_, UserSpec}| _] = escalus_users:get_users(all),
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    false = escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Username, Server]).

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
    log_one(Config).

log_one_scram(Config) ->
    log_one(Config).

log_one_basic_plain(Config) ->
    log_one(Config).

log_one_basic_digest(Config) ->
    log_one(Config).


messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        % Alice sends a message to Bob
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),

        % Bob gets the message
        escalus_assert:is_chat_message(<<"Hi!">>, escalus_client:wait_for_stanza(Bob))

    end).

message_zlib_limit(Config) ->
    escalus:story(Config, [{alice, 1}, {hacker, 1}], fun(Alice, Hacker) ->
        ManySpaces = [ 32 || _N <- lists:seq(1, 10*1024) ],

        escalus:send(Hacker, escalus_stanza:chat_to(Alice, ManySpaces)),

        escalus:assert(is_stream_error, [<<"policy-violation">>, <<"XML stanza is too big">>],
                       escalus:wait_for_stanza(Hacker)),
        escalus:assert(is_stream_end, escalus:wait_for_stanza(Hacker))

    end).

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
    escalus_ejabberd:rpc(ejabberd_config, add_local_option,
                         [{auth_password_format, XMPPDomain}, Type]).
