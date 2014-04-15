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
     {group, messages}
    ].

groups() ->
    [{register, [sequence], [register,
                             check_unregistered]},
     {registration_timeout, [sequence], [registration_timeout]},
     {login, [sequence], [log_one,
                          log_one_digest
%%                          log_one_basic_plain,
%%                          log_one_basic_digest
                         ]},
     {messages, [sequence], [messages_story]}].

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
            escalus:create_users(Config);
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
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(register, _Config) ->
    ok;
end_per_group(registration_timeout, Config) ->
    Config1 = restore_registration_timeout(Config),
    escalus_users:delete_users(Config1, {by_name, [alice, bob]});
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(log_one_digest, Config) ->
    XMPPDomain = ct:get_config(ejabberd_domain),
    case escalus_ejabberd:rpc(ejabberd_config, get_local_option,
                              [{auth_method, XMPPDomain}]) of
        external ->
            {skip, "external authentication requires plain password"};
        ldap ->
            {skip, "ldap authentication requires plain password"};
        _ ->
            Conf1 = [ {escalus_auth_method, <<"DIGEST-MD5">>} | Config],
            escalus:init_per_testcase(log_one_digest, Conf1)
    end;
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
    %% User should be registered in an init function
    [{_, UserSpec} | _] = escalus_config:get_config(escalus_users, Config),
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    true = escalus_ejabberd:rpc(ejabberd_auth, is_user_exists, [Username, Server]).

check_unregistered(Config) ->
    escalus:delete_users(Config),
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
