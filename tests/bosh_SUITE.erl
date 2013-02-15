%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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

-module(bosh_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, essential},
     {group, chat}].

groups() ->
    [{essential, [], [create_session,
                      terminate_session,
                      interleave_requests]},
     {chat, [], [interactive,
                 chat_msg]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).


init_per_group(essential, Config) ->
    Config;
init_per_group(chat, Config) ->
    escalus_users:create_users(Config, {by_name, [carol, geralt]});
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(essential, Config) ->
    Config;
end_per_group(chat, Config) ->
    escalus_users:delete_users(Config, {by_name, [carol, geralt]});
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

create_session(Config) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    CarolSpec = proplists:get_value(carol, NamedSpecs),
    {ok, Conn} = escalus_bosh:connect(CarolSpec),
    Rid = 155555,
    Domain = escalus_config:get_config(ejabberd_domain, Config),
    Body = escalus_bosh:session_creation_body(Rid, Domain),
    ok = escalus_bosh:send_raw(Conn, Body),
    timer:sleep(5000).
    %Stanza = escalus_connection:get_stanza(Conn, session_creation_response).
    %verify_session_exists().

terminate_session(_Config) ->
    throw(fail).

interleave_requests(_Config) ->
    throw(fail).

interactive(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        escalus_client:send(Carol, escalus_stanza:chat_to(<<"asd@localhost/esl1">>, <<"Hi1!">>)),
        escalus_client:send(Carol, escalus_stanza:chat_to(<<"asd@localhost/esl1">>, <<"Hi2!">>))

    end).

chat_msg(Config) ->
    escalus:story(Config, [{carol, 1}, {geralt, 1}], fun(Carol, Geralt) ->

        escalus_client:send(Carol, escalus_stanza:chat_to(Geralt, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Geralt)),

        escalus_client:send(Geralt, escalus_stanza:chat_to(Carol, <<"Hello!">>)),
        escalus:assert(is_chat_message, [<<"Hello!">>], escalus_client:wait_for_stanza(Carol))

        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

