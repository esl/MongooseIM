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

-module(anonymous_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("jid/include/jid.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0, rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, anonymous}].

groups() ->
    [{anonymous, [sequence], all_tests()}].

all_tests() ->
    [connection_is_registered_with_sasl_anon,
     connection_is_registered_with_login,
     messages_story].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config0) ->
    NewUsers = escalus_ct:get_config(escalus_anon_users),
    Config = [{escalus_users, NewUsers}] ++ Config0,
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    AnonJID = erlang:get(anon_user),
    mongoose_helper:clear_last_activity(Config, AnonJID),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Anonymous tests
%%--------------------------------------------------------------------

connection_is_registered_with_sasl_anon(Config) ->
    escalus:story(Config, [{jon, 1}], fun(Jon) ->
        JID = jid:from_binary(escalus_client:short_jid(Jon)),
        OrigName = escalus_users:get_username(Config, jon),
        ?assertNotEqual(OrigName, JID#jid.luser),
        F = fun() -> rpc(mim(), ejabberd_auth, does_user_exist, [JID]) end,
        true = F(),
        escalus_connection:kill(Jon),
        mongoose_helper:wait_until(F, false)
    end).

connection_is_registered_with_login(Config) ->
    escalus:story(Config, [{anna, 1}], fun(Anna) ->
        JID = jid:from_binary(escalus_client:short_jid(Anna)),
        OrigName = escalus_users:get_username(Config, anna),
        ?assertEqual(OrigName, JID#jid.luser),
        F = fun() -> rpc(mim(), ejabberd_auth, does_user_exist, [JID]) end,
        true = F(),
        escalus_connection:kill(Anna),
        mongoose_helper:wait_until(F, false)
    end).

messages_story(Config) ->
    escalus:story(Config, [{anna, 1}, {jon, 1}], fun(Anna, Jon) ->
        erlang:put(anon_user, escalus_utils:get_jid(Jon)),
        escalus_client:send(Jon, escalus_stanza:chat_to(Anna, <<"Hi!">>)),
        Stanza = escalus_client:wait_for_stanza(Anna),
        %% Below's dirty, but there is no other easy way...
        escalus_assert:is_chat_message(<<"Hi!">>, Stanza)
    end).
