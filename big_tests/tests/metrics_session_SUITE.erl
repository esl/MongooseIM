%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
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

-module(metrics_session_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").

-import(metrics_helper, [assert_counter/2,
                         assert_counter/3,
                         get_counter_value/1,
                         wait_for_counter/2,
                         wait_for_counter/3]).
-import(domain_helper, [host_type/0]).
-import(mongooseimctl_helper, [rpc_call/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, session},
     {group, session_global}].

groups() ->
    [{session, [parallel], [login_one,
                            login_many,
                            auth_failed]},
     {session_global, [sequence], [session_global,
                                   session_unique,
                                   session_node]}].

suite() ->
    [{require, ejabberd_node} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    instrument_helper:start([{sm_session, #{host_type => host_type()}},
                             {c2s_auth_failed, #{host_type => host_type()}},
                             {sm_total_sessions, #{}},
                             {sm_unique_sessions, #{}},
                             {sm_node_sessions, #{}}]),
    Config1 = mongoose_helper:backup_and_set_config_option(Config,
                                                           [instrumentation, probe_interval], 1),
    restart_sm_probes(),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config),
    mongoose_helper:restore_config_option(Config, [instrumentation, probe_interval]),
    instrument_helper:stop(),
    restart_sm_probes().

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

login_one(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun login_one_story/1).

login_one_story(Alice) ->
    assert_sm_login_event(Alice),
    sm_helper:stop_client_and_wait_for_termination(Alice),
    assert_sm_logout_event(Alice).

login_many(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun login_many_story/2).

login_many_story(Alice, Bob) ->
    assert_sm_login_event(Alice),
    assert_sm_login_event(Bob),
    sm_helper:stop_client_and_wait_for_termination(Alice),
    assert_sm_logout_event(Alice),
    sm_helper:stop_client_and_wait_for_termination(Bob),
    assert_sm_logout_event(Bob).

auth_failed(Config) ->
    UserSpec = escalus_fresh:create_fresh_user(Config, alice),
    UserSpecM = proplists:delete(password, UserSpec) ++ [{password, <<"mazabe">>}],
    {error, _} = escalus_client:start(Config, UserSpecM, <<"res1">>),
    assert_no_sm_login_event(UserSpec),
    assert_c2s_auth_failed(UserSpec).

%% Global

session_global(Config) ->
    escalus:story(Config, [{alice, 1}], fun(_Alice) ->
        assert_sm_total_sessions(1)
        end).

session_unique(Config) ->
    escalus:story(Config, [{alice, 2}], fun(_Alice1, _Alice2) ->
        assert_sm_unique_sessions(1),
        assert_sm_total_sessions(2)
        end).

session_node(Config) ->
    escalus:story(Config, [{alice, 2}, {bob, 1}], fun(_Alice1, _Alice2, _Bob) ->
        assert_sm_node_sessions(3),
        assert_sm_unique_sessions(2),
        assert_sm_total_sessions(3)
        end).

%% Instrumentation events

assert_sm_login_event(Client) ->
    JID = jid:from_binary(escalus_client:full_jid(Client)),
    F = fun(M) -> M =:= #{logins => 1, count => 1, jid => JID} end,
    instrument_helper:assert_one(sm_session, #{host_type => host_type()}, F).

assert_no_sm_login_event(UserSpec) ->
    LUser = jid:nodeprep(proplists:get_value(username, UserSpec)),
    F = fun(#{jid := JID}) -> jid:luser(JID) =:= LUser end,
    instrument_helper:assert_not_emitted(sm_session, #{host_type => host_type()}, F).

assert_sm_logout_event(Client) ->
    JID = jid:from_binary(escalus_client:full_jid(Client)),
    F = fun(M) -> M =:= #{logouts => 1, count => -1, jid => JID} end,
    instrument_helper:assert_one(sm_session, #{host_type => host_type()}, F).

assert_c2s_auth_failed(UserSpec) ->
    Server = proplists:get_value(server, UserSpec),
    UserName = proplists:get_value(username, UserSpec),
    F = fun(M) -> M =:= #{count => 1, server => Server, username => UserName} end,
    instrument_helper:assert_one(c2s_auth_failed, #{host_type => host_type()}, F).

assert_sm_total_sessions(ExpectedCount) ->
    assert_sm_sessions(ExpectedCount, sm_total_sessions).

assert_sm_unique_sessions(ExpectedCount) ->
    assert_sm_sessions(ExpectedCount, sm_unique_sessions).

assert_sm_node_sessions(ExpectedCount) ->
    assert_sm_sessions(ExpectedCount, sm_node_sessions).

assert_sm_sessions(ExpectedCount, Event) ->
    F = fun(#{count := Count}) -> ExpectedCount =:= Count end,
    instrument_helper:wait_and_assert_new(Event, #{}, F).

restart_sm_probes() ->
    rpc_call(ejabberd_sm, stop_probes, []),
    rpc_call(ejabberd_sm, start_probes, []).
