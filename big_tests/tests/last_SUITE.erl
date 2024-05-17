%%==============================================================================
%% Copyright 2011 Erlang Solutions Ltd.
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
-module(last_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(config_parser_helper, [mod_config_with_auto_backend/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, valid_queries},
     {group, invalid_queries}].

groups() ->
    [{valid_queries, [sequence], valid_test_cases()},
     {invalid_queries, invalid_test_cases()}].

valid_test_cases() -> [online_user_query,
                       last_online_user,
                       last_offline_user,
                       last_server, sessions_cleanup].

invalid_test_cases() -> [user_not_subscribed_receives_error].

suite() ->
    escalus:suite().

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, required_modules()),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(valid_queries, Config0) ->
    Config1 = escalus_fresh:create_users(Config0, [{alice, 1}, {bob, 1}]),
    Config2 = escalus:make_everyone_friends(Config1),
    %% This check ensures that there are no registered sessions.
    %% But in mongoose_c2s we first unset session,
    %% then broadcast presence unavailable.
    %% This check uses ejabberd_sm to get information about sessions.
    escalus_ejabberd:wait_for_session_count(Config2, 0),
    %% Kick "friendly" users
    %% kick_everyone uses mongoose_c2s_sup to information about client processes.
    mongoose_helper:kick_everyone(),
    Config2;
init_per_group(invalid_queries, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    mongoose_helper:kick_everyone(),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Last tests
%%--------------------------------------------------------------------
online_user_query(Config) ->
    %% Alice and Bob are friends
    escalus:story(Config, [{alice, 1}, {bob, 1}],
                  fun(Alice, Bob) ->
                          %% Alice asks about Bob's last activity
                          BobJid = escalus_utils:get_jid(Bob),
                          escalus_client:send(Alice, escalus_stanza:last_activity(BobJid)),
                          %% Bob gets IQ and answers
                          BobGetsIQ = escalus_client:wait_for_stanza(Bob),
                          escalus_client:send(Bob, answer_last_activity(BobGetsIQ)),
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_last_result, Stanza),
                          0 = get_last_activity(Stanza)
                  end).

last_online_user(Config) ->
    %% Alice and Bob are friends
    escalus:story(Config, [{alice, 1}, {bob, 1}],
                  fun(Alice, Bob) ->
                          %% Alice asks about Bob's last activity
                          BobShortJID = escalus_client:short_jid(Bob),
                          escalus_client:send(Alice, escalus_stanza:last_activity(BobShortJID)),

                          %% server replies on Bob's behalf
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_last_result, Stanza),
                          0 = get_last_activity(Stanza)
                  end).

last_offline_user(Config) ->
    %% Alice and Bob are friends
    escalus:story(Config, [{alice, 1}],
                  fun(Alice) ->
                          %% Bob logs in
                          {ok, Bob} = escalus_client:start_for(Config, bob, <<"bob">>),

                          %% Bob logs out with a status
                          Status = escalus_stanza:tags([{<<"status">>, <<"I am a banana!">>}]),
                          Presence = escalus_stanza:presence(<<"unavailable">>, Status),
                          escalus_client:send(Bob, Presence),
                          escalus_client:stop(Config, Bob),
                          timer:sleep(1024), % more than a second

                          %% Alice asks for Bob's last availability
                          BobShortJID = escalus_client:short_jid(Bob),
                          escalus_client:send(Alice, escalus_stanza:last_activity(BobShortJID)),

                          %% Alice receives Bob's status and last online time > 0
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_last_result, Stanza),
                          true = (1 =< get_last_activity(Stanza)),
                          <<"I am a banana!">> = get_last_status(Stanza)
                  end).

last_server(Config) ->
    %% This story can be fresh_story
    escalus:fresh_story(Config, [{alice, 1}],
                  fun(Alice) ->
                          %% Alice asks for server's uptime
                          Server = escalus_users:get_server(Config, alice),
                          escalus_client:send(Alice, escalus_stanza:last_activity(Server)),

                          %% Server replies with the uptime > 0
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_last_result, Stanza),
                          true = (get_last_activity(Stanza) > 0)
                  end).

user_not_subscribed_receives_error(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Alice asks about Bob's last activity
        BobShortJID = escalus_client:short_jid(Bob),
        escalus_client:send(Alice, escalus_stanza:last_activity(BobShortJID)),

        %% server replies with an error, since there is no subscription
        Error = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Error),

        %% Alice asks Bob directly for last activity
        BobFullJID = escalus_client:full_jid(Bob),
        escalus_client:send(Alice, escalus_stanza:last_activity(BobFullJID)),

        Error1 = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Error1),
        ok
    end).

sessions_cleanup(Config) ->
    N = distributed_helper:mim(),
    HostType = domain_helper:host_type(),
    Server = domain_helper:domain(),
    CreateUser = fun(Name) ->
        SID = {erlang:system_time(microsecond), spawn(fun() -> ok end)},
        JID = mongoose_helper:make_jid(Name, Server, <<"res">>),
        Priority = 0,
        Info = #{},
        distributed_helper:rpc(N, ejabberd_sm, open_session, [HostType, SID, JID, Priority, Info])
        end,
    Names = [<<"user", (list_to_binary((integer_to_list(X))))/binary>> || X <- lists:seq(1, 345)],
    measure("create users", fun() ->
            lists:foreach(CreateUser, Names)
        end),
    %% Check that user3 is properly updated
    %% User should be registered if we want to use mod_last_api
    {ok, _} = distributed_helper:rpc(N, mongoose_account_api, register_user, [<<"user3">>, Server, <<"secret123">>]),
    Jid3 = mongoose_helper:make_jid(<<"user3">>, Server, <<>>),
    {ok, _} = distributed_helper:rpc(N, mod_last_api, set_last, [Jid3, 1714000000, <<"old status">>]),
    {ok, #{timestamp := 1714000000}} = distributed_helper:rpc(N, mod_last_api, get_last, [Jid3]),
    measure("node cleanup", fun() ->
            distributed_helper:rpc(N#{timeout => timer:minutes(1)}, mongoose_hooks, node_cleanup, [node()])
        end),
    {ok, #{timestamp := TS, status := Status} = Data} = distributed_helper:rpc(N, mod_last_api, get_last, [Jid3]),
    ?assertNotEqual(TS, 1714000000, Data),
    ?assertEqual(Status, <<>>, Data),
    distributed_helper:rpc(N, mongoose_metrics, update, [HostType, sessionCount, -345]),
    {ok, _} = distributed_helper:rpc(N, mongoose_account_api, unregister_user, [<<"user3">>, Server]).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------
get_last_activity(Stanza) ->
    S = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"seconds">>}]),
    list_to_integer(binary_to_list(S)).

get_last_status(Stanza) ->
    exml_query:path(Stanza, [{element, <<"query">>}, cdata]).

answer_last_activity(IQ = #xmlel{name = <<"iq">>}) ->
    From = exml_query:attr(IQ, <<"from">>),
    To = exml_query:attr(IQ, <<"to">>),
    Id = exml_query:attr(IQ, <<"id">>),
    #xmlel{name = <<"iq">>,
           attrs = [{<<"from">>, To}, {<<"to">>, From}, {<<"id">>, Id}, {<<"type">>, <<"result">>}],
           children = [#xmlel{name = <<"query">>,
                              attrs = [{<<"xmlns">>, ?NS_LAST_ACTIVITY},
                                       {<<"seconds">>, <<"0">>}]}
                      ]}.

required_modules() ->
    [{mod_last, mod_config_with_auto_backend(mod_last, #{iqdisc => one_queue})}].

measure(Text, F) ->
    {Time, _} = timer:tc(F),
    ct:pal("Time  ~ts = ~p", [Text, Time]),
    ok.
