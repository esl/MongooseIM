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
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, valid_queries},
     {group, invalid_queries}].

groups() ->
    G = [{valid_queries, [sequence], valid_test_cases()},
         {invalid_queries, invalid_test_cases()}],
    ct_helper:repeat_all_until_all_ok(G).

valid_test_cases() -> [last_online_user,
                       last_offline_user,
                       last_server ].

invalid_test_cases() -> [user_not_subscribed_receives_error].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(valid_queries, Config0) ->
    Config1 = escalus_fresh:create_users(Config0, [{alice, 1}, {bob, 1}]),
    Config2 = escalus:make_everyone_friends(Config1),
    %% This check ensures that there are no registered sessions.
    %% But in ejabberd_c2s we first unset session,
    %% then broadcast presence unavailable.
    %% This check uses ejabberd_sm to get information about sessions.
    escalus_ejabberd:wait_for_session_count(Config2, 0),
    %% Kick "friendly" users
    %% kick_everyone uses ejabberd_c2s_sup to information about client processes.
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
    escalus:story(Config, [{alice, 1}],
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

        BobFullJID = escalus_client:full_jid(Bob),
        escalus_client:send(Alice, escalus_stanza:last_activity(BobFullJID)),

        Error1 = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Error1),
        ok
    end).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------
get_last_activity(Stanza) ->
    S = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"seconds">>}]),
    list_to_integer(binary_to_list(S)).

get_last_status(Stanza) ->
    exml_query:path(Stanza, [{element, <<"query">>}, cdata]).
