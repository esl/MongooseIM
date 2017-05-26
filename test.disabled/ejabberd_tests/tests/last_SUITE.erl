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
    [{group, last}].

groups() ->
     [{last, [sequence], test_cases()}].

test_cases() -> [last_online_user,
                 last_offline_user,
                 last_server].
suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config0) ->
    Config1 = escalus:create_users(Config0, escalus:get_users([alice, bob])),
    Config2 = escalus:make_everyone_friends(Config1),
    escalus_ejabberd:wait_for_session_count(Config2, 0),
    Config2.

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Last tests
%%--------------------------------------------------------------------
last_online_user(Config) ->
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
    escalus:story(Config, [{alice, 1}],
                  fun(Alice) ->
                          %% Bob logs in
                          {ok, Bob} = escalus_client:start_for(Config, bob, <<"bob">>),

                          %% Bob logs out with a status
                          Status = escalus_stanza:tags([{<<"status">>, <<"I am a banana!">>}]),
                          Presence = escalus_stanza:presence(<<"unavailable">>, Status),
                          escalus_client:send(Bob, Presence),
                          escalus_client:stop(Bob),
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

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------
get_last_activity(Stanza) ->
    S = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"seconds">>}]),
    list_to_integer(binary_to_list(S)).

get_last_status(Stanza) ->
    exml_query:path(Stanza, [{element, <<"query">>}, cdata]).
