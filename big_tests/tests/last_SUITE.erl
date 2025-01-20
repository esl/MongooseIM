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
-include_lib("jid/include/jid.hrl").
%% Import LOCATION macro
-include_lib("kernel/include/logger.hrl").

-import(config_parser_helper, [mod_config_with_auto_backend/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, valid_queries},
     {group, invalid_queries},
     {group, sessions_cleanup}].

groups() ->
    [{valid_queries, [sequence], valid_test_cases()},
     {invalid_queries, invalid_test_cases()},
     {sessions_cleanup, [], [sessions_cleanup]}].

valid_test_cases() -> [online_user_query,
                       last_online_user,
                       last_offline_user,
                       last_server].

invalid_test_cases() -> [user_not_subscribed_receives_error].

suite() ->
    distributed_helper:require_rpc_nodes([mim, mim2]) ++ escalus:suite().

init_per_suite(Config0) ->
    mongoose_helper:inject_module(distributed_helper:mim2(), ?MODULE, reload),
    distributed_helper:add_node_to_cluster(distributed_helper:mim2(), Config0),
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
    Config;
init_per_group(sessions_cleanup, Config) ->
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

%% Runs sessions_cleanup and check if it was done by checking last_seen timestamp.
sessions_cleanup(_Config) ->
    %% 345 would test 3 types of insert queries (insert 1, 10, 100).
    %% For load testing, increase NumberOfUsers to big number like 500000
    %% and check "node cleanup" time.
    NumberOfUsers = 345,
    Node1 = distributed_helper:mim(),
    Node2 = distributed_helper:mim2(),
    #{node := Node2Atom} = Node2,
    LongNode1 = Node1#{timeout => timer:minutes(1)},
    LongNode2 = Node2#{timeout => timer:minutes(1)},
    HostType = domain_helper:host_type(),
    Server = domain_helper:domain(),
    OldTS = 1714000000,
    Jid3 = create_user_and_set_lastseen(Node1, Server, <<"user3">>, OldTS),
    Sessions = measure("create users", fun() ->
            distributed_helper:rpc(LongNode2, ?MODULE, create_sessions,
                                   [HostType, Server, NumberOfUsers])
        end),
    NumberOfUsers = length(Sessions),
    measure("node cleanup", fun() ->
            Res = distributed_helper:rpc(LongNode1, mongoose_hooks,
                                         node_cleanup, [Node2Atom]),
            ct:pal("node_cleanup result ~p", [Res])
        end),
    {ok, #{timestamp := TS, status := Status} = Data} =
        distributed_helper:rpc(Node1, mod_last_api, get_last, [Jid3]),
    ?assertNotEqual(TS, OldTS, Data),
    ?assertEqual(Status, <<>>, Data),
    {ok, _} = distributed_helper:rpc(Node1, mongoose_account_api, unregister_user,
                                     [<<"user3">>, Server]),
    measure("close sessions", fun() ->
            distributed_helper:rpc(LongNode2, ?MODULE, close_sessions,
                                   [HostType, Sessions])
        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

create_sessions(HostType, Server, NumberOfUsers) ->
    [create_session(HostType, Server, Num)
     || Num <- lists:seq(1, NumberOfUsers)].

close_sessions(HostType, Sessions) ->
    lists:foreach(fun(Session) -> close_session(HostType, Session) end, Sessions).

create_session(HostType, Server, Num) ->
    Name = <<"user", (list_to_binary((integer_to_list(Num))))/binary>>,
    SID = {erlang:system_time(microsecond), spawn(fun() -> ok end)},
    JID = jid:make(Name, Server, <<"res">>),
    Priority = 0,
    Info = #{},
    ejabberd_sm:open_session(HostType, SID, JID, Priority, Info),
    #{sid => SID, jid => JID}.

close_session(HostType, #{sid := SID, jid := JID}) ->
    #jid{lserver = LServer} = JID,
    Reason = normal,
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             host_type => HostType,
                             lserver => LServer}),
    Info = #{},
    ejabberd_sm:close_session(Acc, SID, JID, Reason, Info).

create_user_and_set_lastseen(Node, Server, User, TS) ->
    Jid = mongoose_helper:make_jid(User, Server, <<>>),
    %% User should be registered if we want to use mod_last_api
    %% Check that user3's last seen timestamp is properly updated
    {ok, _} = distributed_helper:rpc(Node, mongoose_account_api, register_user,
                                     [User, Server, <<"secret123">>]),
    {ok, _} = distributed_helper:rpc(Node, mod_last_api, set_last,
                                     [Jid, TS, <<"old status">>]),
    {ok, #{timestamp := TS}} =
        distributed_helper:rpc(Node, mod_last_api, get_last, [Jid]),
    Jid.

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
           attrs = #{<<"from">> => To, <<"to">> => From, <<"id">> => Id,
                     <<"type">> => <<"result">>},
           children = [#xmlel{name = <<"query">>,
                              attrs = #{<<"xmlns">> => ?NS_LAST_ACTIVITY,
                                        <<"seconds">> => <<"0">>}}
                      ]}.

required_modules() ->
    [{mod_last, mod_config_with_auto_backend(mod_last, #{iqdisc => one_queue})}].

measure(Text, F) ->
    {Time, Val} = timer:tc(F),
    ct:pal("Time  ~ts = ~p", [Text, Time]),
    Val.
