%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Suite for testing s2s connection
%%% @end
%%%===================================================================

-module(s2s_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, s2s_tests}].

all_tests() ->
    [simple_message, nonexist_user, unknown_domain].

groups() ->
    [{s2s_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    NewUsers = ct:get_config(escalus_server2_users) ++ ct:get_config(escalus_users),
    io:format("~p~n", [NewUsers]),
    Config1 = escalus:init_per_suite(Config0),
    escalus_users:create_users(Config1, NewUsers).

end_per_suite(Config) ->
    escalus:delete_users(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% Server-to-server communication test
%%%===================================================================

simple_message(Config) ->
    escalus:story(Config, [1,1], fun(Alice2, Alice1) ->
        %% Alice@localhost1 sends message to Alice@localhost2
        escalus:send(Alice1, escalus_stanza:chat_to(
            get_jid_from_config(Config, alice2), <<"Hi, foreign Alice!">>)),
        %% Alice@localhost2 receives message from Alice@localhost1
        Stanza = escalus:wait_for_stanza(Alice2, 10000),
        escalus_new_assert:assert(is_chat(<<"Hi, foreign Alice!">>), Stanza),
        %% Alice@localhost2 sends message to Alice@localhost1
        escalus:send(Alice2, escalus_stanza:chat_to(alice, <<"Nice to meet you!">>)),
        %% Alice@localhost1 receives message from Alice@localhost2
        Stanza2 = escalus:wait_for_stanza(Alice1, 10000),
        escalus_new_assert:assert(is_chat(<<"Nice to meet you!">>), Stanza2)
    end).

nonexist_user(Config) ->
    escalus:story(Config, [0,1], fun(Alice1) ->
        %% Alice@localhost1 sends message to Xyz@localhost2
        RemoteServer = get_server_from_config(Config, alice2),
        escalus:send(Alice1, escalus_stanza:chat_to(
            <<"xyz@", RemoteServer/binary>>,
            <<"Hello, nonexistent!">>)),
        %% Alice@localhost1 receives stanza error: service-unavailable
        Stanza1 = escalus:wait_for_stanza(Alice1),
        escalus_new_assert:assert(
            fun(Stanza) ->
                escalus_pred:is_error(<<"cancel">>, <<"service-unavailable">>, Stanza)
            end, Stanza1)
    end).

unknown_domain(Config) ->
    escalus:story(Config, [0,1], fun(Alice1) ->
        %% Alice@localhost1 sends message to Xyz@localhost3
        escalus:send(Alice1, escalus_stanza:chat_to(
            <<"xyz@somebogushost">>,
            <<"Hello, unreachable!">>)),
        %% Alice@localhost1 receives stanza error: remote-server-not-found
        Stanza1 = escalus:wait_for_stanza(Alice1),
        escalus_new_assert:assert(
            fun(Stanza) ->
                escalus_pred:is_error(<<"cancel">>, <<"remote-server-not-found">>, Stanza)
            end, Stanza1)
    end).

%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

%%%===================================================================
%%% Helpers
%%%===================================================================

get_jid_from_config(Config, Name) ->
    {value, {escalus_users, UserList}} = lists:keysearch(escalus_users, 1, Config),
    {value, {Name, UserSpec}} = lists:keysearch(Name, 1, UserList),
    {value, {username, Username}} = lists:keysearch(username, 1, UserSpec),
    {value, {server, Server}} = lists:keysearch(server, 1, UserSpec),
    <<Username/binary, "@", Server/binary>>.
get_server_from_config(Config, Name) ->
    {value, {escalus_users, UserList}} = lists:keysearch(escalus_users, 1, Config),
    {value, {Name, UserSpec}} = lists:keysearch(Name, 1, UserList),
    {value, {server, Server}} = lists:keysearch(server, 1, UserSpec),
    Server.
