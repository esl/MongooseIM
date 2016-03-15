%%%----------------------------------------------------------------------
%%% File    : mod_http_notification_SUITE
%%% Author  : Baibossynv Valery <baibossynov.valery@gmail.com>
%%% Purpose : Testing passing via http
%%% Created : 16 Dec 2015 by Baibossynv Valery <baibossynov.valery@gmail.com>
%%%----------------------------------------------------------------------

-module(mod_http_notification_SUITE).
-author("baibossynov.valery@gmail.com").

-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").


%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, mod_http_notification_tests}].

all_tests() ->
    [simple_message, simple_message_no_listener, simple_message_failing_listener].

groups() ->
    [{mod_http_notification_tests, [sequence], all_tests()}].


suite() ->
    escalus:suite().

set_worker_timeout() ->
    dynamic_modules:restart(host(), mod_http_notification, [{worker_timeout, 500}, {host, "http://localhost:8000"}]),
    ok.

host() -> <<"localhost">>.

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    set_worker_timeout(),
    escalus:create_users(Config1, {by_name, [alice, bob]}).

end_per_suite(Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

start_mod_http_notification(Opts) ->
    Domain = ct:get_config(ejabberd_domain),
    dynamic_modules:start(Domain, mod_http_notification, Opts).


%%%===================================================================
%%% offline tests
%%%===================================================================

simple_message(Config) ->
    %% we expect one notification message
    http_helper:listen_once(self(), 8000, [<<"alice">>, <<"Simple">>]),
    do_simple_message(Config, <<"Hi, Simple!">>),
    %% fail if we didn't receive http notification
    Notified = receive
                   {ok, got_http_request, _} ->
                       true
               after 2000 ->
                       false
               end,
    assert_true("notified", Notified).

simple_message_no_listener(Config) ->
    do_simple_message(Config, <<"Hi, NoListener!">>).

simple_message_failing_listener(Config) ->
    http_helper:listen_once(self(), 8000, none, none),
    do_simple_message(Config, <<"Hi, Failing!">>).

do_simple_message(Config, Msg) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}],
        fun(Alice) -> escalus:send(Alice, escalus_stanza:chat_to(bob, Msg)) end),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and the message
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
%%  ct:pal("Stanzas:~p", [Stanzas]),
    escalus_new_assert:mix_match([is_presence, is_chat(Msg)], Stanzas),
    escalus_cleaner:clean(Config).


%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

%%%===================================================================
%%% Helpers
%%%===================================================================

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.


assert_true(_, true) ->
    ok;
assert_true(Name, _) ->
    ct:fail("~p is not true, while should be", [Name]).
