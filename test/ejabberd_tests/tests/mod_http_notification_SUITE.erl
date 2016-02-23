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
  [simple_message].

groups() ->
  [{mod_http_notification_tests, [sequence], all_tests()}].


suite() ->
  escalus:suite().

init_per_suite(Config0) ->
  Config1 = escalus:init_per_suite(Config0),
  escalus:create_users(Config1, {by_name, [alice, bob]}).


end_per_suite(Config) ->
  escalus:delete_users(Config, {by_name, [alice, bob]}),
  escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
%%  start_mod_http_notification ([
%%    {host, "http://localhost:8000"},
%%    {prefix_path, "/test"},
%%    {pool_size, 5}]),
  Config.

end_per_group(_GroupName, _Config) ->
%%  Domain = ct:get_config(ejabberd_domain),
%%  dynamic_modules:stop(Domain, mod_http_notification),
  ok.

init_per_testcase(CaseName, Config) ->
  escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
  escalus:end_per_testcase(CaseName, Config).

start_mod_http_notification(Opts)->
  Domain = ct:get_config(ejabberd_domain),
  dynamic_modules:start(Domain, mod_http_notification, Opts).


%%%===================================================================
%%% offline tests
%%%===================================================================

assert_true(_, true) ->
  ok;
assert_true(Name, _) ->
  ct:fail("~p is not true, while should be", [Name]).


listen_once(PPid) ->
  spawn(fun() -> onetime_http_server(PPid) end).

onetime_http_server(PPid) ->
  %% a crappy hand-crafted listener with everything hardcoded
  {ok, LSock} = gen_tcp:listen(8000, [binary, {packet, 0},
    {active, false}, {reuseaddr, true}]),
  {ok, Sock} = gen_tcp:accept(LSock),
  do_recv(Sock, [], PPid),
  gen_tcp:close(Sock).

do_recv(Sock, Bs, PPid) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
      case erlang:decode_packet(http, B, []) of
        {ok, {http_request, 'POST', _, _}, _} ->
          Resp = "HTTP/1.1 200 OK\r\nDate: Tue, 23 Feb 2016 14:59:37 GMT\r\nContent-Length: 2\r\nContent-Type: text/html\r\nServer: TwistedWeb/12.2.0\r\n\r\nOK",
          gen_tcp:send(Sock, Resp),
          PPid ! {ok, got_it},
          ok;
        _ ->
          do_recv(Sock, [Bs, B], PPid)
      end;
    {error, closed} ->
      ok
  end.

simple_message(Config) ->
  %% we expect one notification message
  listen_once(self()),
  %% Alice sends a message to Bob, who is offline
  escalus:story(Config, [{alice, 1}], fun(Alice) ->
    escalus:send(Alice, escalus_stanza:chat_to(bob, <<"Hi, Offline!">>))
                                      end),

  %% Bob logs in
  Bob = login_send_presence(Config, bob),

  %% He receives his initial presence and the message
  Stanzas = escalus:wait_for_stanzas(Bob, 2),
  escalus_new_assert:mix_match([is_presence,
    is_chat(<<"Hi, Offline!">>)],
    Stanzas),
  %% fail if we didn't receive http notification
  Notified = receive
    {ok, got_it} ->
      true
  after 2000 ->
    false
  end,
  assert_true("notified", Notified),
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
