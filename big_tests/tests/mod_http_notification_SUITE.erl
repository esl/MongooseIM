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
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-import(push_helper, [http_notifications_port/0, http_notifications_host/0]).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [
        {group, mod_http_notification_tests},
        {group, mod_http_notification_tests_with_prefix}
    ].

all_tests() ->
    [simple_message, simple_message_no_listener, simple_message_failing_listener, proper_http_message_encode_decode].

groups() ->
    G = [{mod_http_notification_tests, [sequence], all_tests()},
         {mod_http_notification_tests_with_prefix, [sequence], all_tests()}],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

set_worker(Config) ->
    set_modules(Config, [{worker_timeout, 500}, {host, http_notifications_host()}]).

set_worker(Config, Prefix) ->
    set_modules(Config, [{worker_timeout, 500}, {host, http_notifications_host()}, {path, Prefix}]).

set_modules(Config0, Opts) ->
    Config = dynamic_modules:save_modules(host(), Config0),
    HTTPOpts = [{server, http_notifications_host()}],
    PoolOpts = [{strategy, available_worker}, {workers, 20}],
    ejabberd_node_utils:call_fun(mongoose_wpool, start_configured_pools,
                                 [[{http, global, http_pool, PoolOpts, HTTPOpts}]]),
    dynamic_modules:ensure_modules(host(), [{mod_http_notification, Opts}]),
    Config.

host() -> ct:get_config({hosts, mim, domain}).

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1, escalus:get_users([alice, bob])).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])),
    escalus:end_per_suite(Config).

init_per_group(mod_http_notification_tests, Config) ->
    set_worker(Config);
init_per_group(mod_http_notification_tests_with_prefix, Config) ->
    set_worker(Config, "/prefix").

end_per_group(_GroupName, Config) ->
    ejabberd_node_utils:call_fun(mongoose_wpool, stop, [http, global, http_pool]),
    dynamic_modules:restore_modules(host(), Config),
    ok.

init_per_testcase(CaseName, Config) ->
    start_http_listener(CaseName, get_prefix(Config)),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    http_helper:stop(),
    escalus:end_per_testcase(CaseName, Config).

start_http_listener(simple_message, Prefix) ->
    Pid = self(),
    http_helper:start(http_notifications_port(), Prefix, fun(Req) -> process_notification(Req, Pid) end);
start_http_listener(simple_message_no_listener, _) ->
    ok;
start_http_listener(simple_message_failing_listener, Prefix) ->
    http_helper:start(http_notifications_port(), Prefix, fun(Req) -> Req end);
start_http_listener(proper_http_message_encode_decode, Prefix) ->
    Pid = self(),
    http_helper:start(http_notifications_port(), Prefix, fun(Req) -> process_notification(Req, Pid) end).

process_notification(Req, Pid) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req1),
    Pid ! {got_http_request, Body},
    Req2.

%%%===================================================================
%%% offline tests
%%%===================================================================
proper_http_message_encode_decode(Config) ->
    Receiver= <<"bob">>,
    Message = <<"Hi Test!&escape=Hello">>,
    Sender = <<"alice">>,
    Server = escalus_users:get_host(Config, alice),
    BobJid = escalus_users:get_jid(Config, bob),

    escalus:story(Config, [{alice, 1}, {bob,1}],
        fun(Alice, Bob) ->
            escalus:send(Alice, escalus_stanza:chat_to(BobJid, Message)),
            escalus:wait_for_stanzas(Bob, 2)
        end),

    Body = receive
               {got_http_request, Bin} ->
                   Bin
           after 5000 ->
                ct:fail(http_request_timeout)
           end,
    ExtractedAndDecoded = rpc(mim(), cow_qs, parse_qs, [Body]),
    ExpectedList = [{<<"author">>,<<Sender/binary>>},
        {<<"server">>,<<Server/binary>>},
        {<<"receiver">>,<<Receiver/binary>>},
        {<<"message">>,<<Message/binary>>}],
    SortedExtractedAndDecoded = lists:sort(ExtractedAndDecoded),
    SortedExpectedList = lists:sort(ExpectedList),
    ?assertEqual(SortedExpectedList, SortedExtractedAndDecoded).

simple_message(Config) ->
    %% we expect one notification message
    do_simple_message(Config, <<"Hi, Simple!">>),
    %% fail if we didn't receive http notification
    Body = receive
               {got_http_request, Bin} -> Bin
           after 2000 ->
            error(missing_request)
           end,

    {_, _} = binary:match(Body, <<"alice">>),
    {_, _} = binary:match(Body, <<"Simple">>).

simple_message_no_listener(Config) ->
    do_simple_message(Config, <<"Hi, NoListener!">>).

simple_message_failing_listener(Config) ->
    do_simple_message(Config, <<"Hi, Failing!">>).

do_simple_message(Config, Msg) ->
    BobJid = escalus_users:get_jid(Config, bob),

    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}],
        fun(Alice) -> escalus:send(Alice, escalus_stanza:chat_to(BobJid, Msg)) end),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and the message
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
%%  ct:pal("Stanzas:~p", [Stanzas]),
    escalus_new_assert:mix_match([is_presence, is_chat(Msg)], Stanzas),
    escalus_client:stop(Config, Bob).


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

get_prefix(mod_http_notification_tests) ->
    "/";
get_prefix(mod_http_notification_tests_with_prefix) ->
    "/prefix";
get_prefix(Config) ->
    GroupName = proplists:get_value(name, proplists:get_value(tc_group_properties, Config)),
    get_prefix(GroupName).
