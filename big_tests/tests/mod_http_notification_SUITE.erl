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

-define(ETS_TABLE, mod_http_notification).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-import(push_helper, [http_notifications_port/0, http_notifications_host/0]).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [
     {group, no_prefix},
     {group, with_prefix}
    ].

all_tests() ->
    [
     simple_message,
     simple_message_no_listener,
     simple_message_failing_listener,
     proper_http_message_encode_decode
    ].

groups() ->
    G = [{no_prefix, [sequence], all_tests()},
         {with_prefix, [sequence], all_tests()}],
    ct_helper:repeat_all_until_all_ok(G).

init_per_suite(Config0) ->
    escalus:init_per_suite(Config0).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(no_prefix, Config) ->
    set_modules(Config, []);
init_per_group(with_prefix, Config) ->
    set_modules(Config, [{path, "/prefix"}]).

end_per_group(_GroupName, Config) ->
    dynamic_modules:restore_modules(host(), Config),
    ok.

init_per_testcase(CaseName, Config) ->
    create_events_collection(),
    start_http_listener(CaseName, get_prefix(Config)),
    start_pool(),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    stop_pool(),
    stop_http_listener(CaseName),
    clear_events_collection(),
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% offline tests
%%%===================================================================
proper_http_message_encode_decode(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Sender = jid:nameprep(escalus_client:username(Alice)),
            Receiver = jid:nameprep(escalus_client:username(Bob)),
            Server = jid:nodeprep(escalus_users:get_host(Config, alice)),
            Message = <<"Hi Test!&escape=Hello">>,

            Stanza = escalus_stanza:chat_to(Bob, Message),
            escalus:send(Alice, Stanza),
            escalus:wait_for_stanza(Bob),

            Body = get_http_request(),

            ExtractedAndDecoded = rpc(mim(), cow_qs, parse_qs, [Body]),
            ExpectedList = [{<<"author">>,<<Sender/binary>>},
                            {<<"server">>,<<Server/binary>>},
                            {<<"receiver">>,<<Receiver/binary>>},
                            {<<"message">>,<<Message/binary>>}],
            SortedExtractedAndDecoded = lists:sort(ExtractedAndDecoded),
            SortedExpectedList = lists:sort(ExpectedList),
            ?assertEqual(SortedExpectedList, SortedExtractedAndDecoded)
        end).


simple_message(Config) ->
    %% we expect one notification message
    do_simple_message(Config, <<"Hi, Simple!">>),
    %% fail if we didn't receive http notification
    Body = get_http_request(),
    {_, _} = binary:match(Body, <<"alice">>),
    {_, _} = binary:match(Body, <<"Simple">>).

simple_message_no_listener(Config) ->
    do_simple_message(Config, <<"Hi, NoListener!">>).

simple_message_failing_listener(Config) ->
    do_simple_message(Config, <<"Hi, Failing!">>).

do_simple_message(Config0, Msg) ->
    Config = escalus_fresh:create_users(Config0, [{alice, 1}, {bob, 1}]),
    %% Alice sends a message to Bob, who is offline
    {ok, Alice} = escalus_client:start(Config, alice, <<"res1">>),
    escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
    BobJid = escalus_users:get_jid(Config, bob),
    Stanza = escalus_stanza:chat_to(BobJid, Msg),
    escalus:send(Alice, Stanza),
    escalus_client:stop(Config, Alice),
    %% Bob logs in
    {ok, Bob} = escalus_client:start(Config, bob, <<"res1">>),
    escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
    %% He receives his initial presence and the message
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
    escalus_new_assert:mix_match([is_presence, is_chat(Msg)], Stanzas),
    escalus_client:stop(Config, Bob).

%%%===================================================================
%%% Helpers
%%%===================================================================

get_http_request() ->
    Key = got_http_request,
    mongoose_helper:wait_until(
      fun() -> 1 =:= length(ets:lookup(?ETS_TABLE, Key)) end,
      true, #{name => missing_request}),
    [Bins] = lists:map(fun({_, El}) -> El end, ets:lookup(?ETS_TABLE, Key)),
    ets:delete(?ETS_TABLE, Key),
    Bins.

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

get_prefix(no_prefix) ->
    "/";
get_prefix(with_prefix) ->
    "/prefix";
get_prefix(Config) ->
    GroupName = proplists:get_value(name, proplists:get_value(tc_group_properties, Config)),
    get_prefix(GroupName).

start_pool() ->
    HTTPOpts = [{server, http_notifications_host()}],
    PoolOpts = [{strategy, available_worker}, {workers, 5}],
    ejabberd_node_utils:call_fun(mongoose_wpool, start_configured_pools,
                                 [[{http, global, http_pool, PoolOpts, HTTPOpts}]]).

stop_pool() ->
    ejabberd_node_utils:call_fun(mongoose_wpool, stop, [http, global, http_pool]).

set_modules(Config0, Opts) ->
    Config = dynamic_modules:save_modules(host(), Config0),
    ModOpts = [{worker_timeout, 500}, {host, http_notifications_host()}] ++ Opts,
    dynamic_modules:ensure_modules(host(), [{mod_http_notification, ModOpts}]),
    Config.

start_http_listener(simple_message, Prefix) ->
    http_helper:start(http_notifications_port(), Prefix, fun process_notification/1);
start_http_listener(simple_message_no_listener, _) ->
    ok;
start_http_listener(simple_message_failing_listener, Prefix) ->
    http_helper:start(http_notifications_port(), Prefix, fun(Req) -> Req end);
start_http_listener(proper_http_message_encode_decode, Prefix) ->
    http_helper:start(http_notifications_port(), Prefix, fun process_notification/1).

stop_http_listener(simple_message_no_listener) ->
    ok;
stop_http_listener(_) ->
    http_helper:stop().

process_notification(Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req1),
    Event = {got_http_request, Body},
    ets:insert(?ETS_TABLE, Event),
    Req2.

create_events_collection() ->
    ets:new(?ETS_TABLE, [duplicate_bag, named_table, public]).

clear_events_collection() ->
    ets:delete_all_objects(?ETS_TABLE).

host() ->
    ct:get_config({hosts, mim, domain}).
