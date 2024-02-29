-module(push_http_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_TABLE, push_http).

-import(push_helper, [http_notifications_port/0, http_notifications_host/0]).
-import(domain_helper, [domain/0]).
-import(config_parser_helper, [config/2, mod_event_pusher_http_handler/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, single},
        {group, customised},
        {group, multiple}
    ].

groups() ->
    [{single, [sequence], [simple_push]},
     {customised, [sequence], [custom_push]},
     {multiple, [sequence], [push_to_many]}
    ].

suite() ->
    distributed_helper:require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    start_http_listener(),
    start_pool(),
    setup_modules(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    stop_pool(),
    stop_http_listener(),
    teardown_modules(),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), required_modules(GroupName)),
    escalus:create_users(Config2, escalus:get_users([alice, bob])).

end_per_group(_, Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    create_events_collection(),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    clear_events_collection(),
    escalus:end_per_testcase(CaseName, Config).

required_modules(GroupName) ->
    [{mod_event_pusher, #{http => #{handlers => push_http_handler_opts(GroupName)}}}].

push_http_handler_opts(GroupName) ->
    BasicOpts = mod_event_pusher_http_handler(),
    [maps:merge(BasicOpts, ExtraOpts) || ExtraOpts <- push_http_handler_extra_opts(GroupName)].

push_http_handler_extra_opts(single) ->
    [#{path => <<"push">>}];
push_http_handler_extra_opts(customised) ->
    [#{path => <<"push">>, callback_module => mod_event_pusher_http_custom}];
push_http_handler_extra_opts(multiple) ->
    [#{path => <<"push">>, callback_module => mod_event_pusher_http_custom},
     #{path => <<"push2">>, callback_module => mod_event_pusher_http_custom_2}].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

simple_push(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            send(Alice, Bob, <<"hej">>),
            [R] = got_push(push, 1),
            check_default_format(Alice, Bob, <<"hej">>, R),
            send(Alice, Bob, <<>>),
            got_no_push(push),
            ok
        end).

custom_push(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            send(Alice, Bob, <<"hej">>),
            send(Alice, Bob, <<>>),
            % now we receive them both ways, and with a custom body
            Res = got_push(push, 4),
            ?assertEqual([<<"in-">>,<<"in-hej">>,<<"out-">>,<<"out-hej">>], lists:sort(Res)),
            ok
        end).

push_to_many(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            send(Alice, Bob, <<"hej">>),
            send(Alice, Bob, <<>>),
            % now we receive them both ways, and with a custom body
            Res = got_push(push, 4),
            ?assertEqual([<<"in-">>,<<"in-hej">>,<<"out-">>,<<"out-hej">>], lists:sort(Res)),
            % while the other backend sends only those 'out'
            Res2 = got_push(push2, 2),
            ?assertEqual([<<"2-out-">>,<<"2-out-hej">>], lists:sort(Res2)),
            ok
        end).

%%--------------------------------------------------------------------
%% Libs
%%--------------------------------------------------------------------

got_no_push(Type) ->
    ?assertEqual(0, length(ets:lookup(?ETS_TABLE, {got_http_push, Type})), unwanted_push).

got_push(Type, Count)->
    Key = {got_http_push, Type},
    mongoose_helper:wait_until(
      fun() -> length(ets:lookup(?ETS_TABLE, Key)) end,
      Count, #{name => http_request_timeout}),
    Bins = lists:map(fun({_, El}) -> El end, ets:lookup(?ETS_TABLE, Key)),
    ?assertEqual(Count, length(Bins)), % Assert that this didn't magically grow in the meantime
    ets:delete(?ETS_TABLE, Key),
    Bins.

create_events_collection() ->
    ets:new(?ETS_TABLE, [duplicate_bag, named_table, public]).

clear_events_collection() ->
    ets:delete_all_objects(?ETS_TABLE).

start_http_listener() ->
    http_helper:start(http_notifications_port(), '_', fun process_notification/1).

stop_http_listener() ->
    http_helper:stop().

process_notification(Req) ->
    <<$/, BType/binary>> = cowboy_req:path(Req),
    Type = binary_to_atom(BType, utf8),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req1),
    Event = {{got_http_push, Type}, Body},
    ets:insert(?ETS_TABLE, Event),
    Req2.

check_default_format(From, To, Body, Msg) ->
    Attrs = lists:map(fun(P) -> list_to_tuple(binary:split(P, <<$=>>)) end, binary:split(Msg, <<$&>>, [global])),
    ?assertEqual(to_lower(escalus_client:username(From)), proplists:get_value(<<"author">>, Attrs)),
    ?assertEqual(to_lower(escalus_client:username(To)), proplists:get_value(<<"receiver">>, Attrs)),
    ?assertEqual(Body, proplists:get_value(<<"message">>, Attrs)),
    ?assertEqual(<<"localhost">>, proplists:get_value(<<"server">>, Attrs)),
    ok.

start_pool() ->
    PoolOpts = #{strategy => random_worker, call_timeout => 5000, workers => 10},
    ConnOpts = #{host => http_notifications_host(), request_timeout => 5000},
    Pool = config([outgoing_pools, http, http_pool],
                  #{scope => host_type, opts => PoolOpts, conn_opts => ConnOpts}),
    [{ok, _Pid}] = rpc(mongoose_wpool, start_configured_pools, [[Pool], [<<"localhost">>]]).

stop_pool() ->
    rpc(mongoose_wpool, stop, [http, <<"localhost">>, http_pool]).

setup_modules() ->
    {Mod, Code} = rpc(dynamic_compile, from_string, [custom_module_code()]),
    rpc(code, load_binary, [Mod, "mod_event_pusher_http_custom.erl", Code]),
    {Mod2, Code2} = rpc(dynamic_compile, from_string, [custom_module_code_2()]),
    rpc(code, load_binary, [Mod2, "mod_event_pusher_http_custom_2.erl", Code2]),
    ok.

teardown_modules() ->
    ok.

rpc(M, F, A) ->
    distributed_helper:rpc(distributed_helper:mim(), M, F, A).

custom_module_code() ->
    "-module(mod_event_pusher_http_custom).
     -export([should_make_req/6, prepare_body/7, prepare_headers/7]).
     should_make_req(Acc, _, _, _, _, _) ->
         case mongoose_acc:stanza_name(Acc) of
             <<\"message\">> -> true;
             _ -> false
         end.
     prepare_headers(_, _, _, _, _, _, _) ->
         mod_event_pusher_http_defaults:prepare_headers(x, x, x, x, x, x, x).
     prepare_body(_Acc, Dir, _Host, Message, _Sender, _Receiver, _Opts) ->
         <<(atom_to_binary(Dir, utf8))/binary, $-, Message/binary>>.
     "
.

custom_module_code_2() ->
    "-module(mod_event_pusher_http_custom_2).
     -export([should_make_req/6, prepare_body/7, prepare_headers/7]).
     should_make_req(Acc, out, _, _, _, _) ->
         case mongoose_acc:stanza_name(Acc) of
             <<\"message\">> -> true;
             _ -> false
         end;
     should_make_req(_, in, _, _, _, _) -> false.
     prepare_headers(_, _, _, _, _, _, _) ->
         mod_event_pusher_http_defaults:prepare_headers(x, x, x, x, x, x, x).
     prepare_body(_Acc, Dir, _Host, Message, _Sender, _Receiver, _Opts) ->
         <<$2, $-, (atom_to_binary(Dir, utf8))/binary, $-, Message/binary>>.
     "
    .

to_lower(B) ->
    list_to_binary(
        string:to_lower(
            binary_to_list(
                B
            )
        )
    ).

send(Alice, Bob, Body) ->
    Stanza = escalus_stanza:chat_to(Bob, Body),
    escalus_client:send(Alice, Stanza).
