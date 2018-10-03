-module(push_http_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").


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
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    start_pool(),
    setup_modules(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    stop_pool(),
    teardown_modules(),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(),
                                   required_modules(GroupName)),
    escalus:create_users(Config2, escalus:get_users([alice, bob])).

end_per_group(_, Config) ->
    dynamic_modules:restore_modules(domain(), Config),
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    start_http_listener(),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    stop_http_listener(),
    escalus:end_per_testcase(CaseName, Config).

required_modules(single) ->
    [{mod_event_pusher,
        [{backends,
            [{http,
                [{path, "/push"},
                    {pool_name, http_pool}]
                }]
        }]
    }];
required_modules(customised) ->
    [{mod_event_pusher,
        [{backends,
            [{http,
                [{path, "/push"},
                    {callback_module, mod_event_pusher_http_custom},
                    {pool_name, http_pool}]
                }]
        }]
    }];
required_modules(multiple) ->
    [{mod_event_pusher,
        [{backends,
            [{http,
                [{path, "/push"},
                 {callback_module, mod_event_pusher_http_custom},
                 {pool_name, http_pool}]
             },
             {http,
                [{path, "/push2"},
                 {callback_module, mod_event_pusher_http_custom_2},
                 {pool_name, http_pool}]
            }]
        }]
     }].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

simple_push(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Send = fun(Body) ->
                       Stanza = escalus_stanza:chat_to(Bob, Body),
                       escalus_client:send(Alice, Stanza)
                   end,
            Send(<<"hej">>),
            [R] = got_push(push, 1),
            check_default_format(Alice, Bob, <<"hej">>, R),
            Send(<<>>),
            got_no_push(push),
            ok
        end).

custom_push(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Send = fun(Body) ->
                       Stanza = escalus_stanza:chat_to(Bob, Body),
                ct:pal("Stanza: ~p", [Stanza]),
                       escalus_client:send(Alice, Stanza)
                   end,
            Send(<<"hej">>),
            Send(<<>>),
%%            now we receive them both ways, and with a custom body
            Res = got_push(push, 4),
            ?assertEqual([<<"in-">>,<<"in-hej">>,<<"out-">>,<<"out-hej">>], lists:sort(Res)),
            ok
        end).

push_to_many(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Send = fun(Body) ->
                       Stanza = escalus_stanza:chat_to(Bob, Body),
                       escalus_client:send(Alice, Stanza)
                   end,
            Send(<<"hej">>),
            Send(<<>>),
%%            now we receive them both ways, and with a custom body
            Res = got_push(push, 4),
            ?assertEqual([<<"in-">>,<<"in-hej">>,<<"out-">>,<<"out-hej">>], lists:sort(Res)),
%%            while the other backend sends only those 'out'
            Res2 = got_push(push2, 2),
            ?assertEqual([<<"2-out-">>,<<"2-out-hej">>], lists:sort(Res2)),
            ok
        end).

start_pool() ->
    ejabberd_node_utils:call_fun(mongoose_http_client, start, [[]]),
    ejabberd_node_utils:call_fun(mongoose_http_client,
        start_pool,
        [http_pool, [{server, "http://localhost:8000"}]]),
    ok.

stop_pool() ->
    ejabberd_node_utils:call_fun(mongoose_http_client, stop_pool, [http_pool]),
    ejabberd_node_utils:call_fun(mongoose_http_client, stop, []),
    ok.

%%--------------------------------------------------------------------
%% Libs
%%--------------------------------------------------------------------

receive_push(Type) ->
    receive
        {{got_http_push, Type}, Bin} ->
            ct:pal("{Type, Bin}: ~p", [{Type, Bin}]),
            Bin
    after 1000 ->
        nothing
    end.

got_push(Type) ->
    case receive_push(Type) of
        nothing -> ct:fail(http_request_timeout);
        Bin -> Bin
    end.

got_no_push(Type) ->
    case receive_push(Type) of
        nothing -> ok;
        _ -> ct:fail(unwanted_push)
    end.

got_push(Type, Count)->
    got_push(Type, Count, []).

got_push(Type, 0, Res) ->
    got_no_push(Type),
    lists:reverse(Res);
got_push(Type, N, Res) ->
    got_push(Type, N - 1, [got_push(Type) | Res]).


start_http_listener() ->
    Pid = self(),
    http_helper:start(8000, '_', fun(Req) -> process_notification(Req, Pid) end).

stop_http_listener() ->
    http_helper:stop().

process_notification(Req, Pid) ->
    <<$/, BType/binary>> = cowboy_req:path(Req),
    Type = binary_to_atom(BType, utf8),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req1),
    Pid ! {{got_http_push, Type}, Body},
    Req2.

check_default_format(From, To, Body, Msg) ->
    Attrs = lists:map(fun(P) -> list_to_tuple(binary:split(P, <<$=>>)) end, binary:split(Msg, <<$&>>, [global])),
    ?assertEqual(to_lower(escalus_client:username(From)), proplists:get_value(<<"author">>, Attrs)),
    ?assertEqual(to_lower(escalus_client:username(To)), proplists:get_value(<<"receiver">>, Attrs)),
    ?assertEqual(Body, proplists:get_value(<<"message">>, Attrs)),
    ?assertEqual(<<"localhost">>, proplists:get_value(<<"server">>, Attrs)),
    ok.

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
     -export([should_make_req/5, prepare_body/6, prepare_headers/6]).
     should_make_req(Acc, _, _, _, _) ->
         case mongoose_acc:stanza_name(Acc) of
             <<\"message\">> -> true;
             _ -> false
         end.
     prepare_headers(_, _, _, _, _, _) ->
         mod_event_pusher_http_defaults:prepare_headers(x, x, x, x, x, x).
     prepare_body(_Acc, Dir, _Host, Message, _Sender, _Receiver) ->
         <<(atom_to_binary(Dir, utf8))/binary, $-, Message/binary>>.
     "
.

custom_module_code_2() ->
    "-module(mod_event_pusher_http_custom_2).
     -export([should_make_req/5, prepare_body/6, prepare_headers/6]).
     should_make_req(Acc, out, _, _, _) ->
         case mongoose_acc:stanza_name(Acc) of
             <<\"message\">> -> true;
             _ -> false
         end;
     should_make_req(_, in, _, _, _) -> false.
     prepare_headers(_, _, _, _, _, _) ->
         mod_event_pusher_http_defaults:prepare_headers(x, x, x, x, x, x).
     prepare_body(_Acc, Dir, _Host, Message, _Sender, _Receiver) ->
         <<$2, $-, (atom_to_binary(Dir, utf8))/binary, $-, Message/binary>>.
     "
    .

domain() ->
    ct:get_config({hosts, mim, domain}).

to_lower(B) ->
    list_to_binary(
        string:to_lower(
            binary_to_list(
                B
            )
        )
    ).
