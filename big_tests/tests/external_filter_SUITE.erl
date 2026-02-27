-module(external_filter_SUITE).

-author('pawel.dlugosz@erlang-solutions.com').

-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, pm}, {group, muc_light}, {group, graphql_user}, {group, graphql_admin}].

groups() ->
    [{pm, [parallel], pm_test_cases()},
     {muc_light, [parallel], muc_light_test_cases()},
     {graphql_user, [parallel], graphql_user_test_cases()},
     {graphql_admin, [parallel], graphql_admin_test_cases()}].

pm_test_cases() ->
    [allow_appropriate_message,
     block_inappropriate_message,
     allow_message_on_request_error,
     allow_non_body_messages,
     allow_message_on_external_error].

muc_light_test_cases() ->
    [allow_appropriate_message_muclight,
     block_inappropriate_message_muclight,
     allow_message_on_request_error_muclight].

graphql_user_test_cases() ->
    [filter_messages_sent_via_client_api].

graphql_admin_test_cases() ->
    [dont_filter_messages_sent_via_admin_api].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, module_config()),
    start_mock(),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    stop_mock(),
    dynamic_modules:restore_modules(Config),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(muc_light, Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, muc_light_config()),
    Config1;
init_per_group(graphql_user, Config) ->
    graphql_helper:init_user(Config);
init_per_group(graphql_admin, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(_, Config) ->
    Config.

end_per_group(muc_light, Config) ->
    dynamic_modules:restore_modules(Config),
    Config;
end_per_group(GN, Config) when GN =:= graphql_user; GN =:= graphql_admin ->
    graphql_helper:clean(),
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

module_config() ->
    [{mod_external_filter,
      config_parser_helper:mod_config(mod_external_filter, #{pool_tag => external_filter})},
     {mod_stanzaid, #{}}].

muc_light_config() ->
    [{mod_muc_light, config_parser_helper:mod_config_with_auto_backend(mod_muc_light)}].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

allow_appropriate_message(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"hello">>,
                           subscribe_request(Message, allow_response()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           wait_for_request(Message, 1000),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_chat_message, [Message], R)
                        end).

block_inappropriate_message(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"send me money">>,
                           subscribe_request(Message, block_response()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           wait_for_request(Message, 1000),
                           ?assertNot(escalus_client:has_stanzas(Bob))
                        end).

allow_message_on_request_error(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"hi there">>,
                           subscribe_request(Message, error_response()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           wait_for_request(Message, 1000),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_chat_message, [Message], R)
                        end).

allow_non_body_messages(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           MarkerName = <<"displayed">>,
                           MessageId = <<"some_id">>,
                           escalus:send(Alice,
                                        escalus_stanza:chat_marker(Bob, MarkerName, MessageId)),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_chat_marker, [MarkerName, MessageId], R)
                        end).

allow_message_on_external_error(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"is something wrong?">>,
                           subscribe_request(Message, server_error()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           wait_for_request(Message, 1000),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_chat_message, [Message], R)
                        end).

allow_appropriate_message_muclight(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           RoomName = muc_helper:fresh_room_name(),
                           muc_light_helper:create_room(RoomName,
                                                        muc_light_helper:muc_host(),
                                                        Alice,
                                                        [Bob],
                                                        Config,
                                                        muc_light_helper:ver(1)),
                           Message = <<"hello people">>,
                           MucHost = muc_light_helper:muc_host(),
                           RoomAddr = <<RoomName/binary, "@", MucHost/binary>>,
                           subscribe_request(Message, allow_response()),
                           escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Message)),
                           wait_for_request(Message, 1000),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_groupchat_message, [Message], R)
                        end).

block_inappropriate_message_muclight(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           RoomName = muc_helper:fresh_room_name(),
                           muc_light_helper:create_room(RoomName,
                                                        muc_light_helper:muc_host(),
                                                        Alice,
                                                        [Bob],
                                                        Config,
                                                        muc_light_helper:ver(1)),
                           Message = <<"send me your credentials">>,
                           MucHost = muc_light_helper:muc_host(),
                           RoomAddr = <<RoomName/binary, "@", MucHost/binary>>,
                           subscribe_request(Message, block_response()),
                           escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Message)),
                           wait_for_request(Message, 1000),
                           ?assertNot(escalus_client:has_stanzas(Bob))
                        end).

allow_message_on_request_error_muclight(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           RoomName = muc_helper:fresh_room_name(),
                           muc_light_helper:create_room(RoomName,
                                                        muc_light_helper:muc_host(),
                                                        Alice,
                                                        [Bob],
                                                        Config,
                                                        muc_light_helper:ver(1)),
                           Message = <<"hi there people">>,
                           MucHost = muc_light_helper:muc_host(),
                           RoomAddr = <<RoomName/binary, "@", MucHost/binary>>,
                           subscribe_request(Message, error_response()),
                           escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Message)),
                           wait_for_request(Message, 1000),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_groupchat_message, [Message], R)
                        end).

filter_messages_sent_via_client_api(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"give me your password">>,
                           subscribe_request(Message, block_response()),
                           From = escalus_client:full_jid(Alice),
                           To = escalus_client:short_jid(Bob),
                           user_send_message(Alice, From, To, Message, Config),
                           wait_for_request(Message, 1000),
                           ?assertNot(escalus_client:has_stanzas(Bob))
                        end).

dont_filter_messages_sent_via_admin_api(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"check this out: virus.com">>,
                           subscribe_request(Message, block_response()),
                           From = escalus_client:full_jid(Alice),
                           To = escalus_client:short_jid(Bob),
                           admin_send_message(From, To, Message, Config),
                           assert_no_request(Message),
                           escalus:assert(is_message, escalus:wait_for_stanza(Bob))
                        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_mock() ->
    Port = 8081,
    {ok, MockPid} = http_helper:start(Port, "/api", fun external_filter_mock_handler/1),
    ets:new(external_filter_mock_subscribers,
            [public, named_table, {heir, MockPid, take_care}]),
    PoolOpts = #{strategy => available_worker, workers => 20},
    ConnOpts =
        #{host => "http://localhost:8081",
          path_prefix => <<"/api">>,
          request_timeout => 2000},
    Pool =
        config_parser_helper:config([outgoing_pools, http, external_filter],
                                    #{opts => PoolOpts, conn_opts => ConnOpts}),
    [{ok, _Pid}] =
        distributed_helper:rpc(
            distributed_helper:mim(), mongoose_wpool, start_configured_pools, [[Pool]]).

subscribe_request(Message, Response) ->
    ets:insert(external_filter_mock_subscribers, {Message, self(), Response}).

assert_no_request(Message) ->
    receive
        {request, Message, _Body, _Resp} ->
            ct:fail("request came for message ~p", [Message])
    after 1000 ->
        ok
    end.

wait_for_request(Message, Timeout) ->
    receive
        {request, Message, Body, Resp} ->
            {jiffy:decode(Body, [return_maps]), Resp}
    after Timeout ->
        ct:fail("timeout_waiting_for_request")
    end.

external_filter_mock_handler(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    #{<<"variables">> := #{<<"body">> := Message}} = jiffy:decode(Body, [return_maps]),
    [{Message, Subscriber, Response}] = ets:lookup(external_filter_mock_subscribers, Message),
    Subscriber ! {request, Message, Body, Response},
    Req3 =
        case Response of
            server_error ->
                cowboy_req:reply(500, #{}, <<"Internal server error">>, Req2);
            _ when is_map(Response) ->
                RespBody = jiffy:encode(Response),
                cowboy_req:reply(200, #{}, RespBody, Req2)
        end,
    Req3.

stop_mock() ->
    distributed_helper:rpc(
        distributed_helper:mim(), mongoose_wpool, stop, [http, global, external_filter]),
    http_helper:stop().

user_send_message(User, From, To, Body, Config) ->
    Vars =
        #{from => From,
          to => To,
          body => Body},
    graphql_helper:execute_user_command(<<"stanza">>, <<"sendMessage">>, User, Vars, Config).

admin_send_message(From, To, Body, Config) ->
    Vars =
        #{from => From,
          to => To,
          body => Body},
    graphql_helper:execute_command(<<"stanza">>, <<"sendMessage">>, Vars, Config).

allow_response() ->
    #{<<"data">> => #{<<"verifyMessage">> => #{<<"action">> => <<"ALLOW">>}}}.

block_response() ->
    #{<<"data">> => #{<<"verifyMessage">> => #{<<"action">> => <<"BLOCK">>}}}.

error_response() ->
    #{<<"errors">> => [#{<<"message">> => <<"Invalid argument type.">>}]}.

server_error() ->
    server_error.
