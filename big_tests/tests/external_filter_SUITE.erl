-module(external_filter_SUITE).

-author('pawel.dlugosz@erlang-solutions.com').

-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, external_filter}].

groups() ->
    [{external_filter, [parallel], external_filter_test_cases()}].

external_filter_test_cases() ->
    [allow_appropriate_message,
     block_inappropriate_message,
     allow_message_on_request_error,
     allow_non_body_messages,
     allow_message_on_external_error,
     allow_appropriate_message_muclight,
     block_inappropriate_message_muclight,
     allow_message_on_request_error_muclight].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, module_config()),
    start_mock(Config1),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    stop_mock(),
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

module_config() ->
    [{mod_external_filter,
      config_parser_helper:mod_config(mod_external_filter, #{pool_tag => external_filter})},
     {mod_stanzaid, #{}},
     {mod_muc_light, config_parser_helper:mod_config_with_auto_backend(mod_muc_light)}].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

allow_appropriate_message(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"hello">>,
                           external_filter_mock:subscribe(Message, allow_response()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           external_filter_mock:wait_for_request(Message, 1000),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_chat_message, [Message], R)
                        end).

block_inappropriate_message(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"send me money">>,
                           external_filter_mock:subscribe(Message, block_response()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           external_filter_mock:wait_for_request(Message, 1000),
                           ?assertNot(escalus_client:has_stanzas(Bob))
                        end).

allow_message_on_request_error(Config) ->
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
                           Message = <<"hi there">>,
                           external_filter_mock:subscribe(Message, error_response()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           external_filter_mock:wait_for_request(Message, 1000),
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
                           external_filter_mock:subscribe(Message, server_error()),
                           escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
                           external_filter_mock:wait_for_request(Message, 1000),
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
                           external_filter_mock:subscribe(Message, allow_response()),
                           escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Message)),
                           external_filter_mock:wait_for_request(Message, 1000),
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
                           external_filter_mock:subscribe(Message, block_response()),
                           escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Message)),
                           external_filter_mock:wait_for_request(Message, 1000),
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
                           external_filter_mock:subscribe(Message, error_response()),
                           escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Message)),
                           external_filter_mock:wait_for_request(Message, 1000),
                           R = escalus_client:wait_for_stanza(Bob),
                           escalus:assert(is_groupchat_message, [Message], R)
                        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_mock(Config) ->
    catch external_filter_mock:stop(),
    external_filter_mock:start(Config),
    Port = external_filter_mock:port(),
    PoolOpts = #{strategy => available_worker, workers => 20},
    ConnOpts =
        #{host => "https://localhost:" ++ integer_to_list(Port),
          path_prefix => <<"/api">>,
          request_timeout => 2000,
          tls => #{verify_mode => none}},
    Pool =
        config_parser_helper:config([outgoing_pools, http, external_filter],
                                    #{opts => PoolOpts, conn_opts => ConnOpts}),
    [{ok, _Pid}] =
        distributed_helper:rpc(
            distributed_helper:mim(), mongoose_wpool, start_configured_pools, [[Pool]]).

stop_mock() ->
    distributed_helper:rpc(
        distributed_helper:mim(), mongoose_wpool, stop, [http, global, external_filter]),
    mongoose_push_mock:stop().

allow_response() ->
    #{<<"data">> => #{<<"verify">> => #{<<"action">> => <<"ALLOW">>}}}.

block_response() ->
    #{<<"data">> => #{<<"verify">> => #{<<"action">> => <<"BLOCK">>}}}.

error_response() ->
    #{<<"errors">> => [#{<<"message">> => <<"Invalid argument type.">>}]}.

server_error() ->
    server_error.
