-module(sm_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, negotiation},
     {group, acking}].

groups() ->
    [{negotiation, [], [server_announces_sm,
                        server_enables_sm_before_session,
                        server_enables_sm_after_session,
                        server_returns_failed_after_start,
                        server_returns_failed_after_auth]},
     {acking, [], [basic_ack_before_session,
                   basic_ack_after_session]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

server_announces_sm(Config) ->
    Alice = [{stream_management, true}
             | escalus_users:get_userspec(Config, alice)],
    {ok, _, Props, Features} = escalus_connection:start(Alice,
                                                        [start_stream]),
    true = escalus_session:can_use_stream_management(Props, Features).

server_enables_sm_before_session(Config) ->
    Alice = [{stream_management, true}
             | escalus_users:get_userspec(Config, alice)],
    {ok, _, _, _} = escalus_connection:start(Alice, [start_stream,
                                                     authenticate,
                                                     bind,
                                                     stream_management]).

server_enables_sm_after_session(Config) ->
    Alice = [{stream_management, true}
             | escalus_users:get_userspec(Config, alice)],
    {ok, _, _, _} = escalus_connection:start(Alice, [start_stream,
                                                     authenticate,
                                                     bind,
                                                     session,
                                                     stream_management]).

server_returns_failed_after_start(Config) ->
    server_returns_failed(Config, []).

server_returns_failed_after_auth(Config) ->
    server_returns_failed(Config, [authenticate]).

server_returns_failed(Config, ConnActions) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_userspec(Config, alice)],
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream]
                                                 ++ ConnActions),
    escalus_connection:send(Alice, escalus_stanza:enable_sm()),
    escalus:assert(is_failed,
                   escalus_connection:get_stanza(Alice, enable_sm_failed)).

basic_ack_before_session(Config) ->
    basic_ack(Config, [stream_management, session], 123).

basic_ack_after_session(Config) ->
    basic_ack(Config, [session, stream_management], 312).

basic_ack(Config, ConnActions, Expected) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_userspec(Config, alice)],
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  authenticate,
                                                  bind]
                                                 ++ ConnActions),
    escalus_connection:send(Alice, escalus_stanza:roster_get()),
    escalus:assert(is_roster_result,
                   escalus_connection:get_stanza(Alice, roster_result)),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_ack, [Expected],
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

