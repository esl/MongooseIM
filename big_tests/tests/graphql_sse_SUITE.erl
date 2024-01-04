%% @doc Tests for the SSE handling of GraphQL subscriptions
-module(graphql_sse_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [get_bad_request/1, get_unauthorized/1, get_method_not_allowed/1,
                         build_request/4, make_creds/1, execute_auth/2,
                         execute_sse/3, execute_user_sse/3, execute_auth_sse/2]).

%% common_test callbacks

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin},
     {group, user},
     {group, timeout}].

groups() ->
    [{admin, [parallel], admin_tests()},
     {user, [parallel], user_tests()},
     {timeout, [], [sse_should_not_get_timeout]}].

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    application:ensure_all_started(gun),
    Config1.

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(user, Config) ->
    graphql_helper:init_user(Config);
init_per_group(admin, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(timeout, Config) ->
    % Change the default idle_timeout for the listener to 1s to test if sse will override it
    Listener = get_graphql_user_listener(),
    mongoose_helper:change_listener_idle_timeout(Listener, 1000),
    graphql_helper:init_user(Config).

end_per_group(user, _Config) ->
    escalus_fresh:clean(),
    graphql_helper:clean();
end_per_group(admin, _Config) ->
    graphql_helper:clean();
end_per_group(timeout, _Config) ->
    Listener = get_graphql_user_listener(),
    mongoose_helper:restart_listener(mim(), Listener),
    escalus_fresh:clean(),
    graphql_helper:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

admin_tests() ->
    [admin_missing_query,
     admin_invalid_query_string,
     admin_missing_creds,
     admin_invalid_creds,
     admin_invalid_method,
     admin_invalid_operation_type].

user_tests() ->
    [user_missing_query,
     user_invalid_query_string,
     user_missing_creds,
     user_invalid_creds,
     user_invalid_method,
     user_invalid_operation_type].

%% Test cases and stories

admin_missing_query(Config) ->
    get_bad_request(execute_auth_sse(#{}, Config)).

user_missing_query(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_missing_query_story/2).

user_missing_query_story(Config, Alice) ->
    get_bad_request(execute_user_sse(#{}, Alice, Config)).

admin_invalid_query_string(_Config) ->
    Port = graphql_helper:get_listener_port(admin),
    get_bad_request(sse_helper:connect_to_sse(Port, "/api/graphql/sse?=invalid", undefined, #{})).

user_invalid_query_string(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun user_invalid_query_string_story/1).

user_invalid_query_string_story(Alice) ->
    Port = graphql_helper:get_listener_port(user),
    Creds = make_creds(Alice),
    get_bad_request(sse_helper:connect_to_sse(Port, "/api/graphql/sse?=invalid", Creds, #{})).

admin_missing_creds(_Config) ->
    get_unauthorized(execute_sse(admin, #{query => doc(), variables => args()}, undefined)).

user_missing_creds(_Config) ->
    get_unauthorized(execute_sse(user, #{query => doc()}, undefined)).

admin_invalid_creds(_Config) ->
    Creds = {<<"invalid">>, <<"creds">>},
    get_unauthorized(execute_sse(admin, #{query => doc(), variables => args()}, Creds)).

user_invalid_creds(_Config) ->
    get_unauthorized(execute_sse(user, #{query => doc()}, {<<"invalid">>, <<"creds">>})).

admin_invalid_method(_Config) ->
    #{node := Node} = mim(),
    Request = build_request(Node, admin, #{query => doc(), variables => args()}, undefined),
    %% POST was used, while SSE accepts only GET
    get_method_not_allowed(rest_helper:make_request(Request#{path => "/graphql/sse"})).

user_invalid_method(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun user_invalid_method_story/1).

user_invalid_method_story(Alice) ->
    #{node := Node} = mim(),
    Request = build_request(Node, user, #{query => doc()}, make_creds(Alice)),
    %% POST was used, while SSE accepts only GET
    get_method_not_allowed(rest_helper:make_request(Request#{path => "/graphql/sse"})).

admin_invalid_operation_type(Config) ->
    Creds = graphql_helper:make_admin_creds(admin, Config),
    get_bad_request(execute_sse(admin, #{query => query_doc(), variables => args()}, Creds)).

user_invalid_operation_type(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun user_invalid_operation_type_story/1).

user_invalid_operation_type_story(Alice) ->
    get_bad_request(execute_sse(user, #{query => query_doc()}, make_creds(Alice))).

sse_should_not_get_timeout(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        From = escalus_client:full_jid(Bob),
        To = escalus_client:short_jid(Alice),
        {200, Stream} = graphql_helper:execute_user_command_sse(<<"stanza">>, <<"subscribeForMessages">>, Alice, #{}, Config),
        escalus:send(Bob, escalus_stanza:chat(From, To, <<"Hello!">>)),
        sse_helper:wait_for_event(Stream),
        timer:sleep(2000),
        escalus:send(Bob, escalus_stanza:chat(From, To, <<"Hello again!">>)),
        sse_helper:wait_for_event(Stream),
        sse_helper:stop_sse(Stream)
    end).

%% Helpers

get_graphql_user_listener() ->
    Handler = #{module => mongoose_graphql_handler, schema_endpoint => user},
    ListenerOpts = #{handlers => [Handler]},
    [Listener] = mongoose_helper:get_listeners(mim(), ListenerOpts),
    Listener.

%% Subscription - works only with the SSE handler
doc() ->
    graphql_helper:get_doc(<<"stanza">>, <<"subscribeForMessages">>).

%% Query - works only with the REST handler
query_doc() ->
    graphql_helper:get_doc(<<"stanza">>, <<"getLastMessages">>).

%% Same args used by both operations - only for Admin
args() ->
    #{caller => <<"alice@localhost">>}.
