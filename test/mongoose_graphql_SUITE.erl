-module(mongoose_graphql_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("jid/include/jid.hrl").

-define(assertPermissionsFailed(Config, Doc),
        ?assertThrow({error, #{error_term := {no_permissions, _}}},
                     check_permissions(Config, Doc))).
-define(assertPermissionsSuccess(Config, Doc),
        ?assertMatch(ok, check_permissions(Config, Doc))).

-define(assertErrMsg(Code, MsgContains, ErrorMsg),
        assert_err_msg(Code, MsgContains, ErrorMsg)).

all() ->
    [can_create_endpoint,
     can_load_split_schema,
     unexpected_internal_error,
     admin_and_user_load_global_types,
     {group, unprotected_graphql},
     {group, protected_graphql},
     {group, error_handling},
     {group, error_formatting},
     {group, permissions},
     {group, user_listener},
     {group, admin_listener}].

groups() ->
    [{protected_graphql, [parallel], protected_graphql()},
     {unprotected_graphql, [parallel], unprotected_graphql()},
     {error_handling, [parallel], error_handling()},
     {error_formatting, [parallel], error_formatting()},
     {permissions, [parallel], permissions()},
     {admin_listener, [parallel], admin_listener()},
     {user_listener, [parallel], user_listener()}].

protected_graphql() ->
    [auth_can_execute_protected_query,
     auth_can_execute_protected_mutation,
     unauth_cannot_execute_protected_query,
     unauth_cannot_execute_protected_mutation,
     unauth_can_access_introspection].

unprotected_graphql() ->
    [can_execute_query_with_vars,
     auth_can_execute_query,
     auth_can_execute_mutation,
     unauth_can_execute_query,
     unauth_can_execute_mutation].

error_handling() ->
    [should_catch_parsing_error,
     should_catch_type_check_params_error,
     should_catch_type_check_error,
     should_catch_validation_error].

error_formatting() ->
    [format_internal_crash,
     format_parse_errors,
     format_decode_errors,
     format_authorize_error,
     format_validate_error,
     format_type_check_error,
     format_execute_error,
     format_uncategorized_error,
     format_any_error].

permissions() ->
    [check_object_permissions,
     check_field_permissions,
     check_child_object_permissions,
     check_child_object_field_permissions,
     check_fragment_permissions,
     check_interface_permissions,
     check_interface_field_permissions,
     check_inline_fragment_permissions,
     check_union_permissions
    ].

user_listener() ->
    [auth_user_can_access_protected_types | common_tests()].
admin_listener() ->
    [no_creds_defined_admin_can_access_protected,
     auth_admin_can_access_protected_types | common_tests()].

common_tests() ->
    [malformed_auth_header_error,
     auth_wrong_creds_error,
     invalid_json_body_error,
     no_query_supplied_error,
     variables_invalid_json_error,
     listener_reply_with_parsing_error,
     listener_reply_with_type_check_error,
     listener_reply_with_validation_error,
     listener_unauth_cannot_access_protected_types,
     listener_unauth_can_access_unprotected_types,
     listener_can_execute_query_with_variables].

init_per_suite(Config) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(user_listener, Config) ->
    meck:new(mongoose_api_common, [no_link]),
    meck:expect(mongoose_api_common, check_password,
                fun
                    (#jid{user = <<"alice">>}, <<"makota">>) -> {true, {}};
                    (_, _) -> false
                end),
    ListenerOpts = [{schema_endpoint, <<"user">>}],
    init_ep_listener(5557, user_schema_ep, ListenerOpts, Config);
init_per_group(admin_listener, Config) ->
    ListenerOpts = [{username, <<"admin">>},
                    {password, <<"secret">>},
                    {schema_endpoint, <<"admin">>}],
    init_ep_listener(5558, admin_schema_ep, ListenerOpts, Config);
init_per_group(no_creds_admin_listener, Config) ->
    ListenerOpts = [{schema_endpoint, <<"admin">>}],
    init_ep_listener(5559, admin_schema_ep, ListenerOpts, Config);
init_per_group(_G, Config) ->
    Config.

end_per_group(user_listener, Config) ->
    meck:unload(mongoose_api_common),
    ?config(test_process, Config) ! stop,
    Config;
end_per_group(admin_listener, Config) ->
    ?config(test_process, Config) ! stop,
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(C, Config) when C =:= auth_can_execute_protected_query;
                                  C =:= auth_can_execute_protected_mutation;
                                  C =:= unauth_cannot_execute_protected_query;
                                  C =:= unauth_cannot_execute_protected_mutation;
                                  C =:= unauth_can_access_introspection ->
    {Mapping, Pattern} = example_schema_protected_data(Config),
    {ok, _} = mongoose_graphql:create_endpoint(C, Mapping, [Pattern]),
    Ep = mongoose_graphql:get_endpoint(C),
    [{endpoint, Ep} | Config];
init_per_testcase(C, Config) when C =:= can_execute_query_with_vars;
                                  C =:= auth_can_execute_query;
                                  C =:= auth_can_execute_mutation;
                                  C =:= unauth_can_execute_query;
                                  C =:= unauth_can_execute_mutation;
                                  C =:= should_catch_type_check_params_error;
                                  C =:= should_catch_type_check_error;
                                  C =:= should_catch_parsing_error;
                                  C =:= should_catch_validation_error ->
    {Mapping, Pattern} = example_schema_data(Config),
    {ok, _} = mongoose_graphql:create_endpoint(C, Mapping, [Pattern]),
    Ep = mongoose_graphql:get_endpoint(C),
    [{endpoint, Ep} | Config];
init_per_testcase(C, Config) when C =:= check_object_permissions;
                                  C =:= check_field_permissions;
                                  C =:= check_child_object_permissions;
                                  C =:= check_child_object_field_permissions;
                                  C =:= check_fragment_permissions;
                                  C =:= check_interface_permissions;
                                  C =:= check_interface_field_permissions;
                                  C =:= check_inline_fragment_permissions;
                                  C =:= check_union_permissions ->
    {Mapping, Pattern} = example_permissions_schema_data(Config),
    {ok, _} = mongoose_graphql:create_endpoint(C, Mapping, [Pattern]),
    Ep = mongoose_graphql:get_endpoint(C),
    [{endpoint, Ep} | Config];
init_per_testcase(C, Config) ->
    [{endpoint_name, C} | Config].

end_per_testcase(_, _Config) ->
    ok.

can_create_endpoint(Config) ->
    Name = ?config(endpoint_name, Config),
    {Mapping, Pattern} = example_schema_protected_data(Config),
    {ok, Pid} = mongoose_graphql:create_endpoint(Name, Mapping, [Pattern]),

    Ep = mongoose_graphql:get_endpoint(Name),
    ?assertMatch({endpoint_context, Name, Pid, _, _}, Ep),
    ?assertMatch(#root_schema{id = 'ROOT', query = <<"UserQuery">>,
                              mutation = <<"UserMutation">>},
                  graphql_schema:get(Ep, 'ROOT')).

can_load_split_schema(Config) ->
    Name = ?config(endpoint_name, Config),
    {Mapping, Pattern} = example_split_schema_data(Config),
    {ok, Pid} = mongoose_graphql:create_endpoint(Name, Mapping, [Pattern]),

    Ep = mongoose_graphql:get_endpoint(Name),
    ?assertMatch({endpoint_context, Name, Pid, _, _}, Ep),
    ?assertMatch(#root_schema{id = 'ROOT', query = <<"Query">>,
                              mutation = <<"Mutation">>},
                  graphql_schema:get(Ep, 'ROOT')),
    ?assertMatch(#object_type{id = <<"Query">>}, graphql_schema:get(Ep, <<"Query">>)),
    ?assertMatch(#object_type{id = <<"Mutation">>}, graphql_schema:get(Ep, <<"Mutation">>)).

unexpected_internal_error(Config) ->
    Name = ?config(endpoint_name, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Name, undefined, Doc),
    ?assertEqual({error, internal_crash}, Res).

admin_and_user_load_global_types(_Config) ->
    mongoose_graphql:init(),
    AdminEp = mongoose_graphql:get_endpoint(admin),
    ?assertMatch(#object_type{id = <<"JID">>}, graphql_schema:get(AdminEp, <<"JID">>)),
    ?assertMatch(#directive_type{id = <<"protected">>},
                 graphql_schema:get(AdminEp, <<"protected">>)),

    UserEp = mongoose_graphql:get_endpoint(user),
    ?assertMatch(#object_type{id = <<"JID">>}, graphql_schema:get(UserEp, <<"JID">>)),
    ?assertMatch(#directive_type{id = <<"protected">>},
                 graphql_schema:get(UserEp, <<"protected">>)).

%% Protected graphql

auth_can_execute_protected_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"{ field }">>,
    Res = mongoose_graphql:execute(Ep, undefined, Doc),
    ?assertEqual({ok, #{data => #{<<"field">> => <<"Test field">>}}}, Res).

auth_can_execute_protected_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, undefined, Doc),
    ?assertEqual({ok, #{data => #{<<"field">> => <<"Test field">>}}}, Res).

unauth_cannot_execute_protected_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query Q1 { field }">>,
    Res = mongoose_graphql:execute(Ep, request(<<"Q1">>, Doc, false)),
    ?assertMatch({error, #{error_term := {no_permissions, <<"Q1">>}, path := [<<"Q1">>]}}, Res).

unauth_cannot_execute_protected_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, #{error_term := {no_permissions, <<"ROOT">>}}}, Res).

unauth_can_access_introspection(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"{ __schema { queryType { name } } __type(name: \"UserQuery\") { name } }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    Expected =
        {ok,
            #{data =>
                #{<<"__schema">> =>
                    #{<<"queryType">> =>
                        #{<<"name">> => <<"UserQuery">>}
                },
                <<"__type">> =>
                    #{<<"name">> =>
                        <<"UserQuery">>
                     }
                 }
             }
        },
    ?assertEqual(Expected, Res).

%% Unprotected graphql

can_execute_query_with_vars(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query Q1($value: String!) { id(value: $value)}">>,
    Req =
        #{document => Doc,
          operation_name => <<"Q1">>,
          vars => #{<<"value">> => <<"Hello">>},
          authorized => false,
          ctx => #{}},
    Res = mongoose_graphql:execute(Ep, Req),
    ?assertEqual({ok, #{data => #{<<"id">> => <<"Hello">>}}}, Res).

unauth_can_execute_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertEqual({ok, #{data => #{<<"field">> => <<"Test field">>}}}, Res).

unauth_can_execute_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertEqual({ok, #{data => #{<<"field">> => <<"Test field">>}}}, Res).

auth_can_execute_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, true)),
    ?assertEqual({ok, #{data => #{<<"field">> => <<"Test field">>}}}, Res).

auth_can_execute_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, true)),
    ?assertEqual({ok, #{data => #{<<"field">> => <<"Test field">>}}}, Res).

%% Error handling

should_catch_parsing_error(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { field ">>,
    DocScan = <<"query { id(value: \"ala) }">>,
    ResParseErr = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, #{phase := parse, error_term := {parser_error, _}}}, ResParseErr),
    ResScanErr = mongoose_graphql:execute(Ep, request(DocScan, false)),
    ?assertMatch({error, #{phase := parse, error_term := {scanner_error, _}}}, ResScanErr).

should_catch_type_check_error(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { notExistingField(value: \"Hello\") }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, #{phase := type_check, error_term := unknown_field}}, Res).

should_catch_type_check_params_error(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { id(value: 12) }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, #{phase := type_check, error_term := {input_coercion, _, _, _}}}, Res).

should_catch_validation_error(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query Q1{ id(value: \"ok\") } query Q1{ id(value: \"ok\") }">>,
    % Query name must be unique
    Res = mongoose_graphql:execute(Ep, request(<<"Q1">>, Doc, false)),
    ?assertMatch({error, #{phase := validate, error_term := {not_unique, _}}}, Res).

%% Permissions

check_object_permissions(Config) ->
    Doc = <<"query { field }">>,
    FDoc = <<"mutation { field }">>,
    ?assertPermissionsSuccess(Config, Doc),
    ?assertPermissionsFailed(Config, FDoc).

check_field_permissions(Config) ->
    Doc = <<"{ field protectedField }">>,
    ?assertPermissionsFailed(Config, Doc).

check_child_object_permissions(Config) ->
    Doc = <<"{ protectedObj{ type } }">>,
    ?assertPermissionsFailed(Config, Doc).

check_child_object_field_permissions(Config) ->
    Doc = <<"{ obj { field } }">>,
    FDoc = <<"{ obj { field protectedField } }">>,
    ?assertPermissionsSuccess(Config, Doc),
    ?assertPermissionsFailed(Config, FDoc).

check_fragment_permissions(Config) ->
    Config2 = [{op, <<"Q1">>} | Config],
    Doc = <<"query Q1{ obj { ...body } } fragment body on Object { name field }">>,
    FDoc = <<"query Q1{ obj { ...body } } fragment body on Object { name field protectedField }">>,
    ?assertPermissionsSuccess(Config2, Doc),
    ?assertPermissionsFailed(Config2, FDoc).

check_interface_permissions(Config) ->
    Doc = <<"{ interface { name } }">>,
    FDoc = <<"{ protInterface { name } }">>,
    ?assertPermissionsSuccess(Config, Doc),
    ?assertPermissionsFailed(Config, FDoc).

check_interface_field_permissions(Config) ->
    Doc = <<"{ interface { protectedName } }">>,
    FieldProtectedNotEnaugh = <<"{ obj { protectedName } }">>,
    FieldProtectedEnaugh = <<"{ obj { otherName } }">>,
    % field is protected in interface and object, so cannnot be accessed.
    ?assertPermissionsFailed(Config, Doc),
    ?assertPermissionsFailed(Config, FieldProtectedEnaugh),
    % field is protected only in interface, so can by accessed from implementing objects.
    ?assertPermissionsSuccess(Config, FieldProtectedNotEnaugh).

check_inline_fragment_permissions(Config) ->
    Doc = <<"{ interface { name otherName ... on Object { field } } }">>,
    FDoc = <<"{ interface { name otherName ... on Object { field protectedField } } }">>,
    FDoc2 = <<"{ interface { name ... on Object { field otherName} } }">>,
    ?assertPermissionsSuccess(Config, Doc),
    ?assertPermissionsFailed(Config, FDoc),
    ?assertPermissionsFailed(Config, FDoc2).

check_union_permissions(Config) ->
    Doc = <<"{ union { ... on O1 { field1 } } }">>,
    FDoc = <<"{ union { ... on O1 { field1 field1Protected } } }">>,
    FDoc2 = <<"{ union { ... on O1 { field1 } ... on O2 { field2 } } }">>,
    ?assertPermissionsSuccess(Config, Doc),
    ?assertPermissionsFailed(Config, FDoc),
    ?assertPermissionsFailed(Config, FDoc2).

%% Error formatting

format_internal_crash(_Config) ->
    {Code, Res} = mongoose_graphql_errors:format_error(internal_crash),
    ?assertEqual(500, Code),
    ?assertMatch(#{extensions := #{code := internal_server_error}}, Res).

format_parse_errors(_Config) ->
    ParserError = make_error(parse, {parser_error, {0, graphql_parser, "parser_error_msg"}}),
    ScannerError = make_error(parse, {scanner_error,
                                      {0, graphql_scanner, {illegal, "illegal_characters"}}}),
    ScannerError2 = make_error(parse, {scanner_error,
                                      {0, graphql_scanner, {user, "user_scanner_err"}}}),

    {400, ResParser} = mongoose_graphql_errors:format_error(ParserError),
    {400, ResScanner} = mongoose_graphql_errors:format_error(ScannerError),
    {400, ResScanner2} = mongoose_graphql_errors:format_error(ScannerError2),
    ?assertErrMsg(parser_error, <<"parser_error_msg">>, ResParser),
    ?assertErrMsg(scanner_error, <<"illegal_characters">>, ResScanner),
    ?assertErrMsg(scanner_error, <<"user_scanner_err">>, ResScanner2).

format_decode_errors(_Config) ->
    {400, Msg1} = mongoose_graphql_errors:format_error(make_error(decode, no_query_supplied)),
    {400, Msg2} = mongoose_graphql_errors:format_error(make_error(decode, invalid_json_body)),
    {400, Msg3} = mongoose_graphql_errors:format_error(make_error(decode, variables_invalid_json)),

    ?assertErrMsg(no_query_supplied, <<"The query was not supplied">>, Msg1),
    ?assertErrMsg(invalid_json_body, <<"invalid">>, Msg2),
    ?assertErrMsg(variables_invalid_json, <<"invalid">>, Msg3).

format_authorize_error(_Config) ->
    {401, Msg1} = mongoose_graphql_errors:format_error(make_error(authorize, wrong_credentials)),
    {401, Msg2} = mongoose_graphql_errors:format_error(
                    make_error([<<"ROOT">>], authorize, {no_permissions, <<"ROOT">>})),
    {401, Msg3} = mongoose_graphql_errors:format_error(
                    make_error(authorize, {request_error, {header, <<"authorization">>}, 'msg'})),

    ?assertErrMsg(wrong_credentials, <<"provided credentials are wrong">>, Msg1),
    ?assertErrMsg(no_permissions, <<"without permissions">>, Msg2),
    ?assertMatch(#{path := [<<"ROOT">>]}, Msg2),
    ?assertErrMsg(request_error, <<"Malformed authorization header">>, Msg3).

format_validate_error(_Config) ->
    % Ensure the module can format this phase
    {400, Msg} = mongoose_graphql_errors:format_error(
                   make_error(validate, {not_unique, <<"OpName">>})),
    ?assertMatch(#{extensions := #{code := not_unique}}, Msg).

format_type_check_error(_Config) ->
    % Ensure the module can format this phase
    {400, Msg} = mongoose_graphql_errors:format_error(
                   make_error(type_check, non_null)),
    ?assertMatch(#{extensions := #{code := non_null}}, Msg).

format_execute_error(_Config) ->
    % Ensure the module can format this phase
    {400, Msg} = mongoose_graphql_errors:format_error(
                   make_error(execute, {resolver_error, any_error})),
    ?assertMatch(#{extensions := #{code := resolver_error}}, Msg).

format_uncategorized_error(_Config) ->
    % Ensure the module can format this phase
    {400, Msg} = mongoose_graphql_errors:format_error(
                   make_error(uncategorized, any_error)),
    ?assertMatch(#{extensions := #{code := any_error}}, Msg).

format_any_error(_Config) ->
    {400, Msg1} = mongoose_graphql_errors:format_error(any_error),
    {400, Msg2} = mongoose_graphql_errors:format_error(<<"any_error">>),
    {400, Msg3} = mongoose_graphql_errors:format_error({1, any_error}),
    {400, Msg4} = mongoose_graphql_errors:format_error(#{msg => any_error}),
    ?assertErrMsg(uncategorized, <<"any_error">>, Msg1),
    ?assertErrMsg(uncategorized, <<"any_error">>, Msg2),
    ?assertErrMsg(uncategorized, <<"any_error">>, Msg3),
    ?assertErrMsg(uncategorized, <<"any_error">>, Msg4).

%% Listeners

auth_user_can_access_protected_types(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"alice@localhost">>, <<"makota">>}),
    assert_access_granted(Status, Data).

no_creds_defined_admin_can_access_protected(_Config) ->
    Port = 5559,
    Ep = "http://localhost:" ++ integer_to_list(Port),
    start_listener(no_creds_admin_listener, Port, [{schema_endpoint, <<"admin">>}]),
    Body = #{<<"query">> => <<"{ field }">>},
    {Status, Data} = execute(Ep, Body, undefined),
    assert_access_granted(Status, Data).

auth_admin_can_access_protected_types(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"admin">>, <<"secret">>}),
    assert_access_granted(Status, Data).

malformed_auth_header_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    % The encoded credentials value is malformed and cannot be decoded.
    Headers = [{<<"Authorization">>, <<"Basic YWRtaW46c2VjcmV">>}],
    {Status, Data} = post_request(Ep, Headers, <<"">>),
    assert_no_permissions(request_error, Status, Data).

auth_wrong_creds_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"user">>, <<"wrong_password">>}),
    assert_no_permissions(wrong_credentials, Status, Data).

invalid_json_body_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = <<"">>,
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>,<<"Bad Request">>}, Status),
    assert_code(invalid_json_body, Data).

no_query_supplied_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>,<<"Bad Request">>}, Status),
    assert_code(no_query_supplied, Data).

variables_invalid_json_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"{ field }">>, <<"variables">> => <<"{1: 2}">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>,<<"Bad Request">>}, Status),
    assert_code(variables_invalid_json, Data).

listener_reply_with_parsing_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"{ field ">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>,<<"Bad Request">>}, Status),
    assert_code(parser_error, Data),

    BodyScanner = #{<<"query">> => <<"mutation { id(value: \"asdfsad) } ">>},
    {StatusScanner, DataScanner} = execute(Ep, BodyScanner, undefined),
    ?assertEqual({<<"400">>,<<"Bad Request">>}, StatusScanner),
    assert_code(scanner_error, DataScanner).

listener_reply_with_type_check_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"mutation { id(value: 12) }">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>,<<"Bad Request">>}, Status),
    assert_code(input_coercion, Data).

listener_reply_with_validation_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"query Q1 { field } query Q1 { field }">>,
             <<"operationName">> => <<"Q1">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>,<<"Bad Request">>}, Status),
    assert_code(not_unique, Data).

listener_can_execute_query_with_variables(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "mutation M1($value: String!){ id(value: $value) } query Q1{ field }",
             variables => #{value => <<"Hello">>},
             operationName => <<"M1">>
            },
    {Status, Data} = execute(Ep, Body, undefined),
    assert_access_granted(Status, Data),
    ?assertMatch(#{<<"data">> := #{<<"id">> := <<"Hello">>}}, Data).

listener_unauth_cannot_access_protected_types(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertMatch(#{<<"errors">> := [#{<<"path">> := [<<"ROOT">>]}]}, Data),
    assert_no_permissions(no_permissions, Status, Data).

listener_unauth_can_access_unprotected_types(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "mutation { field }"},
    {Status, Data} = execute(Ep, Body, undefined),
    assert_access_granted(Status, Data).

%% Helpers

assert_code(Code, Data) ->
    BinCode = atom_to_binary(Code),
    ?assertMatch(#{<<"errors">> := [#{<<"extensions">> := #{<<"code">> := BinCode}}]}, Data).

assert_no_permissions(ExpectedCode, Status, Data) ->
    ?assertEqual({<<"401">>,<<"Unauthorized">>}, Status),
    assert_code(ExpectedCode, Data).

assert_access_granted(Status, Data) ->
    ?assertEqual({<<"200">>,<<"OK">>}, Status),
    % access was granted, no error was returned
    ?assertNotMatch(#{<<"errors">> := _}, Data).

assert_err_msg(Code, MsgContains, #{message := Msg} = ErrorMsg) ->
    ?assertMatch(#{extensions := #{code := Code}}, ErrorMsg),
    ?assertNotEqual(nomatch, binary:match(Msg, MsgContains)).

make_error(Phase, Term) ->
    #{phase => Phase, error_term => Term}.

make_error(Path, Phase, Term) ->
    #{path => Path, phase => Phase, error_term => Term}.

check_permissions(Config, Doc) ->
    Ep = ?config(endpoint, Config),
    Op = proplists:get_value(op, Config, undefined),
    {ok, Ast} = graphql:parse(Doc),
    {ok, #{ast := Ast2}} = graphql:type_check(Ep, Ast),
    ok = graphql:validate(Ast2),
    ok = mongoose_graphql_permissions:check_permissions(Op, false, Ast2).

request(Doc, Authorized) ->
    request(undefined, Doc, Authorized).

request(Op, Doc, Authorized) ->
    #{document => Doc,
      operation_name => Op,
      vars => #{},
      authorized => Authorized,
      ctx => #{}}.

example_split_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config),
                             "split_schema", "*.gql"]),
    Mapping =
        #{objects =>
              #{'Query' => mongoose_graphql_default_resolver,
                'Mutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_schema_protected_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "protected_schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_permissions_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "permissions_schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver},
         interfaces => #{default => mongoose_graphql_default_resolver},
         unions => #{default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_listener_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "listener_schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

-spec init_ep_listener(integer(), atom(), [{atom(), term()}], [{atom(), term()}]) ->
    [{atom(), term()}].
init_ep_listener(Port, EpName, ListenerOpts, Config) ->
    Pid = spawn(fun() ->
                    Name = list_to_atom("gql_listener_" ++ atom_to_list(EpName)),
                    ok = start_listener(Name, Port, ListenerOpts),
                    {Mapping, Pattern} = example_listener_schema_data(Config),
                    {ok, _} = mongoose_graphql:create_endpoint(EpName, Mapping, [Pattern]),
                    receive
                        stop ->
                            ok
                    end
                end),
    [{test_process, Pid}, {endpoint_addr, "http://localhost:" ++ integer_to_list(Port)} | Config].

-spec start_listener(atom(), integer(), [{atom(), term()}]) -> ok.
start_listener(Ref, Port, Opts) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/graphql", mongoose_graphql_cowboy_handler, Opts}]}
    ]),
    {ok, _} = cowboy:start_clear(Ref,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

-spec execute(binary(), map(), undefined | {binary(), binary()}) -> {{binary(), binary()}, map()}.
execute(EpAddr, Body, undefined) ->
    post_request(EpAddr, [], Body);
execute(EpAddr, Body, {Username, Password}) ->
    Creds = base64:encode(<<Username/binary, ":", Password/binary>>),
    Headers = [{<<"Authorization">>, <<"Basic ", Creds/binary>>}],
    post_request(EpAddr, Headers, Body).

post_request(EpAddr, HeadersIn, Body) when is_binary(Body) ->
    {ok, Client} = fusco:start(EpAddr, []),
    Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"Request-Id">>, random_request_id()} | HeadersIn],
    {ok, {ResStatus, _, ResBody, _, _}} = Res =
        fusco:request(Client, <<"/graphql">>, <<"POST">>, Headers, Body, 5000),
    fusco:disconnect(Client),
    ct:log("~p", [Res]),
    {ResStatus, jiffy:decode(ResBody, [return_maps])};
post_request(Ep, HeadersIn, Body) ->
    post_request(Ep, HeadersIn, jiffy:encode(Body)).

random_request_id() ->
    base16:encode(crypto:strong_rand_bytes(8)).
