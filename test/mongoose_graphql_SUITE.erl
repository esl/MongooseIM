-module(mongoose_graphql_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("graphql/src/graphql_schema.hrl").

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
     admin_and_user_load_global_types,
     {group, unprotected_graphql},
     {group, protected_graphql},
     {group, error_handling},
     {group, error_formatting},
     {group, permissions}].

groups() ->
    [{protected_graphql, [parallel], protected_graphql()},
     {unprotected_graphql, [parallel], unprotected_graphql()},
     {error_handling, [parallel], error_handling()},
     {error_formatting, [parallel], error_formatting()},
     {permissions, [parallel], permissions()}].

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
    [should_catch_parsing_errors,
     should_catch_type_check_params_errors,
     should_catch_type_check_errors].

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
                                  C =:= should_catch_type_check_params_errors;
                                  C =:= should_catch_type_check_errors;
                                  C =:= should_catch_parsing_errors ->
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
    Doc = <<"{ field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, #{error_term := {no_permissions, <<"ROOT">>}}}, Res).

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

should_catch_parsing_errors(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { field ">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, _}, Res).

should_catch_type_check_errors(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { notExistingField(value: \"Hello\") }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, _}, Res).

should_catch_type_check_params_errors(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { id(value: 12) }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, _}, Res).

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
                    make_error(authorize, {no_permissions, <<"ROOT">>})),
    {401, Msg3} = mongoose_graphql_errors:format_error(
                    make_error(authorize, {request_error, {header, <<"authorization">>}, 'msg'})),

    ?assertErrMsg(wrong_credentials, <<"provided credentials are wrong">>, Msg1),
    ?assertErrMsg(no_permissions, <<"without permissions">>, Msg2),
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

%% Helpers

assert_err_msg(Code, MsgContains, #{message := Msg} = ErrorMsg) ->
    ?assertMatch(#{extensions := #{code := Code}}, ErrorMsg),
    ?assertNotEqual(nomatch, binary:match(Msg, MsgContains)).

make_error(Phase, Term) ->
    #{phase => Phase, error_term => Term}.

check_permissions(Config, Doc) ->
    Ep = ?config(endpoint, Config),
    Op = proplists:get_value(op, Config, undefined),
    {ok, Ast} = graphql:parse(Doc),
    {ok, #{ast := Ast2}} = graphql:type_check(Ep, Ast),
    ok = graphql:validate(Ast2),
    ok = mongoose_graphql_permissions:check_permissions(Op, false, Ast2).

request(Doc, Authorized) ->
    #{document => Doc,
      operation_name => undefined,
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
