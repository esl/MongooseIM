-module(mongoose_graphql_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("jid/include/jid.hrl").

-type listener_opts() :: #{endpoint_schema := binary(),
                           atom() => any()}.

-define(assertPermissionsFailed(Config, Doc),
        ?assertThrow({error, #{error_term := {no_permissions, _}}},
                     check_permissions(Config, false, Doc))).
-define(assertPermissionsSuccess(Config, Doc),
        ?assertMatch(#document{}, check_permissions(Config, false, Doc))).

-define(assertDomainPermissionsFailed(Config, Domain, Args, Doc),
        ?assertThrow({error, #{error_term := {no_permissions, _, #{type := domain,
                                                                   invalid_args := Args}}}},
                     check_domain_permissions(Config, Domain, Doc))).
-define(assertPermissionsSuccess(Config, Domain, Doc),
        ?assertMatch(#document{}, check_domain_permissions(Config, Domain, Doc))).

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
     {group, domain_permissions},
     {group, use_directive},
     {group, user_listener},
     {group, admin_listener},
     {group, domain_admin_listener}].

groups() ->
    [{protected_graphql, [parallel], protected_graphql()},
     {unprotected_graphql, [parallel], unprotected_graphql()},
     {error_handling, [parallel], error_handling()},
     {error_formatting, [parallel], error_formatting()},
     {permissions, [parallel], permissions()},
     {domain_permissions, [parallel], domain_permissions()},
     {use_directive, [parallel], use_directive()},
     {admin_listener, [parallel], admin_listener()},
     {domain_admin_listener, [parallel], domain_admin_listener()},
     {user_listener, [parallel], user_listener()}].

protected_graphql() ->
    [auth_can_execute_protected_query,
     auth_can_execute_protected_mutation,
     unauth_cannot_execute_protected_query,
     unauth_cannot_execute_protected_mutation].

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

domain_permissions() ->
    [check_field_domain_permissions,
     check_field_input_arg_domain_permissions,
     check_field_list_arg_domain_permissions,
     check_field_null_arg_domain_permissions,
     check_field_jid_arg_domain_permissions,
     check_child_object_field_domain_permissions,
     check_field_subdomain_permissions,
     check_field_global_permissions,
     check_interface_field_domain_permissions
    ].

use_directive() ->
    [use_dir_module_not_loaded,
     use_dir_all_modules_loaded,
     use_dir_all_modules_and_services_loaded,
     use_dir_module_and_service_not_loaded,
     use_dir_module_service_and_db_loaded,
     use_dir_db_not_loaded,
     use_dir_module_service_and_db_not_loaded,
     use_dir_object_module_service_and_db_loaded,
     use_dir_object_all_modules_services_and_dbs_loaded,
     use_dir_object_module_and_db_not_loaded,
     use_dir_object_service_and_db_not_loaded,
     use_dir_auth_admin_all_modules_services_and_dbs_loaded,
     use_dir_auth_user_all_modules_services_and_dbs_loaded,
     use_dir_auth_admin_module_service_and_db_not_loaded,
     use_dir_auth_user_module_service_and_db_not_loaded,
     use_dir_auth_admin_db_not_loaded,
     use_dir_auth_user_db_not_loaded
    ].

user_listener() ->
    [auth_user_can_access_protected_types,
     use_directive_can_use_auth_user_domain | common_tests()].

admin_listener() ->
    [no_creds_defined_admin_can_access_protected,
     auth_admin_can_access_protected_types | common_tests()].

domain_admin_listener() ->
    [auth_domain_admin_can_access_protected_types,
     auth_domain_admin_wrong_password_error,
     auth_domain_admin_nonexistent_domain_error,
     auth_domain_admin_cannot_access_other_domain,
     auth_domain_admin_cannot_access_global,
     auth_domain_admin_can_access_owned_domain,
     use_directive_can_use_auth_domain_admin_domain
     | common_tests()].

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
    %% Register atoms for `binary_to_existing_atom`
    [mod_x, mod_z, service_x, service_d, db_x],
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(user_listener, Config) ->
    Config1 = meck_module_and_service_checking(Config),
    Config2 = meck_domain_api(Config1),
    meck:new(mongoose_api_common, [no_link]),
    meck:expect(mongoose_api_common, check_password,
                fun
                    (#jid{luser = <<"alice">>}, <<"makota">>) -> {true, {}};
                    (_, _) -> false
                end),
    ListenerOpts = #{schema_endpoint => user},
    init_ep_listener(5557, user_schema_ep, ListenerOpts, Config2);
init_per_group(admin_listener, Config) ->
    ListenerOpts = #{username => <<"admin">>,
                     password => <<"secret">>,
                     schema_endpoint => admin},
    init_ep_listener(5558, admin_schema_ep, ListenerOpts, Config);
init_per_group(domain_admin_listener, Config) ->
    Config1 = meck_module_and_service_checking(Config),
    Config2 = meck_domain_api(Config1),
    meck:expect(mongoose_domain_api, check_domain_password,
                fun
                    (<<"localhost">>, <<"makota">>) -> ok;
                    (<<"localhost">>, _) -> {error, wrong_password};
                    (_, _) -> {error, not_found}
                end),
    ListenerOpts = #{schema_endpoint => domain_admin},
    init_ep_listener(5560, domain_admin_schema_ep, ListenerOpts, Config2);
init_per_group(domain_permissions, Config) ->
    meck:new(mongoose_domain_api, [no_link]),
    meck:expect(mongoose_domain_api, get_subdomain_info,
                fun
                    (<<"subdomain.test-domain.com">>) ->
                        {ok, #{parent_domain => <<"test-domain.com">>}};
                    (<<"subdomain.test-domain2.com">>) ->
                        {ok, #{parent_domain => <<"test-domain2.com">>}};
                    (_) ->
                        {error, not_found}
                end),
    Domains = [{<<"subdomain.test-domain.com">>, <<"test-domain.com">>},
               {<<"subdomain.test-domain2.com">>, <<"test-domain2.com">>}],
    [{domains, Domains} | Config];
init_per_group(use_directive, Config) ->
    Config1 = meck_domain_api(Config),
    mongoose_config:set_opts(#{internal_databases => #{db_a => #{}}}),
    meck_module_and_service_checking(Config1);
init_per_group(_G, Config) ->
    Config.

end_per_group(user_listener, Config) ->
    unmeck_module_and_service_checking(Config),
    unmeck_domain_api(Config),
    ?config(test_process, Config) ! stop,
    Config;
end_per_group(admin_listener, Config) ->
    ?config(test_process, Config) ! stop,
    Config;
end_per_group(domain_admin_listener, Config) ->
    unmeck_module_and_service_checking(Config),
    unmeck_domain_api(Config),
    ?config(test_process, Config) ! stop,
    Config;
end_per_group(domain_permissions, _Config) ->
    meck:unload(mongoose_domain_api);
end_per_group(use_directive, Config) ->
    unmeck_domain_api(Config),
    unmeck_module_and_service_checking(Config),
    mongoose_config:erase_opts();
end_per_group(_, Config) ->
    Config.

init_per_testcase(C, Config) when C =:= auth_can_execute_protected_query;
                                  C =:= auth_can_execute_protected_mutation;
                                  C =:= unauth_cannot_execute_protected_query;
                                  C =:= unauth_cannot_execute_protected_mutation ->
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
                                  C =:= check_union_permissions;
                                  C =:= check_field_domain_permissions;
                                  C =:= check_field_input_arg_domain_permissions;
                                  C =:= check_field_list_arg_domain_permissions;
                                  C =:= check_field_null_arg_domain_permissions;
                                  C =:= check_field_jid_arg_domain_permissions;
                                  C =:= check_field_subdomain_permissions;
                                  C =:= check_field_global_permissions;
                                  C =:= check_child_object_field_domain_permissions;
                                  C =:= check_interface_field_domain_permissions ->
    {Mapping, Pattern} = example_permissions_schema_data(Config),
    {ok, _} = mongoose_graphql:create_endpoint(C, Mapping, [Pattern]),
    Ep = mongoose_graphql:get_endpoint(C),
    [{endpoint, Ep} | Config];
init_per_testcase(C, Config) when C =:= use_dir_module_not_loaded;
                                  C =:= use_dir_all_modules_loaded;
                                  C =:= use_dir_module_and_service_not_loaded;
                                  C =:= use_dir_module_service_and_db_loaded;
                                  C =:= use_dir_db_not_loaded;
                                  C =:= use_dir_module_service_and_db_not_loaded;
                                  C =:= use_dir_all_modules_and_services_loaded;
                                  C =:= use_dir_object_module_service_and_db_loaded;
                                  C =:= use_dir_object_all_modules_services_and_dbs_loaded;
                                  C =:= use_dir_object_module_and_db_not_loaded;
                                  C =:= use_dir_object_service_and_db_not_loaded;
                                  C =:= use_dir_auth_user_all_modules_services_and_dbs_loaded;
                                  C =:= use_dir_auth_admin_all_modules_services_and_dbs_loaded;
                                  C =:= use_dir_auth_user_module_service_and_db_not_loaded;
                                  C =:= use_dir_auth_admin_module_service_and_db_not_loaded;
                                  C =:= use_dir_auth_admin_db_not_loaded;
                                  C =:= use_dir_auth_user_db_not_loaded ->
    {Mapping, Pattern} = example_directives_schema_data(Config),
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
    ?assertMatch(#scalar_type{id = <<"JID">>}, graphql_schema:get(AdminEp, <<"JID">>)),
    ?assertMatch(#directive_type{id = <<"protected">>},
                 graphql_schema:get(AdminEp, <<"protected">>)),

    UserEp = mongoose_graphql:get_endpoint(user),
    ?assertMatch(#scalar_type{id = <<"JID">>}, graphql_schema:get(UserEp, <<"JID">>)),
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
    FieldProtectedNotEnough = <<"{ obj { protectedName } }">>,
    FieldProtectedEnough = <<"{ obj { otherName } }">>,
    % Field is protected in interface and object, so it cannot be accessed.
    ?assertPermissionsFailed(Config, Doc),
    ?assertPermissionsFailed(Config, FieldProtectedEnough),
    % Field is protected only in an interface, so it can be accessed from implementing objects.
    ?assertPermissionsSuccess(Config, FieldProtectedNotEnough).

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

%% Domain permissions

check_field_domain_permissions(Config) ->
    Domain = <<"my-domain.com">>,
    Config2 = [{op, <<"Q1">>}, {args, #{<<"domain">> => Domain}} | Config],
    Doc = <<"{ field protectedField }">>,
    Doc2 = <<"query Q1($domain: String) { protectedField domainProtectedField(argA: $domain"
             ", argB: \"domain\") }">>,
    FDoc = <<"{protectedField domainProtectedField(argA: \"domain.com\","
             " argB: \"domain.com\") }">>,
    ?assertPermissionsSuccess(Config, Domain, Doc),
    ?assertPermissionsSuccess(Config2, Domain, Doc2),
    ?assertDomainPermissionsFailed(Config, Domain, [<<"argA">>], FDoc).

check_child_object_field_domain_permissions(Config) ->
    Domain = <<"my-domain.com">>,
    Config2 = [{op, <<"Q1">>}, {args, #{<<"domain">> => Domain}} | Config],
    Doc = <<"{ obj { field protectedField } }">>,
    Doc2 = <<"query Q1($domain: String) { obj { protectedField domainProtectedField(argA: $domain"
             ", argB: \"domain\") } }">>,
    FDoc = <<"{ obj {protectedField domainProtectedField(argA: \"domain.com\","
             " argB: \"domain.com\") } }">>,
    ?assertPermissionsSuccess(Config, Domain, Doc),
    ?assertPermissionsSuccess(Config2, Domain, Doc2),
    ?assertDomainPermissionsFailed(Config, Domain, [<<"argA">>], FDoc).

check_interface_field_domain_permissions(Config) ->
    Domain = <<"my-domain.com">>,
    OkDomain = <<"{ interface { protectedDomainName(domain: \"my-domain.com\") } }">>,
    OkDomain1 = <<"{ obj { protectedDomainName(domain: \"my-domain.com\") } }">>,
    OkDomain2 = <<"{ obj { domainName(domain: \"my-domain.com\") } }">>,
    WrongDomain = <<"{ interface { protectedDomainName(domain: \"domain.com\") } }">>,
    WrongDomain1 = <<"{ obj { domainName(domain: \"domain.com\") } }">>,
    ProtectedNotEnough = <<"{ obj { protectedDomainName(domain: \"domain.com\") } }">>,
    ?assertPermissionsSuccess(Config, Domain, OkDomain),
    ?assertPermissionsSuccess(Config, Domain, OkDomain1),
    ?assertPermissionsSuccess(Config, Domain, OkDomain2),
    % Field is protected in interface and object, so it cannot be accessed with the wrong domain.
    ?assertDomainPermissionsFailed(Config, Domain, [<<"domain">>], WrongDomain),
    ?assertDomainPermissionsFailed(Config, Domain, [<<"domain">>], WrongDomain1),
    % Field is protected only in an interface, so it can be accessed from implementing objects
    % with the wrong domain.
    ?assertPermissionsSuccess(Config, Domain, ProtectedNotEnough).

check_field_input_arg_domain_permissions(Config) ->
    Domain = <<"my-domain.com">>,
    DomainInput = #{<<"domain">> => Domain, <<"notDomain">> => <<"random text here">>},
    Config2 = [{op, <<"Q1">>}, {args, #{<<"domain">> => Domain,
                                        <<"domainInput">> => DomainInput}} | Config],
    Doc = <<"query Q1($domain: String, $domainInput: DomainInput!) "
            "{ domainInputProtectedField(argA: $domain, argB: $domainInput)"
            "  domainProtectedField(argA: $domain, argB: \"domain.com\") }">>,

    FDoc = <<"{ domainInputProtectedField(argA: \"do.com\", argB: { domain: \"do.com\" }) }">>,
    ?assertPermissionsSuccess(Config2, Domain, Doc),
    ?assertDomainPermissionsFailed(Config, Domain, [<<"argA">>, <<"argB.domain">>], FDoc).


check_field_list_arg_domain_permissions(Config) ->
    [{Subdomain, Domain} | _] = ?config(domains, Config),
    Domains = [#{<<"domain">> => Domain, <<"notDomain">> => <<"random text here">>},
               #{<<"domain">> => Subdomain}],
    Config2 = [{op, <<"Q1">>}, {args, #{<<"domains">> => Domains}} | Config],
    Doc = <<"query Q1($domains: [DomainInput!]) "
            "{ domainListInputProtectedField(domains: $domains) }">>,

    FDoc = <<"{ domainListInputProtectedField(domains: [{ domain: \"do.com\" }]) }">>,
    ?assertPermissionsSuccess(Config2, Domain, Doc),
    ?assertDomainPermissionsFailed(Config, Domain, [<<"domains.domain">>], FDoc).

check_field_null_arg_domain_permissions(Config) ->
    [{_, Domain} | _] = ?config(domains, Config),
    FDoc = <<"{ domainProtectedField domainInputProtectedField }">>,
    ?assertDomainPermissionsFailed(Config, Domain, [<<"argA">>], FDoc).

check_field_jid_arg_domain_permissions(Config) ->
    Domain = <<"my-domain.com">>,
    Config2 = [{op, <<"Q1">>},
               {args, #{<<"jid">> => <<"bob@", Domain/binary>>}} | Config],
    Doc = <<"query Q1($jid: JID) { domainJIDProtectedField(argA: $jid, argB: \"bob@bob\") }">>,
    FDoc = <<"{ domainJIDProtectedField(argA: \"bob@do.com\", argB: \"bob@do.com\") }">>,
    ?assertPermissionsSuccess(Config2, Domain, Doc),
    ?assertDomainPermissionsFailed(Config, Domain, [<<"argA">>], FDoc).

check_field_subdomain_permissions(Config) ->
    [{Subdomain, Domain}, {FSubdomain, _Domain2}] = ?config(domains, Config),
    Config2 = [{op, <<"Q1">>}, {args, #{<<"domain">> => Subdomain}} | Config],
    FConfig2 = [{op, <<"Q1">>}, {args, #{<<"domain">> => FSubdomain}} | Config],
    Doc = <<"query Q1($domain: String) "
             "{ protectedField domainProtectedField(argA: $domain, argB: \"do.com\") }">>,
    ?assertPermissionsSuccess(Config2, Domain, Doc),
    ?assertDomainPermissionsFailed(FConfig2, Domain, [<<"argA">>], Doc).

check_field_global_permissions(Config) ->
    Domain = <<"my-domain.com">>,
    Doc = <<"{ protectedField onlyForGlobalAdmin }">>,
    ?assertMatch(#document{}, check_permissions(Config, true, Doc)),
    ?assertThrow({error, #{error_term := {no_permissions, _, #{type := global}}}},
                 check_domain_permissions(Config, Domain, Doc)).

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

use_directive_can_use_auth_user_domain(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ command }"},
    {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Error]}} =
        execute(Ep, Body, {<<"alice@localhost">>, <<"makota">>}),
    #{<<"extensions">> :=
        #{<<"code">> := <<"deps_not_loaded">>,
          <<"not_loaded_modules">> := [<<"mod_x">>]}
     } = Error.

no_creds_defined_admin_can_access_protected(_Config) ->
    Port = 5559,
    Ep = "http://localhost:" ++ integer_to_list(Port),
    start_listener(no_creds_admin_listener, Port, #{schema_endpoint => admin}),
    Body = #{<<"query">> => <<"{ field }">>},
    {Status, Data} = execute(Ep, Body, undefined),
    assert_access_granted(Status, Data).

auth_admin_can_access_protected_types(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"admin">>, <<"secret">>}),
    assert_access_granted(Status, Data).

auth_domain_admin_can_access_protected_types(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"admin@localhost">>, <<"makota">>}),
    assert_access_granted(Status, Data).

use_directive_can_use_auth_domain_admin_domain(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ command }"},
    {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Error]}} =
        execute(Ep, Body, {<<"admin@localhost">>, <<"makota">>}),
    #{<<"extensions">> :=
        #{<<"code">> := <<"deps_not_loaded">>,
          <<"not_loaded_modules">> := [<<"mod_x">>]}
     } = Error.

auth_domain_admin_wrong_password_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"admin@localhost">>, <<"mapsa">>}),
    assert_no_permissions(wrong_credentials, Status, Data).

auth_domain_admin_nonexistent_domain_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"admin@localhost2">>, <<"makota">>}),
    assert_no_permissions(wrong_credentials, Status, Data).

auth_domain_admin_can_access_owned_domain(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ fieldDP(argA: \"localhost\") }"},
    {Status, Data} = execute(Ep, Body, {<<"admin@localhost">>, <<"makota">>}),
    assert_access_granted(Status, Data).

auth_domain_admin_cannot_access_other_domain(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ field fieldDP(argA: \"domain.com\") }"},
    {Status, Data} = execute(Ep, Body, {<<"admin@localhost">>, <<"makota">>}),
    assert_no_permissions(no_permissions, Status, Data).

auth_domain_admin_cannot_access_global(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{query => "{ fieldGlobal(argA: \"localhost\") }"},
    {Status, Data} = execute(Ep, Body, {<<"admin@localhost">>, <<"makota">>}),
    assert_no_permissions(no_permissions, Status, Data).

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
    ?assertEqual({<<"400">>, <<"Bad Request">>}, Status),
    assert_code(invalid_json_body, Data).

no_query_supplied_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>, <<"Bad Request">>}, Status),
    assert_code(no_query_supplied, Data).

variables_invalid_json_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"{ field }">>, <<"variables">> => <<"{1: 2}">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>, <<"Bad Request">>}, Status),
    assert_code(variables_invalid_json, Data).

listener_reply_with_parsing_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"{ field ">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>, <<"Bad Request">>}, Status),
    assert_code(parser_error, Data),

    BodyScanner = #{<<"query">> => <<"mutation { id(value: \"asdfsad) } ">>},
    {StatusScanner, DataScanner} = execute(Ep, BodyScanner, undefined),
    ?assertEqual({<<"400">>, <<"Bad Request">>}, StatusScanner),
    assert_code(scanner_error, DataScanner).

listener_reply_with_type_check_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"mutation { id(value: 12) }">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>, <<"Bad Request">>}, Status),
    assert_code(input_coercion, Data).

listener_reply_with_validation_error(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Body = #{<<"query">> => <<"query Q1 { field } query Q1 { field }">>,
             <<"operationName">> => <<"Q1">>},
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"400">>, <<"Bad Request">>}, Status),
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

%% Use Directive

use_dir_all_modules_loaded(Config) ->
    Doc = <<"{catA { command(domain: \"localhost\")} }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    Res = execute_ast(Config, Ctx2, Ast),
    ?assertEqual(#{data => #{<<"catA">> => #{<<"command">> => <<"command">>}}}, Res).

use_dir_module_not_loaded(Config) ->
    Doc = <<"{catA { command(domain: \"test-domain.com\")} }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_modules := [<<"mod_b">>]
         },
      message := <<"Some of the required modules are not loaded">>,
      path := [<<"catA">>, <<"command">>]
     } = Error.

use_dir_all_modules_and_services_loaded(Config) ->
    Doc = <<"{catA { command2(domain: \"localhost\")} }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    Res = execute_ast(Config, Ctx2, Ast),
    ?assertEqual(#{data => #{<<"catA">> => #{<<"command2">> => <<"command2">>}}}, Res).

use_dir_module_and_service_not_loaded(Config) ->
    Doc = <<"{catA { command3(domain: \"localhost\")} }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_modules := [<<"mod_z">>],
          not_loaded_services := [<<"service_d">>]
         },
      message := <<"Some of the required modules and services are not loaded">>,
      path := [<<"catA">>, <<"command3">>]
     } = Error.

use_dir_module_service_and_db_loaded(Config) ->
    Doc = <<"{catA { command4(domain: \"localhost\")} }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    Res = execute_ast(Config, Ctx2, Ast),
    ?assertEqual(#{data => #{<<"catA">> => #{<<"command4">> => <<"command4">>}}}, Res).

use_dir_db_not_loaded(Config) ->
    Doc = <<"{catA { command5(domain: \"localhost\")} }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_internal_databases := [<<"db_x">>]},
      message :=
        <<"Some of the required internal databases are not loaded">>,
      path := [<<"catA">>, <<"command5">>]
     } = Error.

use_dir_module_service_and_db_not_loaded(Config) ->
    Doc = <<"{catA { command6(domain: \"localhost\")} }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_modules := [<<"mod_x">>],
          not_loaded_services := [<<"service_x">>],
          not_loaded_internal_databases := [<<"db_x">>]},
      message :=
        <<"Some of the required modules and services and internal databases are not loaded">>,
      path := [<<"catA">>, <<"command6">>]
     } = Error.

use_dir_object_all_modules_services_and_dbs_loaded(Config) ->
    Doc = <<"{ catC { command(domain: \"localhost\") } }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    Res = execute_ast(Config, Ctx2, Ast),
    ?assertEqual(#{data => #{<<"catC">> => #{<<"command">> => <<"command">>}}}, Res).

use_dir_object_module_and_db_not_loaded(Config) ->
    Doc = <<"{ catD { command(domain: \"localhost\") } }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_modules := [<<"mod_x">>],
          not_loaded_internal_databases := [<<"db_x">>]},
      message :=
        <<"Some of the required modules and internal databases are not loaded">>,
      path := [<<"catD">>, <<"command">>]
     } = Error.

use_dir_object_service_and_db_not_loaded(Config) ->
    Doc = <<"{ catD { command3 } }">>,
    Ctx = #{user => jid:make_bare(<<"user">>, <<"localhost">>)},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_services := [<<"service_x">>],
          not_loaded_internal_databases := [<<"db_x">>]},
      message := <<"Some of the required services and internal databases are not loaded">>,
      path := [<<"catD">>, <<"command3">>]
     } = Error.

use_dir_object_module_service_and_db_loaded(Config) ->
    Doc = <<"{ catB { command(domain: \"localhost\") } }">>,
    Ctx = #{},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_modules := [<<"mod_x">>],
          not_loaded_services := [<<"service_x">>],
          not_loaded_internal_databases := [<<"db_x">>]
         },
      message :=
        <<"Some of the required modules and services and internal databases are not loaded">>,
      path := [<<"catB">>, <<"command">>]
     } = Error.

use_dir_auth_all_modules_services_and_dbs_loaded(UserRole, Config) ->
    Doc = <<"{ catC { command2 } }">>,
    Ctx = #{user => jid:make_bare(UserRole, <<"localhost">>)},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    Res = execute_ast(Config, Ctx2, Ast),
    ?assertEqual(#{data => #{<<"catC">> => #{<<"command2">> => <<"command2">>}}}, Res).

use_dir_auth_user_all_modules_services_and_dbs_loaded(Config) ->
    use_dir_auth_all_modules_services_and_dbs_loaded(<<"user">>, Config).

use_dir_auth_admin_all_modules_services_and_dbs_loaded(Config) ->
    use_dir_auth_all_modules_services_and_dbs_loaded(<<"admin">>, Config).

use_dir_auth_module_service_and_db_not_loaded(UserRole, Config) ->
    Doc = <<"{ catB { command2 } }">>,
    Ctx = #{user => jid:make_bare(UserRole, <<"localhost">>)},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_modules := [<<"mod_x">>],
          not_loaded_services := [<<"service_x">>],
          not_loaded_internal_databases := [<<"db_x">>]
         },
      message :=
        <<"Some of the required modules and services and internal databases are not loaded">>,
      path := [<<"catB">>, <<"command2">>]
     } = Error.

use_dir_auth_user_module_service_and_db_not_loaded(Config) ->
    use_dir_auth_module_service_and_db_not_loaded(<<"user">>, Config).

use_dir_auth_admin_module_service_and_db_not_loaded(Config) ->
    use_dir_auth_module_service_and_db_not_loaded(<<"admin">>, Config).

use_dir_auth_db_not_loaded(UserRole, Config) ->
    Doc = <<"{ catD { command2 } }">>,
    Ctx = #{user => jid:make_bare(UserRole, <<"localhost">>)},
    {Ast, Ctx2} = check_directives(Config, Ctx, Doc),
    #{errors := [Error]} = execute_ast(Config, Ctx2, Ast),
    #{extensions :=
        #{code := deps_not_loaded,
          not_loaded_internal_databases := [<<"db_x">>]},
      message := <<"Some of the required internal databases are not loaded">>,
      path := [<<"catD">>, <<"command2">>]
     } = Error.

use_dir_auth_user_db_not_loaded(Config) ->
    use_dir_auth_db_not_loaded(<<"user">>, Config).

use_dir_auth_admin_db_not_loaded(Config) ->
    use_dir_auth_db_not_loaded(<<"admin">>, Config).

%% Helpers

assert_code(Code, Data) ->
    BinCode = atom_to_binary(Code),
    ?assertMatch(#{<<"errors">> := [#{<<"extensions">> := #{<<"code">> := BinCode}}]}, Data).

assert_no_permissions(ExpectedCode, Status, Data) ->
    ?assertEqual({<<"401">>, <<"Unauthorized">>}, Status),
    assert_code(ExpectedCode, Data).

assert_access_granted(Status, Data) ->
    ?assertEqual({<<"200">>, <<"OK">>}, Status),
    % access was granted, no error was returned
    ?assertNotMatch(#{<<"errors">> := _}, Data).

assert_err_msg(Code, MsgContains, #{message := Msg} = ErrorMsg) ->
    ?assertMatch(#{extensions := #{code := Code}}, ErrorMsg),
    ?assertNotEqual(nomatch, binary:match(Msg, MsgContains)).

make_error(Phase, Term) ->
    #{phase => Phase, error_term => Term}.

make_error(Path, Phase, Term) ->
    #{path => Path, phase => Phase, error_term => Term}.

check_directives(Config, Ctx, Doc) ->
    Ep = ?config(endpoint, Config),
    {ok, Ast} = graphql:parse(Doc),
    {ok, #{ast := Ast2}} = graphql:type_check(Ep, Ast),
    ok = graphql:validate(Ast2),
    Default = #{operation_name => undefined, params => #{}, authorized => true,
                error_module => mongoose_graphql_errors},
    Ctx2 = maps:merge(Default, Ctx),
    {mongoose_graphql_directive:process_directives(Ctx2, Ast2), Ctx2}.

execute_ast(Config, Ctx, Ast) ->
    Ep = ?config(endpoint, Config),
    graphql:execute(Ep, Ctx, Ast).

check_permissions(Config, Auth, Doc) ->
    Ep = ?config(endpoint, Config),
    Op = proplists:get_value(op, Config, undefined),
    {ok, Ast} = graphql:parse(Doc),
    {ok, #{ast := Ast2}} = graphql:type_check(Ep, Ast),
    ok = graphql:validate(Ast2),
    Ctx = #{operation_name => Op, authorized => Auth, params => #{}},
    #document{} = mongoose_graphql_directive:process_directives(Ctx, Ast2).

check_domain_permissions(Config, Domain, Doc) ->
    Ep = ?config(endpoint, Config),
    Args = proplists:get_value(args, Config, #{}),
    Op = proplists:get_value(op, Config, undefined),
    {ok, Ast} = graphql:parse(Doc),
    {ok, #{ast := Ast2, fun_env := FunEnv}} = graphql:type_check(Ep, Ast),
    ok = graphql:validate(Ast2),
    Coerced = graphql:type_check_params(Ep, FunEnv, Op, Args),
    Admin = jid:make_bare(<<"admin">>, Domain),
    Ctx = #{operation_name => Op, authorized => true, authorized_as => domain_admin,
            admin => Admin, params => Coerced},
    #document{} = mongoose_graphql_directive:process_directives(Ctx, Ast2).

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
          enums => #{default => mongoose_graphql_default_resolver},
          scalars => #{default => mongoose_graphql_scalar},
          interfaces => #{default => mongoose_graphql_default_resolver},
          unions => #{default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_directives_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "directives_schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver},
          enums => #{default => mongoose_graphql_default_resolver},
          scalars => #{default => mongoose_graphql_scalar},
          interfaces => #{default => mongoose_graphql_default_resolver},
          unions => #{default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_listener_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "listener_schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver},
          enums => #{default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

-spec init_ep_listener(integer(), atom(), listener_opts(), [{atom(), term()}]) ->
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

-spec start_listener(atom(), integer(), listener_opts()) -> ok.
start_listener(Ref, Port, Opts) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/graphql", mongoose_graphql_handler, Opts}]}
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
    binary:encode_hex(crypto:strong_rand_bytes(8), lowercase).

meck_domain_api(Config) ->
    Hosts = [<<"test-domain.com">>, <<"localhost">>],
    % mongoose_domain_api
    meck:new(mongoose_domain_api, [no_link]),
    meck:expect(mongoose_domain_api, get_host_type,
        fun (Host) ->
                case lists:member(Host, Hosts) of
                    true -> {ok, Host};
                    false -> {error, not_found}
                end
        end),
    meck:expect(mongoose_domain_api, get_subdomain_info, fun (_) -> {error, not_found} end),
    [{hosts, Hosts} | Config].

unmeck_domain_api(_Config) ->
    meck:unload(mongoose_domain_api).

meck_module_and_service_checking(Config) ->
    LoadedModules = #{<<"test-domain.com">> => [mod_a, mod_d],
                      <<"localhost">> => [mod_a, mod_b, mod_c]},
    LoadedServices = [service_a, service_b],
    % gen_mod
    meck:new(gen_mod, [no_link]),
    meck:expect(gen_mod, is_loaded,
                fun (Domain, M) -> lists:member(M, maps:get(Domain, LoadedModules, [])) end),
    % mongoose_service
    meck:new(mongoose_service, [no_link]),
    meck:expect(mongoose_service, is_loaded, fun (M) -> lists:member(M, LoadedServices) end),
    [{loaded_services, LoadedServices},
     {loaded_modules, LoadedModules} | Config].

unmeck_module_and_service_checking(_Config) ->
    meck:unload(gen_mod),
    meck:unload(mongoose_service),
    mongoose_config:erase_opts().
