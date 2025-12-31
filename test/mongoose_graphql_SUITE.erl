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
    admin_schema_has_server_host_types,
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

admin_schema_has_server_host_types(_Config) ->
    mongoose_graphql:init(),
    AdminEp = mongoose_graphql:get_endpoint(admin),
    ?assertMatch(#object_type{id = <<"ServerAdminQuery">>},
                 graphql_schema:get(AdminEp, <<"ServerAdminQuery">>)),
    ?assertMatch(#object_type{id = <<"HostTypeInfo">>},
                 graphql_schema:get(AdminEp, <<"HostTypeInfo">>)),
    #object_type{fields = ServerFields} = graphql_schema:get(AdminEp, <<"ServerAdminQuery">>),
    ?assertMatch(#schema_field{ty = {non_null, {list, {non_null, <<"HostTypeInfo">>}}}},
                 maps:get(<<"hostTypes">>, ServerFields)),
    #object_type{fields = HostTypeFields} = graphql_schema:get(AdminEp, <<"HostTypeInfo">>),
    ?assertMatch(#schema_field{ty = {non_null, <<"String">>}}, maps:get(<<"name">>, HostTypeFields)),
    ?assertMatch(#schema_field{ty = {non_null, {list, {non_null, <<"String">>}}}},
                 maps:get(<<"domains">>, HostTypeFields)),
    ?assertMatch(#schema_field{ty = {non_null, {list, {non_null, <<"ModuleInfo">>}}}},
                 maps:get(<<"modules">>, HostTypeFields)).

%% Protected graphql

auth_can_execute_protected_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"{ field }">>,
    Res = mongoose_graphql:execute(Ep, undefined, Doc),
    ?assertEqual({ok, #{data => #{<<"field">> => <<"Test field">>}}}, Res).

... (truncated for brevity in this message)
