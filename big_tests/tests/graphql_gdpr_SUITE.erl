-module(graphql_gdpr_SUITE).

-compile([export_all, nowarn_export_all]).

-import(domain_helper, [host_type/0, domain/0]).
-import(distributed_helper, [mim/0, rpc/4, require_rpc_nodes/1]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, user_to_bin/1,
                         get_ok_value/2, get_err_code/1, get_unauthorized/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_gdpr_http},
     {group, admin_gdpr_cli},
     {group, domain_admin_gdpr}].

groups() ->
    [{admin_gdpr_http, [], admin_stats_tests()},
     {admin_gdpr_cli, [], admin_stats_tests()},
     {domain_admin_gdpr, [], domain_admin_stats_tests()}].

admin_stats_tests() ->
    [admin_retrive_user_data,
     admin_gdpr_no_user_test].

domain_admin_stats_tests() ->
    [admin_retrive_user_data,
     admin_gdpr_no_user_test,
     domain_admin_retrive_user_data_no_permission].

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    ejabberd_node_utils:init(mim(), Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(admin_gdpr_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_gdpr_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_gdpr, Config) ->
    graphql_helper:init_domain_admin_handler(Config).

end_per_group(_, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus_fresh:clean(),
    escalus:end_per_testcase(CaseName, Config).

% Admin test cases

admin_retrive_user_data(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_retrive_user_data/2).

admin_retrive_user_data(Config, Alice) ->
    Filename = random_filename(Config),
    Res = admin_retrieve_personal_data(escalus_client:username(Alice), escalus_client:server(Alice),
                                       list_to_binary(Filename), Config),
    ParsedResult = get_ok_value([data, gdpr, retrievePersonalData], Res),
    ?assertEqual(<<"Data retrieved">>, ParsedResult),
    FullPath = get_mim_cwd() ++ "/" ++ Filename,
    Dir = make_dir_name(Filename, escalus_client:username(Alice)),
    ct:log("extracting logs ~s", [Dir]),
    ?assertMatch({ok, _}, zip:extract(FullPath, [{cwd, Dir}])).

admin_gdpr_no_user_test(Config) ->
    Res = admin_retrieve_personal_data(<<"AAAA">>, domain(), <<"AAA">>, Config),
    ?assertEqual(<<"user_does_not_exist_error">>, get_err_code(Res)).

domain_admin_retrive_user_data_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_retrive_user_data_no_permission/2).

domain_admin_retrive_user_data_no_permission(Config, Alice) ->
    Filename = random_filename(Config),
    Res = admin_retrieve_personal_data(escalus_client:username(Alice), escalus_client:server(Alice),
                                        list_to_binary(Filename), Config),
    get_unauthorized(Res).

% Helpers

admin_retrieve_personal_data(Username, Domain, ResultFilepath, Config) ->
    Vars = #{<<"username">> => Username, <<"domain">> => Domain,
             <<"resultFilepath">> => ResultFilepath},
    execute_command(<<"gdpr">>, <<"retrievePersonalData">>, Vars, Config).

random_filename(Config) ->
    TCName = atom_to_list(?config(tc_name, Config)),
    TCName ++ "." ++ integer_to_list(erlang:system_time()) ++ ".zip".

get_mim_cwd() ->
    {ok, Cwd} = rpc(mim(), file, get_cwd, []),
    Cwd.

make_dir_name(Filename, User) when is_binary(User) ->
    make_dir_name(Filename, binary_to_list(User));
make_dir_name(Filename, User) when is_list(User) ->
    Filename ++ "." ++ User ++ ".unzipped".
