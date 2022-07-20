-module(graphql_gdpr_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1]).
-import(distributed_helper, [mim/0, rpc/4]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_gdpr}].

groups() ->
    [{admin_gdpr, [], admin_stats_handler()}].

admin_stats_handler() ->
    [admin_gdpr_test,
     admin_gdpr_no_user_test].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    graphql_helper:init_admin_handler(Config).

end_per_group(_, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus_fresh:clean(),
    escalus:end_per_testcase(CaseName, Config).

% Admin test cases

admin_gdpr_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_gdpr_test/2).

admin_gdpr_test(Config, Alice) ->
    Filename = random_filename(Config),
    Vars = #{<<"username">> => escalus_client:username(Alice),
             <<"domain">> => escalus_client:server(Alice),
             <<"resultFilepath">> => list_to_binary(Filename)},
    Result = admin_retrieve_personal_data(Config, Vars),
    ParsedResult = ok_result(<<"gdpr">>, <<"retrievePersonalData">>, Result),
    ?assertEqual(<<"Data retrieved">>, ParsedResult),
    FullPath = get_mim_cwd() ++ "/" ++ Filename,
    Dir = make_dir_name(Filename, escalus_client:username(Alice)),
    ct:log("extracting logs ~s", [Dir]),
    ?assertMatch({ok, _}, zip:extract(FullPath, [{cwd, Dir}])).

admin_gdpr_no_user_test(Config) ->
    Vars = #{<<"username">> => <<"AAAA">>, <<"domain">> => domain(),
             <<"resultFilepath">> => <<"AAA">>},
    Result = admin_retrieve_personal_data(Config, Vars),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, Result),
    ?assertEqual(<<"user_does_not_exist_error">>, ParsedResult).

% Helpers

admin_retrieve_personal_data(Config, Vars) ->
    Query = <<"query Q1($username: String!, $domain: String!, $resultFilepath: String!)
                   {gdpr{retrievePersonalData(username: $username, domain: $domain,
                                              resultFilepath: $resultFilepath)}}">>,
    Body = #{query => Query, operationName => <<"Q1">>, variables => Vars},
    execute_auth(Body, Config).

error_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What2, maps:get(What1, Data)).

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

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
