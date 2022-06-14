-module(graphql_http_upload_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include("../../include/mod_roster.hrl").

-define(S3_HOSTNAME, <<"http://bucket.s3-eu-east-25.example.com">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_http_upload},
     {group, user_http_upload_not_configured},
     {group, admin_http_upload},
     {group, admin_http_upload_not_configured}].

groups() ->
    [{user_http_upload, [], user_http_upload_handler()},
     {user_http_upload_not_configured, [], user_http_upload_not_configured_handler()},
     {admin_http_upload, [], admin_http_upload_handler()},
     {admin_http_upload_not_configured, [], admin_http_upload_not_configured_handler()}].

user_http_upload_handler() ->
    [user_get_url_test,
     user_get_url_zero_size,
     user_get_url_too_large_size,
     user_get_url_zero_timeout].

user_http_upload_not_configured_handler() ->
    [user_http_upload_not_configured].

admin_http_upload_handler() ->
    [admin_get_url_test,
     admin_get_url_zero_size,
     admin_get_url_too_large_size,
     admin_get_url_zero_timeout,
     admin_get_url_no_domain].

admin_http_upload_not_configured_handler() ->
    [admin_http_upload_not_configured].

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(user_http_upload, Config) ->
    dynamic_modules:ensure_modules(host_type(),
        [{mod_http_upload, create_opts(?S3_HOSTNAME, true)}]),
    [{schema_endpoint, user} | Config];
init_per_group(user_http_upload_not_configured, Config) ->
    dynamic_modules:ensure_modules(host_type(), [{mod_http_upload, stopped}]),
    [{schema_endpoint, user} | Config];
init_per_group(admin_http_upload, Config) ->
    dynamic_modules:ensure_modules(host_type(),
        [{mod_http_upload, create_opts(?S3_HOSTNAME, true)}]),
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_http_upload_not_configured, Config) ->
    dynamic_modules:ensure_modules(host_type(), [{mod_http_upload, stopped}]),
    graphql_helper:init_admin_handler(Config).

end_per_group(_, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

create_opts(Host, AddAcl) ->
    config_parser_helper:mod_config(mod_http_upload,
        #{
            max_file_size => 1234,
            s3 => #{
                bucket_url => Host,
                add_acl => AddAcl,
                region => <<"eu-east-25">>,
                access_key_id => <<"AKIAIAOAONIULXQGMOUA">>,
                secret_access_key => <<"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">>
            }
        }).

% User test cases

user_get_url_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_url_test/2).

user_get_url_test(Config, Alice) ->
    Vars = #{<<"filename">> => <<"test">>, <<"size">> => 123,
             <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = user_send_request(Config, Vars, Alice),
    ParsedResult = ok_result(<<"httpUpload">>, <<"getUrl">>, GraphQlRequest),
    #{<<"PutUrl">> := PutURL, <<"GetUrl">> := GetURL, <<"Header">> := _Headers} = ParsedResult,
    ?assertMatch({_, _}, binary:match(PutURL, [?S3_HOSTNAME])),
    ?assertMatch({_, _}, binary:match(GetURL, [?S3_HOSTNAME])).

user_get_url_zero_size(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_url_zero_size/2).

user_get_url_zero_size(Config, Alice) ->
    Vars = #{<<"filename">> => <<"test">>, <<"size">> => 0,
             <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = user_send_request(Config, Vars, Alice),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"size_error">>, ParsedResult).

user_get_url_too_large_size(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_url_too_large_size/2).

user_get_url_too_large_size(Config, Alice) ->
    Vars = #{<<"filename">> => <<"test">>, <<"size">> => 100000,
             <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = user_send_request(Config, Vars, Alice),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"file_too_large_error">>, ParsedResult).

user_get_url_zero_timeout(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_url_zero_timeout/2).

user_get_url_zero_timeout(Config, Alice) ->
    Vars = #{<<"filename">> => <<"test">>, <<"size">> => 123,
             <<"contentType">> => <<"Test">>, <<"timeout">> => 0},
    GraphQlRequest = user_send_request(Config, Vars, Alice),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"timeout_error">>, ParsedResult).

user_http_upload_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_http_upload_not_configured/2).

user_http_upload_not_configured(Config, Alice) ->
    Vars = #{<<"filename">> => <<"test">>, <<"size">> => 123,
             <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = user_send_request(Config, Vars, Alice),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"module_not_loaded_error">>, ParsedResult).

% Admin test cases

admin_get_url_test(Config) ->
    Vars = #{<<"domain">> => domain(), <<"filename">> => <<"test">>,
             <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = admin_send_request(Config, Vars),
    ParsedResult = ok_result(<<"httpUpload">>, <<"getUrl">>, GraphQlRequest),
    #{<<"PutUrl">> := PutURL, <<"GetUrl">> := GetURL, <<"Header">> := _Headers} = ParsedResult,
    ?assertMatch({_, _}, binary:match(PutURL, [?S3_HOSTNAME])),
    ?assertMatch({_, _}, binary:match(GetURL, [?S3_HOSTNAME])).

admin_get_url_zero_size(Config) ->
    Vars = #{<<"domain">> => domain(), <<"filename">> => <<"test">>,
             <<"size">> => 0, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = admin_send_request(Config, Vars),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"size_error">>, ParsedResult).

admin_get_url_too_large_size(Config) ->
    Vars = #{<<"domain">> => domain(), <<"filename">> => <<"test">>,
             <<"size">> => 1000000, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = admin_send_request(Config, Vars),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"file_too_large_error">>, ParsedResult).

admin_get_url_zero_timeout(Config) ->
    Vars = #{<<"domain">> => domain(), <<"filename">> => <<"test">>,
             <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 0},
    GraphQlRequest = admin_send_request(Config, Vars),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"timeout_error">>, ParsedResult).

admin_get_url_no_domain(Config) ->
    Vars = #{<<"domain">> => <<"AAAAA">>, <<"filename">> => <<"test">>,
             <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = admin_send_request(Config, Vars),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"domain_not_found">>, ParsedResult).

admin_http_upload_not_configured(Config) ->
    Vars = #{<<"domain">> => domain(), <<"filename">> => <<"test">>,
             <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    GraphQlRequest = admin_send_request(Config, Vars),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"module_not_loaded_error">>, ParsedResult).

% Helpers

user_get_url() ->
    <<"mutation M1($filename: String!, $size: Int!,
                   $contentType: String!, $timeout: Int!)",
          "{httpUpload{getUrl(filename: $filename, size: $size,
                              contentType: $contentType, timeout: $timeout)
                {PutUrl, GetUrl, Header}}}">>.

admin_get_url() ->
    <<"mutation M1($domain: String!, $filename: String!, $size: Int!,
                   $contentType: String!, $timeout: Int!)",
          "{httpUpload{getUrl(domain: $domain, filename: $filename,
                       size: $size, contentType: $contentType, timeout: $timeout)
                {PutUrl, GetUrl, Header}}}">>.

user_send_request(Config, Vars, User) ->
    Body = #{query => user_get_url(), operationName => <<"M1">>, variables => Vars},
    execute_user(Body, User, Config).

admin_send_request(Config, Vars) ->
    Body = #{query => admin_get_url(), operationName => <<"M1">>, variables => Vars},
    execute_auth(Body, Config).

error_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What2, maps:get(What1, Data)).

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

url_contains(UrlType, Filename, Result) ->
    Url = extract_url(Result, UrlType),
    binary:match(Url, Filename) =/= nomatch.

extract_url(Result, UrlType) ->
    exml_query:path(Result, [{element, <<"slot">>}, {element, UrlType}, {attr, <<"url">>}]).
