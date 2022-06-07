-module(graphql_http_upload_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(domain_helper, [host_type/0]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("../../include/mod_roster.hrl").

-define(S3_HOSTNAME, <<"http://bucket.s3-eu-east-25.example.com">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_http_upload},
     {group, admin_not_configured_http_upload},
     {group, admin_http_upload}].

groups() ->
    [{user_http_upload, [], user_http_upload_handler()},
     {admin_not_configured_http_upload, [], admin_not_configured_http_upload_handler()},
     {user_not_configured_http_upload, [], user_not_configured_http_upload_handler()},
     {admin_http_upload, [], admin_http_upload_handler()}].

user_http_upload_handler() ->
    [user_get_url_test].

admin_not_configured_http_upload_handler() ->
    [admin_not_configured_http_upload].

user_not_configured_http_upload_handler() ->
    [user_not_configured_http_upload].

admin_http_upload_handler() ->
    [admin_get_url_test].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(admin_http_upload, Config) ->
    dynamic_modules:start(host_type(), mod_http_upload, create_opts(?S3_HOSTNAME, true)),
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_not_configured_http_upload, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(user_http_upload, Config) ->
    dynamic_modules:start(host_type(), mod_http_upload, create_opts(?S3_HOSTNAME, true)),
    [{schema_endpoint, user} | Config].

end_per_group(_, _Config) ->
    dynamic_modules:stop(host_type(), mod_http_upload),
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
    Vars = #{<<"filename">> => <<"test">>, <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    Body = #{query => user_get_url(), operationName => <<"M1">>, variables => Vars},
    execute_user(Body, Alice, Config).

admin_get_url_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_url_test/2).

admin_get_url_test(Config, Alice) ->
    Vars = #{<<"user">> => user_to_bin(Alice), <<"filename">> => <<"test">>, <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    Body = #{query => admin_get_url(), operationName => <<"M1">>, variables => Vars},
    execute_auth(Body, Config).

admin_not_configured_http_upload(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_not_configured_http_upload/2).

admin_not_configured_http_upload(Config, Alice) ->
    Vars = #{<<"user">> => user_to_bin(Alice), <<"filename">> => <<"test">>, <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    Body = #{query => admin_get_url(), operationName => <<"M1">>, variables => Vars},
    execute_auth(Body, Config).

user_not_configured_http_upload(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_not_configured_http_upload/2).

user_not_configured_http_upload(Config, Alice) ->
    Vars = #{<<"filename">> => <<"test">>, <<"size">> => 123, <<"contentType">> => <<"Test">>, <<"timeout">> => 123},
    Body = #{query => user_get_url(), operationName => <<"M1">>, variables => Vars},
    execute_user(Body, Alice, Config).

user_get_url() ->
    <<"mutation M1($filename: String!, $size: Int!, $contentType: String!, $timeout: Int!)",
      "{httpUpload{getUrl(filename: $filename, size: $size, contentType: $contentType, timeout: $timeout)}}">>.

admin_get_url() ->
    <<"mutation M1($user: JID!, $filename: String!, $size: Int!, $contentType: String!, $timeout: Int!)",
      "{httpUpload{getUrl(user: $user, filename: $filename, size: $size, contentType: $contentType, timeout: $timeout)}}">>.
