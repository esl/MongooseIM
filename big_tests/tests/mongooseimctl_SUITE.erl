%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongooseimctl_SUITE).
-compile([export_all, nowarn_export_all, nowarn_shadow_vars]).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(mongooseimctl_helper, [mongooseimctl/3, rpc_call/3]).
-import(mongoose_helper, [auth_modules/0]).
-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).
-import(domain_helper, [host_type/0, domain/0]).
-import(config_parser_helper, [config/2, default_mod_config/1]).

-define(HTTP_UPLOAD_FILENAME, "tmp.txt").
-define(HTTP_UPLOAD_FILESIZE, "5").
-define(HTTP_UPLOAD_TIMEOUT, "666").
-define(HTTP_UPLOAD_PARAMS(ContentType), ?HTTP_UPLOAD_PARAMS(?HTTP_UPLOAD_FILENAME,
                                                             ?HTTP_UPLOAD_FILESIZE,
                                                             ContentType,
                                                             ?HTTP_UPLOAD_TIMEOUT)).
-define(HTTP_UPLOAD_PARAMS_WITH_FILESIZE(X), ?HTTP_UPLOAD_PARAMS(?HTTP_UPLOAD_FILENAME, X,
                                                                 "", ?HTTP_UPLOAD_TIMEOUT)).
-define(HTTP_UPLOAD_PARAMS_WITH_TIMEOUT(X), ?HTTP_UPLOAD_PARAMS(?HTTP_UPLOAD_FILENAME,
                                                                ?HTTP_UPLOAD_FILESIZE, "", X)).
-define(HTTP_UPLOAD_PARAMS(FileName, FileSize, ContentType, Timeout),
    [domain(), FileName, FileSize, ContentType, Timeout]).

-define(CTL_ERROR(Messsage), "Error: \"" ++ Messsage ++ "\"\n").
-define(HTTP_UPLOAD_NOT_ENABLED_ERROR, ?CTL_ERROR("mod_http_upload is not loaded for this host")).
-define(HTTP_UPLOAD_FILESIZE_ERROR, ?CTL_ERROR("size must be positive integer")).
-define(HTTP_UPLOAD_TIMEOUT_ERROR, ?CTL_ERROR("timeout must be positive integer")).

-define(S3_BUCKET_URL, "http://localhost:9000/mybucket/").
-define(S3_REGION, "eu-east-25").
-define(S3_ACCESS_KEY_ID, "AKIAIAOAONIULXQGMOUA").

%%Prefix MUST be a constant string, otherwise it results in compilation error
-define(GET_URL(Prefix, Sting), fun() -> Prefix ++ URL = Sting, URL end()).

%% The following is an example presigned URL:
%%
%%     https://s3.amazonaws.com/examplebucket/test.txt
%%     ?X-Amz-Algorithm=AWS4-HMAC-SHA256
%%     &X-Amz-Credential=<your-access-key-id>/20130721/us-east-1/s3/aws4_request
%%     &X-Amz-Date=20130721T201207Z
%%     &X-Amz-Expires=86400
%%     &X-Amz-SignedHeaders=host
%%     &X-Amz-Signature=<signature-value>
%%
%% for more details see
%%     https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
-define(S3_BASE_URL_REGEX, "^"?S3_BUCKET_URL".+/"?HTTP_UPLOAD_FILENAME).
-define(S3_ALGORITHM_REGEX, "[?&]X-Amz-Algorithm=AWS4-HMAC-SHA256(&|$)").
-define(S3_CREDENTIAL_REGEX,
        % X-Amz-Credential=<your-access-key-id>/<date>/<AWS-region>/<AWS-service>/aws4_request
        "[?&]X-Amz-Credential="?S3_ACCESS_KEY_ID"%2F[0-9]{8}%2F"?S3_REGION"%2Fs3%2Faws4_request(&|$)").
-define(S3_DATE_REGEX, "X-Amz-Date=[0-9]{8}T[0-9]{6}Z(&|$)").
-define(S3_EXPIRATION_REGEX, "[?&]X-Amz-Expires="?HTTP_UPLOAD_TIMEOUT"(&|$)").
-define(S3_SIGNED_HEADERS, "[?&]X-Amz-SignedHeaders=content-length%3Bhost(&|$)").
-define(S3_SIGNED_HEADERS_WITH_ACL,
        "[?&]X-Amz-SignedHeaders=content-length%3Bhost%3Bx-amz-acl(&|$)").
-define(S3_SIGNED_HEADERS_WITH_CONTENT_TYPE,
        "[?&]X-Amz-SignedHeaders=content-length%3Bcontent-type%3Bhost(&|$)").
-define(S3_SIGNED_HEADERS_WITH_CONTENT_TYPE_AND_ACL,
        "[?&]X-Amz-SignedHeaders=content-length%3Bcontent-type%3Bhost%3Bx-amz-acl(&|$)").
-define(S3_SIGNATURE_REGEX, "[?&]X-Amz-Signature=[^&]+(&|$)").


-record(offline_msg, {us, timestamp, expire, from, to, packet, permanent_fields = []}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, accounts},
     {group, sessions},
     {group, vcard},
     {group, roster},
     {group, roster_advanced},
     {group, last},
     {group, private},
     {group, stanza},
     {group, stats},
     {group, basic},
     {group, upload},
     {group, graphql},
     {group, help},
     {group, server}
    ].

groups() ->
    [{accounts, [sequence], accounts()},
     {sessions, [sequence], sessions()},
     {vcard, [sequence], vcard()},
     {roster, [sequence], roster()},
     {last, [sequence], last()},
     {private, [sequence], private()},
     {stanza, [sequence], stanza()},
     {roster_advanced, [sequence], roster_advanced()},
     {basic, [sequence], basic()},
     {stats, [sequence], stats()},
     {upload, [], upload()},
     {upload_with_acl, [], upload_enabled()},
     {upload_without_acl, [], upload_enabled()},
     {graphql, [], graphql()},
     {help, [], help()},
     {server, [], server()}].

basic() ->
    [simple_register,
     simple_unregister,
     register_twice,
     backup_restore_mnesia,
     restore_mnesia_wrong,
     dump_and_load,
     load_mnesia_wrong,
     dump_table,
     get_loglevel,
     remove_old_messages_test,
     remove_expired_messages_test].

accounts() -> [change_password, check_password_hash, check_password,
               check_account, ban_account, num_active_users, delete_old_users,
               delete_old_users_vhost].

sessions() -> [num_resources_num, kick_session, status,
               sessions_info, set_presence].

vcard() -> [vcard_rw, vcard2_rw, vcard2_multi_rw].

roster() -> [rosteritem_rw,
             presence_after_add_rosteritem,
             push_roster,
             push_roster_all,
             push_roster_alltoall].

roster_advanced() -> [process_rosteritems_list_simple,
                      process_rosteritems_list_nomatch,
                      process_rosteritems_list_advanced1,
                      process_rosteritems_list_advanced2,
                      process_rosteritems_delete_advanced,
                      process_rosteritems_delete_advanced2].

last() -> [set_last].

private() -> [private_rw].

stanza() -> [send_message, send_message_wrong_jid, send_stanza, send_stanzac2s_wrong].

stats() -> [stats_global, stats_host].

upload() ->
    [upload_not_enabled, upload_wrong_filesize, upload_wrong_timeout,
     {group, upload_with_acl}, {group, upload_without_acl}].

upload_enabled() ->
    [upload_returns_correct_urls_without_content_type,
     upload_returns_correct_urls_with_content_type,
     real_upload_without_content_type,
     real_upload_with_content_type].

graphql() ->
    [graphql_wrong_arguments_number,
     can_execute_admin_queries_with_permissions,
     can_handle_execution_error,
     graphql_error_unknown_command_with_args,
     graphql_error_unknown_command_without_args,
     graphql_no_command,
     graphql_error_invalid_args,
     graphql_error_invalid_arg_value,
     graphql_error_no_arg_value,
     graphql_error_missing_args,
     graphql_error_unknown_arg,
     graphql_arg_help,
     graphql_command].

help() ->
    [default_help,
     old_help].

server() ->
    [server_status,
     server_is_started].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config) ->
    TemplatePath = filename:join(?config(mim_data_dir, Config), "roster.template"),
    AuthMods = auth_modules(),
    Node = mim(),
    Config1 = ejabberd_node_utils:init(Node, Config),
    Config2 = escalus:init_per_suite([{ctl_auth_mods, AuthMods},
                                      {roster_template, TemplatePath} | Config1]),
    dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_last,
        default_mod_config(mod_last)}]),
    dynamic_modules:ensure_modules(domain_helper:secondary_host_type(),
        [{mod_last, default_mod_config(mod_last)}]),
    prepare_roster_template(TemplatePath, domain()),
    %% dump_and_load requires at least one mnesia table
    %% ensure, that passwd table is available
    catch rpc_call(ejabberd_auth_internal, start, [host_type()]),
    escalus:create_users(Config2, escalus:get_users([alice, mike, bob, kate])).

prepare_roster_template(TemplatePath, Domain) ->
    {ok, [RosterIn]} = file:consult(TemplatePath ++ ".in"),
    DomainStr = binary_to_list(Domain),
    Roster = [{User, DomainStr, Group, Name} || {User, Group, Name} <- RosterIn],
    FormattedRoster = io_lib:format("~tp.~n", [Roster]),
    file:write_file(TemplatePath, FormattedRoster).

end_per_suite(Config) ->
    Config1 = lists:keydelete(ctl_auth_mods, 1, Config),
    delete_users(Config1),
    dynamic_modules:stop(domain_helper:host_type(), mod_last),
    dynamic_modules:stop(domain_helper:secondary_host_type(), mod_last),
    escalus:end_per_suite(Config1).

init_per_group(basic, Config) ->
    dynamic_modules:ensure_modules(domain_helper:host_type(),
        [{mod_offline, default_mod_config(mod_offline)}]),
    Config;
init_per_group(private, Config) ->
    dynamic_modules:ensure_modules(domain_helper:host_type(),
                                   [{mod_private, #{iqdisc => one_queue}}]
                                  ),
    Config;
init_per_group(vcard, Config) ->
    case rpc(mim(), gen_mod, get_module_opt, [host_type(), mod_vcard, backend, mnesia]) of
        ldap ->
            {skip, vcard_set_not_supported_with_ldap};
        _ ->
            Config
    end;
init_per_group(roster_advanced, Config) ->
    case rpc(mim(), gen_mod, get_module_opt, [host_type(), mod_roster, backend, mnesia]) of
        mnesia ->
            Config;
        _ ->
            {skip, command_process_rosteritems_supports_only_mnesia}
    end;
init_per_group(upload_without_acl, Config) ->
    dynamic_modules:start(host_type(), mod_http_upload, mod_http_upload_config(false)),
    [{with_acl, false} | Config];
init_per_group(upload_with_acl, Config) ->
    dynamic_modules:start(host_type(), mod_http_upload, mod_http_upload_config(true)),
    [{with_acl, true} | Config];
init_per_group(_GroupName, Config) ->
    Config.

mod_http_upload_config(AddAcl) ->
    config([modules, mod_http_upload],
        #{max_file_size => 1234,
          s3 => #{bucket_url => <<?S3_BUCKET_URL>>,
                  add_acl => AddAcl,
                  region => <<?S3_REGION>>,
                  access_key_id => <<?S3_ACCESS_KEY_ID>>,
                  secret_access_key => <<"CG5fGqG0/n6NCPJ10FylpdgRnuV52j8IZvU7BSj8">>
                 }
        }).

end_per_group(basic, Config) ->
    dynamic_modules:stop(domain_helper:host_type(), mod_offline),
    Config;
end_per_group(private, Config) ->
    dynamic_modules:stop(domain_helper:host_type(), mod_private),
    Config;
end_per_group(Rosters, Config) when (Rosters == roster) or (Rosters == roster_advanced) ->
    TemplatePath = escalus_config:get_config(roster_template, Config),
    RegUsers = [atom_to_list(U) || {U, _} <- escalus_config:get_config(escalus_users, Config)],
    {ok, [Roster]} = file:consult(TemplatePath),
    C = fun({U, S, _, _}) ->
        case lists:member(U, RegUsers) of
            true ->
                SB = string_to_binary(S),
                UB = string_to_binary(U),
                Acc = mongoose_helper:new_mongoose_acc(SB),
                rpc(mim(), mongoose_hooks, remove_user, [Acc, SB, UB]);
            _ ->
               ok
        end
    end,
    lists:foreach(C, Roster),
    Config;
end_per_group(UploadGroup, Config) when UploadGroup =:= upload_without_acl;
                                        UploadGroup =:= upload_with_acl ->
    dynamic_modules:stop(host_type(), mod_http_upload),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

get_registered_users() ->
    rpc(mim(), ejabberd_auth, get_vh_registered_users, [domain()]).

init_per_testcase(CaseName, Config)
  when CaseName == delete_old_users_vhost
       orelse CaseName == stats_global
       orelse CaseName == stats_host ->
    {_, AuthMods} = lists:keyfind(ctl_auth_mods, 1, Config),
    case lists:member(ejabberd_auth_ldap, AuthMods) of
        true -> {skip, "not supported for LDAP"};
        false -> escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(check_password_hash, Config) ->
    {_, AuthMods} = lists:keyfind(ctl_auth_mods, 1, Config),
    case lists:member(ejabberd_auth_ldap, AuthMods) of
        true ->
            {skip, not_fully_supported_with_ldap};
        false ->
            AuthOpts = mongoose_helper:auth_opts_with_password_format(plain),
            Config1 = mongoose_helper:backup_and_set_config_option(Config, {auth, host_type()},
                                                                   AuthOpts),
            Config2 = escalus:create_users(Config1, escalus:get_users([carol])),
            escalus:init_per_testcase(check_password_hash, Config2)
    end;
init_per_testcase(delete_old_users, Config) ->
    {_, AuthMods} = lists:keyfind(ctl_auth_mods, 1, Config),
    case lists:member(ejabberd_auth_ldap, AuthMods) of
        true -> {skip, not_fully_supported_with_ldap};
        false -> escalus:init_per_testcase(delete_old_users, Config)
    end;
init_per_testcase(CaseName, Config) when CaseName == real_upload_without_content_type;
                                         CaseName == real_upload_with_content_type ->
    case mongoose_helper:should_minio_be_running(Config) of
        true -> escalus:init_per_testcase(CaseName, Config);
        false -> {skip, "minio is not running"}
    end;
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(delete_old_users, Config) ->
    Users = escalus_users:get_users([alice, bob, kate, mike]),
    lists:foreach(fun({_User, UserSpec}) ->
                {Username, Domain, Pass} = get_user_data(UserSpec, Config),
                JID = mongoose_helper:make_jid(Username, Domain),
                rpc(mim(), ejabberd_auth, try_register, [JID, Pass])
        end, Users),
    escalus:end_per_testcase(delete_old_users, Config);
end_per_testcase(check_password_hash, Config) ->
    mongoose_helper:restore_config(Config),
    escalus:delete_users(Config, escalus:get_users([carol]));
end_per_testcase(CaseName, Config) ->
    %% Because kick_session fails with unexpected stanza received:
    %% <presence from="alicE@localhost/res3"
    %%     to="alice@localhost/res1" type="unavailable" />
    %% TODO: Remove when escalus learns how to automatically deal
    %% with 'unavailable' stanzas on client stop.
    mongoose_helper:kick_everyone(),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% http upload tests
%%--------------------------------------------------------------------
upload_not_enabled(Config) ->
    Ret = mongooseimctl("http_upload", ?HTTP_UPLOAD_PARAMS("text/plain"), Config),
    ?assertEqual({?HTTP_UPLOAD_NOT_ENABLED_ERROR, 1}, Ret).

upload_wrong_filesize(Config) ->
    Ret = mongooseimctl("http_upload", ?HTTP_UPLOAD_PARAMS_WITH_FILESIZE("0"), Config),
    ?assertEqual({?HTTP_UPLOAD_FILESIZE_ERROR, 1}, Ret),
    Ret = mongooseimctl("http_upload", ?HTTP_UPLOAD_PARAMS_WITH_FILESIZE("-1"), Config),
    ?assertEqual({?HTTP_UPLOAD_FILESIZE_ERROR, 1}, Ret).

upload_wrong_timeout(Config) ->
    Ret = mongooseimctl("http_upload", ?HTTP_UPLOAD_PARAMS_WITH_TIMEOUT("0"), Config),
    ?assertEqual({?HTTP_UPLOAD_TIMEOUT_ERROR, 1}, Ret),
    Ret = mongooseimctl("http_upload", ?HTTP_UPLOAD_PARAMS_WITH_TIMEOUT("-1"), Config),
    ?assertEqual({?HTTP_UPLOAD_TIMEOUT_ERROR, 1}, Ret).

upload_returns_correct_urls_with_content_type(Config) ->
    upload_returns_correct_urls(Config, "text/plain").

upload_returns_correct_urls_without_content_type(Config) ->
    upload_returns_correct_urls(Config, "").

real_upload_with_content_type(Config) ->
    real_upload(Config, "text/plain").

real_upload_without_content_type(Config) ->
    real_upload(Config, "").

upload_returns_correct_urls(Config, ContentType) ->
    HttpUploadParams = ?HTTP_UPLOAD_PARAMS(ContentType),
    {Output, 0} = mongooseimctl("http_upload", HttpUploadParams, Config),
    {PutURL, GetURL} = get_urls(Output),
    WithACL = proplists:get_value(with_acl, Config),
    check_urls(PutURL, GetURL, WithACL, ContentType).

get_urls(Output) ->
    [PutStr, GetStr | _] = string:split(Output, "\n", all),
    PutURL = ?GET_URL("PutURL: ", PutStr),
    GetURL = ?GET_URL("GetURL: ", GetStr),
    {PutURL, GetURL}.

check_urls(PutURL, GetURL, WithACL, ContentType) ->
    check_bucket_url_and_filename(put, PutURL),
    check_bucket_url_and_filename(get, GetURL),
    check_substring(?S3_ALGORITHM_REGEX, PutURL),
    check_substring(?S3_CREDENTIAL_REGEX, PutURL),
    check_substring(?S3_DATE_REGEX, PutURL),
    check_substring(?S3_EXPIRATION_REGEX, PutURL),
    SignedHeadersRegex = signed_headers_regex(WithACL, ContentType),
    check_substring(SignedHeadersRegex, PutURL),
    check_substring(?S3_SIGNATURE_REGEX, PutURL).

check_bucket_url_and_filename(Type, Url) ->
    UrlRegex = case Type of
                   get -> ?S3_BASE_URL_REGEX"$";
                   put -> ?S3_BASE_URL_REGEX"\?.*"
               end,
    ct:log("check_bucket_url_and_filename type=~p url=~p regex=~p",
           [Type, Url, UrlRegex]),
    ?assertMatch({match, [{0, _}]}, re:run(Url, UrlRegex)).

check_substring(SubString, String) ->
    ?assertMatch({match, [_]}, re:run(String, SubString, [global])).

signed_headers_regex(false, "") -> ?S3_SIGNED_HEADERS;
signed_headers_regex(false, _)  -> ?S3_SIGNED_HEADERS_WITH_CONTENT_TYPE;
signed_headers_regex(true, "")  -> ?S3_SIGNED_HEADERS_WITH_ACL;
signed_headers_regex(true, _)   -> ?S3_SIGNED_HEADERS_WITH_CONTENT_TYPE_AND_ACL.

real_upload(Config, ContentType) ->
    #{node := Node} = mim(),
    BinPath = distributed_helper:bin_path(Node, Config),
    UploadScript = filename:join(?config(mim_data_dir, Config), "test_file_upload.sh"),
    Ret = mongooseimctl_helper:run(UploadScript, [ContentType], [{cd, BinPath}]),
    ?assertMatch({_, 0}, Ret),
    ok.
%%--------------------------------------------------------------------
%% service_admin_extra_accounts tests
%%--------------------------------------------------------------------

change_password(Config) ->
    {User, Domain, OldPassword} = get_user_data(alice, Config),
    mongooseimctl("change_password", [User, Domain, <<OldPassword/binary, $2>>], Config),
    {error, {connection_step_failed, _, _}} = escalus_client:start_for(Config, alice, <<"newres">>),
    mongooseimctl("change_password", [User, Domain, OldPassword], Config),
    {ok, Alice2} = escalus_client:start_for(Config, alice, <<"newres2">>),
    escalus_client:stop(Config, Alice2).

check_password_hash(Config) ->
    {User, Domain, Pass} = get_user_data(carol, Config),
    MD5Hash = get_md5(Pass),
    MD5HashBad = get_md5(<<Pass/binary, "bad">>),
    SHAHash = get_sha(Pass),

    {_, 0} = mongooseimctl("check_password_hash", [User, Domain, MD5Hash, "md5"], Config),
    {_, ErrCode} = mongooseimctl("check_password_hash", [User, Domain, MD5HashBad, "md5"], Config),
    true = (ErrCode =/= 0), %% Must return code other than 0
    {_, 0} = mongooseimctl("check_password_hash", [User, Domain, SHAHash, "sha"], Config).

check_password(Config) ->
    {User, Domain, Pass} = get_user_data(alice, Config),
    MetricName = [backends, auth, check_password],
    OldValue = get_metric(MetricName),
    {_, 0} = mongooseimctl("check_password", [User, Domain, Pass], Config),
    {_, ErrCode} = mongooseimctl("check_password", [User, Domain, <<Pass/binary, "Bad">>], Config),
    mongoose_helper:wait_until(
      fun() -> get_metric(MetricName) end, true,
      #{validator => fun(NewValue) -> OldValue =/= NewValue end, name => ?FUNCTION_NAME}),
    true = (ErrCode =/= 0). %% Must return code other than 0

get_metric(MetricName) ->
    HostType = domain_helper:host_type(mim),
    HostTypePrefix = domain_helper:make_metrics_prefix(HostType),
    {ok, Value} = rpc(mim(), mongoose_metrics, get_metric_value, [[HostTypePrefix | MetricName]]),
    Value.

check_account(Config) ->
    {User, Domain, _Pass} = get_user_data(alice, Config),

    {_, 0} = mongooseimctl("check_account", [User, Domain], Config),
    {_, ErrCode} = mongooseimctl("check_account", [<<User/binary, "Bad">>, Domain], Config),
    true = (ErrCode =/= 0). %% Must return code other than 0

ban_account(Config) ->
    {User, Domain, Pass} = get_user_data(mike, Config),

    {ok, Mike} = escalus_client:start_for(Config, mike, <<"newres">>),
    {_, 0} = mongooseimctl("ban_account", [User, Domain, "SomeReason"], Config),
    escalus:assert(is_stream_error, [<<"conflict">>, <<"SomeReason">>],
                   escalus:wait_for_stanza(Mike)),
    {error, {connection_step_failed, _, _}} = escalus_client:start_for(Config, mike, <<"newres2">>),
    mongooseimctl("change_password", [User, Domain, Pass], Config),
    escalus_connection:wait_for_close(Mike, 1000),
    escalus_cleaner:remove_client(Config, Mike).

num_active_users(Config) ->
    %% Given some users with recorded last activity timestamps
    {AliceName, Domain, _} = get_user_data(alice, Config),
    {MikeName, Domain, _} = get_user_data(mike, Config),
    {Mega, Secs, _} = os:timestamp(),
    Now = Mega * 1000000 + Secs,
    set_last(AliceName, Domain, Now),
    set_last(MikeName, Domain, Now),
    {SLastActiveBefore, _} = mongooseimctl("num_active_users", [Domain, "5"], Config),
    %% When we artificially remove a user's last activity timestamp in the given period
    TenDaysAgo = Now - 864000,
    set_last(MikeName, Domain, TenDaysAgo),
    %% Then we expect that the number of active users in the last 5 days is one less
    %% than before the change above
    {SLastActiveAfter, _} = mongooseimctl("num_active_users", [Domain, "5"], Config),
    NLastActiveBefore = list_to_integer(string:strip(SLastActiveBefore, both, $\n)),
    NLastActiveAfter = list_to_integer(string:strip(SLastActiveAfter, both, $\n)),
    NLastActiveAfter = NLastActiveBefore - 1.

delete_old_users(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    {BobName, Domain, _} = get_user_data(bob, Config),
    {KateName, Domain, _} = get_user_data(kate, Config),
    {MikeName, Domain, _} = get_user_data(mike, Config),

    {Mega, Secs, _} = os:timestamp(),
    Now = Mega*1000000+Secs,
    set_last(AliceName, Domain, Now),
    set_last(BobName, Domain, Now),
    set_last(MikeName, Domain, Now),
    set_last(KateName, Domain, 0),

    {_, 0} = mongooseimctl("delete_old_users", ["10"], Config),
    {_, 0} = mongooseimctl("check_account", [AliceName, Domain], Config),
    {_, ErrCode} = mongooseimctl("check_account", [KateName, Domain], Config),
    true = (ErrCode =/= 0). %% Must return code other than 0

delete_old_users_vhost(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    {KateName, Domain, KatePass} = get_user_data(kate, Config),
    SecDomain = ct:get_config({hosts, mim, secondary_domain}),

    {Mega, Secs, _} = os:timestamp(),
    Now = Mega*1000000+Secs,
    set_last(AliceName, Domain, Now-86400*30),

    {_, 0} = mongooseimctl("register_identified", [KateName, SecDomain, KatePass], Config),
    {_, 0} = mongooseimctl("check_account", [KateName, SecDomain], Config),
    {_, 0} = mongooseimctl("delete_old_users_vhost", [SecDomain, "10"], Config),
    {_, 0} = mongooseimctl("check_account", [AliceName, Domain], Config),
    {_, ErrCode} = mongooseimctl("check_account", [KateName, SecDomain], Config),
    true = (ErrCode =/= 0). %% Must return code other than 0

%%--------------------------------------------------------------------
%% service_admin_extra_accounts tests
%%--------------------------------------------------------------------

%% Checks both num_resources and resource_num
num_resources_num(Config) ->
    escalus:story(Config, [{alice, 3}, {bob, 1}], fun(_, Alice2, _, _) ->
                {Username, Domain, _} = get_user_data(alice, Config),
                ResName = binary_to_list(escalus_client:resource(Alice2)) ++ "\n",

                {"3\n", _} = mongooseimctl("num_resources", [Username, Domain], Config),
                {ResName, _} = mongooseimctl("resource_num", [Username, Domain, "2"], Config)
        end).

kick_session(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                Username = escalus_client:username(Alice),
                Domain = escalus_client:server(Alice),
                Resource = escalus_client:resource(Alice),
                Args = [Username, Domain, Resource, "Because I can!"],

                {_, 0} = mongooseimctl("kick_session", Args, Config),
                Stanza = escalus:wait_for_stanza(Alice),
                escalus:assert(is_stream_error, [<<"conflict">>, <<"Because I can!">>], Stanza)
        end).

status(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {bob, 1}], fun(User1, User2, User3) ->
                PriDomain = escalus_client:server(User1),
                SecDomain = ct:get_config({hosts, mim, secondary_domain}),
                AwayPresence = escalus_stanza:presence_show(<<"away">>),
                escalus_client:send(User2, AwayPresence),

                {"2\n", _} = mongooseimctl("status_num", ["available"], Config),

                {"2\n", _} = mongooseimctl("status_num_host", [PriDomain, "available"], Config),
                {"0\n", _} = mongooseimctl("status_num_host", [SecDomain, "available"], Config),

                {StatusList, _} = mongooseimctl("status_list", ["available"], Config),
                match_user_status([User1, User3], StatusList),

                {StatusList2, _} = mongooseimctl("status_list_host",
                                               [PriDomain, "available"], Config),
                match_user_status([User1, User3], StatusList2),
                {[], _} = mongooseimctl("status_list_host", [SecDomain, "available"], Config)
        end).

sessions_info(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(User1, User2, User3) ->
                Username1 = escalus_client:username(User1),
                PriDomain = escalus_client:server(User1),
                SecDomain = ct:get_config({hosts, mim, secondary_domain}),
                AwayPresence = escalus_stanza:presence_show(<<"away">>),
                escalus_client:send(User2, AwayPresence),

                {UserList, _} = mongooseimctl("connected_users_info", [], Config),
                match_user_info([User1, User2, User3], UserList),

                {UserList2, _} = mongooseimctl("connected_users_vhost", [PriDomain], Config),
                match_user_info([User1, User2, User3], UserList2),
                {[], _} = mongooseimctl("connected_users_vhost", [SecDomain], Config),

                {UserList3, _} = mongooseimctl("user_sessions_info",
                                               [Username1, PriDomain], Config),
                match_user_info([User1], UserList3)
        end).

set_presence(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                Username = escalus_client:username(Alice),
                Domain = escalus_client:server(Alice),
                Resource = escalus_client:resource(Alice),

                {_, 0} = mongooseimctl("set_presence",
                                     [Username, Domain, Resource,
                                      "available", "away", "mystatus", "10"],
                                     Config),
                Presence = escalus:wait_for_stanza(Alice),
                escalus:assert(is_presence_with_show, [<<"away">>], Presence),
                escalus:assert(is_presence_with_status, [<<"mystatus">>], Presence),
                escalus:assert(is_presence_with_priority, [<<"10">>], Presence)
        end).

%%--------------------------------------------------------------------
%% service_admin_extra_vcard tests
%%--------------------------------------------------------------------

vcard_rw(Config) ->
    {Username, Domain, _} = get_user_data(alice, Config),

    {_, ExitCode} = mongooseimctl("get_vcard", [Username, Domain, "NICKNAME"], Config),
    true = (ExitCode /= 0),

    {_, 0} = mongooseimctl("set_vcard", [Username, Domain, "NICKNAME", "SomeNickname"], Config),
    {"SomeNickname\n", 0} = mongooseimctl("get_vcard", [Username, Domain, "NICKNAME"], Config).

vcard2_rw(Config) ->
    {Username, Domain, _} = get_user_data(alice, Config),

    {_, ExitCode} = mongooseimctl("get_vcard2", [Username, Domain, "ORG", "ORGNAME"], Config),
    true = (ExitCode /= 0),

    {_, 0} = mongooseimctl("set_vcard2", [Username, Domain, "ORG", "ORGNAME", "ESL"], Config),
    {"ESL\n", 0} = mongooseimctl("get_vcard2", [Username, Domain, "ORG", "ORGNAME"], Config).

vcard2_multi_rw(Config) ->
    {Username, Domain, _} = get_user_data(alice, Config),

    {_, ExitCode} = mongooseimctl("get_vcard2_multi", [Username, Domain, "ORG", "ORGUNIT"], Config),
    true = (ExitCode /= 0),

    Args = [Username, Domain, "ORG", "ORGUNIT", "sales;marketing"],
    {_, 0} = mongooseimctl("set_vcard2_multi", Args, Config),
    {OrgUnits0, 0} = mongooseimctl("get_vcard2_multi",
                                   [Username, Domain, "ORG", "ORGUNIT"], Config),
    OrgUnits = string:tokens(OrgUnits0, "\n"),
    2 = length(OrgUnits),
    true = (lists:member("sales", OrgUnits) andalso lists:member("marketing", OrgUnits)).

%%--------------------------------------------------------------------
%% service_admin_extra_vcard tests
%%--------------------------------------------------------------------

rosteritem_rw(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                MikeJid = escalus_users:get_jid(Config, mike),

                {AliceName, Domain, _} = get_user_data(alice, Config),
                {BobName, Domain, _} = get_user_data(bob, Config),
                {MikeName, Domain, _} = get_user_data(mike, Config),

                {_, 0} = add_rosteritem1(AliceName, Domain, BobName, Config),
                {_, 0} = mongooseimctl("add_rosteritem",
                                     [AliceName, Domain, MikeName,
                                      Domain, "My Mike",
                                      "My Group", "both"], Config),

                [Push1, Push2] = escalus:wait_for_stanzas(Alice, 2), % Check roster broadcasts
                escalus:assert(is_roster_set, Push1),
                escalus:assert(roster_contains, [BobJid], Push1),
                escalus:assert(is_roster_set, Push2),
                escalus:assert(roster_contains, [MikeJid], Push2),

                {Items1, 0} = mongooseimctl("get_roster", [AliceName, Domain], Config),
                match_roster([{BobName, Domain, "MyBob", "MyGroup", "both"},
                              {MikeName, Domain, "MyMike", "MyGroup", "both"}], Items1),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                escalus:assert(roster_contains, [BobJid], Roster1),
                escalus:assert(roster_contains, [MikeJid], Roster1),

                {_, 0} = mongooseimctl("delete_rosteritem",
                                     [AliceName, Domain, BobName, Domain],
                                     Config),

                Push3 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_set, Push3),
                escalus:assert(roster_contains, [BobJid], Push3),

                {Items2, 0} = mongooseimctl("get_roster", [AliceName, Domain], Config),
                match_roster([{MikeName, Domain, "MyMike", "MyGroup", "both"}], Items2),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(MikeJid)),  % cleanup
                escalus:wait_for_stanzas(Alice, 2, 5000)
        end).

presence_after_add_rosteritem(Config) ->
     escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
                 BobJid = escalus_users:get_jid(Config, bob),
                 {AliceName, Domain, _} = get_user_data(alice, Config),
                 {BobName, Domain, _} = get_user_data(bob, Config),

                 {_, 0} = add_rosteritem1(AliceName, Domain, BobName, Config),

                 escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
                 escalus:assert(is_presence, escalus:wait_for_stanza(Bob)),

                 escalus:send(Alice, escalus_stanza:roster_remove_contact(BobJid)),  % cleanup
                 %% Wait for stanzas, so they would not end up in the next story
                 escalus:wait_for_stanzas(Alice, 3, 5000)
         end).

push_roster(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                {AliceName, Domain, _} = get_user_data(alice, Config),
                TemplatePath = escalus_config:get_config(roster_template, Config),

                {_, 0} = mongooseimctl("push_roster", [TemplatePath, AliceName, Domain], Config),
                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                escalus:assert(roster_contains, [BobJid], Roster1),

                escalus:send(Alice, escalus_stanza:roster_remove_contact(BobJid)), % cleanup
                escalus:wait_for_stanzas(Alice, 2, 5000)
        end).

process_rosteritems_list_simple(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% given
        Action = "list",
        Subs = "any",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        Contact = string:to_lower(binary_to_list(escalus_client:short_jid(Bob))),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {BobName, Domain, _} = get_user_data(bob, Config),
        %% when
        {_, 0} = add_rosteritem1(AliceName, Domain, BobName, Config),
        _S = escalus:wait_for_stanzas(Alice, 2),
        {R, 0} = mongooseimctl("process_rosteritems", [Action, Subs, Asks, User, Contact], Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*" ++ Contact ++ ".*"),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config)
    end).

process_rosteritems_list_nomatch(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% given
        Action = "list",
        Subs = "from:both",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        Contact =string:to_lower(binary_to_list(escalus_client:short_jid(Bob))),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {BobName, Domain, _} = get_user_data(bob, Config),
        {_, 0} = mongooseimctl("add_rosteritem", [AliceName, Domain, BobName,
                                                Domain, "MyBob", "MyGroup", "to"], Config),
        escalus:wait_for_stanzas(Alice, 2),
        %% when
        {R, 0} = mongooseimctl("process_rosteritems", [Action, Subs, Asks, User, Contact], Config),
        %% then
        nomatch = re:run(R, ".*Matches:.*" ++ Contact ++ ".*"),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, BobName, Domain], Config)
    end).

process_rosteritems_list_advanced1(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {kate, 1}], fun(Alice, Mike, Kate) ->
        %% given
        Action = "list",
        Subs = "from:both",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactsRegexp = ContactMike ++ ":" ++
                         string:substr(binary_to_list(KateName), 1, 2) ++
                         ".*@.*",

        {_, 0} = add_rosteritem2(AliceName, Domain, MikeName, Domain, Config),
        {_, 0} = mongooseimctl("add_rosteritem", [AliceName, Domain, KateName,
                                                Domain, "BestFriend", "MyGroup", "both"], Config),
        escalus:wait_for_stanzas(Alice, 4),
        %% when
        {R, 0} = mongooseimctl("process_rosteritems",
                             [Action, Subs, Asks, User, ContactsRegexp],
                             Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*" ++ ContactMike ++ ".*"),
        {match, _} = re:run(R, ".*Matches:.*" ++ ContactKate ++ ".*"),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config)
    end).

process_rosteritems_delete_advanced(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {kate, 1}], fun(Alice, Mike, Kate) ->
        %% given
        Action = "delete",
        Subs = "from",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactsRegexp = ".*" ++ string:substr(ContactMike, 3) ++
                         ":" ++ string:substr(ContactKate, 1, 2) ++
                         "@" ++ binary_to_list(Domain),
        {_, 0} = mongooseimctl("add_rosteritem", [AliceName, Domain, MikeName,
                                                Domain, "DearMike", "MyGroup", "from"], Config),
        {_, 0} = mongooseimctl("add_rosteritem", [AliceName, Domain, KateName,
                                                Domain, "Friend", "MyGroup", "from"], Config),
        escalus:wait_for_stanzas(Alice, 4),
        %% when
        {R, 0} = mongooseimctl("process_rosteritems",
                             [Action, Subs, Asks, User, ContactsRegexp],
                             Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*" ++ ContactMike ++ ".*"),
        nomatch = re:run(R, ".*Matches:.*" ++ ContactKate ++ ".*"),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config)
    end).

process_rosteritems_list_advanced2(Config) ->
    escalus:story(Config, [{alice, 1}, {mike, 1}, {kate, 1}], fun(Alice, Mike, Kate) ->
        %% given
        Action = "list",
        Subs = "any",
        Asks = "any",
        User = escalus_client:short_jid(Alice),
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactsRegexp = ".*e@lo.*",
        {_, 0} = add_rosteritem2(AliceName, Domain, MikeName, Domain, Config),
        {_, 0} = mongooseimctl("add_rosteritem", [AliceName, Domain, KateName,
                                                Domain, "KateFromSchool",
                                                "MyGroup", "from"], Config),
        escalus:wait_for_stanzas(Alice, 4),
        %% when
        {R, 0} = mongooseimctl("process_rosteritems",
                             [Action, Subs, Asks, User, ContactsRegexp],
                             Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*" ++ ContactMike ++ ".*"),
        {match, _} = re:run(R, ".*Matches:.*" ++ ContactKate ++ ".*"),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config)
    end).

process_rosteritems_delete_advanced2(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {mike, 1}, {kate, 1}],
      fun(Alice, Bob, Mike, Kate) ->
        %% given
        Action = "delete",
        Subs = "to:from",
        Asks = "any",
        User = "al.c[e]@.*host:((b[o]b)|(mike))@loc.*t2",
        {AliceName, Domain, _} = get_user_data(alice, Config),
        {BobName, Domain, _} = get_user_data(bob, Config),
        {MikeName, Domain, _} = get_user_data(mike, Config),
        {KateName, Domain, _} = get_user_data(kate, Config),
        ContactMike = string:to_lower(binary_to_list(escalus_client:short_jid(Mike))),
        ContactKate= string:to_lower(binary_to_list(escalus_client:short_jid(Kate))),
        ContactBob= string:to_lower(binary_to_list(escalus_client:short_jid(Bob))),
        ContactsReg = ".ik[ea]@localho+.*:k@loc.*st:(alice)+@.*:no",
        {_, 0} = mongooseimctl("add_rosteritem",
                             [AliceName, Domain, MikeName,
                              Domain, "DearMike", "MyGroup", "to"],
                             Config),
        {_, 0} = mongooseimctl("add_rosteritem",
                             [AliceName, Domain, KateName,
                              Domain, "HateHerSheHasSoNiceLegs",
                              "MyGroup", "to"], Config),
        {_, 0} = mongooseimctl("add_rosteritem", [BobName, Domain, AliceName,
                                                Domain, "Girlfriend", "MyGroup", "from"], Config),
        escalus:wait_for_stanzas(Alice, 4),
        escalus:wait_for_stanzas(Bob, 2),
        %% when
        {R, 0} = mongooseimctl("process_rosteritems",
                             [Action, Subs, Asks, User, ContactsReg],
                             Config),
        %% then
        {match, _} = re:run(R, ".*Matches:.*" ++ ContactMike ++ ".*"),
        nomatch = re:run(R, ".*Matches:.*" ++ ContactKate ++ ".*"),
        nomatch = re:run(R, ".*Matches:.*" ++ ContactBob ++ ".*"),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, MikeName, Domain], Config),
        {_, 0} = mongooseimctl("delete_rosteritem", [AliceName, Domain, KateName, Domain], Config),
        {_, 0} = mongooseimctl("delete_rosteritem", [BobName, Domain, AliceName, Domain], Config)
    end).

push_roster_all(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
                TemplatePath = escalus_config:get_config(roster_template, Config),

                {_, 0} = mongooseimctl("push_roster_all", [TemplatePath], Config),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_roster_result, Roster1),
                BobJid = escalus_client:short_jid(Bob),
                escalus:assert(roster_contains, [BobJid], Roster1),

                escalus:send(Bob, escalus_stanza:roster_get()),
                Roster2 = escalus:wait_for_stanza(Bob),
                escalus:assert(is_roster_result, Roster2),
                AliceJid = escalus_client:short_jid(Alice),
                escalus:assert(roster_contains, [AliceJid], Roster2),

                escalus:send_and_wait(Alice, escalus_stanza:roster_remove_contact(bob)), % cleanup
                escalus:send_and_wait(Bob, escalus_stanza:roster_remove_contact(alice)) % cleanup
        end).

push_roster_alltoall(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                MikeJid = escalus_users:get_jid(Config, mike),
                KateJid = escalus_users:get_jid(Config, kate),
                {_, Domain, _} = get_user_data(alice, Config),

                {_, 0} = mongooseimctl("push_roster_alltoall", [Domain, "MyGroup"], Config),

                escalus:send(Alice, escalus_stanza:roster_get()),
                Roster = escalus:wait_for_stanza(Alice),

                escalus:assert(is_roster_result, Roster),
                escalus:assert(roster_contains, [BobJid], Roster),
                escalus:assert(roster_contains, [MikeJid], Roster),
                escalus:assert(roster_contains, [KateJid], Roster)
        end).

%%--------------------------------------------------------------------
%% service_admin_extra_last tests
%%--------------------------------------------------------------------

set_last(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                BobJid = escalus_users:get_jid(Config, bob),
                {AliceName, Domain, _} = get_user_data(alice, Config),
                {BobName, Domain, _} = get_user_data(bob, Config),

                {_, 0} = add_rosteritem1(AliceName, Domain, BobName, Config),
                {_, 0} = mongooseimctl("add_rosteritem",
                                     [BobName, Domain, AliceName,
                                      Domain, "MyAlice", "MyGroup", "both"],
                                     Config),

                escalus:wait_for_stanza(Alice), % ignore push

                Now = os:system_time(second),
                TS = integer_to_list(Now - 7200),
                {_, 0} = mongooseimctl("set_last", [BobName, Domain, TS, "Status"], Config),
                escalus:send(Alice, escalus_stanza:last_activity(BobJid)),
                LastAct = escalus:wait_for_stanza(Alice),
                escalus:assert(is_last_result, LastAct),
                Seconds = list_to_integer(binary_to_list(
                            exml_query:path(LastAct, [{element, <<"query">>},
                            {attr, <<"seconds">>}]))),
                true = (( (Seconds > 7100) andalso (Seconds < 7300) ) orelse Seconds),

                {_, 0} = mongooseimctl("delete_rosteritem",
                                     [AliceName, Domain, BobName, Domain],
                                     Config), % cleanup
                {_, 0} = mongooseimctl("delete_rosteritem",
                                     [BobName, Domain, AliceName, Domain],
                                     Config)
        end).

%%--------------------------------------------------------------------
%% service_admin_extra_private tests
%%--------------------------------------------------------------------

private_rw(Config) ->
    {AliceName, Domain, _} = get_user_data(alice, Config),
    XmlEl1 = "<secretinfo xmlns=\"nejmspejs\">1</secretinfo>",
    XmlEl2 = "<secretinfo xmlns=\"inny\">2</secretinfo>",

    {_, 0} = mongooseimctl("private_set", [AliceName, Domain, XmlEl1], Config),
    {_, 0} = mongooseimctl("private_set", [AliceName, Domain, XmlEl2], Config),

    {Result, 0} = mongooseimctl("private_get",
                              [AliceName, Domain, "secretinfo", "nejmspejs"],
                              Config),
    {ok, #xmlel{ name = <<"secretinfo">>, attrs = [{<<"xmlns">>, <<"nejmspejs">>}],
                children = [#xmlcdata{ content = <<"1">> }]}} = exml:parse(list_to_binary(Result)).

%%--------------------------------------------------------------------
%% service_admin_extra_stanza tests
%%--------------------------------------------------------------------

send_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 2}], fun(Alice, Bob1, Bob2) ->
                {_, 0} = mongooseimctl("send_message_chat", [escalus_client:full_jid(Alice),
                                                           escalus_client:full_jid(Bob1),
                                                           "Hi Bob!"], Config),
                Stanza1 = escalus:wait_for_stanza(Bob1),
                escalus:assert(is_chat_message, [<<"Hi Bob!">>], Stanza1),

                {_, 0} = mongooseimctl("send_message_headline",
                                     [escalus_client:full_jid(Alice),
                                      escalus_client:short_jid(Bob1),
                                      "Subj", "Hi Bob!!"], Config),
                Stanza2 = escalus:wait_for_stanza(Bob1),
                Stanza3 = escalus:wait_for_stanza(Bob2),
                escalus:assert(is_headline_message, [<<"Subj">>, <<"Hi Bob!!">>], Stanza2),
                escalus:assert(is_headline_message, [<<"Subj">>, <<"Hi Bob!!">>], Stanza3)
        end).

send_message_wrong_jid(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {_, Err1} = mongooseimctl("send_message_chat", ["@@#$%!!.§§£",
                                                   escalus_client:full_jid(Bob),
                                                   "Hello bobby!"], Config),
        {_, Err2} = mongooseimctl("send_message_headline", ["%%@&@&@==//\///",
                                                       escalus_client:short_jid(Bob),
                                                       "Subj", "Are
                                                       you there?"],
                             Config),
        true = Err1 =/= 0,
        true = Err2 =/= 0,
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob)
    end).

send_stanza(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
                Domain = escalus_client:server(Alice),
                Resource = escalus_client:resource(Alice),
                {BobName, _, _} = get_user_data(bob, Config),
                BobJID = <<BobName/binary, $@, Domain/binary, $/,
                           (escalus_client:resource(Bob))/binary>>,

                Stanza = create_stanza(Alice, BobJID),
                {_, 0} = mongooseimctl("send_stanza_c2s",
                       [BobName, Domain, Resource, Stanza],
                       Config),

                Message = escalus:wait_for_stanza(Alice),
                escalus:assert(is_chat_message, [<<"Hi">>], Message),
                escalus:assert(is_stanza_from, [Bob], Message)
        end).

send_stanzac2s_wrong(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Domain = escalus_client:server(Alice),
        Resource = escalus_client:resource(Alice),
        WrongBobName = "bobby_the_great",
        {BobName, _, _} = get_user_data(bob, Config),
        BobJID = <<BobName/binary, $@, Domain/binary, $/, (escalus_client:resource(Bob))/binary>>,
        Stanza = create_stanza(Alice, BobJID),
        StanzaWrong = <<"<iq type='get' id='234234'><xmlns='wrongwrong'>">>,
        {_, Err} = mongooseimctl("send_stanza_c2s",
                  [WrongBobName, Domain, Resource, Stanza],
                  Config),
        {_, Err2} = mongooseimctl("send_stanza_c2s",
                  [BobName, Domain, Resource,  StanzaWrong],
                  Config),

        true = Err =/= 0,
        true = Err2 =/= 0,
        escalus_assert:has_no_stanzas(Alice)
    end).

create_stanza(Name1, JID2) ->
    exml:to_binary(escalus_stanza:from(escalus_stanza:chat_to(Name1, "Hi"), JID2)).

%%--------------------------------------------------------------------
%% service_admin_extra_stats tests
%%--------------------------------------------------------------------

stats_global(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(_Alice, _Bob) ->
                RegisteredCount = length(escalus_config:get_config(escalus_users, Config, [])),
                Registered = integer_to_list(RegisteredCount) ++ "\n",

                {UpTime, 0} = mongooseimctl("stats", ["uptimeseconds"], Config),
                _ = list_to_integer(string:strip(UpTime, both, $\n)),
                {Registered, 0} = mongooseimctl("stats", ["registeredusers"], Config),

                {"2\n", 0} = mongooseimctl("stats", ["onlineusersnode"], Config),

                {"2\n", 0} = mongooseimctl("stats", ["onlineusers"], Config)
        end).

stats_host(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->
                RegisteredCount = length(escalus_config:get_config(escalus_users, Config, [])),
                Registered = integer_to_list(RegisteredCount) ++ "\n",

                PriDomain = escalus_client:server(Alice),
                SecDomain = ct:get_config({hosts, mim, secondary_domain}),

                {Registered, 0} = mongooseimctl("stats_host",
                                                ["registeredusers", PriDomain], Config),
                {"0\n", 0} = mongooseimctl("stats_host", ["registeredusers", SecDomain], Config),

                {"2\n", 0} = mongooseimctl("stats_host", ["onlineusers", PriDomain], Config),
                {"0\n", 0} = mongooseimctl("stats_host", ["onlineusers", SecDomain], Config)
        end).

%%--------------------------------------------------------------------
%% mongoose_graphql tests
%%--------------------------------------------------------------------

can_execute_admin_queries_with_permissions(Config) ->
    Query = "query { checkAuth { authStatus } }",
    Res = mongooseimctl("graphql", [Query], Config),
    ?assertMatch({_, 0}, Res),
    Data = element(1, Res),
    ?assertNotEqual(nomatch, string:find(Data, "AUTHORIZED")).

can_handle_execution_error(Config) ->
    Query = "{}",
    Res = mongooseimctl("graphql", [Query], Config),
    ?assertMatch({_, 1}, Res),
    Data = element(1, Res),
    ?assertNotEqual(nomatch, string:find(Data, "parser_error")).

graphql_wrong_arguments_number(Config) ->
    ExpectedFragment = "This command requires",
    ResNoArgs = mongooseimctl("graphql", [], Config),
    ?assertMatch({_, 1}, ResNoArgs),
    Data1 = element(1, ResNoArgs),
    ?assertNotEqual(nomatch, string:find(Data1, ExpectedFragment)),

    ResTooManyArgs = mongooseimctl("graphql", ["{}", "{}"], Config),
    ?assertMatch({_, 1}, ResTooManyArgs),
    Data2 = element(1, ResTooManyArgs),
    ?assertNotEqual(nomatch, string:find(Data2, ExpectedFragment)).

%% Generic GraphQL command tests
%% Specific commands are tested in graphql_*_SUITE

graphql_error_unknown_command_with_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["makeCoffee", "--strength", "medium"], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown command")),
    expect_existing_commands(Res).

graphql_error_unknown_command_without_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["makeCoffee"], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown command")),
    expect_existing_commands(Res).

graphql_no_command(Config) ->
    %% Not an error - lists commands in the given category
    {Res, 0} = mongooseimctl("account", [], Config),
    expect_existing_commands(Res).

graphql_error_invalid_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers", "now"], Config),
    ?assertMatch({match, _}, re:run(Res, "Could not parse")),
    expect_command_arguments(Res).

graphql_error_invalid_arg_value(Config) ->
    {Res, 1} = mongooseimctl("vcard", ["setVcard", "--user", "user@host", "--vcard", "x"], Config),
    %% vCard should be provided in JSON
    ?assertMatch({match, _}, re:run(Res, "Invalid value 'x' of argument 'vcard'")),
    ?assertMatch({match, _}, re:run(Res, "vcard\s+VcardInput!")).

graphql_error_no_arg_value(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers", "--domain"], Config),
    ?assertMatch({match, _}, re:run(Res, "Could not parse")),
    expect_command_arguments(Res).

graphql_error_missing_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers"], Config),
    ?assertMatch({match, _}, re:run(Res, "Missing mandatory arguments")),
    expect_command_arguments(Res).

graphql_error_unknown_arg(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers", "--domain", "localhost",
                                         "--x", "y"], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown argument")),
    expect_command_arguments(Res).

graphql_arg_help(Config) ->
    {Res, 0} = mongooseimctl("account", ["countUsers", "--help"], Config),
    expect_command_arguments(Res).

graphql_command(Config) ->
    {ResJSON, 0} = mongooseimctl("account", ["countUsers", "--domain", "localhost"], Config),
    #{<<"data">> := Data} = rest_helper:decode(ResJSON, #{return_maps => true}),
    ?assertMatch(#{<<"account">> := #{<<"countUsers">> := _}}, Data).

expect_existing_commands(Res) ->
    ?assertMatch({match, _}, re:run(Res, "countUsers")).

expect_command_arguments(Res) ->
    ?assertMatch({match, _}, re:run(Res, "domain\s+DomainName!")).

%%-----------------------------------------------------------------
%% Help tests
%%-----------------------------------------------------------------

default_help(Config) ->
    #{node := Node} = mim(),
    CtlCmd = distributed_helper:ctl_path(Node, Config),
    {Res, 2} = mongooseimctl_helper:run(CtlCmd, []),
    %% Expect category list and no deprecated command list
    ?assertMatch({match, _}, re:run(Res, "Usage")),
    ?assertMatch({match, _}, re:run(Res, "account\s+Account management")),
    ?assertMatch(nomatch, re:run(Res, "add_rosteritem\s+Add an item")).

old_help(Config) ->
    {Res, 2} = mongooseimctl("help", [], Config),
    %% Expect deprecated command list and no category list
    ?assertMatch({match, _}, re:run(Res, "The following commands are deprecated")),
    ?assertMatch({match, _}, re:run(Res, "Usage")),
    ?assertMatch(nomatch, re:run(Res, "account\s+Account management")),
    ?assertMatch({match, _}, re:run(Res, "add_rosteritem")).

%%-----------------------------------------------------------------
%% Server management tests
%%-----------------------------------------------------------------

server_status(Config) ->
    {Res, 0} = mongooseimctl("status", [], Config),
    ?assertMatch({match, _}, re:run(Res, "Erlang VM status: started")).

server_is_started(Config) ->
    %% Wait for the server to start, but it is already running
    {Res, 0} = mongooseimctl("started", [], Config),
    %% Expect only whitespace
    ?assertMatch(nomatch, re:run(Res, "\S")).

%%-----------------------------------------------------------------
%% Improve coverage
%%-----------------------------------------------------------------

simple_register(Config) ->
    %% given
    Domain = domain(),
    {Name, Password} = {<<"tyler">>, <<"durden">>},
    %% when
    {R1, 0} = mongooseimctl("registered_users", [Domain], Config),
    Before = length(string:tokens(R1, "\n")),
    {_, 0} = mongooseimctl("register", [Domain, Password], Config),
    {_, 0} = mongooseimctl("register_identified", [Name, Domain, Password], Config),

    {R2, 0} = mongooseimctl("registered_users", [Domain], Config),
    After = length(string:tokens(R2, "\n")),
    %% then
    2 = After - Before.

simple_unregister(Config) ->
    %% given
    Domain = domain(),
    {Name, _} = {<<"tyler">>, <<"durden">>},
    %% when
    {_, 0} = mongooseimctl("unregister", [Name, Domain], Config),
    {R2, 0} = mongooseimctl("registered_users", [Domain], Config),
    %% then
    nomatch = re:run(R2, ".*(" ++ binary_to_list(Name) ++ ").*").

register_twice(Config) ->
    %% given
    Domain = domain(),
    {Name,  Password} = {<<"tyler">>, <<"durden">>},
    %% when
    {_, 0} = mongooseimctl("register_identified", [Name, Domain, Password], Config),
    {R, Code} = mongooseimctl("register_identified", [Name, Domain, Password], Config),
    %% then
    {match, _} = re:run(R, ".*(already registered).*"),
    true = (Code =/= 0),
    {_, 0} = mongooseimctl("unregister", [Name, Domain], Config).

backup_restore_mnesia(Config) ->
    %% given
    TableName = passwd,
    TableSize = rpc_call(mnesia, table_info, [TableName, size]),
    %% Table passwd should not be empty
    FileName = "backup_mnesia.bup",
    %% when
    {R, 0} = mongooseimctl("backup", [FileName], Config),
    nomatch = re:run(R, ".+"),
    rpc_call(mnesia, clear_table, [TableName]),
    0 = rpc_call(mnesia, table_info, [TableName, size]),
    {R2, 0} = mongooseimctl("restore", [FileName], Config),
    %% then
    nomatch = re:run(R2, ".+"),
    TableSize = rpc_call(mnesia, table_info, [TableName, size]).

restore_mnesia_wrong(Config) ->
    FileName = "file that doesnt exist13123.bup",
    {R2, _} = mongooseimctl("restore", [FileName], Config),
    {match, Code} = re:run(R2, ".+"),
    true = (Code =/= 0).

dump_and_load(Config) ->
    FileName = "dump.bup",
    TableName = passwd,
    %% Table passwd should not be empty
    TableSize = rpc_call(mnesia, table_info, [TableName, size]),
    {DumpReturns, 0} = mongooseimctl("dump", [FileName], Config),
    ct:log("DumpReturns ~p", [DumpReturns]),
    {ok, DumpData} = rpc_call(file, consult, [FileName]),
    ct:log("DumpData ~p", [DumpData]),
    rpc_call(mnesia, clear_table, [TableName]),
    0 = rpc_call(mnesia, table_info, [TableName, size]),
    {R, 0} = mongooseimctl("load", [FileName], Config),
    ct:log("LoadReturns ~p", [R]),
    {match, _} = re:run(R, ".+"),
    TableSize = rpc_call(mnesia, table_info, [TableName, size]).

load_mnesia_wrong(Config) ->
    FileName = "file that doesnt existRHCP.bup",
    {R2, Code} = mongooseimctl("restore", [FileName], Config),
    {match, _} = re:run(R2, ".+"),
    true = (Code =/= 0).

dump_table(Config) ->
    FileName = "dump.mn",
    TableName = passwd,
    %% Table passwd should not be empty
    TableSize = rpc_call(mnesia, table_info, [TableName, size]),
    {_, 0} = mongooseimctl("dump_table", [FileName, atom_to_list(TableName)], Config),
    rpc_call(mnesia, clear_table, [TableName]),
    0 = rpc_call(mnesia, table_info, [TableName, size]),
    {R, 0} = mongooseimctl("load", [FileName], Config),
    {match, _} = re:run(R, ".+"),
    TableSize = rpc_call(mnesia, table_info, [TableName, size]).

get_loglevel(Config) ->
    {R, 0} = mongooseimctl("get_loglevel", [], Config),
    LogLevel = rpc_call(mongoose_logs, get_global_loglevel, []),
    Regexp = io_lib:format("global loglevel is \(.\)\{1,2\}, which means '~p'", [LogLevel]),
    {match, _} = re:run(R, Regexp, [{capture, first}]).

remove_old_messages_test(Config) ->
    escalus:story(Config, [{alice, 1}], fun(_) ->
        %% given
        JidA = nick_to_jid(alice, Config),
        JidB = nick_to_jid(bob, Config),
        JidRecordAlice = jid:from_binary(JidA),
        JidRecordBob = jid:from_binary(JidB),
        Domain = domain(),
        Msg1 = escalus_stanza:chat_to(<<"bob@", Domain/binary>>,
                                      "Hi, how are you? Its old message!"),
        Msg2 = escalus_stanza:chat_to(<<"bob@", Domain/binary>>,
                                      "Hello its new message!"),
        OldTimestamp = fallback_timestamp(10, os:system_time(microsecond)),
        OfflineOld = generate_offline_message(JidRecordAlice, JidRecordBob, Msg1, OldTimestamp),
        OfflineNew = generate_offline_message(JidRecordAlice, JidRecordBob, Msg2, os:system_time(microsecond)),
        {LUser, LServer} = jid:to_lus(JidRecordBob),
        HostType = host_type(),
        rpc_call(mod_offline_backend, write_messages, [host_type(), LUser, LServer, [OfflineOld, OfflineNew]]),
        %% when
        {_, 0} = mongooseimctl("delete_old_messages", [LServer, "1"], Config),
        {ok, SecondList} = rpc_call(mod_offline_backend, pop_messages, [HostType, JidRecordBob]),
        %% then
        1 = length(SecondList)
    end).

remove_expired_messages_test(Config) ->
    escalus:story(Config, [{mike, 1}], fun(_) ->
        %% given
        JidA = nick_to_jid(mike, Config),
        JidB = nick_to_jid(kate, Config),
        JidRecordMike = jid:from_binary(JidA),
        JidRecordKate = jid:from_binary(JidB),
        Domain = domain(),
        Msg1 = escalus_stanza:chat_to(<<"kate@", Domain/binary>>, "Rolling stones"),
        Msg2 = escalus_stanza:chat_to(<<"kate@", Domain/binary>>, "Arctic monkeys!"),
        Msg3 = escalus_stanza:chat_to(<<"kate@", Domain/binary>>, "More wine..."),
        Msg4 = escalus_stanza:chat_to(<<"kate@", Domain/binary>>, "kings of leon"),
        OldTimestamp = fallback_timestamp(10, os:system_time(microsecond)),
        ExpirationTime = fallback_timestamp(2, os:system_time(microsecond)),
        ExpirationTimeFuture= fallback_timestamp(-5, os:system_time(microsecond)),
        OfflineOld = generate_offline_expired_message(JidRecordMike,
                                                      JidRecordKate, Msg1,
                                                      OldTimestamp,
                                                      ExpirationTime),
        OfflineNow = generate_offline_expired_message(JidRecordMike,
                             JidRecordKate, Msg2, os:system_time(microsecond), ExpirationTime),
        OfflineFuture = generate_offline_expired_message(JidRecordMike,
                             JidRecordKate, Msg3, os:system_time(microsecond), ExpirationTimeFuture),
        OfflineFuture2 = generate_offline_expired_message(JidRecordMike,
                                                          JidRecordKate, Msg4,
                                                          OldTimestamp,
                                                          ExpirationTimeFuture),
        {LUser, LServer} = jid:to_lus(JidRecordKate),
        Args = [OfflineOld, OfflineNow, OfflineFuture, OfflineFuture2],
        HostType = host_type(),
        rpc_call(mod_offline_backend, write_messages, [HostType, LUser, LServer, Args]),
        %% when
        {_, 0} = mongooseimctl("delete_expired_messages", [LServer], Config),
        {ok, SecondList} = rpc_call(mod_offline_backend, pop_messages, [HostType, JidRecordKate]),
        %% then
        2 = length(SecondList)
    end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------


nick_to_jid(UserName, Config) when is_atom(UserName) ->
    UserSpec = escalus_users:get_userspec(Config, UserName),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

generate_offline_message(From, To, Msg, TimeStamp) ->
    {LUser, LServer} = jid:to_lus(To),
    #offline_msg{us = {LUser, LServer}, timestamp = TimeStamp, expire = never,
                 from = From, to = To, packet = Msg}.

generate_offline_expired_message(From, To, Msg, TimeStamp, ExpirationTime) ->
    {LUser, LServer} = jid:to_lus(To),
    #offline_msg{us = {LUser, LServer}, timestamp = TimeStamp,
                 expire = ExpirationTime, from = From, to = To, packet = Msg}.


fallback_timestamp(HowManyDays, TS_MicroSeconds) ->
    HowManySeconds = HowManyDays * 86400,
    HowManyMicroSeconds = erlang:convert_time_unit(HowManySeconds, second, microsecond),
    TS_MicroSeconds - HowManyMicroSeconds.

get_user_data(User, Config) when is_atom(User) ->
    get_user_data(escalus_users:get_options(Config, User, <<"newres">>), Config);
get_user_data(User, _Config) ->
    {_, Password} = lists:keyfind(password, 1, User),
    {_, Username} = lists:keyfind(username, 1, User),
    {_, Domain} = lists:keyfind(server, 1, User),
    {Username, Domain, Password}.

get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(md5, AccountPass))]).
get_sha(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(sha, AccountPass))]).

set_last(User, Domain, TStamp) ->
    rpc(mim(), mod_last, store_last_info,
        [host_type(), escalus_utils:jid_to_lower(User), Domain, TStamp, <<>>]).

delete_users(_Config) ->
    lists:foreach(fun({User, Domain}) ->
                JID = mongoose_helper:make_jid(User, Domain),
                rpc(mim(), ejabberd_auth, remove_user, [JID])
        end, get_registered_users()).

%%-----------------------------------------------------------------
%% Predicates
%%-----------------------------------------------------------------

match_user_status(Users, StatusTxt) ->
    Statuses = string:tokens(StatusTxt, "\n"),
    true = (length(Users) == length(Statuses)),
    match_user_status2(Users, Statuses).

match_user_status2([], _) ->
    true;
match_user_status2([User | UserR], Statuses) ->
    Username = binary_to_list(escalus_client:username(User)),
    Domain = binary_to_list(escalus_client:server(User)),
    Resource = binary_to_list(escalus_client:resource(User)),

    true = lists:any(fun(Status) ->
                [Username, Domain, Resource]
                =:=
                lists:sublist(string:tokens(Status, "\t"), 1, 3)
        end, Statuses),
    match_user_status2(UserR, Statuses).

match_user_info(Users, UsersTxt) ->
    UsersInfo = string:tokens(UsersTxt, "\n"),
    case length(Users) == length(UsersInfo) of
        true ->
            ok;
        false ->
            ct:fail(#{what => match_user_info_failed,
                      users => Users, user_info => UsersInfo})
    end,
    match_user_info2(Users, UsersInfo).

match_user_info2([], _) ->
    true;
match_user_info2([User | UserR], UsersInfo) ->
    Username = binary_to_list(escalus_client:username(User)),
    Domain = binary_to_list(escalus_client:server(User)),
    Resource = binary_to_list(escalus_client:resource(User)),
    FullJID = Username ++ "@" ++ Domain ++ "/" ++ Resource,

    true = lists:any(fun(UserInfo) ->
                string:str(UserInfo, string:to_lower(FullJID)) =:= 1
        end, UsersInfo),
    match_user_info2(UserR, UsersInfo).

match_roster(ItemsValid, Items) ->
    ItemsTokens = [ string:tokens(ItemToken, "\t") || ItemToken <- string:tokens(Items, "\n") ],

    true = (length(ItemsValid) == length(ItemsTokens)),
    true = lists:all(fun({Username, Domain, _Nick, _Group, _Sub}) ->
                    JID = escalus_utils:jid_to_lower(<<Username/binary, "@", Domain/binary >>),
                    lists:any(fun
                                ([RosterJID, _Nick, _Sub, "none", _Group]) ->
                                    JID =:= escalus_utils:jid_to_lower(list_to_binary(RosterJID));
                                (_) ->
                                    false
                              end, ItemsTokens)
            end, ItemsValid).

string_to_binary(List) ->
    case erlang:system_info(otp_release) of
        [$R|_] ->
            list_to_binary(List);
        _ ->
            unicode:characters_to_binary(List)
    end.

add_rosteritem1(UserName1, Domain, UserName2, Config) ->
    mongooseimctl("add_rosteritem",
                [UserName1, Domain, UserName2,
                 Domain, "MyBob", "MyGroup", "both"], Config).

add_rosteritem2(Name1, Domain1, Name2, Domain2, Config) ->
    mongooseimctl("add_rosteritem",
                [Name1, Domain1, Name2,
                 Domain2, "DearMike", "MyGroup", "both"], Config).
