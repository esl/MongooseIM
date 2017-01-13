-module(mod_aws_sns_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_HTTP_UPLOAD, <<"urn:xmpp:http:upload">>).
-define(S3_HOSTNAME, "http://bucket.s3-eu-east-25.example.com").
-define(SNS_OPTS,
    [
        {access_key_id, "AKIAIH54ALYGMZTESTID"},
        {secret_access_key, "buRqHOxXCFUQkiuYgdUAy+XoGKt0Ec6DTESTKEY+"},
        {region, "eu-west-1"},
        {account_id, "123456789012"},
        {sns_host, "sns.eu-west-1.amazonaws.com"},
        {plugin_module, mod_aws_sns_defaults},
        {presence_updates_topic, "user_presence_updated-dev-1"},
        {pm_messages_topic, "user_message_sent-dev-1"},
        {muc_messages_topic, "user_messagegroup_sent-dev-1"}
    ]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, presence_status_publish},
        {group, message_publish},
        {group, configuration}
    ].

groups() ->
    [
     {presence_status_publish, [], [
         connected_user_changes_status,
         disconnected_user_becomes_unavailable
     ]},
     {message_publish, [], [
         pm_messages,
         muc_messages
     ]},
     {configuration, [], [
         plugin_module_changes_user_guid,
         plugin_module_adds_message_attributes,
         disable_presence_updates,
         disable_pm_message_notifications,
         disable_muc_message_notifications
     ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Host, mod_aws_sns, ?SNS_OPTS),
    escalus:create_users(Config, escalus:get_users([bob, alice, john])).

end_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_aws_sns),
    escalus:delete_users(Config, escalus:get_users([bob, alice, john])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% GROUP presence_status_publish
%%--------------------------------------------------------------------

connected_user_changes_status(Config) ->
    ok.

disconnected_user_becomes_unavailable(Config) ->
    ok.

%%--------------------------------------------------------------------
%% GROUP message_publish
%%--------------------------------------------------------------------

pm_messages(Config) ->
    ok.

muc_messages(Config) ->
    ok.

%%--------------------------------------------------------------------
%% GROUP configuration
%%--------------------------------------------------------------------

plugin_module_changes_user_guid(Config) ->
    ok.

plugin_module_adds_message_attributes(Config) ->
    ok.

disable_presence_updates(Config) ->
    ok.

disable_pm_message_notifications(Config) ->
    ok.

disable_muc_message_notifications(Config) ->
    ok.


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------
