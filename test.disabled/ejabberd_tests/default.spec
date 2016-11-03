%% Spec examples:
%%
%%   {suites, "tests", amp_SUITE}.
%%   {groups, "tests", amp_SUITE, [discovery]}.
%%   {groups, "tests", amp_SUITE, [discovery], {cases, [stream_feature_test]}}.
%%   {cases, "tests", amp_SUITE, [stream_feature_test]}.
%%
%% For more info see:
%% http://www.erlang.org/doc/apps/common_test/run_test_chapter.html#test_specifications

%% do not remove below SUITE if testing mongoose
{suites, "tests", mongoose_sanity_checks_SUITE}.

{suites, "tests", adhoc_SUITE}.
{suites, "tests", amp_big_SUITE}.
{suites, "tests", anonymous_SUITE}.
{suites, "tests", bosh_SUITE}.
{suites, "tests", carboncopy_SUITE}.
{suites, "tests", cluster_commands_SUITE}.
{suites, "tests", conf_reload_SUITE}.
{suites, "tests", connect_SUITE}.
{suites, "tests", component_SUITE}.
{suites, "tests", disco_and_caps_SUITE}.
{suites, "tests", ejabberdctl_SUITE}.
{suites, "tests", last_SUITE}.
{suites, "tests", login_SUITE}.
{suites, "tests", mam_SUITE}.
{suites, "tests", metrics_api_SUITE}.
{suites, "tests", metrics_c2s_SUITE}.
{suites, "tests", metrics_roster_SUITE}.
{suites, "tests", metrics_register_SUITE}.
{suites, "tests", metrics_session_SUITE}.
{suites, "tests", mod_blocking_SUITE}.
{suites, "tests", mod_http_notification_SUITE}.
{suites, "tests", mod_ping_SUITE}.
{suites, "tests", mod_time_SUITE}.
{suites, "tests", mod_version_SUITE}.
{suites, "tests", muc_SUITE}.
{suites, "tests", muc_light_SUITE}.
{suites, "tests", muc_light_legacy_SUITE}.
{suites, "tests", muc_http_api_SUITE}.
{suites, "tests", muc_light_http_api_SUITE}.
{suites, "tests", oauth_SUITE}.
{suites, "tests", offline_SUITE}.
{suites, "tests", pep_SUITE}.
{suites, "tests", presence_SUITE}.
{suites, "tests", privacy_SUITE}.
{suites, "tests", private_SUITE}.
{suites, "tests", pubsub_SUITE}.
{suites, "tests", rest_SUITE}.
{suites, "tests", rest_client_SUITE}.
{suites, "tests", s2s_SUITE}.
{suites, "tests", sasl_SUITE}.
{suites, "tests", shared_roster_SUITE}.
{suites, "tests", sic_SUITE}.
{suites, "tests", sm_SUITE}.
{suites, "tests", users_api_SUITE}.
{suites, "tests", vcard_simple_SUITE}.
{suites, "tests", vcard_SUITE}.
{suites, "tests", websockets_SUITE}.
{suites, "tests", xep_0352_csi_SUITE}.

{config, ["test.config"]}.
{logdir, "ct_report"}.

%% ct_tty_hook will log CT failures to TTY verbosely
%% ct_mongoose_hook will:
%% * log suite start/end events in the MongooseIM console
%% * ensure preset value is passed to ct Config
%% * check server's purity after SUITE
{ct_hooks, [ct_tty_hook, ct_mongoose_hook, ct_progress_hook]}.

%% To enable printing group and case enters on server side
%%{ct_hooks, [{ct_mongoose_hook, [print_group, print_case]}]}.
