%% Spec examples:
%%
%%   {suites, "tests", amp_SUITE}.
%%   {groups, "tests", amp_SUITE, [discovery]}.
%%   {groups, "tests", amp_SUITE, [discovery], {cases, [stream_feature_test]}}.
%%   {cases, "tests", amp_SUITE, [stream_feature_test]}.
%%
%% For more info see:
%% http://www.erlang.org/doc/apps/common_test/run_test_chapter.html#test_specifications
{include, "tests"}.

{suites, "tests", acc_e2e_SUITE}.

{suites, "tests", carboncopy_SUITE}.

{suites, "tests", disco_and_caps_SUITE}.

{suites, "tests", domain_isolation_SUITE}.

{suites, "tests", inbox_SUITE}.

{suites, "tests", inbox_extensions_SUITE}.

{suites, "tests", last_SUITE}.

{suites, "tests", mam_SUITE}.

{suites, "tests", mod_blocking_SUITE}.

{suites, "tests", mod_ping_SUITE}.

{suites, "tests", muc_SUITE}.
{skip_groups, "tests", muc_SUITE,
 [register_over_s2s],
 "at the moment S2S doesn't support dynamic domains "
 "(requires mod_register creating CT users)"}.

{suites, "tests", muc_light_SUITE}.

{suites, "tests", muc_light_legacy_SUITE}.

{suites, "tests", offline_SUITE}.
{skip_groups, "tests", offline_SUITE, [chatmarkers],
 "at the moment mod_offline_chatmarkers does not support dynamic domains"}.

{suites, "tests", offline_stub_SUITE}.

{suites, "tests", presence_SUITE}.

{suites, "tests", privacy_SUITE}.

{suites, "tests", private_SUITE}.

{suites, "tests", race_conditions_SUITE}.

{suites, "tests", rest_client_SUITE}.
{skip_cases, "tests", rest_client_SUITE,
 [non_default_http_server_name_is_returned_if_configured],
 "at the moment mim2 node is not configured for dynamic domains"}.

{suites, "tests", sm_SUITE}.

{suites, "tests", vcard_SUITE}.
{suites, "tests", vcard_simple_SUITE}.
{suites, "tests", domain_removal_SUITE}.

{config, ["dynamic_domains.config", "test.config"]}.

{logdir, "ct_report"}.

%% ct_tty_hook will log CT failures to TTY verbosely
%% ct_mongoose_hook will:
%% * log suite start/end events in the MongooseIM console
%% * ensure preset value is passed to ct Config
%% * check server's purity after SUITE
{ct_hooks, [ct_groups_summary_hook, ct_tty_hook, ct_mongoose_hook, ct_progress_hook,
            ct_mim_config_hook,
            ct_markdown_errors_hook,
            {ct_mongoose_log_hook, [ejabberd_node, ejabberd_cookie]},
            {ct_mongoose_log_hook, [ejabberd2_node, ejabberd_cookie]}
           ]}.

%% To enable printing group and case enters on server side
%%{ct_hooks, [{ct_mongoose_hook, [print_group, print_case]}]}.
