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

{groups, "tests", sm_SUITE, [[tcp_tests, parallel_unacknowledged_message_hook]]%, {cases, [unacknowledged_message_hook_filter]}
}.

%% {suites, "tests", mim_c2s_SUITE}.
%% {suites, "tests", acc_e2e_SUITE}.
%% {suites, "tests", accounts_SUITE}.
%% {suites, "tests", adhoc_SUITE}.
%% {suites, "tests", amp_big_SUITE}.
%% {suites, "tests", anonymous_SUITE}.
%% {suites, "tests", bind2_SUITE}.
%% {suites, "tests", fast_auth_token_SUITE}.
%% {suites, "tests", bosh_SUITE}.
%% {suites, "tests", carboncopy_SUITE}.
%% {suites, "tests", connect_SUITE}.
%% {suites, "tests", disco_and_caps_SUITE}.
%% {suites, "tests", domain_isolation_SUITE}.
%% {suites, "tests", extdisco_SUITE}.

%% {suites, "tests", gdpr_SUITE}.
%% {skip_groups, "tests", gdpr_SUITE,
%%  [retrieve_personal_data_pubsub,
%%   remove_personal_data_pubsub],
%%  "at the moment mod_pubsub doesn't support dynamic domains"}.

%% {suites, "tests", graphql_SUITE}.
%% {suites, "tests", graphql_sse_SUITE}.
%% {suites, "tests", graphql_account_SUITE}.
%% {suites, "tests", graphql_domain_SUITE}.
%% {suites, "tests", graphql_inbox_SUITE}.
%% {suites, "tests", graphql_last_SUITE}.
%% {suites, "tests", graphql_muc_SUITE}.
%% {suites, "tests", graphql_muc_light_SUITE}.
%% {suites, "tests", graphql_private_SUITE}.
%% {suites, "tests", graphql_roster_SUITE}.
%% {suites, "tests", graphql_session_SUITE}.
%% {suites, "tests", graphql_stanza_SUITE}.
%% {suites, "tests", graphql_vcard_SUITE}.
%% {suites, "tests", graphql_offline_SUITE}.
%% {suites, "tests", graphql_stats_SUITE}.
%% {suites, "tests", graphql_gdpr_SUITE}.
%% {suites, "tests", graphql_token_SUITE}.
%% {suites, "tests", graphql_mnesia_SUITE}.
%% {suites, "tests", graphql_cets_SUITE}.
%% {suites, "tests", graphql_http_upload_SUITE}.
%% {suites, "tests", graphql_metric_SUITE}.
%% {suites, "tests", inbox_SUITE}.
%% {suites, "tests", inbox_extensions_SUITE}.
%% {suites, "tests", login_SUITE}.
%% {suites, "tests", mam_SUITE}.
%% {suites, "tests", mam_proper_SUITE}.
%% {suites, "tests", mam_send_message_SUITE}.
%% {suites, "tests", metrics_c2s_SUITE}.
%% {suites, "tests", metrics_roster_SUITE}.
%% {suites, "tests", metrics_session_SUITE}.
%% {suites, "tests", mod_blocking_SUITE}.
%% {suites, "tests", mod_http_upload_SUITE}.
%% {suites, "tests", mod_ping_SUITE}.
%% {suites, "tests", mod_time_SUITE}.
%% {suites, "tests", mod_version_SUITE}.
%% {suites, "tests", mongooseimctl_SUITE}.

%% {suites, "tests", muc_SUITE}.
%% {skip_groups, "tests", muc_SUITE,
%%  [register_over_s2s],
%%  "at the moment S2S doesn't support dynamic domains "
%%  "(requires mod_register creating CT users)"}.

%% {suites, "tests", muc_http_api_SUITE}.
%% {suites, "tests", muc_light_SUITE}.
%% {suites, "tests", muc_light_legacy_SUITE}.
%% {suites, "tests", muc_light_http_api_SUITE}.
%% {suites, "tests", oauth_SUITE}.
%% {suites, "tests", offline_SUITE}.
%% {suites, "tests", offline_stub_SUITE}.
%% {suites, "tests", presence_SUITE}.
%% {suites, "tests", privacy_SUITE}.
%% {suites, "tests", private_SUITE}.
%% {suites, "tests", race_conditions_SUITE}.
%% {suites, "tests", rdbms_SUITE}.
%% {suites, "tests", rest_SUITE}.
%% {suites, "tests", rest_client_SUITE}.
%% {suites, "tests", sasl_SUITE}.
%% {suites, "tests", sasl2_SUITE}.
%% {suites, "tests", sic_SUITE}.
%% {suites, "tests", smart_markers_SUITE}.
%% {suites, "tests", sm_SUITE}.
%% {suites, "tests", system_probes_SUITE}.
%% {suites, "tests", vcard_SUITE}.
%% {suites, "tests", vcard_simple_SUITE}.
%% {suites, "tests", websockets_SUITE}.
%% {suites, "tests", xep_0352_csi_SUITE}.
%% {suites, "tests", domain_removal_SUITE}.
%% {suites, "tests", local_iq_SUITE}.
%% {suites, "tests", cets_disco_SUITE}.
%% {suites, "tests", start_node_id_SUITE}.
%% {suites, "tests", tr_util_SUITE}.

%% %% the below suites restart MIM nodes, so they were moved to the end
%% %% to minimise impact on other tests
%% {suites, "tests", auth_methods_for_c2s_SUITE}.
%% {suites, "tests", cluster_commands_SUITE}.
%% {suites, "tests", component_SUITE}.
%% {suites, "tests", dynamic_domains_SUITE}.
%% {suites, "tests", graphql_server_SUITE}.
%% {suites, "tests", last_SUITE}.
%% {suites, "tests", metrics_api_SUITE}.
%% {suites, "tests", persistent_cluster_id_SUITE}.
%% {suites, "tests", sasl_external_SUITE}.
%% {suites, "tests", service_domain_db_SUITE}.

%% {suites, "tests", service_mongoose_system_metrics_SUITE}.
%% {skip_cases, "tests", service_mongoose_system_metrics_SUITE,
%%  [xmpp_components_are_reported],
%%  "at the moment external components doesn't support dynamic domains"}.

%% {suites, "tests", shutdown_SUITE}.

{config, ["dynamic_domains.config", "test.config"]}.

{logdir, "ct_report"}.

%% ct_tty_hook will log CT failures to TTY verbosely
%% ct_mongoose_hook will:
%% * ensure preset & mim_data_dir values are passed to ct Config
%% * check server's purity after SUITE
{ct_hooks, [cth_validate_nodes,
            ct_groups_summary_hook,
            ct_tty_hook,
            ct_mongoose_hook,
            ct_progress_hook,
            ct_markdown_errors_hook,
            ct_mongoose_log_hook,
            {ct_mongoose_log_hook, [{host, mim2}, {print_init_and_done_for_testcases, false}]},
            {ct_mongoose_log_hook, [{host, mim3}, {print_init_and_done_for_testcases, false}]}]}.
