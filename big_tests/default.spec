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

%% do not remove below SUITE if testing mongoose
{suites, "tests", mongoose_sanity_checks_SUITE}.

{suites, "tests", mim_c2s_SUITE}.

{suites, "tests", acc_e2e_SUITE}.
{suites, "tests", accounts_SUITE}.
{suites, "tests", adhoc_SUITE}.
{suites, "tests", amp_big_SUITE}.
{suites, "tests", anonymous_SUITE}.
{suites, "tests", auth_methods_for_c2s_SUITE}.
{suites, "tests", bind2_SUITE}.
{suites, "tests", bosh_SUITE}.
{suites, "tests", carboncopy_SUITE}.
{suites, "tests", cluster_commands_SUITE}.
{suites, "tests", component_SUITE}.
{suites, "tests", connect_SUITE}.
{suites, "tests", disco_and_caps_SUITE}.
{suites, "tests", extdisco_SUITE}.
{suites, "tests", gdpr_SUITE}.
{suites, "tests", graphql_SUITE}.
{suites, "tests", graphql_sse_SUITE}.
{suites, "tests", graphql_account_SUITE}.
{suites, "tests", graphql_domain_SUITE}.
{suites, "tests", graphql_inbox_SUITE}.
{suites, "tests", graphql_last_SUITE}.
{suites, "tests", graphql_muc_SUITE}.
{suites, "tests", graphql_muc_light_SUITE}.
{suites, "tests", graphql_offline_SUITE}.
{suites, "tests", graphql_private_SUITE}.
{suites, "tests", graphql_roster_SUITE}.
{suites, "tests", graphql_session_SUITE}.
{suites, "tests", graphql_stanza_SUITE}.
{suites, "tests", graphql_stats_SUITE}.
{suites, "tests", graphql_gdpr_SUITE}.
{suites, "tests", graphql_token_SUITE}.
{suites, "tests", graphql_mnesia_SUITE}.
{suites, "tests", graphql_cets_SUITE}.
{suites, "tests", graphql_vcard_SUITE}.
{suites, "tests", graphql_http_upload_SUITE}.
{suites, "tests", graphql_server_SUITE}.
{suites, "tests", graphql_metric_SUITE}.
{suites, "tests", inbox_SUITE}.
{suites, "tests", inbox_extensions_SUITE}.
{suites, "tests", jingle_SUITE}.
{suites, "tests", last_SUITE}.
{suites, "tests", login_SUITE}.
{suites, "tests", mam_SUITE}.
{suites, "tests", mam_proper_SUITE}.
{suites, "tests", mam_send_message_SUITE}.
{suites, "tests", metrics_api_SUITE}.
{suites, "tests", metrics_c2s_SUITE}.
{suites, "tests", metrics_register_SUITE}.
{suites, "tests", metrics_roster_SUITE}.
{suites, "tests", metrics_session_SUITE}.
{suites, "tests", mod_blocking_SUITE}.
{suites, "tests", mod_event_pusher_http_SUITE}.
{suites, "tests", mod_event_pusher_rabbit_SUITE}.
{suites, "tests", mod_event_pusher_sns_SUITE}.
{suites, "tests", mod_global_distrib_SUITE}.
{suites, "tests", mod_http_upload_SUITE}.
{suites, "tests", mod_ping_SUITE}.
{suites, "tests", mod_time_SUITE}.
{suites, "tests", mod_version_SUITE}.
{suites, "tests", mongoose_cassandra_SUITE}.
{suites, "tests", mongoose_elasticsearch_SUITE}.
{suites, "tests", mongooseimctl_SUITE}.
{suites, "tests", muc_SUITE}.
{suites, "tests", muc_http_api_SUITE}.
{suites, "tests", muc_light_SUITE}.
{suites, "tests", muc_light_http_api_SUITE}.
{suites, "tests", muc_light_legacy_SUITE}.
{suites, "tests", oauth_SUITE}.
{suites, "tests", offline_SUITE}.
{suites, "tests", offline_stub_SUITE}.
{suites, "tests", pep_SUITE}.
{suites, "tests", persistent_cluster_id_SUITE}.
{suites, "tests", presence_SUITE}.
{suites, "tests", privacy_SUITE}.
{suites, "tests", private_SUITE}.
{suites, "tests", pubsub_SUITE}.
{suites, "tests", pubsub_s2s_SUITE}.
{suites, "tests", push_SUITE}.
{suites, "tests", push_http_SUITE}.
{suites, "tests", push_integration_SUITE}.
{suites, "tests", push_pubsub_SUITE}.
{suites, "tests", race_conditions_SUITE}.
{suites, "tests", rdbms_SUITE}.
{suites, "tests", rest_SUITE}.
{suites, "tests", rest_client_SUITE}.
{suites, "tests", s2s_SUITE}.
{suites, "tests", sasl_SUITE}.
{suites, "tests", sasl2_SUITE}.
{suites, "tests", sasl_external_SUITE}.
{suites, "tests", service_mongoose_system_metrics_SUITE}.
{suites, "tests", shared_roster_SUITE}.
{suites, "tests", sic_SUITE}.
{suites, "tests", smart_markers_SUITE}.
{suites, "tests", sm_SUITE}.
{suites, "tests", vcard_SUITE}.
{suites, "tests", vcard_simple_SUITE}.
{suites, "tests", websockets_SUITE}.
{suites, "tests", xep_0352_csi_SUITE}.
{suites, "tests", service_domain_db_SUITE}.
{suites, "tests", domain_isolation_SUITE}.
{suites, "tests", domain_removal_SUITE}.
{suites, "tests", dynamic_domains_SUITE}.
{suites, "tests", local_iq_SUITE}.
{suites, "tests", tcp_listener_SUITE}.
{suites, "tests", cets_disco_SUITE}.
{suites, "tests", start_node_id_SUITE}.

{config, ["test.config"]}.
{logdir, "ct_report"}.

%% ct_tty_hook will log CT failures to TTY verbosely
%% ct_mongoose_hook will:
%% * ensure preset & mim_data_dir values are passed to ct Config
%% * check server's purity after SUITE
{ct_hooks, [ct_groups_summary_hook, ct_tty_hook, ct_mongoose_hook, ct_progress_hook,
            ct_markdown_errors_hook, ct_mongoose_log_hook]}.

%% since test-runner.sh can be executed with the --one-node option,
%% log collection is enabled by default for host mim1 only.
% {ct_hooks, [{ct_mongoose_log_hook,[{host, mim2}, {log, []}]}]}.
