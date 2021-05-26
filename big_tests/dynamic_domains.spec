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
{suites, "tests", domain_isolation_SUITE}.
{suites, "tests", muc_SUITE}.
{suites, "tests", muc_light_SUITE}.
{suites, "tests", muc_light_legacy_SUITE}.
{suites, "tests", mam_SUITE}.

{suites, "tests", carboncopy_SUITE}.
{skip_cases, "tests", carboncopy_SUITE, [discovering_support],
 "at the moment mod_disco doesn't support dynamic domains"}.

{suites, "tests", mod_ping_SUITE}.

{suites, "tests", inbox_SUITE}.
{skip_cases, "tests", inbox_SUITE, [disco_service],
 "at the moment mod_disco doesn't support dynamic domains"}.
{skip_cases, "tests", inbox_SUITE, [msg_sent_to_offline_user],
 "at the moment mod_offline doesn't support dynamic domains"}.
{skip_groups, "tests", inbox_SUITE, [muclight, muc],
 "at the moment muc/muclight doesn't support dynamic domains"}.

{suites, "tests", inbox_extensions_SUITE}.
{skip_groups, "tests", inbox_extensions_SUITE, [muclight],
 "at the moment muclight doesn't support dynamic domains"}.

{skip_cases, "tests", mam_SUITE,
 [muc_service_discovery, mam_service_discovery],
 "at the moment mod_disco doesn't support dynamic domains"}.
{skip_cases, "tests", mam_SUITE,
 [messages_filtered_when_prefs_default_policy_is_roster],
 "at the moment mod_roster doesn't support dynamic domains"}.

{skip_groups, "tests", muc_SUITE,
 [disco, disco_non_parallel, disco_rsm, disco_rsm_with_offline],
 "at the moment mod_disco doesn't support dynamic domains"}.
{skip_groups, "tests", muc_SUITE,
 [register_over_s2s],
 "at the moment S2S doesn't support dynamic domains "
 "(requires mod_register creating CT users)"}.

{skip_cases, "tests", muc_light_SUITE,
 [disco_service,
  disco_features,
  disco_features_with_mam,
  disco_info,
  disco_info_with_mam,
  disco_rooms,
  disco_rooms_rsm,
  disco_rooms_created_page_1,
  disco_rooms_created_page_infinity,
  disco_rooms_empty_page_infinity,
  disco_rooms_empty_page_1],
 "at the moment mod_disco doesn't support dynamic domains"}.
{skip_cases, "tests", muc_light_SUITE,
 [rooms_in_rosters,
  rooms_in_rosters_doesnt_break_disco_info,
  no_roomname_in_schema_doesnt_break_disco_and_roster],
 "at the moment mod_roster doesn't support dynamic domains"}.

{skip_cases, "tests", muc_light_legacy_SUITE,
 [disco_service,
  disco_features,
  disco_features_with_mam,
  disco_info,
  disco_info_with_mam,
  disco_rooms,
  disco_rooms_rsm],
 "at the moment mod_disco doesn't support dynamic domains"}.

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
