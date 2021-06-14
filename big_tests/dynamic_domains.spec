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
{skip_cases, "tests", disco_and_caps_SUITE,
 [caps_feature_is_advertised,
  user_can_query_server_caps_via_disco],
 "at the moment mod_caps doesn't support dynamic domains"}.
{skip_cases, "tests", disco_and_caps_SUITE,
 [user_can_query_friend_resources,
  user_can_query_friend_features,
  user_cannot_query_friend_resources_with_unknown_node],
 "at the moment mod_roster doesn't support dynamic domains"}.

{suites, "tests", inbox_SUITE}.
{skip_cases, "tests", inbox_SUITE, [msg_sent_to_offline_user],
 "at the moment mod_offline doesn't support dynamic domains"}.

{suites, "tests", inbox_extensions_SUITE}.

{suites, "tests", mam_SUITE}.
{skip_cases, "tests", mam_SUITE,
 [muc_service_discovery, mam_service_discovery],
 "at the moment mod_mam config options don't support dynamic domains"}.
{skip_cases, "tests", mam_SUITE,
 [messages_filtered_when_prefs_default_policy_is_roster],
 "at the moment mod_roster doesn't support dynamic domains"}.

{suites, "tests", mod_ping_SUITE}.

{suites, "tests", muc_SUITE}.
{skip_groups, "tests", muc_SUITE,
 [register_over_s2s],
 "at the moment S2S doesn't support dynamic domains "
 "(requires mod_register creating CT users)"}.

{suites, "tests", muc_light_SUITE}.
{skip_cases, "tests", muc_light_SUITE,
 [rooms_in_rosters,
  rooms_in_rosters_doesnt_break_disco_info,
  no_roomname_in_schema_doesnt_break_disco_and_roster],
 "at the moment mod_roster doesn't support dynamic domains"}.

{suites, "tests", muc_light_legacy_SUITE}.

{suites, "tests", offline_stub_SUITE}.

{suites, "tests", sm_SUITE}.
{skip_cases, "tests", sm_SUITE,
 [basic_ack,
  h_ok_after_session_enabled_after_session,
  subscription_requests_are_buffered_properly],
 "at the moment mod_roster doesn't support dynamic domains"}.
{skip_cases, "tests", sm_SUITE,
 [resend_unacked_on_reconnection,
  session_established,
  wait_for_resumption,
  resume_session_kills_old_C2S_gracefully,
  resend_unacked_after_resume_timeout,
  resend_more_offline_messages_than_buffer_size,
  resume_expired_session_returns_correct_h,
  unacknowledged_message_hook_offline],
 "at the moment mod_offline doesn't support dynamic domains"}.

{suites, "tests", rest_client_SUITE}.
{skip_groups, "tests", rest_client_SUITE, [roster],
 "at the moment mod_roster doesn't support dynamic domains"}.
{skip_cases, "tests", rest_client_SUITE,
 [non_default_http_server_name_is_returned_if_configured],
 "at the moment mim2 node is not configured for dynamic domains"}.

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
