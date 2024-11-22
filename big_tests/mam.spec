%% Spec examples:
%%
%%   {suites, "tests", amp_SUITE}.
%%   {groups, "tests", amp_SUITE, [discovery]}.
%%   {groups, "tests", amp_SUITE, [discovery], {cases, [stream_feature_test]}}.
%%   {cases, "tests", amp_SUITE, [stream_feature_test]}.
%%
%% For more info see:
%% http://www.erlang.org/doc/apps/common_test/run_test_chapter.html#test_specifications

{suites, "tests", mam_SUITE}.
{suites, "tests", gdpr_SUITE}.
{suites, "tests", mongoose_cassandra_SUITE}.
{suites, "tests", mongoose_elasticsearch_SUITE}.

{config, ["test.config"]}.
{logdir, "ct_report"}.

%% ct_tty_hook will log CT failures to TTY verbosely
%% ct_mongoose_hook will:
%% * ensure preset & mim_data_dir values are passed to ct Config
%% * check server's purity after SUITE
{ct_hooks, [ct_groups_summary_hook, ct_tty_hook, ct_mongoose_hook,
            ct_progress_hook,
            {ct_mongoose_log_hook, [{host, mim2}, {print_init_and_done_for_testcases, false}]},
            {ct_mongoose_log_hook, [{host, mim3}, {print_init_and_done_for_testcases, false}]},
            ct_markdown_errors_hook, ct_mongoose_log_hook]}.

%% since test-runner.sh can be executed with the --one-node option,
%% log collection is enabled by default for host mim1 only.
% {ct_hooks, [{ct_mongoose_log_hook,[{host, mim2}, {log, []}]}]}.
