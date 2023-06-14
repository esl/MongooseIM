{suites, "tests", test_SUITE}.

{config, ["test.config"]}.
{logdir, "ct_report"}.

%% ct_test_hook should be executed just once,
%% see ct_test_hook:id/1 implementation and
%% ct_hooks documentation:
%%   https://erlang.org/doc/man/ct_hooks.html
{ct_hooks, [ct_test_hook, ct_test_hook]}.

% {ct_hooks, [ct_groups_summary_hook, ct_markdown_errors_hook]}.
