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

{suites, "tests", pubsub_SUITE}.
{suites, "tests", pep_SUITE}.

{config, ["test.config"]}.
{logdir, "ct_report"}.
{ct_hooks, [ct_tty_hook, ct_mongoose_hook]}.
%%To enable printing group and case enters on server side
%%{ct_hooks, [{ct_tty_hook, [print_group, print_case]}]}.
