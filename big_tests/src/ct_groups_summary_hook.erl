%% @doc Count successful and failed groups in the test suite.
%%
%% Use by adding the following line to `test.spec`:
%% {ct_hooks, [ct_results_summary_hook]}.
-module(ct_groups_summary_hook).

%% Callbacks
-export([id/1,
         init/2,
         pre_init_per_suite/3,
         post_end_per_suite/4,
         post_end_per_group/4]).

-include_lib("common_test/include/ct.hrl").

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_results_summary_hook_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, Opts) ->
    %ct:pal("init opts: ~p", [Opts]),
    {ok, #{}}.

pre_init_per_suite(_Suite, Config, State) ->
    %ct:pal("pre_init_per_suite config: ~p", [Config]),
    %ct:pal("pre_init_per_suite state: ~p", [State]),
    %% Start with an empty CT hook state for each suite.
    {Config, #{}}.

post_end_per_suite(_SuiteName, Config, Return, State) ->
    %ct:pal("post_end_per_suite config: ~p", [Config]),
    %ct:pal("post_end_per_suite state: ~p", [State]),
    %ct:pal("post_end_per_suite return: ~p", [Return]),
    write_groups_summary(Config, State),
    {Return, State}.

post_end_per_group(GroupName, Config, Return, State) ->
    %ct:pal("post_end_per_group config: ~p", [Config]),
    %ct:pal("post_end_per_group state: ~p", [State]),
    %ct:pal("post_end_per_group return: ~p", [Return]),
    State1 = update_group_status(GroupName, Config, State),
    {Return, State1}.

%% @doc If the group property repeat_until_all_ok is enabled,
%% then update the outcome of this group in State.
%% In other words, if a previous run of this group failed,
%% and this one succeeded, State will be updated to reflect success.
update_group_status(GroupName, Config, State) ->
    case lists:keyfind(tc_group_properties, 1, Config) of
        false -> State;
        {tc_group_properties, Properties} ->
            case proplists:is_defined(repeat_until_all_ok, Properties) of
                false -> State;
                true ->
                    GroupNameWPath = group_name_with_path(GroupName, Config),
                    GroupResult = ?config(tc_group_result, Config),
                    case proplists:get_value(failed, GroupResult, []) of
                        [] ->
                            %% If there are no failed cases, then the only skipped cases present
                            %% in the group result are user-skipped cases.
                            %% In this case we do not want to fail the whole group.
                            do_update_group_status(ok, GroupNameWPath, 0, State);
                        Failed ->
                            %% If there are failed cases, it doesn't matter if the skipped cases
                            %% are user-skipped or auto-skipped (which might depend on `sequence`
                            %% group property being enabled or not) - we fail in either case.
                            %% TODO: Report to the respective GitHub PR
                            ct:pal("Failed in this group: ~p", [Failed]),
                            do_update_group_status(failed, GroupNameWPath, length(Failed), State)
                    end
            end
    end.

do_update_group_status(Status, GroupNameWPath, NFailed, State) ->
    maps:update_with(GroupNameWPath,
                     fun ({_PreviousStatus, {n_failed, NFailedAcc}}) ->
                             {Status, {n_failed, NFailedAcc + NFailed}}
                     end,
                     {Status, {n_failed, NFailed}},
                     State).

group_name_with_path(GroupName, Config) ->
    case lists:keyfind(tc_group_path, 1, Config) of
        false -> GroupName;
        {tc_group_path, PathProperties} ->
            [ proplists:get_value(name, ParentGroup)
              || ParentGroup <- PathProperties ] ++ [GroupName]
    end.

%% @doc Write down the number successful and failing groups in a per-suite groups.summary file.
write_groups_summary(Config, State) ->
    {Ok, Failed, FailedTests} = maps:fold(fun acc_groups_summary/3, {0, 0, 0}, State),
    PrivDir = ?config(priv_dir, Config),
    case PrivDir of
        undefined ->
            error(priv_dir_undefined);
        _ ->
            SuiteDir = filename:dirname(string:strip(PrivDir, right, $/)),
            ok = file:write_file(filename:join([SuiteDir, "groups.summary"]),
                                 io_lib:format("~p.\n"
                                               "~p.\n",
                                               [{groups_summary, {Ok, Failed}},
                                                {eventually_ok_tests, FailedTests}]))
    end.

acc_groups_summary(_GroupName, {ok, {n_failed, NFailedTests}},
                   {OkGroupsAcc, FailedGroupsAcc, EventuallyOkAcc}) ->
    {OkGroupsAcc + 1, FailedGroupsAcc,
     %% The group was repeated, but it eventually passed, so the tests must finally be ok.
     EventuallyOkAcc + NFailedTests};
acc_groups_summary(_GroupName, {__, {n_failed, _NFailedTests}},
                   {OkGroupsAcc, FailedGroupsAcc, EventuallyOkAcc}) ->
    {OkGroupsAcc, FailedGroupsAcc + 1,
     %% The group never succeeded, the failed tests are NOT eventually ok.
     EventuallyOkAcc}.
