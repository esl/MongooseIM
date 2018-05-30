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
    ct:pal("post_end_per_suite config: ~p", [Config]),
    ct:pal("post_end_per_suite state: ~p", [State]),
    ct:pal("post_end_per_suite return: ~p", [Return]),
    write_groups_summary(Config, State),
    {Return, State}.

post_end_per_group(GroupName, Config, Return, State) ->
    ct:pal("post_end_per_group config: ~p", [Config]),
    ct:pal("post_end_per_group state: ~p", [State]),
    ct:pal("post_end_per_group return: ~p", [Return]),
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
                            State#{GroupNameWPath => ok};
                        Failed ->
                            %% If there are failed cases, it doesn't matter if the skipped cases
                            %% are user-skipped or auto-skipped (which might depend on `sequence`
                            %% group property being enabled or not) - we fail in either case.
                            %% TODO: Report to the respective GitHub PR
                            ct:pal("Failed in this group: ~p", [Failed]),
                            State#{GroupNameWPath => failed}
                    end
            end
    end.

group_name_with_path(GroupName, Config) ->
    case lists:keyfind(tc_group_path, 1, Config) of
        false -> GroupName;
        {tc_group_path, PathProperties} ->
            [ proplists:get_value(name, ParentGroup)
              || ParentGroup <- PathProperties ] ++ [GroupName]
    end.

%% @doc Write down the number successful and failing groups in a per-suite groups.summary file.
write_groups_summary(Config, State) ->
    {Ok, Failed} = maps:fold(fun (_GroupName, ok, {OkAcc, FailedAcc}) -> {OkAcc + 1, FailedAcc};
                                 (_GroupName, __, {OkAcc, FailedAcc}) -> {OkAcc, FailedAcc + 1} end,
                             {0, 0},
                             State),
    PrivDir = ?config(priv_dir, Config),
    case PrivDir of
        undefined ->
            error(priv_dir_undefined);
        _ ->
            SuiteDir = filename:dirname(string:strip(PrivDir, right, $/)),
            ok = file:write_file(filename:join([SuiteDir, "groups.summary"]),
                                 io_lib:format("~p.", [{groups_summary, {Ok, Failed}}]))
    end.
