-module(ct_helper).
-compile([export_all]).

is_ct_running() ->
    ct:get_status() =/= no_tests_running.

%% @doc See repeat_all_until_all_ok/2
repeat_all_until_all_ok(GroupDefs) ->
    repeat_all_until_all_ok(GroupDefs, 3).

%% @doc repeat_all_until_all_ok/2 will rewrite your group definitions so that
%% the `{repeat_until_all_ok, Retries}` property is added to all of them.
%% For example, for the following definitions:
%%
%%   [{accounts, [sequence], [change_password, check_password_hash]},
%%    {sessions, [sequence], [num_resources_num, kick_session]},
%%    {vcard, [sequence], [vcard_rw, vcard2_rw, vcard2_multi_rw]}]
%%
%% It will produce:
%%
%%   [{accounts, [{repeat_until_all_ok, 5}, sequence], [change_password, check_password_hash]},
%%    {sessions, [{repeat_until_all_ok, 5}, sequence], [num_resources_num, kick_session]},
%%    {vcard, [{repeat_until_all_ok, 5}, sequence], [vcard_rw, vcard2_rw, vcard2_multi_rw]}]
%%
%% However, if `repeat_until_all_ok` is already defined in Properties it will not be redefined.
%% This allows you to declaratively override Retries for a particular group definition,
%% while allowing repeat_all_until_all_ok/2 to add the default for the remaining groups.
repeat_all_until_all_ok(GroupDefs, Retries) ->
    [ {Name, repeat_type(repeat_until_all_ok, Retries, Properties), Tests}
      || {Name, Properties, Tests} <- prepare_group_defs(GroupDefs) ].

repeat_type(RepeatType, Retries, Properties) ->
    %% lists:keyfind expects tuples, but Properties might contain just RepeatType atoms,
    %% not their tuple forms. Make sure they're expanded to {RepeatType, true} if they
    %% are present.
    case lists:keyfind(RepeatType, 1, proplists:unfold(Properties)) of
        false ->
            [{RepeatType, Retries} | Properties];
        {RepeatType, _} ->
            ct:pal("~s present in ~p - leaving as is", [RepeatType, Properties]),
            Properties
    end.

%% @doc Expand 2-element group definitions into 3-element group definitions.
prepare_group_defs(GroupDefs) ->
    lists:map(fun ({Name, Properties, Tests}) -> {Name, Properties, Tests};
                  ({Name, Tests}) -> {Name, [], Tests} end,
              GroupDefs).
