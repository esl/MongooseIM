-module(ct_helper).
-export([is_ct_running/0,
         repeat_all_until_all_ok/1,
         repeat_all_until_all_ok/2,
         repeat_all_until_any_fail/1,
         repeat_all_until_any_fail/2,
         groups_to_all/1,
         get_preset_var/3]).

-type group_name() :: atom().

-type group_def_incomplete() :: {group_name(), [any()]}.
-type group_def_dirty() :: {group_name(), group_props() | group_props_dirty(), [any()]}.
-type group_def() :: {group_name(), group_props(), [any()]}.

-type group_props_dirty() :: [parallel | sequence | shuffle() | repeat_type()].
-type group_props() :: [parallel | sequence | shuffle() | {repeat_type(), repeat_num()}].
-type shuffle() :: shuffle | {shuffle, {integer(), integer(), integer()}}.
-type repeat_type() :: repeat | repeat_until_all_ok | repeat_until_all_fail |
                       repeat_until_any_ok | repeat_until_any_fail.
-type repeat_num() :: integer(). %% We exclude forever

-spec is_ct_running() -> boolean().
is_ct_running() ->
    ct:get_status() =/= no_tests_running.

%% @doc See repeat_all_until_all_ok/2
-spec repeat_all_until_all_ok([group_def() | group_def_dirty() | group_def_incomplete()]) ->
    [group_def()].
repeat_all_until_all_ok(GroupDefs) ->
    repeat_all_until_all_ok(GroupDefs, 3).

-spec repeat_all_until_any_fail([group_def() | group_def_dirty() | group_def_incomplete()]) ->
    [group_def()].
repeat_all_until_any_fail(GroupDefs) ->
    repeat_all_until_any_fail(GroupDefs, 100).

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
-spec repeat_all_until_all_ok([group_def() | group_def_dirty() | group_def_incomplete()],
                              repeat_num()) ->
    [group_def()].
repeat_all_until_all_ok(GroupDefs, Retries) ->
    [ {Name, maybe_add_repeat_type(repeat_until_all_ok, Retries, Properties), Tests}
      || {Name, Properties, Tests} <- prepare_group_defs(GroupDefs) ].

-spec repeat_all_until_any_fail([group_def() | group_def_dirty() | group_def_incomplete()],
                                 repeat_num()) ->
    [group_def()].
repeat_all_until_any_fail(GroupDefs, Retries) ->
    [ {Name, maybe_add_repeat_type(repeat_until_any_fail, Retries, Properties), Tests}
      || {Name, Properties, Tests} <- prepare_group_defs(GroupDefs) ].

-spec maybe_add_repeat_type(repeat_type(), repeat_num(), group_props()) -> group_props().
maybe_add_repeat_type(RepeatType, Retries, Properties) ->
    case lists:any(fun(El) -> proplists:is_defined(El, Properties) end, all_repeat_modes()) of
        true ->
            Properties;
        _ ->
            [{RepeatType, Retries} | Properties]
    end.

%% @doc Expand dirty group definitions into 3-elements clean group definitions.
-spec prepare_group_defs([group_def() | group_def_dirty() | group_def_incomplete()]) ->
    [group_def()].
prepare_group_defs(GroupDefs) ->
    lists:map(fun ({Name, Tests}) -> {Name, [], Tests};
                  ({Name, Properties, Tests}) ->
                      {Name, clean_properties(Properties), Tests}
              end,
              GroupDefs).

%% Properties might contain just RepeatType atoms,
%% so we expand them to {RepeatType, sensible_maximum} if so.
% -spec clean_properties(group_props() | group_props_dirty()) -> group_props().
clean_properties(Props) when is_list(Props) ->
    Max = sensible_maximum_repeats(),
    proplists:expand(
      lists:flatten(
        [[{R, [{R, Max}]}, {{R, forever}, [{R, Max}]}]
         || R <- all_repeat_modes()]), Props).

all_repeat_modes() ->
    [repeat,
     repeat_until_all_ok,
     repeat_until_any_ok,
     repeat_until_all_fail,
     repeat_until_any_fail].

sensible_maximum_repeats() ->
    case is_ct_started() of
        true ->
            ct:get_config(sensible_maximum_repeats, 100);
        false -> %% In case it's called from test-runner-complete script
            100
    end.

is_ct_started() ->
    lists:keymember(common_test, 1, application:which_applications()).

groups_to_all(Groups) ->
    [{group, Name} || {Name, _Opts, _Cases} <- Groups].

get_preset_var(Config, Opt, Def) ->
    case proplists:get_value(preset, Config, undefined) of
        Preset ->
            PresetAtom = list_to_existing_atom(Preset),
            ct:get_config({presets, toml, PresetAtom, Opt}, Def);
        _ ->
            Def
    end.
