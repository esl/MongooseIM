-module(multiple_config_cth).
%% @doc Support running a test multiple times with different configuration options
%% inside a single test group. Used by amp_big_SUITE.

%% Callbacks

-export([id/1, init/2]).

-export([pre_init_per_suite/3, post_init_per_suite/4,
         pre_end_per_suite/3, post_end_per_suite/4,
         pre_init_per_group/3, post_init_per_group/4,
         pre_end_per_group/3, post_end_per_group/4,
         pre_init_per_testcase/3, post_init_per_testcase/4,
         pre_end_per_testcase/3, post_end_per_testcase/4]).

-export([on_tc_fail/3, on_tc_skip/3, terminate/1]).

-export([add_config/2, flatten_and_strip_config/1]).

-record(state, {conf_fun, group_conf}).

%% CTH callbacks

id(_) ->
    ?MODULE.

init(_Id, ConfFun) ->
    {ok, #state{conf_fun = ConfFun}}.

pre_init_per_suite(_Suite, Config, State) ->
    {Config, State}.

post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.

post_end_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

pre_init_per_group(Group, Config, State = #state{conf_fun = ConfFun}) ->
    {Config, State#state{group_conf = maps:from_list(ConfFun(Group))}}.

post_init_per_group(_Group, _Config, Return, State) ->
    {Return, State}.

pre_end_per_group(_Group, Config, State) ->
    {Config, State}.

post_end_per_group(_Group, _Config, Return, State) ->
    {Return, State}.

pre_init_per_testcase(TC, Config, #state{group_conf = Conf} = State) ->
    case Conf of
        #{TC := [ExtraConfig | Rest]} ->
            ct:log("Extra test config: ~p", [ExtraConfig]),
            {ExtraConfig ++ Config, State#state{group_conf = Conf#{TC := Rest}}};
        _ -> {Config, State}
    end.

post_init_per_testcase(_TC, _Config, Return, State) ->
    {Return, State}.

pre_end_per_testcase(_TC, Config, State) ->
    {Config, State}.

post_end_per_testcase(_TC, _Config, Return, State) ->
    {Return, State}.

on_tc_fail(_TC, _Reason, State) ->
    State.

on_tc_skip(_TC, _Reason, State) ->
    State.

terminate(_State) ->
    ok.

%% Helper API

add_config(Configs, Cases) ->
    [{TC, Configs} || TC <- Cases].

flatten_and_strip_config(CasesWithConfig) ->
    lists:flatmap(fun({TC, Confs}) -> [TC || _ <- Confs] end, CasesWithConfig).
