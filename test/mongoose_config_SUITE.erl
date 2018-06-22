-module(mongoose_config_SUITE).
-author("mikhail uvarov").
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").


all() -> [
    does_pattern_match_case,
    flatten_state_case,
    parse_config_with_underscore_pattern_case,
    node_specific_options_presents_case,
    node_specific_options_missing_case,
    cluster_reload_strategy_case,
    get_config_diff_case
].

init_per_suite(C) ->
    stringprep:start(),
    C.

end_per_suite(C) -> C.
init_per_testcase(_TC, C) -> C.
end_per_testcase(_TC, C) -> C.


%% TESTS

%% Check each pattern with cases that match or not
does_pattern_match_case(_C) ->
    [check_case(Case) || Case <- match_cases()],
    ok.

check_case(Case=#{pattern := Pattern, matches := Matches}) ->
    [check_match(Match, Pattern) || Match <- Matches],
    [check_nomatch(Match, Pattern) || Match <- maps:get(nomatches, Case, [])],
    ok.

check_match(Subject, Pattern) ->
    Match = mongoose_config:does_pattern_match(Subject, Pattern),
    case Match of
        true ->
            ok;
        false ->
            ct:fail(#{issue => check_match_failed,
                      subject => Subject,
                      pattern => Pattern})
    end.

check_nomatch(Subject, Pattern) ->
    Match = mongoose_config:does_pattern_match(Subject, Pattern),
    case Match of
        false ->
            ok;
        true ->
            ct:fail(#{issue => check_nomatch_failed,
                      subject => Subject,
                      pattern => Pattern})
    end.

match_cases() ->
    [
        #{pattern => '_',
          matches => [
            1, {}, [1]
          ]},

        #{pattern => [{'_', <<"host">>}],
          matches => [
            [{test, <<"host">>}]
          ],
          nomatches => [
            [{test, <<"host2">>}],
            [{test, <<"host">>}, key2]
          ]
         }

    ].


flatten_state_case(_C) ->
    State = mongoose_config:parse_terms(cool_mod_mam_config()),
    ?assertEqual(cool_mod_mam_config_flatten(),
                 mongoose_config:state_to_flatten_local_opts(State)).

cool_mod_mam_config() ->
    [{hosts, ["localhost"]},
     {modules, [{mod_mam, [{pool, cool_pool}]}]}].

cool_mod_mam_config_flatten() ->
    [{[l,odbc_pools],[]},
     {[h,<<"localhost">>,modules],flatten},
     {[h,<<"localhost">>,module,mod_mam],flatten},
     {[h,<<"localhost">>,module_opt,mod_mam,pool],cool_pool}].


%% Check that underscore is not treated as a config macro by config parser
parse_config_with_underscore_pattern_case(_C) ->
    mongoose_config:parse_terms(node_specific_cool_mod_mam_config()).

%% Check that we can convert state into node_specific_options list
node_specific_options_presents_case(_C) ->
    State = mongoose_config:parse_terms(node_specific_cool_mod_mam_config()),
    NodeOpts = mongoose_config:state_to_global_opt(node_specific_options, State, missing),
    ?assertEqual([ [h,'_',module_opt,mod_mam,pool] ],
                 NodeOpts).

%% Check that we would not crash if node_specific_options is not defined
node_specific_options_missing_case(_C) ->
    State = mongoose_config:parse_terms(cool_mod_mam_config()),
    NodeOpts = mongoose_config:state_to_global_opt(node_specific_options, State, missing),
    ?assertEqual(missing, NodeOpts).

node_specific_cool_mod_mam_config() ->
    [{hosts, ["localhost"]},
     {node_specific_options, [ [h,'_',module_opt,mod_mam,pool] ]},
     {modules, [{mod_mam, [{pool, cool_pool}]}]}].


terms_to_categorized_options(Terms) ->
    State = mongoose_config:parse_terms(Terms),
    mongoose_config:state_to_categorized_options(State).

cluster_reload_strategy_case(_C) ->
    Strategy = mongoose_config:cluster_reload_strategy(example_config_states()),
    ct:pal("Strategy ~p", [Strategy]),
    ct:fail(oops),
    ok.


%% ejabberd_config:config_states/0 example
%% Password was modified on both nodes
example_config_states() ->
    [config_node1_config_v2(),
     config_node2_config_v2()].

%% node1_config_v1 configuration both in memory and on disc
config_node1_config_v1() ->
    Terms = node1_config_v1(),
    #{mongoose_node => mim1,
      config_file => "/etc/ejabberd.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms),
      ondisc_config_terms => Terms}.

%% node2_config_v1 configuration both in memory and on disc
config_node2_config_v1() ->
    Terms = node2_config_v1(),
    #{mongoose_node => mim2,
      config_file => "/etc/ejabberd.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms),
      ondisc_config_terms => Terms}.

%% node1_config_v1 configuration in memory
%% node1_config_v2 configuration on disc
config_node1_config_v2() ->
    Terms_v1 = node1_config_v1(),
    Terms_v2 = node1_config_v2(),
    #{mongoose_node => mim1,
      config_file => "/etc/ejabberd.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms_v1),
      ondisc_config_terms => Terms_v2}.

%% node2_config_v1 configuration in memory
%% node2_config_v2 configuration on disc
config_node2_config_v2() ->
    Terms_v1 = node2_config_v1(),
    Terms_v2 = node2_config_v2(),
    #{mongoose_node => mim2,
      config_file => "/etc/ejabberd.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms_v1),
      ondisc_config_terms => Terms_v2}.

node1_config_v1() ->
    [
        {hosts, ["localhost"]},
        {node_specific_options,
         [
            [h,'_',module_opt,mod_mam,pool],
            [h,'_',module_opt,mod_mam_muc,pool]
         ]},
        {modules,
         [
            {mod_mam,
             [
                {pool, node1_pool},
                {iqdisc, parallel},
                {password, <<"secret">>}
             ]},
            {mod_mam_muc, [{pool, node1_pool}]}
         ]}
    ].

node1_config_v2() ->
    %% Different more secure password was applied in version 2
    [
        {hosts, ["localhost"]},
        {node_specific_options,
         [
            [h,'_',module_opt,mod_mam,pool],
            [h,'_',module_opt,mod_mam_muc,pool]
         ]},
        {modules,
         [
            {mod_mam,
             [
                {pool, node1_pool},
                {iqdisc, parallel},
                {password, <<"secret123">>}
             ]},
            {mod_mam_muc, [{pool, node1_pool}]}
         ]}
    ].

node2_config_v1() ->
    %% Pools are different for different nodes
    [
        {hosts, ["localhost"]},
        {node_specific_options,
         [
            [h,'_',module_opt,mod_mam,pool],
            [h,'_',module_opt,mod_mam_muc,pool]
         ]},
        {modules,
         [
            {mod_mam,
             [
                {pool, node2_pool},
                {iqdisc, parallel},
                {password, <<"secret">>}
             ]},
            {mod_mam_muc, [{pool, node1_pool}]}
         ]}
    ].

node2_config_v2() ->
    %% Pools are different for different nodes
    %% Different more secure password was applied in version 2
    [
        {hosts, ["localhost"]},
        {node_specific_options,
         [
            [h,'_',module_opt,mod_mam,pool],
            [h,'_',module_opt,mod_mam_muc,pool]
         ]},
        {modules,
         [
            {mod_mam,
             [
                {pool, node2_pool},
                {iqdisc, parallel},
                {password, <<"secret123">>}
             ]},
            {mod_mam_muc, [{pool, node1_pool}]}
         ]}
    ].


get_config_diff_case(_C) ->
    %% Calculate changes to node1 reload_local to transit from v1 to v2
    Terms_v1 = node1_config_v1(),
    Terms_v2 = node1_config_v2(),
    CatOptions = terms_to_categorized_options(Terms_v1),
    State = mongoose_config:parse_terms(Terms_v2),
    Diff = mongoose_config:get_config_diff(State, CatOptions),
    #{local_hosts_changes := #{ to_reload := ToReload }} = Diff,
    [{ {modules,<<"localhost">>}, OldModules, NewModules }] = ToReload,
    ?assertEqual(<<"secret">>, get_module_opt(mod_mam, password, OldModules)),
    ?assertEqual(<<"secret123">>, get_module_opt(mod_mam, password, NewModules)),
    ok.

get_module_opt(Module, Key, Modules) ->
    {Module, Opts} = lists:keyfind(Module, 1, Modules),
    proplists:get_value(Key, Opts).
