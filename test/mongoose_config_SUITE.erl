-module(mongoose_config_SUITE).
-author("mikhail uvarov").
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").


all() -> [
    does_pattern_match_case,
    flat_state_case,
    parse_config_with_underscore_pattern_case,
    node_specific_options_presents_case,
    node_specific_options_missing_case,
    states_to_reloading_context_case,
    auth_method_and_cluster_reload_case,
    no_duplicate_options_case,
    get_config_diff_case,
    flat_module_subopts_case,
    expand_opts_case,
    expand_module_subopts_case,
    parse_config_with_required_files_case
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
    Match = mongoose_config_flat:does_pattern_match(Subject, Pattern),
    case Match of
        true ->
            ok;
        false ->
            ct:fail(#{issue => check_match_failed,
                      subject => Subject,
                      pattern => Pattern})
    end.

check_nomatch(Subject, Pattern) ->
    Match = mongoose_config_flat:does_pattern_match(Subject, Pattern),
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


flat_state_case(_C) ->
    State = mongoose_config_parser:parse_terms(cool_mod_mam_config()),
    ?assertEqual(cool_mod_mam_config_flat(),
                 mongoose_config_reload:state_to_flat_local_opts(State)).

cool_mod_mam_config() ->
    [{hosts, ["localhost"]},
     {modules, [{mod_mam, [{pool, cool_pool}]}]}].

cool_mod_mam_config_flat() ->
    [{[h,<<"localhost">>,modules],'FLAT'},
     {[h,<<"localhost">>,module,mod_mam],'FLAT'},
     {[h,<<"localhost">>,module_opt,mod_mam,pool],cool_pool}].

flat_module_subopts_case(_C) ->
    State = mongoose_config_parser:parse_terms(gd_config()),
    FlatOpts = mongoose_config_reload:state_to_flat_local_opts(State),
    NumConnsKey = [h,<<"localhost">>,module_subopt,mod_global_distrib,
                   connections,num_of_connections],
    ConnsKey = [h,<<"localhost">>,module_opt,mod_global_distrib,
                connections],
    RedisServerKey = [h,<<"localhost">>,module_subopt,mod_global_distrib,
                redis,server],
    ?assertEqual(22, proplists:get_value(NumConnsKey, FlatOpts)),
    ?assertEqual('FLAT', proplists:get_value(ConnsKey, FlatOpts)),
    ?assertEqual("172.16.0.3", proplists:get_value(RedisServerKey, FlatOpts)),
    ok.

expand_opts_case(_C) ->
    State = mongoose_config_parser:parse_terms(cool_mod_mam_config()),
    FlatOpts = mongoose_config_reload:state_to_flat_local_opts(State),
    ExpandedOpts = mongoose_config_flat:expand_all_opts(FlatOpts),
    CatOpts = mongoose_config_reload:state_to_categorized_options(State),
    ?assertEqual(maps:get(local_config, CatOpts),
                 maps:get(local_config, ExpandedOpts)),
    ?assertEqual(maps:get(host_config, CatOpts),
                 maps:get(host_config, ExpandedOpts)),
    ok.

expand_module_subopts_case(_C) ->
    State = mongoose_config_parser:parse_terms(gd_config()),
    FlatOpts = mongoose_config_reload:state_to_flat_local_opts(State),
    ExpandedOpts = mongoose_config_flat:expand_all_opts(FlatOpts),
    CatOpts = mongoose_config_reload:state_to_categorized_options(State),
    ?assertEqual(maps:get(local_config, CatOpts),
                 maps:get(local_config, ExpandedOpts)),
    ?assertEqual(maps:get(host_config, CatOpts),
                 maps:get(host_config, ExpandedOpts)),
    ok.

gd_config() ->
    [{hosts, ["localhost"]},
     {modules, [

         {mod_global_distrib, [
             {global_host, "example.com"},
             {local_host, "datacenter1.example.com"},
             {connections, [
                   {endpoints, [{"172.16.0.2", 5555}]},
                   {num_of_connections, 22},
                   {tls_opts, [
                         {certfile, "/home/user/dc1.pem"},
                         {cafile, "/home/user/ca.pem"}
                        ]}
                  ]},
             {cache, [
                   {domain_lifetime_seconds, 60}
                  ]},
             {bounce, [
                   {resend_after_ms, 300},
                   {max_retries, 3}
                  ]},
             {redis, [
                   {pool_size, 24},
                   {server, "172.16.0.3"}
                  ]}
            ]}

      ]}
    ].

auth_config() ->
    [{hosts, ["localhost", "anonymous.localhost"]},
     {auth_method, internal},
     {host_config,"anonymous.localhost",
       [{auth_method,anonymous}]}].

auth_config_states() ->
    [auth_config_node1_config_v1()].

auth_config_node1_config_v1() ->
    Terms = auth_config(),
    #{mongoose_node => mim1,
      config_file => "/etc/mongooseim.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms),
      ondisc_config_terms => Terms,
      missing_files => [], required_files => []}.

auth_host_local_config() ->
    Terms = auth_config(),
    CatOpts = terms_to_categorized_options(Terms),
    maps:get(host_config, CatOpts).

auth_config_state() ->
    Terms = auth_config(),
    mongoose_config_parser:parse_terms(Terms).

%% Check that underscore is not treated as a config macro by config parser
parse_config_with_underscore_pattern_case(_C) ->
    mongoose_config_parser:parse_terms(node_specific_cool_mod_mam_config()).

%% Check that we can convert state into node_specific_options list
node_specific_options_presents_case(_C) ->
    State = mongoose_config_parser:parse_terms(node_specific_cool_mod_mam_config()),
    NodeOpts = mongoose_config_parser:state_to_global_opt(node_specific_options, State, missing),
    ?assertEqual([ [h,'_',module_opt,mod_mam,pool] ],
                 NodeOpts).

%% Check that we would not crash if node_specific_options is not defined
node_specific_options_missing_case(_C) ->
    State = mongoose_config_parser:parse_terms(cool_mod_mam_config()),
    NodeOpts = mongoose_config_parser:state_to_global_opt(node_specific_options, State, missing),
    ?assertEqual(missing, NodeOpts).

node_specific_cool_mod_mam_config() ->
    [{hosts, ["localhost"]},
     {node_specific_options, [ [h,'_',module_opt,mod_mam,pool] ]},
     {modules, [{mod_mam, [{pool, cool_pool}]}]}].


terms_to_categorized_options(Terms) ->
    State = mongoose_config_parser:parse_terms(Terms),
    mongoose_config_reload:state_to_categorized_options(State).

states_to_reloading_context_case(_C) ->
    Context = mongoose_config_reload:states_to_reloading_context(example_config_states()),
%   ct:pal("Context ~p", [Context]),
    ok.

%% Check that auth_method is treated correctly
auth_method_and_cluster_reload_case(_C) ->
    Context = mongoose_config_reload:states_to_reloading_context(auth_config_states()),
%   ct:pal("Auth context ~p", [Context]),
    ok.

no_duplicate_options_case(_C) ->
    HostOpts = auth_host_local_config(),
    %% Check that there are no duplicates
    %% Bad case example:
    %% HostOpts =
    %%  [{local_config,{auth_method,<<"localhost">>},internal},
    %%   {local_config,{auth_method,<<"anonymous.localhost">>},internal},
    %%   {local_config,{auth_method,<<"anonymous.localhost">>},anonymous}]
    ?assertEqual({local_config,{auth_method,<<"anonymous.localhost">>},anonymous},
                 lists:keyfind({auth_method,<<"anonymous.localhost">>}, 2,
                               HostOpts)),
    ?assertEqual({local_config,{auth_method,<<"anonymous.localhost">>},anonymous},
                 lists:keyfind({auth_method,<<"anonymous.localhost">>}, 2,
                               lists:reverse(HostOpts))),
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
      config_file => "/etc/mongooseim.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms),
      ondisc_config_terms => Terms,
      missing_files => [], required_files => []}.

%% node2_config_v1 configuration both in memory and on disc
config_node2_config_v1() ->
    Terms = node2_config_v1(),
    #{mongoose_node => mim2,
      config_file => "/etc/mongooseim.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms),
      ondisc_config_terms => Terms,
      missing_files => [], required_files => []}.

%% node1_config_v1 configuration in memory
%% node1_config_v2 configuration on disc
config_node1_config_v2() ->
    Terms_v1 = node1_config_v1(),
    Terms_v2 = node1_config_v2(),
    #{mongoose_node => mim1,
      config_file => "/etc/mongooseim.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms_v1),
      ondisc_config_terms => Terms_v2,
      missing_files => [], required_files => []}.

%% node2_config_v1 configuration in memory
%% node2_config_v2 configuration on disc
config_node2_config_v2() ->
    Terms_v1 = node2_config_v1(),
    Terms_v2 = node2_config_v2(),
    #{mongoose_node => mim2,
      config_file => "/etc/mongooseim.cfg",
      loaded_categorized_options => terms_to_categorized_options(Terms_v1),
      ondisc_config_terms => Terms_v2,
      missing_files => [], required_files => []}.

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
    State = mongoose_config_parser:parse_terms(Terms_v2),
    Diff = mongoose_config_reload:get_config_diff(State, CatOptions),
    #{local_hosts_changes := #{ to_reload := ToReload }} = Diff,
    [{ {modules,<<"localhost">>}, OldModules, NewModules }] = ToReload,
    ?assertEqual(<<"secret">>, get_module_opt(mod_mam, password, OldModules)),
    ?assertEqual(<<"secret123">>, get_module_opt(mod_mam, password, NewModules)),
    ok.

get_module_opt(Module, Key, Modules) ->
    {Module, Opts} = lists:keyfind(Module, 1, Modules),
    proplists:get_value(Key, Opts).


config_with_required_files() ->
    [
        {hosts, ["localhost"]},
        {s2s_certfile, "priv/ssl/fake_server.pem"},
        {domain_certfile, "localhost", "priv/ssl/localhost_server.pem"}
    ].

parse_config_with_required_files_case(_C) ->
    State = mongoose_config_parser:parse_terms(config_with_required_files()),
    ?assertEqual(["priv/ssl/localhost_server.pem",
                  "priv/ssl/fake_server.pem"],
                 mongoose_config_parser:state_to_required_files(State)),
    ok.
