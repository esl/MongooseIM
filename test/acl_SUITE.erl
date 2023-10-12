-module(acl_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-import(mongoose_config, [set_opt/2, unset_opt/1]).

all() ->
    [{group, dynamic_domains},
     {group, static_domains}].

groups() ->
    [{dynamic_domains, [], basic_test_cases() ++ host_type_test_cases()},
     {static_domains, [], basic_test_cases()}].

basic_test_cases() ->
    [
     all_rule_returns_allow,
     none_rule_returns_deny,
     basic_access_rules,
     compound_access_rules,
     host_specific_access_rules,
     all_and_none_specs,
     match_domain,
     different_specs_matching_the_same_user
    ].

host_type_test_cases() ->
    [
     match_host_specific_rule_for_host_type
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    async_helper:start(Config, mongoose_domain_sup, start_link, [[], []]).

end_per_suite(Config) ->
    async_helper:stop_all(Config).

init_per_group(dynamic_domains, Config) ->
    [{dynamic_domains, true}|Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_TC, Config) ->
    mongoose_config:set_opts(#{}),
    Config.

end_per_testcase(_TC, _Config) ->
    mongoose_config:erase_opts().

host_type() ->
    <<"test host type">>.

all_rule_returns_allow(_Config) ->
    JID = jid:make(<<"pawel">>, <<"phost">>, <<"test">>),
    ?assertEqual(allow, acl:match_rule(global, all, JID)),
    ?assertEqual(allow, acl:match_rule(<<"phost">>, all, JID)),
    ?assertEqual(allow, acl:match_rule(<<"localhost">>, all, JID)),
    ok.

none_rule_returns_deny(_Config) ->
    JID = jid:make(<<"gawel">>, <<"phost">>, <<"test">>),
    ?assertEqual(deny, acl:match_rule(global, none, JID)),
    ?assertEqual(deny, acl:match_rule(<<"phost">>, none, JID)),
    ?assertEqual(deny, acl:match_rule(<<"localhost">>, none, JID)),
    ok.

basic_access_rules(_Config) ->
    JID = jid:make(<<"pawel">>, <<"phost">>, <<"test">>),

    %% rule is not defined deny by default - deny
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),

    %% add the rule and recheck
    set_opt({access, global}, #{single_rule => [#{acl => all, value => allow}]}),
    ?assertEqual(allow, (acl:match_rule(global, single_rule, JID))),

    %% override it to deny rule
    set_opt({access, global}, #{single_rule => [#{acl => all, value => deny}]}),
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),

    %% deny by default
    set_opt({access, global}, #{single_rule => []}),
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),

    %% allow nobody
    set_opt({access, global}, #{single_rule => [#{acl => none, value => allow}]}),
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),
    ok.

host_specific_access_rules(Config) ->
    given_registered_domains(Config, [<<"poznan">>, <<"wroclaw">>]),

    PozAdmin = jid:make(<<"gawel">>, <<"poznan">>, <<"test">>),
    Pawel = jid:make(<<"pawel">>, <<"wroclaw">>, <<"test">>),

    set_opt({acl, <<"poznan">>}, #{admin_poz => acl(#{user => <<"gawel">>,
                                                      server => <<"poznan">>})}),
    set_opt({acl, <<"wroclaw">>}, #{admin_poz => acl(#{user => <<"gawel">>,
                                                       server => <<"poznan">>})}),
    set_opt({access, <<"poznan">>}, #{only_poz_admin => [#{acl => admin_poz, value => allow},
                                                         #{acl => all, value => deny}]}),
    set_opt({access, <<"wroclaw">>}, #{only_poz_admin => [#{acl => admin_poz, value => deny},
                                                          #{acl => all, value => allow}]}),

    ?assertEqual(allow, acl:match_rule(<<"poznan">>, <<"poznan">>, only_poz_admin, PozAdmin)),
    ?assertEqual(deny, acl:match_rule(<<"poznan">>, <<"wroclaw">>, only_poz_admin, Pawel)),

    ?assertEqual(deny, acl:match_rule(<<"wroclaw">>, <<"poznan">>, only_poz_admin, PozAdmin)),
    ?assertEqual(allow, acl:match_rule(<<"wroclaw">>, <<"wroclaw">>, only_poz_admin, Pawel)),
    ok.

compound_access_rules(Config) ->
    given_registered_domains(Config, [<<"krakow">>]),

    KrkAdmin = jid:make(<<"gawel">>, <<"krakow">>, <<"test">>),
    KrkNormal = jid:make(<<"pawel">>, <<"krakow">>, <<"test">>),

    %% add admin user rule
    set_opt({acl, global}, #{admin_wawa => acl(#{user => <<"gawel">>, server => <<"wawa">>}),
                             admin_krakow => acl(#{user => <<"gawel">>, server => <<"krakow">>})}),
    set_opt({access, global}, #{only_krakow_admin => [#{acl => admin_krakow, value => allow},
                                                      #{acl => all, value => deny}],
                                only_wawa_admin => [#{acl => admin_wawa, value => allow},
                                                    #{acl => all, value => deny}]}),

    ?assertEqual(deny, acl:match_rule(global, only_krakow_admin, KrkNormal)),
    ?assertEqual(deny, acl:match_rule(global, only_wawa_admin, KrkNormal)),

    ?assertEqual(allow, acl:match_rule(global, only_krakow_admin, KrkAdmin)),
    ?assertEqual(deny,  acl:match_rule(global, only_wawa_admin, KrkAdmin)),
    ok.

all_and_none_specs(Config) ->
    given_registered_domains(Config, [<<"zakopane">>]),

    User = jid:make(<<"pawel">>, <<"zakopane">>, <<"test">>),
    set_opt({acl, global}, #{a_users => acl(#{match => all}),
                             n_users => acl(#{match => none})}),
    set_opt({access, global}, #{all_users => [#{acl => a_users, value => allow},
                                              #{acl => all, value => deny}],
                                none_users => [#{acl => n_users, value => allow},
                                               #{acl => all, value => deny}]}),

    ?assertEqual(allow, acl:match_rule(global, <<"zakopane">>, all_users, User)),
    ?assertEqual(deny, acl:match_rule(global, <<"zakopane">>, none_users, User)),

    %% domain doesn't matter for 'all'
    ?assertEqual(allow, acl:match_rule(global, <<"any domain">>, all_users, User)),
    ?assertEqual(deny, acl:match_rule(global, <<"any domain">>, none_users, User)),
    ok.

match_domain(Config) ->
    given_registered_domains(Config, [<<"zakopane">>, <<"gdansk">>]),

    UserZa = jid:make(<<"pawel">>, <<"zakopane">>, <<"test">>),
    UserGd = jid:make(<<"pawel">>, <<"gdansk">>, <<"test">>),
    UserTo = jid:make(<<"pawel">>, <<"torun">>, <<"test">>),
    UserZb = jid:make(<<"pawel">>, <<"zakopane">>, <<"best">>),

    set_opt({acl, global}, #{a_users => [#{match => current_domain, resource => <<"test">>}],
                             b_users => [#{match => any_hosted_domain, resource => <<"test">>}],
                             c_users => [#{match => all, resource => <<"test">>}]}),
    set_opt({access, global}, #{rule => [#{acl => a_users, value => a},
                                         #{acl => b_users, value => b},
                                         #{acl => c_users, value => c},
                                         #{acl => all, value => d}]}),

    ?assertEqual(a, acl:match_rule(global, <<"zakopane">>, rule, UserZa)),
    ?assertEqual(b, acl:match_rule(global, <<"zakopane">>, rule, UserGd)),
    ?assertEqual(c, acl:match_rule(global, <<"zakopane">>, rule, UserTo)),
    ?assertEqual(d, acl:match_rule(global, <<"zakopane">>, rule, UserZb)),
    ok.

match_host_specific_rule_for_host_type(Config) ->
    given_registered_domains(Config, [<<"gdansk">>, <<"koszalin">>]),

    UserGd = jid:make(<<"pawel">>, <<"gdansk">>,  <<"res">>),

    set_opt({acl, <<"test type">>}, #{admin => acl(#{user => <<"pawel">>})}),
    set_opt({access, <<"test type">>}, #{allow_admin => [#{acl => admin, value => allow},
                                                         #{acl => all, value => deny}]}),

    %% Check for host type for a specific domain
    ?assertEqual(allow, acl:match_rule(<<"test type">>, <<"gdansk">>, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(<<"test type">>, <<"koszalin">>, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(<<"empty type">>, <<"gdansk">>, allow_admin, UserGd)),

    %% Check for host type for any domain
    ?assertEqual(allow, acl:match_rule(<<"test type">>,  allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(<<"empty type">>, allow_admin, UserGd)),

    %% Check globally for a specific domain
    ?assertEqual(deny, acl:match_rule(global, <<"gdansk">>, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, <<"koszalin">>, allow_admin, UserGd)),

    %% Check globally for any domain
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserGd)).

different_specs_matching_the_same_user(Config) ->
    given_registered_domains(Config, [<<"gdansk">>, <<"koszalin">>]),

    UserGd = jid:make(<<"pawel">>, <<"gdansk">>,  <<"res">>),
    UserKo = jid:make(<<"pawel">>, <<"koszalin">>,<<"res1">>),

    %% invariand we are going to change admin acl only
    set_opt({access, global}, #{allow_admin => [#{acl => admin, value => allow},
                                                #{acl => all, value => deny}]}),

    %% match on pawel
    set_opt({acl, global}, #{admin => acl(#{user => <<"pawel">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on pawel@gdansk
    set_opt({acl, global}, #{admin => acl(#{user => <<"pawel">>, server => <<"gdansk">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on gdansk
    set_opt({acl, global}, #{admin => acl(#{server => <<"gdansk">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on res
    set_opt({acl, global}, #{admin => acl(#{resource => <<"res">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user regex
    set_opt({acl, global}, #{admin => acl(#{user_regexp => <<"^paw.*">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user regex
    set_opt({acl, global}, #{admin => acl(#{user_regexp => <<"^paw.*">>, server => <<"gdansk">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server regex
    set_opt({acl, global}, #{admin => acl(#{server_regexp => <<"^gda.*">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on resource regex
    set_opt({acl, global}, #{admin => acl(#{resource_regexp => <<"^res.*">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match node regex
    set_opt({acl, global}, #{admin => acl(#{user_regexp => <<"^pawe.*">>,
                                            server_regexp => <<"^gda.*">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user glob
    set_opt({acl, global}, #{admin => acl(#{user_glob => <<"paw??">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user glob
    set_opt({acl, global}, #{admin => acl(#{user_glob => <<"paw??">>, server => <<"gdansk">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server glob
    set_opt({acl, global}, #{admin => acl(#{server_glob => <<"gda*">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server glob
    set_opt({acl, global}, #{admin => acl(#{resource_glob => <<"re*">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on node glob
    set_opt({acl, global}, #{admin => acl(#{user_glob => <<"pawe?">>, server_glob => <<"gd*">>})}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    ok.

acl(Spec) ->
    [maps:merge(#{match => current_domain}, Spec)].

given_registered_domains(Config, DomainsList) ->
    case proplists:get_value(dynamic_domains, Config, false) of
        true ->
            register_dynamic_domains(DomainsList);
        false ->
            register_static_domains(DomainsList)
    end.

register_static_domains(DomainsList) ->
    mongoose_config:set_opt(hosts, DomainsList),
    mongoose_config:set_opt(host_types, []),
    mongoose_domain_sup:restart_core([]).

register_dynamic_domains(DomainsList) ->
    mongoose_config:set_opt(hosts, []),
    mongoose_config:set_opt(host_types, [<<"test type">>, <<"empty type">>]),
    mongoose_domain_sup:restart_core([]),
    [mongoose_domain_core:insert(Domain, <<"test type">>, test) || Domain <- DomainsList].
