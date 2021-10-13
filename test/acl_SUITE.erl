-module(acl_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


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
     global_host_priority,
     all_and_none_specs,
     invalid_spec,
     different_specs_matching_the_same_user
    ].

host_type_test_cases() ->
    [
     match_host_specific_rule_for_host_type,
     match_global_rule_for_host_type
    ].

init_per_suite(Config) ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    mongoose_domain_api:stop(),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    meck:unload(),
    ok.

init_per_group(dynamic_domains, Config) ->
    [{dynamic_domains, true}|Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_TC, Config) ->
    given_clean_config(),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

all_rule_returns_allow(_Config) ->
    JID = jid:make(<<"pawel">>, <<"phost">>, <<"test">>),
    ?assertEqual(allow, acl:match_rule(global, all, JID)),
    ?assertEqual(allow, acl:match_rule(<<"phost">>, all, JID)),
    ?assertEqual(allow, acl:match_rule(<<"localhost">>, all, JID)),
    ok.

none_rule_returns_deny(_Config) ->
    JID = jid:make(<<"gawel">>, <<"phost">>, <<"test">>),
    ?assertEqual(deny, acl:match_rule(global, none, JID)),
    ?assertEqual(deny, acl:match_rule(<<"phosty">>, none, JID)),
    ?assertEqual(deny, acl:match_rule(<<"localhosty">>, none, JID)),
    ok.

basic_access_rules(_Config) ->
    JID = jid:make(<<"pawel">>, <<"phost">>, <<"test">>),

    %% rule is not defined deny by default - deny
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),

    %% add the rule and recheck
    set_global_rule(single_rule, [{allow, all}]),
    ?assertEqual(allow, (acl:match_rule(global, single_rule, JID))),

    %% override it to deny rule
    set_global_rule(single_rule, [{deny, all}]),
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),

    %% deny by default
    set_global_rule(single_rule, []),
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),

    %% allow nobody
    set_global_rule(single_rule, [{allow, none}]),
    ?assertEqual(deny, (acl:match_rule(global, single_rule, JID))),
    ok.

host_specific_access_rules(Config) ->
    given_registered_domains(Config, [<<"poznan">>, <<"wroclaw">>]),

    PozAdmin = jid:make(<<"gawel">>, <<"poznan">>, <<"test">>),
    Pawel = jid:make(<<"pawel">>, <<"wroclaw">>, <<"test">>),

    set_acl(global, admin_poz, {user, <<"gawel">>, <<"poznan">>}),

    set_host_rule(only_poz_admin, <<"poznan">>, [{allow, admin_poz}, {deny, all}]),
    set_host_rule(only_poz_admin, <<"wroclaw">>, [{deny, admin_poz}, {allow, all}]),

    ?assertEqual(allow, acl:match_rule(<<"poznan">>, only_poz_admin, PozAdmin)),
    ?assertEqual(deny, acl:match_rule(<<"poznan">>, only_poz_admin, Pawel)),

    ?assertEqual(deny, acl:match_rule(<<"wroclaw">>, only_poz_admin, PozAdmin)),
    ?assertEqual(allow, acl:match_rule(<<"wroclaw">>, only_poz_admin, Pawel)),
    ok.

compound_access_rules(Config) ->
    given_registered_domains(Config, [<<"krakow">>]),

    KrkAdmin = jid:make(<<"gawel">>, <<"krakow">>, <<"test">>),
    KrkNormal = jid:make(<<"pawel">>, <<"krakow">>, <<"test">>),

    %% add admin user rule
    set_acl(global, admin_wawa, {user, <<"gawel">>, <<"wawa">>}),
    set_acl(global, admin_krakow, {user, <<"gawel">>, <<"krakow">>}),

    set_global_rule(only_krakow_admin, [{allow, admin_krakow}, {deny, all}]),
    set_global_rule(only_wawa_admin, [{allow, admin_wawa}, {deny, all}]),

    ?assertEqual(deny, acl:match_rule(global, only_krakow_admin, KrkNormal)),
    ?assertEqual(deny, acl:match_rule(global, only_wawa_admin, KrkNormal)),

    ?assertEqual(allow, acl:match_rule(global, only_krakow_admin, KrkAdmin)),
    ?assertEqual(deny,  acl:match_rule(global, only_wawa_admin, KrkAdmin)),
    ok.

global_host_priority(Config) ->
    given_registered_domains(Config, [<<"rzeszow">>, <<"lublin">>]),

    RzeAdmin = jid:make(<<"pawel">>, <<"rzeszow">>, <<"test">>),

    %% add admin user rule
    set_acl(<<"rzeszow">>, admin, {user, <<"pawel">>, <<"rzeszow">>}),
    set_acl(global, admin, {user, <<"pawel">>, <<"lublin">>}),

    %% allow only admin
    set_global_rule(only_admin, [{allow, admin}, {deny, all}]),
    %% deny all
    set_host_rule(only_admin, <<"rzeszow">>, [{deny, admin}, {deny, all}]),

    set_global_rule(ban_admin, [{allow, all}]),
    set_host_rule(ban_admin, <<"rzeszow">>, [{deny, admin}, {allow, all}]),

    %% host rule is more important than the host one
    ?assertEqual(allow, acl:match_rule(<<"rzeszow">>, only_admin, RzeAdmin)),

    %% host rule applies when global doesn't match and ends up with {allow, all}
    %% ...
    ?assertEqual(deny, acl:match_rule(<<"rzeszow">>, ban_admin, RzeAdmin)),
    ok.

all_and_none_specs(Config) ->
    given_registered_domains(Config, [<<"zakopane">>]),

    User = jid:make(<<"pawel">>, <<"zakopane">>, <<"test">>),
    set_acl(global, a_users, all),
    set_acl(global, n_users, none),

    set_global_rule(all_users, [{allow, a_users}, {deny, all}]),
    set_global_rule(none_users, [{allow, n_users}, {deny, all}]),

    ?assertEqual(allow, acl:match_rule(global, all_users, User)),
    ?assertEqual(deny, acl:match_rule(global, none_users, User)),
    ok.

invalid_spec(Config) ->
    given_registered_domains(Config, [<<"bialystok">>]),

    User = jid:make(<<"pawel">>, <<"bialystok">>, <<"test">>),
    set_acl(global, invalid, {non_existing_spec, "lalala"}),

    set_global_rule(invalid, [{allow, invalid}, {deny, all}]),
    ?assertEqual(deny, acl:match_rule(global, invalid, User)),
    ok.

match_host_specific_rule_for_host_type(Config) ->
    given_registered_domains(Config, [<<"gdansk">>, <<"koszalin">>]),

    UserGd = jid:make(<<"pawel">>, <<"gdansk">>,  <<"res">>),

    set_host_rule(allow_admin, <<"test type">>, [{allow, admin}, {deny, all}]),
    set_acl(<<"test type">>, admin, {user, <<"pawel">>}),

    %% Check for host type for a specific domain
    ?assertEqual(allow, acl:match_rule_for_host_type(<<"test type">>, <<"gdansk">>,
                                                     allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule_for_host_type(<<"test type">>, <<"koszalin">>,
                                                    allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule_for_host_type(<<"empty type">>, <<"gdansk">>,
                                                    allow_admin, UserGd)),

    %% Check for host type for any domain
    ?assertEqual(allow, acl:match_rule_for_host_type(<<"test type">>, global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule_for_host_type(<<"empty type">>, global, allow_admin, UserGd)),

    %% Check globally for a specific domain
    ?assertEqual(deny, acl:match_rule_for_host_type(global, <<"gdansk">>, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule_for_host_type(global, <<"koszalin">>, allow_admin, UserGd)),

    %% Check globally for any domain
    ?assertEqual(deny, acl:match_rule_for_host_type(global, global, allow_admin, UserGd)).

match_global_rule_for_host_type(Config) ->
    given_registered_domains(Config, [<<"gdansk">>, <<"koszalin">>]),

    UserGd = jid:make(<<"pawel">>, <<"gdansk">>,  <<"res">>),

    set_global_rule(allow_admin, [{allow, admin}, {deny, all}]),
    set_acl(global, admin, {user, <<"pawel">>}),

    %% Check for host type for a specific domain
    ?assertEqual(allow, acl:match_rule_for_host_type(<<"test type">>, <<"gdansk">>,
                                                     allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule_for_host_type(<<"test type">>, <<"koszalin">>,
                                                    allow_admin, UserGd)),

    %% Check for host type for any domain
    ?assertEqual(allow, acl:match_rule_for_host_type(<<"test type">>, global, allow_admin, UserGd)),

    %% Check globally for a specific domain
    ?assertEqual(allow, acl:match_rule_for_host_type(global, <<"gdansk">>, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule_for_host_type(global, <<"koszalin">>, allow_admin, UserGd)),

    %% Check globally for any domain
    ?assertEqual(allow, acl:match_rule_for_host_type(global, global, allow_admin, UserGd)).

different_specs_matching_the_same_user(Config) ->
    given_registered_domains(Config, [<<"gdansk">>, <<"koszalin">>]),

    UserGd = jid:make(<<"pawel">>, <<"gdansk">>,  <<"res">>),
    UserKo = jid:make(<<"pawel">>, <<"koszalin">>,<<"res1">>),

    %% invariand we are going to change admin acl only
    set_global_rule(allow_admin, [{allow, admin}, {deny, all}]),

    %% match on pawel
    set_acl(global, admin, {user, <<"pawel">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on pawel@gdansk
    set_acl(global, admin, {user, <<"pawel">>, <<"gdansk">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on gdansk
    set_acl(global, admin, {server, <<"gdansk">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on res
    set_acl(global, admin, {resource, <<"res">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user regex
    set_acl(global, admin, {user_regexp, <<"^paw.*">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user regex
    set_acl(global, admin, {user_regexp, <<"^paw.*">>, <<"gdansk">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server regex
    set_acl(global, admin, {server_regexp, <<"^gda.*">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on resource regex
    set_acl(global, admin, {resource_regexp, <<"^res.*">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match node regex
    set_acl(global, admin, {node_regexp, <<"^pawe.*">>, <<"^gda.*">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user glob
    set_acl(global, admin, {user_glob, <<"paw??">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user glob
    set_acl(global, admin, {user_glob, <<"paw??">>, <<"gdansk">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server glob
    set_acl(global, admin, {server_glob, <<"gda*">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server glob
    set_acl(global, admin, {resource_glob, <<"re*">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on node glob
    set_acl(global, admin, {node_glob, <<"pawe?">>, <<"gd*">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    ok.

set_acl(HostType, ACLName, ACLSpec) ->
    ejabberd_config:add_global_option({acl, ACLName, HostType}, [ACLSpec]).

given_clean_config() ->
    meck:unload(),
    %% skip loading part
    meck:new(ejabberd_config, [no_link, unstick, passthrough]),
    meck:expect(ejabberd_config, load_file, fun(_File) -> ok end),
    ejabberd_config:start(),
    mnesia:clear_table(config),
    mnesia:clear_table(local_config),
    ok.

given_registered_domains(Config, DomainsList) ->
    case proplists:get_value(dynamic_domains, Config, false) of
        true ->
            register_dynamic_domains(DomainsList);
        false ->
            register_static_domains(DomainsList)
    end.

register_static_domains(DomainsList) ->
    ejabberd_config:add_global_option(hosts, DomainsList),
    ejabberd_config:add_global_option(host_types, []),
    mongoose_domain_api:stop(),
    mongoose_domain_api:init().

register_dynamic_domains(DomainsList) ->
    ejabberd_config:add_global_option(hosts, []),
    ejabberd_config:add_global_option(host_types, [<<"test type">>, <<"empty type">>]),
    mongoose_domain_api:stop(),
    mongoose_domain_api:init(),
    [mongoose_domain_core:insert(Domain, <<"test type">>, test) || Domain <- DomainsList].

%% ACLs might be an empty list
set_host_rule(Rule, Host, ACLs) ->
    ejabberd_config:add_global_option({access, Rule, Host}, ACLs),
    ok.

%% ACLs might be an empty list
set_global_rule(Rule, ACLs) ->
    ejabberd_config:add_global_option({access, Rule, global}, ACLs),
    ok.
