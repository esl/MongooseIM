-module(acl_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [
     all_rule_returns_allow,
     none_rule_returns_deny,
     basic_access_rules,
     compound_access_rules,
     host_sepcific_access_rules,
     global_host_priority,
     all_and_none_specs,
     invalid_spec,
     different_specs_matching_the_same_user
    ].

init_per_suite(Config) ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = stringprep:start(),
    ok = acl:start(),
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    meck:unload(),
    ok.

init_per_testcase(_TC, Config) ->
    mnesia:clear_table(acl),
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

host_sepcific_access_rules(_Config) ->
    given_registered_domains([<<"poznan">>, <<"wroclaw">>]),

    PozAdmin = jid:make(<<"gawel">>, <<"poznan">>, <<"test">>),
    Pawel = jid:make(<<"pawel">>, <<"wroclaw">>, <<"test">>),

    acl:add(global, admin_poz, {user, <<"gawel">>, <<"poznan">>}),

    set_host_rule(only_poz_admin, <<"poznan">>, [{allow, admin_poz}, {deny, all}]),
    set_host_rule(only_poz_admin, <<"wroclaw">>, [{deny, admin_poz}, {allow, all}]),

    ?assertEqual(allow, acl:match_rule(<<"poznan">>, only_poz_admin, PozAdmin)),
    ?assertEqual(deny, acl:match_rule(<<"poznan">>, only_poz_admin, Pawel)),

    ?assertEqual(deny, acl:match_rule(<<"wroclaw">>, only_poz_admin, PozAdmin)),
    ?assertEqual(allow, acl:match_rule(<<"wroclaw">>, only_poz_admin, Pawel)),
    ok.

compound_access_rules(_Config) ->
    given_registered_domains([<<"krakow">>]),

    KrkAdmin = jid:make(<<"gawel">>, <<"krakow">>, <<"test">>),
    KrkNormal = jid:make(<<"pawel">>, <<"krakow">>, <<"test">>),

    %% add admin user rule
    acl:add(global, admin_wawa, {user, <<"gawel">>, <<"wawa">>}),
    acl:add(global, admin_krakow, {user, <<"gawel">>, <<"krakow">>}),

    set_global_rule(only_krakow_admin, [{allow, admin_krakow}, {deny, all}]),
    set_global_rule(only_wawa_admin, [{allow, admin_wawa}, {deny, all}]),

    ?assertEqual(deny, acl:match_rule(global, only_krakow_admin, KrkNormal)),
    ?assertEqual(deny, acl:match_rule(global, only_wawa_admin, KrkNormal)),

    ?assertEqual(allow, acl:match_rule(global, only_krakow_admin, KrkAdmin)),
    ?assertEqual(deny,  acl:match_rule(global, only_wawa_admin, KrkAdmin)),
    ok.

global_host_priority(_Config) ->
    given_registered_domains([<<"rzeszow">>, <<"lublin">>]),

    RzeAdmin = jid:make(<<"pawel">>, <<"rzeszow">>, <<"test">>),

    %% add admin user rule
    acl:add(<<"rzeszow">>, admin, {user, <<"pawel">>, <<"rzeszow">>}),
    acl:add(global, admin, {user, <<"pawel">>, <<"lublin">>}),

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

all_and_none_specs(_Config) ->
    given_registered_domains([<<"zakopane">>]),

    User = jid:make(<<"pawel">>, <<"zakopane">>, <<"test">>),
    acl:add(global, a_users, all),
    acl:add(global, n_users, none),

    set_global_rule(all_users, [{allow, a_users}, {deny, all}]),
    set_global_rule(none_users, [{allow, n_users}, {deny, all}]),

    ?assertEqual(allow, acl:match_rule(global, all_users, User)),
    ?assertEqual(deny, acl:match_rule(global, none_users, User)),
    ok.

invalid_spec(_Config) ->
    given_registered_domains([<<"bialystok">>]),

    User = jid:make(<<"pawel">>, <<"bialystok">>, <<"test">>),
    acl:add(global, invalid, {non_existing_spec, "lalala"}),

    set_global_rule(invalid, [{allow, invalid}, {deny, all}]),
    ?assertEqual(deny, acl:match_rule(global, invalid, User)),
    ok.

different_specs_matching_the_same_user(_Config) ->
    given_registered_domains([<<"gdansk">>, <<"koszalin">>]),

    UserGd = jid:make(<<"pawel">>, <<"gdansk">>,  <<"res">>),
    UserKo = jid:make(<<"pawel">>, <<"koszalin">>,<<"res1">>),

    %% invariand we are going to change admin acl only
    set_global_rule(allow_admin, [{allow, admin}, {deny, all}]),

    %% match on pawel
    mnesia:clear_table(acl),
    acl:add(global, admin, {user, <<"pawel">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on pawel@gdansk
    mnesia:clear_table(acl),
    acl:add(global, admin, {user, <<"pawel">>, <<"gdansk">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on gdansk
    mnesia:clear_table(acl),
    acl:add(global, admin, {server, <<"gdansk">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on res
    mnesia:clear_table(acl),
    acl:add(global, admin, {resource, <<"res">>}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user regex
    mnesia:clear_table(acl),
    acl:add(global, admin, {user_regexp, "^paw.*"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user regex
    mnesia:clear_table(acl),
    acl:add(global, admin, {user_regexp, "^paw.*", "gdansk"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server regex
    mnesia:clear_table(acl),
    acl:add(global, admin, {server_regexp, "^gda.*"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on resource regex
    mnesia:clear_table(acl),
    acl:add(global, admin, {resource_regexp, "^res.*"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match node regex
    mnesia:clear_table(acl),
    acl:add(global, admin, {node_regexp, "^pawe.*", "^gda.*"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user glob
    mnesia:clear_table(acl),
    acl:add(global, admin, {user_glob, "paw??"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on user glob
    mnesia:clear_table(acl),
    acl:add(global, admin, {user_glob, "paw??", "gdansk"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server glob
    mnesia:clear_table(acl),
    acl:add(global, admin, {server_glob, "gda*"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    %% match on server glob
    mnesia:clear_table(acl),
    acl:add(global, admin, {resource_glob, "re*"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserKo)),

    %% match on node glob
    mnesia:clear_table(acl),
    acl:add(global, admin, {node_glob, "pawe?", "gd*"}),
    ?assertEqual(allow, acl:match_rule(global, allow_admin, UserGd)),
    ?assertEqual(deny, acl:match_rule(global, allow_admin, UserKo)),

    ok.

given_clean_config() ->
    meck:unload(),
    %% skip loading part
    meck:new(ejabberd_config, [no_link, unstick, passthrough]),
    meck:expect(ejabberd_config, load_file, fun(_File) -> ok end),
    ejabberd_config:start(),
    mnesia:clear_table(config),
    mnesia:clear_table(local_config),
    ok.

given_registered_domains(DomainsList) ->
    ejabberd_config:add_global_option(hosts, DomainsList).

%% ACLs might be an empty list
set_host_rule(Rule, Host, ACLs) ->
    ejabberd_config:add_global_option({access, Rule, Host}, ACLs),
    ok.

%% ACLs might be an empty list
set_global_rule(Rule, ACLs) ->
    ejabberd_config:add_global_option({access, Rule, global}, ACLs),
    ok.
