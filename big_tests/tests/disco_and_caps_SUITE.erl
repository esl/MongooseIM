-module(disco_and_caps_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-import(domain_helper, [host_type/0, domain/0]).
-import(config_parser_helper, [default_mod_config/1, mod_config/2, mod_config_with_auto_backend/1]).

all() ->
    [{group, disco_with_caps},
     {group, disco_with_caps_and_extra_features},
     {group, disco_with_extra_features}].

groups() ->
    G = [{disco_with_caps, [parallel], basic_test_cases() ++ caps_test_cases()},
         {disco_with_caps_and_extra_features, [parallel],
          basic_test_cases() ++ caps_test_cases() ++ extra_feature_test_cases()},
         {disco_with_extra_features, [parallel], basic_test_cases() ++ extra_feature_test_cases()}],
    ct_helper:repeat_all_until_all_ok(G).

basic_test_cases() ->
    [user_cannot_query_stranger_resources,
     user_cannot_query_stranger_features,
     user_can_query_friend_resources,
     user_can_query_friend_features,
     user_cannot_query_own_resources_with_unknown_node,
     user_cannot_query_friend_resources_with_unknown_node,
     user_can_query_server_features].

caps_test_cases() ->
    [caps_feature_is_advertised,
     user_can_query_server_caps_via_disco].

extra_feature_test_cases() ->
    [user_can_query_extra_domains,
     user_can_query_server_info].

init_per_suite(C) ->
    C.

end_per_suite(C) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(C).

init_per_group(Name, C) ->
    C2 = escalus:init_per_suite(dynamic_modules:save_modules(host_type(), C)),
    dynamic_modules:ensure_modules(host_type(), required_modules(Name)),
    C2.

end_per_group(_Name, C) ->
    dynamic_modules:restore_modules(C).

init_per_testcase(Name, C) ->
    escalus:init_per_testcase(Name, C).

end_per_testcase(Name, C) ->
    escalus:end_per_testcase(Name, C).

caps_feature_is_advertised(Config) ->
    Spec = escalus_users:get_userspec(Config, alice),
    {ok, Connection, Features} = escalus_connection:start(Spec, [start_stream, stream_features]),
    true = is_map(proplists:get_value(caps, Features)),
    escalus_connection:stop(Connection).

user_can_query_server_caps_via_disco(Config) ->
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    Spec = escalus_users:get_userspec(NewConfig, alice),
    {ok, Alice, Features} = escalus_connection:start(Spec),
    #{<<"node">> := Node,
      <<"ver">> := Ver} = proplists:get_value(caps, Features),
    NodeVer = <<Node/binary, $#, Ver/binary>>,
    Server = proplists:get_value(server, Spec),
    Disco = escalus_stanza:disco_info(Server, NodeVer),
    escalus:send(Alice, Disco),
    DiscoResp = escalus:wait_for_stanza(Alice),
    escalus:assert(is_iq_result, [Disco], DiscoResp),
    Identity  = exml_query:path(DiscoResp, [{element, <<"query">>},
                                            {element, <<"identity">>},
                                            {attr, <<"name">>}]),
    <<"MongooseIM">> = Identity,
    escalus_connection:stop(Alice).

user_cannot_query_stranger_resources(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        Request = escalus_stanza:disco_items(BobJid),
        escalus:send(Alice, Request),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Request], Stanza),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Stanza),
        escalus:assert(is_stanza_from, [BobJid], Stanza)
    end).

user_cannot_query_stranger_features(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        Request = escalus_stanza:disco_info(BobJid),
        escalus:send(Alice, Request),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Request], Stanza),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Stanza),
        escalus:assert(is_stanza_from, [BobJid], Stanza)
    end).

user_can_query_friend_resources(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus_story:make_all_clients_friends([Alice, Bob]),
        BobJid = escalus_client:short_jid(Bob),
        escalus:send(Alice, escalus_stanza:disco_items(BobJid)),
        Stanza = escalus:wait_for_stanza(Alice),
        Query = exml_query:subelement(Stanza, <<"query">>),
        BobFullJid = escalus_client:full_jid(Bob),
        BobName = escalus_client:username(Bob),
        Item = exml_query:subelement_with_attr(Query, <<"jid">>, BobFullJid),
        ?assertEqual(BobName, exml_query:attr(Item, <<"name">>)),
        escalus:assert(is_stanza_from, [BobJid], Stanza)
    end).

user_can_query_friend_features(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus_story:make_all_clients_friends([Alice, Bob]),
        BobJid = escalus_client:short_jid(Bob),
        escalus:send(Alice, escalus_stanza:disco_info(BobJid)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_identity, [<<"account">>, <<"registered">>], Stanza),
        escalus:assert(is_stanza_from, [BobJid], Stanza)
    end).

user_cannot_query_own_resources_with_unknown_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        Request = escalus_stanza:disco_items(AliceJid, <<"unknown-node">>),
        escalus:send(Alice, Request),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Request], Stanza),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Stanza),
        escalus:assert(is_stanza_from, [AliceJid], Stanza)
    end).

user_cannot_query_friend_resources_with_unknown_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus_story:make_all_clients_friends([Alice, Bob]),
        BobJid = escalus_client:short_jid(Bob),
        Request = escalus_stanza:disco_items(BobJid, <<"unknown-node">>),
        escalus:send(Alice, Request),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Request], Stanza),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Stanza),
        escalus:assert(is_stanza_from, [BobJid], Stanza)
    end).

user_can_query_extra_domains(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [extra_domain()], Stanza),
        escalus:assert(is_stanza_from, [domain()], Stanza)
    end).

user_can_query_server_features(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:disco_info(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_identity, [<<"server">>, <<"im">>], Stanza),
        escalus:assert(has_feature, [<<"iq">>], Stanza),
        escalus:assert(has_feature, [<<"presence">>], Stanza),
        escalus:assert(is_stanza_from, [domain()], Stanza)
    end).

%% XEP-0157: Contact Addresses for XMPP Services
user_can_query_server_info(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:disco_info(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_stanza_from, [domain()], Stanza),

        %% 'sales' is hidden for mod_disco, so only 'abuse' and 'admin' are expected
        [HiddenField, AbuseField, AdminField] = get_form_fields(Stanza),
        ?assertEqual(<<"FORM_TYPE">>, exml_query:attr(HiddenField, <<"var">>)),
        ?assertEqual(<<"hidden">>, exml_query:attr(HiddenField, <<"type">>)),
        ?assertEqual([?NS_SERVERINFO], exml_query:paths(HiddenField, [{element, <<"value">>},
                                                                      cdata])),
        ?assertEqual(name(abuse), exml_query:attr(AbuseField, <<"var">>)),
        ?assertEqual(urls(abuse), exml_query:paths(AbuseField, [{element, <<"value">>},
                                                                              cdata])),
        ?assertEqual(name(admin), exml_query:attr(AdminField, <<"var">>)),
        ?assertEqual(urls(admin), exml_query:paths(AdminField, [{element, <<"value">>},
                                                                cdata]))
    end).

%% Helpers

required_modules(disco_with_caps) ->
    [{mod_caps, mod_config_with_auto_backend(mod_caps)},
     {mod_disco, default_mod_config(mod_disco)}];
required_modules(disco_with_caps_and_extra_features) ->
    [{mod_caps, mod_config_with_auto_backend(mod_caps)},
     {mod_disco, mod_config(mod_disco, extra_disco_opts())}];
required_modules(disco_with_extra_features) ->
    [{mod_disco, mod_config(mod_disco, extra_disco_opts())}].

extra_disco_opts() ->
    #{extra_domains => [extra_domain()],
      server_info => [server_info(abuse, #{}),
                      server_info(admin, #{modules => [mod_disco]}),
                      server_info(sales, #{modules => [mod_pubsub]})]}.

get_form_fields(Stanza) ->
     exml_query:paths(Stanza, [{element_with_ns, <<"query">>, ?NS_DISCO_INFO},
                               {element_with_ns, <<"x">>, ?NS_DATA_FORMS},
                               {element, <<"field">>}]).

extra_domain() ->
    <<"eXtra.example.com">>.

server_info(Type, Extra) ->
    maps:merge(#{name => name(Type), urls => urls(Type)}, Extra).

name(abuse) -> <<"abuse-addresses">>;
name(admin) -> <<"admin-addresses">>;
name(sales) -> <<"sales-addresses">>.

urls(abuse) -> [<<"abuse@example.com">>];
urls(admin) -> [<<"admin@example.com">>, <<"operations@example.com">>];
urls(sales) -> [<<"sales@example.com">>].
