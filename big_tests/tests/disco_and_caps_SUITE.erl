-module(disco_and_caps_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, all_tests}].

groups() ->
    G = [{all_tests, [parallel], all_test_cases()}],
    ct_helper:repeat_all_until_all_ok(G).

all_test_cases() ->
    [caps_feature_is_advertised,
     user_can_query_server_caps_via_disco,
     user_cannot_query_stranger_resources,
     user_cannot_query_stranger_features,
     user_can_query_friend_resources,
     user_can_query_friend_features,
     user_cannot_query_own_resources_with_unknown_node,
     user_cannot_query_friend_resources_with_unknown_node,
     user_can_query_extra_domains,
     user_can_query_server_features].

domain() ->
    ct:get_config({hosts, mim, domain}).

init_per_suite(C) ->
    C2 = escalus:init_per_suite(dynamic_modules:save_modules(domain(), C)),
    dynamic_modules:ensure_modules(domain(), [{mod_caps, []},
                                              {mod_disco, [{extra_domains, [extra_domain()]}]}]),
    C2.

end_per_suite(C) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(domain(), C),
    escalus:end_per_suite(C).

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
        escalus:assert(has_feature, [<<"presence-invisible">>], Stanza),
        escalus:assert(is_stanza_from, [domain()], Stanza)
    end).

extra_domain() ->
    <<"eXtra.example.com">>.
