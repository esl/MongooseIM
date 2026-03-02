-module(disco_and_caps_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-import(domain_helper, [host_type/0, host_type/1, domain/0]).
-import(config_parser_helper, [default_mod_config/1, mod_config/2, mod_config_with_auto_backend/1]).
-import(distributed_helper, [fed/0]).

suite() ->
    distributed_helper:require_rpc_nodes([mim, fed]) ++ escalus:suite().

all() ->
    [{group, disco_with_caps},
     {group, disco_with_caps_and_extra_features},
     {group, disco_with_extra_features},
     {group, client_caps},
     {group, client_caps_timeout}].

groups() ->
    [{disco_with_caps, [parallel], basic_test_cases() ++ server_caps_test_cases()},
     {disco_with_caps_and_extra_features, [parallel],
      basic_test_cases() ++ server_caps_test_cases() ++ extra_feature_test_cases()},
     {disco_with_extra_features, [parallel], basic_test_cases() ++ extra_feature_test_cases()},
     {client_caps, [parallel], client_caps_test_cases()},
     {client_caps_timeout, [parallel], client_caps_timeout_test_cases()}].

basic_test_cases() ->
    [user_cannot_query_stranger_resources,
     user_cannot_query_stranger_features,
     user_can_query_friend_resources,
     user_can_query_friend_features,
     user_cannot_query_own_resources_with_unknown_node,
     user_cannot_query_friend_resources_with_unknown_node,
     user_can_query_server_features].

server_caps_test_cases() ->
    [caps_feature_is_advertised,
     user_can_query_server_caps_via_disco].

extra_feature_test_cases() ->
    [user_can_query_extra_domains,
     user_can_query_server_info].

client_caps_test_cases() ->
    [client_caps_are_requested_and_cached,
     client_caps_are_requested_with_error,
     client_caps_are_deleted_but_cached_between_sessions,
     client_caps_are_deleted_on_presence_unavailable,
     client_caps_are_deleted_on_presence_without_caps,
     client_caps_are_updated,
     client_caps_can_be_different_per_session,
     client_caps_are_unchanged_by_directed_presence].

client_caps_timeout_test_cases() ->
    [client_caps_are_requested_with_timeout].

init_per_suite(C) ->
    instrument_helper:start(instrument_helper:declared_events(mod_disco)),
    escalus:init_per_suite(C).

end_per_suite(C) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(C),
    s2s_helper:end_s2s(C),
    dynamic_modules:restore_modules(C),
    instrument_helper:stop().

init_per_group(Name, C) ->
    Modules = required_modules(Name),
    HasCaps = proplists:is_defined(mod_caps, Modules),
    maybe
        ok ?= check_caps_backend(HasCaps),
        C2 = dynamic_modules:save_modules(host_type(), C),
        dynamic_modules:ensure_modules(host_type(), Modules),
        [{caps, HasCaps} | C2]
    end.

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
        escalus:assert(is_stanza_from, [BobJid], Stanza),
        assert_roster_get_event(Alice)
    end).

user_cannot_query_stranger_features(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        Request = escalus_stanza:disco_info(BobJid),
        escalus:send(Alice, Request),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Request], Stanza),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Stanza),
        escalus:assert(is_stanza_from, [BobJid], Stanza),
        assert_roster_get_event(Alice)
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
        escalus:assert(is_stanza_from, [BobJid], Stanza),
        assert_roster_get_event(Alice)
    end).

user_can_query_friend_features(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus_story:make_all_clients_friends([Alice, Bob]),
        BobJid = escalus_client:short_jid(Bob),
        escalus:send(Alice, escalus_stanza:disco_info(BobJid)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_identity, [<<"account">>, <<"registered">>], Stanza),
        escalus:assert(is_stanza_from, [BobJid], Stanza),
        assert_roster_get_event(Alice)
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
        case proplists:get_value(caps, Config) of
            true -> escalus:assert(has_feature, [?NS_CAPS], Stanza);
            false -> ok
        end,
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

%% Client caps tests

client_caps_are_requested_and_cached(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature = caps_helper:random_name(),
    caps_helper:enable_new_caps(Alice, [Feature]),
    caps_helper:wait_for_caps(HostType, Alice, [Feature]),
    {ok, Bob} = escalus_client:start(NewConfig, bob, ~"res1"),
    caps_helper:enable_caps(Bob, [Feature]),
    caps_helper:assert_caps(HostType, Alice, [Feature]),

    %% Make sure caps are not requested again
    ct:sleep(100),
    escalus_assert:has_no_stanzas(Alice),
    escalus_client:stop(NewConfig, Alice),
    escalus_client:stop(NewConfig, Bob).

client_caps_are_requested_with_error(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature = caps_helper:random_name(),
    Caps = caps_helper:caps([Feature]),
    caps_helper:send_presence_with_caps(Alice, Caps),
    Request = caps_helper:receive_caps_request(Alice, Caps),
    caps_helper:receive_presence_with_caps(Alice, Alice, Caps),
    ErrorEl = escalus_stanza:error_element(~"cancel", ~"item-not-found"),
    Error = escalus_stanza:setattr(escalus_stanza:iq_result(Request, [ErrorEl]), ~"type", ~"error"),
    escalus:send(Alice, Error),

    %% Make sure no caps are stored
    ct:sleep(100),
    caps_helper:assert_no_caps(HostType, Alice),
    escalus_client:stop(NewConfig, Alice).

client_caps_are_requested_with_timeout(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature = caps_helper:random_name(),
    Caps = caps_helper:caps([Feature]),
    caps_helper:send_presence_with_caps(Alice, Caps),
    Request = caps_helper:receive_caps_request(Alice, Caps),
    caps_helper:receive_presence_with_caps(Alice, Alice, Caps),

    %% Make sure the server timeout of 100 ms passes
    ct:sleep(200),
    caps_helper:send_caps_disco_result(Alice, Request, [Feature]),

    %% Make sure no caps are stored after the response
    ct:sleep(100),
    caps_helper:assert_no_caps(HostType, Alice),
    escalus_client:stop(NewConfig, Alice).

client_caps_are_deleted_but_cached_between_sessions(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature = caps_helper:random_name(),
    caps_helper:enable_new_caps(Alice, [Feature]),
    caps_helper:wait_for_caps(HostType, Alice, [Feature]),
    escalus_client:stop(NewConfig, Alice),
    caps_helper:wait_for_no_caps(HostType, Alice),

    %% Client caps were deleted with the session, but the hash is still known
    {ok, Alice1} = escalus_client:start(NewConfig, alice, ~"res1"),
    caps_helper:enable_caps(Alice1, [Feature]),
    caps_helper:assert_caps(HostType, Alice1, [Feature]),

    %% Make sure caps are not requested again
    ct:sleep(100),
    escalus_assert:has_no_stanzas(Alice),
    escalus_client:stop(NewConfig, Alice1).

client_caps_are_deleted_on_presence_unavailable(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature = caps_helper:random_name(),
    caps_helper:enable_new_caps(Alice, [Feature]),
    caps_helper:wait_for_caps(HostType, Alice, [Feature]),
    escalus:send(Alice, escalus_stanza:presence(~"unavailable")),
    caps_helper:wait_for_no_caps(HostType, Alice),
    escalus_client:stop(NewConfig, Alice).

client_caps_are_deleted_on_presence_without_caps(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature = caps_helper:random_name(),
    caps_helper:enable_new_caps(Alice, [Feature]),
    caps_helper:wait_for_caps(HostType, Alice, [Feature]),
    escalus:send(Alice, escalus_stanza:presence(~"available")),
    escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),
    caps_helper:assert_no_caps(HostType, Alice),
    escalus_client:stop(NewConfig, Alice).

client_caps_are_updated(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature1 = caps_helper:random_name(),
    caps_helper:enable_new_caps(Alice, [Feature1]),
    caps_helper:wait_for_caps(HostType, Alice, [Feature1]),
    Feature2 = caps_helper:random_name(),
    caps_helper:enable_new_caps(Alice, [Feature2]),
    caps_helper:wait_for_caps(HostType, Alice, [Feature2]),
    escalus_client:stop(NewConfig, Alice),
    caps_helper:wait_for_no_caps(HostType, Alice).

client_caps_can_be_different_per_session(Config) ->
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    {ok, Alice1} = escalus_client:start(NewConfig, alice, ~"res1"),
    {ok, Alice2} = escalus_client:start(NewConfig, alice, ~"res2"),
    HostType = host_type(),
    Feature1 = caps_helper:random_name(),
    Caps1 = caps_helper:enable_new_caps(Alice1, [Feature1]),
    caps_helper:wait_for_caps(HostType, Alice1, [Feature1]),
    Feature2 = caps_helper:random_name(),
    Caps2 = caps_helper:enable_new_caps(Alice2, [Feature2]),
    caps_helper:wait_for_caps(HostType, Alice2, [Feature2]),
    caps_helper:receive_presence_with_caps(Alice2, Alice1, Caps1),
    caps_helper:receive_presence_with_caps(Alice1, Alice2, Caps2),
    escalus_client:stop(NewConfig, Alice1),
    caps_helper:wait_for_no_caps(HostType, Alice1),
    escalus_client:stop(NewConfig, Alice2),
    caps_helper:wait_for_no_caps(HostType, Alice2).

client_caps_are_unchanged_by_directed_presence(Config) ->
    HostType = host_type(),
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    {ok, Alice} = escalus_client:start(NewConfig, alice, ~"res1"),
    Feature = caps_helper:random_name(),
    caps_helper:enable_new_caps(Alice, [Feature]),
    caps_helper:wait_for_caps(HostType, Alice, [Feature]),
    {ok, Bob} = escalus_client:start(NewConfig, bob, ~"res1"),
    escalus:send(Alice, escalus_stanza:to(escalus_stanza:presence(~"available"), Bob)),
    caps_helper:assert_caps(HostType, Alice, [Feature]),
    escalus_client:stop(NewConfig, Alice),
    escalus_client:stop(NewConfig, Bob).

%% Helpers

required_modules(disco_with_caps) ->
    [{mod_caps, default_mod_config(mod_caps)},
     {mod_disco, default_mod_config(mod_disco)}];
required_modules(disco_with_caps_and_extra_features) ->
    [{mod_caps, default_mod_config(mod_caps)},
     {mod_disco, mod_config(mod_disco, extra_disco_opts())}];
required_modules(disco_with_extra_features) ->
    [{mod_disco, mod_config(mod_disco, extra_disco_opts())}];
required_modules(client_caps) ->
    [{mod_caps, default_mod_config(mod_caps)}];
required_modules(client_caps_timeout) ->
    [{mod_caps, mod_config(mod_caps, #{iq_response_timeout => 100})}].

check_caps_backend(true) ->
    caps_helper:check_backend();
check_caps_backend(false) ->
    ok.

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

assert_roster_get_event(Client) ->
    ClientJid = jid:from_binary(escalus_client:full_jid(Client)),
    instrument_helper:assert_one(mod_disco_roster_get, #{host_type => host_type()},
                                 fun(#{count := 1, jid := Jid}) -> ClientJid =:= Jid end).
