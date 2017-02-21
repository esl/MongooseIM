-module(disco_and_caps_SUITE).
-compile(export_all).

all() ->
    [{group, all_tests}].

groups() ->
    [{all_tests, [parallel], all_test_cases()}].

all_test_cases() ->
    [caps_feature_is_advertised,
     user_can_query_server_caps_via_disco].

domain() ->
    ct:get_config({hosts, mim, domain}).

init_per_suite(C) ->
    C2 = escalus:init_per_suite(dynamic_modules:save_modules(domain(), C)),
    dynamic_modules:ensure_modules(domain(), [{mod_caps, []}]),
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
    {ok, Connection, _, Features} = escalus_connection:start(Spec, [start_stream, stream_features]),
    true = is_map(proplists:get_value(caps, Features)),
    escalus_connection:stop(Connection).

user_can_query_server_caps_via_disco(Config) ->
    NewConfig = escalus_fresh:create_users(Config, [alice]),
    Spec = escalus_users:get_userspec(NewConfig, alice),
    {ok, Alice, _, Features} = escalus_connection:start(Spec),
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
