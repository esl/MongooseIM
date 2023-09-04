-module(bind2_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-define(NS_SASL_2, <<"urn:xmpp:sasl:2">>).
-define(NS_BIND_2, <<"urn:xmpp:bind:0">>).
-define(BAD_TAG, <<"\x{EFBB}"/utf8>>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, basic}
    ].

groups() ->
    [
     {basic, [parallel],
      [
       server_announces_bind2,
       server_announces_bind2_with_sm,
       auth_and_bind_ignores_invalid_resource_and_generates_a_new_one,
       auth_and_bind_to_random_resource,
       auth_and_bind_do_not_expose_user_agent_id_in_client,
       auth_and_bind_contains_client_tag,
       stream_resumption_failing_does_bind_and_contains_sm_status,
       stream_resumption_overrides_bind_request
      ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = load_modules(Config),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

load_modules(Config) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    sasl2_helper:load_all_sasl2_modules(HostType),
    Config1.

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------

server_announces_bind2(Config) ->
    Steps = [create_connect_tls, start_stream_get_features],
    #{features := Features} = sasl2_helper:apply_steps(Steps, Config),
    Bind2 = exml_query:path(Features, [{element_with_ns, <<"authentication">>, ?NS_SASL_2},
                                       {element, <<"inline">>},
                                       {element_with_ns, <<"bind">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bind2).

server_announces_bind2_with_sm(Config) ->
    Steps = [create_connect_tls, start_stream_get_features],
    #{features := Features} = sasl2_helper:apply_steps(Steps, Config),
    SM = exml_query:path(Features, [{element_with_ns, <<"authentication">>, ?NS_SASL_2},
                                    {element, <<"inline">>},
                                    {element_with_ns, <<"bind">>, ?NS_BIND_2},
                                    {element, <<"inline">>},
                                    {element_with_attr, <<"var">>, ?NS_STREAM_MGNT_3}]),
    ?assertNotEqual(undefined, SM).

auth_and_bind_ignores_invalid_resource_and_generates_a_new_one(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind_wrong_resource}, has_no_more_stanzas],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    Success = exml_query:path(Response, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Success).

auth_and_bind_to_random_resource(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind}, has_no_more_stanzas],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{resource = LResource} = jid:from_binary(Identifier),
    ?assert(0 =< byte_size(LResource), LResource).

auth_and_bind_do_not_expose_user_agent_id_in_client(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind_with_user_agent_uuid}, has_no_more_stanzas],
    #{answer := Success, uuid := Uuid} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{resource = LResource} = jid:from_binary(Identifier),
    ?assertNotEqual(Uuid, LResource).

auth_and_bind_contains_client_tag(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind_with_tag}, has_no_more_stanzas],
    #{answer := Success, tag := Tag} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{resource = LResource} = jid:from_binary(Identifier),
    ResourceParts = binary:split(LResource, <<"/">>, [global]),
    ?assertMatch([Tag, _], ResourceParts).

stream_resumption_failing_does_bind_and_contains_sm_status(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             {?MODULE, auth_and_bind_with_resumption_unknown_smid}, has_no_more_stanzas],
    #{answer := Success, tag := Tag} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"failed">>, ?NS_STREAM_MGNT_3}]),
    escalus:assert(is_sm_failed, [<<"item-not-found">>], Resumed),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{resource = LResource} = jid:from_binary(Identifier),
    ResourceParts = binary:split(LResource, <<"/">>, [global]),
    ?assertMatch([Tag, _], ResourceParts).

stream_resumption_overrides_bind_request(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             {?MODULE, auth_and_bind_with_resumption}, has_no_more_stanzas],
    #{answer := Success, smid := SMID} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"resumed">>, ?NS_STREAM_MGNT_3}]),
    ?assert(escalus_pred:is_sm_resumed(SMID, Resumed)).


%% Step helpers
auth_and_bind(Config, Client, Data) ->
    plain_auth(Config, Client, Data, [], []).

auth_and_bind_wrong_resource(Config, Client, Data) ->
    Tag = ?BAD_TAG,
    plain_auth(Config, Client, Data#{tag => Tag}, [bind_tag(Tag)], []).

auth_and_bind_with_user_agent_uuid(Config, Client, Data) ->
    Uuid = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    plain_auth(Config, Client, Data#{uuid => Uuid}, [], [good_user_agent_elem(Uuid)]).

auth_and_bind_with_tag(Config, Client, Data) ->
    Tag = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    plain_auth(Config, Client, Data#{tag => Tag}, [bind_tag(Tag)], []).

auth_and_bind_with_resumption_unknown_smid(Config, Client, Data) ->
    Tag = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Resume = escalus_stanza:resume(<<"123456">>, 1),
    plain_auth(Config, Client, Data#{tag => Tag}, [bind_tag(Tag)], [Resume]).

auth_and_bind_with_resumption(Config, Client, #{smid := SMID, texts := Texts} = Data) ->
    Tag = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Resume = escalus_stanza:resume(SMID, 1),
    {Client1, Data1} = plain_auth(Config, Client, Data#{tag => Tag}, [bind_tag(Tag)], [Resume]),
    Msgs = sm_helper:wait_for_messages(Client, Texts),
    {Client1, Data1#{sm_storage => Msgs}}.

plain_auth(_Config, Client, Data, BindElems, Extra) ->
    InitEl = sasl2_helper:plain_auth_initial_response(Client),
    BindEl = #xmlel{name = <<"bind">>,
                  attrs = [{<<"xmlns">>, ?NS_BIND_2}],
                  children = BindElems},
    Authenticate = auth_elem(<<"PLAIN">>, [InitEl, BindEl | Extra]),
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

%% XML helpers
auth_elem(Mech, Children) ->
    #xmlel{name = <<"authenticate">>,
           attrs = [{<<"xmlns">>, ?NS_SASL_2}, {<<"mechanism">>, Mech}],
           children = Children}.

bind_tag(Tag) ->
    #xmlel{name = <<"tag">>, children = [#xmlcdata{content = Tag}]}.

good_user_agent_elem(Uuid) ->
    user_agent_elem(Uuid, <<"cool-xmpp-client">>, <<"latest-and-greatest-device">>).

user_agent_elem(Id, Software, Device) ->
    SoftEl = [#xmlel{name = <<"software">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Software], Value =/= undefined ],
    DeviEl = [#xmlel{name = <<"device">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Device], Value =/= undefined ],
    Attrs = [ {<<"id">>, Value} || Value <- [Id], Value =/= undefined ],
    #xmlel{name = <<"user-agent">>, attrs = Attrs, children = SoftEl ++ DeviEl}.
