-module(bind2_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-define(NS_CSI, <<"urn:xmpp:csi:0">>).
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
       server_announces_bind2_with_features,
       auth_and_bind_ignores_invalid_resource_and_generates_a_new_one,
       auth_and_bind_to_random_resource,
       auth_and_bind_do_not_expose_user_agent_id_in_client,
       auth_and_bind_contains_client_tag,
       carbons_are_enabled_with_bind_inline_request,
       csi_is_active_with_bind_inline_request,
       csi_is_inactive_with_bind_inline_request,
       stream_resumption_enable_sm_on_bind,
       stream_resumption_enable_sm_on_bind_with_resume,
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

server_announces_bind2_with_features(Config) ->
    Steps = [create_connect_tls, start_stream_get_features],
    #{features := Features} = sasl2_helper:apply_steps(Steps, Config),
    Bind2 = exml_query:path(Features, [{element_with_ns, <<"authentication">>, ?NS_SASL_2},
                                       {element, <<"inline">>},
                                       {element_with_ns, <<"bind">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bind2),
    InlineFeatures = exml_query:path(Bind2, [{element, <<"inline">>}]),
    check_var(InlineFeatures, ?NS_STREAM_MGNT_3),
    check_var(InlineFeatures, ?NS_CARBONS_2),
    check_var(InlineFeatures, ?NS_CSI).

auth_and_bind_ignores_invalid_resource_and_generates_a_new_one(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind_wrong_resource}, receive_features, has_no_more_stanzas],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    Success = exml_query:path(Response, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Success).

auth_and_bind_to_random_resource(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind}, receive_features, has_no_more_stanzas],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{lresource = LResource} = jid:from_binary(Identifier),
    ?assert(0 =< byte_size(LResource), LResource).

auth_and_bind_do_not_expose_user_agent_id_in_client(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind_with_user_agent_uuid}, receive_features, has_no_more_stanzas],
    #{answer := Success, uuid := Uuid} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{lresource = LResource} = jid:from_binary(Identifier),
    ?assertNotEqual(Uuid, LResource).

auth_and_bind_contains_client_tag(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_bind_with_tag}, receive_features, has_no_more_stanzas],
    #{answer := Success, tag := Tag} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{lresource = LResource} = jid:from_binary(Identifier),
    ResourceParts = binary:split(LResource, <<"/">>, [global]),
    ?assertMatch([Tag, _], ResourceParts).

carbons_are_enabled_with_bind_inline_request(Config) ->
    Steps = [start_new_user,
             {?MODULE, start_peer},
             {?MODULE, auth_and_bind_with_carbon_copies}, receive_features,
             {?MODULE, receive_message_carbon_arrives}, has_no_more_stanzas],
    sasl2_helper:apply_steps(Steps, Config).

csi_is_active_with_bind_inline_request(Config) ->
    Steps = [start_new_user,
             {?MODULE, start_peer},
             {?MODULE, auth_and_bind_with_csi_active}, receive_features,
             {?MODULE, inactive_csi_msg_wont_arrive}, has_no_more_stanzas],
    sasl2_helper:apply_steps(Steps, Config).

csi_is_inactive_with_bind_inline_request(Config) ->
    Steps = [start_new_user,
             {?MODULE, start_peer},
             {?MODULE, auth_and_bind_with_csi_inactive}, has_no_more_stanzas,
             {?MODULE, inactive_csi_msgs_do_not_arrive},
             {?MODULE, activate_csi}, receive_features,
             {?MODULE, receive_csi_msgs}, has_no_more_stanzas],
    sasl2_helper:apply_steps(Steps, Config).

stream_resumption_enable_sm_on_bind(Config) ->
    Steps = [start_new_user,
             {?MODULE, start_peer},
             {?MODULE, auth_and_bind_with_sm_enabled},
             receive_features, has_no_more_stanzas],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
    Enabled = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2},
                                        {element_with_ns, <<"enabled">>, ?NS_STREAM_MGNT_3}]),
    ?assertNotEqual(undefined, Enabled).

stream_resumption_enable_sm_on_bind_with_resume(Config) ->
    Steps = [start_new_user,
             {?MODULE, start_peer},
             {?MODULE, auth_and_bind_with_sm_resume_enabled},
             receive_features, has_no_more_stanzas],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
    Enabled = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2},
                                        {element_with_ns, <<"enabled">>, ?NS_STREAM_MGNT_3}]),
    ?assertNotEqual(undefined, Enabled).

stream_resumption_failing_does_bind_and_contains_sm_status(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             {?MODULE, auth_and_bind_with_resumption_unknown_smid},
             receive_features, has_no_more_stanzas],
    #{answer := Success, tag := Tag} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
    Bound = exml_query:path(Success, [{element_with_ns, <<"bound">>, ?NS_BIND_2}]),
    ?assertNotEqual(undefined, Bound),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"failed">>, ?NS_STREAM_MGNT_3}]),
    escalus:assert(is_sm_failed, [<<"item-not-found">>], Resumed),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{lresource = LResource} = jid:from_binary(Identifier),
    ResourceParts = binary:split(LResource, <<"/">>, [global]),
    ?assertMatch([Tag, _], ResourceParts).

stream_resumption_overrides_bind_request(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             {?MODULE, auth_and_bind_with_resumption}, has_no_more_stanzas],
    #{answer := Success, smid := SMID} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
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

auth_and_bind_with_carbon_copies(Config, Client, #{spec := Spec} = Data) ->
    CarbonEnable = enable_carbons_el(),
    {Client1, Data1} = plain_auth(Config, Client, Data, [CarbonEnable], []),
    Resource = <<"second_resource">>,
    {ok, Client2, _} = escalus_connection:start(
                         [{carbons, true}, {resource, Resource} | Spec]),
    Jid = <<(escalus_client:short_jid(Client2))/binary, "/", Resource/binary>>,
    {Client1, Data1#{client_2 => Client2, client_2_jid => Jid}}.

auth_and_bind_with_csi_active(Config, Client, Data) ->
    CsiActive = csi_helper:csi_stanza(<<"active">>),
    plain_auth(Config, Client, Data, [CsiActive], []).

inactive_csi_msg_wont_arrive(_Config, Client, #{peer := Bob} = Data) ->
    escalus_client:send(Bob, escalus_stanza:chat_to(Client, <<"hello 1">>)),
    AliceReceived = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_message, AliceReceived),
    csi_helper:given_client_is_inactive_and_no_messages_arrive(Client),
    csi_helper:given_messages_are_sent(Client, Bob, 1),
    csi_helper:then_client_does_not_receive_any_message(Client),
    {Client, Data}.

auth_and_bind_with_csi_inactive(Config, Client, Data) ->
    CsiInactive = csi_helper:csi_stanza(<<"inactive">>),
    plain_auth(Config, Client, Data, [CsiInactive], []).

inactive_csi_msgs_do_not_arrive(_Config, Client, #{peer := Bob} = Data) ->
    Msgs = csi_helper:given_messages_are_sent(Client, Bob, 2),
    csi_helper:then_client_does_not_receive_any_message(Client),
    {Client, Data#{msgs => Msgs}}.

activate_csi(_Config, Client, Data) ->
    csi_helper:given_client_is_active(Client),
    {Client, Data}.

receive_csi_msgs(_Config, Client, #{msgs := Msgs} = Data) ->
    csi_helper:then_client_receives_message(Client, Msgs),
    {Client, Data}.

auth_and_bind_with_sm_enabled(Config, Client, Data) ->
    SmEnable = escalus_stanza:enable_sm(),
    plain_auth(Config, Client, Data, [SmEnable], []).

auth_and_bind_with_sm_resume_enabled(Config, Client, Data) ->
    SmEnable = escalus_stanza:enable_sm([{resume, true}]),
    plain_auth(Config, Client, Data, [SmEnable], []).

receive_message_carbon_arrives(
  _Config, Client1, #{client_1_jid := Jid1, client_2_jid := Jid2,
                      client_2 := Client2, peer := Bob} = Data) ->
    escalus_client:send(Bob, escalus_stanza:chat_to(Jid1, <<"hello 1">>)),
    Answers1 = [ escalus_client:wait_for_stanza(C) || C <- [Client1, Client2]],
    escalus_client:send(Bob, escalus_stanza:chat_to(Jid2, <<"hello 2">>)),
    Answers2 = [ escalus_client:wait_for_stanza(C) || C <- [Client1, Client2]],
    {Client1, Data#{answers_1 => Answers1, answers_2 => Answers2}}.

plain_auth(_Config, Client, Data, BindElems, Extra) ->
    InitEl = sasl2_helper:plain_auth_initial_response(Client),
    BindEl = #xmlel{name = <<"bind">>,
                  attrs = #{<<"xmlns">> => ?NS_BIND_2},
                  children = BindElems},
    Authenticate = auth_elem(<<"PLAIN">>, [InitEl, BindEl | Extra]),
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    Identifier = exml_query:path(Answer, [{element, <<"authorization-identifier">>}, cdata]),
    #jid{lresource = LResource} = jid:from_binary(Identifier),
    {Client, Data#{answer => Answer, client_1_jid => Identifier, bind2_resource => LResource}}.

start_peer(Config, Client, Data) ->
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    {Client, Data#{peer => Bob}}.

%% XML helpers
auth_elem(Mech, Children) ->
    #xmlel{name = <<"authenticate">>,
           attrs = #{<<"xmlns">> => ?NS_SASL_2, <<"mechanism">> => Mech},
           children = Children}.

bind_tag(Tag) ->
    #xmlel{name = <<"tag">>, children = [#xmlcdata{content = Tag}]}.

enable_carbons_el() ->
    #xmlel{name = <<"enable">>,
           attrs = #{<<"xmlns">> => ?NS_CARBONS_2}}.

good_user_agent_elem(Uuid) ->
    user_agent_elem(Uuid, <<"cool-xmpp-client">>, <<"latest-and-greatest-device">>).

user_agent_elem(Id, Software, Device) ->
    SoftEl = [#xmlel{name = <<"software">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Software], Value =/= undefined ],
    DeviEl = [#xmlel{name = <<"device">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Device], Value =/= undefined ],
    Attrs = #{<<"id">> => Value || Value <- [Id], Value =/= undefined},
    #xmlel{name = <<"user-agent">>, attrs = Attrs, children = SoftEl ++ DeviEl}.

check_var(InlineFeatures, NS) ->
    Var = exml_query:subelement_with_attr(InlineFeatures, <<"var">>, NS),
    ?assertNotEqual(undefined, Var).
