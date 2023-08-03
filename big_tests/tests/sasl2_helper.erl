-module(sasl2_helper).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").

-define(NS_SASL_2, <<"urn:xmpp:sasl:2">>).

-type step(Config, Client, Data) :: fun((Config, Client, Data) -> {Client, Data}).

ns() ->
    ?NS_SASL_2.

apply_steps(Steps, Config) ->
    apply_steps(Steps, Config, undefined, #{}).

-spec apply_steps([step(Config, Client, Data)], Config, Client, Data) -> Data.
apply_steps([], _Config, Client, LastData) ->
    escalus_connection:stop(Client),
    LastData;
apply_steps([{Module, Step} | MoreSteps], Config, Client, Data) ->
    {LastClient, LastData} = Module:Step(Config, Client, Data),
    apply_steps(MoreSteps, Config, LastClient, LastData);
apply_steps([Step | MoreSteps], Config, Client, Data) ->
    {LastClient, LastData} = ?MODULE:Step(Config, Client, Data),
    apply_steps(MoreSteps, Config, LastClient, LastData).

connect_non_tls_user(Config, _, Data) ->
    Spec = escalus_fresh:freshen_spec(Config, alice),
    Client1 = escalus_connection:connect(Spec),
    {Client1, Data#{spec => Spec}}.

connect_tls_user(Config, _, Data) ->
    TlsPort = ct:get_config({hosts, mim, c2s_tls_port}),
    Spec = [{port, TlsPort}, {tls_module, ssl}, {ssl, true}, {ssl_opts, [{verify, verify_none}]}
            | escalus_fresh:create_fresh_user(Config, alice)],
    Client1 = escalus_connection:connect(Spec),
    {Client1, Data#{spec => Spec}}.

start_stream_get_features(_Config, Client, Data) ->
    Client1 = escalus_session:start_stream(Client),
    Features = escalus_connection:get_stanza(Client1, wait_for_features),
    {Client1, Data#{features => Features}}.

send_invalid_authenticate_stanza(_Config, Client, Data) ->
    Authenticate = #xmlel{name = <<"authenticate">>,
                          attrs = [{<<"xmlns">>, ?NS_SASL_2},
                                   {<<"mechanism">>, <<"invalid-non-existent-mechanism">>}]},
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

send_bad_user_agent(_Config, Client, Data) ->
    InitialResponse = initial_response_elem(<<"some-random-payload">>),
    Agent = bad_user_agent_elem(),
    Authenticate = auth_elem(<<"PLAIN">>, [InitialResponse, Agent]),
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

plain_auth_user_agent_without_id(Config, Client, Data) ->
    plain_auth(Config, Client, Data, [user_agent_elem_without_id()]).

plain_authentication(Config, Client, Data) ->
    plain_auth(Config, Client, Data, []).

plain_auth(_Config, Client, Data, Extra) ->
    InitEl = plain_auth_initial_response(Client),
    Authenticate = auth_elem(<<"PLAIN">>, [InitEl | Extra]),
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

scram_abort(_Config, Client,
            Data = #{client_state := ClientState3, challenge_stanza := ChallengeStanza}) ->
    Challenge = base64:decode(exml_query:cdata(ChallengeStanza)),
    {continue, _ClientFinal, _ClientState5} = fast_scram:mech_step(ClientState3, Challenge),
    AbortStanza = abort_elem(),
    ok = escalus_connection:send(Client, AbortStanza),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

scram_bad_abort(_Config, Client,
                Data = #{client_state := ClientState3, challenge_stanza := ChallengeStanza}) ->
    Challenge = base64:decode(exml_query:cdata(ChallengeStanza)),
    {continue, _ClientFinal, _ClientState5} = fast_scram:mech_step(ClientState3, Challenge),
    BadAbortStanza = abort_elem(<<"bad-namespace">>),
    ok = escalus_connection:send(Client, BadAbortStanza),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

scram_bad_ns_response(_Config, Client,
                      Data = #{client_state := ClientState3, challenge_stanza := ChallengeStanza}) ->
    Challenge = base64:decode(exml_query:cdata(ChallengeStanza)),
    {continue, ClientFinal, _ClientState5} = fast_scram:mech_step(ClientState3, Challenge),
    BadResponseStanza = response_elem(ClientFinal, <<"bad-namespace">>),
    ok = escalus_connection:send(Client, BadResponseStanza),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

scram_step_1(_Config, Client = #client{props = Props}, Data) ->
    Username = proplists:get_value(username, Props),
    Password = proplists:get_value(password, Props),
    {ok, ClientState1} = fast_scram:mech_new(
        #{entity => client, username => Username, hash_method => sha256, nonce_size => 16,
          channel_binding => {undefined, <<>>}, auth_data => #{password => Password}}),
    {continue, ClientFirst, ClientState3} = fast_scram:mech_step(ClientState1, <<>>),
    InitialResponse = initial_response_elem(ClientFirst),
    AuthStanza = auth_elem(<<"SCRAM-SHA-256">>, [InitialResponse, good_user_agent_elem()]),
    ok = escalus_connection:send(Client, AuthStanza),
    ChallengeStanza = escalus_connection:get_stanza(Client, get_challenge),
    {Client, Data#{client_state => ClientState3, challenge_stanza => ChallengeStanza}}.

scram_step_2(_Config, Client,
             Data = #{client_state := ClientState3, challenge_stanza := ChallengeStanza}) ->
    Challenge = base64:decode(exml_query:cdata(ChallengeStanza)),
    {continue, ClientFinal, ClientState5} = fast_scram:mech_step(ClientState3, Challenge),
    Response = response_elem(ClientFinal),
    ok = escalus_connection:send(Client, Response),
    SuccessStanza = escalus_connection:get_stanza(Client, get_success),
    Success = base64:decode(exml_query:path(SuccessStanza, [{element, <<"additional-data">>}, cdata])),
    case fast_scram:mech_step(ClientState5, Success) of
        {ok, _, _} -> {Client, Data#{answer => SuccessStanza}};
        {error, _, _} -> throw({auth_failed, SuccessStanza})
    end.

receive_features(_Config, Client, Data) ->
    Features = escalus_client:wait_for_stanza(Client),
    {Client, Data#{features => Features}}.

%% XML helpers
auth_elem(Mech, Children) ->
    #xmlel{name = <<"authenticate">>,
           attrs = [{<<"xmlns">>, ?NS_SASL_2}, {<<"mechanism">>, Mech}],
           children = Children}.

plain_auth_initial_response(#client{props = Props}) ->
    Username = proplists:get_value(username, Props),
    Password = proplists:get_value(password, Props),
    AuthPayload = <<0:8, Username/binary, 0:8, Password/binary>>,
    initial_response_elem(AuthPayload).

initial_response_elem(Payload) ->
    Encoded = base64:encode(Payload),
    #xmlel{name = <<"initial-response">>,
           children = [#xmlcdata{content = Encoded}]}.

response_elem(Response) ->
    response_elem(Response, ?NS_SASL_2).

response_elem(Response, NS) ->
    #xmlel{name = <<"response">>,
           attrs = [{<<"xmlns">>, NS}],
           children = [#xmlcdata{content = base64:encode(Response)}]}.

abort_elem() ->
    abort_elem(?NS_SASL_2).

abort_elem(NS) ->
    #xmlel{name = <<"abort">>,
           attrs = [{<<"xmlns">>, NS}],
           children = []}.

user_agent_elem_without_id() ->
    user_agent_elem(undefined, <<"cool-xmpp-client">>, <<"latest-and-greatest-device">>).

good_user_agent_elem() ->
    Uuid = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    user_agent_elem(Uuid, <<"cool-xmpp-client">>, <<"latest-and-greatest-device">>).

bad_user_agent_elem() ->
    user_agent_elem(<<"bad-id">>, undefined, undefined).

user_agent_elem(Id, undefined, Device) ->
    user_agent_elem(Id, [], Device);
user_agent_elem(Id, Software, undefined) ->
    user_agent_elem(Id, Software, []);

user_agent_elem(Id, Software, Device) ->
    SoftEl = [#xmlel{name = <<"software">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Software], Value =/= undefined ],
    DeviEl = [#xmlel{name = <<"device">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Device], Value =/= undefined ],
    Attrs = [{<<"id">>, Value} || Value <- [Id], Value =/= undefined ],
    #xmlel{name = <<"user-agent">>, attrs = Attrs, children = SoftEl ++ DeviEl}.
