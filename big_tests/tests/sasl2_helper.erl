-module(sasl2_helper).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-define(VALID_UUID_BUT_NOT_V4, <<"a55c8fde-2cef-0655-a55c-8fde2cefc655">>).
-define(NS_SASL_2, <<"urn:xmpp:sasl:2">>).

-type step(Config, Client, Data) :: fun((Config, Client, Data) -> {Client, Data}).
-import(config_parser_helper, [mod_config/2, default_mod_config/1]).

ns() ->
    ?NS_SASL_2.

load_all_sasl2_modules(HostType) ->
    MemBackend = ct_helper:get_internal_database(),
    SMOpts = #{ack_freq => never, backend => MemBackend},
    Modules = [{mod_bind2, default_mod_config(mod_bind2)},
               {mod_sasl2, default_mod_config(mod_sasl2)},
               {mod_csi, default_mod_config(mod_csi)},
               {mod_carboncopy, default_mod_config(mod_carboncopy)},
               {mod_stream_management, mod_config(mod_stream_management, SMOpts)}]
        ++ rdbms_mods(),
    dynamic_modules:ensure_modules(HostType, Modules).

rdbms_mods() ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true ->
            [{mod_fast_auth_token, mod_config(mod_fast_auth_token, #{backend => rdbms})}];
        false ->
            []
    end.

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

create_connect_tls(Config, Client, Data) ->
    {Client1, Data1} = create_user(Config, Client, Data),
    connect_tls(Config, Client1, Data1).

start_new_user(Config, Client, Data) ->
    {Client1, Data1} = create_connect_tls(Config, Client, Data),
    start_stream_get_features(Config, Client1, Data1).

create_user(Config, Client, Data) ->
    Spec = escalus_fresh:create_fresh_user(Config, alice),
    {Client, Data#{spec => Spec}}.

connect_tls(Config, _, #{spec := Spec} = Data) ->
    %% Direct TLS port
    TlsPort = ct:get_config({hosts, mim, c2s_tls_port}),
    SSLOpts = proplists:get_value(ssl_opts, Spec, []),
    SSLOpts2 = SSLOpts ++ [{verify, verify_none}],
    Spec1 = [{port, TlsPort}, {tls_module, ssl}, {ssl, true},
             {ssl_opts, SSLOpts2} | lists:keydelete(ssl_opts, 1, Spec)],
    Client1 = escalus_connection:connect(Spec1),
    {Client1, Data#{spec => Spec1}}.

start_stream_get_features(_Config, Client, Data) ->
    Client1 = escalus_session:start_stream(Client),
    Features = escalus_connection:get_stanza(Client1, wait_for_features),
    {Client1, Data#{features => Features}}.

%% From escalus_connection:start_stream
-spec start_stream(escalus_connection:client()) -> binary().
start_stream(#client{module = Mod, props = Props} = Client) ->
    exml:to_binary(Mod:stream_start_req(Props)).

send_invalid_mech_auth_stanza(_Config, Client, Data) ->
    Authenticate = auth_elem(<<"invalid-non-existent-mechanism">>, []),
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

send_invalid_ns_auth_stanza(_Config, Client, Data) ->
    Authenticate = auth_elem(<<"PLAIN">>, <<"bad-namespace">>, []),
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

send_bad_user_agent_uuid(_Config, Client, Data) ->
    InitialResponse = initial_response_elem(<<"some-random-payload">>),
    Agent = bad_user_agent_elem(?VALID_UUID_BUT_NOT_V4),
    Authenticate = auth_elem(<<"PLAIN">>, [InitialResponse, Agent]),
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    {Client, Data#{answer => Answer}}.

auth_with_resumption(Config, Client, #{smid := SMID, texts := Texts} = Data) ->
    Resume = escalus_stanza:resume(SMID, 1),
    {Client1, Data1} = plain_auth(Config, Client, Data, [Resume]),
    Msgs = sm_helper:wait_for_messages(Client, Texts),
    {Client1, Data1#{sm_storage => Msgs}}.

auth_with_resumption_invalid_h(Config, Client, #{smid := SMID} = Data) ->
    Resume = #xmlel{name = <<"resume">>,
                    attrs = #{<<"xmlns">> => ?NS_STREAM_MGNT_3,
                              <<"previd">> => SMID,
                              <<"h">> => <<"aaa">>}},
    plain_auth(Config, Client, Data, [Resume]).

auth_with_resumption_missing_previd(Config, Client, Data) ->
    Resume = #xmlel{name = <<"resume">>,
                    attrs = #{<<"xmlns">> => ?NS_STREAM_MGNT_3,
                              <<"h">> => <<"aaa">>}},
    plain_auth(Config, Client, Data, [Resume]).

auth_with_resumption_exceeding_h(Config, Client, #{smid := SMID} = Data) ->
    Resume = escalus_stanza:resume(SMID, 999),
    plain_auth(Config, Client, Data, [Resume]).

auth_with_resumption_unknown_smid(Config, Client, Data) ->
    Resume = escalus_stanza:resume(<<"123456">>, 1),
    plain_auth(Config, Client, Data, [Resume]).

has_no_more_stanzas(_Config, Client, Data) ->
    escalus_assert:has_no_stanzas(Client),
    {Client, Data}.

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

buffer_messages_and_die(Config, _Client, #{spec := Spec} = Data) ->
    Client = sm_helper:connect_spec(Spec, sr_presence, manual),
    C2SPid = mongoose_helper:get_session_pid(Client),
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    Texts = [ integer_to_binary(N) || N <- [1, 2, 3]],
    sm_helper:send_messages(Bob, Client, Texts),
    %% Client receives them, but doesn't ack.
    sm_helper:wait_for_messages(Client, Texts),
    %% Client's connection is violently terminated.
    escalus_client:kill_connection(Config, Client),
    sm_helper:wait_until_resume_session(C2SPid),
    SMID = sm_helper:client_to_smid(Client),
    {C2SPid, Data#{bob => Bob, smid => SMID, smh => 3, texts => Texts}}.

can_send_messages(_Config, Client, #{bob := Bob} = Data) ->
    escalus:send(Bob, escalus_stanza:chat_to(Client, <<"hello">>)),
    AliceReceived = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_message, AliceReceived),
    escalus:send(Client, escalus_stanza:chat_to(Bob, <<"hello">>)),
    BobReceived = escalus_client:wait_for_stanza(Bob),
    escalus:assert(is_message, BobReceived),
    {Client, Data}.

receive_features(_Config, Client, Data) ->
    Features = escalus_client:wait_for_stanza(Client),
    {Client, Data#{features => Features}}.

%% XML helpers
auth_elem(Mech, Children) ->
    auth_elem(Mech, ?NS_SASL_2, Children).

auth_elem(Mech, NS, Children) ->
    #xmlel{name = <<"authenticate">>,
           attrs = #{<<"xmlns">> => NS, <<"mechanism">> => Mech},
           children = Children}.

plain_auth_initial_response(#client{props = Props}) ->
    plain_auth_initial_response_from_spec(Props).

plain_auth_initial_response_from_spec(Props) ->
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
           attrs = #{<<"xmlns">> => NS},
           children = [#xmlcdata{content = base64:encode(Response)}]}.

abort_elem() ->
    abort_elem(?NS_SASL_2).

abort_elem(NS) ->
    #xmlel{name = <<"abort">>,
           attrs = #{<<"xmlns">> => NS},
           children = []}.

user_agent_elem_without_id() ->
    user_agent_elem(undefined, <<"cool-xmpp-client">>, <<"latest-and-greatest-device">>).

good_user_agent_elem() ->
    Uuid = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    user_agent_elem(Uuid, <<"cool-xmpp-client">>, <<"latest-and-greatest-device">>).

bad_user_agent_elem() ->
    user_agent_elem(<<"bad-id">>, undefined, undefined).

bad_user_agent_elem(Uuid) ->
    user_agent_elem(Uuid, undefined, undefined).

user_agent_elem(Id, undefined, Device) ->
    user_agent_elem(Id, [], Device);
user_agent_elem(Id, Software, undefined) ->
    user_agent_elem(Id, Software, []);

user_agent_elem(Id, Software, Device) ->
    SoftEl = [#xmlel{name = <<"software">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Software], Value =/= undefined ],
    DeviEl = [#xmlel{name = <<"device">>, children = [#xmlcdata{content = Value}]}
              || Value <- [Device], Value =/= undefined ],
    Attrs = #{<<"id">> => Value || Value <- [Id], Value =/= undefined},
    #xmlel{name = <<"user-agent">>, attrs = Attrs, children = SoftEl ++ DeviEl}.
