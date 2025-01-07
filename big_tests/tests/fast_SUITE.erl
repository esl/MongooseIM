%% Tests for XEP-0484: Fast Authentication Streamlining Tokens
-module(fast_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-define(NS_SASL_2, <<"urn:xmpp:sasl:2">>).
-define(NS_BIND_2, <<"urn:xmpp:bind:0">>).
-define(NS_FAST, <<"urn:xmpp:fast:0">>).

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
       server_advertises_support_for_fast,
       request_token_with_initial_authentication,
       request_token_with_unknown_mechanism_type,
       client_authenticates_using_fast,
       token_auth_fails_when_token_is_wrong,
       token_auth_fails_when_token_is_not_found
      ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        false ->
            {skip, "No RDBMS enabled"};
        true ->
             Config1 = load_modules(Config),
             escalus:init_per_suite(Config1)
    end.

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

%% 3.1 Server advertises support for FAST
%% https://xmpp.org/extensions/xep-0484.html#support
server_advertises_support_for_fast(Config) ->
    Steps = [create_connect_tls, start_stream_get_features],
    #{features := Features} = sasl2_helper:apply_steps(Steps, Config),
    Fast = exml_query:path(Features, [{element_with_ns, <<"authentication">>, ?NS_SASL_2},
                                      {element, <<"inline">>},
                                      {element_with_ns, <<"fast">>, ?NS_FAST}]),
    ?assertNotEqual(undefined, Fast).

%% Client performs initial authentication
%% https://xmpp.org/extensions/xep-0484.html#initial-auth
%% 3.3 Server provides token to client
%% https://xmpp.org/extensions/xep-0484.html#token-response
request_token_with_initial_authentication(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_request_token},
             receive_features],
    #{answer := Success, spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Fast = exml_query:path(Success, [{element_with_ns, <<"token">>, ?NS_FAST}]),
    Expire = exml_query:attr(Fast, <<"expire">>),
    Token = exml_query:attr(Fast, <<"token">>),
    ?assertEqual(true, byte_size(Token) > 5),
    %% Check timestamp in ISO 8601 format
    ?assertEqual(true, time_helper:validate_datetime(binary_to_list(Expire))).

request_token_with_unknown_mechanism_type(Config0) ->
    Config = [{ht_mech, <<"HT-WEIRD-ONE">>} | Config0],
    Steps = [start_new_user, {?MODULE, auth_and_request_token},
             receive_features],
    #{answer := Success, spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Fast = exml_query:path(Success, [{element_with_ns, <<"token">>, ?NS_FAST}]),
    ?assertEqual(undefined, Fast).

%% 3.4 Client authenticates using FAST
%% https://xmpp.org/extensions/xep-0484.html#fast-auth
client_authenticates_using_fast(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_request_token},
             receive_features],
    #{answer := Success, spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Fast = exml_query:path(Success, [{element_with_ns, <<"token">>, ?NS_FAST}]),
    Token = exml_query:attr(Fast, <<"token">>),
    auth_with_token(true, Token, Config, Spec).

token_auth_fails_when_token_is_wrong(Config) ->
    %% New token is not set, but we try to login with a wrong one
    Steps = [start_new_user, {?MODULE, auth_and_request_token},
             receive_features],
    #{answer := Success, spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Token = <<"wrongtoken">>,
    auth_with_token(false, Token, Config, Spec),
    ok.

token_auth_fails_when_token_is_not_found(Config) ->
    %% New token is not set
    Steps = [start_new_user, receive_features],
    #{spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    Token = <<"wrongtoken">>,
    auth_with_token(false, Token, Config, Spec),
    ok.

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

auth_and_request_token(Config, Client, Data) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_token(Mech), user_agent()],
    auth_with_method(Config, Client, Data, [], Extra, <<"PLAIN">>).

auth_using_token(Config, Client, Data) ->
    Extra = [user_agent()],
    auth_with_method(Config, Client, Data, [], Extra, <<"HT-SHA-256-NONE">>).

%% <request-token xmlns='urn:xmpp:fast:0' mechanism='HT-SHA-256-NONE'/>
request_token(Mech) ->
    #xmlel{name = <<"request-token">>,
           attrs = [{<<"xmlns">>, ?NS_FAST},
                    {<<"mechanism">>, Mech}]}.

auth_with_token(Success, Token, Config, Spec) ->
    Spec2 = [{secret_token, Token} | Spec],
    Steps = steps(Success),
    Data = #{spec => Spec2},
    #{answer := Answer} = sasl2_helper:apply_steps(Steps, Config, undefined, Data),
    case Success of
        true ->
            ?assertMatch(#xmlel{name = <<"success">>,
                                attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Answer);
        false ->
            ?assertMatch(#xmlel{name = <<"failure">>,
                                attrs = [{<<"xmlns">>, ?NS_SASL_2}],
                                children = [#xmlel{name = <<"not-authorized">>}]},
                         Answer)
    end.

steps(true) ->
    [connect_tls, start_stream_get_features,
     {?MODULE, auth_using_token},
     receive_features,
     has_no_more_stanzas];
steps(false) ->
    [connect_tls, start_stream_get_features,
     {?MODULE, auth_using_token}].

user_agent() ->
  #xmlel{name = <<"user-agent">>,
         attrs = [{<<"id">>, <<"d4565fa7-4d72-4749-b3d3-740edbf87770">>}],
         children = [cdata_elem(<<"software">>, <<"AwesomeXMPP">>),
                     cdata_elem(<<"device">>, <<"Kiva's Phone">>)]}.

cdata_elem(Name, Value) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Value}]}.

%% See bind2_SUITE:plain_auth
auth_with_method(_Config, Client, Data, BindElems, Extra, Method) ->
    %% we need proof of posesion mechanism
    InitEl = case Method of
        <<"PLAIN">> ->
            sasl2_helper:plain_auth_initial_response(Client);
        <<"HT-SHA-256-NONE">> ->
            ht_auth_initial_response(Client)
        end,
    BindEl = #xmlel{name = <<"bind">>,
                  attrs = [{<<"xmlns">>, ?NS_BIND_2}],
                  children = BindElems},
    Authenticate = auth_elem(Method, [InitEl, BindEl | Extra]),
    escalus:send(Client, Authenticate),
    Answer = escalus_client:wait_for_stanza(Client),
    ct:log("Answer ~p", [Answer]),
    Identifier = exml_query:path(Answer, [{element, <<"authorization-identifier">>}, cdata]),
    case Identifier of
        undefined ->
            {Client, Data#{answer => Answer}};
        _ ->
            #jid{lresource = LResource} = jid:from_binary(Identifier),
            {Client, Data#{answer => Answer, client_1_jid => Identifier, bind2_resource => LResource}}
    end.

auth_elem(Mech, Children) ->
    #xmlel{name = <<"authenticate">>,
           attrs = [{<<"xmlns">>, ?NS_SASL_2}, {<<"mechanism">>, Mech}],
           children = Children}.

%% Creates "Initiator First Message"
%% https://www.ietf.org/archive/id/draft-schmaus-kitten-sasl-ht-09.html#section-3.1
%%
%% The HT mechanism starts with the initiator-msg, send by the initiator to the
%% responder. The following lists the ABNF grammar for the initiator-msg:
%%
%% initiator-msg = authcid NUL initiator-hashed-token
%% authcid = 1*SAFE ; MUST accept up to 255 octets
%% initiator-hashed-token = 1*OCTET
%%
%% NUL    = %0x00 ; The null octet
%% SAFE   = UTF8-encoded string
ht_auth_initial_response(#client{props = Props}) ->
    %% authcid is the username before "@" sign.
    Username = proplists:get_value(username, Props),
    Token = proplists:get_value(secret_token, Props),
    CBData = <<>>,
    ToHash = <<"Initiator", CBData/binary>>,
    InitiatorHashedToken = crypto:mac(hmac, sha256, Token, ToHash),
    Payload = <<Username/binary, 0, InitiatorHashedToken/binary>>,
    initial_response_elem(Payload).

initial_response_elem(Payload) ->
    Encoded = base64:encode(Payload),
    #xmlel{name = <<"initial-response">>,
           children = [#xmlcdata{content = Encoded}]}.
