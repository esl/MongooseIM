%% Tests for XEP-0484: Fast Authentication Streamlining Tokens
-module(fast_auth_token_SUITE).

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
    [{group, Group} || {Group, _, _} <- groups()].

groups() ->
    [{ht_sha_256_none, [parallel], tests()} || {Group, _Mech} <- mechanisms()].

tests() ->
   [server_advertises_support_for_fast,
    request_token_with_initial_authentication,
    request_token_with_unknown_mechanism_type,
    client_authenticates_using_fast,
    client_authenticate_several_times_with_the_same_token,
    token_auth_fails_when_token_is_wrong,
    token_auth_fails_when_token_is_not_found,
    server_initiates_token_rotation,
    could_still_use_old_token_when_server_initiates_token_rotation,
    server_initiates_token_rotation_for_the_current_slot,
    could_still_use_old_token_when_server_initiates_token_rotation_for_the_current_slot,
    rerequest_token_with_initial_authentication,
    can_use_new_token_after_rerequest_token_with_initial_authentication,
    can_use_current_token_after_rerequest_token_with_initial_authentication,
    client_requests_token_invalidation,
    client_requests_token_invalidation_1,
    both_tokens_do_not_work_after_invalidation,
    token_auth_fails_when_mechanism_does_not_match
   ].

mechanisms() ->
   [{ht_sha_256_none, <<"HT-SHA-256-NONE">>},
    {ht_sha_384_none, <<"HT-SHA-384-NONE">>},
    {ht_sha_512_none, <<"HT-SHA-512-NONE">>},
    {ht_sha_3_256_none, <<"HT-SHA-3-256-NONE">>},
    {ht_sha_3_384_none, <<"HT-SHA-3-384-NONE">>},
    {ht_sha_3_512_none, <<"HT-SHA-3-512-NONE">>}].

mech_to_algo(<<"HT-SHA-256-NONE">>) -> sha256;
mech_to_algo(<<"HT-SHA-384-NONE">>) -> sha384;
mech_to_algo(<<"HT-SHA-512-NONE">>) -> sha512;
mech_to_algo(<<"HT-SHA-3-256-NONE">>) -> sha3_256;
mech_to_algo(<<"HT-SHA-3-384-NONE">>) -> sha3_384;
mech_to_algo(<<"HT-SHA-3-512-NONE">>) -> sha3_512.

another_mechanism(<<"HT-SHA-256-NONE">>) -> <<"HT-SHA-3-512-NONE">>;
another_mechanism(<<"HT-SHA-384-NONE">>) -> <<"HT-SHA-3-256-NONE">>;
another_mechanism(<<"HT-SHA-512-NONE">>) -> <<"HT-SHA-3-512-NONE">>;
another_mechanism(<<"HT-SHA-3-512-NONE">>) -> <<"HT-SHA-256-NONE">>;
another_mechanism(<<"HT-SHA-3-384-NONE">>) -> <<"HT-SHA-384-NONE">>;
another_mechanism(<<"HT-SHA-3-256-NONE">>) -> <<"HT-SHA-512-NONE">>.

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

init_per_group(Group, Config) ->
    case lists:keyfind(Group, 1, mechanisms()) of
        Mech when is_binary(Mech) ->
            [{ht_mech, Mech} | Config];
        _ ->
            Config
    end.

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
    #{token := Token, expire := Expire} = connect_and_ask_for_token(Config),
    ?assertEqual(true, byte_size(Token) > 5),
    %% Check timestamp in ISO 8601 format
    ?assertEqual(true, time_helper:validate_datetime(binary_to_list(Expire))).

request_token_with_unknown_mechanism_type(Config0) ->
    Config = [{ht_mech, <<"HT-WEIRD-ONE">>} | Config0],
    Steps = [start_new_user, {?MODULE, auth_and_request_token},
             receive_features],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Fast = exml_query:path(Success, [{element_with_ns, <<"token">>, ?NS_FAST}]),
    ?assertEqual(undefined, Fast).

%% 3.4 Client authenticates using FAST
%% https://xmpp.org/extensions/xep-0484.html#fast-auth
client_authenticates_using_fast(Config) ->
    #{token := Token, spec := Spec} = connect_and_ask_for_token(Config),
    auth_with_token(success, Token, Config, Spec).

%% Check that we can reuse the token
client_authenticate_several_times_with_the_same_token(Config) ->
    #{token := Token, spec := Spec} = connect_and_ask_for_token(Config),
    auth_with_token(success, Token, Config, Spec),
    auth_with_token(success, Token, Config, Spec),
    auth_with_token(success, Token, Config, Spec).

token_auth_fails_when_token_is_wrong(Config) ->
    %% New token is not set, but we try to login with a wrong one
    #{spec := Spec} = connect_and_ask_for_token(Config),
    Token = <<"wrongtoken">>,
    auth_with_token(failure, Token, Config, Spec).

token_auth_fails_when_token_is_not_found(Config) ->
    %% New token is not set
    Steps = [start_new_user],
    #{spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    Token = <<"wrongtoken">>,
    auth_with_token(failure, Token, Config, Spec).

%% 3.5 Server initiates token rotation
%% If client connects with the `current' token (and it is about to expire), we
%% create a brand new token and set it to the `new' slot.
%% Most likely the client lost his token from tthe `new' slot.
%%
%% If client connects with the `new' token (and it is about to expire), we
%% should set this token into the `current' position and generate a `new' token.
%%
%% Output from server is in the same format as for the regular token request.
server_initiates_token_rotation(Config) ->
    #{new_token := NewToken, spec := Spec} = connect_with_almost_expired_token(Config),
    %% Can use new token
    auth_with_token(success, NewToken, Config, Spec).

could_still_use_old_token_when_server_initiates_token_rotation(Config) ->
    #{old_token := OldToken, spec := Spec} = connect_with_almost_expired_token(Config),
    %% Can still use old token
    auth_with_token(success, OldToken, Config, Spec).

%% Connect with almost exired token in the current slot
server_initiates_token_rotation_for_the_current_slot(Config) ->
    #{new_token := NewToken, spec := Spec} = connect_with_almost_expired_token_in_the_current_slot(Config),
    %% Can use new token
    auth_with_token(success, NewToken, Config, Spec).

could_still_use_old_token_when_server_initiates_token_rotation_for_the_current_slot(Config) ->
    #{old_token := OldToken, spec := Spec} = connect_with_almost_expired_token_in_the_current_slot(Config),
    %% Can still use old token
    auth_with_token(success, OldToken, Config, Spec).

rerequest_token_with_initial_authentication(Config) ->
    #{token := Token, spec := Spec} = connect_and_ask_for_token(Config),
    ConnectRes = auth_with_token(success, Token, Config, Spec, request_token),
    #{token := NewToken} = parse_connect_result(ConnectRes),
    ?assertNotEqual(Token, NewToken),
    #{token => Token, new_token => NewToken, spec => Spec}.

can_use_new_token_after_rerequest_token_with_initial_authentication(Config) ->
    #{new_token := Token, spec := Spec} = rerequest_token_with_initial_authentication(Config),
    auth_with_token(success, Token, Config, Spec).

can_use_current_token_after_rerequest_token_with_initial_authentication(Config) ->
    #{token := Token, spec := Spec} = rerequest_token_with_initial_authentication(Config),
    auth_with_token(success, Token, Config, Spec).

client_requests_token_invalidation(Config) ->
    #{token := Token, spec := Spec} = connect_and_ask_for_token(Config),
    ConnectRes = auth_with_token(success, Token, Config, Spec, request_invalidation),
    auth_with_token(failure, Token, Config, Spec).

client_requests_token_invalidation_1(Config) ->
    #{token := Token, spec := Spec} = connect_and_ask_for_token(Config),
    ConnectRes = auth_with_token(success, Token, Config, Spec, request_invalidation_1),
    auth_with_token(failure, Token, Config, Spec).

both_tokens_do_not_work_after_invalidation(Config) ->
    #{new_token := NewToken, token := Token, spec := Spec} =
        rerequest_token_with_initial_authentication(Config),
    auth_with_token(success, Token, Config, Spec, request_invalidation),
    auth_with_token(failure, NewToken, Config, Spec),
    auth_with_token(failure, Token, Config, Spec).

%% Servers MUST bind tokens to the mechanism selected by the client in its
%% original request, and reject attempts to use them with other mechanisms.
%% For example, if the client selected a mechanism capable of channel binding,
%% an attempt to use a mechanism without channel binding MUST fail even if the
%% token would otherwise be accepted by that mechanism.
token_auth_fails_when_mechanism_does_not_match(Config) ->
    #{token := Token, spec := Spec} = connect_and_ask_for_token(Config),
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Config2 = [{ht_mech, another_mechanism(Mech)} | Config],
    auth_with_token(failure, Token, Config2, Spec).

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

connect_with_almost_expired_token(Config) ->
    Steps = [start_new_user],
    #{spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    HostType = domain_helper:host_type(),
    {LUser, LServer} = spec_to_lus(Spec),
    AgentId = user_agent_id(),
    Token = <<"verysecret">>,
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    ExpireTS = erlang:system_time(second) + 600, %% 10 minutes into the future
    %% Set almost expiring token into the new slot
    Args = [HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, false],
    ok = distributed_helper:rpc(distributed_helper:mim(), mod_fast_auth_token_backend, store_new_token, Args),
    ConnectRes = auth_with_token(success, Token, Config, Spec),
    #{token := NewToken} = parse_connect_result(ConnectRes),
    ?assertNotEqual(Token, NewToken),
    #{new_token => NewToken, spec => Spec, old_token => Token}.

connect_with_almost_expired_token_in_the_current_slot(Config) ->
    Now = erlang:system_time(second),
    Steps = [start_new_user],
    #{spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    HostType = domain_helper:host_type(),
    {LUser, LServer} = spec_to_lus(Spec),
    AgentId = user_agent_id(),
    Token = <<"verysecret">>,
    CurrentToken = <<"currentsecret">>,
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    ExpireTS = Now + 86400, %% 24 hours into the future
    SetCurrent = #{
         current_token => CurrentToken,
         current_expire => Now + 600, %% 10 minutes into the future
         current_count => 0,
         current_mech => Mech
    },
    %% Set almost expiring token into the new slot
    Args = [HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent],
    ok = distributed_helper:rpc(distributed_helper:mim(), mod_fast_auth_token_backend, store_new_token, Args),
    ConnectRes = auth_with_token(success, CurrentToken, Config, Spec),
    #{token := NewToken} = parse_connect_result(ConnectRes),
    ?assertNotEqual(Token, NewToken),
    ?assertNotEqual(Token, CurrentToken),
    #{new_token => NewToken, spec => Spec, old_token => CurrentToken}.

connect_and_ask_for_token(Config) ->
    Steps = [start_new_user, {?MODULE, auth_and_request_token},
             receive_features],
    ConnectRes = sasl2_helper:apply_steps(Steps, Config),
    parse_connect_result(ConnectRes).

parse_connect_result(#{answer := Success, spec := Spec}) ->
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Fast = exml_query:path(Success, [{element_with_ns, <<"token">>, ?NS_FAST}]),
    Expire = exml_query:attr(Fast, <<"expire">>),
    Token = exml_query:attr(Fast, <<"token">>),
    #{expire => Expire, token => Token, spec => Spec}.

auth_and_request_token(Config, Client, Data) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_token(Mech), user_agent()],
    auth_with_method(Config, Client, Data, [], Extra, <<"PLAIN">>).

auth_using_token(Config, Client, Data) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [user_agent()],
    auth_with_method(Config, Client, Data, [], Extra, Mech).

auth_using_token_and_request_token(Config, Client, Data) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_token(Mech), user_agent()],
    auth_with_method(Config, Client, Data, [], Extra, Mech).

auth_using_token_and_request_invalidation(Config, Client, Data) ->
    %% While XEP does not specify, what to do with another tokens,
    %% we invalidate both new and current tokens.
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_invalidation(), user_agent()],
    auth_with_method(Config, Client, Data, [], Extra, Mech).

auth_using_token_and_request_invalidation_1(Config, Client, Data) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_invalidation_1(), user_agent()],
    auth_with_method(Config, Client, Data, [], Extra, Mech).

%% <request-token xmlns='urn:xmpp:fast:0' mechanism='HT-SHA-256-NONE'/>
request_token(Mech) ->
    #xmlel{name = <<"request-token">>,
           attrs = [{<<"xmlns">>, ?NS_FAST},
                    {<<"mechanism">>, Mech}]}.

%% <fast xmlns='urn:xmpp:fast:0' count='123' invalidate='true'/>
request_invalidation() ->
    #xmlel{name = <<"fast">>,
           attrs = [{<<"xmlns">>, ?NS_FAST},
                    {<<"invalidate">>, <<"true">>}]}.

%% or <fast xmlns='urn:xmpp:fast:0' count='123' invalidate='1'/>
request_invalidation_1() ->
    #xmlel{name = <<"fast">>,
           attrs = [{<<"xmlns">>, ?NS_FAST},
                    {<<"invalidate">>, <<"1">>}]}.

auth_with_token(Success, Token, Config, Spec) ->
    auth_with_token(Success, Token, Config, Spec, dont_request_token).

auth_with_token(Success, Token, Config, Spec, RequestToken) ->
    Spec2 = [{secret_token, Token} | Spec],
    Steps = steps(Success, auth_function(RequestToken)),
    Data = #{spec => Spec2},
    Res = sasl2_helper:apply_steps(Steps, Config, undefined, Data),
    #{answer := Answer} = Res,
    case Success of
        success ->
            ?assertMatch(#xmlel{name = <<"success">>,
                                attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Answer);
        failure ->
            ?assertMatch(#xmlel{name = <<"failure">>,
                                attrs = [{<<"xmlns">>, ?NS_SASL_2}],
                                children = [#xmlel{name = <<"not-authorized">>}]},
                         Answer)
    end,
    Res.

auth_function(dont_request_token) ->
    auth_using_token;
auth_function(request_token) ->
    auth_using_token_and_request_token;
auth_function(request_invalidation) ->
    auth_using_token_and_request_invalidation;
auth_function(request_invalidation_1) ->
    auth_using_token_and_request_invalidation_1.

steps(success, AuthFun) ->
    [connect_tls, start_stream_get_features,
     {?MODULE, AuthFun},
     receive_features,
     has_no_more_stanzas];
steps(failure, AuthFun) ->
    [connect_tls, start_stream_get_features,
     {?MODULE, AuthFun}].

user_agent_id() ->
    <<"d4565fa7-4d72-4749-b3d3-740edbf87770">>.

user_agent() ->
  #xmlel{name = <<"user-agent">>,
         attrs = [{<<"id">>, user_agent_id()}],
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
        <<"HT-", _/binary>> ->
            ht_auth_initial_response(Client, Method)
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
ht_auth_initial_response(#client{props = Props}, Method) ->
    %% authcid is the username before "@" sign.
    Username = proplists:get_value(username, Props),
    Token = proplists:get_value(secret_token, Props),
    CBData = <<>>,
    ToHash = <<"Initiator", CBData/binary>>,
    Algo = mech_to_algo(Method),
    InitiatorHashedToken = crypto:mac(hmac, Algo, Token, ToHash),
    Payload = <<Username/binary, 0, InitiatorHashedToken/binary>>,
    initial_response_elem(Payload).

initial_response_elem(Payload) ->
    Encoded = base64:encode(Payload),
    #xmlel{name = <<"initial-response">>,
           children = [#xmlcdata{content = Encoded}]}.

spec_to_lus(Spec) ->
    #{username := Username, server := Server} = maps:from_list(Spec),
    jid:to_lus(jid:make_bare(Username, Server)).
