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
    [{group, default},
     {group, early_data}].

groups() ->
    [{default, [], group_names_for_mechanisms()},
     {early_data, [], group_names_for_mechanisms()}]
    ++
    groups_for_mechanisms().

groups_for_mechanisms() ->
    [{Group, [parallel], tests()} || {Group, _Mech} <- mechanisms()].

group_names_for_mechanisms() ->
    [{group, Group} || {Group, _, _} <- groups_for_mechanisms()].

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
    cannot_use_expired_token,
    cannot_use_expired_token_in_the_current_slot,
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

enable_session_tickets(Config) ->
    Users = escalus_ct:get_config(escalus_users),
    Alice = proplists:get_value(alice, Users),
    Alice2 = [{ssl_opts, [{session_tickets, manual}]},
              {receive_session_tickets_on_connect, true} | Alice],
    [{escalus_users, [{alice, Alice2}]} | Config].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(early_data, Config) ->
    enable_session_tickets([{early_data, true}|configure_tls_listener(Config)]);
init_per_group(Group, Config) ->
    case lists:keyfind(Group, 1, mechanisms()) of
        {Group, Mech} when is_binary(Mech) ->
            [{ht_mech, Mech} | Config];
        false ->
            Config
    end.

end_per_group(early_data, Config) ->
    restore_tls_listener(Config);
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

configure_tls_listener(Config) ->
    Node = distributed_helper:mim(),
    C2SPort = ct:get_config({hosts, mim, c2s_tls_port}),
    Listener = #{port => C2SPort, module => mongoose_c2s_listener},
    Extra = #{early_data => true, session_tickets => stateless},
    [C2SListener] = mongoose_helper:get_listeners(Node, Listener),
    C2SListener2 = add_tls_opts(C2SListener, Extra),
    ok = mongoose_helper:restart_listener(Node, C2SListener2),
    [{c2s_listener, C2SListener} | Config].

restore_tls_listener(Config) ->
    Node = distributed_helper:mim(),
    C2SListener = proplists:get_value(c2s_listener, Config),
    mongoose_helper:restart_listener(Node, C2SListener).

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
    Steps = [start_new_user,
             {?MODULE, maybe_receive_session_tickets_on_connect},
             {?MODULE, auth_and_request_token},
             receive_features],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
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
    Steps = [start_new_user,
             {?MODULE, maybe_receive_session_tickets_on_connect}],
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
    #{new_token := NewToken, spec := Spec} =
        connect_with_almost_expired_token_in_the_current_slot(Config),
    %% Can use new token
    auth_with_token(success, NewToken, Config, Spec).

could_still_use_old_token_when_server_initiates_token_rotation_for_the_current_slot(Config) ->
    #{old_token := OldToken, spec := Spec} =
        connect_with_almost_expired_token_in_the_current_slot(Config),
    %% Can still use old token
    auth_with_token(success, OldToken, Config, Spec).

cannot_use_expired_token(Config) ->
    #{expired_token := Token, spec := Spec} =
        start_new_user_and_make_expired_token(Config),
    auth_with_token(failure, Token, Config, Spec).

cannot_use_expired_token_in_the_current_slot(Config) ->
    #{new_token := NewToken, spec := Spec, old_token := CurrentToken} =
        start_new_user_and_make_expired_token_in_the_current_slot(Config),
    auth_with_token(failure, CurrentToken, Config, Spec),
    %% But could use the new non-expired token
    auth_with_token(success, NewToken, Config, Spec),
    %% NewToken should be moved into the current slot and still work
    auth_with_token(success, NewToken, Config, Spec).

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
    auth_with_token(success, Token, Config, Spec, request_invalidation),
    auth_with_token(failure, Token, Config, Spec).

client_requests_token_invalidation_1(Config) ->
    #{token := Token, spec := Spec} = connect_and_ask_for_token(Config),
    auth_with_token(success, Token, Config, Spec, request_invalidation_1),
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

start_new_user_and_make_expired_token(Config) ->
    Steps = [start_new_user,
             {?MODULE, maybe_receive_session_tickets_on_connect}],
    #{spec := Spec} = sasl2_helper:apply_steps(Steps, Config),
    HostType = domain_helper:host_type(),
    {LUser, LServer} = spec_to_lus(Spec),
    AgentId = user_agent_id(),
    Token = <<"verysecret">>,
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    ExpireTS = erlang:system_time(second) - 600, %% 10 minutes ago
    %% Set almost expiring token into the new slot
    Args = [HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, false],
    ok = distributed_helper:rpc(distributed_helper:mim(), mod_fast_auth_token_backend, store_new_token, Args),
    #{expired_token => Token, spec => Spec}.

start_new_user_and_make_expired_token_in_the_current_slot(Config) ->
    Now = erlang:system_time(second),
    Steps = [start_new_user,
             {?MODULE, maybe_receive_session_tickets_on_connect}],
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
         current_expire => Now - 600, %% 10 minutes ago
         current_count => 0,
         current_mech => Mech
    },
    %% Set almost expiring token into the new slot
    Args = [HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent],
    ok = distributed_helper:rpc(distributed_helper:mim(), mod_fast_auth_token_backend, store_new_token, Args),
    #{new_token => Token, spec => Spec, old_token => CurrentToken}.

connect_with_almost_expired_token(Config) ->
    Steps = [start_new_user,
             {?MODULE, maybe_receive_session_tickets_on_connect}],
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
    Steps = [start_new_user,
             {?MODULE, maybe_receive_session_tickets_on_connect}],
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
    Steps = [start_new_user,
             {?MODULE, maybe_receive_session_tickets_on_connect},
             {?MODULE, auth_and_request_token},
             receive_features],
    ConnectRes = sasl2_helper:apply_steps(Steps, Config),
    parse_connect_result(ConnectRes).

parse_connect_result(#{answer := Success, spec := Spec}) ->
    ?assertMatch(#xmlel{name = <<"success">>,
                        attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Success),
    Fast = exml_query:path(Success, [{element_with_ns, <<"token">>, ?NS_FAST}]),
    Expire = exml_query:attr(Fast, <<"expire">>),
    Token = exml_query:attr(Fast, <<"token">>),
    #{expire => Expire, token => Token, spec => Spec}.

maybe_receive_session_tickets_on_connect(_Config, Client, Data = #{}) ->
    #{} = Data2 = maybe_receive_session_tickets_on_connect(Client, Data),
    {Client, Data2}.

auth_and_request_token_stanza(Config, Spec) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_token(Mech), user_agent()],
    auth_with_method_stanza(Config, Spec, Extra, <<"PLAIN">>).

auth_using_token_stanza(Config, Spec) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [user_agent()],
    auth_with_method_stanza(Config, Spec, Extra, Mech).

auth_using_token_and_request_token_stanza(Config, Spec) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_token(Mech), user_agent()],
    auth_with_method_stanza(Config, Spec, Extra, Mech).

auth_using_token_and_request_invalidation_stanza(Config, Spec) ->
    %% While XEP does not specify, what to do with another tokens,
    %% we invalidate both new and current tokens.
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_invalidation(), user_agent()],
    auth_with_method_stanza(Config, Spec, Extra, Mech).

auth_using_token_and_request_invalidation_1_stanza(Config, Spec) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    Extra = [request_invalidation_1(), user_agent()],
    auth_with_method_stanza(Config, Spec, Extra, Mech).

%% <request-token xmlns='urn:xmpp:fast:0' mechanism='HT-SHA-256-NONE'/>
request_token(Mech) ->
    #xmlel{name = <<"request-token">>,
           attrs = #{<<"xmlns">> => ?NS_FAST,
                     <<"mechanism">> => Mech}}.

%% <fast xmlns='urn:xmpp:fast:0' count='123' invalidate='true'/>
request_invalidation() ->
    #xmlel{name = <<"fast">>,
           attrs = #{<<"xmlns">> => ?NS_FAST,
                     <<"invalidate">> => <<"true">>}}.

%% or <fast xmlns='urn:xmpp:fast:0' count='123' invalidate='1'/>
request_invalidation_1() ->
    #xmlel{name = <<"fast">>,
           attrs = #{<<"xmlns">> => ?NS_FAST,
                     <<"invalidate">> => <<"1">>}}.

auth_with_token(Success, Token, Config, Spec) ->
    auth_with_token(Success, Token, Config, Spec, dont_request_token).

auth_with_token(Success, Token, Config, Spec, RequestToken) ->
    Spec2 = [{secret_token, Token}] ++ Spec,
    Authenticate = auth_stanza(RequestToken, Config, Spec2),
    Steps = steps(Success),
    EarlyDataEnabled = proplists:get_value(early_data, Config, false),
    Data = #{spec => set_early_data(Spec2, EarlyDataEnabled, Authenticate),
             early_data => EarlyDataEnabled,
             auth_stanza => case EarlyDataEnabled of true -> skip; false -> Authenticate end},
    Res = sasl2_helper:apply_steps(Steps, Config, undefined, Data),
    #{answer := Answer} = Res,
    case Success of
        success ->
            ?assertMatch(#xmlel{name = <<"success">>,
                                attrs = #{<<"xmlns">> := ?NS_SASL_2}}, Answer);
        failure ->
            ?assertMatch(#xmlel{name = <<"failure">>,
                                attrs = #{<<"xmlns">> := ?NS_SASL_2},
                                children = [#xmlel{name = <<"not-authorized">>}]},
                         Answer)
    end,
    Res.

set_early_data(Spec, false, _Authenticate) ->
    Spec;
set_early_data(Spec, true, Authenticate) ->
    Map = proplists:to_map(Spec),
    #{ssl_opts := SslOpts} = Map,
    Map2 = Map#{
         ssl_opts => [{early_data, make_early_data(Spec, Authenticate)} | SslOpts]},
    proplists:from_map(Map2).

make_early_data(Spec, Authenticate) ->
    Start = start_stream(Spec),
    Auth = exml:to_binary(Authenticate),
    <<Start/binary, Auth/binary>>.

receive_ticket(Client) ->
    Pid = escalus_client_to_pid(Client),
    receive
        {escalus_ssl_session_ticket, ConnPid, Ticket} when Pid =:= ConnPid ->
            Ticket
        after 3000 ->
            ct:fail({receive_ticket, timeout, Client,
                     erlang:process_info(self(), messages)})
    end.

receive_early_data_accepted(Client) ->
    Pid = escalus_client_to_pid(Client),
    receive
        {escalus_ssl_early_data_result, ConnPid, Result} when Pid =:= ConnPid ->
            ?assertEqual(accepted, Result)
        after 3000 ->
            ct:fail({receive_early_data, timeout, Client,
                     erlang:process_info(self(), messages)})
    end.

auth_stanza(dont_request_token, Config, Spec) ->
    auth_using_token_stanza(Config, Spec);
auth_stanza(request_token, Config, Spec) ->
    auth_using_token_and_request_token_stanza(Config, Spec);
auth_stanza(request_invalidation, Config, Spec) ->
    auth_using_token_and_request_invalidation_stanza(Config, Spec);
auth_stanza(request_invalidation_1, Config, Spec) ->
    auth_using_token_and_request_invalidation_1_stanza(Config, Spec).

%% From escalus_connection:start_stream
start_stream(Spec) ->
    exml:to_binary(escalus_tcp:stream_start_req(Spec)).

steps(success) ->
    [connect_tls,
     {?MODULE, start_stream_get_features},
     {?MODULE, auth_with_method},
     receive_features,
     has_no_more_stanzas];
steps(failure) ->
    [connect_tls,
     {?MODULE, start_stream_get_features},
     {?MODULE, auth_with_method}].

user_agent_id() ->
    <<"d4565fa7-4d72-4749-b3d3-740edbf87770">>.

user_agent() ->
  #xmlel{name = <<"user-agent">>,
         attrs = #{<<"id">> => user_agent_id()},
         children = [cdata_elem(<<"software">>, <<"AwesomeXMPP">>),
                     cdata_elem(<<"device">>, <<"Kiva's Phone">>)]}.

cdata_elem(Name, Value) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Value}]}.

make_0rtt_data(Spec, Config) ->
    Mech = proplists:get_value(ht_mech, Config, <<"HT-SHA-256-NONE">>),
    <<"HT-", _/binary>> = Mech,
    InitEl = ht_auth_initial_response(Spec, Mech),
    BindEl = #xmlel{name = <<"bind">>,
                  attrs = #{<<"xmlns">> => ?NS_BIND_2}},
    exml:to_binary(auth_elem(Mech, [InitEl, BindEl])).

%% See bind2_SUITE:plain_auth
auth_with_method_stanza(_Config, Spec, Extra, Mech) ->
    %% we need proof of posesion mechanism
    InitEl = case Mech of
        <<"PLAIN">> ->
            sasl2_helper:plain_auth_initial_response_from_spec(Spec);
        <<"HT-", _/binary>> ->
            ht_auth_initial_response(Spec, Mech)
        end,
    BindEl = #xmlel{name = <<"bind">>,
                  attrs = #{<<"xmlns">> => ?NS_BIND_2}},
    auth_elem(Mech, [InitEl, BindEl | Extra]).

start_stream_get_features(_Config, Client, Data = #{early_data := true}) ->
    receive_early_data_accepted(Client),
    %% Stream start is already sent in the early data.
    %% We just need to receive the response.
    StreamStartResp = escalus_connection:get_stanza(Client, stream_start, 5000),
    Features = escalus_connection:get_stanza(Client, wait_for_features),
    {Client, Data#{features => Features, stream_start_resp => StreamStartResp}};
start_stream_get_features(Config, Client, Data = #{}) ->
    %% If early data is enabled, start_stream is already sent
    {Client2, Data2} = sasl2_helper:start_stream_get_features(Config, Client, Data),
    {Client2, Data2}.

auth_and_request_token(Config, Client, Data = #{spec := Spec}) ->
    Authenticate = auth_and_request_token_stanza(Config, Spec),
    auth_with_method(Config, Client, Data#{auth_stanza => Authenticate}).

auth_with_method(_Config, Client, Data = #{auth_stanza := Authenticate}) ->
    case Authenticate of
        skip ->
            ok;
        _ ->
            escalus:send(Client, Authenticate)
    end,
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
           attrs = #{<<"xmlns">> => ?NS_SASL_2, <<"mechanism">> => Mech},
           children = Children}.

client_to_spec(#client{props = Props}) ->
    Props.

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
ht_auth_initial_response(Props, Mech) ->
    %% authcid is the username before "@" sign.
    Username = proplists:get_value(username, Props),
    Token = proplists:get_value(secret_token, Props),
    is_binary(Token) orelse ct:fail({bad_secret_token, Props}),
    CBData = <<>>,
    ToHash = <<"Initiator", CBData/binary>>,
    Algo = mech_to_algo(Mech),
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

escalus_client_to_pid(#client{rcv_pid = Pid}) ->
    Pid.

escalus_client_to_props(#client{props = Props}) ->
    Props.

set_ticket(Data = #{spec := Spec}, Ticket) ->
    Map = proplists:to_map(Spec),
    #{ssl_opts := SslOpts} = Map,
    Map2 = Map#{
         %% Set Ticket to be used with the next reconnect
         ssl_opts => [{use_ticket, [Ticket]} | SslOpts]},
    Data#{spec => proplists:from_map(Map2)}.

maybe_receive_session_tickets_on_connect(Client, Data = #{spec := Spec}) ->
    case proplists:get_value(receive_session_tickets_on_connect, Spec, false) of
        true ->
            Ticket = receive_ticket(Client),
            set_ticket(Data, Ticket);
        false ->
            Data
    end.

add_tls_opts(C2SListener = #{tls := TLS}, Extra) ->
    C2SListener#{tls => maps:merge(TLS, Extra)}.
