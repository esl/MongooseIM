-module(mim_c2s_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-define(BAD_RESOURCE, <<"\x{EFBB}"/utf8>>).
-define(MAX_STANZA_SIZE, 1024).

-import(distributed_helper, [mim/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, basic},
     {group, backwards_compatible_session}
    ].

groups() ->
    [
     {basic, [parallel],
      [
       client_sets_stream_from_server_answers_with_to,
       stream_from_does_not_match_sasl_jid_results_in_stream_error,
       two_users_can_log_and_chat,
       too_big_stanza_is_rejected,
       too_big_opening_tag_is_rejected,
       message_sent_to_malformed_jid_results_in_error,
       verify_session_establishment_is_not_announced,
       invalid_resource_fails_to_log
      ]},
     {backwards_compatible_session, [parallel],
      [
       verify_session_establishment_is_announced
      ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    dynamic_modules:ensure_stopped(HostType, [mod_presence]),
    EscalusOverrides = [{initial_activity, fun(_) -> ok end},
                        {start_ready_clients, fun ?MODULE:escalus_start/2}],
    escalus:init_per_suite([{escalus_overrides, EscalusOverrides} | Config1 ]).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    mongoose_helper:restore_config(Config),
    escalus:end_per_suite(Config).

init_per_group(basic, Config) ->
    Steps = [start_stream, stream_features, maybe_use_ssl, authenticate, bind],
    Config1 = save_c2s_listener(Config),
    Config2 = escalus_users:update_userspec(Config1, alice, connection_steps, Steps),
    Config3 = escalus_users:update_userspec(Config2, bob, connection_steps, Steps),
    configure_c2s_listener(Config3, #{backwards_compatible_session => false,
                                      max_stanza_size => ?MAX_STANZA_SIZE}),
    Config3;
init_per_group(backwards_compatible_session, Config) ->
    Config.

end_per_group(basic, Config) ->
    escalus_fresh:clean(),
    restore_c2s_listener(Config),
    Config;
end_per_group(backwards_compatible_session, Config) ->
    escalus_fresh:clean(),
    Config.

init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------
client_sets_stream_from_server_answers_with_to(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Alice = escalus_connection:connect(AliceSpec),
    escalus_client:send(Alice, stream_start(Alice)),
    [StreamStartAnswer, _StreamFeatures] = escalus_client:wait_for_stanzas(Alice, 2, 500),
    #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs} = StreamStartAnswer,
    FromClient = jid:from_binary(escalus_utils:get_jid(Alice)),
    {_, FromServerBin} = lists:keyfind(<<"to">>, 1, Attrs),
    FromServer = jid:from_binary(FromServerBin),
    ?assert(jid:are_equal(FromClient, FromServer)),
    escalus_connection:stop(Alice).

stream_from_does_not_match_sasl_jid_results_in_stream_error(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Alice = escalus_connection:connect(AliceSpec),
    Server = escalus_utils:get_server(Alice),
    escalus_client:send(Alice, stream_start(Server, <<"not_alice@", Server/binary>>)),
    [_StreamStartAnswer, _StreamFeatures] = escalus_client:wait_for_stanzas(Alice, 2, 500),
    try escalus_auth:auth_plain(Alice, AliceSpec) of
        _ -> error(authentication_with_inconsistent_jid_succeeded)
    catch
        throw:{auth_failed, _User, AuthReply} ->
            escalus:assert(is_stream_error, [<<"invalid-from">>, <<>>], AuthReply),
            escalus:assert(is_stream_end, escalus_client:wait_for_stanza(Alice)),
            true = escalus_connection:wait_for_close(Alice, timer:seconds(1))
    end.

two_users_can_log_and_chat(Config) ->
    AliceHost = escalus_users:get_server(Config, alice),
    HostType = domain_helper:domain_to_host_type(mim(), AliceHost),
    HostTypePrefix = domain_helper:make_metrics_prefix(HostType),
    MongooseMetrics = [{[global, data, xmpp, received, xml_stanza_size], changed},
                       {[global, data, xmpp, sent, xml_stanza_size], changed},
                       {[global, data, xmpp, received, c2s, tcp], changed},
                       {[global, data, xmpp, sent, c2s, tcp], changed},
                       {[HostTypePrefix, data, xmpp, c2s, message, processing_time], changed},
                       {[global, data, xmpp, received, c2s, tls], 0},
                       {[global, data, xmpp, sent, c2s, tls], 0}],
    escalus:fresh_story([{mongoose_metrics, MongooseMetrics} | Config],
                        [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Bob)),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Alice))
    end).

too_big_stanza_is_rejected(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _Features} = escalus_connection:start(AliceSpec),
    BigBody = base16:encode(crypto:strong_rand_bytes(?MAX_STANZA_SIZE)),
    escalus_client:send(Alice, escalus_stanza:chat_to(Alice, BigBody)),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>], escalus_client:wait_for_stanza(Alice)),
    escalus:assert(is_stream_end, escalus_client:wait_for_stanza(Alice)),
    true = escalus_connection:wait_for_close(Alice, timer:seconds(1)).

too_big_opening_tag_is_rejected(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _Features} = escalus_connection:start(AliceSpec, []),
    BigAttrs = [{<<"bigattr">>,  base16:encode(crypto:strong_rand_bytes(?MAX_STANZA_SIZE))}],
    escalus_client:send(Alice, #xmlel{name = <<"stream:stream">>, attrs = BigAttrs}),
    escalus:assert(is_stream_start, escalus_client:wait_for_stanza(Alice)),
    escalus:assert(is_stream_error, [<<"xml-not-well-formed">>, <<>>],
                   escalus_client:wait_for_stanza(Alice)),
    escalus:assert(is_stream_end, escalus_client:wait_for_stanza(Alice)),
    true = escalus_connection:wait_for_close(Alice, timer:seconds(1)).

message_sent_to_malformed_jid_results_in_error(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends message with malformed "to"
        Stanza = escalus_client:send_and_wait(Alice,
                                              escalus_stanza:chat_to(<<"@invalid">>, <<"Hi!">>)),
        % Alice receives error
        escalus_assert:is_error(Stanza, <<"modify">>, <<"jid-malformed">>),
        % Alice resends message with proper "to"
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        % Bob gets the message
        escalus_assert:is_chat_message(<<"Hi!">>, escalus_client:wait_for_stanza(Bob))
    end).

invalid_resource_fails_to_log(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = [start_stream, stream_features, authenticate],
    {ok, Alice, _Features} = escalus_connection:start(AliceSpec, Steps),
    BindStanza = escalus_stanza:bind(?BAD_RESOURCE),
    escalus_connection:send(Alice, BindStanza),
    Response = escalus_client:wait_for_stanza(Alice),
    escalus_assert:is_error(Response, <<"modify">>, <<"bad-request">>),
    escalus_connection:stop(Alice).

verify_session_establishment_is_not_announced(Config) ->
    MaybeSessionFeature = start_connection_maybe_get_session_feature(Config),
    ?assertEqual(undefined, MaybeSessionFeature).

verify_session_establishment_is_announced(Config) ->
    MaybeSessionFeature = start_connection_maybe_get_session_feature(Config),
    ?assertNotEqual(undefined, MaybeSessionFeature).

start_connection_maybe_get_session_feature(Config) ->
    Steps = [start_stream, stream_features],
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Client = #client{props = Props}, _} = escalus_connection:start(AliceSpec, Steps),
    ok = escalus_auth:auth_plain(Client, Props),
    escalus_connection:reset_parser(Client),
    Client1 = escalus_session:start_stream(Client),
    Features = escalus_connection:get_stanza(Client1, wait_for_features),
    escalus_connection:stop(Client1),
    exml_query:path(Features, [{element_with_ns, <<"session">>, ?NS_SESSION}]).

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

stream_start(Client) ->
    Server = escalus_utils:get_server(Client),
    From = escalus_utils:get_jid(Client),
    stream_start(Server, From).

stream_start(Server, From) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = [{<<"to">>, Server},
                             {<<"from">>, From},
                             {<<"version">>, <<"1.0">>},
                             {<<"xml:lang">>, <<"en">>},
                             {<<"xmlns">>, <<"jabber:client">>},
                             {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>}]}.

save_c2s_listener(Config) ->
    C2SPort = ct:get_config({hosts, mim, c2s_port}),
    [C2SListener] = mongoose_helper:get_listeners(mim(), #{port => C2SPort, module => mongoose_c2s_listener}),
    [{c2s_listener, C2SListener} | Config].

restore_c2s_listener(Config) ->
    C2SListener = ?config(c2s_listener, Config),
    mongoose_helper:restart_listener(mim(), C2SListener).

configure_c2s_listener(Config, ExtraC2SOpts) ->
    C2SListener = ?config(c2s_listener, Config),
    NewC2SListener = maps:merge(C2SListener, ExtraC2SOpts),
    mongoose_helper:restart_listener(mim(), NewC2SListener).

escalus_start(Cfg, FlatCDs) ->
    {_, RClients} = lists:foldl(
        fun({UserSpec, BaseResource}, {N, Acc}) ->
                Resource = escalus_overridables:do(Cfg, modify_resource, [BaseResource],
                                                   {escalus_utils, identity}),
                {ok, Client} = escalus_client:start(Cfg, UserSpec, Resource),
                {N+1, [Client|Acc]}
        end, {1, []}, FlatCDs),
    Clients = lists:reverse(RClients),
    [ escalus_assert:has_no_stanzas(Client) || Client <- Clients ],
    Clients.
