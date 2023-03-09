-module(mim_c2s_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

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
       two_users_can_log_and_chat,
       too_big_stanza_rejected,
       message_sent_to_malformed_jid_results_in_error,
       verify_session_establishment_is_not_announced
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
    configure_c2s_listener(Config3, #{backwards_compatible_session => false, max_stanza_size => 1024}),
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
two_users_can_log_and_chat(Config) ->
    MongooseMetrics = [{[global, data, xmpp, received, xml_stanza_size], changed},
                       {[global, data, xmpp, sent, xml_stanza_size], changed},
                       {[global, data, xmpp, received, c2s, tcp], changed},
                       {[global, data, xmpp, sent, c2s, tcp], changed},
                       {[global, data, xmpp, received, c2s, tls], 0},
                       {[global, data, xmpp, sent, c2s, tls], 0}],
    escalus:fresh_story([{mongoose_metrics, MongooseMetrics} | Config],
                        [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Bob)),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(Alice))
    end).

too_big_stanza_rejected(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _Features} = escalus_connection:start(AliceSpec),
    BigBody = base16:encode(crypto:strong_rand_bytes(1024)),
    escalus_client:send(Alice, escalus_stanza:chat_to(Alice, BigBody)),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>], escalus_client:wait_for_stanza(Alice)),
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
