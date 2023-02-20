-module(mim_c2s_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0]).

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
       two_users_can_log_and_chat,
       too_big_stanza_rejected
      ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    Steps = [start_stream, stream_features, maybe_use_ssl, authenticate, bind],
    EscalusOverrides = [{initial_activity, fun(_) -> ok end},
                        {start_ready_clients, fun ?MODULE:escalus_start/2}],
    Config1 = save_c2s_listener(Config),
    Config2 = dynamic_modules:save_modules(HostType, Config1),
    dynamic_modules:ensure_stopped(HostType, [mod_presence]),
    Config3 = escalus_users:update_userspec(Config2, alice, connection_steps, Steps),
    Config4 = escalus_users:update_userspec(Config3, bob, connection_steps, Steps),
    configure_c2s_listener(Config4, #{backwards_compatible_session => false, max_stanza_size => 1024}),
    escalus:init_per_suite([{escalus_overrides, EscalusOverrides} | Config4 ]).

end_per_suite(Config) ->
    restore_c2s_listener(Config),
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(Name, Config) ->
    mongoose_helper:restore_config(Config),
    escalus:end_per_testcase(Name, Config).

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------
two_users_can_log_and_chat(Config) ->
    MongooseMetrics = [{[global, data, xmpp, received, xml_stanza_size], changed},
                       {[global, data, xmpp, sent, xml_stanza_size], changed},
                       {[global, data, xmpp, received, c2s, tcp, raw], changed},
                       {[global, data, xmpp, sent, c2s, tcp, raw], changed},
                       {[global, data, xmpp, received, c2s, tls, raw], 0},
                       {[global, data, xmpp, sent, c2s, tls, raw], 0}],
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
