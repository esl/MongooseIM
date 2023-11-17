-module(domain_isolation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4, subhost_pattern/1]).
-import(domain_helper, [host_type/0, secondary_host_type/0]).
-import(config_parser_helper, [mod_config/2]).

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [{group, two_domains}].

groups() ->
    [{two_domains, [parallel], cases()}].

cases() ->
    [routing_one2one_message_inside_one_domain_works,
     routing_one2one_message_to_another_domain_gets_dropped,
     routing_one2one_message_to_another_domain_results_in_service_unavailable,
     routing_to_yours_subdomain_gets_passed_to_muc_module,
     routing_to_foreign_subdomain_results_in_service_unavailable].

host_types() ->
    %% This suite tests domain isolation.
    %% But two domains could be on the same host type, and still should be isolated.
    %% So, we could need to init modules only once.
    lists:usort([host_type(), secondary_host_type()]).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

modules() ->
    MucHost = subhost_pattern(muc_helper:muc_host_pattern()),
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    [{mod_domain_isolation, []},
     {mod_muc_light, mod_config(mod_muc_light, #{host => MucHost, backend => Backend})}].

init_per_group(two_domains, Config) ->
    Config2 = dynamic_modules:save_modules(host_types(), Config),
    [dynamic_modules:ensure_modules(HostType, modules()) || HostType <- host_types()],
    Config2.

end_per_group(two_domains, Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    Config.

init_per_testcase(Testcase, Config) ->
    escalus:init_per_testcase(Testcase, Config).

end_per_testcase(Testcase, Config) ->
    escalus:end_per_testcase(Testcase, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

routing_one2one_message_inside_one_domain_works(Config) ->
    F = fun(Alice, Bob) ->
          %% WHEN Routed inside the same domain
          escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hello">>)),
          %% THEN Message gets delivered
          Stanza = escalus:wait_for_stanza(Alice),
          escalus:assert(is_chat_message, [<<"Hello">>], Stanza)
        end,
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], F).

routing_one2one_message_to_another_domain_gets_dropped(Config) ->
    F = fun(Alice, Bob, Bis) ->
          %% GIVEN Alice and Bis are on different domains
          %% WHEN A stanza is sent to another domain
          escalus_client:send(Bis, escalus_stanza:chat_to(Alice, <<"Hello">>)),
          %% THEN Receiver does not receive a message
          verify_alice_has_no_pending_messages(Alice, Bob)
        end,
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {alice_bis, 1}], F).

routing_one2one_message_to_another_domain_results_in_service_unavailable(Config) ->
    F = fun(Alice, Bis) ->
          %% GIVEN Alice and Bis are on different domains
          %% WHEN A stanza is sent to another domain
          escalus_client:send(Bis, escalus_stanza:chat_to(Alice, <<"Hello">>)),
          %% THEN Sender receives an error
          receives_service_unavailable(Bis)
        end,
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], F).

routing_to_yours_subdomain_gets_passed_to_muc_module(Config) ->
    F = fun(Alice) ->
          %% GIVEN Alice is on the same domain
          %% WHEN Alice routes a stanza
          escalus_client:send(Alice, invalid_muc_stanza()),
          %% THEN Alice receives an error from mod_muc,
          %%      like if there is no mod_domain_isolation.
          receives_muc_bad_request(Alice)
        end,
    escalus:fresh_story(Config, [{alice, 1}], F).

routing_to_foreign_subdomain_results_in_service_unavailable(Config) ->
    F = fun(Alice) ->
          %% GIVEN Alice is on another domain
          %% WHEN Alice routes a stanza
          escalus_client:send(Alice, invalid_muc_stanza()),
          %% THEN Sender receives an error about the drop
          receives_service_unavailable(Alice)
        end,
    escalus:fresh_story(Config, [{alice_bis, 1}], F).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_error_text(Err) ->
    exml_query:path(Err, [{element, <<"error">>}, {element, <<"text">>}, cdata]).

invalid_muc_address() ->
    MucHost = muc_helper:muc_host(),
    <<MucHost/binary, "/wow_resource_not_so_empty">>.

invalid_muc_stanza() ->
    escalus_stanza:chat_to(invalid_muc_address(), <<"Hi muc!">>).

receives_service_unavailable(Alice) ->
    Err = escalus:wait_for_stanza(Alice),
    escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Err),
    <<"Filtered by the domain isolation">> = get_error_text(Err).

receives_muc_bad_request(Alice) ->
    Err = escalus:wait_for_stanza(Alice),
    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], Err),
    %% This error is generated by mod_muc:
    <<"Resource expected to be empty">> = get_error_text(Err).

%% Verify than there is no unreceived messages by Alice by routing a message from Bob.
%% Bob should be able to send messages to Alice.
%% If the Bob's message gets received - there is no pending messages.
verify_alice_has_no_pending_messages(Alice, Bob) ->
    escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Forces to flush">>)),
    Stanza = escalus:wait_for_stanza(Alice),
    escalus:assert(is_chat_message, [<<"Forces to flush">>], Stanza).
