-module(domain_isolation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4, subhost_pattern/1]).

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
     isolation_works_for_one2one,
     isolation_works_for_one2one_2domains,
     isolation_works_for_subdomains
    ].

domain() -> ct:get_config({hosts, mim, domain}).
domain2() -> ct:get_config({hosts, mim, secondary_domain}).
domains() -> [domain(), domain2()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config2 = dynamic_modules:save_modules(domain(), Config),
    escalus:init_per_suite(Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(domain(), Config),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).


init_per_testcase(isolation_works_for_one2one_2domains = TestcaseName, Config) ->
    [dynamic_modules:start(Host, mod_domain_isolation, []) || Host <- domains()],
    escalus:init_per_testcase(TestcaseName, Config);
init_per_testcase(TestcaseName, Config) ->
    Host = domain(),
    MucHost = subhost_pattern(<<"muclight.", Host/binary>>),
    dynamic_modules:start(Host, mod_domain_isolation, [{extra_domains, [MucHost]}]),
    dynamic_modules:start(Host, mod_muc_light, [{host, MucHost}]),
    escalus:init_per_testcase(TestcaseName, Config).

end_per_testcase(isolation_works_for_one2one_2domains = TestcaseName, Config) ->
    [dynamic_modules:stop(Host, mod_domain_isolation) || Host <- domains()],
    escalus:end_per_testcase(TestcaseName, Config);
end_per_testcase(TestcaseName, Config) ->
    Host = domain(),
    dynamic_modules:stop(Host, mod_domain_isolation),
    dynamic_modules:stop(Host, mod_muc_light),
    escalus:end_per_testcase(TestcaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

isolation_works_for_one2one(Config) ->
    F = fun(Alice, Bob, Bis) ->
          %% Ignored from another domain
          escalus_client:send(Bis, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
          %% Routed from the local domain
          escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hello">>)),
          Stanza = escalus:wait_for_stanza(Alice),
          escalus:assert(is_chat_message, [<<"Hello">>], Stanza),
          %% Sender receives an error about the drop
          Err = escalus:wait_for_stanza(Bis),
          escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Err),
          <<"Filtered by the domain isolation">> = get_error_text(Err)
        end,
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {alice_bis, 1}], F).

isolation_works_for_one2one_2domains(Config) ->
    isolation_works_for_one2one(Config).

isolation_works_for_subdomains(Config) ->
    F = fun(Alice, Bis) ->
          Domain = domain(),
          To = <<"muclight.", Domain/binary, "/room">>,
          %% Ignored from another domain
          escalus_client:send(Bis, escalus_stanza:chat_to(To, <<"Hi muc!">>)),
          %% Sender receives an error about the drop
          Err = escalus:wait_for_stanza(Bis),
          escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Err),
          <<"Filtered by the domain isolation">> = get_error_text(Err),
          %% But if Alice is on the same domain, her message passes
          escalus_client:send(Alice, escalus_stanza:chat_to(To, <<"Hi muc!">>)),
          Ok = escalus:wait_for_stanza(Alice),
          escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], Ok)
        end,
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], F).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_error_text(Err) ->
    exml_query:path(Err, [{element, <<"error">>}, {element, <<"text">>}, cdata]).
