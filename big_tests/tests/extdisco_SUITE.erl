%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(extdisco_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0,
                             rpc/4]).

-define(EXT_DISCO, <<"urn:xmpp:extdisco:2">>).

-compile([export_all]).

all() ->
    [{group, extdisco_not_configured},
     {group, extdisco_configured},
     {group, multiple_extdisco_configured}].

groups() ->
    G = [{extdisco_not_configured, [sequence], extdisco_not_configured_tests()},
         {extdisco_configured, [sequence], extdisco_configured_tests()},
         {multiple_extdisco_configured, [sequence], multiple_extdisco_configured_tests()}],
    ct_helper:repeat_all_until_all_ok(G).

extdisco_not_configured_tests() ->
    [ external_services_discovery_not_supported,
      no_external_services_configured_no_services_returned ].

extdisco_configured_tests() ->
    tests().

multiple_extdisco_configured_tests() ->
    tests().

tests() ->
    [ external_services_discovery_supported,
      external_services_configured_all_returned,
      external_services_configured_only_matching_by_type_returned,
      external_services_configured_no_matching_services_no_returned,
      external_services_configured_credentials_returned,
      external_services_configured_no_matching_credentials_no_returned,
      external_services_configured_incorrect_request_no_returned ].

init_per_suite(C) ->
    Config = dynamic_modules:save_modules(domain(), C),
    escalus:init_per_suite(Config).

init_per_group(extdisco_configured, C) ->
    ExternalServices = [{stun, [
                            {host, "1.1.1.1"},
                            {port, "3478"},
                            {transport, "udp"},
                            {username, "username"},
                            {password, "secret"}
                            ]}],
    set_external_services(ExternalServices, C);
init_per_group(multiple_extdisco_configured, C) ->
    ExternalServices = [{stun, [
                            {host, "1.1.1.1"},
                            {port, "3478"},
                            {transport, "udp"},
                            {username, "username"},
                            {password, "secret"}
                            ]},
                        {turn, [
                            {host, "2.2.2.2"},
                            {port, "3478"},
                            {transport, "tcp"},
                            {username, "username"},
                            {password, "secret"}
                            ]}],
    set_external_services(ExternalServices, C);
init_per_group(_GroupName, Config) ->
   Config.

init_per_testcase(external_services_discovery_not_supported = Name, C) ->
    Config = remove_external_services(C),
    escalus:init_per_testcase(Name, Config);
init_per_testcase(no_external_services_configured_no_services_returned = Name, C) ->
    ExternalServices = [],
    Config = set_external_services(ExternalServices, C),
    escalus:init_per_testcase(Name, Config);
init_per_testcase(Name, C) ->
    escalus:init_per_testcase(Name, C).

end_per_group(_GroupName, Config) ->
    dynamic_modules:restore_modules(domain(), Config),
    Config.

end_per_suite(C) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(C).

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

external_services_discovery_not_supported(Config) ->
    % Given external service discovery is not configured
    Test = fun(Alice) ->

       % When requesting for disco_info
       IqGet = escalus_stanza:disco_info(domain()),
       escalus_client:send(Alice, IqGet),

       % Then extdisco feature is not listed as supported feature
       Result = escalus_client:wait_for_stanza(Alice),
       escalus:assert(is_iq_result, [IqGet], Result),
       escalus:assert(fun(Stanza) ->
           not escalus_pred:has_feature(?EXT_DISCO, Stanza)
           end, Result)
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

no_external_services_configured_no_services_returned(Config) ->
    % Given external service discovery is configured with empty list
    Test = fun(Alice) ->

        % When requesting for external services
        Iq = request_external_services(domain()),
        escalus_client:send(Alice, Iq),

        % Then serivces but no serivce element is in the iq result,
        % which means that empty serivces element got returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?EXT_DISCO)),
        ?assertEqual([], get_service_element(Result))
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_discovery_supported(Config) ->
    % Given external service discovery is configured
    Test = fun(Alice) ->

        % When requesting for disco_info
        IqGet = escalus_stanza:disco_info(domain()),
        escalus_client:send(Alice, IqGet),

        % Then extdisco feature is listed as supported feature
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [IqGet], Result),
        escalus:assert(has_feature, [?EXT_DISCO], Result)
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_all_returned(Config) ->
    % Given external service discovery is configured
    Test = fun(Alice) ->

        % When requesting for external services
        Iq = request_external_services(domain()),
        escalus_client:send(Alice, Iq),

        % Then list of external services with containing all
        % supported_elements() is returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?EXT_DISCO)),
        [all_services_are_returned(Service) || Service <- get_service_element(Result)]
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_only_matching_by_type_returned(Config) ->
    % Given external service discovery is configured
    Test = fun(Alice) ->

        % When requesting for external service of specified type
        Type = <<"stun">>,
        Iq = request_external_services_with_type(domain(), Type),
        escalus_client:send(Alice, Iq),

        % Then the list of external services of the specified type is returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?EXT_DISCO)),
        [all_services_are_returned(Service, Type) || Service <- get_service_element(Result)]
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_no_matching_services_no_returned(Config) ->
    % Given external service discovery is configured
    Test = fun(Alice) ->

        % When requesting for external service of unknown or unconfigured type
        Type = <<"unknown_type">>,
        Iq = request_external_services_with_type(domain(), Type),
        escalus_client:send(Alice, Iq),

        % Then the serivces but no serivce element is in the iq result,
        % which means that empty serivces element got returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?EXT_DISCO)),
        ?assertEqual([], get_service_element(Result))
    end,
escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_credentials_returned(Config) ->
    % Given external service discovery is configured with credentials
    Test = fun(Alice) ->

        % When requesting for credentials of external service of given type
        % and specified host
        Type = <<"stun">>,
        Host = <<"1.1.1.1">>,
        Iq = request_external_services_credentials(domain(), Type, Host),
        escalus_client:send(Alice, Iq),

        % Then the list of external services of the specified type and host
        % is returned together with STUN/TURN login credentials
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"credentials">>, ?EXT_DISCO)),
        Services = get_service_element(Result),
        ?assertNotEqual([], Services),
        [all_services_are_returned(Service, Type) || Service <- Services]
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_no_matching_credentials_no_returned(Config) ->
    % Given external service discovery is configured with credentials
    Test = fun(Alice) ->

        % When requesting for credentials of external service of unknown type
        % and unknown host
        Type = <<"unknown_type">>,
        Host = <<"unknown_host">>,
        Iq = request_external_services_credentials(domain(), Type, Host),
        escalus_client:send(Alice, Iq),

        % Then the credentials but no serivce element is in the iq result,
        % which means that empty credentials element got returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"credentials">>, ?EXT_DISCO)),
        ?assertEqual([], get_service_element(Result))
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_incorrect_request_no_returned(Config) ->
    % Given external service discovery is configured
    Test = fun(Alice) ->

        % When sending request with incorrect elements
        Iq = request_external_services_incorrect(domain()),
        escalus_client:send(Alice, Iq),

        % Then empty iq_result is returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(false, has_subelement_with_ns(Result, <<"incorrect">>, ?EXT_DISCO))
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

domain() ->
    ct:get_config({hosts, mim, domain}).

set_external_services(Opts, C) ->
    Module = [{mod_extdisco, Opts}],
    Config = dynamic_modules:save_modules(domain(), C),
    ok = dynamic_modules:ensure_modules(domain(), Module),
    Config.

remove_external_services(C) ->
    OldModules = rpc(mim(), gen_mod, loaded_modules_with_opts, [domain()]),
    NewModules = lists:keydelete(mod_extdisco, 1, OldModules),
    ok = rpc(mim(), gen_mod_deps, replace_modules, [domain(), OldModules, NewModules]),
    C.

request_external_services(To) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"services">>,
                attrs = [{<<"xmlns">>, <<"urn:xmpp:extdisco:2">>}]}]).

request_external_services_with_type(To, Type) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"services">>,
                attrs = [{<<"xmlns">>, <<"urn:xmpp:extdisco:2">>},
                         {<<"type">>, Type}]}]).

request_external_services_credentials(To, Type, Host) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"credentials">>,
                attrs = [{<<"xmlns">>, <<"urn:xmpp:extdisco:2">>}],
                children = [#xmlel{name = <<"service">>,
                                   attrs = [{<<"host">>, Host},
                                            {<<"type">>, Type}]}]}]).

request_external_services_incorrect(To) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"incorrect">>,
                attrs = [{<<"xmlns">>, <<"urn:xmpp:extdisco:2">>}]}]).

get_service_element(Result) ->
    Services = exml_query:subelement_with_ns(Result, ?EXT_DISCO),
    exml_query:subelements(Services, <<"service">>).

supported_elements() ->
    [<<"host">>, <<"type">>, <<"port">>, <<"username">>, <<"password">>].

all_services_are_returned(Service) ->
    [?assertEqual(true, has_subelement(Service, E)) || E <- supported_elements()].

all_services_are_returned(Service, Type) ->
    ?assertEqual(true, has_attr_with_value(Service, <<"type">>, Type)),
    all_services_are_returned(Service).

no_services_are_returned(Service) ->
    [?assertEqual(false, has_subelement(Service, E)) || E <- supported_elements()].

has_subelement(Stanza, Element) ->
    undefined =/= exml_query:attr(Stanza, Element).

has_attr_with_value(Stanza, Element, Value) ->
    Value ==  exml_query:attr(Stanza, Element).

has_subelement_with_ns(Stanza, Element, NS) ->
    [] =/= exml_query:subelements_with_name_and_ns(Stanza, Element, NS).
