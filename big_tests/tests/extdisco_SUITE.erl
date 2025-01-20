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

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0,
                             rpc/4]).

-import(domain_helper, [domain/0, host_type/0]).

-define(NS_EXTDISCO, <<"urn:xmpp:extdisco:2">>).

-compile([export_all, nowarn_export_all]).

all() ->
    [{group, extdisco_not_configured},
     {group, extdisco_configured},
     {group, multiple_extdisco_configured},
     {group, extdisco_required_elements_configured}].

groups() ->
    [{extdisco_not_configured, [sequence], extdisco_not_configured_tests()},
     {extdisco_configured, [sequence], extdisco_configured_tests()},
     {multiple_extdisco_configured, [sequence], multiple_extdisco_configured_tests()},
     {extdisco_required_elements_configured, [sequence], extdisco_required_elements_configured_tests()}].

extdisco_not_configured_tests() ->
    [external_services_discovery_not_supported,
     no_external_services_configured_no_services_returned].

extdisco_configured_tests() ->
    tests().

multiple_extdisco_configured_tests() ->
    tests().

tests() ->
    [external_services_discovery_supported,
     external_services_configured_all_returned,
     external_services_configured_only_matching_by_type_returned,
     external_services_configured_no_matching_services_no_returned,
     external_services_configured_credentials_returned,
     external_services_configured_no_matching_credentials_no_returned,
     external_services_configured_no_matching_credentials_type_no_returned,
     external_services_configured_incorrect_request_no_returned].

extdisco_required_elements_configured_tests() ->
    [external_service_required_elements_configured].

init_per_suite(Config) ->
    NewConfig = dynamic_modules:save_modules(host_type(), Config),
    escalus:init_per_suite(NewConfig).

init_per_group(extdisco_configured, Config) ->
    ExternalServices = [stun_service()],
    set_external_services(ExternalServices, Config);
init_per_group(multiple_extdisco_configured, Config) ->
    ExternalServices = [stun_service(), stun_service(), turn_service()],
    set_external_services(ExternalServices, Config);
init_per_group(extdisco_required_elements_configured, Config) ->
    ExternalServices = [#{type => ftp, host => <<"3.3.3.3">>}],
    set_external_services(ExternalServices, Config);
init_per_group(_GroupName, Config) ->
   Config.

init_per_testcase(external_services_discovery_not_supported = Name, Config) ->
    NewConfig = remove_external_services(Config),
    escalus:init_per_testcase(Name, NewConfig);
init_per_testcase(no_external_services_configured_no_services_returned = Name, Config) ->
    ExternalServices = [],
    NewConfig = set_external_services(ExternalServices, Config),
    escalus:init_per_testcase(Name, NewConfig);
init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(Name, Config) when
    Name == external_services_discovery_not_supported;
    Name == no_external_services_configured_no_services_returned ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_testcase(Name, Config);
end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

end_per_group(_GroupName, Config) ->
    dynamic_modules:restore_modules(Config),
    Config.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

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
           not escalus_pred:has_feature(?NS_EXTDISCO, Stanza)
           end, Result)
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

no_external_services_configured_no_services_returned(Config) ->
    % Given external service discovery is configured with empty list
    Test = fun(Alice) ->

        % When requesting for external services
        Iq = request_external_services(domain()),
        escalus_client:send(Alice, Iq),

        % Then services but no service element is in the iq result,
        % which means that empty services element got returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?NS_EXTDISCO)),
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
        escalus:assert(has_feature, [?NS_EXTDISCO], Result)
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
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?NS_EXTDISCO)),
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
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?NS_EXTDISCO)),
        [all_services_are_returned(Service, Type) || Service <- get_service_element(Result)]
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_no_matching_services_no_returned(Config) ->
    % Given external service discovery is configured
    Test = fun(Alice) ->

        % When requesting for external service of unknown or unconfigured type
        Type = <<"unknown_service">>,
        Iq = request_external_services_with_type(domain(), Type),
        escalus_client:send(Alice, Iq),

        % Then the iq_errror is returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Result)
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
        ?assertEqual(true, has_subelement_with_ns(Result, <<"credentials">>, ?NS_EXTDISCO)),
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
        Type = <<"unknown_service">>,
        Host = <<"unknown_host">>,
        Iq = request_external_services_credentials(domain(), Type, Host),
        escalus_client:send(Alice, Iq),

        % Then iq_error is retured
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Result)
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_no_matching_credentials_type_no_returned(Config) ->
    % Given external service discovery is configured with credentials
    Test = fun(Alice) ->

        % When requesting for credentials of external service without defining
        % the service type
        Host = <<"stun1">>,
        Iq = request_external_services_credentials_host_only(domain(), Host),
        escalus_client:send(Alice, Iq),

        % Then iq_error is retured
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Result)
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_services_configured_incorrect_request_no_returned(Config) ->
    % Given external service discovery is configured
    Test = fun(Alice) ->

        % When sending request with incorrect elements
        Iq = request_external_services_incorrect(domain()),
        escalus_client:send(Alice, Iq),

        % Then iq_error is returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Result)
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

external_service_required_elements_configured(Config) ->
    % Given external service discovery is configured only with required elements
    Test = fun(Alice) ->

        % When requesting for external services
        Iq = request_external_services(domain()),
        escalus_client:send(Alice, Iq),

        % Then list of external services with containing all
        % required_elements() is returned
        Result = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, [Iq], Result),
        ?assertEqual(true, has_subelement_with_ns(Result, <<"services">>, ?NS_EXTDISCO)),
        [required_services_are_returned(Service) || Service <- get_service_element(Result)]
    end,
    escalus:fresh_story(Config, [{alice, 1}], Test).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

stun_service() ->
    #{type => stun,
      host => <<"1.1.1.1">>,
      port => 3478,
      transport => <<"udp">>,
      username => <<"username">>,
      password => <<"secret">>}.

turn_service() ->
    #{type => turn,
      host => <<"2.2.2.2">>,
      port => 3478,
      transport => <<"tcp">>,
      username => <<"username">>,
      password => <<"secret">>}.

set_external_services(Services, Config) ->
    Module = [{mod_extdisco, #{iqdisc => no_queue, service => Services}}],
    ok = dynamic_modules:ensure_modules(host_type(), Module),
    Config.

remove_external_services(Config) ->
    dynamic_modules:ensure_stopped(host_type(), [mod_extdisco]),
    Config.

request_external_services(To) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"services">>,
                attrs = #{<<"xmlns">> => <<"urn:xmpp:extdisco:2">>}}]).

request_external_services_with_type(To, Type) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"services">>,
                attrs = #{<<"xmlns">> => <<"urn:xmpp:extdisco:2">>,
                          <<"type">> => Type}}]).

request_external_services_credentials(To, Type, Host) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"credentials">>,
                attrs = #{<<"xmlns">> => <<"urn:xmpp:extdisco:2">>},
                children = [#xmlel{name = <<"service">>,
                                   attrs = #{<<"host">> => Host,
                                             <<"type">> => Type}}]}]).

request_external_services_credentials_host_only(To, Host) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"credentials">>,
                attrs = #{<<"xmlns">> => <<"urn:xmpp:extdisco:2">>},
                children = [#xmlel{name = <<"service">>,
                                   attrs = #{<<"host">> => Host}}]}]).

request_external_services_incorrect(To) ->
    escalus_stanza:iq(To, <<"get">>,
        [#xmlel{name = <<"incorrect">>,
                attrs = #{<<"xmlns">> => <<"urn:xmpp:extdisco:2">>}}]).

get_service_element(Result) ->
    Services = exml_query:subelement_with_ns(Result, ?NS_EXTDISCO),
    exml_query:subelements(Services, <<"service">>).

required_elements() ->
    [<<"host">>, <<"type">>].

supported_elements() ->
    required_elements() ++ [<<"port">>, <<"username">>, <<"password">>].

all_services_are_returned(Service) ->
    [?assertEqual(true, has_subelement(Service, E)) || E <- supported_elements()].

required_services_are_returned(Service) ->
    [?assertEqual(true, has_subelement(Service, E)) || E <- required_elements()].

all_services_are_returned(Service, Type) ->
    ?assertEqual(true, has_attr_with_value(Service, <<"type">>, Type)),
    all_services_are_returned(Service).

no_services_are_returned(Service) ->
    [?assertEqual(false, has_subelement(Service, E)) || E <- supported_elements()].

has_subelement(Stanza, Element) ->
    undefined =/= exml_query:attr(Stanza, Element).

has_attr_with_value(Stanza, Element, Value) ->
    Value == exml_query:attr(Stanza, Element).

has_subelement_with_ns(Stanza, Element, NS) ->
    [] =/= exml_query:subelements_with_name_and_ns(Stanza, Element, NS).
