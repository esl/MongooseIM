%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
%%
%% Test the mod_vcard running on the server.
%%
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

-module(vcard_simple_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

%% Element CData
-define(EL(Element, Name), exml_query:path(Element, [{element, Name}])).
-define(EL_CD(Element, Name), exml_query:path(Element, [{element, Name}, cdata])).

-import(vcard_helper, [is_vcard_ldap/0]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             subhost_pattern/1,
                             rpc/4]).
-import(domain_helper, [host_type/0,
                        host_types/0,
                        domain/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, all}
    ].

groups() ->
    %% setting test data before tests is proving awkward so might as well use the
    %% data set in the update tests to test the rest.
    G = [{all, [sequence], all_tests()}
        ],
    ct_helper:repeat_all_until_all_ok(G).

all_tests() ->
    [update_own_card,
     retrieve_own_card,
     user_doesnt_exist,
     update_other_card,
     retrieve_others_card,
     request_search_fields,
     search_empty,
     search_some,
     search_wildcard].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = prepare_vcard_module(escalus:init_per_suite(Config)),
    configure_mod_vcard(Config1),
    escalus:create_users(Config1, escalus:get_users([alice, bob])).

end_per_suite(Config) ->
    NewConfig = escalus:delete_users(Config, escalus:get_users([alice, bob])),
    restore_vcard_module(NewConfig),
    escalus:end_per_suite(NewConfig).

init_per_group(_GN, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% XEP-0054: vcard-temp Test cases
%%--------------------------------------------------------------------

update_own_card(Config) ->
    case is_vcard_ldap() of
        true ->
            {skip,ldap_vcard_is_readonly};
        _ ->
            escalus:story(
              Config, [{alice, 1}],
              fun(Client1) ->
                      %% set some initial value different from the actual test data
                      %% so we know it really got updated and wasn't just old data
                      FN = get_FN(Config),
                      Client1Fields = [{<<"FN">>, FN}],
                      Client1SetResultStanza
                      = escalus:send_and_wait(Client1,
                                              escalus_stanza:vcard_update(Client1Fields)),
                      escalus:assert(is_iq_result, Client1SetResultStanza),
                      escalus_stanza:vcard_request(),
                      Client1GetResultStanza
                      = escalus:send_and_wait(Client1, escalus_stanza:vcard_request()),
                      FN
                      = stanza_get_vcard_field_cdata(Client1GetResultStanza, <<"FN">>)
              end)
    end.

retrieve_own_card(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request()),
              ClientVCardTups = [{<<"FN">>, get_FN(Config)}],
              check_vcard(ClientVCardTups, Res)
      end).



%% If no vCard exists, the server MUST return a stanza error
%% (which SHOULD be <item-not-found/>) or an IQ-result
%% containing an empty <vCard/> element.
%% We return <item-not-found/>
user_doesnt_exist(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              Domain = domain(),
              BadJID = <<"nonexistent@", Domain/binary>>,
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(BadJID)),
                case
                  escalus_pred:is_error(<<"cancel">>,
                                        <<"item-not-found">>,
                                        Res) of
                  true ->
                      ok;
                  _ ->
                      [] = Res#xmlel.children,
                      ct:comment("empty result instead of error")
              end
      end).

update_other_card(Config) ->
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Client, OtherClient) ->
              JID = escalus_client:short_jid(Client),
              Fields = [{<<"FN">>, <<"New name">>}],
              Res = escalus:send_and_wait(OtherClient,
                        escalus_stanza:vcard_update(JID, Fields)),

              %% check that nothing was changed
              Res2 = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request()),
              ClientVCardTups = [{<<"FN">>, get_FN(Config)}],
              check_vcard(ClientVCardTups, Res2),

              case escalus_pred:is_error(<<"cancel">>,
                                        <<"not-allowed">>, Res) of
                  true ->
                      ok;
                  _ ->
                        ct:comment("no error returned")
              end
      end).

retrieve_others_card(Config) ->
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Client, OtherClient) ->
              JID = escalus_client:short_jid(Client),
              Res = escalus:send_and_wait(OtherClient,
                        escalus_stanza:vcard_request(JID)),
              OtherClientVCardTups = [{<<"FN">>, get_FN(Config)}],
              check_vcard(OtherClientVCardTups, Res),

              %% In accordance with XMPP Core [5], a compliant server MUST
              %% respond on behalf of the requestor and not forward the IQ to
              %% the requestee's connected resource.

              Res2 = (catch escalus:wait_for_stanza(Client)),
              escalus:assert(stanza_timeout, Res2)
      end).

%%--------------------------------------------------------------------
%% XEP-0055 jabber:iq:search User Directory service Test cases
%%
%%--------------------------------------------------------------------

%% all.search.domain

request_search_fields(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              Domain = domain(),
              DirJID = <<"vjud.", Domain/binary>>,
              Res = escalus:send_and_wait(Client,
                                      escalus_stanza:search_fields_iq(DirJID)),
              escalus:assert(is_iq_result, Res),
              Result = ?EL(Res, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlel{ children = XChildren } = XData,
              FieldTups = field_tuples(XChildren),
              true = lists:member({<<"text-single">>,
                                   get_field_name(user), <<"User">>},
                                  FieldTups),
              true = lists:member({<<"text-single">>,
                                   get_field_name(fn),
                                   <<"Full Name">>},
                                  FieldTups)

      end).

search_empty(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              Domain = domain(),
              DirJID = <<"vjud.", Domain/binary>>,
              Fields = [{get_field_name(fn), <<"nobody">>}],
              Res = escalus:send_and_wait(Client,
                                escalus_stanza:search_iq(DirJID,
                                    escalus_stanza:search_fields(Fields))),
              escalus:assert(is_iq_result, Res),
              [] = search_result_item_tuples(Res)
      end).

search_some(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Client) ->
              Domain = domain(),
              DirJID = <<"vjud.", Domain/binary>>,
              Fields = [{get_field_name(fn), get_FN(Config)}],
              Res = escalus:send_and_wait(Client,
                                escalus_stanza:search_iq(DirJID,
                                    escalus_stanza:search_fields(Fields))),
              escalus:assert(is_iq_result, Res),

              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = search_result_item_tuples(Res),
              1 = length(ItemTups)
      end).

search_wildcard(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Client) ->
              Domain = domain(),
              DirJID = <<"vjud.", Domain/binary>>,
              Fields = [{get_field_name(fn), get_FN_wildcard()}],
              Res = escalus:send_and_wait(Client,
                              escalus_stanza:search_iq(DirJID,
                                  escalus_stanza:search_fields(Fields))),
              escalus:assert(is_iq_result, Res),
              ItemTups = search_result_item_tuples(Res),
              1 = length(ItemTups)
      end).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

expected_search_results(Key, Config) ->
    {_, ExpectedResults} =
    lists:keyfind(expected_results, 1,
                  escalus_config:get_config(search_data, Config)),
    lists:keyfind(Key, 1, ExpectedResults).

%%----------------------
%% xmlel shortcuts
stanza_get_vcard_field(Stanza, FieldName) ->
    VCard = ?EL(Stanza, <<"vCard">>),
    ?EL(VCard, FieldName).

stanza_get_vcard_field_cdata(Stanza, FieldName) ->
    VCard = ?EL(Stanza, <<"vCard">>),
    ?EL_CD(VCard, FieldName).

%%---------------------
%% test helpers

%%
%% -> [{Type, Var, Label}]
%%
field_tuples([]) ->
    [];
field_tuples([#xmlel{name = <<"field">>,
                     attrs=Attrs,
                     children=_Children} = El| Rest]) ->
    {<<"type">>,Type} = lists:keyfind(<<"type">>, 1, Attrs),
    {<<"var">>,Var} = lists:keyfind(<<"var">>, 1, Attrs),
    {<<"label">>,Label} = lists:keyfind(<<"label">>, 1, Attrs),
    case ?EL_CD(El, <<"value">>) of
        undefined ->
            [{Type, Var, Label}|field_tuples(Rest)];
        ValCData ->
            [{Type, Var, Label, ValCData}|field_tuples(Rest)]
    end;
field_tuples([_SomeOtherEl|Rest]) ->
    field_tuples(Rest).


%%
%%  -> [{Type, Var, Label, ValueCData}]
%%
%% This is naiive and expensive LOL!
item_field_tuples(_, []) ->
    [];
item_field_tuples(ReportedFieldTups,
                  [#xmlel{name = <<"field">>,
                          attrs=Attrs,
                          children=_Children} = El| Rest]) ->
    {<<"var">>,Var} = lists:keyfind(<<"var">>, 1, Attrs),
    {Type, Var, Label} = lists:keyfind(Var, 2, ReportedFieldTups),
    [{Type, Var, Label, ?EL_CD(El, <<"value">>)}
     | item_field_tuples(ReportedFieldTups, Rest)];

item_field_tuples(ReportedFieldTups, [_SomeOtherEl|Rest]) ->
    item_field_tuples(ReportedFieldTups, Rest).


%%
%% -> [{JID, [ItemFieldTups]}]
%%
%% Finds the JID and maps fields to their labels and types
%%
item_tuples(_, []) ->
    [];
item_tuples(ReportedFieldTups, [#xmlel{name = <<"item">>,
                                       children = Children} | Rest]) ->
    ItemFieldTups = item_field_tuples(ReportedFieldTups, Children),
    {_,_,_,JID} = lists:keyfind(<<"jid">>, 2, ItemFieldTups),
    [{JID, ItemFieldTups}|item_tuples(ReportedFieldTups, Rest)];
item_tuples(ReportedFieldTypes, [_SomeOtherChild | Rest]) ->
    item_tuples(ReportedFieldTypes, Rest).


%% This tests that at least the values in the ExpectedVCardTups are in the
%% VCardUnderTest.
%% Any extra values in the vcard are ignored by this function and should be
%% checked or rejected elsewhere.
%% crash means fail, return means success.
check_vcard(ExpectedVCardTups, Stanza) ->
    escalus_pred:is_iq(<<"result">>, Stanza),
    VCardUnderTest = ?EL(Stanza, <<"vCard">>),
    check_xml_element(ExpectedVCardTups, VCardUnderTest).


check_xml_element([], _ElUnderTest) ->
    ok;  %% just return true to be consistent with other clauses.
check_xml_element([{ExpdFieldName, ExpdChildren}|Rest], ElUnderTest)
  when is_list(ExpdChildren) ->
    check_xml_element(ExpdChildren, ?EL(ElUnderTest, ExpdFieldName)),
    check_xml_element(Rest, ElUnderTest);
check_xml_element([{ExpdFieldName, ExpdCData}|Rest], ElUnderTest) ->
    case ?EL_CD(ElUnderTest, ExpdFieldName) of
        ExpdCData ->
            check_xml_element(Rest, ElUnderTest);
        Else ->
            ct:fail("Expected ~p got ~p~n", [ExpdCData, Else])
    end.

%% Checks that the elements of two lists with matching keys are equal
%% while the order of the elements does not matter.
%% Returning means success. Crashing via ct:fail means failure.
%% Prints the lists in the ct:fail Result term.
list_unordered_key_match(Keypos, Expected, Actual) ->
    case length(Actual) of
        ActualLength when ActualLength == length(Expected) ->
            list_unordered_key_match2(Keypos, Expected, Actual);
        ActualLength ->
            ct:fail("Expected size ~p, actual size ~p~nExpected: ~p~nActual: ~p",
                    [length(Expected), ActualLength, Expected, Actual])
    end.

list_unordered_key_match2(_, [], _) ->
    ok;
list_unordered_key_match2(Keypos, [ExpctdTup|Rest], ActualTuples) ->
    Key = element(Keypos, ExpctdTup),
    ActualTup = lists:keyfind(Key, Keypos, ActualTuples),
    case ActualTup of
        ExpctdTup ->
            list_unordered_key_match2(Keypos, Rest, ActualTuples);
        _ ->
            ct:fail("~nExpected ~p~nGot ~p", [ExpctdTup, ActualTup])
    end.

search_result_item_tuples(Stanza) ->
    Result = ?EL(Stanza, <<"query">>),
    XData = ?EL(Result, <<"x">>),
    #xmlel{ attrs = _XAttrs,
            children = XChildren } = XData,
    Reported = ?EL(XData, <<"reported">>),
    ReportedFieldTups = field_tuples(Reported#xmlel.children),
    _ItemTups = item_tuples(ReportedFieldTups, XChildren).

get_field_name(fn)->
    case is_vcard_ldap() of
        true -> <<"cn">>;
        false -> <<"fn">>
    end;
get_field_name(user)->
    case is_vcard_ldap() of
        true -> <<"uid">>;
        false -> <<"user">>
    end.

get_FN_wildcard() ->
    case is_vcard_ldap() of
        true -> <<"*li*e">>;
        false -> <<"old*">>
    end.
get_FN(Config) ->
    case is_vcard_ldap() of
        true ->
            escalus_utils:jid_to_lower(escalus_users:get_username(Config, alice));
        false ->
            <<"Old Name">>
    end.

configure_mod_vcard(Config) ->
    HostType = ct:get_config({hosts, mim, host_type}),
    case is_vcard_ldap() of
        true ->
            ensure_started(HostType, ldap_opts());
        _ ->
            ensure_started(HostType, ?config(mod_vcard_opts, Config))
    end.

ldap_opts() ->
    LDAPOpts = #{filter => <<"(objectClass=inetOrgPerson)">>,
                 base => <<"ou=Users,dc=esl,dc=com">>,
                 search_fields => [{<<"Full Name">>, <<"cn">>}, {<<"User">>, <<"uid">>}],
                 vcard_map => [{<<"FN">>, <<"%s">>, [<<"cn">>]}]},
    LDAPOptsWithDefaults = config_parser_helper:config([modules, mod_vcard, ldap], LDAPOpts),
    config_parser_helper:mod_config(mod_vcard, #{backend => ldap, ldap => LDAPOptsWithDefaults}).

ensure_started(HostType, Opts) ->
    dynamic_modules:stop(HostType, mod_vcard),
    dynamic_modules:start(HostType, mod_vcard, Opts).

prepare_vcard_module(Config) ->
    %% Keep the old config, so we can undo our changes, once finished testing
    Config1 = dynamic_modules:save_modules(host_types(), Config),
    %% Get a list of options, we can use as a prototype to start new modules
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    VCardOpts = config_parser_helper:mod_config(mod_vcard, #{backend => Backend}),
    [{mod_vcard_opts, VCardOpts} | Config1].

restore_vcard_module(Config) ->
    dynamic_modules:restore_modules(Config).
