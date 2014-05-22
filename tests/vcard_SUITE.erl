%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
%%
%% Test the mod_vcard* modules.
%% mod_vcard uses mnesia
%% mod_vcard_ldap uses ldap
%% mod_vcard_odbc uses odbc
%%
%% They share many comonalities but sometimes behave differently or have
%% some extra or missing features. They also need different config depending
%% on which vhost they're running on.
%%
%%           -----
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

-module(vcard_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

%% Element CData
-define(EL(Element, Name), exml_query:path(Element, [{element, Name}])).
-define(EL_CD(Element, Name), exml_query:path(Element, [{element, Name}, cdata])).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, rw},
     {group, ro_full},
     {group, ro_limited},
     {group, ro_no}].

groups() ->
    %% setting test data before tests is proving awkward so might as well use the
    %% data set in the update tests to test the rest.
    [{rw, [sequence], rw_tests()},
     {ro_full, [sequence], ro_full_search_tests()},
     {ro_limited, [sequence], ro_limited_search_tests()},
     {ro_no, [sequence], ro_no_search_tests()}
    ].

rw_tests() ->
    [update_own_card].

ro_full_search_tests() ->
    [retrieve_own_card,
     user_doesnt_exist,
     update_other_card,
     retrieve_others_card,
     vcard_service_discovery,
     server_vcard,
     directory_service_vcard,
     request_search_fields,
     search_open,
     search_empty,
     search_some,
     search_wildcard].

ro_limited_search_tests() ->
    [search_open_limited,
     search_some_limited,
     search_in_service_discovery].

ro_no_search_tests() ->
    [search_not_allowed,
     search_not_in_service_discovery].

suite() ->
    [{require, vcard} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    NewConfig = escalus:init_per_suite(Config),
    NewConfig1 = dynamic_modules:stop_running(mod_vcard, NewConfig),
    [{ModVcard, _}] = ?config(running, NewConfig1),
    escalus:create_users([{mod_vcard, ModVcard} | NewConfig1]).

end_per_suite(Config) ->
    dynamic_modules:start_running(Config),
    NewConfig = escalus:delete_users(Config, {by_name, [alice, bob]}),
    escalus:end_per_suite(NewConfig).

init_per_group(rw, Config) ->
    restart_mod(?config(mod_vcard, Config), rw),
    Config;
init_per_group(GroupName, Config) ->
    restart_mod(?config(mod_vcard, Config), GroupName),
    prepare_vcards(Config).

end_per_group(_, Config) ->
    Domain = escalus_config:get_ct(
            {vcard, data, all_search, server_jid}),
    dynamic_modules:stop(Domain, ?config(mod_vcard, Config)),
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% XEP-0054: vcard-temp Test cases
%%--------------------------------------------------------------------

update_own_card(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Client1) ->
                %% set some initial value different from the actual test data
                %% so we know it really got updated and wasn't just old data
                Client1Fields = [{<<"FN">>, <<"Old name">>}],
                Client1SetResultStanza
                    = escalus:send_and_wait(Client1,
                                        escalus_stanza:vcard_update(Client1Fields)),
                escalus:assert(is_iq_result, Client1SetResultStanza),
                escalus_stanza:vcard_request(),
                Client1GetResultStanza
                    = escalus:send_and_wait(Client1, escalus_stanza:vcard_request()),
                <<"Old name">>
                    = stanza_get_vcard_field_cdata(Client1GetResultStanza, <<"FN">>)
        end).

retrieve_own_card(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request()),
              JID = escalus_client:short_jid(Client),
              ClientVCardTups =
                  escalus_config:get_ct(
                    {vcard, data, all_search, expected_vcards, JID}),
              check_vcard(ClientVCardTups, Res)
      end).



%% If no vCard exists or the user does not exist, the server MUST
%% return a stanza error, which SHOULD be either
%% <service-unavailable/> or <item-not-found/>
user_doesnt_exist(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              BadJID = escalus_config:get_ct(
                         {vcard, data, all_search, nonexistent_jid}),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(BadJID)),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Res)
      end).

update_other_card(Config) ->
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Client, OtherClient) ->
              JID = escalus_client:short_jid(Client),
              Fields = [{<<"FN">>, <<"New name">>}],
              Res = escalus:send_and_wait(OtherClient,
                        escalus_stanza:vcard_update(JID, Fields)),

              %% auth forbidden is also allowed
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"not-allowed">>], Res),

              %% check that nothing was changed
              Res2 = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request()),
              ClientVCardTups = escalus_config:get_ct(
                    {vcard, data, all_search, expected_vcards, JID}),
              check_vcard(ClientVCardTups, Res2)
      end).

retrieve_others_card(Config) ->
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Client, OtherClient) ->
              OtherJID = escalus_client:short_jid(OtherClient),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(OtherJID)),
              OtherClientVCardTups = escalus_config:get_ct(
                    {vcard, data, all_search, expected_vcards, OtherJID}),
              check_vcard(OtherClientVCardTups, Res),

              StreetMD5 = escalus_config:get_ct({vcard, common, utf8_street_md5}),
              ADR = stanza_get_vcard_field(Res, <<"ADR">>),
              StreetMD5 = crypto:md5(?EL_CD(ADR, <<"STREET">>)),

              %% In accordance with XMPP Core [5], a compliant server MUST
              %% respond on behalf of the requestor and not forward the IQ to
              %% the requestee's connected resource.

              Res2 = (catch escalus:wait_for_stanza(OtherClient)),
              escalus:assert(stanza_timeout, Res2)
      end).

server_vcard(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                          {vcard, data, all_search, server_jid}),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(ServJID)),
              ServerVCardTups =
                  escalus_config:get_ct(
                    {vcard, data, all_search, expected_vcards, ServJID}),
              check_vcard(ServerVCardTups, Res)
      end).

directory_service_vcard(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, data, all_search, directory_jid}),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(DirJID)),
              DirVCardTups =
                  escalus_config:get_ct(
                    {vcard, data, all_search, expected_vcards, DirJID}),
              check_vcard(DirVCardTups, Res)
      end).

vcard_service_discovery(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                          {vcard, data, all_search, server_jid}),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:disco_info(ServJID)),
              escalus:assert(is_iq_result, Res),
              escalus:assert(has_feature, [<<"vcard-temp">>], Res)
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
              DirJID = escalus_config:get_ct(
                         {vcard, data, all_search, directory_jid}),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_fields_iq(DirJID)),
              escalus:assert(is_iq_result, Res),
              Result = ?EL(Res, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlel{ children = XChildren } = XData,
              FieldTups = field_tuples(XChildren),
              true = lists:member({<<"text-single">>,
                                   <<"user">>, <<"User">>},
                                  FieldTups),
              true = lists:member({<<"text-single">>,
                                   <<"fn">>,
                                   <<"Full Name">>},
                                  FieldTups)
      end).

search_open(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, data, all_search, directory_jid}),
              Fields = [#xmlel{ name = <<"field">> }],
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_iq(DirJID, Fields)),
              escalus:assert(is_iq_result, Res),
              
              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = search_result_item_tuples(Res),
              ExpectedItemTups =
                  escalus_config:get_ct(
                    {vcard, data, all_search, search_results, open}),
              list_unordered_key_match(1, ExpectedItemTups, ItemTups)
      end).

search_empty(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, data, all_search, directory_jid}),
              Fields = [{<<"fn">>, <<"nobody">>}],
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
              DirJID = escalus_config:get_ct(
                         {vcard, data, all_search, directory_jid}),
              MoscowRUBin = escalus_config:get_ct({vcard, common, moscow_ru_utf8}),
              Fields = [{<<"locality">>, MoscowRUBin}],
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_iq(DirJID,
                            escalus_stanza:search_fields(Fields))),
              escalus:assert(is_iq_result, Res),

              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = search_result_item_tuples(Res),
              ExpectedItemTups =
                  escalus_config:get_ct(
                    {vcard, data, all_search, search_results, some}),
              list_unordered_key_match(1, ExpectedItemTups, ItemTups)
      end).

search_wildcard(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Client) ->
                DirJID = escalus_config:get_ct(
                        {vcard, data, all_search, directory_jid}),
                Fields = [{<<"fn">>, <<"doe*">>}],
                Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_iq(DirJID,
                            escalus_stanza:search_fields(Fields))),
                escalus:assert(is_iq_result, Res),
                ItemTups = search_result_item_tuples(Res),
                ExpectedItemTups =
                                   escalus_config:get_ct(
                        {vcard, data, all_search, search_results, wildcard}),
                list_unordered_key_match(1, ExpectedItemTups, ItemTups)
        end).



%%------------------------------------
%% @limited.search.domain

search_open_limited(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, data, ltd_search, directory_jid}),
              Fields = [null],
              Res = escalus:send_and_wait(Client,
                           escalus_stanza:search_iq(DirJID,
                               escalus_stanza:search_fields(Fields))),
              escalus:assert(is_iq_result, Res),
              %% {allow_return_all, false}
              [] = search_result_item_tuples(Res)
      end).

search_some_limited(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, data, ltd_search, directory_jid}),
              Server = escalus_client:server(Client),
              Fields = [{<<"last">>, <<"Doe">>}],
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_iq(DirJID,
                            escalus_stanza:search_fields(Fields))),
              escalus:assert(is_iq_result, Res),
              ItemTups = search_result_item_tuples(Res),
              %% exactly one result returned and its JID domain is correct
              [{SomeJID, _JIDsFields}] = ItemTups,
              {_Start, _Length} = binary:match(SomeJID, <<"@", Server/binary>>)
      end).

%% disco#items to limited.search.domain says directory.limited.search.domain exists
%% disco#info to directory.limited.search.domain says it has feature jabber:iq:search
%% and an <identity category='directory' type='user'/>
%%   http://xmpp.org/extensions/xep-0030.html#registrar-reg-identity
search_in_service_discovery(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                         {vcard, data, ltd_search, server_jid}),
              DirJID = escalus_config:get_ct(
                         {vcard, data, ltd_search, directory_jid}),

              %% Item
              ItemsRes = escalus:send_and_wait(Client,
                                escalus_stanza:disco_items(ServJID)),
              escalus:assert(is_iq_result, ItemsRes),
              escalus:assert(has_item, [DirJID], ItemsRes),

              %% Feature
              InfoRes = escalus:send_and_wait(Client,
                            escalus_stanza:disco_info(DirJID)),
              escalus:assert(is_iq_result, InfoRes),
              escalus:assert(has_feature, [?NS_SEARCH], InfoRes),

              %% Identity
              escalus:assert(has_identity, [<<"directory">>,
                                            <<"user">>], InfoRes)
      end).

%%------------------------------------
%% @no.search.domain

search_not_allowed(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, data, no_search, directory_jid}),
              Fields = [null],
              Res = escalus:send_and_wait(Client,
                           escalus_stanza:search_iq(DirJID, 
                               escalus_stanza:search_fields(Fields))),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Res)
      end).

%% disco#items to no.search.domain doesn't say vjud.no.search.domain exists
search_not_in_service_discovery(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                         {vcard, data, no_search, server_jid}),
              DirJID = escalus_config:get_ct(
                         {vcard, data, no_search, directory_jid}),
              %% Item
              ItemsRes = escalus:send_and_wait(Client,
                            escalus_stanza:disco_items(ServJID)),
              escalus:assert(is_iq_result, ItemsRes),
              escalus:assert(has_no_such_item, [DirJID], ItemsRes)
      end).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

expected_search_results(Key, Config) ->
    {_, ExpectedResults} =
        lists:keyfind(expected_results, 1,
                      escalus_config:get_config(search_data, Config)),
    lists:keyfind(Key, 1, ExpectedResults).

prepare_vcards(Config) ->
    AllVCards
        = escalus_config:get_ct({vcard, data, all_search, expected_vcards}),

    lists:foreach(
        fun({JID, Fields}) ->
                case binary:match(JID, <<"@">>) of
                    nomatch ->
                        ok;
                    _ ->
                        RJID = get_jid_record(JID),
                        ok = vcard_rpc(RJID,
                                   escalus_stanza:vcard_update(JID, Fields))
                end
        end, AllVCards),
    Config.

delete_vcards(Config) ->
     AllVCards
        = escalus_config:get_ct({vcard, data, all_search, expected_vcards}),

    lists:foreach(
        fun({JID, _}) ->
                case binary:match(JID, <<"@">>) of
                    nomatch ->
                        ok;
                    _ ->
                        RJID = get_jid_record(JID),
                        ok = vcard_rpc(RJID,
                                       escalus_stanza:vcard_update(JID, []))
                end
        end, AllVCards),
    Config.

get_jid_record(JID) ->
    [User, Server] = binary:split(JID, <<"@">>),
    {jid, User, Server, <<"">>, User, Server, <<"">>}.

vcard_rpc(JID, Stanza) ->
    escalus_ejabberd:rpc(ejabberd_sm, route, [JID, JID, Stanza]).

restart_mod(Module, rw) ->
    restart_mod(Module, params_all(), Module);
restart_mod(Module, ro_full) ->
    restart_mod(Module, params_all(), Module);
restart_mod(Module, ro_limited) ->
    restart_mod(Module, params_limited(), Module);
restart_mod(Module, ro_no) ->
    restart_mod(Module, params_no(), Module).

params_all() ->
    [{allow_return_all, true},
     {search_all_hosts, true}].

params_limited() ->
    [{matches, 1},
     {search_all_hosts, false},
     {allow_return_all, false},
     {host, "directory.@HOST@"}].

params_no() ->
    [{search, false}].

restart_mod(Mod, Params, undefined) ->
    Domain = escalus_config:get_ct(
            {vcard, data, all_search, server_jid}),
    dynamic_modules:stop(Domain, Mod),
    dynamic_modules:start(Domain, Mod, Params);
restart_mod(Mod, Params, NewMod) ->
    Domain = escalus_config:get_ct(
            {vcard, data, all_search, server_jid}),
    SecDomain = escalus_config:get_ct(
            {vcard, data, all_search, secondary_server_jid}),
    dynamic_modules:stop(Domain, Mod),
    dynamic_modules:stop(SecDomain, Mod),
    dynamic_modules:start(Domain, NewMod, Params),
    dynamic_modules:start(SecDomain, NewMod, Params).

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

