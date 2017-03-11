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

-define(PHOTO_BIN, <<130, 192, 33, 159, 204, 86, 12, 63, 132, 164>>).
-define(PHOTO_BASE_64, <<"gsAhn8xWDD+EpA==">>). %% jlib:encode_base64(?PHOTO_BIN)

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, rw},
     {group, ro_full},
     {group, ro_limited},
     {group, ro_no},
     {group, ldap_only}
     ].

groups() ->
    %% setting test data before tests is proving awkward so might as well use the
    %% data set in the update tests to test the rest.
    [{rw, [sequence], rw_tests()},
     {ro_full, [], ro_full_search_tests()},
     {ro_limited, [], ro_limited_search_tests()},
     {ro_no, [sequence], ro_no_search_tests()},
     {ldap_only, [], ldap_only_tests()}
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
     search_some_many_fields,
     search_wildcard
    ].

ro_limited_search_tests() ->
    [search_open_limited,
     search_some_limited,
     search_in_service_discovery].

ro_no_search_tests() ->
    [search_not_allowed,
     search_not_in_service_discovery].

ldap_only_tests() ->
    [search_some_many_fields_with_or_operator,
     return_photo_inserted_as_binary_by_3rd_party_service].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    NewConfig = escalus:init_per_suite(Config),
    NewConfig1 = vcard_config(NewConfig),
    NewConfig2 = stop_running_vcard_mod(NewConfig1),
    AliceAndBob = escalus_users:get_users([alice, bob]),
    Domain = domain(),
    BisUsers = [{aliceb, [{username, <<"aliceb">>},
                          {server, <<Domain/binary, ".bis">>},
                          {host, Domain},
                          {password, <<"makota">>}]},
                {bobb, [{username, <<"bobb">>},
                        {server, <<Domain/binary, ".bis">>},
                        {host, Domain},
                        {password, <<"makrolika">>}]}],
    NewUsers = AliceAndBob ++ BisUsers,
    escalus:create_users([{escalus_users, NewUsers} | NewConfig2], NewUsers).

vcard_config(Config) ->
    Domain = domain(),
    [{all_vcards, get_all_vcards()},
     {server_vcards, get_server_vcards()},
     {directory_jid, <<"vjud.", Domain/binary>>}
    ] ++ Config.

end_per_suite(Config) ->
    start_running_vcard_mod(Config),
    Who = ?config(escalus_users, Config),
    NewConfig = escalus:delete_users(Config, Who),
    escalus:end_per_suite(NewConfig).

init_per_group(rw, Config) ->
    restart_vcard_mod(Config, rw),
    Config;
init_per_group(ldap_only, Config) ->
    VCardConfig = ?config(mod_vcard, Config),
    case proplists:get_value(backend, VCardConfig) of
        ldap ->
            Config1 = restart_and_prepare_vcard(ldap_only, Config),
            insert_alice_photo(Config1);
            %Config1;
        _ ->
            {skip, "this group is only for ldap vCard backend"}
    end;
init_per_group(GroupName, Config) ->
    restart_and_prepare_vcard(GroupName, Config).

end_per_group(_, Config) ->
    stop_vcard_mod(Config),
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

restart_and_prepare_vcard(GroupName, Config) ->
    restart_vcard_mod(Config, GroupName),
    prepare_vcards(Config).

%%--------------------------------------------------------------------
%% XEP-0054: vcard-temp Test cases
%%--------------------------------------------------------------------

update_own_card(Config) ->
    case vcard_simple_SUITE:is_vcard_ldap() of
        true ->
            {skip, ldap_vcard_is_readonly};
        _ ->
            do_update_own_card(Config)
    end.

do_update_own_card(Config) ->
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
              JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
              ClientVCardTups = get_user_vcard(JID, Config),
              check_vcard(ClientVCardTups, Res)
      end).



%% If no vCard exists or the user does not exist, the server MUST
%% return a stanza error, which SHOULD be either
%% <service-unavailable/> or <item-not-found/>
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
              <<"service-unavailable">>,
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
              JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
              Fields = [{<<"FN">>, <<"New name">>}],
              Res = escalus:send_and_wait(OtherClient,
                        escalus_stanza:vcard_update(JID, Fields)),

              %% auth forbidden is also allowed
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"not-allowed">>], Res),

              %% check that nothing was changed
              Res2 = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request()),
              ClientVCardTups = get_user_vcard(JID, Config),
              check_vcard(ClientVCardTups, Res2)
      end).

retrieve_others_card(Config) ->
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Client, OtherClient) ->
              OtherJID = escalus_utils:jid_to_lower(escalus_client:short_jid(OtherClient)),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(OtherJID)),
              OtherClientVCardTups = get_user_vcard(OtherJID, Config),
              check_vcard(OtherClientVCardTups, Res),

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
              ServJID = ct:get_config({hosts, mim, domain}),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(ServJID)),
              ServerVCardTups = get_server_vcard(ServJID, Config),
              check_vcard(ServerVCardTups, Res)
      end).

directory_service_vcard(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = ?config(directory_jid, Config),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:vcard_request(DirJID)),
              DirVCardTups = get_server_vcard(DirJID, Config),
              check_vcard(DirVCardTups, Res)
      end).

vcard_service_discovery(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
          ServJID = ct:get_config({hosts, mim, domain}),
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
              DirJID = ?config(directory_jid, Config),
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_fields_iq(DirJID)),
              escalus:assert(is_iq_result, Res),
              Result = ?EL(Res, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlel{ children = XChildren } = XData,
              FieldTups = field_tuples(XChildren),
              true = lists:member({<<"text-single">>,
                                   get_user_search_field(), <<"User">>},
                                  FieldTups),
              true = lists:member({<<"text-single">>,
                                   get_full_name_search_field(),
                                   <<"Full Name">>},
                                  FieldTups)
      end).

search_open(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = ?config(directory_jid, Config),
              Fields = [#xmlel{ name = <<"field">> }],
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_iq(DirJID, Fields)),
              escalus:assert(is_iq_result, Res),

              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = search_result_item_tuples(Res),
              ExpectedItemTups = [],
              list_unordered_key_match(ExpectedItemTups, ItemTups)
      end).

search_empty(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              DirJID = ?config(directory_jid, Config),
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
              DirJID = ?config(directory_jid, Config),
              MoscowRUBin = get_utf8_city(),
              Fields = [{get_locality_search_field(), MoscowRUBin}],
              Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_iq(DirJID,
                            escalus_stanza:search_fields(Fields))),
              escalus:assert(is_iq_result, Res),

              %% Basically test that the right values exist
              %% and map to the right column headings
              Domain = ct:get_config({hosts, mim, domain}),
              AliceJID = <<"alice@", Domain/binary>>,

              [{AliceJID, ItemTups}] = search_result_item_tuples(Res),

              {_, _, <<"City">>, MoscowRUBin} = lists:keyfind(<<"City">>, 3, ItemTups)
      end).

search_wildcard(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Client) ->
                Domain = ct:get_config({hosts, mim, secondary_domain}),
                DirJID = <<"vjud.", Domain/binary>>,
                Fields = [{get_full_name_search_field(),
                           <<"Doe*">>}],
                Res = escalus:send_and_wait(Client,
                        escalus_stanza:search_iq(DirJID,
                            escalus_stanza:search_fields(Fields))),
                escalus:assert(is_iq_result, Res),

                ItemTups = search_result_item_tuples(Res),
                ExpectedItemTups = get_search_results(Config,
                                                      [<<"bobb@", Domain/binary>>,
                                                       <<"aliceb@", Domain/binary>>]),
                case vcard_simple_SUITE:is_vcard_ldap() of
                    true ->
                        3 = length(ItemTups);
                    _ ->
                        list_unordered_key_match2(ExpectedItemTups, ItemTups)
                end
        end).

search_some_many_fields(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              {Domain, ItemTups} = search_vcard_by_many_fields(Client),
              SomeJID = <<"aliceb@", Domain/binary>>,
              [{SomeJID, _JIDsFields}] = ItemTups
              %{_Start, _Length} = binary:match(SomeJID, <<"@", Server/binary>>)
      end).

search_vcard_by_many_fields(Client) ->
    Domain = ct:get_config({hosts, mim, secondary_domain}),
    DirJID = <<"vjud.", Domain/binary>>,

    Fields = [{get_first_name_search_field(), <<"Alice">>},
              {get_last_name_search_field(), <<"Doe">>}],
    Stanza = escalus_stanza:search_iq(DirJID,
                                      escalus_stanza:search_fields(Fields)),
    Res = escalus:send_and_wait(Client, Stanza),
    escalus:assert(is_iq_result, Res),
    ItemTups = search_result_item_tuples(Res),
    %% exactly one result returned and its JID domain is correct
    {Domain, ItemTups}.

search_some_many_fields_with_or_operator(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Client) ->
        {_Domain, ItemTups} = search_vcard_by_many_fields(Client),
        3 = length(ItemTups)
    end).

return_photo_inserted_as_binary_by_3rd_party_service(Config) ->
    %% The PHOTO was inserted by script and not by modifing vCard by the client
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Res = escalus:send_and_wait(Alice,
                                    escalus_stanza:vcard_request()),
        JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),

        Photo = exml_query:path(Res, [{element, <<"vCard">>},
                                      {element, <<"PHOTO">>},
                                      {element, <<"BINVAL">>},
                                      cdata]),

        ?PHOTO_BASE_64 = Photo,

        DirJID = ?config(directory_jid, Config),
        Fields = [{<<"mail">>, <<"alice@mail.example.com">>}],
        Res2 = escalus:send_and_wait(Alice,
                                    escalus_stanza:search_iq(DirJID,
                                                             escalus_stanza:search_fields(Fields))),
        [{JID, ItemTups}] = search_result_item_tuples(Res2),

        {_, <<"PHOTO">>, _, ?PHOTO_BASE_64} = lists:keyfind(<<"PHOTO">>, 2, ItemTups)

    end).

%%------------------------------------
%% @limited.search.domain

search_open_limited(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Client) ->
              Server = ct:get_config({hosts, mim, domain}),
              DirJID = <<"directory.", Server/binary>>,
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
              Domain = ct:get_config({hosts, mim, secondary_domain}),
              DirJID = <<"directory.", Domain/binary>>,
              Server = escalus_client:server(Client),
              Fields = [{get_last_name_search_field(), <<"Doe">>}],
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
              ServJID = ct:get_config({hosts, mim, domain}),
              DirJID = <<"directory.", ServJID/binary>>,

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
              DirJID = ?config(directory_jid, Config),
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
              ServJID = ct:get_config({hosts, mim, domain}),
              DirJID = ?config(directory_jid, Config),
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
    AllVCards = ?config(all_vcards, Config),
    ModVcardBackend = case lists:keyfind(backend, 1, ?config(mod_vcard, Config)) of
                          {backend, Backend} ->
                              Backend;
                          _ ->
                              mnesia
                      end,
    lists:foreach(
        fun({JID, Fields}) ->
                case binary:match(JID, <<"@">>) of
                    nomatch ->
                        ok;
                    _ ->
                        prepare_vcard(ModVcardBackend, JID, Fields)
                end
        end, AllVCards),
    timer:sleep(timer:seconds(3)), %give some time to Yokozuna to index vcards
    Config.

prepare_vcard(ldap, JID, Fields) ->
    [User, Server] = binary:split(JID, <<"@">>),
    {EPid, Base} = get_ldap_pid_and_base(Server),
    VCardMap = [{<<"NICKNAME">>, <<"%u">>, []},
                {<<"FN">>, <<"%s">>, [<<"displayName">>]},
                {<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
                {<<"GIVEN">>, <<"%s">>, [<<"givenName">>]},
                {<<"MIDDLE">>, <<"%s">>, [<<"initials">>]},
                {<<"ORGNAME">>, <<"%s">>, [<<"o">>]},
                {<<"ORGUNIT">>, <<"%s">>, [<<"ou">>]},

                %{<<"CTRY">>, <<"%s">>, [<<"c">>]},
                {<<"LOCALITY">>, <<"%s">>, [<<"l">>]},
                {<<"STREET">>, <<"%s">>, [<<"street">>]},
                {<<"REGION">>, <<"%s">>, [<<"st">>]},

                {<<"PCODE">>, <<"%s">>, [<<"postalCode">>]},
                {<<"TITLE">>, <<"%s">>, [<<"title">>]},
                {<<"URL">>, <<"%s">>, [<<"labeleduri">>]},
                {<<"DESC">>, <<"%s">>, [<<"description">>]},
                {<<"TEL">>, <<"%s">>, [<<"telephoneNumber">>]},
                {<<"NUMBER">>, <<"%s">>, [<<"telephoneNumber">>]},

                {<<"EMAIL">>, <<"%s">>, [<<"mail">>]},
                {<<"USERID">>, <<"%s">>, [<<"mail">>]},
%%                 {<<"BDAY">>, <<"%s">>, [<<"birthDay">>]}, %OpenLDAP doesn't sport it by default
                {<<"ROLE">>, <<"%s">>, [<<"employeeType">>]},
                {<<"PHOTO">>, <<"%s">>, [<<"jpegPhoto">>]}
    ],
    Fun = fun(Field, Val) ->
        case vcard_field_to_ldap(VCardMap, Field) of
            undefined ->
                undefined;
            LdapField ->
                escalus_ejabberd:rpc(eldap, mod_replace, [LdapField, [Val]])
        end
    end,
    Modificators = convert_vcard_fields(Fields, [], Fun),
    Dn = <<"cn=", User/binary, ",", Base/binary>>,
    ok = escalus_ejabberd:rpc(eldap, modify, [EPid, Dn, Modificators]);
prepare_vcard(_, JID, Fields) ->
    RJID = get_jid_record(JID),
    VCard = escalus_stanza:vcard_update(JID, Fields),
    vcard_rpc(RJID, VCard).

insert_alice_photo(Config) ->
    User = <<"alice">>,
    Server = domain(),
    {EPid, Base} = get_ldap_pid_and_base(Server),
    Photo = ?PHOTO_BIN,
    Modificators = [escalus_ejabberd:rpc(eldap, mod_replace, [<<"jpegPhoto">>, [Photo]])],
    Dn = <<"cn=", User/binary, ",", Base/binary>>,
    ok = escalus_ejabberd:rpc(eldap, modify, [EPid, Dn, Modificators]),
    Config.


fields_to_ldap_modificators(_, [], Acc) ->
    Acc;
fields_to_ldap_modificators(VcardMap, [{Field, Val} | Rest], Acc) when is_binary(Val) ->
    case vcard_field_to_ldap(VcardMap, Field) of
        undefined ->
            NewAcc = Acc;
        LdapField ->
            LdapModify = escalus_ejabberd:rpc(eldap, mod_replace, [LdapField, [Val]]),
            NewAcc = [LdapModify | Acc]
    end,
    fields_to_ldap_modificators(VcardMap, Rest, NewAcc);
fields_to_ldap_modificators(VcardMap, [{_, Children} | Rest], Acc) when is_list(Children) ->
    NewAcc = fields_to_ldap_modificators(VcardMap, Children, Acc),
    fields_to_ldap_modificators(VcardMap, Rest, NewAcc).

vcard_field_to_ldap(Map, Field) ->
    case lists:keyfind(Field, 1, Map) of
        {Field, _, [LdapField]} ->
            LdapField;
        _ ->
            undefined
    end.

get_ldap_pid_and_base(Server) ->
    {ok, State} = escalus_ejabberd:rpc(eldap_utils, get_state,
        [Server, ejabberd_mod_vcard_ldap]),
    EldapId = element(4, State),
    PoolId = binary_to_atom(<<"eldap_pool_", EldapId/binary>>, utf8),
    Pid = escalus_ejabberd:rpc(pg2, get_closest_pid, [PoolId]),
    Base = element(10, State),
    {Pid, Base}.

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

restart_vcard_mod(Config, ro_limited) ->
    restart_mod(params_limited(Config));
restart_vcard_mod(Config, ro_no) ->
    restart_mod(params_no(Config));
restart_vcard_mod(Config, ldap_only) ->
    restart_mod(params_ldap_only(Config));
restart_vcard_mod(Config, _GN) ->
    restart_mod(params_all(Config)).

start_running_vcard_mod(Config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    OriginalVcardConfig = ?config(mod_vcard, Config),
    dynamic_modules:start(Domain, mod_vcard, OriginalVcardConfig).
stop_running_vcard_mod(Config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    CurrentConfigs = escalus_ejabberd:rpc(gen_mod, loaded_modules_with_opts, [Domain]),
    {mod_vcard, CurrentVcardConfig} = lists:keyfind(mod_vcard, 1, CurrentConfigs),
    dynamic_modules:stop(Domain, mod_vcard),
    [{mod_vcard, CurrentVcardConfig} | Config].

stop_vcard_mod(Config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Domain, mod_vcard).

params_all(Config) ->
    add_backend_param([], ?config(mod_vcard, Config)).

params_limited(Config) ->
    add_backend_param([{matches, 1},
                       {host, "directory.@HOST@"}], ?config(mod_vcard, Config)).

params_no(Config) ->
    add_backend_param([{search, false}], ?config(mod_vcard, Config)).

params_ldap_only(Config) ->
    Reported = [{<<"Full Name">>, <<"FN">>},
   {<<"Given Name">>, <<"FIRST">>},
   {<<"Middle Name">>, <<"MIDDLE">>},
   {<<"Family Name">>, <<"LAST">>},
   {<<"Nickname">>, <<"NICK">>},
   {<<"Birthday">>, <<"BDAY">>},
   {<<"Country">>, <<"CTRY">>},
   {<<"City">>, <<"LOCALITY">>},
   {<<"Email">>, <<"EMAIL">>},
   {<<"Organization Name">>, <<"ORGNAME">>},
   {<<"Organization Unit">>, <<"ORGUNIT">>},
     {<<"Photo">>, <<"PHOTO">>}],
    add_backend_param([{ldap_search_operator, 'or'},
                       {ldap_binary_search_fields, [<<"PHOTO">>]},
                       {ldap_search_reported, Reported}],
                      ?config(mod_vcard, Config)).

add_backend_param(Opts, CurrentVCardConfig) ->
    F = fun({Key, _} = Item, Cfg) ->
        lists:keystore(Key, 1, Cfg, Item)
    end,
    lists:foldl(F, CurrentVCardConfig, Opts).


restart_mod(Params) ->
    Domain = ct:get_config({hosts, mim, domain}),
    SecDomain = ct:get_config({hosts, mim, secondary_domain}),
    dynamic_modules:stop(Domain, mod_vcard),
    dynamic_modules:stop(SecDomain, mod_vcard),
    {ok, _Pid} = dynamic_modules:start(Domain, mod_vcard, Params),
    {ok, _Pid2} = dynamic_modules:start(SecDomain, mod_vcard, Params).

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
    {<<"type">>, Type} = lists:keyfind(<<"type">>, 1, Attrs),
    {<<"var">>, Var} = lists:keyfind(<<"var">>, 1, Attrs),
    {<<"label">>, Label} = lists:keyfind(<<"label">>, 1, Attrs),
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
    {<<"var">>, Var} = lists:keyfind(<<"var">>, 1, Attrs),
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
    {_, _, _, JID} = lists:keyfind(<<"jid">>, 2, ItemFieldTups),
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
list_unordered_key_match(Expected, Actual) ->
    case length(Actual) of
        ActualLength when ActualLength == length(Expected) ->
            list_unordered_key_match2(Expected, Actual);
        ActualLength ->
            ct:fail("Expected size ~p, actual size ~p~nExpected: ~p~nActual: ~p",
                    [length(Expected), ActualLength, Expected, Actual])
    end.

list_unordered_key_match2([], _) ->
    ok;
list_unordered_key_match2([{User, ExpectedTup} | Rest], ActualTuples) ->

    case lists:keyfind(User, 1, ActualTuples) of
        {User, ReceivedTuple} ->
            verify_tuples(ReceivedTuple, ExpectedTup);
        _ ->
            ct:fail("can't find user ~p in received results: ~p",
                    [User, ActualTuples])

    end.

verify_tuples(Received, Expected) ->
    Fun = fun({_, _, Name, Value} = ExpectedItem) ->
        case lists:keyfind(Name, 3, Received) of
            {_, _, Name, Value} ->
                true;
            _ ->
                ct:fail("can't find item ~p in received items:~p", [ExpectedItem, Received])
        end
    end,
    lists:all(Fun, Expected).


search_result_item_tuples(Stanza) ->
    Result = ?EL(Stanza, <<"query">>),
    XData = ?EL(Result, <<"x">>),
    #xmlel{ attrs = _XAttrs,
                 children = XChildren } = XData,
    Reported = ?EL(XData, <<"reported">>),
    ReportedFieldTups = field_tuples(Reported#xmlel.children),
    _ItemTups = item_tuples(ReportedFieldTups, XChildren).

get_all_vcards() ->
    Domain = domain(),
    [{<<"alice@", Domain/binary>>,
      [{<<"NICKNAME">>, <<"alice">>},
       {<<"FN">>, <<"Wonderland, Alice">>},
       {<<"TITLE">>, <<"Executive Director">>},
       {<<"ROLE">>, <<"Patron Saint">>},
       {<<"DESC">>, <<"active">>},
       {<<"URL">>, <<"http://john.doe/">>},
       %{<<"PHOTO">>, crypto:rand_bytes(10)},
       {<<"EMAIL">>,
        [{<<"USERID">>, <<"alice@mail.example.com">>}]},
       {<<"N">>,
        [{<<"FAMILY">>, <<"Wonderland">>},
         {<<"GIVEN">>, <<"Alice">>},
         {<<"MIDDLE">>, <<"I.N.">>}]},
       {<<"ADR">>,
        [{<<"STREET">>, <<"1899 Wynkoop Street">>},
         {<<"LOCALITY">>, get_utf8_city()},
         {<<"REGION">>, <<"CO">>},
         {<<"PCODE">>, <<"91210">>}
        ] ++ maybe_add_ctry()},
       {<<"TEL">>,
        [{<<"NUMBER">>, <<"+1 512 305 0280">>}]},
       {<<"ORG">>,
        [{<<"ORGNAME">>, <<"The world">>},
         {<<"ORGUNIT">>, <<"People">>}]}
      ] ++ maybe_add_bday() ++ maybe_add_jabberd_id(<<"alice@", Domain/binary>>)},
     {<<"bob@", Domain/binary>>,
      [{<<"NICKNAME">>, <<"bob">>},
       {<<"FN">>, <<"Doe, Bob">>},
       {<<"ADR">>, [{<<"STREET">>,
           <<208, 146, 32, 208, 161, 208, 190, 208, 178,
           208, 181, 209, 130, 209, 129, 208, 186, 208, 190, 208, 185, 32,
           208, 160, 208, 190, 209, 129, 209, 129, 208, 184, 208, 184, 44,
           32, 208, 180, 208, 190, 209, 128, 208, 190, 208, 179, 208, 176,
           32, 209, 128, 208, 176, 208, 183, 208, 178, 208, 181, 209, 130,
           208, 178, 208, 187, 209, 143, 208, 181, 209, 130, 209, 129, 209,
           143, 32, 208, 178, 209, 139>>}]}
      ] ++ maybe_add_jabberd_id(<<"bob@", Domain/binary>>)},
     {<<"bobb@", Domain/binary, ".bis">>,
      [{<<"NICKNAME">>, <<"bobb">>},
       {<<"FN">>, <<"Doe, Bob">>},
       {<<"N">>,
        [{<<"FAMILY">>, <<"Doe">>},
         {<<"GIVEN">>, <<"Bob">>}]}
      ] ++ maybe_add_jabberd_id(<<"bobb@", Domain/binary, ".bis">>)},
     {<<"aliceb@", Domain/binary, ".bis">>,
      [{<<"NICKNAME">>, <<"aliceb">>},
       {<<"FN">>, <<"Doe, Alice">>},
       {<<"N">>,
        [{<<"FAMILY">>, <<"Doe">>},
         {<<"GIVEN">>, <<"Alice">>}]}
      ] ++ maybe_add_jabberd_id(<<"aliceb@", Domain/binary, ".bis">>)}
    ].

maybe_add_ctry() ->
    maybe_add([{<<"CTRY">>, <<"PL">>}]).

maybe_add_bday() ->
    maybe_add([{<<"BDAY">>, <<"2011-08-06">>}]).

maybe_add_jabberd_id(JabberId) ->
    maybe_add([{<<"JABBERID">>, JabberId}]).

maybe_add(Elems) ->
    case vcard_simple_SUITE:is_vcard_ldap() of
        true ->
            [];
        _ ->
            Elems
    end.

get_server_vcards() ->
    [{domain(),
      [{<<"FN">>, <<"MongooseIM">>},
       {<<"DESC">>, <<"MongooseIM XMPP Server\nCopyright (c) Erlang Solutions Ltd.">>}]},
     {<<"vjud.localhost">>,
      [{<<"FN">>, <<"MongooseIM/mod_vcard">>},
       {<<"DESC">>, <<"MongooseIM vCard module\nCopyright (c) Erlang Solutions Ltd.">>}]}].


get_user_vcard(JID, Config) ->
    {JID, ClientVCardTups} = lists:keyfind(JID, 1, ?config(all_vcards, Config)),
    ClientVCardTups.

get_server_vcard(ServerJid, Config) ->
    {ServerJid, VCard} = lists:keyfind(ServerJid, 1, ?config(server_vcards, Config)),
    VCard.

get_search_results(Config, Users) ->
    [{User, get_search_result(get_user_vcard(User, Config))} || User <- Users].

get_search_result(VCard) ->
    convert_vcard_fields(VCard, [], fun vcard_field_to_result/2).

get_locality_search_field() ->
    get_search_field(<<"locality">>, <<"l">>).

get_user_search_field() ->
    get_search_field(<<"user">>, <<"%u">>).

get_full_name_search_field() ->
    get_search_field(<<"fn">>, <<"displayName">>).

get_last_name_search_field() ->
    get_search_field(<<"last">>, <<"sn">>).

get_first_name_search_field() ->
    get_search_field(<<"first">>, <<"givenName">>).

get_search_field(Default, LDAP) ->
    case vcard_simple_SUITE:is_vcard_ldap() of
        true ->
            LDAP;
        _ ->
            Default
    end.

convert_vcard_fields([], Acc, _) -> Acc;
convert_vcard_fields([{_Field, Children} | Rest], Acc, Fun) when is_list(Children) ->
    NewAcc = convert_vcard_fields(Children, Acc, Fun),
    convert_vcard_fields(Rest, NewAcc, Fun);
convert_vcard_fields([{Field, Value} | Rest], Acc, Fun) ->
    NewAcc = case Fun(Field, Value) of
                 undefined ->
                     Acc;
                 Result ->
                     [Result | Acc]
    end,
    convert_vcard_fields(Rest, NewAcc, Fun).


vcard_field_to_result(Field, Value) ->
    case vcard_result_mapping(Field) of
        undefined ->
            undefined;
        Short ->
            case lists:keyfind(Short, 2, reported_fields()) of
                {Type, Short, Long} ->
                    {Type, Short, Long, Value};
                _ ->
                    undefined
            end
    end.

reported_fields() ->
    [{<<"text-single">>, <<"fn">>, <<"Full Name">>},
     {<<"text-single">>, <<"last">>, <<"Family Name">>},
     {<<"text-single">>, <<"first">>, <<"Name">>},
     {<<"text-single">>, <<"middle">>, <<"Middle Name">>},
     {<<"text-single">>, <<"nick">>, <<"Nickname">>},
     {<<"text-single">>, <<"bday">>, <<"Birthday">>},
     {<<"text-single">>, <<"ctry">>, <<"Country">>},
     {<<"text-single">>, <<"locality">>, <<"City">>},
     {<<"text-single">>, <<"email">>, <<"Email">>},
     {<<"text-single">>, <<"orgname">>, <<"Organization Name">>},
     {<<"text-single">>, <<"orgunit">>, <<"Organization Unit">>}
    ] ++ maybe_add_jabberd_field().

maybe_add_jabberd_field() ->
    case vcard_simple_SUITE:is_vcard_ldap() of
        true ->
            [];
        _ ->
            [{<<"jid-single">>, <<"jid">>, <<"Jabber ID">>}]
    end.


vcard_result_mapping(<<"NICKNAME">>) -> <<"nick">>;
vcard_result_mapping(<<"FN">>) -> <<"fn">>;
vcard_result_mapping(<<"USERID">>) -> <<"email">>;
vcard_result_mapping(<<"FAMILY">>) -> <<"last">>;
vcard_result_mapping(<<"GIVEN">>) -> <<"first">>;
vcard_result_mapping(<<"MIDDLE">>) -> <<"middle">>;
vcard_result_mapping(<<"LOCALITY">>) -> <<"locality">>;
vcard_result_mapping(<<"CTRY">>) -> <<"ctry">>;
vcard_result_mapping(<<"ORGNAME">>) -> <<"orgname">>;
vcard_result_mapping(<<"ORGUNIT">>) -> <<"orgunit">>;
vcard_result_mapping(<<"JABBERID">>) -> <<"jid">>;
vcard_result_mapping(<<"BDAY">>) -> <<"bday">>;
vcard_result_mapping(_) -> undefined.

get_utf8_city() ->
    %% This is the UTF-8 of Москва
    <<208, 156, 208, 190, 209, 129, 208, 186, 208, 178, 208, 176>>.

domain() ->
    ct:get_config({hosts, mim, domain}).
