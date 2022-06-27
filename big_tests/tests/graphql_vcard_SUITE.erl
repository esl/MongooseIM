-module(graphql_vcard_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("../../include/mod_roster.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_vcard}, {group, admin_vcard}].

groups() ->
    [{user_vcard, [], user_vcard_handler()},
     {admin_vcard, [], admin_vcard_handler()}].

user_vcard_handler() ->
    [user_set_vcard,
     user_get_their_vcard,
     user_get_their_vcard_no_vcard,
     user_get_others_vcard,
     user_get_others_vcard_no_user,
     user_get_others_vcard_no_vcard].

admin_vcard_handler() ->
    [admin_set_vcard,
     admin_set_vcard_incomplete_fields,
     admin_set_vcard_no_user,
     admin_get_vcard,
     admin_get_vcard_no_vcard,
     admin_get_vcard_no_user].

init_per_suite(Config) ->
    case vcard_helper:is_vcard_ldap() of
        true ->
            {skip, ldap_vcard_is_not_supported};
        _ ->
            Config2 = escalus:init_per_suite(Config),
            dynamic_modules:save_modules(domain_helper:host_type(), Config2)
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_vcard, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(user_vcard, Config) ->
    [{schema_endpoint, user} | Config].

end_per_group(admin_vcard, _Config) ->
    escalus_fresh:clean();
end_per_group(user_vcard, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

% User test cases

user_set_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_set_vcard/2).

user_set_vcard(Config, Alice) ->
    Vcard = complete_vcard_input(),
    BodySet = set_vcard_body_user(Vcard),
    GraphQlRequestSet = execute_user(BodySet, Alice, Config),
    ParsedResultSet = ok_result(<<"vcard">>, <<"setVcard">>, GraphQlRequestSet),
    ?assertEqual(Vcard, ParsedResultSet),
    QueryGet = user_get_full_vcard_as_result_query(),
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => #{}},
    GraphQlRequestGet = execute_user(BodyGet, Alice, Config),
    ParsedResultGet = ok_result(<<"vcard">>, <<"getVcard">>, GraphQlRequestGet),
    ?assertEqual(Vcard, ParsedResultGet).

user_get_their_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_their_vcard/2).

user_get_their_vcard(Config, Alice) ->
    Client1Fields = [{<<"FN">>, <<"TESTNAME">>}, {<<"EMAIL">>, [{<<"USERID">>, <<"TESTEMAIL">>},
    {<<"HOME">>, []}, {"WORK", []}]}, {<<"EMAIL">>, [{<<"USERID">>, <<"TESTEMAIL2">>},
    {<<"HOME">>, []}]}],
    ExpectedResult = #{<<"formattedName">> => <<"TESTNAME">>,
        <<"email">> => [#{<<"userId">> => <<"TESTEMAIL">>, <<"tags">> => [<<"HOME">>, <<"WORK">>]},
                        #{<<"userId">> => <<"TESTEMAIL2">>, <<"tags">> => [<<"HOME">>]}]},
    escalus_client:send_and_wait(Alice, escalus_stanza:vcard_update(Client1Fields)),
    Body = #{query => user_get_query(), operationName => <<"Q1">>, variables => #{}},
    GraphQlRequest = execute_user(Body, Alice, Config),
    ParsedResult = ok_result(<<"vcard">>, <<"getVcard">>, GraphQlRequest),
    ?assertEqual(ExpectedResult, ParsedResult).

user_get_their_vcard_no_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_their_vcard_no_vcard/2).

user_get_their_vcard_no_vcard(Config, Alice) ->
    Body = #{query => user_get_query(), operationName => <<"Q1">>, variables => #{}},
    GraphQlRequest = execute_user(Body, Alice, Config),
    ParsedResult = error_result(<<"message">>, GraphQlRequest),
    ?assertEqual(<<"Vcard for user not found">>, ParsedResult).

user_get_others_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_others_vcard/3).

user_get_others_vcard(Config, Alice, Bob) ->
    Client1Fields = [{<<"FN">>, <<"TESTNAME">>}, {<<"EMAIL">>, [{<<"USERID">>, <<"TESTEMAIL">>},
    {<<"HOME">>, []}, {"WORK", []}]}, {<<"EMAIL">>, [{<<"USERID">>, <<"TESTEMAIL2">>},
    {<<"HOME">>, []}]}],
    ExpectedResult = #{<<"formattedName">> => <<"TESTNAME">>,
        <<"email">> => [#{<<"userId">> => <<"TESTEMAIL">>, <<"tags">> => [<<"HOME">>, <<"WORK">>]},
                        #{<<"userId">> => <<"TESTEMAIL2">>, <<"tags">> => [<<"HOME">>]}]},
    escalus_client:send_and_wait(Bob, escalus_stanza:vcard_update([{<<"VERSION">>, <<"TESTVERSION">>} | Client1Fields])),
    Body = #{query => user_get_query(), operationName => <<"Q1">>,
             variables => #{user => user_to_bin(Bob)}},
    GraphQlRequest = execute_user(Body, Alice, Config),
    ParsedResult = ok_result(<<"vcard">>, <<"getVcard">>, GraphQlRequest),
    ?assertEqual(ExpectedResult, ParsedResult).

user_get_others_vcard_no_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_others_vcard_no_vcard/3).

user_get_others_vcard_no_vcard(Config, Alice, Bob) ->
    Body = #{query => user_get_query(), operationName => <<"Q1">>,
             variables => #{user => user_to_bin(Bob)}},
    GraphQlRequest = execute_user(Body, Alice, Config),
    ParsedResult = error_result(<<"message">>, GraphQlRequest),
    ?assertEqual(<<"Vcard for user not found">>, ParsedResult).

user_get_others_vcard_no_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_others_vcard_no_user/2).

user_get_others_vcard_no_user(Config, Alice) ->
    Body = #{query => user_get_query(), operationName => <<"Q1">>,
             variables => #{user => <<"AAAAA">>}},
    GraphQlRequest = execute_user(Body, Alice, Config),
    ParsedResult = error_result(<<"message">>, GraphQlRequest),
    ?assertEqual(<<"User does not exist">>, ParsedResult).

%% Admin test cases

admin_set_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_set_vcard/3).

admin_set_vcard(Config, Alice, _Bob) ->
    Vcard = complete_vcard_input(),
    BodySet = set_vcard_body_admin(Vcard, user_to_bin(Alice)),
    GraphQlRequestSet = execute_auth(BodySet, Config),
    ParsedResultSet = ok_result(<<"vcard">>, <<"setVcard">>, GraphQlRequestSet),
    ?assertEqual(Vcard, ParsedResultSet),
    QueryGet = admin_get_full_vcard_as_result_query(),
    VarsGet = #{user => user_to_bin(Alice)},
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => VarsGet},
    GraphQlRequestGet = execute_auth(BodyGet, Config),
    ParsedResultGet = ok_result(<<"vcard">>, <<"getVcard">>, GraphQlRequestGet),
    ?assertEqual(Vcard, ParsedResultGet).

admin_set_vcard_incomplete_fields(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_set_vcard_incomplete_fields/3).

admin_set_vcard_incomplete_fields(Config, Alice, _Bob) ->
    QuerySet = admin_get_full_vcard_as_result_mutation(),
    VcardInput = address_vcard_input(),
    [VcardAddress] = maps:get(<<"address">> ,VcardInput),
    Vcard = [maps:put(<<"pcode">>, null, VcardAddress)],
    VarsSet = #{user => user_to_bin(Alice), vcard => VcardInput},
    BodySet = #{query => QuerySet, operationName => <<"M1">>, variables => VarsSet},
    GraphQlRequestSet = execute_auth(BodySet, Config),
    ParsedResultSet = ok_result(<<"vcard">>, <<"setVcard">>, GraphQlRequestSet),
    ?assertEqual(Vcard, maps:get(<<"address">>, ParsedResultSet)),
    QueryGet = admin_get_full_vcard_as_result_query(),
    VarsGet = #{user => user_to_bin(Alice)},
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => VarsGet},
    GraphQlRequestGet = execute_auth(BodyGet, Config),
    ParsedResultGet = ok_result(<<"vcard">>, <<"getVcard">>, GraphQlRequestGet),
    ?assertEqual(Vcard, maps:get(<<"address">>, ParsedResultGet)).

admin_set_vcard_no_user(Config) ->
    Query = admin_get_full_vcard_as_result_mutation(),
    OpName = <<"M1">>,
    Vcard = complete_vcard_input(),
    Vars = #{user => <<"AAAAA">>, vcard => Vcard},
    Body = #{query => Query, operationName => OpName, variables => Vars},
    GraphQlRequest = execute_auth(Body, Config),
    ParsedResult = error_result(<<"message">>, GraphQlRequest),
    ?assertEqual(<<"User does not exist">>, ParsedResult).

admin_get_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_vcard/3).

admin_get_vcard(Config, Alice, _Bob) ->
    Client1Fields = [{<<"ADR">>, [{<<"POBOX">>, <<"TESTPobox">>}, {<<"EXTADD">>, <<"TESTExtadd">>},
        {<<"STREET">>, <<"TESTStreet">>}, {<<"LOCALITY">>, <<"TESTLocality">>},
        {<<"REGION">>, <<"TESTRegion">>}, {<<"PCODE">>, <<"TESTPCODE">>},
        {<<"CTRY">>, <<"TESTCTRY">>}, {<<"HOME">>, []}, {<<"WORK">>, []}]}],
    ExpectedResult = #{<<"address">> => [#{<<"pobox">> => <<"TESTPobox">>,
        <<"extadd">> => <<"TESTExtadd">>, <<"street">> => <<"TESTStreet">>,
        <<"locality">> => <<"TESTLocality">>, <<"region">> => <<"TESTRegion">>,
        <<"pcode">> => <<"TESTPCODE">>, <<"country">> => <<"TESTCTRY">>,
        <<"tags">> => [<<"HOME">>, <<"WORK">>]}]},
    escalus_client:send_and_wait(Alice, escalus_stanza:vcard_update(Client1Fields)),
    Vars = #{user => user_to_bin(Alice)},
    Body = #{query => admin_get_address_query(), operationName => <<"Q1">>, variables => Vars},
    GraphQlRequest = execute_auth(Body, Config),
    ParsedResult = ok_result(<<"vcard">>, <<"getVcard">>, GraphQlRequest),
    ?assertEqual(ExpectedResult, ParsedResult).

admin_get_vcard_no_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_vcard_no_vcard/2).

admin_get_vcard_no_vcard(Config, Alice) ->
    Query = admin_get_address_query(),
    OpName = <<"Q1">>,
    Vars = #{user => user_to_bin(Alice)},
    Body = #{query => Query, operationName => OpName, variables => Vars},
    GraphQlRequest = execute_auth(Body, Config),
    ParsedResult = error_result(<<"message">>, GraphQlRequest),
    ?assertEqual(<<"Vcard for user not found">>, ParsedResult).

admin_get_vcard_no_user(Config) ->
    Query = admin_get_address_query(),
    OpName = <<"Q1">>,
    Vars = #{user => <<"AAAAA">>},
    Body = #{query => Query, operationName => OpName, variables => Vars},
    GraphQlRequest = execute_auth(Body, Config),
    ParsedResult = error_result(<<"message">>, GraphQlRequest),
    ?assertEqual(<<"User does not exist">>, ParsedResult).

%% Helpers

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

error_result(What, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What, Data).

set_vcard_body_user(Body) ->
    QuerySet = user_get_full_vcard_as_result_mutation(),
    #{query => QuerySet, operationName => <<"M1">>, variables => #{vcard => Body}}.

set_vcard_body_admin(Body, User) ->
    QuerySet = admin_get_full_vcard_as_result_mutation(),
    #{query => QuerySet, operationName => <<"M1">>, variables => #{vcard => Body, user => User}}.

address_vcard_input() ->
   #{<<"formattedName">> => <<"TestName">>,
     <<"nameComponents">> => #{},
     <<"address">> => [
         #{
             <<"tags">> => [<<"HOME">>, <<"WORK">>],
             <<"pobox">> => <<"poboxTest">>,
             <<"extadd">> => <<"extaddTest">>,
             <<"street">> => <<"TESTSTREET123">>,
             <<"locality">> => <<"LOCALITY123">>,
             <<"region">> => <<"REGION777">>,
             <<"country">> => <<"COUNTRY123">>
         }
     ]}.

complete_vcard_input() ->
   #{<<"formattedName">> => <<"TestName">>,
     <<"nameComponents">> => #{
         <<"family">> => <<"familyName">>,
         <<"givenName">> => <<"givenName">>,
         <<"middleName">> => <<"middleName">>,
         <<"prefix">> => <<"prefix">>,
         <<"suffix">> => <<"sufix">>
     },
     <<"nickname">> => [<<"NicknameTest">>, <<"SecondNickname">>],
     <<"photo">> => [
        #{<<"type">> => <<"image/jpeg">>,
          <<"binValue">> => <<"TestBinaries">>},
        #{<<"extValue">> => <<"External Value">>}
    ],
     <<"birthday">> => [<<"birthdayTest">>, <<"SecondBirthday">>],
     <<"address">> => [
         #{
             <<"tags">> => [<<"HOME">>, <<"WORK">>],
             <<"pobox">> => <<"poboxTest">>,
             <<"extadd">> => <<"extaddTest">>,
             <<"street">> => <<"TESTSTREET123">>,
             <<"locality">> => <<"LOCALITY123">>,
             <<"region">> => <<"REGION777">>,
             <<"pcode">> => <<"PcodeTest">>,
             <<"country">> => <<"COUNTRY123">>
         },
         #{
             <<"tags">> => [<<"HOME">>, <<"WORK">>, <<"POSTAL">>],
             <<"pobox">> => <<"poboxTestSecond">>,
             <<"extadd">> => <<"extaddTestSecond">>,
             <<"street">> => <<"TESTSTREET123Second">>,
             <<"locality">> => <<"LOCALITY123Second">>,
             <<"region">> => <<"REGION777TEST">>,
             <<"pcode">> => <<"PcodeTestSECOND">>,
             <<"country">> => <<"COUNTRY123SECOND">>
         }
     ],
     <<"label">> => [
        #{<<"tags">> => [<<"WORK">>, <<"HOME">>], <<"line">> => [<<"LineTest">>, <<"AAA">>]},
        #{<<"tags">> => [<<"POSTAL">>, <<"WORK">>], <<"line">> => [<<"LineTest2">>, <<"AAA">>]}
     ],
     <<"telephone">> => [
         #{<<"tags">> => [<<"HOME">>, <<"BBS">>], <<"number">> => <<"590190190">>},
         #{<<"tags">> => [<<"WORK">>, <<"BBS">>], <<"number">> => <<"590190191">>}
     ],
     <<"email">> => [
         #{<<"tags">> => [<<"PREF">>], <<"userId">> => <<"userIDTEst">>},
         #{<<"tags">> => [<<"PREF">>, <<"HOME">>], <<"userId">> => <<"userIDTEsTETt">>}
     ],
     <<"jabberId">> => [<<"JabberId">>, <<"JabberIDSecind">>],
     <<"mailer">> => [<<"MailerTest">>, <<"MailerSecond">>],
     <<"timeZone">> => [<<"TimeZoneTest">>, <<"TimeZOneSecond">>],
     <<"geo">> => [
         #{<<"lat">> => <<"LatitudeTest">>, <<"lon">> => <<"LongtitudeTest">>},
         #{<<"lat">> => <<"LatitudeTest2">>, <<"lon">> => <<"LongtitudeTest2">>}
     ],
     <<"title">> => [<<"TitleTest">>, <<"SEcondTest">>],
     <<"role">> => [<<"roleTest">>, <<"SecondRole">>],
     <<"logo">> => [
        #{<<"type">> => <<"image/jpeg">>,
          <<"binValue">> => <<"TestBinariesLogo">>},
        #{<<"extValue">> => <<"External Value Logo">>}
     ],
     <<"agent">> => [
         #{<<"vcard">> => agent_vcard_input()},
         #{<<"extValue">> => <<"TESTVALUE">>},
         #{<<"vcard">> => agent_vcard_input()}
     ],
     <<"org">> => [
         #{<<"orgname">> => <<"TESTNAME">>, <<"orgunit">> => [<<"test1">>, <<"TEST2">>]},
         #{<<"orgname">> => <<"TESTNAME123">>, <<"orgunit">> => [<<"test1">>]}
     ],
     <<"categories">> => [
         #{<<"keyword">> => [<<"KeywordTest">>]},
         #{<<"keyword">> => [<<"KeywordTest">>, <<"Keyword2">>]}
     ],
     <<"note">> => [<<"NoteTest">>, <<"NOTE2">>],
     <<"prodId">> => [<<"ProdIdTest">>, <<"PRodTEST2">>],
     <<"rev">> => [<<"revTest">>, <<"RevTest2">>],
     <<"sortString">> => [<<"sortStringTest">>, <<"String2">>],
     <<"sound">> => [
         #{<<"binValue">> => <<"TestBinValue">>},
         #{<<"phonetic">> => <<"PhoneticTest">>},
         #{<<"extValue">> => <<"ExtValueTest">>}
     ],
     <<"uid">> => [<<"UidTest">>, <<"UID2">>],
     <<"url">> => [<<"UrlTest">>, <<"URL2">>],
     <<"desc">> => [<<"DescTest">>, <<"DESC2">>],
     <<"class">> => [
         #{<<"tags">> => [<<"CONFIDENTIAL">>, <<"PRIVATE">>]},
         #{<<"tags">> => [<<"CONFIDENTIAL">>]}
     ],
     <<"key">> => [
         #{<<"type">> => <<"TYPETEST1">>, <<"credential">> => <<"TESTCREDENTIAL1">>},
         #{<<"type">> => <<"TYPETEST2">>, <<"credential">> => <<"TESTCREDENTIAL2">>}
     ]
    }.

agent_vcard_input() ->
   #{<<"formattedName">> => <<"TestName">>,
     <<"nameComponents">> => #{
         <<"family">> => <<"familyName">>,
         <<"givenName">> => <<"givenName">>,
         <<"middleName">> => <<"middleName">>,
         <<"prefix">> => <<"prefix">>,
         <<"suffix">> => <<"sufix">>
     },
     <<"nickname">> => [<<"NicknameTest">>, <<"SecondNickname">>],
     <<"photo">> => [
        #{<<"type">> => <<"image/jpeg">>,
          <<"binValue">> => <<"TestBinaries">>},
        #{<<"extValue">> => <<"External Value">>}
    ]}.

admin_get_address_query() ->
    <<"query Q1($user: JID!)
       {
           vcard
           {
               getVcard(user: $user)
               {
                   address
                   {
                       tags
                       pobox
                       extadd
                       street
                       locality
                       region
                       pcode
                       country
                   }
               }
           }
       }">>.

user_get_query() ->
    <<"query Q1($user: JID)
       {
           vcard
           {
               getVcard(user: $user)
               {
                    formattedName
                    email
                    {
                        userId
                        tags
                    }
               }
           }
       }">>.

admin_get_full_vcard_as_result_mutation() ->
    ResultFormat = get_full_vcard_as_result(),
    <<"mutation M1($vcard: VcardInput!, $user: JID!)",
      "{vcard{setVcard(vcard: $vcard, user: $user)", ResultFormat/binary, "}}">>.

user_get_full_vcard_as_result_mutation() ->
    ResultFormat = get_full_vcard_as_result(),
    <<"mutation M1($vcard: VcardInput!){vcard{setVcard(vcard: $vcard)",
      ResultFormat/binary, "}}">>.

user_get_full_vcard_as_result_query() ->
    ResultFormat = get_full_vcard_as_result(),
    <<"query Q1($user: JID){vcard{getVcard(user: $user)",
      ResultFormat/binary, "}}">>.

admin_get_full_vcard_as_result_query() ->
    ResultFormat = get_full_vcard_as_result(),
    <<"query Q1($user: JID!)",
      "{vcard{getVcard(user: $user)", ResultFormat/binary, "}}">>.

get_full_vcard_as_result() ->
    <<"{
           formattedName
           nameComponents
           { family givenName middleName prefix suffix }
           nickname
           photo
           {
               ... on ImageData
               { type binValue }
               ... on External
               { extValue }
           }
           birthday
           address
           { tags pobox extadd street locality region pcode country }
           label
           { tags line }
           telephone
           { tags number }
           email
           { tags userId }
           jabberId
           mailer
           timeZone
           geo
           { lat lon }
           title
           role
           logo
           {
               ... on ImageData
               { type binValue }
               ... on External
               { extValue }
           }
           agent
           {
               ... on External
               { extValue }
               ... on AgentVcard
               {
                   vcard
                   {
                       formattedName
                       nameComponents
                       { family givenName middleName prefix suffix }
                       nickname
                       photo
                       {
                           ... on ImageData
                           { type binValue }
                           ... on External
                           { extValue }
                       }
                   }
               }
           }
           org
           { orgname orgunit }
           categories
           { keyword }
           note
           prodId
           rev
           sortString
           sound
           {
               ... on External
               { extValue }
               ... on BinValue
               { binValue }
               ... on Phonetic
               { phonetic }
           }
           uid
           url
           desc
           class
           { tags }
           key
           { type credential }
       }">>.
