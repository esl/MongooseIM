-module(graphql_vcard_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_user/3, execute_auth/2, get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_msg/1,
                         get_err_msg/2, make_creds/1, user_to_jid/1, user_to_bin/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("../../include/mod_roster.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [%{group, user_vcard},
     {group, admin_vcard}].

groups() ->
    [%{user_vcard, [], user_vcard_handler()},
     {admin_vcard, [], admin_vcard_handler()}].

init_per_suite(Config) ->
    Config2 = escalus:init_per_suite(Config),
    dynamic_modules:save_modules(domain_helper:host_type(), Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

admin_vcard_handler() ->
    [admin_set_vcard,
     admin_set_vcard_no_user,
     admin_get_vcard].
%     admin_get_vcard_no_user].

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

%% Admin test cases
admin_set_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_set_vcard/3).

admin_set_vcard(Config, Alice, _Bob) ->
    Query = admin_get_full_vcard_as_result_mutation(),
    OpName = <<"M1">>,
    Vcard = #{
        <<"formattedName">> => <<"TestName">>,
        <<"nameComponents">> => #{
            <<"family">> => <<"familyName">>,
            <<"givenName">> => <<"givenName">>,
            <<"middleName">> => <<"middleName">>,
            <<"prefix">> => <<"prefix">>,
            <<"sufix">> => <<"sufix">>
        },
        <<"nickname">> => [<<"NicknameTest">>, <<"SecondNickname">>],
        <<"photo">> => [<<"photoTest">>, <<"SecondPhoto">>],
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
        <<"logo">> => [<<"LogoTest">>, <<"SecondLogo">>],
        <<"agent">> => [<<"AgentTest">>, <<"SecondAgent">>],
        <<"org">> =>[
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
        <<"sound">> => [<<"SoundTest">>, <<"Sound2">>],
        <<"uid">> => [<<"UidTest">>, <<"UID2">>],
        <<"url">> => [<<"UrlTest">>, <<"URL2">>],
        <<"desc">> => [<<"DescTest">>, <<"DESC2">>],
        <<"class">> => [
            #{<<"tags">> => [<<"CONFIDENTIAL">>, <<"PRIVATE">>]},
            #{<<"tags">> => [<<"CONFIDENTIAL">>]}
        ],
        <<"key">> => [
            #{<<"credential">> => <<"TESTCREDENTIAL1">>},
            #{<<"credential">> => <<"TESTCREDENTIAL2">>}
        ]
    },
    Vars = #{user => user_to_bin(Alice), vcard => Vcard},
    Body = #{query => Query, operationName => OpName, variables => Vars},
    GraphQlRequest = execute_auth(Body, Config),
    ParsedResult = ok_result(<<"vcard">>, <<"setVcard">>, GraphQlRequest),
    ?assertEqual(Vcard, ParsedResult).

admin_set_vcard_no_user(Config) ->
    Query = admin_get_full_vcard_as_result_mutation(),
    OpName = <<"M1">>,
    Vcard = #{
        <<"formattedName">> => <<"TestName">>,
        <<"nameComponents">> => #{}
    },
    Vars = #{user => <<"AAAAA">>, vcard => Vcard},
    Body = #{query => Query, operationName => OpName, variables => Vars},
    GraphQlRequest = execute_auth(Body, Config),
    ok_result(<<"vcard">>, <<"setVcard">>, GraphQlRequest).


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

%% Helpers
ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).


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

admin_get_full_vcard_as_result_mutation() ->
    <<"mutation M1($vcard: VcardInput!, $user: JID!)
       {
           vcard
           {
               setVcard(vcard: $vcard, user: $user)
               {
                   formattedName
                   nameComponents
                   {
                       family
                       givenName
                       middleName
                       prefix
                       sufix
                   }
                   nickname
                   photo
                   birthday
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
                   label
                   {
                       tags
                       line
                   }
                   telephone
                   {
                       tags
                       number
                   }
                   email
                   {
                      tags
                      userId
                   }
                   jabberId
                   mailer
                   timeZone
                   geo
                   {
                       lat
                       lon
                   }
                   title
                   role
                   logo
                   agent
                   org
                   {
                       orgname
                       orgunit
                   }
                   categories
                   {
                       keyword
                   }
                   note
                   prodId
                   rev
                   sortString
                   sound
                   uid
                   url
                   desc
                   class
                   {
                       tags
                   }
                   key
                   {
                       credential
                   }
               }
           }
       }">>.
