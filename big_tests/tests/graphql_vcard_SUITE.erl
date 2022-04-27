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
     admin_get_vcard].

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
    Query = get_full_vcard_as_result_mutation(),
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
        <<"nickname">> => [<<"NicknameTest">>],
        <<"photo">> => [<<"photoTest">>],
        <<"birthday">> => [<<"birthdayTest">>],
        <<"address">> => [
            #{
                <<"tags">> => [<<"HOME">>],
                <<"pobox">> => <<"poboxTest">>,
                <<"extadd">> => <<"extaddTest">>,
                <<"street">> => <<"TESTSTREET123">>,
                <<"locality">> => <<"LOCALITY123">>,
                <<"region">> => <<"REGION777">>,
                <<"pcode">> => <<"PcodeTest">>,
                <<"country">> => <<"COUNTRY123">>
            }
        ],
        <<"label">> => [
           #{
                <<"tags">> => [<<"WORK">>],
                <<"line">> => [<<"LineTest">>]
            }
        ],
        <<"telephone">> => [
            #{
                <<"tags">> => [<<"HOME">>],
                <<"number">> => <<"590190190">>
            }
        ],
        <<"email">> => [
            #{
                <<"tags">> => [<<"PREF">>],
                <<"userId">> => <<"userIDTEst">>
            }
        ],
        <<"jabberId">> => [<<"JabberId">>],
        <<"mailer">> => [<<"MailerTest">>],
        <<"timeZone">> => [<<"TimeZoneTest">>],
        <<"geo">> => [
            #{
                <<"lat">> => <<"LatitudeTest">>,
                <<"lon">> => <<"LongtitudeTest">>
            }
        ],
        <<"title">> => [<<"TitleTest">>],
        <<"role">> => [<<"roleTest">>],
        <<"logo">> => [<<"LogoTest">>],
        <<"agent">> => [<<"AgentTest">>],
        <<"org">> =>[
            #{
                <<"orgname">> => <<"TESTNAME">>,
                <<"orgunit">> => [<<"TEST1">>, <<"TEST2">>]
            }
        ],
        <<"categories">> => [
            #{
                <<"keyword">> => [<<"KeywordTest">>]
            }
        ],
        <<"note">> => [<<"NoteTest">>],
        <<"prodId">> => [<<"ProdIdTest">>],
        <<"rev">> => [<<"revTest">>],
        <<"sortString">> => [<<"sortStringTest">>],
        <<"sound">> => [<<"SoundTest">>],
        <<"uid">> => [<<"UidTest">>],
        <<"url">> => [<<"UrlTest">>],
        <<"desc">> => [<<"DescTest">>],
        <<"class">> => [
            #{<<"tags">> => [<<"CONFIDENTIAL">>]}
        ],
        <<"key">> => [
            #{<<"credential">> => <<"TESTCREDENTIAL1">>}
        ]
    },
    Vars = #{user => user_to_bin(Alice), vcard => Vcard},
    Body = #{query => Query, operationName => OpName, variables => Vars},
    GraphQlRequest = execute_auth(Body, Config),
    ParsedResult = ok_result(<<"vcard">>, <<"setVcard">>, GraphQlRequest),
    ?assertEqual(Vcard, ParsedResult).
    %Stanza = escalus_client:send_and_wait(Alice, escalus_stanza:vcard_request()).

admin_get_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_vcard/3).

admin_get_vcard(Config, Alice, _Bob) ->
    Query = <<"query Q1($user: JID!)
               { vcard { getVcard(user: $user) {formattedName nameComponents {givenName} nickname telephone{number}}}}">>,
    OpName = <<"Q1">>,
    Vars = #{user => user_to_bin(Alice)},
    Body = #{query => Query, operationName => OpName, variables => Vars},
    execute_auth(Body, Config).

%% Helpers
ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

get_full_vcard_as_result_mutation() ->
    <<"mutation M1($vcard: VcardInput!, $user: JID!)
       {vcard
       {setVcard(vcard: $vcard, user: $user)
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