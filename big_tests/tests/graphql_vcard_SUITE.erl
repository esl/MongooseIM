-module(graphql_vcard_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1, mim/0]).
-import(graphql_helper, [execute_command/4, execute_user_command/5,
                         user_to_bin/1, get_ok_value/2, skip_null_fields/1, get_err_msg/1,
                         get_unauthorized/1, get_not_loaded/1, get_err_code/1]).
-import(domain_helper, [domain/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, domain_admin_vcard},
     {group, admin_http},
     {group, admin_cli}].

groups() ->
    [{user, [], user_groups()},
     {domain_admin_vcard, [], domain_admin_vcard_tests()},
     {admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {user_vcard_configured, [], user_vcard_tests()},
     {user_vcard_not_configured, [], user_vcard_not_configured_tests()},
     {admin_vcard_configured, [], admin_vcard_tests()},
     {admin_vcard_not_configured, [], admin_vcard_not_configured_tests()}].

user_groups() ->
    [{group, user_vcard_configured},
     {group, user_vcard_not_configured}].

admin_groups() ->
    [{group, admin_vcard_configured},
     {group, admin_vcard_not_configured}].

user_vcard_tests() ->
    [user_set_vcard,
     user_get_their_vcard,
     user_get_their_vcard_no_vcard,
     user_get_others_vcard,
     user_get_others_vcard_no_user,
     user_get_others_vcard_no_vcard].

user_vcard_not_configured_tests() ->
    [user_set_vcard_not_configured,
     user_get_their_vcard_not_configured,
     user_get_others_vcard_not_configured].

domain_admin_vcard_tests() ->
    [admin_set_vcard,
     admin_set_vcard_incomplete_fields,
     domain_admin_set_vcard_no_host,
     domain_admin_set_vcard_no_permission,
     admin_get_vcard,
     admin_get_vcard_no_vcard,
     domain_admin_get_vcard_no_host,
     domain_admin_get_vcard_no_permission].

admin_vcard_tests() ->
    [admin_set_vcard,
     admin_set_vcard_incomplete_fields,
     admin_set_vcard_no_host,
     admin_get_vcard,
     admin_get_vcard_no_vcard,
     admin_get_vcard_no_host].

admin_vcard_not_configured_tests() ->
    [admin_set_vcard_not_configured,
     admin_get_vcard_not_configured].

init_per_suite(Config) ->
    case vcard_helper:is_vcard_ldap() of
        true ->
            {skip, ldap_vcard_is_not_supported};
        _ ->
            HostType = domain_helper:host_type(),
            Config1 = escalus:init_per_suite(Config),
            Config2 = ejabberd_node_utils:init(mim(), Config1),
            Config3 = dynamic_modules:save_modules(domain_helper:host_type(), Config2),
            VCardOpts = dynamic_modules:get_saved_config(HostType, mod_vcard, Config3),
            [{mod_vcard_opts, VCardOpts} | Config3]
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_vcard, Config) ->
    Config1 = ensure_vcard_started(Config),
    graphql_helper:init_domain_admin_handler(Config1);
init_per_group(user, Config) ->
    graphql_helper:init_user(Config);
init_per_group(Group, Config) when Group =:= admin_vcard_configured;
                                   Group =:= user_vcard_configured ->
    ensure_vcard_started(Config);
init_per_group(Group, Config) when Group =:= admin_vcard_not_configured;
                                   Group =:= user_vcard_not_configured ->
    ensure_vcard_stopped(Config).

end_per_group(GroupName, _Config) when GroupName =:= admin_http;
                                       GroupName =:= admin_cli;
                                       GroupName =:= user;
                                       GroupName =:= domain_admin_vcard ->
    graphql_helper:clean();
end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean().

ensure_vcard_started(Config) ->
    HostType = domain_helper:host_type(),
    VCardConfig = ?config(mod_vcard_opts, Config),
    dynamic_modules:restart(HostType, mod_vcard, VCardConfig),
    Config.

ensure_vcard_stopped(Config) ->
    HostType = domain_helper:host_type(),
    dynamic_modules:stop(HostType, mod_vcard),
    Config.

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
    ResultSet = user_set_vcard(Alice, Vcard, Config),
    ParsedResultSet = get_ok_value([data, vcard, setVcard], ResultSet),
    ?assertEqual(Vcard, skip_null_fields(ParsedResultSet)),
    ResultGet = user_get_own_vcard(Alice, Config),
    ParsedResultGet = get_ok_value([data, vcard, getVcard], ResultGet),
    ?assertEqual(Vcard, skip_null_fields(ParsedResultGet)).

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
    Result = user_get_own_vcard(Alice, Config),
    ParsedResult = get_ok_value([data, vcard, getVcard], Result),
    ?assertEqual(ExpectedResult, skip_null_fields(ParsedResult)).

user_get_their_vcard_no_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_their_vcard_no_vcard/2).

user_get_their_vcard_no_vcard(Config, Alice) ->
    Result = user_get_own_vcard(Alice, Config),
    ?assertEqual(<<"Vcard for user not found">>, get_err_msg(Result)).

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
    escalus_client:send_and_wait(Bob, escalus_stanza:vcard_update([{<<"VERSION">>, <<"TESTVERSION">>}
                                                                  | Client1Fields])),
    Result = user_get_vcard(Alice, user_to_bin(Bob), Config),
    ParsedResult = get_ok_value([data, vcard, getVcard], Result),
    ?assertEqual(ExpectedResult, skip_null_fields(ParsedResult)).

user_get_others_vcard_no_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_others_vcard_no_vcard/3).

user_get_others_vcard_no_vcard(Config, Alice, Bob) ->
    Result = user_get_vcard(Alice, user_to_bin(Bob), Config),
    ?assertEqual(<<"Vcard for user not found">>, get_err_msg(Result)).

user_get_others_vcard_no_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_others_vcard_no_user/2).

user_get_others_vcard_no_user(Config, Alice) ->
    Result = user_get_vcard(Alice, <<"eddie@otherhost">>, Config),
    ?assertEqual(<<"User's domain does not exist">>, get_err_msg(Result)),
    Domain = domain(),
    Result2 = user_get_vcard(Alice, <<"eddie@", Domain/binary>>, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Result2)).

% User VCard not configured test cases

user_set_vcard_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_set_vcard_not_configured/2).

user_set_vcard_not_configured(Config, Alice) ->
    Vcard = complete_vcard_input(),
    Res = user_set_vcard(Alice, Vcard, Config),
    get_not_loaded(Res).

user_get_others_vcard_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_others_vcard_not_configured/3).

user_get_others_vcard_not_configured(Config, Alice, Bob) ->
    Res = user_get_vcard(Alice, user_to_bin(Bob), Config),
    get_not_loaded(Res).

user_get_their_vcard_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_their_vcard_not_configured/2).

user_get_their_vcard_not_configured(Config, Alice) ->
    Res = user_get_own_vcard(Alice, Config),
    ?assertEqual(<<"vcard_not_configured_error">>, get_err_code(Res)).

%% Domain admin test cases

domain_admin_set_vcard_no_host(Config) ->
    Vcard = complete_vcard_input(),
    get_unauthorized(set_vcard(Vcard, <<"eddie@otherhost">>, Config)).

domain_admin_get_vcard_no_host(Config) ->
    get_unauthorized(get_vcard(<<"eddie@otherhost">>, Config)).

domain_admin_get_vcard_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_get_vcard_no_permission/2).

domain_admin_get_vcard_no_permission(Config, AliceBis) ->
    Result = get_vcard(user_to_bin(AliceBis), Config),
    get_unauthorized(Result).

domain_admin_set_vcard_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_set_vcard_no_permission/2).

domain_admin_set_vcard_no_permission(Config, AliceBis) ->
    Vcard = complete_vcard_input(),
    Result = set_vcard(Vcard, user_to_bin(AliceBis), Config),
    get_unauthorized(Result).

%% Admin test cases

admin_set_vcard(Config) ->
    Config1 = [{vcard, complete_vcard_input()} | Config],
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun admin_set_vcard/3).

admin_set_vcard_incomplete_fields(Config) ->
    Config1 = [{vcard, address_vcard_input()} | Config],
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun admin_set_vcard/3).

admin_set_vcard(Config, Alice, _Bob) ->
    Vcard = ?config(vcard, Config),
    ResultSet = set_vcard(Vcard, user_to_bin(Alice), Config),
    ParsedResultSet = get_ok_value([data, vcard, setVcard], ResultSet),
    ?assertEqual(Vcard, skip_null_fields(ParsedResultSet)),
    ResultGet = get_vcard(user_to_bin(Alice), Config),
    ParsedResultGet = get_ok_value([data, vcard, getVcard], ResultGet),
    ?assertEqual(Vcard, skip_null_fields(ParsedResultGet)).

admin_set_vcard_no_host(Config) ->
    Vcard = complete_vcard_input(),
    Result = set_vcard(Vcard, <<"eddie@otherhost">>, Config),
    ?assertEqual(<<"User's domain does not exist">>, get_err_msg(Result)),
    Domain = domain(),
    Result2 = set_vcard(Vcard, <<"eddie@", Domain/binary>>, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Result2)).

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
    Result = get_vcard(user_to_bin(Alice), Config),
    ParsedResult = get_ok_value([data, vcard, getVcard], Result),
    ?assertEqual(ExpectedResult, skip_null_fields(ParsedResult)).

admin_get_vcard_no_vcard(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_vcard_no_vcard/2).

admin_get_vcard_no_vcard(Config, Alice) ->
    Result = get_vcard(user_to_bin(Alice), Config),
    ?assertEqual(<<"Vcard for user not found">>, get_err_msg(Result)).

admin_get_vcard_no_host(Config) ->
    Result = get_vcard(<<"eddie@otherhost">>, Config),
    ?assertEqual(<<"User's domain does not exist">>, get_err_msg(Result)),
    Domain = domain(),
    Result2 = get_vcard(<<"eddie@", Domain/binary>>, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Result2)).

%% Admin VCard not configured test cases

admin_get_vcard_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_vcard_not_configured/2).

admin_get_vcard_not_configured(Config, Alice) ->
    Res = get_vcard(user_to_bin(Alice), Config),
    get_not_loaded(Res).

admin_set_vcard_not_configured(Config) ->
    Config1 = [{vcard, complete_vcard_input()} | Config],
    escalus:fresh_story_with_config(Config1, [{alice, 1}],
                                    fun admin_set_vcard_not_configured/2).

admin_set_vcard_not_configured(Config, Alice) ->
    Vcard = ?config(vcard, Config),
    Res = set_vcard(Vcard, user_to_bin(Alice), Config),
    get_not_loaded(Res).

%% Commands

user_set_vcard(User, VCard, Config) ->
    Vars = #{vcard => VCard},
    execute_user_command(<<"vcard">>, <<"setVcard">>, User, Vars, Config).

user_get_own_vcard(User, Config) ->
    execute_user_command(<<"vcard">>, <<"getVcard">>, User, #{}, Config).

user_get_vcard(User, JID, Config) ->
    Vars = #{user => JID},
    execute_user_command(<<"vcard">>, <<"getVcard">>, User, Vars, Config).

set_vcard(VCard, User, Config) ->
    Vars = #{vcard => VCard, user => User},
    execute_command(<<"vcard">>, <<"setVcard">>, Vars, Config).

get_vcard(User, Config) ->
    Vars = #{user => User},
    execute_command(<<"vcard">>, <<"getVcard">>, Vars, Config).

%% Helpers

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
