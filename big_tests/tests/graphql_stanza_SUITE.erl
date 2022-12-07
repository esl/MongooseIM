-module(graphql_stanza_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1]).
-import(graphql_helper, [execute_user_command/5, execute_command/4,
                         execute_user_command_sse/5, execute_command_sse/4,
                         get_ok_value/2, get_value/2,
                         get_err_code/1, get_err_msg/1, get_coercion_err_msg/1,
                         get_unauthorized/1, get_not_loaded/1]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_stanza_http},
     {group, admin_stanza_cli},
     {group, domain_admin_stanza},
     {group, user_stanza}].

groups() ->
    [{admin_stanza_http, [], [{group, admin_mam},
                              {group, admin_no_mam_http}]},
     {admin_stanza_cli, [], [{group, admin_mam},
                             {group, admin_no_mam_cli}]},
     {domain_admin_stanza, [], [{group, admin_mam}, % same as for admin
                                {group, domain_admin_no_mam}]},
     {user_stanza, [], [{group, user_mam},
                        {group, user_no_mam}]},
     {admin_mam, [parallel], admin_mam_cases()},
     {admin_no_mam_http, [parallel], admin_stanza_cases(http) ++ admin_no_mam_cases()},
     {admin_no_mam_cli, [], admin_stanza_cases(cli) ++ admin_no_mam_cases()},
     {domain_admin_no_mam, [parallel], domain_admin_stanza_cases() ++ admin_no_mam_cases()},
     {user_mam, [parallel], user_mam_cases()},
     {user_no_mam, [parallel], user_stanza_cases() ++ user_no_mam_cases()}].

admin_stanza_cases(cli) ->
    admin_stanza_cases();
admin_stanza_cases(http) ->
    admin_stanza_cases() ++ admin_sse_cases().

admin_stanza_cases() ->
    [admin_send_message,
     admin_send_message_to_unparsable_jid,
     admin_send_message_headline,
     admin_send_stanza,
     admin_send_unparsable_stanza,
     admin_send_stanza_from_unknown_user,
     admin_send_stanza_from_unknown_domain].

admin_sse_cases() ->
    [admin_subscribe_for_messages,
     admin_subscribe_for_messages_to_unknown_user].

admin_mam_cases() ->
    [admin_get_last_messages,
     admin_get_last_messages_for_unknown_user,
     admin_get_last_messages_with,
     admin_get_last_messages_limit,
     admin_get_last_messages_limit_null,
     admin_get_last_messages_limit_enforced,
     admin_get_last_messages_before].

admin_no_mam_cases() ->
    [admin_get_last_messages_no_mam].

domain_admin_stanza_cases() ->
    [admin_send_message,
     admin_send_message_to_unparsable_jid,
     admin_send_message_headline,
     domain_admin_send_message_no_permission,
     domain_admin_send_stanza,
     admin_send_unparsable_stanza,
     domain_admin_send_stanza_from_unknown_user,
     domain_admin_send_stanza_from_unknown_domain,
     domain_admin_get_last_messages_no_permission].

user_stanza_cases() ->
    [user_send_message,
     user_subscribe_for_messages,
     user_send_message_without_from,
     user_send_message_with_spoofed_from,
     user_send_message_headline,
     user_send_message_headline_with_spoofed_from,
     user_send_stanza,
     user_send_stanza_without_from_with_id,
     user_send_stanza_with_spoofed_from].

user_mam_cases() ->
    [user_get_last_messages].

user_no_mam_cases() ->
    [user_get_last_messages_no_mam].

init_per_suite(Config) ->
    Config1 = ejabberd_node_utils:init(mim(), Config),
    Config2 = escalus:init_per_suite(Config1),
    dynamic_modules:save_modules(domain_helper:host_type(), Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_stanza_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_stanza_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_stanza, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(user_stanza, Config) ->
    graphql_helper:init_user(Config);
init_per_group(GN, Config) when GN =:= admin_mam;
                                GN =:= domain_admin_mam;
                                GN =:= user_mam ->
    init_mam(Config);
init_per_group(GN, Config) when GN =:= admin_no_mam_http;
                                GN =:= admin_no_mam_cli;
                                GN =:= domain_admin_no_mam;
                                GN =:= user_no_mam ->
    Mods = [{mod_mam, stopped}],
    dynamic_modules:ensure_modules(domain_helper:host_type(), Mods),
    Config.

end_per_group(GN, _Config) when GN =:= admin_stanza_http;
                                GN =:= admin_stanza_cli;
                                GN =:= domain_admin_stanza;
                                GN =:= user_stanza ->
    graphql_helper:clean();
end_per_group(_GN, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

init_mam(Config) when is_list(Config) ->
    case mam_helper:backend() of
        disabled ->
            {skip, no_mam};
        Backend ->
            MAMOpts = mam_helper:config_opts(#{backend => Backend, pm => #{}}),
            Mods = [{mod_mam, MAMOpts}],
            dynamic_modules:ensure_modules(domain_helper:host_type(), Mods),
            Config
    end;
init_mam(Other) ->
    Other.

%% Test Cases

admin_send_message(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_story/3).

admin_send_message_story(Config, Alice, Bob) ->
    From = escalus_client:full_jid(Alice),
    To = escalus_client:short_jid(Bob),
    Res = send_message(From, To, <<"Hi!">>, Config),
    #{<<"id">> := StanzaId} = get_ok_value([data, stanza, sendMessage], Res),
    assert_not_empty(StanzaId),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

admin_subscribe_for_messages_to_unknown_user(Config) ->
    Domain = domain_helper:domain(),
    Res = subscribe_for_messages(<<"baduser@", Domain/binary>>, Config),
    ?assertEqual(<<"unknown_user">>, get_err_code(Res)),
    ?assertEqual(<<"User does not exist">>, get_err_msg(Res)).

admin_subscribe_for_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_subscribe_for_messages_story/3).

admin_subscribe_for_messages_story(Config, Alice, Bob) ->
    From = escalus_client:full_jid(Alice),
    To = escalus_client:short_jid(Bob),
    {200, Stream} = subscribe_for_messages(To, Config),
    %% Presence should be skipped
    escalus:send(Alice, escalus_stanza:presence_direct(To, <<"available">>)),
    %% Message should be delivered
    escalus:send(Alice, escalus_stanza:chat(From, To, <<"Hi!">>)),
    Event = sse_helper:wait_for_event(Stream),
    #{<<"stanza">> := StanzaBin, <<"sender">> := From} =
        graphql_helper:get_value([data, stanza, subscribeForMessages], Event),
    {ok, Stanza} = exml:parse(StanzaBin),
    escalus:assert(is_message, Stanza),
    sse_helper:stop_sse(Stream).

user_send_message(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_story/3).

user_send_message_story(Config, Alice, Bob) ->
    From = escalus_client:full_jid(Alice),
    To = escalus_client:short_jid(Bob),
    Res = user_send_message(Alice, From, To, <<"Hi!">>, Config),
    #{<<"id">> := StanzaId} = get_ok_value([data, stanza, sendMessage], Res),
    assert_not_empty(StanzaId),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

user_subscribe_for_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_subscribe_for_messages_story/3).

user_subscribe_for_messages_story(Config, Alice, Bob) ->
    From = escalus_client:full_jid(Alice),
    To = escalus_client:short_jid(Bob),
    {200, Stream} = user_subscribe_for_messages(Bob, Config),
    escalus:send(Alice, escalus_stanza:chat(From, To, <<"Hi!">>)),
    Event = sse_helper:wait_for_event(Stream),
    #{<<"stanza">> := StanzaBin, <<"sender">> := From} =
        graphql_helper:get_value([data, stanza, subscribeForMessages], Event),
    {ok, Stanza} = exml:parse(StanzaBin),
    escalus:assert(is_message, Stanza),
    sse_helper:stop_sse(Stream).

user_send_message_without_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_without_from_story/3).

user_send_message_without_from_story(Config, Alice, Bob) ->
    To = escalus_client:short_jid(Bob),
    Res = user_send_message(Alice, null, To, <<"Hi!">>, Config),
    #{<<"id">> := StanzaId} = get_ok_value([data, stanza, sendMessage], Res),
    assert_not_empty(StanzaId),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

user_send_message_with_spoofed_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_with_spoofed_from_story/3).

user_send_message_with_spoofed_from_story(Config, Alice, Bob) ->
    To = From = escalus_client:short_jid(Bob),
    Res = user_send_message(Alice, From, To, <<"Hi!">>, Config),
    spoofed_error(sendMessage, Res).

admin_send_message_to_unparsable_jid(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_send_message_to_unparsable_jid_story/2).

admin_send_message_to_unparsable_jid_story(Config, Alice) ->
    From = escalus_client:full_jid(Alice),
    To = <<"test@">>,
    Res = send_message(From, To, <<"Hi!">>, Config),
    ?assertEqual(<<"Input coercion failed for type JID with value "
                   "<<\"test@\">>. The reason it failed is: failed_to_parse_jid">>,
                 get_coercion_err_msg(Res)).

admin_send_message_headline(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_headline_story/3).

admin_send_message_headline_story(Config, Alice, Bob) ->
    From = escalus_client:full_jid(Alice),
    To = escalus_client:short_jid(Bob),
    Res = send_message_headline(From, To, <<"Welcome">>, <<"Hi!">>, Config),
    #{<<"id">> := StanzaId} = get_ok_value([data, stanza, sendMessageHeadLine], Res),
    assert_not_empty(StanzaId),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)),
    Res2 = send_message_headline(From, To, null, null, Config),
    #{<<"id">> := StanzaId2} = get_ok_value([data, stanza, sendMessageHeadLine], Res2),
    assert_not_empty(StanzaId2),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

user_send_message_headline(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_headline_story/3).

user_send_message_headline_story(Config, Alice, Bob) ->
    From = escalus_client:full_jid(Alice),
    To = escalus_client:short_jid(Bob),
    Res = user_send_message_headline(Alice, From, To, <<"Welcome">>, <<"Hi!">>, Config),
    #{<<"id">> := StanzaId} = get_ok_value([data, stanza, sendMessageHeadLine], Res),
    assert_not_empty(StanzaId),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

user_send_message_headline_with_spoofed_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_headline_with_spoofed_from_story/3).

user_send_message_headline_with_spoofed_from_story(Config, Alice, Bob) ->
    To = From = escalus_client:short_jid(Bob),
    Res = user_send_message_headline(Alice, From, To, <<"Welcome">>, <<"Hi!">>, Config),
    spoofed_error(sendMessageHeadLine, Res).

domain_admin_send_message_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_send_message_no_permission_story/3).

domain_admin_send_message_no_permission_story(Config, AliceBis, Bob) ->
    From = escalus_client:full_jid(AliceBis),
    To = escalus_client:short_jid(Bob),
    Res = send_message(From, To, <<"Hi!">>, Config),
    get_unauthorized(Res).

domain_admin_send_stanza(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun domain_admin_send_stanza_story/3).

domain_admin_send_stanza_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Alice),
    Res = send_stanza(exml:to_binary(Stanza), Config),
    get_unauthorized(Res).

domain_admin_send_stanza_from_unknown_user(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun domain_admin_send_stanza_from_unknown_user_story/2).

domain_admin_send_stanza_from_unknown_user_story(Config, Bob) ->
    Body = <<"Hi!">>,
    Server = escalus_client:server(Bob),
    From = <<"YeeeAH@", Server/binary>>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Res = send_stanza(exml:to_binary(Stanza), Config),
    get_unauthorized(Res).

domain_admin_send_stanza_from_unknown_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun domain_admin_send_stanza_from_unknown_domain_story/2).

domain_admin_send_stanza_from_unknown_domain_story(Config, Bob) ->
    Body = <<"Hi!">>,
    From = <<"YeeeAH@oopsie">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Res = send_stanza(exml:to_binary(Stanza), Config),
    get_unauthorized(Res).

domain_admin_get_last_messages_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_get_last_messages_story_no_permission/2).

domain_admin_get_last_messages_story_no_permission(Config, AliceBis) ->
    Res = get_last_messages(escalus_client:full_jid(AliceBis), null, null, Config),
    get_unauthorized(Res).

admin_send_stanza(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_stanza_story/3).

admin_send_stanza_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Alice),
    Res = send_stanza(exml:to_binary(Stanza), Config),
    #{<<"id">> := StanzaId} = get_ok_value([data, stanza, sendStanza], Res),
    assert_not_empty(StanzaId),
    StanzaIn = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, StanzaIn),
    ?assertEqual(StanzaId, exml_query:attr(StanzaIn, <<"id">>)).

user_send_stanza(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_stanza_story/3).

user_send_stanza_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Alice),
    Res = user_send_stanza(Alice, exml:to_binary(Stanza), Config),
    #{<<"id">> := StanzaId} = get_ok_value([data, stanza, sendStanza], Res),
    assert_not_empty(StanzaId),
    StanzaIn = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, StanzaIn),
    ?assertEqual(StanzaId, exml_query:attr(StanzaIn, <<"id">>)).

user_send_stanza_without_from_with_id(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_stanza_without_from_with_id_story/3).

user_send_stanza_without_from_with_id_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    StanzaId = base16:encode(crypto:strong_rand_bytes(8)),
    Stanza = escalus_stanza:set_id(escalus_stanza:chat_to_short_jid(Bob, Body), StanzaId),
    Res = user_send_stanza(Alice, exml:to_binary(Stanza), Config),
    ?assertEqual(#{<<"id">> => StanzaId}, get_ok_value([data, stanza, sendStanza], Res)),
    StanzaIn = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, StanzaIn),
    ?assertEqual(StanzaId, exml_query:attr(StanzaIn, <<"id">>)).

user_send_stanza_with_spoofed_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_stanza_with_spoofed_from_story/3).

user_send_stanza_with_spoofed_from_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Bob),
    Res = user_send_stanza(Alice, exml:to_binary(Stanza), Config),
    spoofed_error(sendStanza, Res).

admin_send_unparsable_stanza(Config) ->
    Res = send_stanza(<<"<test">>, Config),
    ?assertEqual(<<"Input coercion failed for type XmlElement with value <<\"<test\">>. "
                   "The reason it failed is: \"expected >\"">>,
                 get_coercion_err_msg(Res)).

admin_send_stanza_from_unknown_user(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun admin_send_stanza_from_unknown_user_story/2).

admin_send_stanza_from_unknown_user_story(Config, Bob) ->
    Body = <<"Hi!">>,
    Server = escalus_client:server(Bob),
    From = <<"YeeeAH@", Server/binary>>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Res = send_stanza(exml:to_binary(Stanza), Config),
    ?assertEqual(<<"unknown_user">>, get_err_code(Res)),
    ?assertEqual(<<"User does not exist">>, get_err_msg(Res)).

admin_send_stanza_from_unknown_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun admin_send_stanza_from_unknown_domain_story/2).

admin_send_stanza_from_unknown_domain_story(Config, Bob) ->
    Body = <<"Hi!">>,
    From = <<"YeeeAH@oopsie">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Res = send_stanza(exml:to_binary(Stanza), Config),
    ?assertEqual(<<"unknown_user">>, get_err_code(Res)),
    ?assertEqual(<<"User's domain does not exist">>, get_err_msg(Res)).

admin_get_last_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_story/3).

admin_get_last_messages_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    mam_helper:wait_for_archive_size(Bob, 1),
    Res1 = get_last_messages(escalus_client:full_jid(Alice), null, null, Config),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res1),
    check_stanza_map(M1, Alice),
    Res2 = get_last_messages(escalus_client:full_jid(Bob), null, null, Config),
    #{<<"stanzas">> := [M2], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res2),
    check_stanza_map(M2, Alice).

admin_get_last_messages_no_mam(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(FreshConfig, alice),
    Res = get_last_messages(AliceJid, null, null, Config),
    get_not_loaded(Res).

user_get_last_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_last_messages_story/3).

user_get_last_messages_story(Config, Alice, Bob) ->
    user_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    mam_helper:wait_for_archive_size(Bob, 1),
    Res1 = user_get_last_messages(Alice, null, null, Config),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res1),
    check_stanza_map(M1, Alice),
    Res2 = user_get_last_messages(Bob, null, null, Config),
    #{<<"stanzas">> := [M2], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res2),
    check_stanza_map(M2, Alice).

user_get_last_messages_no_mam(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_last_messages_no_mam_story/2).

user_get_last_messages_no_mam_story(Config, Alice) ->
    Res = user_get_last_messages(Alice, null, null, Config),
    get_not_loaded(Res).

admin_get_last_messages_for_unknown_user(Config) ->
    Domain = domain_helper:domain(),
    Jid = <<"maybemaybebutnot@", Domain/binary>>,
    Res = get_last_messages(Jid, null, null, Config),
    ?assertEqual(<<"unknown_user">>, get_err_code(Res)),
    ?assertEqual(<<"User does not exist">>, get_err_msg(Res)).

admin_get_last_messages_with(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_get_last_messages_with_story/4).

admin_get_last_messages_with_story(Config, Alice, Bob, Kate) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    admin_send_message_story(Config, Kate, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    Caller = escalus_client:full_jid(Alice),
    With = escalus_client:short_jid(Bob),
    Res = get_last_messages(Caller, With, null, Config),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res),
    check_stanza_map(M1, Alice).

admin_get_last_messages_limit(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_limit_story/3).

admin_get_last_messages_limit_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    admin_send_message_story(Config, Bob, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    Caller = escalus_client:full_jid(Alice),
    Res = get_last_messages(Caller, null, 1, null, Config),
    #{<<"stanzas">> := [M1], <<"limit">> := 1} =
        get_ok_value([data, stanza, getLastMessages], Res),
    check_stanza_map(M1, Bob).

admin_get_last_messages_limit_null(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_limit_null_story/3).

admin_get_last_messages_limit_null_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    Caller = escalus_client:full_jid(Alice),
    Res = get_last_messages(Caller, null, null, null, Config),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res),
    check_stanza_map(M1, Alice).

admin_get_last_messages_limit_enforced(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_limit_enforced_story/3).

admin_get_last_messages_limit_enforced_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    Caller = escalus_client:full_jid(Alice),
    Res = get_last_messages(Caller, null, 1000, null, Config),
    %% The actual limit is returned
    #{<<"stanzas">> := [M1], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res),
    check_stanza_map(M1, Alice).

admin_get_last_messages_before(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_before_story/3).

admin_get_last_messages_before_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    admin_send_message_story(Config, Bob, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    admin_send_message_story(Config, Bob, Alice),
    mam_helper:wait_for_archive_size(Alice, 3),
    Caller = escalus_client:full_jid(Alice),
    Res1 = get_last_messages(Caller, null, null, Config),
    #{<<"stanzas">> := [M1, M2, _M3], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res1),
    Before = maps:get(<<"timestamp">>, M2),
    Res2 = get_last_messages(Caller, null, Before, Config),
    #{<<"stanzas">> := [M1, M2], <<"limit">> := 50} =
        get_ok_value([data, stanza, getLastMessages], Res2).

%% Commands

send_message(From, To, Body, Config) ->
    Vars = #{from => From, to => To, body => Body},
    execute_command(<<"stanza">>, <<"sendMessage">>, Vars, Config).

user_send_message(User, From, To, Body, Config) ->
    Vars = #{from => From, to => To, body => Body},
    execute_user_command(<<"stanza">>, <<"sendMessage">>, User, Vars, Config).

send_message_headline(From, To, Subject, Body, Config) ->
    Vars = #{from => From, to => To, subject => Subject, body => Body},
    execute_command(<<"stanza">>, <<"sendMessageHeadLine">>, Vars, Config).

user_send_message_headline(User, From, To, Subject, Body, Config) ->
    Vars = #{from => From, to => To, subject => Subject, body => Body},
    execute_user_command(<<"stanza">>, <<"sendMessageHeadLine">>, User, Vars, Config).

send_stanza(Stanza, Config) ->
    Vars = #{stanza => Stanza},
    execute_command(<<"stanza">>, <<"sendStanza">>, Vars, Config).

user_send_stanza(User, Stanza, Config) ->
    Vars = #{stanza => Stanza},
    execute_user_command(<<"stanza">>, <<"sendStanza">>, User, Vars, Config).

subscribe_for_messages(Caller, Config) ->
    Vars = #{caller => Caller},
    execute_command_sse(<<"stanza">>, <<"subscribeForMessages">>, Vars, Config).

user_subscribe_for_messages(User, Config) ->
    execute_user_command_sse(<<"stanza">>, <<"subscribeForMessages">>, User, #{}, Config).

get_last_messages(Caller, With, Before, Config) ->
    Vars = #{caller => Caller, with => With, before => Before},
    execute_command(<<"stanza">>, <<"getLastMessages">>, Vars, Config).

get_last_messages(Caller, With, Limit, Before, Config) ->
    Vars = #{caller => Caller, with => With, limit => Limit, before => Before},
    execute_command(<<"stanza">>, <<"getLastMessages">>, Vars, Config).

user_get_last_messages(User, With, Before, Config) ->
    Vars = #{with => With, before => Before},
    execute_user_command(<<"stanza">>, <<"getLastMessages">>, User, Vars, Config).

%% Helpers

assert_not_empty(Bin) when byte_size(Bin) > 0 -> ok.

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

check_stanza_map(#{<<"sender">> := SenderJID,
                   <<"stanza">> := XML,
                   <<"stanza_id">> := StanzaID,
                   <<"timestamp">> := TS}, SenderClient) ->
    ?assertEqual(escalus_utils:jid_to_lower(escalus_client:full_jid(SenderClient)),
                 escalus_utils:jid_to_lower(SenderJID)),
     true = byte_size(StanzaID) > 6,
     true = is_integer(calendar:rfc3339_to_system_time(binary_to_list(TS))),
     {ok, #xmlel{name = <<"message">>}} = exml:parse(XML).

spoofed_error(Call, Response) ->
    null = graphql_helper:get_err_value([data, stanza, Call], Response),
    ?assertEqual(<<"Sender's JID is different from the user's JID">>, get_err_msg(Response)),
    ?assertEqual(<<"invalid_sender">>, get_err_code(Response)).
