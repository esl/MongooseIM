-module(graphql_stanza_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(graphql_helper, [execute_auth/2]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_stanza_category},
     {group, user_stanza_caregory}].

groups() ->
    [{admin_stanza_category, [parallel], admin_stanza_category()},
     {user_stanza_caregory, [parallel], user_stanza_caregory()}].

admin_stanza_category() ->
    [admin_send_message,
     admin_send_message_to_unparsable_jid,
     admin_send_message_headline,
     admin_send_stanza,
     admin_send_unparsable_stanza,
     admin_send_stanza_from_unknown_user,
     admin_send_stanza_from_unknown_domain]
    ++ admin_get_last_messages_cases().

admin_get_last_messages_cases() ->
    [admin_get_last_messages,
     admin_get_last_messages_for_unknown_user,
     admin_get_last_messages_with,
     admin_get_last_messages_limit,
     admin_get_last_messages_limit_enforced,
     admin_get_last_messages_before].

user_stanza_caregory() ->
    [user_send_message,
     user_send_message_without_from,
     user_send_message_with_spoofed_from,
     user_send_message_headline,
     user_send_message_headline_with_spoofed_from,
     user_send_stanza,
     user_send_stanza_with_spoofed_from]
    ++ user_get_last_messages_cases().

user_get_last_messages_cases() ->
    [user_get_last_messages].

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    dynamic_modules:save_modules(domain_helper:host_type(), Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_stanza_category, Config) ->
    Config2 = graphql_helper:init_admin_handler(Config),
    init_mam(Config2);
init_per_group(user_stanza_caregory, Config) ->
    init_mam(Config).

end_per_group(_, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    HasMam = proplists:get_value(has_mam, Config, false),
    MamOnly = lists:member(CaseName,
                           user_get_last_messages_cases()
                           ++ admin_get_last_messages_cases()),
    case {HasMam, MamOnly} of
        {false, true} ->
            {skip, no_mam};
        _ ->
            escalus:init_per_testcase(CaseName, Config)
    end.

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

init_mam(Config) when is_list(Config) ->
    case mam_helper:backend() of
        disabled ->
            Config;
        Backend ->
            Mods = [{mod_mam_meta, mam_helper:config_opts(#{backend => Backend, pm => #{}})}],
            dynamic_modules:ensure_modules(domain_helper:host_type(), Mods),
            [{has_mam, true}|Config]
    end;
init_mam(Other) ->
    Other.

%% Test Cases

admin_send_message(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_story/3).

admin_send_message_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:full_jid(Alice),
             to => escalus_client:short_jid(Bob),
             body => Body},
    Res = ok_result(<<"stanza">>, <<"sendMessage">>,
                    execute_send_message(Vars, Config)),
    #{<<"id">> := MamID} = Res,
    assert_not_empty(MamID, Config).

user_send_message(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_story/3).

user_send_message_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:full_jid(Alice),
             to => escalus_client:short_jid(Bob),
             body => Body},
    Res = ok_result(<<"stanza">>, <<"sendMessage">>,
                    execute_user_send_message(Alice, Vars, Config)),
    #{<<"id">> := MamID} = Res,
    assert_not_empty(MamID, Config),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

user_send_message_without_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_without_from_story/3).

user_send_message_without_from_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Vars = #{to => escalus_client:short_jid(Bob),
             body => Body},
    Res = ok_result(<<"stanza">>, <<"sendMessage">>,
                    execute_user_send_message(Alice, Vars, Config)),
    #{<<"id">> := MamID} = Res,
    assert_not_empty(MamID, Config),
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

user_send_message_with_spoofed_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_with_spoofed_from_story/3).

user_send_message_with_spoofed_from_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:short_jid(Bob),
             to => escalus_client:short_jid(Bob),
             body => Body},
    Res = execute_user_send_message(Alice, Vars, Config),
    spoofed_error(<<"sendMessage">>, Res).

admin_send_message_to_unparsable_jid(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_send_message_to_unparsable_jid_story/2).

admin_send_message_to_unparsable_jid_story(Config, Alice) ->
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:full_jid(Alice),
             to => <<"test@">>,
             body => Body},
    Res = execute_send_message(Vars, Config),
    {{<<"400">>, <<"Bad Request">>}, #{<<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"input_coercion">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual([<<"M1">>, <<"to">>], ErrPath),
    ?assertEqual(<<"Input coercion failed for type JID with value "
                   "<<\"test@\">>. The reason it failed is: failed_to_parse_jid">>,
                 ErrMsg).

admin_send_message_headline(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_headline_story/3).

admin_send_message_headline_story(Config, Alice, Bob) ->
    Subject = <<"Welcome">>,
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:full_jid(Alice),
             to => escalus_client:short_jid(Bob),
             subject => Subject, body => Body},
    Res = ok_result(<<"stanza">>, <<"sendMessageHeadLine">>,
                    execute_send_message_headline(Vars, Config)),
    #{<<"id">> := MamID} = Res,
    %% Headlines are not stored in MAM
    <<>> = MamID.

user_send_message_headline(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_headline_story/3).

user_send_message_headline_story(Config, Alice, Bob) ->
    Subject = <<"Welcome">>,
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:full_jid(Alice),
             to => escalus_client:short_jid(Bob),
             subject => Subject, body => Body},
    Res = ok_result(<<"stanza">>, <<"sendMessageHeadLine">>,
                    execute_user_send_message_headline(Alice, Vars, Config)),
    #{<<"id">> := MamID} = Res,
    %% Headlines are not stored in MAM
    <<>> = MamID,
    escalus:assert(is_message, escalus:wait_for_stanza(Bob)).

user_send_message_headline_with_spoofed_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_headline_with_spoofed_from_story/3).

user_send_message_headline_with_spoofed_from_story(Config, Alice, Bob) ->
    Subject = <<"Welcome">>,
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:short_jid(Bob),
             to => escalus_client:short_jid(Bob),
             subject => Subject, body => Body},
    Res = execute_user_send_message_headline(Alice, Vars, Config),
    spoofed_error(<<"sendMessageHeadLine">>, Res).

admin_send_stanza(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_stanza_story/3).

admin_send_stanza_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Alice),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = ok_result(<<"stanza">>, <<"sendStanza">>, execute_send_stanza(Vars, Config)),
    #{<<"id">> := MamID} = Res,
    assert_not_empty(MamID, Config).

user_send_stanza(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_stanza_story/3).

user_send_stanza_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Alice),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = ok_result(<<"stanza">>, <<"sendStanza">>,
                    execute_user_send_stanza(Alice, Vars, Config)),
    #{<<"id">> := MamID} = Res,
    assert_not_empty(MamID, Config).

user_send_stanza_with_spoofed_from(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_stanza_with_spoofed_from_story/3).

user_send_stanza_with_spoofed_from_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Bob),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = execute_user_send_stanza(Alice, Vars, Config),
    spoofed_error(<<"sendStanza">>, Res).

admin_send_unparsable_stanza(Config) ->
    Vars = #{stanza => <<"<test">>},
    Res = execute_send_stanza(Vars, Config),
    {{<<"400">>, <<"Bad Request">>}, #{<<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"input_coercion">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual(<<"Input coercion failed for type Stanza with value <<\"<test\">>. "
                   "The reason it failed is: \"expected >\"">>, ErrMsg),
    ?assertEqual([<<"M1">>, <<"stanza">>], ErrPath).

admin_send_stanza_from_unknown_user(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun admin_send_stanza_from_unknown_user_story/2).

admin_send_stanza_from_unknown_user_story(Config, Bob) ->
    Body = <<"Hi!">>,
    Server = escalus_client:server(Bob),
    From = <<"YeeeAH@", Server/binary>>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = execute_send_stanza(Vars, Config),
    {{<<"200">>,<<"OK">>},
         #{<<"data">> := #{<<"stanza">> := #{<<"sendStanza">> := null}},
           <<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"unknown_user">>,
                             <<"jid">> := From},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual(<<"Given user does not exist">>, ErrMsg),
    ?assertEqual([<<"stanza">>, <<"sendStanza">>], ErrPath).

admin_send_stanza_from_unknown_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun admin_send_stanza_from_unknown_domain_story/2).

admin_send_stanza_from_unknown_domain_story(Config, Bob) ->
    Body = <<"Hi!">>,
    From = <<"YeeeAH@oopsie">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = execute_send_stanza(Vars, Config),
    {{<<"200">>, <<"OK">>},
     #{<<"data">> := #{<<"stanza">> := #{<<"sendStanza">> := null}},
       <<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"unknown_domain">>,
                             <<"domain">> := <<"oopsie">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual([<<"stanza">>, <<"sendStanza">>], ErrPath),
    ?assertEqual(<<"Given domain does not exist">>, ErrMsg).

admin_get_last_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_story/3).

admin_get_last_messages_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    mam_helper:wait_for_archive_size(Bob, 1),
    Vars1 = #{caller => escalus_client:full_jid(Alice)},
    Vars2 = #{caller => escalus_client:full_jid(Bob)},
    Res1 = ok_result(<<"stanza">>, <<"getLastMessages">>,
                     execute_get_last_messages(Vars1, Config)),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} = Res1,
    check_stanza_map(M1, Alice),
    Res2 = ok_result(<<"stanza">>, <<"getLastMessages">>,
                     execute_get_last_messages(Vars2, Config)),
    #{<<"stanzas">> := [M2], <<"limit">> := 50} = Res2,
    check_stanza_map(M2, Alice).

user_get_last_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_last_messages_story/3).

user_get_last_messages_story(Config, Alice, Bob) ->
    user_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    mam_helper:wait_for_archive_size(Bob, 1),
    Vars1 = #{caller => escalus_client:full_jid(Alice)},
    Vars2 = #{caller => escalus_client:full_jid(Bob)},
    Res1 = ok_result(<<"stanza">>, <<"getLastMessages">>,
                     execute_user_get_last_messages(Alice, Vars1, Config)),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} = Res1,
    check_stanza_map(M1, Alice),
    Res2 = ok_result(<<"stanza">>, <<"getLastMessages">>,
                     execute_user_get_last_messages(Alice, Vars2, Config)),
    #{<<"stanzas">> := [M2], <<"limit">> := 50} = Res2,
    check_stanza_map(M2, Alice).

admin_get_last_messages_for_unknown_user(Config) ->
    Domain = domain_helper:domain(),
    Jid = <<"maybemaybebutnot@", Domain/binary>>,
    Vars = #{caller => Jid},
    Res = execute_get_last_messages(Vars, Config),
    {{<<"200">>, <<"OK">>},
     #{<<"data">> := #{<<"stanza">> := #{<<"getLastMessages">> := null}},
       <<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"unknown_user">>,
                             <<"jid">> := Jid},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual([<<"stanza">>, <<"getLastMessages">>], ErrPath),
    ?assertEqual(<<"Given user does not exist">>, ErrMsg).

admin_get_last_messages_with(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_get_last_messages_with_story/4).

admin_get_last_messages_with_story(Config, Alice, Bob, Kate) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    admin_send_message_story(Config, Kate, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    Vars = #{caller => escalus_client:full_jid(Alice),
             with => escalus_client:short_jid(Bob)},
    Res = ok_result(<<"stanza">>, <<"getLastMessages">>,
                    execute_get_last_messages(Vars, Config)),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} = Res,
    check_stanza_map(M1, Alice).

admin_get_last_messages_limit(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_limit_story/3).

admin_get_last_messages_limit_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    admin_send_message_story(Config, Bob, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    Vars = #{caller => escalus_client:full_jid(Alice), limit => 1},
    Res = ok_result(<<"stanza">>, <<"getLastMessages">>,
                    execute_get_last_messages(Vars, Config)),
    #{<<"stanzas">> := [M1], <<"limit">> := 1} = Res,
    check_stanza_map(M1, Bob).

admin_get_last_messages_limit_enforced(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_last_messages_limit_enforced_story/3).

admin_get_last_messages_limit_enforced_story(Config, Alice, Bob) ->
    admin_send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    Vars = #{caller => escalus_client:full_jid(Alice), limit => 1000},
    Res = ok_result(<<"stanza">>, <<"getLastMessages">>,
                    execute_get_last_messages(Vars, Config)),
    %% The actual limit is returned
    #{<<"stanzas">> := [M1], <<"limit">> := 500} = Res,
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
    Vars1 = #{caller => escalus_client:full_jid(Alice)},
    Res1 = ok_result(<<"stanza">>, <<"getLastMessages">>,
                     execute_get_last_messages(Vars1, Config)),
    #{<<"stanzas">> := [M1, M2, _M3], <<"limit">> := 50} = Res1,
    Vars2 = #{caller => escalus_client:full_jid(Alice),
              before => maps:get(<<"timestamp">>, M2)},
    Res2 = ok_result(<<"stanza">>, <<"getLastMessages">>,
                     execute_get_last_messages(Vars2, Config)),
    #{<<"stanzas">> := [M1, M2], <<"limit">> := 50} = Res2.

%% Helpers

execute_send_message(Vars, Config) ->
    Q = <<"mutation M1($from: JID!, $to: JID!, $body: String!) "
          "{ stanza { sendMessage(from: $from, to: $to, body: $body) { id } } }">>,
    execute_auth(#{query => Q, variables => Vars,
                   operationName => <<"M1">>}, Config).

execute_user_send_message(User, Vars, _Config) ->
    Creds = graphql_helper:make_creds(User),
    Q = <<"mutation M1($from: JID, $to: JID!, $body: String!) "
          "{ stanza { sendMessage(from: $from, to: $to, body: $body) { id } } }">>,
    QQ = #{query => Q, variables => Vars, operationName => <<"M1">>},
    graphql_helper:execute(user, QQ, Creds).

execute_send_message_headline(Vars, Config) ->
    Q = <<"mutation M1($from: JID!, $to: JID!, $subject: String, $body: String) "
          "{ stanza { sendMessageHeadLine("
            "from: $from, to: $to, subject: $subject, body: $body) { id } } }">>,
    execute_auth(#{query => Q, variables => Vars,
                   operationName => <<"M1">>}, Config).

execute_user_send_message_headline(User, Vars, _Config) ->
    Creds = graphql_helper:make_creds(User),
    Q = <<"mutation M1($from: JID, $to: JID!, $subject: String, $body: String) "
          "{ stanza { sendMessageHeadLine("
            "from: $from, to: $to, subject: $subject, body: $body) { id } } }">>,
    QQ = #{query => Q, variables => Vars, operationName => <<"M1">>},
    graphql_helper:execute(user, QQ, Creds).

execute_send_stanza(Vars, Config) ->
    Q = <<"mutation M1($stanza: Stanza!) "
          "{ stanza { sendStanza(stanza: $stanza) { id } } }">>,
    execute_auth(#{query => Q, variables => Vars,
                   operationName => <<"M1">>}, Config).

execute_user_send_stanza(User, Vars, _Config) ->
    Creds = graphql_helper:make_creds(User),
    Q = <<"mutation M1($stanza: Stanza!) "
          "{ stanza { sendStanza(stanza: $stanza) { id } } }">>,
    QQ = #{query => Q, variables => Vars, operationName => <<"M1">>},
    graphql_helper:execute(user, QQ, Creds).

execute_get_last_messages(Vars, Config) ->
    Q = <<"query Q1($caller: JID!, $with: JID, $limit: Int, $before: DateTime) "
          "{ stanza { getLastMessages(caller: $caller, with: $with, "
                 " limit: $limit, before: $before) "
                     "{ stanzas { stanza_id stanza sender timestamp } limit } } }">>,
    execute_auth(#{query => Q, variables => Vars,
                   operationName => <<"Q1">>}, Config).

execute_user_get_last_messages(User, Vars, _Config) ->
    Creds = graphql_helper:make_creds(User),
    Q = <<"query Q1($with: JID, $limit: Int, $before: DateTime) "
          "{ stanza { getLastMessages(with: $with, limit: $limit, before: $before) "
                     "{ stanzas { stanza_id stanza sender timestamp } limit } } }">>,
    QQ = #{query => Q, variables => Vars, operationName => <<"Q1">>},
    graphql_helper:execute(user, QQ, Creds).

assert_not_empty(Bin, Config) ->
    case proplists:get_value(has_mam, Config) of
        true ->
            assert_not_empty(Bin);
        _ ->
            skip
    end.

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

spoofed_error(Call, Res) ->
    {{<<"200">>, <<"OK">>},
     #{<<"data">> := #{<<"stanza">> := #{Call := null}},
       <<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"bad_from_jid">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual([<<"stanza">>, Call], ErrPath),
    ?assertEqual(<<"Sending from this JID is not allowed">>, ErrMsg).
