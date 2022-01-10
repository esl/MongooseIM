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
    [{group, admin_stanza_category}].

groups() ->
    [{admin_stanza_category, [parallel], admin_stanza_category()}].

admin_stanza_category() ->
    [send_message,
     send_message_to_unparsable_jid,
     send_message_headline,
     send_stanza,
     send_unparsable_stanza,
     send_stanza_from_non_existing_user,
     send_stanza_from_unknown_domain,
     get_last_messages,
     get_last_messages_for_unknown_user,
     get_last_messages_with,
     get_last_messages_limit,
     get_last_messages_limit_enforced,
     get_last_messages_before].

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
init_per_group(_, Config) ->
    Config.

end_per_group(admin_stanza_category, _Config) ->
    dynamic_modules:ensure_stopped(domain_helper:host_type(), [mod_mam_meta]);
end_per_group(_, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

init_mam(Config) when is_list(Config) ->
    case mam_helper:backend() of
        disabled ->
            {skip, no_backend};
        Backend ->
            Mods = [{mod_mam_meta, [{backend, Backend}, {pm, []}]}],
            dynamic_modules:ensure_modules(domain_helper:host_type(), Mods),
            Config
    end;
init_mam(Other) ->
    Other.

%% Test Cases

send_message(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun send_message_story/3).

send_message_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Vars = #{from => escalus_client:full_jid(Alice),
             to => escalus_client:short_jid(Bob),
             body => Body},
    Res = ok_result(<<"stanza">>, <<"sendMessage">>,
                    execute_send_message(Vars, Config)),
    #{<<"id">> := MamID} = Res,
    assert_not_empty(MamID).

send_message_to_unparsable_jid(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun send_message_to_unparsable_jid_story/2).

send_message_to_unparsable_jid_story(Config, Alice) ->
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

send_message_headline(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun send_message_headline_story/3).

send_message_headline_story(Config, Alice, Bob) ->
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

send_stanza(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun send_stanza_story/3).

send_stanza_story(Config, Alice, Bob) ->
    Body = <<"Hi!">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), Alice),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = ok_result(<<"stanza">>, <<"sendStanza">>, execute_send_stanza(Vars, Config)),
    #{<<"id">> := MamID} = Res,
    assert_not_empty(MamID).

send_unparsable_stanza(Config) ->
    Vars = #{stanza => <<"<test">>},
    Res = execute_send_stanza(Vars, Config),
    {{<<"400">>, <<"Bad Request">>}, #{<<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"input_coercion">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual(<<"Input coercion failed for type Stanza with value <<\"<test\">>. "
                   "The reason it failed is: \"expected >\"">>, ErrMsg),
    ?assertEqual([<<"M1">>, <<"stanza">>], ErrPath).

send_stanza_from_non_existing_user(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun send_stanza_from_non_existing_user_story/2).

send_stanza_from_non_existing_user_story(Config, Bob) ->
    Body = <<"Hi!">>,
    Server = escalus_client:server(Bob),
    From = <<"YeeeAH@", Server/binary>>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = execute_send_stanza(Vars, Config),
    {{<<"200">>,<<"OK">>},
         #{<<"data">> := #{<<"stanza">> := #{<<"sendStanza">> := null}},
           <<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"resolver_error">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual(<<"#{jid => <<\"YeeeAH@", Server/binary,
                   "\">>,what => non_existing_user}">>, ErrMsg),
    ?assertEqual([<<"stanza">>, <<"sendStanza">>], ErrPath).

send_stanza_from_unknown_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
                                    fun send_stanza_from_unknown_domain_story/2).

send_stanza_from_unknown_domain_story(Config, Bob) ->
    Body = <<"Hi!">>,
    From = <<"YeeeAH@oopsie">>,
    Stanza = escalus_stanza:from(escalus_stanza:chat_to_short_jid(Bob, Body), From),
    Vars = #{stanza => exml:to_binary(Stanza)},
    Res = execute_send_stanza(Vars, Config),
    {{<<"200">>, <<"OK">>},
     #{<<"data">> := #{<<"stanza">> := #{<<"sendStanza">> := null}},
       <<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"resolver_error">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual([<<"stanza">>, <<"sendStanza">>], ErrPath),
    ?assertEqual(<<"#{domain => <<\"oopsie\">>,what => unknown_domain}">>, ErrMsg).

get_last_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun get_last_messages_story/3).

get_last_messages_story(Config, Alice, Bob) ->
    send_message_story(Config, Alice, Bob),
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

get_last_messages_for_unknown_user(Config) ->
    Domain = domain_helper:domain(),
    Vars = #{caller => <<"maybemaybebutnot@", Domain/binary>>},
    Res = execute_get_last_messages(Vars, Config),
    {{<<"200">>, <<"OK">>},
     #{<<"data">> := #{<<"stanza">> := #{<<"getLastMessages">> := null}},
       <<"errors">> := Errors}} = Res,
    [#{<<"extensions">> := #{<<"code">> := <<"resolver_error">>},
       <<"message">> := ErrMsg, <<"path">> := ErrPath}] = Errors,
    ?assertEqual([<<"stanza">>, <<"getLastMessages">>], ErrPath),
    ?assertEqual(<<"#{jid => <<\"maybemaybebutnot@", Domain/binary, "\">>,"
                     "what => non_existing_user}">>, ErrMsg).

get_last_messages_with(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun get_last_messages_with_story/4).

get_last_messages_with_story(Config, Alice, Bob, Kate) ->
    send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    send_message_story(Config, Kate, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    Vars = #{caller => escalus_client:full_jid(Alice),
             with => escalus_client:short_jid(Bob)},
    Res = ok_result(<<"stanza">>, <<"getLastMessages">>,
                    execute_get_last_messages(Vars, Config)),
    #{<<"stanzas">> := [M1], <<"limit">> := 50} = Res,
    check_stanza_map(M1, Alice).

get_last_messages_limit(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun get_last_messages_limit_story/3).

get_last_messages_limit_story(Config, Alice, Bob) ->
    send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    send_message_story(Config, Bob, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    Vars = #{caller => escalus_client:full_jid(Alice), limit => 1},
    Res = ok_result(<<"stanza">>, <<"getLastMessages">>,
                    execute_get_last_messages(Vars, Config)),
    #{<<"stanzas">> := [M1], <<"limit">> := 1} = Res,
    check_stanza_map(M1, Bob).

get_last_messages_limit_enforced(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun get_last_messages_limit_enforced_story/3).

get_last_messages_limit_enforced_story(Config, Alice, Bob) ->
    send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    Vars = #{caller => escalus_client:full_jid(Alice), limit => 1000},
    Res = ok_result(<<"stanza">>, <<"getLastMessages">>,
                    execute_get_last_messages(Vars, Config)),
    %% The actual limit is returned
    #{<<"stanzas">> := [M1], <<"limit">> := 500} = Res,
    check_stanza_map(M1, Alice).

get_last_messages_before(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun get_last_messages_before_story/3).

get_last_messages_before_story(Config, Alice, Bob) ->
    send_message_story(Config, Alice, Bob),
    mam_helper:wait_for_archive_size(Alice, 1),
    send_message_story(Config, Bob, Alice),
    mam_helper:wait_for_archive_size(Alice, 2),
    send_message_story(Config, Bob, Alice),
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

execute_send_message_headline(Vars, Config) ->
    Q = <<"mutation M1($from: JID!, $to: JID!, $subject: String, $body: String) "
          "{ stanza { sendMessageHeadLine("
            "from: $from, to: $to, subject: $subject, body: $body) { id } } }">>,
    execute_auth(#{query => Q, variables => Vars,
                   operationName => <<"M1">>}, Config).

execute_send_stanza(Vars, Config) ->
    Q = <<"mutation M1($stanza: Stanza!) "
          "{ stanza { sendStanza(stanza: $stanza) { id } } }">>,
    execute_auth(#{query => Q, variables => Vars,
                   operationName => <<"M1">>}, Config).

execute_get_last_messages(Vars, Config) ->
    Q = <<"query Q1($caller: JID!, $with: JID, $limit: Int, $before: DateTime) "
          "{ stanza { getLastMessages(caller: $caller, with: $with, "
                 " limit: $limit, before: $before) "
                     "{ stanzas { stanza_id stanza sender timestamp } limit } } }">>,
    execute_auth(#{query => Q, variables => Vars,
                   operationName => <<"Q1">>}, Config).

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
