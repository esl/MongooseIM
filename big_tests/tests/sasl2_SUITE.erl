-module(sasl2_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-define(NS_SASL_2, <<"urn:xmpp:sasl:2">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, all_tests}
    ].

groups() ->
    [
     {all_tests, [parallel],
      [
       {group, basic},
       {group, scram},
       {group, stream_management}
      ]},
     {basic, [parallel],
      [
       server_does_not_announce_if_not_tls,
       server_announces_sasl2_with_some_mechanism_and_inline_sm,
       authenticate_stanza_has_invalid_mechanism,
       user_agent_is_invalid,
       user_agent_is_invalid_uuid_but_not_v4,
       authenticate_with_plain,
       authenticate_with_plain_and_user_agent_without_id,
       authenticate_again_results_in_stream_error
      ]},
     {scram, [parallel],
      [
       authenticate_with_scram_abort,
       authenticate_with_scram_bad_abort,
       authenticate_with_scram_bad_response,
       authenticate_with_scram
      ]},
     {stream_management, [parallel],
      [
       sm_failure_missing_previd_does_not_stop_sasl2,
       sm_failure_invalid_h_does_not_stop_sasl2,
       sm_failure_exceeding_h_does_not_stop_sasl2,
       sm_failure_unknown_smid_does_not_stop_sasl2,
       sm_is_bound_at_sasl2_success
      ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = load_sasl_extensible(Config),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(scram, Config) ->
    case mongoose_helper:supports_sasl_module(cyrsasl_scram_sha256) of
        false ->
            {skip, "scram password type not supported"};
        true ->
            Config
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

load_sasl_extensible(Config) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    sasl2_helper:load_all_sasl2_modules(HostType),
    Config1.

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------

server_does_not_announce_if_not_tls(Config) ->
    Steps = [connect_non_tls_user, start_stream_get_features],
    #{features := Features} = sasl2_helper:apply_steps(Steps, Config),
    Sasl2 = exml_query:path(Features, [{element_with_ns, <<"authentication">>, ?NS_SASL_2}]),
    ?assertEqual(undefined, Sasl2).

server_announces_sasl2_with_some_mechanism_and_inline_sm(Config) ->
    Steps = [create_connect_tls, start_stream_get_features],
    #{features := Features} = sasl2_helper:apply_steps(Steps, Config),
    Sasl2 = exml_query:path(Features, [{element_with_ns, <<"authentication">>, ?NS_SASL_2}]),
    ?assertNotEqual(undefined, Sasl2),
    Mechs = exml_query:paths(Sasl2, [{element, <<"mechanism">>}]),
    ?assertNotEqual([], Mechs),
    Sm = exml_query:path(Sasl2, [{element, <<"inline">>},
                                 {element_with_ns, <<"sm">>, ?NS_STREAM_MGNT_3}]),
    ?assertNotEqual(undefined, Sm).

authenticate_stanza_has_invalid_mechanism(Config) ->
    Steps = [start_new_user, send_invalid_mech_auth_stanza],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"failure">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Response).

user_agent_is_invalid(Config) ->
    Steps = [start_new_user, send_bad_user_agent],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>], Response).

user_agent_is_invalid_uuid_but_not_v4(Config) ->
    Steps = [start_new_user, send_bad_user_agent_uuid],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>], Response).

authenticate_with_plain(Config) ->
    Steps = [start_new_user, plain_authentication, receive_features],
    auth_with_plain(Steps, Config).

authenticate_with_plain_and_user_agent_without_id(Config) ->
    Steps = [start_new_user, plain_auth_user_agent_without_id, receive_features],
    auth_with_plain(Steps, Config).

auth_with_plain(Steps, Config) ->
    #{answer := Success, features := Features} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    CData = exml_query:path(Success, [{element, <<"additional-data">>}, cdata], <<>>),
    ?assert(is_binary(CData) andalso 0 =< byte_size(CData)),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata], <<>>),
    ?assertNotEqual(error, jid:from_binary(Identifier)),
    ?assertMatch(#xmlel{name = <<"stream:features">>}, Features).

authenticate_with_scram_abort(Config) ->
    Steps = [start_new_user, scram_step_1, scram_abort],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"failure">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Response),
    Aborted = exml_query:path(Response, [{element_with_ns, <<"aborted">>, ?NS_SASL}]),
    ?assertNotEqual(undefined, Aborted).

authenticate_with_scram_bad_abort(Config) ->
    Steps = [start_new_user, scram_step_1, scram_bad_abort],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    escalus:assert(is_stream_error, [<<"invalid-namespace">>, <<>>], Response).

authenticate_with_scram_bad_response(Config) ->
    Steps = [start_new_user, scram_step_1, scram_bad_ns_response],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    escalus:assert(is_stream_error, [<<"invalid-namespace">>, <<>>], Response).

authenticate_with_scram(Config) ->
    Steps = [start_new_user, scram_step_1, scram_step_2, receive_features],
    #{answer := Success, features := Features} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    CData = exml_query:path(Success, [{element, <<"additional-data">>}, cdata], <<>>),
    ?assert(is_binary(CData) andalso 0 =< byte_size(CData)),
    Identifier = exml_query:path(Success, [{element, <<"authorization-identifier">>}, cdata], <<>>),
    ?assertNotEqual(error, jid:from_binary(Identifier)),
    ?assertMatch(#xmlel{name = <<"stream:features">>}, Features).

authenticate_again_results_in_stream_error(Config) ->
    Steps = [start_new_user, plain_authentication, receive_features, plain_authentication],
    #{answer := Response} = sasl2_helper:apply_steps(Steps, Config),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>], Response).

sm_failure_missing_previd_does_not_stop_sasl2(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             auth_with_resumption_missing_previd, receive_features],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"failed">>, ?NS_STREAM_MGNT_3}]),
    escalus:assert(is_sm_failed, [<<"bad-request">>], Resumed).

sm_failure_invalid_h_does_not_stop_sasl2(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             auth_with_resumption_invalid_h, receive_features],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"failed">>, ?NS_STREAM_MGNT_3}]),
    escalus:assert(is_sm_failed, [<<"bad-request">>], Resumed).

sm_failure_exceeding_h_does_not_stop_sasl2(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             auth_with_resumption_exceeding_h, receive_features],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"failed">>, ?NS_STREAM_MGNT_3}]),
    escalus:assert(is_sm_failed, [<<"bad-request">>], Resumed).

sm_failure_unknown_smid_does_not_stop_sasl2(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             auth_with_resumption_unknown_smid, receive_features],
    #{answer := Success} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"failed">>, ?NS_STREAM_MGNT_3}]),
    escalus:assert(is_sm_failed, [<<"item-not-found">>], Resumed).

sm_is_bound_at_sasl2_success(Config) ->
    Steps = [create_user, buffer_messages_and_die, connect_tls, start_stream_get_features,
             auth_with_resumption, has_no_more_stanzas, can_send_messages],
    #{answer := Success, smid := SMID} = sasl2_helper:apply_steps(Steps, Config),
    ?assertMatch(#xmlel{name = <<"success">>, attrs = [{<<"xmlns">>, ?NS_SASL_2}]}, Success),
    Resumed = exml_query:path(Success, [{element_with_ns, <<"resumed">>, ?NS_STREAM_MGNT_3}]),
    ?assert(escalus_pred:is_sm_resumed(SMID, Resumed)).
