%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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
-module(mam_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             subhost_pattern/1,
                             rpc/4]).

-import(muc_helper,
        [muc_host/0,
         room_address/1, room_address/2,
         stanza_muc_enter_room/2,
         stanza_to_room/2]).

-import(mam_helper,
        [rpc_apply/3,
         get_prop/2,
         is_cassandra_enabled/1,
         is_elasticsearch_enabled/1,
         is_mam_possible/1,
         respond_iq/1,
         print_configuration_not_supported/2,
         start_alice_room/1,
         destroy_room/1,
         send_muc_rsm_messages/1,
         send_rsm_messages/1,
         clean_archives/1,
         mam04_props/0,
         mam06_props/0,
         bootstrap_archive/1,
         muc_bootstrap_archive/1,
         start_alice_protected_room/1,
         start_alice_anonymous_room/1,
         maybe_wait_for_archive/1,
         stanza_archive_request/2,
         stanza_text_search_archive_request/3,
         stanza_include_groupchat_request/3,
         stanza_fetch_by_id_request/3,
         stanza_fetch_by_id_request/4,
         stanza_date_range_archive_request_not_empty/3,
         wait_archive_respond/1,
         wait_for_complete_archive_response/3,
         assert_respond_size/2,
         assert_respond_query_id/3,
         parse_result_iq/1,
         nick_to_jid/2,
         stanza_filtered_by_jid_request/2,
         nick/1,
         respond_messages/1,
         parse_forwarded_message/1,
         login_send_presence/2,
         assert_only_one_of_many_is_equal/2,
         add_nostore_hint/1,
         assert_not_stored/2,
         verify_id_error_text_msg/2,
         has_x_user_element/1,
         stanza_date_range_archive_request/1,
         make_iso_time/1,
         stanza_retrieve_form_fields/2,
         stanza_limit_archive_request/1,
         rsm_send/3,
         stanza_page_archive_request/3,
         stanza_flip_page_archive_request/3,
         wait_empty_rset/2,
         wait_message_range/2,
         wait_message_range/3,
         wait_message_range/5,
         message_id/2,
         get_pre_generated_msgs_ids/2,
         get_received_msgs_ids/1,
         stanza_prefs_set_request/4,
         stanza_prefs_get_request/1,
         stanza_query_get_request/1,
         parse_prefs_result_iq/1,
         mam_ns_binary/0,
         mam_ns_binary_v04/0,
         mam_ns_binary_v06/0,
         mam_ns_binary_extended/0,
         retract_ns/0,
         retract_tombstone_ns/0,
         groupchat_field_ns/0,
         groupchat_available_ns/0,
         data_validate_ns/0,
         make_alice_and_bob_friends/2,
         run_prefs_case/6,
         muc_run_prefs_case/6,
         prefs_cases2/0,
         prefs_cases2_muc/0,
         default_policy/1,
         get_all_messages/2,
         parse_messages/1,
         run_set_and_get_prefs_case/4,
         muc_light_host/0,
         host_type/0,
         config_opts/1,
         stanza_metadata_request/0,
         assert_archive_message_event/2,
         assert_lookup_event/2,
         assert_dropped_msg_event/2,
         assert_flushed_event_if_async/2,
         assert_async_batch_flush_event/3,
         assert_async_timed_flush_event/3,
         assert_dropped_iq_event/2,
         assert_event_with_jid/2,
         assert_no_event_with_jid/2
        ]).

-import(muc_light_helper,
        [given_muc_light_room/3,
         when_muc_light_message_is_sent/4,
         then_muc_light_message_is_received_by/2,
         when_muc_light_affiliations_are_set/3,
         then_muc_light_affiliations_are_received_by/2,
         when_archive_query_is_sent/3,
         then_archive_response_is/3]).

-import(domain_helper, [domain/0]).

-include("mam_helper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml_stream.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------



configurations() ->
    case ct_helper:is_ct_running() of
        true ->
            configurations_for_running_ct();
        false ->
            all_configurations()
    end.

%% Called by test-runner for autocompletion
all_configurations() ->
    cassandra_configs(true)
    ++ rdbms_configs(true, mnesia)
    ++ elasticsearch_configs(true).

configurations_for_running_ct() ->
    cassandra_configs(is_cassandra_enabled(host_type()))
    ++ rdbms_configs(mongoose_helper:is_rdbms_enabled(host_type()), ct_helper:get_internal_database())
    ++ elasticsearch_configs(is_elasticsearch_enabled(host_type())).

rdbms_configs(true, mnesia) ->
    [rdbms,
     rdbms_easy,
     rdbms_async_pool,
     rdbms_mnesia,
     rdbms_async_cache,
     rdbms_cache,
     rdbms_mnesia_cache
    ];
rdbms_configs(true, cets) ->
    [rdbms,
     rdbms_no_prefs,
     rdbms_easy,
     rdbms_async_pool,
     rdbms_async_cache,
     rdbms_cache
    ];
rdbms_configs(_, _) ->
    [].

cassandra_configs(true) ->
     [cassandra,
      cassandra_eterm];
cassandra_configs(_) ->
     [].

elasticsearch_configs(true) ->
    [elasticsearch];
elasticsearch_configs(_) ->
    [].

basic_group_names() ->
    [
     mam_all,
     chat_markers,
     muc_all,
     muc_light,
     prefs_cases,
     muc_prefs_cases,
     no_prefs_cases,
     impl_specific,
     disabled_text_search,
     disabled_complex_queries,
     disabled_retraction
    ].

all() ->
    Reasons =
    case ct_helper:is_ct_running() of
        true ->
            case is_mam_possible(host_type())  of
                false -> [require_rdbms];
                true  -> []
            end;
        false ->
            []
    end,
    case Reasons of
        [] ->
            tests();
        [_|_] ->
            {skip, Reasons}
    end.

tests() ->
    [{group, full_group(C, G)}
     || C <- configurations(), G <- basic_group_names(),
        not is_skipped(C, G)].

groups() ->
    [{full_group(C, G), Props, Tests}
     || C <- configurations(), {G, Props, Tests} <- basic_groups(),
        not is_skipped(C, G)].

is_skipped(rdbms_no_prefs, G) ->
    G =:= prefs_cases orelse G =:= muc_prefs_cases;
is_skipped(_, no_prefs_cases) ->
    true;
is_skipped(_, _) ->
    false.

basic_groups() ->
    [
     {mam_all, [parallel],
           [{mam04, [parallel], mam_cases() ++ [retrieve_form_fields] ++ text_search_cases()
                                ++ [{stream_management, [], stream_management_cases()}]},
            {mam06, [parallel], mam_cases() ++ [retrieve_form_fields_extra_features]
                                ++ stanzaid_cases() ++ retract_cases()
                                ++ metadata_cases() ++ fetch_specific_msgs_cases()
                                ++ [{stream_management, [], stream_management_cases()}]},
            {nostore, [parallel], nostore_cases()},
            {archived, [parallel], archived_cases()},
            {configurable_archiveid, [], configurable_archiveid_cases()},
            % Due to the mocking of the DB, the message_dropped test cannot be run in parallel
            {drop_msg, [], [message_dropped]},
            {rsm_all, [], %% not parallel, because we want to limit concurrency
             [
              %% Functions mod_mam_utils:make_fin_element_v03/5 and make_fin_element/5
              %% are almost the same, so don't need to test all versions of
              %% MAM protocol with complete_flag_cases.
              %%
              %% We need a separate group for complete_flag_cases,
              %% because there should not be a lot of cases running
              %% using parallel_story with the same user.
              %% Otherwise there would be a lot of presences sent between devices.
              {rsm04,      [parallel], rsm_cases()},
              {rsm04_comp, [parallel], complete_flag_cases()},
              {with_rsm04, [parallel], with_rsm_cases()}]}]},
     {muc_all, [parallel],
           [{muc04, [parallel], muc_cases() ++ muc_text_search_cases()},
            {muc06, [parallel], muc_cases() ++ muc_stanzaid_cases() ++ muc_retract_cases()
                                ++ muc_metadata_cases() ++ muc_fetch_specific_msgs_cases()},
            {muc_seq, [], [muc_validate_mam_id]},
            {muc_configurable_archiveid, [], muc_configurable_archiveid_cases()},
            {muc_drop_msg, [], [muc_message_dropped]},
            {muc_rsm_all, [parallel],
             [{muc_rsm04, [parallel], muc_rsm_cases()}]}]},
     {muc_light,        [], muc_light_cases()},
     {prefs_cases,      [parallel], prefs_cases()},
     {muc_prefs_cases,  [parallel], muc_prefs_cases()},
     {no_prefs_cases,   [parallel], no_prefs_cases()},
     {impl_specific,    [], impl_specific()},
     {disabled_text_search, [],
         [{mam04, [], disabled_text_search_cases()}]},
     {disabled_complex_queries, [],
         [{mam04, [], disabled_complex_queries_cases()}]},
     {chat_markers, [parallel],
         [{mam04, [parallel], chat_markers_cases()}]},
     {disabled_retraction, [],
      [{mam06, [parallel], disabled_retract_cases() ++
            [mam_service_discovery,
             mam_service_discovery_to_client_bare_jid,
             mam_service_discovery_to_different_client_bare_jid_results_in_error]}]},
     {muc_disabled_retraction, [],
      [{muc06, [parallel], disabled_muc_retract_cases() ++
            [muc_service_discovery]}]}
    ].

chat_markers_cases() ->
    [archive_chat_markers,
     dont_archive_chat_markers].

mam_cases() ->
    [mam_service_discovery,
     mam_service_discovery_to_client_bare_jid,
     mam_service_discovery_to_different_client_bare_jid_results_in_error,
     archive_is_instrumented,
     easy_archive_request,
     easy_archive_request_old_xmlel_format,
     easy_archive_request_for_the_receiver,
     message_sent_to_yourself,
     range_archive_request,
     range_archive_request_not_empty,
     limit_archive_request,
     querying_for_all_messages_with_jid,
     querying_for_all_messages_with_jid_after,
     querying_with_invalid_mam_id_in_after,
     unicode_messages_can_be_extracted
    ].

text_search_cases() ->
    [
     easy_text_search_request,
     long_text_search_request,
     text_search_is_available,
     save_unicode_messages
    ].

disabled_text_search_cases() ->
    [
     text_search_is_not_available,
     text_search_query_fails_if_disabled
    ].

disabled_complex_queries_cases() ->
    [
     pagination_simple_enforced
    ].

metadata_cases() ->
    [
     metadata_archive_request,
     metadata_archive_request_empty,
     metadata_archive_request_one_message
    ].

fetch_specific_msgs_cases() ->
    [
     query_messages_by_ids,
     simple_query_messages_by_ids,
     server_returns_item_not_found_for_ids_filter_with_nonexistent_id
    ].

muc_text_search_cases() ->
    [
     muc_text_search_request
    ].

archived_cases() ->
    [archived,
     filter_forwarded].

stanzaid_cases() ->
    [message_with_stanzaid,
     stanza_id_is_appended_to_carbons].

retract_cases() ->
    [retract_message,
     retract_wrong_message,
     ignore_bad_retraction].

disabled_retract_cases() ->
    [retract_message].

nostore_cases() ->
    [offline_message,
     nostore_hint].

muc_cases() ->
    [muc_service_discovery | muc_cases_with_room()].

muc_cases_with_room() ->
    [muc_archive_is_instrumented,
     muc_archive_request,
     muc_multiple_devices,
     muc_protected_message,
     muc_deny_protected_room_access,
     muc_allow_access_to_owner,
     muc_sanitize_x_user_in_non_anon_rooms,
     muc_delete_x_user_in_anon_rooms,
     muc_show_x_user_to_moderators_in_anon_rooms,
     muc_show_x_user_for_your_own_messages_in_anon_rooms,
     muc_querying_for_all_messages,
     muc_querying_for_all_messages_with_jid].

muc_stanzaid_cases() ->
    [muc_message_with_stanzaid].

muc_retract_cases() ->
    [retract_muc_message,
     retract_muc_message_on_stanza_id,
     retract_wrong_muc_message].

disabled_muc_retract_cases() ->
    [retract_muc_message].

muc_configurable_archiveid_cases() ->
    [
     muc_no_elements,
     muc_only_stanzaid
    ].

muc_metadata_cases() ->
    [
     muc_metadata_archive_request,
     muc_metadata_archive_request_empty,
     muc_metadata_archive_request_one_message
    ].

muc_fetch_specific_msgs_cases() ->
    [
     muc_query_messages_by_ids,
     muc_simple_query_messages_by_ids,
     muc_server_returns_item_not_found_for_ids_filter_with_nonexistent_id
    ].

configurable_archiveid_cases() ->
    [no_elements,
     only_stanzaid,
     same_stanza_id,
     retract_message_on_stanza_id
    ].

muc_light_cases() ->
    [
     muc_light_service_discovery_stored_in_pm,
     muc_light_easy,
     muc_light_shouldnt_modify_pm_archive,
     muc_light_stored_in_pm_if_allowed_to,
     muc_light_include_groupchat_filter,
     muc_light_no_pm_stored_include_groupchat_filter,
     muc_light_include_groupchat_messages_by_default,
     muc_light_chat_markers_are_archived_if_enabled,
     muc_light_chat_markers_are_not_archived_if_disabled,
     muc_light_failed_to_decode_message_in_database,
     muc_light_sql_query_failed,
     muc_light_async_pools_batch_flush
    ].

muc_rsm_cases() ->
    rsm_cases().

with_rsm_cases() ->
    rsm_cases().

rsm_cases() ->
      [pagination_first5,
       pagination_last5,
       pagination_offset5,
       pagination_first0,
       pagination_last0,
       pagination_offset5_max0,
       pagination_before10,
       pagination_after10,
       pagination_empty_rset,
       pagination_flipped_page,
       %% Border cases
       pagination_last_after_id5,
       pagination_last_after_id5_before_id11,
       pagination_first_page_after_id4,
       pagination_last_page_after_id4,
       pagination_border_flipped_page,
       %% Simple cases
       pagination_simple_before10,
       pagination_simple_before3,
       pagination_simple_before6,
       pagination_simple_before1_pagesize0,
       pagination_simple_before2_pagesize0,
       pagination_simple_after5,
       pagination_simple_after10,
       pagination_simple_after12,
       %% item_not_found response for nonexistent message ID in before/after filters
       server_returns_item_not_found_for_before_filter_with_nonexistent_id,
       server_returns_item_not_found_for_after_filter_with_nonexistent_id,
       server_returns_item_not_found_for_after_filter_with_invalid_id].

complete_flag_cases() ->
    [before_complete_false_last5,
     before_complete_false_before10,
     before_complete_true_before1,
     before_complete_true_before5,
     before_complete_true_before6,
     after_complete_false_first_page,
     after_complete_false_after2,
     after_complete_false_after9,
     after_complete_true_after10,
     after_complete_true_after11].

prefs_cases() ->
    [prefs_set_request,
     prefs_set_cdata_request,
     query_get_request,
     messages_filtered_when_prefs_default_policy_is_always,
     messages_filtered_when_prefs_default_policy_is_never,
     messages_filtered_when_prefs_default_policy_is_roster,
     run_set_and_get_prefs_cases].

muc_prefs_cases() ->
    [muc_prefs_set_request,
     muc_prefs_set_request_not_an_owner,
     muc_prefs_set_cdata_request,
     muc_query_get_request,
     muc_messages_filtered_when_prefs_default_policy_is_always,
     muc_messages_filtered_when_prefs_default_policy_is_never,
     muc_messages_filtered_when_prefs_default_policy_is_roster].

no_prefs_cases() ->
    [prefs_disabled_set_request].

impl_specific() ->
    [check_user_exist,
     pm_failed_to_decode_message_in_database,
     pm_sql_query_failed,
     async_pools_batch_flush].

stream_management_cases() ->
    [reconnect_ack,
     reconnect_no_ack,
     reconnect_no_ack_different_resource].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config) ->
    PoolIds = [pm_mam, muc_mam],
    AsyncPoolsEvents = [{async_pool_flush, #{host_type => host_type(), pool_id => PoolId}}
                        || PoolId <- PoolIds],
    instrument_helper:start(
        instrument_helper:declared_events(instrumented_modules()) ++ AsyncPoolsEvents),
    muc_helper:load_muc(),
    mongoose_helper:inject_module(mim(), ?MODULE, reload),
    mam_helper:prepare_for_suite(
      increase_limits(
        delete_users([{escalus_user_db, {module, escalus_ejabberd}}
                  | escalus:init_per_suite(Config)]))).

instrumented_modules() ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        true -> [mod_mam_rdbms_arch_async, mod_mam_muc_rdbms_arch_async];
        false -> []
    end ++ [mod_mam_pm, mod_mam_muc].

end_per_suite(Config) ->
    muc_helper:unload_muc(),
    %% Next function creates a lot of sessions...
    escalus_fresh:clean(),
    %% and this function kicks them without waiting...
    mongoose_helper:kick_everyone(),
    %% so we don't have sessions anymore and other tests will not fail
    mongoose_helper:restore_config(Config),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

user_names() ->
    [alice, bob, kate, carol].

create_users(Config) ->
    escalus:create_users(Config, escalus:get_users(user_names())).

delete_users(Config) ->
    escalus:delete_users(Config, escalus:get_users(user_names())).

increase_limits(Config) ->
    Config1 = mongoose_helper:backup_and_set_config(Config, increased_limits()),
    rpc_apply(mongoose_shaper, reset_all_shapers, [host_type()]),
    Config1.

increased_limits() ->
    #{[shaper, mam_shaper] => #{max_rate => 10000},
      [shaper, normal] => #{max_rate => 10000000},
      [shaper, fast] => #{max_rate => 10000000},
      [{access, host_type()}, max_user_sessions] => [#{acl => all, value => 10000}]}.

init_per_group(mam04, Config) ->
    [{props, mam04_props()}|Config];
init_per_group(mam06, Config) ->
    [{props, mam06_props()}|Config];

init_per_group(rsm_all, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{N, 1} || N <- user_names()]),
    send_rsm_messages(Config1);
init_per_group(rsm04, Config) ->
    [{props, mam04_props()}|Config];
init_per_group(rsm04_comp, Config) ->
    [{props, mam04_props()}|Config];
init_per_group(with_rsm04, Config) ->
    [{props, mam04_props()}, {with_rsm, true}|Config];
init_per_group(nostore, Config) ->
    Config;
init_per_group(stream_management, Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    DefaultSMConfig = config_parser_helper:default_mod_config(mod_stream_management),
    MnesiaOrCets = ct_helper:get_internal_database(),
    SMConfig = DefaultSMConfig#{backend => MnesiaOrCets},
    dynamic_modules:ensure_modules(host_type(), [{mod_stream_management, SMConfig}]),
    Config1;
init_per_group(archived, Config) ->
    Config;
init_per_group(muc04, Config) ->
    [{props, mam04_props()}, {with_rsm, true}|Config];
init_per_group(muc06, Config) ->
    [{props, mam06_props()}, {with_rsm, true}|Config];
init_per_group(muc_seq, Config) ->
    [{props, mam04_props()}, {with_rsm, true}|Config];

init_per_group(muc_configurable_archiveid, Config) ->
    dynamic_modules:save_modules(host_type(), Config);
init_per_group(configurable_archiveid, Config) ->
    dynamic_modules:save_modules(host_type(), Config);

init_per_group(G, Config) when G =:= drop_msg;
                               G =:= muc_drop_msg ->
    setup_meck(G, ?config(configuration, Config)),
    Config;

init_per_group(muc_rsm_all, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{N, 1} || N <- user_names()]),
    Config2 = start_alice_room(Config1),
    Config3 = send_muc_rsm_messages(Config2),
    [{muc_rsm, true} | Config3];
init_per_group(muc_rsm04, Config) ->
    [{props, mam04_props()}|Config];

init_per_group(Group, ConfigIn) ->
    C = configuration(Group),
    B = basic_group(Group),
    {ModulesToStart, Config0} = required_modules_for_group(C, B, ConfigIn),
    ct:log("Init per group ~p; configuration ~p; basic group ~p", [Group, C, B]),
    Config01 = dynamic_modules:save_modules(host_type(), Config0),
    dynamic_modules:ensure_modules(host_type(), ModulesToStart),
    Config1 = do_init_per_group(C, Config01),
    [{basic_group, B}, {configuration, C} | init_state(C, B, Config1)].

do_init_per_group(C, ConfigIn) ->
    Config0 = create_users(ConfigIn),
    case C of
        cassandra ->
            [{archive_wait, 1500} | Config0];
        cassandra_eterm ->
            [{archive_wait, 1500} | Config0];
        elasticsearch ->
            [{archive_wait, 2500} | Config0];
        _ ->
            Config0
    end.

setup_meck(_, elasticsearch) ->
    ok = rpc(mim(), meck, expect,
             [mongoose_elasticsearch, insert_document, 4, {error, simulated}]);
setup_meck(_, Config) when Config =:= cassandra_eterm;
                           Config =:= cassandra ->
    ok = rpc(mim(), meck, expect,
             [mongoose_cassandra, cql_write_async, 5, {error, simulated}]);
setup_meck(drop_msg, Config) when Config =:= rdbms_async_pool;
                                  Config =:= rdbms_async_cache ->
    ok = rpc(mim(), meck, new, [mongoose_rdbms, [no_link, passthrough]]),
    ok = rpc(mim(), meck, expect,
             [mongoose_rdbms, execute,
              fun (_HostType, insert_mam_message, _Parameters) ->
                      {error, simulated};
                  (HostType, Name, Parameters) ->
                      meck:passthrough([HostType, Name, Parameters])
              end]);
setup_meck(muc_drop_msg, Config) when Config =:= rdbms_async_pool;
                                      Config =:= rdbms_async_cache ->
    ok = rpc(mim(), meck, new, [mongoose_rdbms, [no_link, passthrough]]),
    ok = rpc(mim(), meck, expect,
             [mongoose_rdbms, execute,
              fun (_HostType, insert_mam_muc_message, _Parameters) ->
                      {error, simulated};
                  (HostType, Name, Parameters) ->
                      meck:passthrough([HostType, Name, Parameters])
              end]);
setup_meck(drop_msg, _) ->
    ok = rpc(mim(), meck, new, [mongoose_rdbms, [no_link, passthrough]]),
    ok = rpc(mim(), meck, expect,
             [mongoose_rdbms, execute_successfully,
              fun (_HostType, insert_mam_message, _Parameters) ->
                      error(#{what => simulated_error});
                  (HostType, Name, Parameters) ->
                      meck:passthrough([HostType, Name, Parameters])
              end]);
setup_meck(muc_drop_msg, _) ->
    ok = rpc(mim(), meck, new, [mongoose_rdbms, [no_link, passthrough]]),
    ok = rpc(mim(), meck, expect,
             [mongoose_rdbms, execute_successfully,
              fun (_HostType, insert_mam_muc_message, _Parameters) ->
                      error(#{what => simulated_error});
                  (HostType, Name, Parameters) ->
                      meck:passthrough([HostType, Name, Parameters])
              end]).

end_per_group(G, Config) when G == rsm_all; G == nostore;
    G == mam04; G == rsm04; G == with_rsm04; G == muc04; G == muc_rsm04; G == rsm04_comp;
    G == muc06; G == mam06; G == archived; G == muc_seq  ->
      Config;
end_per_group(G, Config) when G == drop_msg;
                              G == muc_drop_msg ->
    teardown_meck(),
    Config;
end_per_group(stream_management, Config) ->
    dynamic_modules:restore_modules(Config);
end_per_group(muc_configurable_archiveid, Config) ->
    dynamic_modules:restore_modules(Config),
    Config;
end_per_group(configurable_archiveid, Config) ->
    dynamic_modules:restore_modules(Config),
    Config;
end_per_group(muc_rsm_all, Config) ->
    destroy_room(Config);
end_per_group(Group, Config) ->
    C = configuration(Group),
    B = basic_group(Group),
    Config0 = end_state(C, B, Config),
    Config1 = dynamic_modules:restore_modules(Config0),
    escalus_fresh:clean(),
    delete_users(Config1).

required_modules_for_group(C, muc_light, Config) ->
    Extra = mam_opts_for_conf(C),
    MUCHost = subhost_pattern(muc_light_helper:muc_host_pattern()),
    Opts = config_opts(Extra#{pm => #{}, muc => #{host => MUCHost}}),
    Config1 = maybe_set_wait(C, [muc, pm], [{mam_meta_opts, Opts} | Config]),
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    {[{mod_muc_light, config_parser_helper:mod_config(mod_muc_light, #{backend => Backend})},
      {mod_mam, Opts}], Config1};
required_modules_for_group(C, BG, Config) when BG =:= muc_all;
                                               BG =:= muc_disabled_retraction;
                                               BG =:= muc_prefs_cases ->
    Extra = maps:merge(mam_opts_for_conf(C), mam_opts_for_base_group(BG)),
    MUCHost = subhost_pattern(muc_domain(Config)),
    Opts = config_opts(Extra#{muc => #{host => MUCHost}}),
    Config1 = maybe_set_wait(C, [muc], [{mam_meta_opts, Opts} | Config]),
    {[{mod_mam, Opts}], Config1};
required_modules_for_group(C, BG, Config) ->
    Extra = maps:merge(mam_opts_for_conf(C), mam_opts_for_base_group(BG)),
    Opts = config_opts(Extra#{pm => #{}}),
    Config1 = maybe_set_wait(C, [pm], [{mam_meta_opts, Opts} | Config]),
    {[{mod_mam, Opts}], Config1}.

maybe_set_wait(C, Types, Config) when C =:= rdbms_async_pool;
                                      C =:= rdbms_async_cache ->
    [{wait_for_parallel_writer, Types} | Config];
maybe_set_wait(_C, _, Config) ->
    Config.

mam_opts_for_conf(elasticsearch) ->
    #{backend => elasticsearch,
      user_prefs_store => mnesia};
mam_opts_for_conf(cassandra) ->
    #{backend => cassandra,
      user_prefs_store => cassandra};
mam_opts_for_conf(cassandra_eterm) ->
    Opts = mam_opts_for_conf(cassandra),
    Opts#{db_message_format => mam_message_eterm};
mam_opts_for_conf(rdbms_easy) ->
    EasyOpts = #{db_jid_format => mam_jid_rfc,
                 db_message_format => mam_message_xml},
    maps:merge(EasyOpts, mam_opts_for_conf(rdbms));
mam_opts_for_conf(rdbms_no_prefs) ->
    #{async_writer => #{enabled => false},
      cache_users => false};
mam_opts_for_conf(rdbms) ->
    #{user_prefs_store => rdbms,
      async_writer => #{enabled => false},
      cache_users => false};
mam_opts_for_conf(rdbms_async_pool) ->
    #{user_prefs_store => rdbms,
      async_writer => #{flush_interval => 1},
      cache_users => false};
mam_opts_for_conf(rdbms_mnesia) ->
    #{user_prefs_store => mnesia,
      async_writer => #{enabled => false},
      cache_users => false};
mam_opts_for_conf(rdbms_cache) ->
    #{user_prefs_store => rdbms,
      async_writer => #{enabled => false}};
mam_opts_for_conf(rdbms_async_cache) ->
    #{user_prefs_store => rdbms,
      async_writer => #{flush_interval => 1}};
mam_opts_for_conf(rdbms_mnesia_cache) ->
    #{user_prefs_store => mnesia,
      async_writer => #{enabled => false}}.

muc_domain(Config) ->
    proplists:get_value(muc_domain, Config, muc_helper:muc_host_pattern()).

mam_opts_for_base_group(disabled_text_search) ->
    #{full_text_search => false};
mam_opts_for_base_group(disabled_complex_queries) ->
    #{enforce_simple_queries => true};
mam_opts_for_base_group(BG) when BG =:= disabled_retraction;
                                 BG =:= muc_disabled_retraction ->
    #{message_retraction => false};
mam_opts_for_base_group(chat_markers) ->
    #{archive_chat_markers => true};
mam_opts_for_base_group(_BG) ->
    #{}.

init_state(_, muc_all, Config) ->
    Config;
init_state(_, muc_prefs_cases, Config) ->
    Config;
init_state(C, muc_light, Config) ->
    clean_archives(Config),
    init_state(C, muc04, Config);
init_state(_C, prefs_cases, Config) ->
    Config;
init_state(_, _, Config) ->
    clean_archives(Config).

end_state(C, muc_light, Config) ->
    muc_light_helper:clear_db(host_type()),
    end_state(C, generic, Config);
end_state(_, _, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    case maybe_skip(CaseName, Config) of
        ok ->
            maybe_setup_meck(CaseName),
            dynamic_modules:ensure_modules(host_type(), required_modules(CaseName, Config)),
            lists:foldl(fun(StepF, ConfigIn) -> StepF(CaseName, ConfigIn) end, Config, init_steps());
        {skip, Msg} ->
            {skip, Msg}
    end.

maybe_setup_meck(muc_light_sql_query_failed) ->
    ok = rpc(mim(), meck, new, [mongoose_rdbms, [no_link, passthrough]]),
    ok = rpc(mim(), meck, expect,
             [mongoose_rdbms, execute_successfully,
              fun (_HostType, mam_muc_message_lookup_a_equ_limit, _Parameters) ->
                      error(#{what => simulated_error});
                  (HostType, Name, Parameters) ->
                      meck:passthrough([HostType, Name, Parameters])
              end]);
maybe_setup_meck(pm_sql_query_failed) ->
    ok = rpc(mim(), meck, new, [mongoose_rdbms, [no_link, passthrough]]),
    ok = rpc(mim(), meck, expect,
             [mongoose_rdbms, execute_successfully,
              fun (_HostType, mam_message_lookup_a_equ_limit, _Parameters) ->
                      error(#{what => simulated_error});
                  (HostType, Name, Parameters) ->
                      meck:passthrough([HostType, Name, Parameters])
              end]);
maybe_setup_meck(_) ->
    ok.

init_steps() ->
    [fun init_users/2, fun init_archive/2, fun start_room/2,
     fun escalus:init_per_testcase/2].

maybe_skip(C, Config) when C =:= retract_message;
                           C =:= retract_wrong_message;
                           C =:= ignore_bad_retraction;
                           C =:= retract_message_on_stanza_id;
                           C =:= retract_muc_message;
                           C =:= retract_muc_message_on_stanza_id;
                           C =:= retract_wrong_muc_message ->
    ConfList = rdbms_configs(true, ct_helper:get_internal_database()),
    skip_if(not lists:member(?config(configuration, Config), ConfList),
            "message retraction not supported");
maybe_skip(C, Config) when C =:= muc_light_failed_to_decode_message_in_database;
                           C =:= pm_failed_to_decode_message_in_database ->
    skip_if(?config(configuration, Config) =:= elasticsearch,
            "elasticsearch does not support encodings");
maybe_skip(C, Config) when C =:= muc_light_sql_query_failed;
                           C =:= pm_sql_query_failed ->
    Configuration = ?config(configuration, Config),
    skip_if(lists:member(Configuration, [elasticsearch, cassandra, cassandra_eterm]),
            "Not an SQL database");
maybe_skip(C, Config) when C =:= muc_light_include_groupchat_filter;
                           C =:= muc_light_no_pm_stored_include_groupchat_filter;
                           C =:= muc_light_include_groupchat_messages_by_default ->
    Configuration = ?config(configuration, Config),
    skip_if(lists:member(Configuration, [cassandra, cassandra_eterm]),
            "include_groupchat field is not supported for cassandra backend");
maybe_skip(C, Config) when C =:= easy_text_search_request;
                           C =:= long_text_search_request;
                           C =:= save_unicode_messages;
                           C =:= muc_text_search_request ->
    Configuration = ?config(configuration, Config),
    skip_if(lists:member(Configuration, [cassandra, cassandra_eterm]),
            "full text search is not implemented for cassandra backend");
maybe_skip(C, Config) when C =:= muc_light_async_pools_batch_flush;
                           C =:= async_pools_batch_flush ->
    skip_if(?config(configuration, Config) =/= rdbms_async_pool,
            "only for async pool");
maybe_skip(C, Config) when C =:= easy_archive_request_old_xmlel_format ->
    MamOpts = ?config(mam_meta_opts, Config),
    MamBackend = maps:get(backend, MamOpts, rdbms),
    DefaultFormat = case MamBackend of
                        rdbms -> mam_message_compressed_eterm;
                        _ -> mam_message_xml
                    end,
    MessageFormat = maps:get(db_message_format, MamOpts, DefaultFormat),
    PmMamOpts = maps:get(pm, MamOpts, #{}),
    PmMessageFormat = maps:get(db_message_format, PmMamOpts, MessageFormat),
    skip_if(PmMessageFormat =:= mam_message_xml, "run only for eterm PM message format");
maybe_skip(_C, _Config) ->
    ok.

skip_if(false, _Msg) -> ok;
skip_if(true, Msg) -> {skip, Msg}.

init_users(CaseName, Config) ->
    case fresh_users(CaseName) of
        [] ->
            Config;
        UserSpecs ->
            escalus_fresh:create_users(Config, UserSpecs)
    end.

-define(requires_pm_archive(C),
        C =:= querying_for_all_messages_with_jid;
        C =:= query_messages_by_ids;
        C =:= simple_query_messages_by_ids;
        C =:= server_returns_item_not_found_for_ids_filter_with_nonexistent_id;
        C =:= pagination_simple_enforced;
        C =:= range_archive_request_not_empty;
        C =:= limit_archive_request;
        C =:= metadata_archive_request).

-define(requires_muc_archive(C),
        C =:= muc_query_messages_by_ids;
        C =:= muc_simple_query_messages_by_ids;
        C =:= muc_server_returns_item_not_found_for_ids_filter_with_nonexistent_id;
        C =:= muc_querying_for_all_messages;
        C =:= muc_querying_for_all_messages_with_jid;
        C =:= muc_metadata_archive_request).

fresh_users(C) when ?requires_pm_archive(C) ->
    [{alice, 1}, {bob, 1}, {carol, 1}];
fresh_users(C) when C =:= offline_message;
                    C =:= archived;
                    C =:= no_elements;
                    C =:= only_stanzaid;
                    C =:= same_stanza_id;
                    C =:= metadata_archive_request_empty;
                    C =:= metadata_archive_request_one_message;
                    C =:= archive_chat_markers;
                    C =:= dont_archive_chat_markers ->
    [{alice, 1}, {bob, 1}];
fresh_users(C) ->
    case lists:member(C, all_cases_with_room()) of
        true -> [{alice, 1}, {bob, 1}];
        false -> []
    end.

init_archive(C, Config) when ?requires_pm_archive(C) ->
    bootstrap_archive(Config);
init_archive(C, Config) when ?requires_muc_archive(C) ->
    muc_bootstrap_archive(start_alice_room(Config));
init_archive(_CaseName, Config) ->
    Config.

start_room(C, Config) when C =:= muc_deny_protected_room_access;
                           C =:= muc_allow_access_to_owner ->
    start_alice_protected_room(Config);
start_room(C, Config) when C =:= muc_delete_x_user_in_anon_rooms;
                           C =:= muc_show_x_user_to_moderators_in_anon_rooms;
                           C =:= muc_show_x_user_for_your_own_messages_in_anon_rooms ->
    start_alice_anonymous_room(Config);
start_room(C, Config) ->
    case lists:member(C, all_cases_with_room()) of
        true -> start_alice_room(Config);
        false -> Config
    end.

end_per_testcase(CaseName, Config) when CaseName =:= pm_sql_query_failed;
                                        CaseName =:= muc_light_sql_query_failed ->
    teardown_meck(),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName = muc_validate_mam_id, Config) ->
    unmock_mongoose_mam_id(mim()),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    maybe_destroy_room(CaseName, Config),
    escalus:end_per_testcase(CaseName, Config).

maybe_destroy_room(CaseName, Config) ->
    case lists:member(CaseName, all_cases_with_room()) of
        true -> destroy_room(Config);
        false -> ok
    end.

all_cases_with_room() ->
    muc_cases_with_room() ++ muc_fetch_specific_msgs_cases() ++ muc_configurable_archiveid_cases() ++
        muc_stanzaid_cases() ++ muc_retract_cases() ++ muc_metadata_cases() ++ muc_text_search_cases() ++
        [muc_message_dropped].

teardown_meck() ->
    rpc(mim(), meck, unload, []).

%% Module configuration per testcase

required_modules(CaseName, Config) when CaseName =:= muc_light_service_discovery_stored_in_pm;
                                        CaseName =:= muc_light_stored_in_pm_if_allowed_to;
                                        CaseName =:= muc_light_include_groupchat_messages_by_default;
                                        CaseName =:= muc_light_no_pm_stored_include_groupchat_filter;
                                        CaseName =:= muc_light_include_groupchat_filter ->
    Opts = #{pm := PM} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{pm := PM#{archive_groupchats => true}},
    [{mod_mam, NewOpts}];
required_modules(CaseName, Config) when CaseName =:= async_pools_batch_flush;
                                        CaseName =:= muc_light_async_pools_batch_flush ->
    Opts = #{async_writer := Async} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{async_writer := Async#{batch_size => 3, flush_interval => 5000}},
    [{mod_mam, NewOpts}];
required_modules(muc_light_chat_markers_are_archived_if_enabled, Config) ->
    Opts = #{muc := MUC} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{muc := MUC#{archive_chat_markers => true}},
    [{mod_mam, NewOpts}];
required_modules(muc_no_elements, Config) ->
    Opts = #{muc := MUC} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{muc := MUC#{no_stanzaid_element => true}},
    [{mod_mam, NewOpts}];
required_modules(muc_light_failed_to_decode_message_in_database, Config) ->
    Opts = #{muc := MUC} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{muc := MUC#{db_message_format => mam_message_eterm}},
    [{mod_mam, NewOpts}];
required_modules(pm_failed_to_decode_message_in_database, Config) ->
    Opts = #{pm := PM} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{pm := PM#{db_message_format => mam_message_eterm}},
    [{mod_mam, NewOpts}];
required_modules(muc_only_stanzaid, Config) ->
    Opts = ?config(mam_meta_opts, Config),
    [{mod_mam, Opts}];
% configurable_archiveid basic group
required_modules(no_elements, Config) ->
    Opts = #{pm := PM} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{pm := PM#{no_stanzaid_element => true}},
    [{mod_mam, NewOpts}];
required_modules(CaseName, Config) when CaseName =:= same_stanza_id;
                                        CaseName =:= retract_message_on_stanza_id ->
    Opts = #{pm := PM} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{pm := PM#{same_mam_id_for_peers => true}},
    [{mod_mam, NewOpts}];
required_modules(_, Config) ->
    Opts = ?config(mam_meta_opts, Config),
    [{mod_mam, Opts}].

pm_with_db_message_format_xml(Config) ->
    Opts = #{pm := PM} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{pm := PM#{db_message_format => mam_message_xml}},
    [{mod_mam, NewOpts}].

muc_with_db_message_format_xml(Config) ->
    Opts = #{muc := MUC} = ?config(mam_meta_opts, Config),
    NewOpts = Opts#{muc := MUC#{db_message_format => mam_message_xml}},
    [{mod_mam, NewOpts}].

%%--------------------------------------------------------------------
%% Group name helpers
%%--------------------------------------------------------------------

full_group(Conf, Group) ->
    list_to_atom(atom_to_list(Conf) ++ "_" ++ atom_to_list(Group)).

%% @doc Delete suffix.
configuration(Group) ->
    match_atom_prefix(Group, make_greedy(configurations())).

%% @doc Rearrange a list of strings (or atoms), that all prefixes
%% will be tested.
%%
%% Example:
%% `make_greedy(rdbms_mnesia_muc, [rdbms, rdbms_mnesia]) -> rdbms'
%% `make_greedy(rdbms_mnesia_muc, match_longer_first([rdbms, rdbms_mnesia])) -> rdbms_mnesia'
%% @end
make_greedy(List) ->
    lists:reverse(lists:usort(List)).

%% @doc Delete prefix.
basic_group(Group) ->
    basic_group(Group, configuration(Group)).

basic_group(Group, Conf) ->
    ConfS = atom_to_list(Conf),
    GroupS = atom_to_list(Group),
    list_to_atom(delete_delimiter(delete_prefix(ConfS, GroupS))).

match_atom_prefix(Target, Prefixes) ->
    match_atom_prefix1(atom_to_list(Target), Prefixes).

match_atom_prefix1(TargetS, [PrefixA | Prefixes]) ->
    PrefixS = atom_to_list(PrefixA),
    case lists:prefix(PrefixS, TargetS) of
        true -> PrefixA;
        false -> match_atom_prefix1(TargetS, Prefixes)
    end.

delete_prefix([H|Prefix], [H|Target]) ->
    delete_prefix(Prefix, Target);
delete_prefix([], Target) ->
    Target.

delete_delimiter("_" ++ Tail) ->
    Tail.

%%--------------------------------------------------------------------
%% Adhoc tests
%%--------------------------------------------------------------------

% @doc Helper function, sends an example message to a user and checks
% if archive id elements are defined or not
send_and_check_archive_elements(Config, Archived, Stanzaid) ->
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        BobMsg = escalus:wait_for_stanza(Bob),
        case exml_query:subelement(BobMsg, <<"archived">>) of
                undefined ->
                    ?assert_equal(Archived, false);
                _ ->
                    ?assert_equal(Archived, true)
        end,
        case exml_query:subelement(BobMsg, <<"stanza-id">>) of
                   undefined ->
                       ?assert_equal(Stanzaid, false);
                   _ ->
                       ?assert_equal(Stanzaid, true)
        end,
        ok
        end,
    %% Made fresh in init_per_testcase
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

% @doc Helper function, sends an example message to a room and checks
% if archive id elements are defined or not
muc_send_and_check_archive_elements(Config, Archived, Stanzaid) ->
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"Hi, Bob!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice sends another message to Bob.
        %% The message is not archived by the room.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:assert(is_message, escalus:wait_for_stanza(Bob)),

        %% Alice sends to the chat room.
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by alicesroom@localhost).
        %% User's archive is disabled (i.e. bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        case exml_query:subelement(BobMsg, <<"archived">>) of
                undefined ->
                    ?assert_equal(Archived, false);
                _ ->
                    ?assert_equal(Archived, true)
        end,
        case exml_query:subelement(BobMsg, <<"stanza-id">>) of
                   undefined ->
                       ?assert_equal(Stanzaid, false);
                   _ ->
                       ?assert_equal(Stanzaid, true)
               end,
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

%% Archive id elements should be present when config says so
muc_no_elements(Config) ->
    muc_send_and_check_archive_elements(Config, false, false).

muc_only_stanzaid(Config) ->
    muc_send_and_check_archive_elements(Config, false, true).

no_elements(Config) ->
    send_and_check_archive_elements(Config, false, false).

only_stanzaid(Config) ->
    send_and_check_archive_elements(Config, false, true).

same_stanza_id(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Body = <<"OH, HAI!">>,
        Msg = escalus_stanza:chat_to(Bob, Body),
        escalus:send(Alice, Msg),
        mam_helper:wait_for_archive_size(Alice, 1),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Result = wait_archive_respond(Alice),
        [AliceCopyOfMessage] = respond_messages(Result),
        AliceId = exml_query:path(AliceCopyOfMessage, [{element, <<"result">>}, {attr, <<"id">>}]),
        %% ... and Bob receives the message
        RecvMsg = escalus:wait_for_stanza(Bob),
        BobId = exml_query:path(RecvMsg, [{element, <<"stanza-id">>}, {attr, <<"id">>}]),
        ?assert_equal(AliceId, BobId)
    end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

archive_is_instrumented(Config) ->
    F = fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:wait_for_stanza(Bob),
        assert_archive_message_event(mod_mam_pm_archive_message, escalus_utils:get_jid(Alice)),
        mam_helper:wait_for_archive_size(Alice, 1),
        assert_flushed_event_if_async(mod_mam_pm_flushed, Config),
        {S, U} = {escalus_utils:get_server(Alice), escalus_utils:get_username(Alice)},
        mam_helper:delete_archive(S, U),
        assert_event_with_jid(mod_mam_pm_remove_archive, escalus_utils:get_short_jid(Alice))
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

message_dropped(Config) ->
    TS = instrument_helper:timestamp(),
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        maybe_wait_for_archive(Config),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Res = wait_archive_respond(Alice),
        assert_respond_size(0, Res),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F),
    assert_dropped_msg_event(mod_mam_pm_dropped, TS).

%% Querying the archive for messages
easy_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        mam_helper:wait_for_archive_size(Alice, 1),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Res = wait_archive_respond(Alice),
        assert_lookup_event(mod_mam_pm_lookup, escalus_utils:get_jid(Alice)),
        assert_respond_size(1, Res),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res)),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

easy_archive_request_for_the_receiver(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        mam_helper:wait_for_archive_size(Bob, 1),
        escalus:send(Bob, stanza_archive_request(P, <<"q1">>)),
        Res = wait_archive_respond(Bob),
        assert_respond_size(1, Res),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res)),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

easy_archive_request_old_xmlel_format(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        AArcId = rest_helper:make_arc_id(Alice),
        {BobJid, _, _} = BArcId = rest_helper:make_arc_id(Bob),
        DateTime = calendar:local_time(),
        Msg = mam_helper:generate_msg_for_date_user(AArcId, BArcId, DateTime, <<"OH, HAI!">>),
        Packet = erlang:element(5, Msg),
        OldFormatPacket =
            {xmlel,<<"message">>,
             [{<<"to">>, BobJid}, {<<"type">>,<<"chat">>}],
             [{xmlel,<<"body">>,[],[{xmlcdata,<<"OH, HAI!">>}]}]},

        Msg1 = erlang:setelement(5, Msg, OldFormatPacket),
        ct:log("Packet: ~p~n", [Packet]),
        ct:log("OldFormatPacket: ~p~n", [OldFormatPacket]),
        mam_helper:put_msg(Msg1),
        mam_helper:wait_for_archive_size(Alice, 1),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Res = wait_archive_respond(Alice),
        assert_lookup_event(mod_mam_pm_lookup, escalus_utils:get_jid(Alice)),
        assert_respond_size(1, Res),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res)),
        [RespMessage] = respond_messages(Res),
        ct:log("ResPacket: ~p~n", [RespMessage]),

        ArchivedMsg = exml_query:path(RespMessage, [{element, <<"result">>},
                                                    {element, <<"forwarded">>},
                                                    {element, <<"message">>}]),
        ct:log("ArchivedMsg: ~p~n", [ArchivedMsg]),
        assert_msg_match(Packet, ArchivedMsg),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

assert_msg_match(Pattern, Msg) ->
    #xmlel{attrs = PatternAttrs, children = PatternChildren} = Pattern,
    #xmlel{attrs = MsgAttrs, children = MsgChildren} = Msg,
    [?assertEqual(Value, maps:get(Name, MsgAttrs, undefined),
                  <<"attribute ", Name/binary>>)
     || Name := Value <- PatternAttrs],
    ?assertEqual(PatternChildren, MsgChildren).

message_sent_to_yourself(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        escalus:send(Alice, escalus_stanza:chat_to(Alice, <<"OH, HAI!">>)),
        escalus:wait_for_stanza(Alice), %% Receive that message
        mam_helper:wait_for_archive_size(Alice, 1),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Res = wait_archive_respond(Alice),
        assert_respond_size(1, Res),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res)),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

text_search_is_not_available(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Namespace = get_prop(mam_ns, P),
        escalus:send(Alice, stanza_retrieve_form_fields(<<"q">>, Namespace)),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_with_ns, [Namespace], Res),
        QueryEl = exml_query:subelement(Res, <<"query">>),
        XEl = exml_query:subelement(QueryEl, <<"x">>),
        Fields = exml_query:paths(XEl, [{element, <<"field">>}]),
        HasFullTextSearch = lists:any(fun(Item) ->
            exml_query:attr(Item, <<"var">>) == <<"full-text-search">>
        end, Fields),

        ?assert_equal(false, HasFullTextSearch)
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

text_search_query_fails_if_disabled(Config) ->
    P = ?config(props, Config),
    F = fun(_Alice, Bob) ->
        escalus:send(Bob, stanza_text_search_archive_request(P, <<"q1">>,
                <<"this IQ is expected to fail">>)),
        Res = escalus:wait_for_stanza(Bob),
        escalus:assert(is_iq_error, Res)
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

pagination_simple_enforced(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Msgs = ?config(pre_generated_msgs, Config),
        [_, _, StartMsg, StopMsg | _] = Msgs,
        {{StartMsgId, _}, _, _, _, _StartMsgPacket} = StartMsg,
        {{StopMsgId, _}, _, _, _, _StopMsgPacket} = StopMsg,
        {StartMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [StartMsgId]),
        {StopMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [StopMsgId]),
        StartTime = make_iso_time(StartMicro),
        StopTime = make_iso_time(StopMicro),
        %% Send
        %% <iq type='get'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%     <start>StartTime</start>
        %%     <end>StopTime</end>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_date_range_archive_request_not_empty(P, StartTime, StopTime)),
        %% Receive two messages and IQ
        Result = wait_archive_respond(Alice),
        IQ = respond_iq(Result),
        [M1, M2|_] = respond_messages(Result),
        escalus:assert(is_iq_result, IQ),
        SetEl = exml_query:path(IQ, [{element, <<"fin">>}, {element, <<"set">>}]),
        ?assert_equal(true, undefined =/= SetEl),
        ?assert_equal(undefined, exml_query:path(SetEl, [{element, <<"count">>}])),
        ?assert_equal(undefined, exml_query:path(SetEl, [{element, <<"first">>}, {attr, <<"index">>}])),
        #forwarded_message{delay_stamp = Stamp1} = parse_forwarded_message(M1),
        #forwarded_message{delay_stamp = Stamp2} = parse_forwarded_message(M2),
        ?assert_equal(list_to_binary(StartTime), Stamp1),
        ?assert_equal(list_to_binary(StopTime), Stamp2)
        end,
    %% Made fresh in init_per_testcase
    escalus:story(Config, [{alice, 1}], F).

text_search_is_available(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Namespace = get_prop(mam_ns, P),
        escalus:send(Alice, stanza_retrieve_form_fields(<<"q">>, Namespace)),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_with_ns, [Namespace], Res),
        QueryEl = exml_query:subelement(Res, <<"query">>),
        XEl = exml_query:subelement(QueryEl, <<"x">>),
        escalus:assert(has_field_with_type, [<<"{https://erlang-solutions.com/}full-text-search">>,
                                             <<"text-single">>], XEl),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

easy_text_search_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi there! My cat's name is John">>)),
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Also my bike broke down so I'm unable ",
                                                          "to return him home">>)),
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Cats are awesome by the way">>)),
        mam_helper:wait_for_archive_size(Alice, 3),
        maybe_wait_for_archive(Config), %% yz lag

        %% 'Cat' query
        escalus:send(Alice, stanza_text_search_archive_request(P, <<"q1">>, <<"cat">>)),
        Res1 = wait_archive_respond(Alice),
        assert_respond_size(2, Res1),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res1)),
        [Msg1, Msg2] = respond_messages(Res1),
        #forwarded_message{message_body = Body1} = parse_forwarded_message(Msg1),
        #forwarded_message{message_body = Body2} = parse_forwarded_message(Msg2),
        ?assert_equal(<<"Hi there! My cat's name is John">>, Body1),
        ?assert_equal(<<"Cats are awesome by the way">>, Body2),

        %% 'Bike' query
        escalus:send(Alice, stanza_text_search_archive_request(P, <<"q2">>, <<"bike">>)),
        Res2 = wait_archive_respond(Alice),
        assert_respond_size(1, Res2),
        assert_respond_query_id(P, <<"q2">>, parse_result_iq(Res2)),
        [Msg3] = respond_messages(Res2),
        #forwarded_message{message_body = Body3} = parse_forwarded_message(Msg3),
        ?assert_equal(<<"Also my bike broke down so I'm unable to return him home">>, Body3),

        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

long_text_search_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Msgs = text_search_messages(),

        [ escalus:send(Alice, escalus_stanza:chat_to(Bob, Msg)) || Msg <- Msgs ],

        %% Just check that Bob receives the messages.
        %% It should help, when the CI server is overloaded.
        %% The test should work without this block.
        %% But sometimes on the CI server we ending up with not all messages
        %% yet archived, which leads to the test failure.
        ExpectedLen = length(Msgs),
        BobMessages = escalus:wait_for_stanzas(Bob, ExpectedLen, 15000),
        ?assert_equal_extra(ExpectedLen, length(BobMessages),
                            #{bob_messages => BobMessages}),

        mam_helper:wait_for_archive_size(Bob, ExpectedLen),
        mam_helper:wait_for_archive_size(Alice, ExpectedLen),
        maybe_wait_for_archive(Config), %% yz lag
        escalus:send(Alice, stanza_text_search_archive_request(P, <<"q1">>,
                                                               <<"Ribs poRk cUlpa">>)),
        Res = wait_archive_respond(Alice),
        assert_respond_size(3, Res),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res)),

        [Msg1, Msg2, Msg3] = respond_messages(Res),
        #forwarded_message{message_body = Body1} = parse_forwarded_message(Msg1),
        #forwarded_message{message_body = Body2} = parse_forwarded_message(Msg2),
        #forwarded_message{message_body = Body3} = parse_forwarded_message(Msg3),

        ?assert_equal(lists:nth(2, Msgs), Body1),
        ?assert_equal(lists:nth(8, Msgs), Body2),
        ?assert_equal(lists:nth(11, Msgs), Body3),

        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

%% Write and read Unicode messages back
unicode_messages_can_be_extracted(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Texts = [<<"Hi! this is an unicode character lol 😂"/utf8>>,
                 <<"this is another one no 🙅"/utf8>>,
                 <<"This is the same again lol 😂"/utf8>>],

        [escalus:send(Alice, escalus_stanza:chat_to(Bob, Text))
         || Text <- Texts],
        mam_helper:wait_for_archive_size(Alice, length(Texts)),

        %% WHEN Getting all messages
        escalus:send(Alice, stanza_archive_request(P, <<"uni-q">>)),
        Res = wait_archive_respond(Alice),
        assert_respond_size(3, Res),

        assert_respond_query_id(P, <<"uni-q">>, parse_result_iq(Res)),
        [Msg1, Msg2, Msg3] = respond_messages(Res),
        #forwarded_message{message_body = Body1} = parse_forwarded_message(Msg1),
        #forwarded_message{message_body = Body2} = parse_forwarded_message(Msg2),
        #forwarded_message{message_body = Body3} = parse_forwarded_message(Msg3),
        ?assert_equal(<<"Hi! this is an unicode character lol 😂"/utf8>>, Body1),
        ?assert_equal(<<"this is another one no 🙅"/utf8>>, Body2),
        ?assert_equal(<<"This is the same again lol 😂"/utf8>>, Body3),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

%% Depends on search feature
%% Consult with unicode_messages_can_be_extracted,
%% which ensures that unicode messages can be processed
save_unicode_messages(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
                escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi! this is an unicode character lol 😂"/utf8>>)),
                escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"this is another one no 🙅"/utf8>>)),
                escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"This is the same again lol 😂"/utf8>>)),
                mam_helper:wait_for_archive_size(Alice, 3),

                %% Each stanza_text_search_archive_request should call it regardless of wait_for_archive_size result.
                maybe_wait_for_archive(Config),

                %% WHEN Searching for a message with "lol" string
                escalus:send(Alice, stanza_text_search_archive_request(P, <<"q1">>, <<"lol"/utf8>>)),
                Res1 = wait_archive_respond(Alice),
                assert_respond_size(2, Res1),
                assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res1)),
                [Msg1, Msg2] = respond_messages(Res1),
                #forwarded_message{message_body = Body1} = parse_forwarded_message(Msg1),
                #forwarded_message{message_body = Body2} = parse_forwarded_message(Msg2),
                ?assert_equal(<<"Hi! this is an unicode character lol 😂"/utf8>>, Body1),
                ?assert_equal(<<"This is the same again lol 😂"/utf8>>, Body2),

                escalus:send(Alice, stanza_text_search_archive_request(P, <<"q2">>, <<"another"/utf8>>)),
                Res2 = wait_archive_respond(Alice),
                assert_respond_size(1, Res2),
                assert_respond_query_id(P, <<"q2">>, parse_result_iq(Res2)),
                [Msg3] = respond_messages(Res2),
                #forwarded_message{message_body = Body3} = parse_forwarded_message(Msg3),
                ?assert_equal(<<"this is another one no 🙅"/utf8>>, Body3),

                ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

stanza_id_is_appended_to_carbons(Config) ->
    F = fun(Alice1, Alice2, Bob1, Bob2) ->
        Msg = <<"OH, HAI!">>,
        mongoose_helper:enable_carbons([Alice1, Alice2, Bob1, Bob2]),
        escalus:send(Alice1, escalus_stanza:chat_to(Bob1, Msg)),
        mam_helper:wait_for_archive_size(Alice1, 1),
        escalus_client:wait_for_stanza(Bob1),
        Alice2CC = escalus_client:wait_for_stanza(Alice2),
        Bob2CC = escalus_client:wait_for_stanza(Bob2),

        SID = fun(Packet, Direction) ->
                  exml_query:path(Packet, [{element_with_ns, Direction, <<"urn:xmpp:carbons:2">>},
                                           {element_with_ns, <<"forwarded">>, <<"urn:xmpp:forward:0">>},
                                           {element_with_ns, <<"message">>, <<"jabber:client">>},
                                           {element_with_ns, <<"stanza-id">>, <<"urn:xmpp:sid:0">>},
                                           {attr, <<"id">>}])
              end,
        ?assert_equal(true, undefined =/= SID(Bob2CC, <<"received">>)),
        ?assert_equal(true, undefined =/= SID(Alice2CC, <<"sent">>)),
        escalus:assert(is_forwarded_sent_message,
          [escalus_client:full_jid(Alice1), escalus_client:full_jid(Bob1), Msg], Alice2CC),
        escalus:assert(is_forwarded_received_message,
          [escalus_client:full_jid(Alice1), escalus_client:full_jid(Bob1), Msg], Bob2CC)
        end,
    escalus_fresh:story(Config, [{alice, 2}, {bob, 2}], F).

muc_text_search_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        Msgs = text_search_messages(),

        lists:foreach(
            fun(Msg) ->
                Stanza = escalus_stanza:groupchat_to(room_address(Room), Msg),
                escalus:send(Alice, Stanza),
                escalus:assert(is_message, escalus:wait_for_stanza(Bob))
            end, Msgs),

        maybe_wait_for_archive(Config),
        SearchStanza = stanza_text_search_archive_request(P, <<"q1">>, <<"Ribs poRk cUlpa">>),
        escalus:send(Bob,  stanza_to_room(SearchStanza, Room)),
        Res = wait_archive_respond(Bob),
        assert_respond_size(3, Res),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res)),

        [Msg1, Msg2, Msg3] = respond_messages(Res),
        #forwarded_message{message_body = Body1} = parse_forwarded_message(Msg1),
        ?assert_equal(lists:nth(2, Msgs), Body1),
        #forwarded_message{message_body = Body2} = parse_forwarded_message(Msg2),
        ?assert_equal(lists:nth(8, Msgs), Body2),
        #forwarded_message{message_body = Body3} = parse_forwarded_message(Msg3),
        ?assert_equal(lists:nth(11, Msgs), Body3),

        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).


querying_for_all_messages_with_jid(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Pregenerated = ?config(pre_generated_msgs, Config),
        BWithJID = nick_to_jid(bob, Config),

        WithBob = [1 || {_, _, {JID, _, _}, _, _} <- Pregenerated,
                        escalus_utils:jid_to_lower(JID) == BWithJID],

        CountWithBob = lists:sum(WithBob),
        escalus:send(Alice, stanza_filtered_by_jid_request(P, BWithJID)),
        assert_respond_size(CountWithBob, wait_archive_respond(Alice)),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

query_messages_by_ids(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Msgs = ?config(pre_generated_msgs, Config),
        IDs = get_pre_generated_msgs_ids(Msgs, [5, 10]),

        Stanza = stanza_fetch_by_id_request(P, <<"fetch-msgs-by-ids">>, IDs),
        escalus:send(Alice, Stanza),

        Result = wait_archive_respond(Alice),
        ResultIDs = get_received_msgs_ids(Result),

        assert_respond_size(2, Result),
        ?assert_equal(lists:sort(ResultIDs), lists:sort(IDs)),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

simple_query_messages_by_ids(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Msgs = ?config(pre_generated_msgs, Config),
        [ID1, ID2, ID5] = get_pre_generated_msgs_ids(Msgs, [1, 2, 5]),

        RSM = #rsm_in{max = 10, direction = 'after', id = ID1, simple = true},
        Stanza = stanza_fetch_by_id_request(P, <<"simple-fetch-msgs-by-ids">>, [ID2, ID5], RSM),
        escalus:send(Alice, Stanza),

        Result = wait_archive_respond(Alice),
        ParsedIQ = parse_result_iq(Result),
        ResultIDs = get_received_msgs_ids(Result),

        ?assert_equal(lists:sort(ResultIDs), lists:sort([ID2, ID5])),
        ?assert_equal(undefined, ParsedIQ#result_iq.count),
        ?assert_equal(undefined, ParsedIQ#result_iq.first_index),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

server_returns_item_not_found_for_ids_filter_with_nonexistent_id(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Msgs = ?config(pre_generated_msgs, Config),
        IDs = get_pre_generated_msgs_ids(Msgs, [3, 12]),
        NonexistentID = <<"AV25E9SCO50K">>,

        Stanza = stanza_fetch_by_id_request(P, <<"ids-not-found">>, IDs ++ [NonexistentID]),
        escalus:send(Alice, Stanza),
        Result = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_error, [Stanza], Result),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Result),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_query_messages_by_ids(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Room = ?config(room, Config),
        Msgs = ?config(pre_generated_muc_msgs, Config),
        IDs = get_pre_generated_msgs_ids(Msgs, [5, 10]),

        Stanza = stanza_fetch_by_id_request(P, <<"fetch-muc-msgs-by-ids">>, IDs),
        escalus:send(Alice, stanza_to_room(Stanza, Room)),

        Result = wait_archive_respond(Alice),
        ResultIDs = get_received_msgs_ids(Result),

        assert_respond_size(2, Result),
        ?assert_equal(lists:sort(ResultIDs), lists:sort(IDs)),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_simple_query_messages_by_ids(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Room = ?config(room, Config),
        Msgs = ?config(pre_generated_muc_msgs, Config),
        [ID1, ID2, ID5] = get_pre_generated_msgs_ids(Msgs, [1, 2, 5]),

        RSM = #rsm_in{max = 10, direction = 'after', id = ID1, simple = true},
        Stanza = stanza_fetch_by_id_request(P, <<"muc-simple-fetch-msgs-by-ids">>, [ID2, ID5], RSM),
        escalus:send(Alice, stanza_to_room(Stanza, Room)),

        Result = wait_archive_respond(Alice),
        ParsedIQ = parse_result_iq(Result),
        ResultIDs = get_received_msgs_ids(Result),

        ?assert_equal(lists:sort(ResultIDs), lists:sort([ID2, ID5])),
        ?assert_equal(undefined, ParsedIQ#result_iq.count),
        ?assert_equal(undefined, ParsedIQ#result_iq.first_index),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_server_returns_item_not_found_for_ids_filter_with_nonexistent_id(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Room = ?config(room, Config),
        Msgs = ?config(pre_generated_muc_msgs, Config),
        IDs = get_pre_generated_msgs_ids(Msgs, [3, 12]),
        NonexistentID = <<"AV25E9SCO50K">>,

        Stanza = stanza_fetch_by_id_request(P, <<"muc-ids-not-found">>, IDs ++ [NonexistentID]),
        escalus:send(Alice, stanza_to_room(Stanza, Room)),
        Result = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_error, [Stanza], Result),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Result),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

%% Based on https://github.com/esl/MongooseIM/issues/4222
querying_for_all_messages_with_jid_after(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob, Kate) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bob!">>)),
        mam_helper:wait_for_archive_size(Alice, 1),
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Kate!">>)),
        mam_helper:wait_for_archive_size(Alice, 2),
        escalus:send(Kate, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
        escalus:assert(is_chat_message, [<<"Hi, Alice!">>], escalus:wait_for_stanza(Alice)),
        mam_helper:wait_for_archive_size(Alice, 3),
        escalus:send(Kate, escalus_stanza:chat_to(Alice, <<"How are you?">>)),
        escalus:assert(is_chat_message, [<<"How are you?">>], escalus:wait_for_stanza(Alice)),
        mam_helper:wait_for_archive_size(Alice, 4),
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"I am busy now">>)),
        escalus:assert(is_chat_message, [<<"I am busy now">>], escalus:wait_for_stanza(Alice)),
        mam_helper:wait_for_archive_size(Alice, 5),
        escalus:send(Alice, mam_helper:stanza_lookup_messages_iq(P, #{})),
        AllRes = wait_archive_respond(Alice),
        assert_respond_size(5, AllRes),
        %% Third message overall, second message in the conversation with Kate
        Msg3 = lists:nth(3, respond_messages(AllRes)),
        #forwarded_message{result_id = MamId3, message_body = <<"Hi, Alice!">>} =
            parse_forwarded_message(Msg3),
        KateJid = escalus_client:short_jid(Kate),
        Params = #{
            with_jid => KateJid,
            rsm => #rsm_in{max = 50, direction = 'after', id = MamId3}
        },
        escalus:send(Alice, mam_helper:stanza_lookup_messages_iq(P, Params)),
        WithRes = wait_archive_respond(Alice),
        assert_respond_size(1, WithRes),
        [WithMsg] = respond_messages(WithRes),
        #forwarded_message{message_body = <<"How are you?">>} =
            parse_forwarded_message(WithMsg)
    end,
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], F).

querying_with_invalid_mam_id_in_after(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Params = #{rsm => #rsm_in{max = 50, direction = 'after', id = <<"PURPLEFE965CC9">>}},
        escalus:send(Alice, mam_helper:stanza_lookup_messages_iq(P, Params)),
        Result = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [], Result),
        escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Result)
    end,
    escalus:fresh_story(Config, [{alice, 1}], F).

muc_querying_for_all_messages(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        maybe_wait_for_archive(Config),

        Room = ?config(room, Config),
        MucMsgs = ?config(pre_generated_muc_msgs, Config),

        MucArchiveLen = length(MucMsgs),

        IQ = stanza_archive_request(P, <<>>),
        escalus:send(Alice, stanza_to_room(IQ, Room)),
        maybe_wait_for_archive(Config),
        assert_respond_size(MucArchiveLen, wait_archive_respond(Alice)),

        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_querying_for_all_messages_with_jid(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
            Room = ?config(room, Config),
            BobNick = ?config(bob_nickname, Config),
            BWithJID = room_address(Room, BobNick),

            MucMsgs = ?config(pre_generated_muc_msgs, Config),
            WithJID = [1 || {_, _, {JID, _, _}, _, _} <- MucMsgs, JID == BWithJID],
            Len = lists:sum(WithJID),

            IQ = stanza_filtered_by_jid_request(P, BWithJID),
            escalus:send(Alice, stanza_to_room(IQ, Room)),
            Result = wait_archive_respond(Alice),

            assert_respond_size(Len, Result),
            ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_message_dropped(Config) ->
    TS = instrument_helper:timestamp(),
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"OH, HAI!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),
        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Alice, 3),

        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),
        escalus:wait_for_stanza(Alice),
        escalus:wait_for_stanza(Bob),
        maybe_wait_for_archive(Config),

        Stanza = stanza_archive_request(P, <<"q1">>),
        escalus:send(Alice, stanza_to_room(Stanza, Room)),
        assert_respond_size(0, wait_archive_respond(Alice)),
        ok
    end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F),
    assert_dropped_msg_event(mod_mam_muc_dropped, TS).

muc_light_service_discovery_stored_in_pm(Config) ->
    F = fun(Alice) ->
        Server = escalus_client:server(Alice),
        discover_features(Config, Alice, Server)
        end,
    escalus:fresh_story(Config, [{alice, 1}], F).

muc_light_easy(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, []),

            M1 = when_muc_light_message_is_sent(Alice, Room,
                                                <<"Msg 1">>, <<"Id1">>),
            then_muc_light_message_is_received_by([Alice], M1),

            M2 = when_muc_light_message_is_sent(Alice, Room,
                                                <<"Message 2">>, <<"MyID2">>),
            then_muc_light_message_is_received_by([Alice], M2),

            Aff = when_muc_light_affiliations_are_set(Alice, Room, [{Bob, member}]),
            then_muc_light_affiliations_are_received_by([Alice, Bob], Aff),

            mam_helper:wait_for_room_archive_size(muc_light_host(), Room, 4),
            when_archive_query_is_sent(Bob, muc_light_helper:room_bin_jid(Room), Config),
            ExpectedResponse = [{create, [{Alice, owner}]},
                                {muc_message, Room, Alice, <<"Msg 1">>},
                                {muc_message, Room, Alice, <<"Message 2">>},
                                {affiliations, [{Bob, member}]}],
            then_archive_response_is(Bob, ExpectedResponse, Config)
        end).

muc_light_shouldnt_modify_pm_archive(Config) ->
    TS = instrument_helper:timestamp(),
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, [{Bob, member}]),

            when_pm_message_is_sent(Alice, Bob, <<"private hi!">>),
            then_pm_message_is_received(Bob, <<"private hi!">>),

            maybe_wait_for_archive(Config),
            when_archive_query_is_sent(Alice, undefined, Config),
            then_archive_response_is(Alice, [{message, Alice, <<"private hi!">>}], Config),
            when_archive_query_is_sent(Bob, undefined, Config),
            then_archive_response_is(Bob, [{message, Alice, <<"private hi!">>}], Config),

            M1 = when_muc_light_message_is_sent(Alice, Room,
                                                <<"Msg 1">>, <<"Id 1">>),
            then_muc_light_message_is_received_by([Alice, Bob], M1),

            maybe_wait_for_archive(Config),
            when_archive_query_is_sent(Alice, muc_light_helper:room_bin_jid(Room), Config),
            then_archive_response_is(Alice, [{create, [{Alice, owner}, {Bob, member}]},
                                             {muc_message, Room, Alice, <<"Msg 1">>}], Config),

            when_archive_query_is_sent(Alice, undefined, Config),
            then_archive_response_is(Alice, [{message, Alice, <<"private hi!">>}], Config),
            when_archive_query_is_sent(Bob, undefined, Config),
            then_archive_response_is(Bob, [{message, Alice, <<"private hi!">>}], Config)
        end),
    assert_async_timed_flush_event(Config, TS, pm_mam),
    assert_async_timed_flush_event(Config, TS, muc_mam).

muc_light_stored_in_pm_if_allowed_to(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, [{Bob, member}]),

            maybe_wait_for_archive(Config),
            AliceAffEvent = {affiliations, [{Alice, owner}]},
            when_archive_query_is_sent(Alice, undefined, Config),
            then_archive_response_is(Alice, [AliceAffEvent], Config),
            BobAffEvent = {affiliations, [{Bob, member}]},
            when_archive_query_is_sent(Bob, undefined, Config),
            then_archive_response_is(Bob, [BobAffEvent], Config),

            M1 = when_muc_light_message_is_sent(Alice, Room, <<"Msg 1">>, <<"Id 1">>),
            then_muc_light_message_is_received_by([Alice, Bob], M1),

            maybe_wait_for_archive(Config),
            MessageEvent = {muc_message, Room, Alice, <<"Msg 1">>},
            when_archive_query_is_sent(Alice, undefined, Config),
            then_archive_response_is(Alice, [AliceAffEvent, MessageEvent], Config),
            when_archive_query_is_sent(Bob, undefined, Config),
            then_archive_response_is(Bob, [BobAffEvent, MessageEvent], Config)
        end).

muc_light_include_groupchat_filter(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            P = ?config(props, Config),
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, [{Bob, member}]),

            M1 = when_muc_light_message_is_sent(Alice, Room, <<"Msg 1">>, <<"Id 1">>),
            then_muc_light_message_is_received_by([Alice, Bob], M1),

            when_pm_message_is_sent(Alice, Bob, <<"private hi!">>),
            then_pm_message_is_received(Bob, <<"private hi!">>),

            maybe_wait_for_archive(Config),

            Stanza = stanza_include_groupchat_request(P, <<"q1">>, <<"false">>),
            escalus:send(Alice, Stanza),
            Res = wait_archive_respond(Alice),
            assert_respond_size(1, Res),

            Stanza2 = stanza_include_groupchat_request(P, <<"q2">>, <<"true">>),
            escalus:send(Alice, Stanza2),
            Res2 = wait_archive_respond(Alice),
            assert_respond_size(3, Res2),
            ok
        end).

muc_light_no_pm_stored_include_groupchat_filter(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            P = ?config(props, Config),
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, [{Bob, member}]),

            M1 = when_muc_light_message_is_sent(Alice, Room, <<"Msg 1">>, <<"Id 1">>),
            then_muc_light_message_is_received_by([Alice, Bob], M1),

            maybe_wait_for_archive(Config),

            Stanza = stanza_include_groupchat_request(P, <<"q1">>, <<"false">>),
            escalus:send(Alice, Stanza),
            Res = wait_archive_respond(Alice),
            assert_respond_size(0, Res),
            ok
        end).

muc_light_include_groupchat_messages_by_default(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        P = ?config(props, Config),
        MsgCount = 4,
        Room = muc_helper:fresh_room_name(),
        given_muc_light_room(Room, Alice, [{Bob, member}]),

        M1 = when_muc_light_message_is_sent(Alice, Room, <<"Msg 1">>, <<"Id 1">>),
        then_muc_light_message_is_received_by([Alice, Bob], M1),

        M2 = when_muc_light_message_is_sent(Alice, Room, <<"Msg 2">>, <<"Id 2">>),
        then_muc_light_message_is_received_by([Alice, Bob], M2),

        when_pm_message_is_sent(Alice, Bob, <<"private hi!">>),
        then_pm_message_is_received(Bob, <<"private hi!">>),

        maybe_wait_for_archive(Config),

        when_archive_query_is_sent(Alice, undefined, Config),
        Res = wait_archive_respond(Alice),

        Stanza = stanza_include_groupchat_request(P, <<"q1">>, <<"true">>),
        escalus:send(Alice, Stanza),
        Res2 = wait_archive_respond(Alice),

        assert_respond_size(MsgCount, Res),
        assert_respond_size(MsgCount, Res2),
        ok
        end).

muc_light_chat_markers_are_archived_if_enabled(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, [{Bob, member}]),

            %% Alice sends 3 chat markers
            MessageID = <<"some-fake-id">>,
            RoomJID = muc_light_helper:room_bin_jid(Room),
            lists:foreach(
              fun(Type) ->
                      Marker1 = escalus_stanza:chat_marker(RoomJID, Type, MessageID),
                      Marker2 = escalus_stanza:setattr(Marker1, <<"type">>, <<"groupchat">>),
                      escalus:send(Alice, Marker2),
                      escalus:wait_for_stanza(Alice),
                      escalus:wait_for_stanza(Bob)
              end, [<<"received">>, <<"displayed">>, <<"acknowledged">>]),

            maybe_wait_for_archive(Config),
            when_archive_query_is_sent(Bob, muc_light_helper:room_bin_jid(Room), Config),
            ExpectedResponse = [
                                {create, [{Alice, owner}, {Bob, member}]},
                                {chat_marker, <<"received">>},
                                {chat_marker, <<"displayed">>},
                                {chat_marker, <<"acknowledged">>}
                               ],
            then_archive_response_is(Bob, ExpectedResponse, Config)
        end).

muc_light_chat_markers_are_not_archived_if_disabled(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, [{Bob, member}]),

            %% Alice sends 3 chat markers
            MessageID = <<"some-fake-id">>,
            RoomJID = muc_light_helper:room_bin_jid(Room),
            lists:foreach(
              fun(Type) ->
                      Marker1 = escalus_stanza:chat_marker(RoomJID, Type, MessageID),
                      Marker2 = escalus_stanza:setattr(Marker1, <<"type">>, <<"groupchat">>),
                      escalus:send(Alice, Marker2),
                      escalus:wait_for_stanza(Alice),
                      escalus:wait_for_stanza(Bob)
              end, [<<"received">>, <<"displayed">>, <<"acknowledged">>]),

            maybe_wait_for_archive(Config),
            when_archive_query_is_sent(Bob, muc_light_helper:room_bin_jid(Room), Config),
            ExpectedResponse = [{create, [{Alice, owner}, {Bob, member}]}],
            then_archive_response_is(Bob, ExpectedResponse, Config)
        end).

muc_light_failed_to_decode_message_in_database(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, []),
            M1 = when_muc_light_message_is_sent(Alice, Room,
                                                <<"Msg 1">>, <<"Id1">>),
            then_muc_light_message_is_received_by([Alice], M1),
            mam_helper:wait_for_room_archive_size(muc_light_host(), Room, 2),
            NewMods = muc_with_db_message_format_xml(Config),
            %% Change the encoding format for messages in the database
            dynamic_modules:ensure_modules(host_type(), NewMods),
            when_archive_query_is_sent(Alice, muc_light_helper:room_bin_jid(Room), Config),
            [ArcMsg | _] = respond_messages(assert_respond_size(2, wait_archive_respond(Alice))),
            assert_failed_to_decode_message(ArcMsg)
        end).

muc_light_sql_query_failed(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, []),
            M1 = when_muc_light_message_is_sent(Alice, Room,
                                                <<"Msg 1">>, <<"Id1">>),
            then_muc_light_message_is_received_by([Alice], M1),
            mam_helper:wait_for_room_archive_size(muc_light_host(), Room, 2),
            when_archive_query_is_sent(Alice, muc_light_helper:room_bin_jid(Room), Config),
            Error = escalus:wait_for_stanza(Alice),
            escalus:assert(is_error, [<<"wait">>, <<"internal-server-error">>], Error)
        end).

muc_light_async_pools_batch_flush(Config) ->
    TS = instrument_helper:timestamp(),
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            Room = muc_helper:fresh_room_name(),
            given_muc_light_room(Room, Alice, []),

            M1 = when_muc_light_message_is_sent(Alice, Room,<<"Msg 1">>, <<"Id1">>),
            then_muc_light_message_is_received_by([Alice], M1),

            M2 = when_muc_light_message_is_sent(Alice, Room, <<"Msg 2">>, <<"Id2">>),
            then_muc_light_message_is_received_by([Alice], M2),

            M3 = when_muc_light_message_is_sent(Alice, Room, <<"Msg 3">>, <<"Id3">>),
            then_muc_light_message_is_received_by([Alice], M3)
        end),
    assert_async_batch_flush_event(TS, 1, muc_mam).

pm_failed_to_decode_message_in_database(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi">>)),
            mam_helper:wait_for_archive_size(Alice, 1),
            NewMods = pm_with_db_message_format_xml(Config),
            %% Change the encoding format for messages in the database
            dynamic_modules:ensure_modules(host_type(), NewMods),
            when_archive_query_is_sent(Alice, undefined, Config),
            [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Alice))),
            assert_failed_to_decode_message(ArcMsg)
        end).

pm_sql_query_failed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            mam_helper:wait_for_archive_size(Alice, 1),
            when_archive_query_is_sent(Alice, undefined, Config),
            Error = escalus:wait_for_stanza(Alice),
            escalus:assert(is_error, [<<"wait">>, <<"internal-server-error">>], Error)
        end).

async_pools_batch_flush(Config) ->
    TS = instrument_helper:timestamp(),
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Msg 1">>)),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Msg 2">>)),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Msg 3">>)),
            mam_helper:wait_for_archive_size(Alice, 3)
        end),
    assert_async_batch_flush_event(TS, 2, pm_mam).

retrieve_form_fields(ConfigIn) ->
    escalus_fresh:story(ConfigIn, [{alice, 1}], fun(Alice) ->
        P = ?config(props, ConfigIn),
        Namespace = get_prop(mam_ns, P),
        escalus:send(Alice, stanza_retrieve_form_fields(<<"q">>, Namespace)),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_with_ns, [Namespace], Res)
    end).

retrieve_form_fields_extra_features(ConfigIn) ->
    escalus_fresh:story(ConfigIn, [{alice, 1}], fun(Alice) ->
        P = ?config(props, ConfigIn),
        Namespace = get_prop(mam_ns, P),
        escalus:send(Alice, stanza_retrieve_form_fields(<<"q">>, Namespace)),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_with_ns, [Namespace], Res),
        QueryEl = exml_query:subelement(Res, <<"query">>),
        XEl = exml_query:subelement(QueryEl, <<"x">>),
        IDsEl = exml_query:subelement_with_attr(XEl, <<"var">>, <<"ids">>),
        ValidateEl = exml_query:path(IDsEl, [{element_with_ns, <<"validate">>, data_validate_ns()},
                                             {element, <<"open">>}]),
        escalus:assert(has_field_with_type, [<<"before-id">>, <<"text-single">>], XEl),
        escalus:assert(has_field_with_type, [<<"after-id">>, <<"text-single">>], XEl),
        escalus:assert(has_field_with_type, [<<"include-groupchat">>, <<"boolean">>], XEl),
        ?assertNotEqual(ValidateEl, undefined)
    end).

archived(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        Msg = escalus:wait_for_stanza(Bob),
        StanzaId = exml_query:subelement(Msg, <<"stanza-id">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(StanzaId, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(StanzaId, <<"id">>),

        ?assert_equal(By, escalus_client:short_jid(Bob)),

        %% Bob calls archive.
        maybe_wait_for_archive(Config),
        escalus:send(Bob, stanza_archive_request(P, <<"q1">>)),
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Bob))),
        #forwarded_message{result_id=ArcId} = parse_forwarded_message(ArcMsg),
        ?assert_equal(Id, ArcId),
        ok
        end,
    %% Made fresh in init_per_testcase
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).


message_with_stanzaid(Config) ->
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        Msg = escalus:wait_for_stanza(Bob),

        ArcStanzaid = exml_query:subelement(Msg, <<"stanza-id">>),

        %% stanza-id has a namespace 'urn:xmpp:sid:0'
        <<"urn:xmpp:sid:0">> = exml_query:attr(ArcStanzaid, <<"xmlns">>),
        ok
    end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

retract_message_on_stanza_id(Config) ->
    test_retract_message([{retract_on, stanza_id} | Config]).

retract_wrong_message(Config) ->
    test_retract_message([{retract_on, {origin_id, <<"wrong-id">>}} | Config]).

ignore_bad_retraction(Config) ->
    test_retract_message([{retract_on, none} | Config]).

retract_message(Config) ->
    test_retract_message([{retract_on, {origin_id, origin_id()}} | Config]).

test_retract_message(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% GIVEN Alice sends a message with 'origin-id' to Bob
        Body = <<"OH, HAI!">>,
        OriginIdElement = origin_id_element(origin_id()),
        Msg = #xmlel{children = Children} = escalus_stanza:chat_to(Bob, Body),
        escalus:send(Alice, Msg#xmlel{children = Children ++ [OriginIdElement]}),

        mam_helper:wait_for_archive_size(Alice, 1),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Result = wait_archive_respond(Alice),
        [AliceCopyOfMessage] = respond_messages(Result),

        %% ... and Bob receives the message
        RecvMsg = escalus:wait_for_stanza(Bob),
        ?assert_equal(OriginIdElement, exml_query:subelement(RecvMsg, <<"origin-id">>)),

        %% WHEN Alice retracts the message
        ApplyToElement = apply_to_element(Config, AliceCopyOfMessage),
        RetractMsg = retraction_message(<<"chat">>, escalus_utils:get_jid(Bob), ApplyToElement),
        escalus:send(Alice, RetractMsg),

        %% THEN Bob receives the message with 'retract' ...
        RecvRetract = escalus:wait_for_stanza(Bob),
        ?assert_equal(ApplyToElement, exml_query:subelement(RecvRetract, <<"apply-to">>)),

        maybe_wait_for_archive(Config),

        %% ... and Alice and Bob have both messages in their archives
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        check_archive_after_retraction(Config, Alice, ApplyToElement, Body),
        escalus:send(Bob, stanza_archive_request(P, <<"q2">>)),
        check_archive_after_retraction(Config, Bob, ApplyToElement, Body),

        ok
    end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

filter_forwarded(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        escalus:wait_for_stanza(Bob),
        mam_helper:wait_for_archive_size(Bob, 1),
        escalus:send(Bob, stanza_archive_request(P, <<"q1">>)),
        assert_respond_size(1, wait_archive_respond(Bob)),

        %% Check, that previous forwarded message was not archived.
        escalus:send(Bob, stanza_archive_request(P, <<"q2">>)),
        assert_respond_size(1, wait_archive_respond(Bob)),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

%% Ensure, that a offline message does not stored twice when delivered.
offline_message(Config) ->
    Msg = <<"Is there anybody here?">>,
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Alice sends a message to Bob while bob is offline.
        escalus:send(Alice,
                     escalus_stanza:chat_to(escalus_users:get_jid(Config, bob), Msg)),
        maybe_wait_for_archive(Config),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% If mod_offline is enabled, then an offline message
    %% will be delivered automatically.

    %% He receives his initial presence and the message.
    escalus:wait_for_stanzas(Bob, 2, 1000),

    %% Bob checks his archive.
    escalus:send(Bob, stanza_archive_request(P, <<"q1">>)),
    ArcMsgs = respond_messages(wait_archive_respond(Bob)),
    assert_only_one_of_many_is_equal(ArcMsgs, Msg),

    escalus_client:stop(Config, Bob).

nostore_hint(Config) ->
    Msg = <<"So secret">>,
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Alice sends a message to Bob with a hint.
        escalus:send(Alice,
                     add_nostore_hint(escalus_stanza:chat_to(Bob, Msg))),
        maybe_wait_for_archive(Config),
        escalus:wait_for_stanzas(Bob, 1, 1000),

        %% Bob checks his archive.
        escalus:send(Bob, stanza_archive_request(P, <<"q1">>)),
        ArcMsgs = respond_messages(wait_archive_respond(Bob)),
        assert_not_stored(ArcMsgs, Msg),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

muc_message_with_stanzaid(Config) ->
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"Hi, Bob!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice sends another message to Bob.
        %% The message is not archived by the room.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:assert(is_message, escalus:wait_for_stanza(Bob)),

        %% Alice sends to the chat room.
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by alicesroom@localhost).
        %% User's archive is disabled (i.e. bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        ArcStanzaid = exml_query:subelement(BobMsg, <<"stanza-id">>),

        %% stanza-id has a namespace 'urn:xmpp:sid:0'
        Xmlns = exml_query:attr(ArcStanzaid, <<"xmlns">>),
        ?assert_equal(Xmlns, <<"urn:xmpp:sid:0">>),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

retract_muc_message_on_stanza_id(Config) ->
    test_retract_muc_message([{retract_on, stanza_id} | Config]).

retract_wrong_muc_message(Config) ->
    test_retract_muc_message([{retract_on, {origin_id, <<"wrong-id">>}} | Config]).

retract_muc_message(Config) ->
    test_retract_muc_message([{retract_on, {origin_id, origin_id()}} | Config]).

test_retract_muc_message(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),
        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),
        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),
        %% Alice receives all the messages Bob did as well
        escalus:wait_for_stanzas(Alice, 3),

        %% GIVEN Alice sends a message with 'origin-id' to the chat room ...
        Body = <<"Hi, Bob!">>,
        OriginIdElement = origin_id_element(origin_id()),
        Msg = #xmlel{children = Children} = escalus_stanza:groupchat_to(RoomAddr, Body),
        escalus:send(Alice, Msg#xmlel{children = Children ++ [OriginIdElement]}),

        AliceCopyOfMessage = escalus:wait_for_stanza(Alice),

        %% ... and Bob receives the message
        RecvMsg = escalus:wait_for_stanza(Bob),
        ?assert_equal(OriginIdElement, exml_query:subelement(RecvMsg, <<"origin-id">>)),

        %% WHEN Alice retracts the message
        ApplyToElement = apply_to_element(Config, AliceCopyOfMessage),
        RetractMsg = retraction_message(<<"groupchat">>, RoomAddr, ApplyToElement),
        escalus:send(Alice, RetractMsg),

        %% THEN Bob receives the message with 'retract' ...
        RecvRetract = escalus:wait_for_stanza(Bob),
        ?assert_equal(ApplyToElement, exml_query:subelement(RecvRetract, <<"apply-to">>)),

        maybe_wait_for_archive(Config),

        %% ... and finds both messages in the room archive
        escalus:send(Bob, stanza_to_room(stanza_archive_request(P, <<"q1">>), Room)),
        check_archive_after_retraction(Config, Bob, ApplyToElement, Body),

        ok
    end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_archive_is_instrumented(Config) ->
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"Hi, Bob!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob received presences, the room's subject and the message.
        escalus:wait_for_stanzas(Bob, 4),
        assert_archive_message_event(mod_mam_muc_archive_message, RoomAddr),
        maybe_wait_for_archive(Config),
        assert_flushed_event_if_async(mod_mam_muc_flushed, Config),

        mam_helper:delete_room_archive(muc_host(), ?config(room, Config)),
        assert_event_with_jid(mod_mam_muc_remove_archive, RoomAddr)
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"Hi, Bob!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice sends another message to Bob.
        %% The message is not archived by the room.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:assert(is_message, escalus:wait_for_stanza(Bob)),

        %% Alice sends to the chat room.
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by alicesroom@localhost).
        %% User's archive is disabled (i.e. bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        Arc = exml_query:subelement(BobMsg, <<"stanza-id">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),

        maybe_wait_for_archive(Config),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(P, <<"q1">>), Room)),
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Bob))),
        #forwarded_message{result_id=ArcId, message_body=ArcMsgBody,
                           message_to=MsgTo, message_from=MsgFrom} =
            parse_forwarded_message(ArcMsg),
        %% XEP: the 'to' of the forwarded stanza MUST be empty
        %% However, Smack crashes if it is present, so it is removed
        ?assert_equal_extra(undefined, MsgTo, message_to),

        %% XEP: the 'from' MUST be the occupant JID of the sender of the archived message
        ?assert_equal_extra(escalus_utils:jid_to_lower(room_address(Room, nick(Alice))),
                            escalus_utils:jid_to_lower(MsgFrom), message_from),

        ?assert_equal(Text, ArcMsgBody),
        ?assert_equal(ArcId, Id),
        ?assert_equal(escalus_utils:jid_to_lower(RoomAddr), By),
        ?assert_equal_extra(true, has_x_user_element(ArcMsg),
                            [{forwarded_message, ArcMsg}]),
        assert_lookup_event(mod_mam_muc_lookup, escalus_utils:get_jid(Bob)),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_validate_mam_id(Config0) ->
    P = ?config(props, Config0),
    F = fun(Config, Alice, Bob) ->
        mock_mongoose_mam_id(mim()),
        StartTS = erlang:system_time(microsecond),
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice sends to the chat room.
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, <<"Hi">>)),

        %% Bob received the message "Hi".
        %% This message will be archived (by alicesroom@localhost).
        escalus:wait_for_stanza(Bob),
        mam_helper:wait_for_room_archive_size(muc_host(), Room, 1),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(P, <<"q1">>), Room)),
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Bob))),
        #forwarded_message{result_id=ArcId} = parse_forwarded_message(ArcMsg),

        %% Check that timestamp is greater than the initial one
        MessId = rpc_apply(mod_mam_utils, external_binary_to_mess_id, [ArcId]),
        {ArcTS, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [MessId]),
        case ArcTS < StartTS of
            true ->
                ct:fail({bad_mam_id, ArcId, ArcTS, StartTS, MessId});
            false ->
                ok
        end
        end,
    muc_helper:story_with_room(Config0, [{persistent, true}], [{alice, 1}, {bob, 1}], F).

muc_multiple_devices(Config) ->
    P = ?config(props, Config),
    F = fun(Alice1, Alice2, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"Hi, Bob!">>,
        %% You should use an unique nick for each device.
        escalus:send(Alice1, stanza_muc_enter_room(Room, <<"alice_1">>)),
        escalus:send(Alice2, stanza_muc_enter_room(Room, <<"alice_2">>)),
        escalus:send(Bob, stanza_muc_enter_room(Room, <<"bob">>)),

        %% Alice received presences.
        escalus:wait_for_stanzas(Alice1, 3),
        escalus:wait_for_stanzas(Alice2, 3),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 3),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice received the room's subject.
        escalus:wait_for_stanzas(Alice1, 1),
        escalus:wait_for_stanzas(Alice2, 1),

        %% Alice sends to the chat room.
        escalus:send(Alice1, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Alice receives her own message.
        Alice1Msg = escalus:wait_for_stanza(Alice1),
        escalus:assert(is_message, Alice1Msg),

        Alice2Msg = escalus:wait_for_stanza(Alice2),
        escalus:assert(is_message, Alice2Msg),

        Alice1Arc = exml_query:subelement(Alice1Msg, <<"stanza-id">>),
        Alice2Arc = exml_query:subelement(Alice2Msg, <<"stanza-id">>),
        ?assert_equal(Alice1Arc, Alice2Arc),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by alicesroom@localhost).
        %% User's archive is disabled (i.e. bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        Arc = exml_query:subelement(BobMsg, <<"stanza-id">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),

        ?assert_equal(Alice1Arc, Arc),

        %% Bob requests the room's archive.

        maybe_wait_for_archive(Config),

        escalus:send(Bob, stanza_to_room(stanza_archive_request(P, <<"q1">>), Room)),
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Bob))),
        #forwarded_message{result_id=ArcId, message_body=ArcMsgBody} =
            parse_forwarded_message(ArcMsg),
        ?assert_equal(Text, ArcMsgBody),
        ?assert_equal(ArcId, Id),
        ?assert_equal(escalus_utils:jid_to_lower(RoomAddr), By),
        ok
        end,
    escalus:story(Config, [{alice, 2}, {bob, 1}], F).

muc_protected_message(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        Text = <<"Hi, Bob!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice sends to Bob, using his occupant JID.
        %% This message will not be put into room's history.
        Msg = escalus_stanza:chat_to(room_address(Room, nick(Bob)), Text),
        escalus:send(Alice, Msg),

        %% Bob received the message "Hi, Bob!".
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),

        BobArchiveAddr = escalus_client:short_jid(Bob),
        ArchivedBy = [exml_query:attr(Arc, <<"by">>)
                      || Arc <- BobMsg#xmlel.children,
                         Arc#xmlel.name =:= <<"archived">>,
                         BobArchiveAddr =/= exml_query:attr(Arc, <<"by">>)],
        ?assert_equal([], ArchivedBy),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(P, <<"q1">>), Room)),
        assert_respond_size(0, wait_archive_respond(Bob)),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_deny_protected_room_access(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"Hi, Bob!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% mod_muc returns error presence.
        Err1 = escalus:wait_for_stanza(Bob),
        escalus_assert:is_error(Err1, <<"auth">>, <<"not-authorized">>),

        %% Alice sends to the chat room.
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(P, <<"q1">>), Room)),
        Err2 = escalus:wait_for_stanza(Bob),
        %% mod_mam_muc returns error iq.
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Err2),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

%% @doc Allow access to non-in-room users who able to connect
muc_allow_access_to_owner(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, _Bob) ->
        Room = ?config(room, Config),
        _RoomAddr = room_address(Room),

        %% Alice (not in room) requests the room's archive.
        escalus:send(Alice, stanza_to_room(stanza_archive_request(P, <<"q1">>), Room)),
        %% mod_mam_muc returns result.
        assert_respond_size(0, wait_archive_respond(Alice)),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_sanitize_x_user_in_non_anon_rooms(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {Room, _} = enter_room(Config, [Alice, Bob]),
        Text = <<"Hi all!">>,
        SpoofJid = <<"spoof@test.com">>,

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice received presences.
        escalus:wait_for_stanzas(Alice, 2),

        %% Alice received the room's subject.
        escalus:wait_for_stanzas(Alice, 1),

        X = #xmlel{name = <<"x">>,
                   attrs = #{<<"xmlns">> => <<"http://jabber.org/protocol/muc#user">>},
                   children = [#xmlel{name = <<"item">>,
                                      attrs = #{<<"affiliation">> => <<"owner">>,
                                                <<"jid">> => SpoofJid,
                                                <<"role">> => <<"moderator">>}}]},
        Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content = Text}]},
        Stanza = #xmlel{name = <<"message">>, attrs = #{<<"type">> => <<"groupchat">>},
                        children = [Body, X]},

        %% Bob sends to the chat room.
        escalus:send(Bob, stanza_to_room(Stanza, Room)),

        %% Alice receives the message.
        escalus:assert(is_message, escalus:wait_for_stanza(Alice)),

        maybe_wait_for_archive(Config),
        Props = ?config(props, Config),

        %% Alice requests the room's archive.
        escalus:send(Alice, stanza_to_room(stanza_archive_request(Props, <<"q1">>), Room)),

        %% mod_mam_muc returns result.
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Alice))),
        Item = exml_query:path(ArcMsg, [{element, <<"result">>},
                                        {element, <<"forwarded">>},
                                        {element, <<"message">>},
                                        {element, <<"x">>},
                                        {element, <<"item">>}]),

        Jid = exml_query:attr(Item, <<"jid">>),
        ?assertNotEqual(Jid, SpoofJid),
        ok
    end).

muc_delete_x_user_in_anon_rooms(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {Room, RoomAddr} = enter_room(Config, [Alice, Bob]),
        Text = <<"Hi all!">>,

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice sends to the chat room.
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob receives the message.
        escalus:assert(is_message, escalus:wait_for_stanza(Bob)),

        maybe_wait_for_archive(Config),
        Props = ?config(props, Config),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(Props, <<"q1">>), Room)),

        %% mod_mam_muc returns result.
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Bob))),

        ?assert_equal_extra(false, has_x_user_element(ArcMsg),
                            [{forwarded_message, ArcMsg}]),
        ok
    end).

muc_show_x_user_to_moderators_in_anon_rooms(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {Room, RoomAddr} = enter_room(Config, [Alice, Bob]),
        Text = <<"What a lovely day!">>,

        %% Alice received presences.
        escalus:wait_for_stanzas(Alice, 2),

        %% Alice received the room's subject.
        escalus:wait_for_stanzas(Alice, 1),

        %% Bob sends to the chat room.
        escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Alice receives the message.
        escalus:assert(is_message, escalus:wait_for_stanza(Alice)),

        maybe_wait_for_archive(Config),
        Props = ?config(props, Config),

        %% Alice requests the room's archive.
        escalus:send(Alice, stanza_to_room(stanza_archive_request(Props, <<"q1">>), Room)),

        %% mod_mam_muc returns result.
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Alice))),

        ?assert_equal_extra(true, has_x_user_element(ArcMsg),
                            [{forwarded_message, ArcMsg}]),
        ok
    end).

muc_show_x_user_for_your_own_messages_in_anon_rooms(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {Room, RoomAddr} = enter_room(Config, [Alice, Bob]),
        Text = <<"How are you?">>,

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Bob sends to the chat room.
        escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob receives the message.
        escalus:assert(is_message, escalus:wait_for_stanza(Bob)),

        maybe_wait_for_archive(Config),
        Props = ?config(props, Config),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(Props, <<"q1">>), Room)),

        %% mod_mam_muc returns result.
        [ArcMsg] = respond_messages(assert_respond_size(1, wait_archive_respond(Bob))),

        ?assert_equal_extra(true, has_x_user_element(ArcMsg),
                            [{forwarded_message, ArcMsg}]),
        ok
    end).

%% @doc Querying the archive for all messages in a certain timespan.
range_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Send
        %% <iq type='get'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%     <start>2010-06-07T00:00:00Z</start>
        %%     <end>2010-07-07T13:23:54Z</end>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_date_range_archive_request(P)),
        IQ = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_iq_result, IQ),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

range_archive_request_not_empty(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Msgs = ?config(pre_generated_msgs, Config),
        [_, _, StartMsg, StopMsg | _] = Msgs,
        {{StartMsgId, _}, _, _, _, _StartMsgPacket} = StartMsg,
        {{StopMsgId, _}, _, _, _, _StopMsgPacket} = StopMsg,
        {StartMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [StartMsgId]),
        {StopMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [StopMsgId]),
        StartTime = make_iso_time(StartMicro),
        StopTime = make_iso_time(StopMicro),
        %% Send
        %% <iq type='get'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%     <start>StartTime</start>
        %%     <end>StopTime</end>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_date_range_archive_request_not_empty(P, StartTime, StopTime)),
        %% Receive two messages and IQ
        Result = wait_archive_respond(Alice),
        IQ = respond_iq(Result),
        [M1, M2|_] = respond_messages(Result),
        escalus:assert(is_iq_result, IQ),
        #forwarded_message{delay_stamp=Stamp1} = parse_forwarded_message(M1),
        #forwarded_message{delay_stamp=Stamp2} = parse_forwarded_message(M2),
        ?assert_equal(list_to_binary(StartTime), Stamp1),
        ?assert_equal(list_to_binary(StopTime), Stamp2),
        ok
        end,
    %% Made fresh in init_per_testcase
    escalus:story(Config, [{alice, 1}], F).

%% @doc A query using Result Set Management.
%% See also `#rsm_in.max'.
limit_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Send
        %% <iq type='get' id='q29302'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%       <start>2010-08-07T00:00:00Z</start>
        %%       <set xmlns='http://jabber.org/protocol/rsm'>
        %%          <limit>10</limit>
        %%       </set>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_limit_archive_request(P)),
        Result = wait_archive_respond(Alice),
        Msgs = respond_messages(Result),
        IQ = respond_iq(Result),
        escalus:assert(is_iq_result, IQ),
        10 = length(Msgs),
        ok
        end,
    %% Made fresh in init_per_testcase
    escalus:story(Config, [{alice, 1}], F).

metadata_archive_request(Config) ->
    F = fun(Alice) ->
        Msgs = ?config(pre_generated_msgs, Config),

        {{StartMsgId, _}, _, _, _, _} = hd(Msgs),
        {{EndMsgId, _}, _, _, _, _} = lists:last(Msgs),
        {StartMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [StartMsgId]),
        {EndMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [EndMsgId]),

        StartTime = list_to_binary(make_iso_time(StartMicro)),
        EndTime = list_to_binary(make_iso_time(EndMicro)),
        StartId = rpc_apply(mod_mam_utils, mess_id_to_external_binary, [StartMsgId]),
        EndId = rpc_apply(mod_mam_utils, mess_id_to_external_binary, [EndMsgId]),

        Stanza = stanza_metadata_request(),
        escalus:send(Alice, Stanza),
        IQ = escalus:wait_for_stanza(Alice),

        Start = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"start">>}]),
        ?assertEqual(StartId, exml_query:attr(Start, <<"id">>)),
        ?assertEqual(StartTime, exml_query:attr(Start, <<"timestamp">>)),

        End = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"end">>}]),
        ?assertEqual(EndId, exml_query:attr(End, <<"id">>)),
        ?assertEqual(EndTime, exml_query:attr(End, <<"timestamp">>)),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

metadata_archive_request_empty(Config) ->
    F = fun(Alice) ->
        Stanza = stanza_metadata_request(),
        escalus:send(Alice, Stanza),
        IQ = escalus:wait_for_stanza(Alice),

        Metadata = exml_query:path(IQ, [{element, <<"metadata">>}]),
        Start = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"start">>}]),
        End = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"end">>}]),

        ?assertNotEqual(Metadata, undefined),
        ?assertEqual(Start, undefined),
        ?assertEqual(End, undefined),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

metadata_archive_request_one_message(Config) ->
    F = fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:wait_for_stanza(Bob),

        mam_helper:wait_for_archive_size(Alice, 1),
        maybe_wait_for_archive(Config),

        Stanza = stanza_metadata_request(),
        escalus:send(Alice, Stanza),
        IQ = escalus:wait_for_stanza(Alice),

        Start = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"start">>}]),
        End = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"end">>}]),
        ?assertEqual(exml_query:attr(Start, <<"id">>), exml_query:attr(End, <<"id">>)),
        ?assertEqual(exml_query:attr(Start, <<"timestamp">>),
                     exml_query:attr(End, <<"timestamp">>)),
        ok
    end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_metadata_archive_request(Config) ->
    F = fun(Alice) ->
            Room = ?config(room, Config),
            MucMsgs = ?config(pre_generated_muc_msgs, Config),

            {{StartMsgId, _}, _, _, _, _} = hd(MucMsgs),
            {{EndMsgId, _}, _, _, _, _} = lists:last(MucMsgs),
            {StartMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [StartMsgId]),
            {EndMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid, [EndMsgId]),

            StartTime = list_to_binary(make_iso_time(StartMicro)),
            EndTime = list_to_binary(make_iso_time(EndMicro)),
            StartId = rpc_apply(mod_mam_utils, mess_id_to_external_binary, [StartMsgId]),
            EndId = rpc_apply(mod_mam_utils, mess_id_to_external_binary, [EndMsgId]),

            Stanza = stanza_metadata_request(),
            escalus:send(Alice, stanza_to_room(Stanza, Room)),
            IQ = escalus:wait_for_stanza(Alice),

            Start = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"start">>}]),
            ?assertEqual(StartId, exml_query:attr(Start, <<"id">>)),
            ?assertEqual(StartTime, exml_query:attr(Start, <<"timestamp">>)),

            End = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"end">>}]),
            ?assertEqual(EndId, exml_query:attr(End, <<"id">>)),
            ?assertEqual(EndTime, exml_query:attr(End, <<"timestamp">>)),
            ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_metadata_archive_request_empty(Config) ->
    F = fun(Alice) ->
        Room = ?config(room, Config),
        Stanza = stanza_metadata_request(),
        escalus:send(Alice, stanza_to_room(Stanza, Room)),
        IQ = escalus:wait_for_stanza(Alice),

        Metadata = exml_query:path(IQ, [{element, <<"metadata">>}]),
        Start = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"start">>}]),
        End = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"end">>}]),

        ?assertNotEqual(Metadata, undefined),
        ?assertEqual(Start, undefined),
        ?assertEqual(End, undefined),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_metadata_archive_request_one_message(Config) ->
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Text = <<"OH, HAI!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        %% Bob received presences.
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob received the room's subject.
        escalus:wait_for_stanzas(Bob, 1),

        %% Alice received presences.
        escalus:wait_for_stanzas(Alice, 2),

        %% Alice received the room's subject.
        escalus:wait_for_stanzas(Alice, 1),

        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),
        escalus:wait_for_stanza(Alice),
        escalus:wait_for_stanza(Bob),

        mam_helper:wait_for_room_archive_size(muc_host(), Room, 1),
        maybe_wait_for_archive(Config),

        Stanza = stanza_metadata_request(),
        escalus:send(Alice, stanza_to_room(Stanza, Room)),
        IQ = escalus:wait_for_stanza(Alice),

        Start = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"start">>}]),
        End = exml_query:path(IQ, [{element, <<"metadata">>}, {element, <<"end">>}]),
        ?assertEqual(exml_query:attr(Start, <<"id">>), exml_query:attr(End, <<"id">>)),
        ?assertEqual(exml_query:attr(Start, <<"timestamp">>),
                     exml_query:attr(End, <<"timestamp">>)),
        ok
    end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

archive_chat_markers(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
            %% Alice sends markable message to Bob
            Message  = escalus_stanza:markable(
                          escalus_stanza:chat_to(Bob, <<"Hello, Bob!">>)
                       ),
            MessageID = escalus_stanza:id(),
            escalus:send(Alice, escalus_stanza:set_id(Message, MessageID)),
            escalus:wait_for_stanza(Bob),

            %% Bob sends 3 chat markers
            Marker1 = escalus_stanza:chat_marker(Alice, <<"received">>,
                                                 MessageID),
            Marker2 = escalus_stanza:chat_marker(Alice, <<"displayed">>,
                                                 MessageID),
            Marker3 = escalus_stanza:chat_marker(Alice, <<"acknowledged">>,
                                                 MessageID),
            escalus:send(Bob, Marker1),
            escalus:send(Bob, Marker2),
            escalus:send(Bob, Marker3),
            escalus:wait_for_stanzas(Alice, 3),

            %% Alice queries MAM
            maybe_wait_for_archive(Config),
            escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
            Result = wait_archive_respond(Alice),

            %% archived message + 3 markers
            assert_respond_size(1 + 3, Result),
            assert_respond_query_id(P, <<"q1">>, parse_result_iq(Result))
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

dont_archive_chat_markers(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
            %% Alice sends markable message to Bob
            Message  = escalus_stanza:markable(
                          escalus_stanza:chat_to(Bob, <<"Hello, Bob!">>)
                       ),
            MessageID = escalus_stanza:id(),
            escalus:send(Alice, escalus_stanza:set_id(Message, MessageID)),
            escalus:wait_for_stanza(Bob),

            %% Bob sends 3 chat markers which also contain non-archivable elements
            Marker = #xmlel{children = Children} =
                escalus_stanza:chat_marker(Alice, <<"received">>, MessageID),
            ResultEl = #xmlel{name = <<"result">>},
            DelayEl = #xmlel{name = <<"delay">>},
            NoStoreEl = mam_helper:hint_elem(no_store),

            escalus:send(Bob, Marker#xmlel{children = [ResultEl|Children]}),
            escalus:send(Bob, Marker#xmlel{children = [DelayEl|Children]}),
            escalus:send(Bob, Marker#xmlel{children = [NoStoreEl|Children]}),
            escalus:wait_for_stanzas(Alice, 3),

            %% Alice queries MAM
            maybe_wait_for_archive(Config),
            escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
            Result = wait_archive_respond(Alice),

            %% archived message (no archived markers)
            assert_respond_size(1, Result),
            assert_respond_query_id(P, <<"q1">>, parse_result_iq(Result))
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

pagination_empty_rset(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=0},

        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"empty_rset">>, RSM)),
        wait_empty_rset(Alice, 15)
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_first5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"first5">>, RSM)),
        wait_message_range(Alice, 1, 5),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_first0(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 0.
        RSM = #rsm_in{max=0},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"first5">>, RSM)),
        wait_empty_rset(Alice, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_last5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last5">>, RSM)),
        wait_message_range(Alice, 11, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_last0(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 0.
        RSM = #rsm_in{max=0, direction=before},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last0">>, RSM)),
        wait_empty_rset(Alice, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_offset5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Skip 5 messages, get 5 messages.
        RSM = #rsm_in{max=5, index=5},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"offset5">>, RSM)),
        wait_message_range(Alice, 6, 10),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_offset5_max0(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Skip 5 messages, get 0 messages.
        RSM = #rsm_in{max=0, index=5},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"offset0_max5">>, RSM)),
        wait_empty_rset(Alice, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_before10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"before10">>, RSM)),
        wait_message_range(Alice, 5, 9),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_flipped_page(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        rsm_send(Config, Alice,
            stanza_flip_page_archive_request(P, <<"first5">>, RSM)),
        wait_message_range(Alice, 5, 1),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_simple_before10(Config) ->
    RSM = #rsm_in{max = 5, direction = before, id = message_id(10, Config), simple = true},
    pagination_test(before10, RSM, simple_range(5, 9, false), Config).

pagination_simple_before3(Config) ->
    RSM = #rsm_in{max = 5, direction = before, id = message_id(3, Config), simple = true},
    pagination_test(before3, RSM, simple_range(1, 2, true), Config).

pagination_simple_before6(Config) ->
    RSM = #rsm_in{max = 5, direction = before, id = message_id(6, Config), simple = true},
    pagination_test(before6, RSM, simple_range(1, 5, true), Config).

pagination_simple_before1_pagesize0(Config) ->
    %% No messages forwarded, but is_complete is set
    RSM = #rsm_in{max = 0, direction = before, id = message_id(1, Config), simple = true},
    pagination_test(before1, RSM, simple_range(undefined, undefined, true), Config).

pagination_simple_before2_pagesize0(Config) ->
    RSM = #rsm_in{max = 0, direction = before, id = message_id(2, Config), simple = true},
    pagination_test(before2, RSM, simple_range(undefined, undefined, false), Config).

pagination_simple_after5(Config) ->
    RSM = #rsm_in{max = 3, direction = 'after', id = message_id(5, Config), simple = true},
    pagination_test(after5, RSM, simple_range(6, 8, false), Config).

pagination_simple_after10(Config) ->
    RSM = #rsm_in{max = 5, direction = 'after', id = message_id(10, Config), simple = true},
    pagination_test(after10, RSM, simple_range(11, 15, true), Config).

pagination_simple_after12(Config) ->
    RSM = #rsm_in{max = 5, direction = 'after', id = message_id(12, Config), simple = true},
    pagination_test(after12, RSM, simple_range(13, 15, true), Config).

pagination_after10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction='after', id=message_id(10, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"after10">>, RSM)),
        wait_message_range(Alice, 11, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

%% Select first page of recent messages after last known id.
%% Paginating from newest messages to oldest ones.
pagination_last_after_id5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5 after 5-th message.
        RSM = #rsm_in{max=5, direction='before',
                after_id=message_id(5, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last_after_id5">>, RSM)),
     %% wait_message_range(Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(Alice,          10,      5,    11,  15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

%% Select second page of recent messages after last known id.
pagination_last_after_id5_before_id11(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        RSM = #rsm_in{max=5, direction='before',
                after_id=message_id(5, Config),
                before_id=message_id(11, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last_after_id5_before_id11">>, RSM)),
     %% wait_message_range(Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(Alice,           5,      0,     6,  10),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_first_page_after_id4(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        % Default direction is after
        RSM = #rsm_in{max = 5, after_id = message_id(4, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"first_page_after_id4">>, RSM)),
        %% Gets 5, 6, 7, 8, 9
        %% Total Count is 11: i.e. 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
        %% Messages 1, 2, 3, 4 are ignored in the result
     %% wait_message_range(Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(Alice,          11,      0,     5,  9),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_last_page_after_id4(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        RSM = #rsm_in{max = 5, after_id = message_id(4, Config), direction=before},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last_page_after_id4">>, RSM)),
        %% Gets 11, 12, 13, 14, 15
        %% Total Count is 11: i.e. 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
        %% Messages 1, 2, 3, 4 are ignored in the result
     %% wait_message_range(Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(Alice,          11,      6,     11,  15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_border_flipped_page(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        RSM = #rsm_in{max=5, direction='before',
                after_id=message_id(5, Config),
                before_id=message_id(11, Config)},
        rsm_send(Config, Alice,
            stanza_flip_page_archive_request(P, <<"border_flipped_page">>, RSM)),
     %% wait_message_range(Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(Alice,           5,      0,     10,  6),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

server_returns_item_not_found_for_before_filter_with_nonexistent_id(Config) ->
    NonexistentID = <<"AV25E9SCO50K">>,
    RSM = #rsm_in{max = 5, direction = 'before', id = NonexistentID},
    StanzaID = <<"before-nonexistent-id">>,
    Condition = [<<"cancel">>, <<"item-not-found">>],
    server_returns_item_not_found_for_nonexistent_id(Config, RSM, StanzaID, Condition).

server_returns_item_not_found_for_after_filter_with_nonexistent_id(Config) ->
    NonexistentID = <<"AV25E9SCO50K">>,
    RSM = #rsm_in{max = 5, direction = 'after', id = NonexistentID},
    StanzaID = <<"after-nonexistent-id">>,
    Condition = [<<"cancel">>, <<"item-not-found">>],
    server_returns_item_not_found_for_nonexistent_id(Config, RSM, StanzaID, Condition).

server_returns_item_not_found_for_after_filter_with_invalid_id(Config) ->
    NonexistentID = <<"bef3a242-99ce-402a-9ffc-2f3c20da92d4">>,
    RSM = #rsm_in{max = 5, direction = 'after', from_id = NonexistentID},
    StanzaID = <<"AV25E9SCO50K">>,
    Condition = [<<"modify">>, <<"not-acceptable">>],
    server_returns_item_not_found_for_nonexistent_id(Config, RSM, StanzaID, Condition).

server_returns_item_not_found_for_nonexistent_id(Config, RSM, StanzaID, Condition) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        IQ = stanza_page_archive_request(P, StanzaID, RSM),
        rsm_send(Config, Alice, IQ),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [IQ], Res),
        escalus:assert(is_error, Condition, Res),
        assert_dropped_iq_event(Config, escalus_utils:get_jid(Alice)),
        verify_id_error_text_msg(Condition, Res),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).


%% Test cases for "complete" attribute
%% Complete attribute is used for pagination, telling when to stop paginating.
%% see complete_flag_cases with the whole list of the cases.
%% -----------------------------------------------

%% Get last page with most recent messages
%% rsm_id.id is undefined
%% GIVEN 15 archived messages
%% WHEN direction=before, page_size=5
%% THEN complete=false
before_complete_false_last5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        %% Get messages: 11,12,13,14,15
        RSM = #rsm_in{max=5, direction=before},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last5">>, RSM)),
               wait_for_complete_archive_response(P, Alice, <<"false">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% Gets some page in the midle of the result set
%% GIVEN 15 archived messages
%% WHEN direction=before, rsm_id=10, page_size=5
%% THEN complete=false
before_complete_false_before10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get messages: 5,6,7,8,9
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"before10">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"false">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% Reaches the end of result set
%% No messages are returned.
%% GIVEN 15 archived messages
%% WHEN direction=before, rsm_id=1, page_size=5
%% THEN complete=true
before_complete_true_before1(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get no messages
        RSM = #rsm_in{max=5, direction=before, id=message_id(1, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"before1">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"true">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% Reaches the end of result set
%% Less than maximum number of messages are returned.
%% GIVEN 15 archived messages
%% WHEN direction=before, rsm_id=5, page_size=5
%% THEN complete=true
before_complete_true_before5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get messages: 1,2,3,4
        RSM = #rsm_in{max=5, direction=before, id=message_id(5, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"before5">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"true">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% Reaches the end of result set
%% A special case when exactly maximum number of messages are returned.
%% GIVEN 15 archived messages
%% WHEN direction=before, rsm_id=6, page_size=5
%% THEN complete=true
before_complete_true_before6(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get messages: 1,2,3,4,5
        RSM = #rsm_in{max=5, direction=before, id=message_id(6, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"before6">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"true">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% First page is not complete, because max is smaller than archive size.
%% rsm_id.id is undefined
%% GIVEN 15 archived messages
%% WHEN direction=after, rsm_id=6, page_size=5
%% THEN complete=false
after_complete_false_first_page(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get messages: 1,2,3,4,5
        RSM = #rsm_in{max=5, direction='after'},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"firstpage">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"false">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% There are still 8-15 messages to paginate after this request.
%% GIVEN 15 archived messages
%% WHEN direction=after, rsm_id=2, page_size=5
%% THEN complete=false
after_complete_false_after2(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get messages: 3,4,5,6,7
        RSM = #rsm_in{max=5, direction='after', id=message_id(2, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"after2">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"false">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% There is still one message to paginate after this request.
%% GIVEN 15 archived messages
%% WHEN direction=after, rsm_id=9, page_size=5
%% THEN complete=false
after_complete_false_after9(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get messages: 10,11,12,13,14
        RSM = #rsm_in{max=5, direction='after', id=message_id(9, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"after9">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"false">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% There are no messages to paginate after this request.
%% Special case, when exactly page_size messages are returned.
%% GIVEN 15 archived messages
%% WHEN direction=after, rsm_id=10, page_size=5
%% THEN complete=true
after_complete_true_after10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        %% Get messages: 11,12,13,14,15
        RSM = #rsm_in{max=5, direction='after', id=message_id(10, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"after10">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"true">>)
        end,
    parallel_story(Config, [{alice, 1}], F).

%% There are no messages to paginate after this request.
%% Less than page_size are returned.
%% GIVEN 15 archived messages
%% WHEN direction=after, rsm_id=10, page_size=5
%% THEN complete=true
after_complete_true_after11(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        %% Get messages: 12,13,14,15
        RSM = #rsm_in{max=5, direction='after', id=message_id(11, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"after11">>, RSM)),
        wait_for_complete_archive_response(P, Alice, <<"true">>)
        end,
    parallel_story(Config, [{alice, 1}], F).


%% Test cases for preferences IQs
%% ------------------------------------------------------------------

prefs_set_request(Config) ->
    F = fun(Alice) ->
        %% Send
        %%
        %% <iq type='set' id='juliet2'>
        %%   <prefs xmlns='urn:xmpp:mam:tmp' default="roster">
        %%     <always>
        %%       <jid>romeo@montague.net</jid>
        %%     </always>
        %%     <never>
        %%       <jid>montague@montague.net</jid>
        %%     </never>
        %%   </prefs>
        %% </iq>
        escalus:send(Alice, stanza_prefs_set_request(<<"roster">>,
                                                     [<<"romeo@montague.net">>],
                                                     [<<"montague@montague.net">>],
                                                     mam_ns_binary())),
        ReplySet = escalus:wait_for_stanza(Alice),
        assert_event_with_jid(mod_mam_pm_set_prefs, escalus_utils:get_short_jid(Alice)),

        escalus:send(Alice, stanza_prefs_get_request(mam_ns_binary())),
        ReplyGet = escalus:wait_for_stanza(Alice),
        assert_event_with_jid(mod_mam_pm_get_prefs, escalus_utils:get_short_jid(Alice)),

        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assert_equal(ResultIQ1, ResultIQ2),
        ok
        end,
    escalus:fresh_story(Config, [{alice, 1}], F).

prefs_disabled_set_request(Config) ->
    F = fun(Alice) ->
        escalus:send(Alice, stanza_prefs_set_request(<<"roster">>,
                                                     [<<"romeo@montague.net">>],
                                                     [<<"montague@montague.net">>],
                                                     mam_ns_binary())),
        escalus:assert(is_error, [<<"cancel">>, <<"feature-not-implemented">>],
                       escalus:wait_for_stanza(Alice)),
        assert_event_with_jid(mod_mam_pm_set_prefs, escalus_utils:get_short_jid(Alice))
    end,
    escalus:fresh_story(Config, [{alice, 1}], F).

query_get_request(Config) ->
    F = fun(Alice) ->
        QueryXmlns = mam_ns_binary_v04(),
        escalus:send(Alice, stanza_query_get_request(QueryXmlns)),
        ReplyFields = escalus:wait_for_stanza(Alice),
        ResponseXmlns = exml_query:path(ReplyFields,
            [{element, <<"query">>},
             {element, <<"x">>},
             {element, <<"field">>},
             {element, <<"value">>},
              cdata]),
        ?assert_equal(QueryXmlns, ResponseXmlns)
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

%% Test reproducing https://github.com/esl/MongooseIM/issues/263
%% The idea is this: in a "perfect" world jid elements are put together
%% without whitespaces. In the real world it is not true.
%% Put "\n" between two jid elements.
prefs_set_cdata_request(Config) ->
    F = fun(Alice) ->
        %% Send
        %%
        %% <iq type='set' id='juliet2'>
        %%   <prefs xmlns='urn:xmpp:mam:tmp' default="roster">
        %%     <always>
        %%       <jid>romeo@montague.net</jid>
        %%       <jid>montague@montague.net</jid>
        %%     </always>
        %%   </prefs>
        %% </iq>
        escalus:send(Alice, stanza_prefs_set_request(<<"roster">>,
                                                     [<<"romeo@montague.net">>,
                                                      #xmlcdata{content = <<"\n">>}, %% Put as it is
                                                      <<"montague@montague.net">>], [],
                                                     mam_ns_binary_v04())),
        ReplySet = escalus:wait_for_stanza(Alice),

        escalus:send(Alice, stanza_prefs_get_request(mam_ns_binary_v04())),
        ReplyGet = escalus:wait_for_stanza(Alice),

        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assert_equal(ResultIQ1, ResultIQ2),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

muc_prefs_set_request(ConfigIn) ->
    F = fun(Config, Alice, _Bob) ->
        %% Send
        %%
        %% <iq type='set' id='juliet2' to='room-alice'>
        %%   <prefs xmlns='urn:xmpp:mam:1' default='roster'>
        %%     <always>
        %%       <jid>romeo@montague.net</jid>
        %%     </always>
        %%     <never>
        %%       <jid>montague@montague.net</jid>
        %%     </never>
        %%   </prefs>
        %% </iq>

        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        escalus:send(Alice, stanza_to_room(stanza_prefs_set_request(<<"roster">>,
                                                                    [<<"romeo@montague.net">>],
                                                                    [<<"montague@montague.net">>],
                                                                    mam_ns_binary()), Room)),
        ReplySet = escalus:wait_for_stanza(Alice),
        assert_event_with_jid(mod_mam_muc_set_prefs, RoomAddr),

        escalus:send(Alice, stanza_to_room(stanza_prefs_get_request(mam_ns_binary()), Room)),
        ReplyGet = escalus:wait_for_stanza(Alice),
        assert_event_with_jid(mod_mam_muc_get_prefs, RoomAddr),

        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assert_equal(ResultIQ1, ResultIQ2),
        ok
        end,
    RoomOpts = [{persistent, true}],
    UserSpecs = [{alice, 1}, {bob, 1}],
    muc_helper:story_with_room(ConfigIn, RoomOpts, UserSpecs, F).

muc_prefs_set_request_not_an_owner(ConfigIn) ->
    F = fun(Config, _Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        escalus:send(Bob, stanza_to_room(stanza_prefs_set_request(<<"roster">>,
                                                                    [<<"romeo@montague.net">>],
                                                                    [<<"montague@montague.net">>],
                                                                    mam_ns_binary()), Room)),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], escalus:wait_for_stanza(Bob)),
        assert_no_event_with_jid(mod_mam_muc_set_prefs, RoomAddr)
    end,
    RoomOpts = [{persistent, true}],
    UserSpecs = [{alice, 1}, {bob, 1}],
    muc_helper:story_with_room(ConfigIn, RoomOpts, UserSpecs, F).

muc_query_get_request(ConfigIn) ->
    F = fun(Config, Alice) ->
        Room = ?config(room, Config),
        QueryXmlns = mam_ns_binary_v04(),
        escalus:send(Alice, stanza_to_room(stanza_query_get_request(QueryXmlns), Room)),
        ReplyFields = escalus:wait_for_stanza(Alice),
        ResponseXmlns = exml_query:path(ReplyFields,
            [{element, <<"query">>},
             {element, <<"x">>},
             {element, <<"field">>},
             {element, <<"value">>},
              cdata]),
        ?assert_equal(QueryXmlns, ResponseXmlns)
        end,
    RoomOpts = [{persistent, true}],
    UserSpecs = [{alice, 1}],
    muc_helper:story_with_room(ConfigIn, RoomOpts, UserSpecs, F).

%% Test reproducing https://github.com/esl/MongooseIM/issues/263
%% The idea is this: in a "perfect" world jid elements are put together
%% without whitespaces. In the real world it is not true.
%% Put "\n" between two jid elements.
muc_prefs_set_cdata_request(ConfigIn) ->
    F = fun(Config, Alice) ->
        %% Send
        %%
        %% <iq type='set' id='juliet2' to='room-alice'>
        %%   <prefs xmlns='urn:xmpp:mam:1' default='roster'>
        %%     <always>
        %%       <jid>romeo@montague.net</jid>
        %%       <jid>montague@montague.net</jid>
        %%     </always>
        %%     <never>
        %%       <jid>montague@montague.net</jid>
        %%     </never>
        %%   </prefs>
        %% </iq>

        Room = ?config(room, Config),
        escalus:send(Alice, stanza_to_room(stanza_prefs_set_request(<<"roster">>,
                                                                    [<<"romeo@montague.net">>,
                                                                     #xmlcdata{content = <<"\n">>}, %% Put as it is
                                                                     <<"montague@montague.net">>], [],
                                                                    mam_ns_binary_v04()), Room)),
        ReplySet = escalus:wait_for_stanza(Alice),

        escalus:send(Alice, stanza_to_room(stanza_prefs_get_request(mam_ns_binary_v04()), Room)),
        ReplyGet = escalus:wait_for_stanza(Alice),

        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assert_equal(ResultIQ1, ResultIQ2),
        ok
        end,
    RoomOpts = [{persistent, true}],
    UserSpecs = [{alice, 1}],
    muc_helper:story_with_room(ConfigIn, RoomOpts, UserSpecs, F).

mam_service_discovery(Config) ->
    F = fun(Alice) ->
        Server = escalus_client:server(Alice),
        discover_features(Config, Alice, Server)
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

mam_service_discovery_to_client_bare_jid(Config) ->
    F = fun(Alice) ->
        Address = inbox_helper:to_bare_lower(Alice),
        discover_features(Config, Alice, Address)
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

mam_service_discovery_to_different_client_bare_jid_results_in_error(Config) ->
    F = fun(Alice, Bob) ->
        Address = inbox_helper:to_bare_lower(Bob),
        escalus:send(Alice, escalus_stanza:disco_info(Address)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Stanza)
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

%% Check, that MUC is supported.
muc_service_discovery(Config) ->
    F = fun(Alice) ->
        Domain = domain(),
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [muc_host()], Stanza),
        escalus:assert(is_stanza_from, [Domain], Stanza),

        discover_features(Config, Alice, muc_host())
        end,
    escalus:fresh_story(Config, [{alice, 1}], F).

discover_features(Config, Client, Service) ->
    escalus:send(Client, escalus_stanza:disco_info(Service)),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_result, Stanza),
    escalus:assert(has_feature, [mam_ns_binary_v04()], Stanza),
    escalus:assert(has_feature, [mam_ns_binary_v06()], Stanza),
    escalus:assert(has_feature, [mam_ns_binary_extended()], Stanza),
    escalus:assert(has_feature, [retract_ns()], Stanza),
    check_include_groupchat_features(Stanza,
                                     ?config(configuration, Config),
                                     ?config(basic_group, Config)),
    ?assert_equal(message_retraction_is_enabled(Config),
                  escalus_pred:has_feature(retract_tombstone_ns(), Stanza)).

messages_filtered_when_prefs_default_policy_is_always(Config) ->
    run_prefs_cases(always, Config).

messages_filtered_when_prefs_default_policy_is_never(Config) ->
    run_prefs_cases(never, Config).

messages_filtered_when_prefs_default_policy_is_roster(Config) ->
    run_prefs_cases(roster, Config).

muc_messages_filtered_when_prefs_default_policy_is_always(Config) ->
    muc_run_prefs_cases(always, Config).

muc_messages_filtered_when_prefs_default_policy_is_never(Config) ->
    muc_run_prefs_cases(never, Config).

muc_messages_filtered_when_prefs_default_policy_is_roster(Config) ->
    muc_run_prefs_cases(roster, Config).


-spec enter_room(Config :: proplists:proplist(), [User :: term()]) ->
    {Room :: binary(), RoomAddr  :: binary()}.
enter_room(Config, Users) ->
    Room = ?config(room, Config),
    RoomAddr = room_address(Room),
    [escalus:send(User, stanza_muc_enter_room(Room, nick(User))) || User <- Users],
    {Room, RoomAddr}.

%% First write all messages, than read and check
run_prefs_cases(DefaultPolicy, ConfigIn) ->
    P = ?config(props, ConfigIn),
    F = fun(Config, Alice, Bob, Kate) ->
        make_alice_and_bob_friends(Alice, Bob),
        %% Just send messages for each prefs configuration
        Namespace = mam_ns_binary_v04(),
        Funs = [run_prefs_case(Case, Namespace, Alice, Bob, Kate, Config)
                || Case <- prefs_cases2(),
                default_policy(Case) =:= DefaultPolicy],

        maybe_wait_for_archive(Config),

        %% Get ALL messages using several queries if required
        Stanzas = get_all_messages(P, Alice),
        ParsedMessages = parse_messages(Stanzas),
        Bodies = [B || #forwarded_message{message_body=B} <- ParsedMessages],

        %% Check messages, print out all failed cases
        Fails = lists:append([Fun(Bodies) || Fun <- Funs]),
        %% If fails consult with ct:pal/2 why
        ?assert_equal([], Fails)
        end,
    escalus_fresh:story_with_config(ConfigIn, [{alice, 1}, {bob, 1}, {kate, 1}], F).

muc_run_prefs_cases(DefaultPolicy, ConfigIn) ->
    F = fun(Config, Alice, Bob, Kate) ->
        %% Just send messages for each prefs configuration
        Namespace = mam_ns_binary_v04(),

        Room = ?config(room, Config),
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),
        escalus:send(Kate, stanza_muc_enter_room(Room, nick(Kate))),
        escalus:wait_for_stanzas(Alice, 4),

        Funs = [muc_run_prefs_case(Case, Namespace, Alice, Bob, Kate, Config)
                || Case <- prefs_cases2_muc(),
                default_policy(Case) =:= DefaultPolicy],

        maybe_wait_for_archive(Config),

        %% Get ALL messages using several queries if required

        Room = ?config(room, Config),
        IQ = stanza_archive_request([{mam_ns, <<"urn:xmpp:mam:1">>}], <<>>),
        escalus:send(Alice, stanza_to_room(IQ, Room)),
        Data = wait_archive_respond(Alice),

        ParsedMessages = parse_messages(Data#mam_archive_respond.respond_messages),
        Bodies = [B || #forwarded_message{message_body=B} <- ParsedMessages],

        %% Check messages, print out all failed cases
        Fails = lists:append([Fun(Bodies) || Fun <- Funs]),
        %% If fails consult with ct:pal/2 why
        ?assert_equal([], Fails)
        end,
    RoomOpts = [{persistent, true}],
    UserSpecs = [{alice, 1}, {bob, 1}, {kate, 1}],
    muc_helper:story_with_room(ConfigIn, RoomOpts, UserSpecs, F).

%% The same as prefs_set_request case but for different configurations
run_set_and_get_prefs_cases(ConfigIn) ->
    F = fun(Config, Alice, _Bob, _Kate) ->
        Namespace = mam_ns_binary_v04(),
        [run_set_and_get_prefs_case(Case, Namespace, Alice, Config) || Case <- prefs_cases2()]
        end,
    escalus_fresh:story_with_config(ConfigIn, [{alice, 1}, {bob, 1}, {kate, 1}], F).

%% MAM's implementation specific test
check_user_exist(Config) ->
  %% when
  [{_, AdminSpec}] = escalus_users:get_users([admin]),
  [AdminU, AdminS, AdminP] = escalus_users:get_usp(Config, AdminSpec),
  JID = mongoose_helper:make_jid(AdminU, AdminS),
  ok = rpc(mim(), ejabberd_auth, try_register, [JID, AdminP]),
  %% admin user already registered
  {ok, HostType} = rpc(mim(), mongoose_domain_core, get_host_type, [AdminS]),
  true = rpc(mim(), ejabberd_auth, does_user_exist,
             [HostType, JID, stored]),
  false = rpc(mim(), ejabberd_auth, does_user_exist,
              [HostType, mongoose_helper:make_jid(<<"fake-user">>, AdminS), stored]),
  false = rpc(mim(), ejabberd_auth, does_user_exist,
              [HostType, mongoose_helper:make_jid(AdminU, <<"fake-domain">>), stored]),
  %% cleanup
  ok = rpc(mim(), ejabberd_auth, remove_user, [JID]).

reconnect_no_ack(Config) ->
    %% Connect Bob and Alice
    Bob = sm_helper:connect_fresh(Config, bob, presence),
    Alice = sm_helper:connect_fresh(Config, alice, sr_presence, manual),
    AliceJid = escalus_client:full_jid(Alice),
    BobJid = escalus_client:full_jid(Bob),
    sm_helper:ack_initial_presence(Alice),

    % 1. Bob sends a msg to Alice
    Body = <<"OH, HAI! Msg 1">>,
    escalus:send(Bob, escalus_stanza:chat_to(Alice, Body)),
    mam_helper:wait_for_archive_size(Alice, 1),

    % 2. Alice receives, and does not acknowledge
    % She may get the ack request before the message for some reason
    Resp = [_, _] = escalus_client:wait_for_stanzas(Alice, 2),
    escalus:assert_many([fun(Msg) -> escalus_pred:is_chat_message_from_to(BobJid, AliceJid, Body, Msg) end,
                         fun(SMRequest) -> escalus_pred:is_sm_ack_request(SMRequest) end],
                        Resp),

    % 3. Alice disconnects abruptly
    C2SPid = mongoose_helper:get_session_pid(Alice),
    escalus_connection:kill(Alice),
    sm_helper:wait_until_resume_session(C2SPid),
    sm_helper:assert_alive_resources(Alice, 1),

    % 4. Alice reconnects
    NewAlice = sm_helper:connect_same(Alice, session),

    % We have to send presence by hand, because the message may be received first
    sm_helper:send_initial_presence(NewAlice),
    % Current behaviour - unacked stanza is rerouted when a quick reconnection occurs
    % there is no delay element, or any indication of retransmission
    NewResp = [_, _] = escalus_client:wait_for_stanzas(NewAlice, 2),
    escalus:assert_many([fun(Msg) -> escalus_pred:is_chat_message_from_to(BobJid, AliceJid, Body, Msg) end,
                         fun(Presence) -> escalus_pred:is_presence(Presence) end],
                        NewResp),

    AliceUsername = escalus_client:username(NewAlice),
    AliceServer = escalus_client:server(NewAlice),

    % There is only one message in MAM, even though it was resent
    ?assertEqual(1, mam_helper:archive_size(AliceServer, AliceUsername)),

    escalus_connection:stop(Bob),
    escalus_connection:stop(Alice).

reconnect_ack(Config) ->
    % Connect Bob and Alice
    Bob = sm_helper:connect_fresh(Config, bob, presence),
    Alice = sm_helper:connect_fresh(Config, alice, sr_presence, manual),
    AliceJid = escalus_client:full_jid(Alice),
    BobJid = escalus_client:full_jid(Bob),
    sm_helper:ack_initial_presence(Alice),

    % 1. Bob sends a msg to Alice
    Body = <<"OH, HAI! Msg 1">>,
    escalus:send(Bob, escalus_stanza:chat_to(Alice, Body)),
    mam_helper:wait_for_archive_size(Alice, 1),

    % 2. Alice receives, and acknowledges
    Resp = [_, _] = escalus_client:wait_for_stanzas(Alice, 2),
    escalus:assert_many([fun(Msg) -> escalus_pred:is_chat_message_from_to(BobJid, AliceJid, Body, Msg) end,
                         fun(SMRequest) -> escalus_pred:is_sm_ack_request(SMRequest) end],
                        Resp),
    escalus_connection:send(Alice, escalus_stanza:sm_ack(2)),

    % 3. Alice disconnects abruptly
    C2SPid = mongoose_helper:get_session_pid(Alice),
    escalus_connection:kill(Alice),
    sm_helper:wait_until_resume_session(C2SPid),
    sm_helper:assert_alive_resources(Alice, 1),

    % 4. Alice reconnects
    NewAlice = sm_helper:connect_same(Alice, presence),

    % 5. Check no new messages received
    timer:sleep(timer:seconds(1)),
    escalus_assert:has_no_stanzas(NewAlice),

    % No new messages in MAM as well
    AliceUsername = escalus_client:username(NewAlice),
    AliceServer = escalus_client:server(NewAlice),
    ?assertEqual(1, mam_helper:archive_size(AliceServer, AliceUsername)),

    escalus_connection:stop(Bob),
    escalus_connection:stop(Alice).

reconnect_no_ack_different_resource(Config) ->
    %% Connect Bob and Alice
    Bob = sm_helper:connect_fresh(Config, bob, presence),
    Spec = escalus_fresh:create_fresh_user(Config, {alice, 2}),
    Alice = sm_helper:connect_spec(Spec, sr_presence, manual),
    AliceJid = escalus_client:full_jid(Alice),
    BobJid = escalus_client:full_jid(Bob),
    sm_helper:ack_initial_presence(Alice),

    % 1. Bob sends a msg to Alice
    Body = <<"OH, HAI! Msg 1">>,
    escalus:send(Bob, escalus_stanza:chat_to(Alice, Body)),
    mam_helper:wait_for_archive_size(Alice, 1),

    % 2. Alice receives, and does not acknowledge
    Resp = [_, _] = escalus_client:wait_for_stanzas(Alice, 2),
    escalus:assert_many([fun(Msg) -> escalus_pred:is_chat_message_from_to(BobJid, AliceJid, Body, Msg) end,
                         fun(SMRequest) -> escalus_pred:is_sm_ack_request(SMRequest) end],
                        Resp),

    % 3. Alice disconnects abruptly
    C2SPid = mongoose_helper:get_session_pid(Alice),
    escalus_connection:kill(Alice),
    sm_helper:wait_until_resume_session(C2SPid),
    sm_helper:assert_alive_resources(Alice, 1),

    % 4. Alice reconnects a different resource
    NewAlice = sm_helper:connect_spec([{resource, <<"mam_sm_test_2nd_resource">>} | Spec], presence, manual),

    % 2nd resource doesn't get the stanza, only the delayed presence.
    Presence = escalus:wait_for_stanza(NewAlice),
    escalus:assert(is_presence, Presence),

    % 5. Check no new messages received
    timer:sleep(timer:seconds(1)),
    escalus_assert:has_no_stanzas(NewAlice),

    % No new messages in MAM as well
    AliceUsername = escalus_client:username(NewAlice),
    AliceServer = escalus_client:server(NewAlice),
    ?assertEqual(1, mam_helper:archive_size(AliceServer, AliceUsername)),

    escalus_connection:stop(Bob),
    escalus_connection:stop(Alice).

%% This function supports only one device, one user.
%% We don't send initial presence to avoid presence broadcasts between resources
%% of the same user from different stories.
%% It is limited comparing to escalus story, but reduces CPU usage, because we don't
%% need to send any presences.
parallel_story(Config, [{_, 1}] = ResourceCounts, F) ->
    Config1 = override_for_parallel(Config),
    escalus:story(Config1, ResourceCounts, F).

override_for_parallel(Config) ->
    Overrides = [
        {start_ready_clients, start_ready_clients()}
        ],
    [{escalus_overrides, Overrides} | Config].

start_ready_clients() ->
    fun(Config, [{UserSpec, BaseResource}]) ->
        Suffix = list_to_binary(pid_to_list(self()) -- "<>"),
        Resource = <<BaseResource/binary, Suffix/binary>>,
        {ok, Client} = escalus_client:start(Config, UserSpec, Resource),
        [Client]
    end.

text_search_messages() ->
    [
     <<"Tongue chicken jowl hamburger duis exercitation.">>,
     <<"Ribs eu aliquip pork veniam dolor jowl id laborum in frankfurter culpa ribs.">>,
     <<"Fatback ut labore pariatur, eiusmod esse dolore turducken jowl exercitation ",
       "shankle shoulder.">>,
     <<"Kevin ribeye short ribs, nostrud short loin quis voluptate cow.  Do brisket eu ",
       "sunt tail ullamco cow in bacon burgdoggen.">>,
     <<"Occaecat in voluptate incididunt aliqua dolor bacon salami anim picanha pork ",
       "reprehenderit pancetta tail.">>,
     <<"Nisi shank doner dolore officia ribeye.  Proident shankle tenderloin consequat ",
       "bresaola quis tongue ut sirloin pork chop pariatur fatback ex cupidatat venison.">>,
     <<"Brisket in pastrami dolore cupidatat.  Est corned beef ad ribeye ball tip aliqua ",
       "cupidatat andouille cillum et consequat leberkas.">>,
     <<"Qui mollit short ribs, capicola bresaola pork meatloaf kielbasa und culpa.">>,
     <<"Meatloaf esse jowl do ham hock consequat.  Duis laboris ribeye ullamco, sed elit ",
       "porchetta sirloin.">>,
     <<"In boudin ad et salami exercitation sausage flank strip steak ball tip dolore ",
       "pig officia.">>,
     <<"Spare ribs landjaeger pork belly, chuck aliquip turducken beef culpa nostrud.">>
    ].

%% --------- MUC Light stories helpers ----------

when_pm_message_is_sent(Sender, Receiver, Body) ->
    escalus:send(Sender, escalus_stanza:chat_to(Receiver, Body)).

then_pm_message_is_received(Receiver, Body) ->
    escalus:assert(is_chat_message, [Body], escalus:wait_for_stanza(Receiver)).

%% Message retraction helpers

check_archive_after_retraction(Config, Client, ApplyToElement, Body) ->
    case message_should_be_retracted(Config) of
        true -> expect_tombstone_and_retraction_message(Client, ApplyToElement);
        false -> expect_original_and_retraction_message(Client, ApplyToElement, Body);
        ignore -> expect_only_original_message(Client, Body)
    end.

message_should_be_retracted(Config) ->
    message_retraction_is_enabled(Config) andalso retraction_requested(Config).

retraction_requested(Config) ->
    OriginId = origin_id(),
    case lists:keyfind(retract_on, 1, Config) of
        {retract_on, none} -> ignore;
        {retract_on, stanza_id} -> true;
        {retract_on, {origin_id, OriginId}} -> true;
        _ -> false
    end.

message_retraction_is_enabled(Config) ->
    BasicGroup = ?config(basic_group, Config),
    BasicGroup =/= disabled_retraction andalso BasicGroup =/= muc_disabled_retraction.

check_include_groupchat_features(Stanza, Config, _BasicGroup) when Config =:= cassandra_eterm;
                                                                   Config =:= cassandra ->
    ?assertNot(escalus_pred:has_feature(groupchat_field_ns(), Stanza)),
    ?assertNot(escalus_pred:has_feature(groupchat_available_ns(), Stanza));
check_include_groupchat_features(Stanza, _Configuration, muc_light) ->
    escalus:assert(has_feature, [groupchat_field_ns()], Stanza),
    escalus:assert(has_feature, [groupchat_available_ns()], Stanza);
check_include_groupchat_features(Stanza, _Configuration, muc_all) ->
    ?assertNot(escalus_pred:has_feature(groupchat_field_ns(), Stanza)),
    ?assertNot(escalus_pred:has_feature(groupchat_available_ns(), Stanza));
check_include_groupchat_features(Stanza, _Configuration, _BasicGroup) ->
    escalus:assert(has_feature, [groupchat_field_ns()], Stanza),
    ?assertNot(escalus_pred:has_feature(groupchat_available_ns(), Stanza)).

expect_tombstone_and_retraction_message(Client, ApplyToElement) ->
    [ArcMsg1, ArcMsg2] = respond_messages(assert_respond_size(2, wait_archive_respond(Client))),
    #forwarded_message{message_body = undefined,
                       message_children = [#xmlel{name = <<"retracted">>}]} = parse_forwarded_message(ArcMsg1),
    #forwarded_message{message_body = undefined,
                       message_children = [ApplyToElement]} = parse_forwarded_message(ArcMsg2).

expect_original_and_retraction_message(Client, ApplyToElement, Body) ->
    [ArcMsg1, ArcMsg2] = respond_messages(assert_respond_size(2, wait_archive_respond(Client))),
    #forwarded_message{message_body = Body} = parse_forwarded_message(ArcMsg1),
    #forwarded_message{message_body = undefined,
                       message_children = [ApplyToElement]} = parse_forwarded_message(ArcMsg2).

expect_only_original_message(Client, Body) ->
    [ArcMsg1] = respond_messages(assert_respond_size(1, wait_archive_respond(Client))),
    #forwarded_message{message_body = Body} = parse_forwarded_message(ArcMsg1).

retraction_message(Type, To, ApplyToElement) ->
    #xmlel{name = <<"message">>,
           attrs = #{<<"type">> => Type,
                     <<"to">> => To},
           children = [ApplyToElement]}.

origin_id_element(OriginId) ->
    #xmlel{name = <<"origin-id">>,
           attrs = #{<<"xmlns">> => <<"urn:xmpp:sid:0">>,
                     <<"id">> => OriginId}}.

apply_to_element(Config, Copy) ->
    {RetractOn, Id} = case ?config(retract_on, Config) of
                          {origin_id, OrigId} -> {origin_id, OrigId};
                          stanza_id -> {stanza_id, stanza_id_from_msg(Copy)};
                          none -> {origin_id, none}
                end,
    Attrs = #{<<"xmlns">> => <<"urn:xmpp:fasten:0">>},
    #xmlel{name = <<"apply-to">>,
           attrs = maybe_append_id(Id, Attrs),
           children = [retract_element(RetractOn)]
          }.

maybe_append_id(none, Attrs) ->
    Attrs;
maybe_append_id(Id, Attrs) ->
    Attrs#{<<"id">> => Id}.

stanza_id_from_msg(Msg) ->
    case exml_query:path(Msg, [{element, <<"stanza-id">>}, {attr, <<"id">>}]) of
        undefined -> exml_query:path(Msg, [{element, <<"result">>}, {attr, <<"id">>}]);
        Id -> Id
    end.

retract_element(origin_id) ->
    #xmlel{name = <<"retract">>,
           attrs = #{<<"xmlns">> => <<"urn:xmpp:message-retract:0">>}};
retract_element(stanza_id) ->
    #xmlel{name = <<"retract">>,
           attrs = #{<<"xmlns">> => <<"urn:esl:message-retract-by-stanza-id:0">>}}.

origin_id() ->
    <<"orig-id-1">>.

simple_range(From, To, IsComplete) ->
    #{total_count => undefined, offset => undefined,
      from => From, to => To, is_complete => IsComplete, step => 1}.

pagination_test(Name, RSM, Range, Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        rsm_send(Config, Alice, stanza_page_archive_request(P, atom_to_binary(Name), RSM)),
        wait_message_range(Alice, Range)
        end,
    parallel_story(Config, [{alice, 1}], F).

assert_failed_to_decode_message(ArcMsg) ->
    Forwarded = parse_forwarded_message(ArcMsg),
    Err = <<"Failed to decode message in database">>,
    ?assertMatch(#forwarded_message{message_body = Err}, Forwarded),
    ?assertMatch(#forwarded_message{message_type = <<"error">>}, Forwarded),
    #forwarded_message{message_children = [Msg]} = Forwarded,
    ?assertMatch(#xmlel{
        name = <<"error">>,
        attrs = #{<<"code">> := <<"500">>, <<"type">> := <<"wait">>},
        children = [#xmlel{name = <<"internal-server-error">>},
                    #xmlel{name = <<"text">>, children = [#xmlcdata{content = Err}]}]}, Msg).

mock_mongoose_mam_id(Node) ->
    ok = rpc(Node, meck, new, [mongoose_mam_id, [passthrough, no_link]]),
    ok = rpc(Node, meck, expect, [mongoose_mam_id, next_unique, 1,
      fun(X) ->
          P = persistent_term:get(mock_mongoose_mam_id, 0),
          N = max(X, P + 1),
          persistent_term:put(mock_mongoose_mam_id, N),
          N
      end]).

unmock_mongoose_mam_id(Node) ->
    ok = rpc(Node, meck, unload, [mongoose_mam_id]).
