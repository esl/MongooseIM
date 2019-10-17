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

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Tests
-export([no_elements/1,
         only_stanzaid/1,
         muc_no_elements/1,
         muc_only_stanzaid/1,
         mam_service_discovery/1,
         muc_service_discovery/1,
         simple_archive_request/1,
         text_search_query_fails_if_disabled/1,
         text_search_is_not_available/1,
         simple_text_search_request/1,
         long_text_search_request/1,
         text_search_is_available/1,
         muc_message_with_stanzaid/1,
         muc_text_search_request/1,
         muc_archive_request/1,
         muc_multiple_devices/1,
         muc_protected_message/1,
         muc_deny_protected_room_access/1,
         muc_allow_access_to_owner/1,
         muc_delete_x_user_in_anon_rooms/1,
         muc_show_x_user_to_moderators_in_anon_rooms/1,
         muc_show_x_user_for_your_own_messages_in_anon_rooms/1,
         range_archive_request/1,
         range_archive_request_not_empty/1,
         limit_archive_request/1,
         prefs_set_request/1,
         retrieve_form_fields/1,
         prefs_set_cdata_request/1,
         query_get_request/1,
         pagination_first5/1,
         pagination_last5/1,
         pagination_offset5/1,
         pagination_first0/1,
         pagination_last0/1,
         pagination_offset5_max0/1,
         pagination_before10/1,
         pagination_after10/1,
         pagination_simple_before10/1,
         pagination_last_after_id5/1,
         pagination_last_after_id5_before_id11/1,
         pagination_empty_rset/1,
         pagination_first5_opt_count/1,
         pagination_first25_opt_count_all/1,
         pagination_last5_opt_count/1,
         pagination_last25_opt_count_all/1,
         pagination_offset5_opt_count/1,
         pagination_offset5_opt_count_all/1,
         server_returns_item_not_found_for_before_filter_with_nonexistent_id/1,
         server_returns_item_not_found_for_after_filter_with_nonexistent_id/1,
         %% complete_flag_cases tests
         before_complete_false_last5/1,
         before_complete_false_before10/1,
         before_complete_true_before1/1,
         before_complete_true_before5/1,
         before_complete_true_before6/1,
         after_complete_false_first_page/1,
         after_complete_false_after2/1,
         after_complete_false_after9/1,
         after_complete_true_after10/1,
         after_complete_true_after11/1,
         archived/1,
         message_with_stanzaid/1,
         filter_forwarded/1,
         offline_message/1,
         nostore_hint/1,
         querying_for_all_messages_with_jid/1,
         muc_querying_for_all_messages/1,
         muc_querying_for_all_messages_with_jid/1,
         muc_light_simple/1,
         muc_light_shouldnt_modify_pm_archive/1,
         muc_light_stored_in_pm_if_allowed_to/1,
         muc_light_chat_markers_are_archived_if_enabled/1,
         muc_light_chat_markers_are_not_archived_if_disabled/1,
         messages_filtered_when_prefs_default_policy_is_always/1,
         messages_filtered_when_prefs_default_policy_is_never/1,
         messages_filtered_when_prefs_default_policy_is_roster/1,
         run_set_and_get_prefs_cases/1,
         check_user_exist/1,
         metric_incremented_on_archive_request/1,
         metric_incremented_when_store_message/1,
         archive_chat_markers/1,
         dont_archive_chat_markers/1,
         save_unicode_messages/1,
         unicode_messages_can_be_extracted/1]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-import(muc_helper,
        [muc_host/0,
         room_address/1, room_address/2,
         stanza_muc_enter_room/2,
         stanza_to_room/2]).

-import(mam_helper,
        [rpc_apply/3,
         get_prop/2,
         is_riak_enabled/1,
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
         has_x_user_element/1,
         stanza_date_range_archive_request/1,
         make_iso_time/1,
         stanza_retrieve_form_fields/2,
         stanza_limit_archive_request/1,
         rsm_send/3,
         stanza_page_archive_request/3,
         wait_empty_rset/2,
         wait_message_range/3,
         message_id/2,
         wait_message_range/5,
         stanza_prefs_set_request/4,
         stanza_prefs_get_request/1,
         stanza_query_get_request/1,
         parse_prefs_result_iq/1,
         mam_ns_binary/0,
         mam_ns_binary_v04/0,
         make_alice_and_bob_friends/2,
         run_prefs_case/6,
         prefs_cases2/0,
         default_policy/1,
         get_all_messages/2,
         parse_messages/1,
         run_set_and_get_prefs_case/4,
         muc_light_host/0,
         host/0
        ]).

-import(muc_light_helper,
        [given_muc_light_room/3,
         when_muc_light_message_is_sent/4,
         then_muc_light_message_is_received_by/2,
         when_muc_light_affiliations_are_set/3,
         then_muc_light_affiliations_are_received_by/2,
         when_archive_query_is_sent/3,
         then_archive_response_is/3]).

-include("mam_helper.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
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
    ++ rdbms_configs(true)
    ++ riak_configs(true)
    ++ elasticsearch_configs(true).

configurations_for_running_ct() ->
    cassandra_configs(is_cassandra_enabled(host()))
    ++ rdbms_configs(mongoose_helper:is_rdbms_enabled(host()))
    ++ riak_configs(is_riak_enabled(host()))
    ++ elasticsearch_configs(is_elasticsearch_enabled(host())).

rdbms_configs(true) ->
    [rdbms,
     rdbms_simple,
     rdbms_async_pool,
     rdbms_mnesia,
     rdbms_async_cache,
     rdbms_cache,
     rdbms_mnesia_cache,
     rdbms_mnesia_muc_cache
    ];
rdbms_configs(_) ->
    [].

riak_configs(true) ->
     [riak_timed_yz_buckets];
riak_configs(_) ->
     [].

cassandra_configs(true) ->
     [cassandra];
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
     impl_specific,
     disabled_text_search
    ].

all() ->
    Reasons =
    case ct_helper:is_ct_running() of
        true ->
            case is_mam_possible(host())  of
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
    Gs = [{full_group(C, G), Props, Tests}
          || C <- configurations(), {G, Props, Tests} <- basic_groups(),
             not is_skipped(C, G)],
    ct_helper:repeat_all_until_all_ok(Gs).

is_skipped(_, _) ->
    false.


basic_groups() ->
    [
     {mam_all, [parallel],
           [{mam_metrics, [], mam_metrics_cases()},
            {mam04, [parallel], mam_cases() ++ [retrieve_form_fields] ++ text_search_cases()},
            {mam06, [parallel], mam_cases() ++ stanzaid_cases()},
            {nostore, [parallel], nostore_cases()},
            {archived, [parallel], archived_cases()},
            {configurable_archiveid, [], configurable_archiveid_cases()},
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
     {chat_markers, [parallel], [archive_chat_markers,
                                 dont_archive_chat_markers]},
     {muc_all, [parallel],
           [{muc04, [parallel], muc_cases() ++ muc_text_search_cases()},
            {muc06, [parallel], muc_cases() ++ muc_stanzaid_cases()},
            {muc_configurable_archiveid, [], muc_configurable_archiveid_cases()},
            {muc_rsm_all, [parallel],
             [{muc_rsm04, [parallel], muc_rsm_cases()}]}]},
     {muc_light,        [], muc_light_cases()},
     {prefs_cases,      [parallel], prefs_cases()},
     {impl_specific,    [], impl_specific()},
     {disabled_text_search, [],
         [
          {mam04, [], disabled_text_search_cases()}
         ]}
    ].


mam_metrics_cases() ->
    [metric_incremented_on_archive_request,
     metric_incremented_when_store_message].

mam_cases() ->
    [mam_service_discovery,
     simple_archive_request,
     range_archive_request,
     range_archive_request_not_empty,
     limit_archive_request,
     querying_for_all_messages_with_jid,
     unicode_messages_can_be_extracted
    ].

text_search_cases() ->
    [
     simple_text_search_request,
     long_text_search_request,
     text_search_is_available,
     save_unicode_messages
    ].

disabled_text_search_cases() ->
    [
     text_search_is_not_available,
     text_search_query_fails_if_disabled
    ].

muc_text_search_cases() ->
    [
     muc_text_search_request
    ].

archived_cases() ->
    [archived,
     filter_forwarded].

stanzaid_cases() ->
    [message_with_stanzaid].

nostore_cases() ->
    [offline_message,
     nostore_hint].

muc_cases() ->
    [muc_service_discovery,
     muc_archive_request,
     muc_multiple_devices,
     muc_protected_message,
     muc_deny_protected_room_access,
     muc_allow_access_to_owner,
     muc_delete_x_user_in_anon_rooms,
     muc_show_x_user_to_moderators_in_anon_rooms,
     muc_show_x_user_for_your_own_messages_in_anon_rooms,
     muc_querying_for_all_messages,
     muc_querying_for_all_messages_with_jid
     ].

muc_stanzaid_cases() ->
    [muc_message_with_stanzaid].

muc_configurable_archiveid_cases() ->
    [
     muc_no_elements,
     muc_only_stanzaid
    ].

configurable_archiveid_cases() ->
    [no_elements,
     only_stanzaid
    ].

muc_light_cases() ->
    [
     muc_light_simple,
     muc_light_shouldnt_modify_pm_archive,
     muc_light_stored_in_pm_if_allowed_to,
     muc_light_chat_markers_are_archived_if_enabled,
     muc_light_chat_markers_are_not_archived_if_disabled
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
       %% Border cases
       pagination_last_after_id5,
       pagination_last_after_id5_before_id11,
       %% Simple cases
       pagination_simple_before10,
       %% opt_count cases
       pagination_first5_opt_count,
       pagination_last5_opt_count,
       pagination_offset5_opt_count,
       %% opt_count cases with all messages on the page
       pagination_first25_opt_count_all,
       pagination_last25_opt_count_all,
       pagination_offset5_opt_count_all,
       %% item_not_found response for nonexistent message ID in before/after filters
       server_returns_item_not_found_for_before_filter_with_nonexistent_id,
       server_returns_item_not_found_for_after_filter_with_nonexistent_id].

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

impl_specific() ->
  [check_user_exist].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config) ->
    muc_helper:load_muc(muc_host()),
    disable_sessions_limit(disable_shaping(
      delete_users([{escalus_user_db, {module, escalus_ejabberd}}
                  | escalus:init_per_suite(Config)]))).

end_per_suite(Config) ->
    muc_helper:unload_muc(),
    %% Next function creates a lot of sessions...
    escalus_fresh:clean(),
    %% and this function kicks them without waiting...
    mongoose_helper:kick_everyone(),
    %% so we don't have sessions anymore and other tests will not fail
    escalus:end_per_suite(restore_sessions_limit(restore_shaping(Config))).

user_names() ->
    [alice, bob, kate, carol].

create_users(Config) ->
    escalus:create_users(Config, escalus:get_users(user_names())).

delete_users(Config) ->
    escalus:delete_users(Config, escalus:get_users(user_names())).

disable_shaping(Config) ->
    OldShaper = get_shaper(),
    set_shaper({{maxrate, 10000}, {maxrate, 10000000}, {maxrate, 10000000}}),
    [{old_mam_shaper, OldShaper}|Config].

restore_shaping(Config) ->
    OldShaper = proplists:get_value(old_mam_shaper, Config),
    set_shaper(OldShaper),
    Config.

get_shaper() ->
    Mam = rpc_apply(ejabberd_config, get_global_option, [{shaper, mam_shaper, global}]),
    Norm = rpc_apply(ejabberd_config, get_global_option, [{shaper, normal, global}]),
    Fast = rpc_apply(ejabberd_config, get_global_option, [{shaper, fast, global}]),
    {Mam, Norm, Fast}.

set_shaper({Mam, Norm, Fast}) ->
    rpc_apply(ejabberd_config, add_global_option, [{shaper, mam_shaper, global}, Mam]),
    rpc_apply(ejabberd_config, add_global_option, [{shaper, normal, global}, Norm]),
    rpc_apply(ejabberd_config, add_global_option, [{shaper, fast, global}, Fast]),
    rpc_apply(shaper_srv, reset_all_shapers, [host()]).

disable_sessions_limit(Config) ->
    OldLimit = get_sessions_limit(),
    set_sessions_limit([{10000, all}]),
    [{old_sessions_limit, OldLimit}|Config].

restore_sessions_limit(Config) ->
    OldLimit = proplists:get_value(old_sessions_limit, Config),
    set_sessions_limit(OldLimit),
    Config.

get_sessions_limit() ->
    rpc_apply(ejabberd_config, get_global_option, [{access, max_user_sessions, global}]).

set_sessions_limit(NewLimit) ->
    rpc_apply(ejabberd_config, add_global_option,
              [{access, max_user_sessions, global}, NewLimit]).

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
init_per_group(archived, Config) ->
    Config;
init_per_group(mam_metrics, Config) ->
    Config;
init_per_group(muc04, Config) ->
    [{props, mam04_props()}, {with_rsm, true}|Config];
init_per_group(muc06, Config) ->
    [{props, mam06_props()}, {with_rsm, true}|Config];

init_per_group(muc_configurable_archiveid, Config) ->
    [backup_module_opts(mod_mam_muc) | Config];
init_per_group(configurable_archiveid, Config) ->
    [backup_module_opts(mod_mam) | Config];

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
   case init_modules(C, B, ConfigIn) of
        skip ->
            {skip, print_configuration_not_supported(C, B)};
        Config0 ->
            ct:pal("Init per group ~p; configuration ~p; basic group ~p",
                   [Group, C, B]),
            Config1 = do_init_per_group(C, Config0),
            [{basic_group, B}, {configuration, C} | init_state(C, B, Config1)]
    end.

backup_module_opts(Module) ->
    {{params_backup, Module}, rpc_apply(gen_mod, get_module_opts, [host(), mod_mam_muc])}.

restore_module_opts(Module, Config) ->
    ParamsB = proplists:get_value({params_backup, Module}, Config),
    rpc_apply(gen_mod, set_module_opts, [host(), Module, ParamsB]).

do_init_per_group(C, ConfigIn) ->
    Config0 = create_users(ConfigIn),
    case C of
        riak_timed_yz_buckets ->
            [{archive_wait, 2500} | Config0];
        cassandra ->
            [{archive_wait, 1500} | Config0];
        elasticsearch ->
            [{archive_wait, 2500} | Config0];
        _ ->
            [{archive_wait, 200} | Config0]
    end.

end_per_group(G, Config) when G == rsm_all; G == nostore;
    G == mam04; G == rsm04; G == with_rsm04; G == muc04; G == muc_rsm04; G == rsm04_comp;
    G == muc06; G == mam06;
    G == archived; G == mam_metrics ->
      Config;
end_per_group(muc_configurable_archiveid, Config) ->
    restore_module_opts(mod_mam_muc, Config),
    Config;
end_per_group(configurable_archiveid, Config) ->
    restore_module_opts(mod_mam, Config),
    Config;
end_per_group(muc_rsm_all, Config) ->
    destroy_room(Config);
end_per_group(Group, Config) ->
    C = configuration(Group),
    B = basic_group(Group),
    Config1 = end_state(C, B, Config),
    Config2 = end_modules(C, B, Config1),
    escalus_fresh:clean(),
    delete_users(Config2).

init_modules(rdbms, muc_light, Config) ->
    Config1 = init_modules_for_muc_light(rdbms, Config),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_muc_rdbms_arch, []),
    Config1;
init_modules(BT = riak_timed_yz_buckets, muc_light, Config) ->
    dynamic_modules:start(host(), mod_muc_light, [{host, binary_to_list(muc_light_host())}]),
    init_modules(BT, generic, [{muc_domain, "muclight.@HOST@"} | Config]);
init_modules(BT = cassandra, muc_light, config) ->
    init_modules_for_muc_light(BT, config);
init_modules(cassandra, muc_all, Config) ->
    init_module(host(), mod_mam_muc_cassandra_arch, []),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(BT = elasticsearch, muc_light, config) ->
    init_modules_for_muc_light(BT, config);
init_modules(elasticsearch, muc_all, Config) ->
    init_module(host(), mod_mam_muc_elasticsearch_arch, []),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, []),
    init_module(host(), mod_mam_rdbms_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms_simple, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, []),
    init_module(host(), mod_mam_rdbms_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms_async_pool, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, [no_writer]),
    init_module(host(), mod_mam_muc_rdbms_async_pool_writer, [{flush_interval, 1}]), %% 1ms
    init_module(host(), mod_mam_rdbms_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms_mnesia, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms_cache, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, []),
    init_module(host(), mod_mam_rdbms_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms_async_cache, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, [no_writer]),
    init_module(host(), mod_mam_muc_rdbms_async_pool_writer, [{flush_interval, 1}]), %% 1ms
    init_module(host(), mod_mam_rdbms_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms_mnesia_muc_cache, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_muc_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(rdbms_mnesia_muc_cache, _, _Config) ->
    skip;
init_modules(rdbms_mnesia_cache, muc_all, Config) ->
    init_module(host(), mod_mam_muc_rdbms_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [muc]),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    init_module(host(), mod_mam_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, muc_domain(Config)}]),
    Config;
init_modules(BackendType, muc_light, Config) ->
    Config1 = init_modules_for_muc_light(BackendType, Config),
    init_module(host(), mod_mam_rdbms_user, [muc, pm]),
    Config1;
init_modules(rdbms, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, []),
    init_module(host(), mod_mam_rdbms_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    Config;
init_modules(rdbms_simple, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, [rdbms_simple_opts()]),
    init_module(host(), mod_mam_rdbms_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    Config;
init_modules(riak_timed_yz_buckets, C, Config) ->
    init_module(host(), mod_mam_riak_timed_arch_yz, [pm, muc]),
    init_module(host(), mod_mam_mnesia_prefs, [pm, muc,
                                               {archive_key, mam_archive_key_server_user}]),
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_muc,
                [{host, muc_domain(Config)}] ++ addin_mam_options(C, Config)),
    Config;
init_modules(cassandra, C, Config) ->
    init_module(host(), mod_mam_cassandra_arch, [pm]),
    init_module(host(), mod_mam_cassandra_prefs, [pm]),
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    Config;
init_modules(elasticsearch, C, Config) ->
    init_module(host(), mod_mam_elasticsearch_arch, [pm]),
    init_module(host(), mod_mam_mnesia_prefs, [pm]),
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    Config;
init_modules(rdbms_async, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, [no_writer]),
    init_module(host(), mod_mam_rdbms_async_writer, [pm, {flush_interval, 1}]), % 1ms
    init_module(host(), mod_mam_rdbms_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    Config;
init_modules(rdbms_async_pool, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, [no_writer]),
    init_module(host(), mod_mam_rdbms_async_pool_writer, [pm, {flush_interval, 1}]), %% 1ms
    init_module(host(), mod_mam_rdbms_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    Config;
init_modules(rdbms_mnesia, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    Config;
init_modules(rdbms_cache, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, []),
    init_module(host(), mod_mam_rdbms_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    init_module(host(), mod_mam_cache_user, [pm]),
    Config;
init_modules(rdbms_async_cache, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, [no_writer]),
    init_module(host(), mod_mam_rdbms_async_pool_writer, [pm, {flush_interval, 1}]), %% 1ms
    init_module(host(), mod_mam_rdbms_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    init_module(host(), mod_mam_cache_user, [pm]),
    Config;
init_modules(rdbms_mnesia_cache, C, Config) ->
    init_module(host(), mod_mam, addin_mam_options(C, Config)),
    init_module(host(), mod_mam_rdbms_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [pm]),
    init_module(host(), mod_mam_rdbms_user, [pm]),
    init_module(host(), mod_mam_cache_user, [pm]),
    Config.

rdbms_simple_opts() ->
    [{db_jid_format, mam_jid_rfc}, {db_message_format, mam_message_xml}].

init_modules_for_muc_light(BackendType, Config) ->
    dynamic_modules:start(host(), mod_muc_light, [{host, binary_to_list(muc_light_host())}]),
    Config1 = init_modules(BackendType, muc_all, [{muc_domain, "muclight.@HOST@"} | Config]),
    init_modules(BackendType, pm, [{archive_groupchats, false} | Config1]).

end_modules(C, muc_light, Config) ->
    end_modules(C, generic, Config),
    dynamic_modules:stop(host(), mod_muc_light),
    Config;
end_modules(_, _, Config) ->
    [stop_module(host(), M) || M <- mam_modules()],
    Config.

muc_domain(Config) ->
    proplists:get_value(muc_domain, Config, "muc.@HOST@").

addin_mam_options(disabled_text_search, Config) ->
    [{full_text_search, false} | addin_mam_options(Config)];
addin_mam_options(chat_markers, Config) ->
    [{archive_chat_markers, true} | addin_mam_options(Config)];
addin_mam_options(_BasicGroup, Config) ->
    addin_mam_options(Config).

addin_mam_options(Config) ->
    [{archive_groupchats, proplists:get_value(archive_groupchats, Config, false)}].

mam_modules() ->
    [mod_mam,
     mod_mam_muc,
     mod_mam_cassandra_arch,
     mod_mam_muc_cassandra_arch,
     mod_mam_cassandra_prefs,
     mod_mam_elasticsearch_arch,
     mod_mam_muc_elasticsearch_arch,
     mod_mam_rdbms_arch,
     mod_mam_muc_rdbms_arch,
     mod_mam_rdbms_async_pool_writer,
     mod_mam_muc_rdbms_async_pool_writer,
     mod_mam_rdbms_prefs,
     mod_mam_mnesia_prefs,
     mod_mam_rdbms_user,
     mod_mam_cache_user,
     mod_mam_muc_cache_user,
     mod_mam_riak_timed_arch_yz].

init_state(_, muc_all, Config) ->
    Config;
init_state(C, muc_light, Config) ->
    clean_archives(Config),
    init_state(C, muc04, Config);
init_state(_C, prefs_cases, Config) ->
    Config;
init_state(_, _, Config) ->
    clean_archives(Config).

end_state(C, muc_light, Config) ->
    muc_light_helper:clear_db(),
    end_state(C, generic, Config);
end_state(_, _, Config) ->
    Config.

init_per_testcase(C=metric_incremented_when_store_message, ConfigIn) ->
    Config = case ?config(configuration, ConfigIn) of
                 rdbms_async_pool ->
                     MongooseMetrics = [
                                        {[global, data, rdbms, default],
                                         [{recv_oct, '>'}, {send_oct, '>'}]}
                                       ],
                     [{mongoose_metrics, MongooseMetrics} | ConfigIn];
                 _ ->
                     ConfigIn
             end,
    escalus:init_per_testcase(C, clean_archives(Config));
init_per_testcase(C=filter_forwarded, Config) ->
    escalus:init_per_testcase(C, Config);
init_per_testcase(C=querying_for_all_messages_with_jid, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}, {carol, 1}]),
    escalus:init_per_testcase(C, bootstrap_archive(Config1));
init_per_testcase(C=archived, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(C=offline_message, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}, {carol, 1}]),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(C=nostore_hint, Config) ->
    escalus:init_per_testcase(C, Config);
init_per_testcase(C=muc_querying_for_all_messages, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C,
        muc_bootstrap_archive(start_alice_room(Config1)));
init_per_testcase(C=muc_querying_for_all_messages_with_jid, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C,
        muc_bootstrap_archive(start_alice_room(Config1)));
init_per_testcase(C=muc_archive_request, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=muc_no_elements, Config) ->
    rpc_apply(gen_mod, set_module_opts, [host(), mod_mam_muc, [no_stanzaid_element]]),
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=muc_only_stanzaid, Config) ->
    rpc_apply(gen_mod, set_module_opts, [host(), mod_mam_muc, []]),
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=no_elements, Config) ->
    rpc_apply(gen_mod, set_module_opts, [host(), mod_mam, [no_stanzaid_element]]),
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=only_stanzaid, Config) ->
    rpc_apply(gen_mod, set_module_opts, [host(), mod_mam, []]),
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=muc_message_with_stanzaid, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=muc_multiple_devices, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=muc_protected_message, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_room(Config1));
init_per_testcase(C=muc_deny_protected_room_access, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_protected_room(Config1));
init_per_testcase(C=muc_allow_access_to_owner, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_protected_room(Config1));
init_per_testcase(C=muc_delete_x_user_in_anon_rooms, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_anonymous_room(Config1));
init_per_testcase(C=muc_show_x_user_to_moderators_in_anon_rooms, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_anonymous_room(Config1));
init_per_testcase(C=muc_show_x_user_for_your_own_messages_in_anon_rooms, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, start_alice_anonymous_room(Config1));
init_per_testcase(C=range_archive_request_not_empty, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}, {carol, 1}]),
    escalus:init_per_testcase(C, bootstrap_archive(Config1));
init_per_testcase(C=limit_archive_request, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}, {carol, 1}]),
    escalus:init_per_testcase(C, bootstrap_archive(Config1));
init_per_testcase(C=prefs_set_request, Config) ->
    skip_if_riak(C, Config);
init_per_testcase(C=prefs_set_cdata_request, Config) ->
    skip_if_riak(C, Config);
init_per_testcase(C=simple_text_search_request, Config) ->
    skip_if_cassandra(Config, fun() -> escalus:init_per_testcase(C, Config) end);
init_per_testcase(C=long_text_search_request, Config) ->
    skip_if_cassandra(Config, fun() -> escalus:init_per_testcase(C, Config) end);
init_per_testcase(C=save_unicode_messages, Config) ->
    skip_if_cassandra(Config, fun() -> escalus:init_per_testcase(C, Config) end);
init_per_testcase(C=muc_text_search_request, Config) ->
    Init =
        fun() ->
            Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
            escalus:init_per_testcase(C, start_alice_room(Config1))
        end,

    skip_if_cassandra(Config, Init);
init_per_testcase(C = muc_light_stored_in_pm_if_allowed_to, Config) ->
    OrigVal = rpc(mim(), gen_mod, get_module_opt, [host(), mod_mam, archive_groupchats, false]),
    true = rpc(mim(), gen_mod, set_module_opt, [host(), mod_mam, archive_groupchats, true]),
    clean_archives(Config),
    escalus:init_per_testcase(C, [{archive_groupchats_backup, OrigVal} | Config]);
init_per_testcase(C = muc_light_chat_markers_are_archived_if_enabled, ConfigIn) ->
    Config1 = [backup_module_opts(mod_mam_muc) | ConfigIn],
    rpc_apply(gen_mod, set_module_opt, [host(), mod_mam_muc, archive_chat_markers, true]),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(C = muc_light_chat_markers_are_not_archived_if_disabled, ConfigIn) ->
    Config1 = [backup_module_opts(mod_mam_muc) | ConfigIn],
    rpc_apply(gen_mod, set_module_opt, [host(), mod_mam_muc, archive_chat_markers, false]),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(C=archive_chat_markers, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(C=dont_archive_chat_markers, Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

skip_if_riak(C, Config) ->
    case ?config(configuration, Config) of
        riak_timed_yz_buckets ->
            {skip, "prefs not implemented for riak"};
        _ ->
            escalus:init_per_testcase(C, Config)
    end.

skip_if_cassandra(Config, Init) ->
    case ?config(configuration, Config) of
        cassandra ->
            {skip, "full text search is not implemented for cassandra backend"};
        _ ->
            Init()
    end.

end_per_testcase(C=muc_text_search_request, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_archive_request, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_multiple_devices, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_protected_message, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_deny_protected_room_access, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_allow_access_to_owner, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_delete_x_user_in_anon_rooms, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_show_x_user_to_moderators_in_anon_rooms, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_show_x_user_for_your_own_messages_in_anon_rooms, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_querying_for_all_messages, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_querying_for_all_messages_with_jid, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_message_with_stanzaid, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_no_elements, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_only_stanzaid, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C = muc_light_stored_in_pm_if_allowed_to, Config0) ->
    {value, {_, OrigVal}, Config1} = lists:keytake(archive_groupchats_backup, 1, Config0),
    true = rpc(mim(), gen_mod, set_module_opt, [host(), mod_mam, archive_groupchats, OrigVal]),
    escalus:end_per_testcase(C, Config1);
end_per_testcase(C = muc_light_chat_markers_are_archived_if_enabled, Config) ->
    restore_module_opts(mod_mam_muc, Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C = muc_light_chat_markers_are_not_archived_if_disabled, Config) ->
    restore_module_opts(mod_mam_muc, Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

init_module(Host, Mod, Args) ->
    lists:member(Mod, mam_modules())
    orelse
    ct:fail("Unknown module ~p", [Mod]),
    stop_module(Host, Mod),
    {ok, _} = start_module(Host, Mod, Args).

is_loaded_module(Host, Mod) ->
    rpc_apply(gen_mod, is_loaded, [Host, Mod]).

start_module(Host, Mod, Args) ->
    rpc_apply(gen_mod, start_module, [Host, Mod, Args]).

stop_module(Host, Mod) ->
    case is_loaded_module(Host, Mod) of
        non_existing -> ok;
        false        -> ok;
        true         -> just_stop_module(Host, Mod)
    end.

just_stop_module(Host, Mod) ->
    {ok, _Opts} = rpc_apply(gen_mod, stop_module, [Host, Mod]).

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

%% Querying the archive for messages
simple_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Alice sends "OH, HAI!" to Bob
        %% {xmlel,<<"message">>,
        %%  [{<<"from">>,<<"alice@localhost/res1">>},
        %%   {<<"to">>,<<"bob@localhost/res1">>},
        %%   {<<"xml:lang">>,<<"en">>},
        %%   {<<"type">>,<<"chat">>}],
        %%   [{xmlel,<<"body">>,[],[{xmlcdata,<<"OH, HAI!">>}]}]}
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        mam_helper:wait_for_archive_size(Alice, 1),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Res = wait_archive_respond(Alice),
        assert_respond_size(1, Res),
        assert_respond_query_id(P, <<"q1">>, parse_result_iq(Res)),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

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

text_search_is_available(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Namespace = get_prop(mam_ns, P),
        escalus:send(Alice, stanza_retrieve_form_fields(<<"q">>, Namespace)),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_with_ns, [Namespace], Res),
        QueryEl = exml_query:subelement(Res, <<"query">>),
        XEl = exml_query:subelement(QueryEl, <<"x">>),
        escalus:assert(has_field_with_type, [<<"full-text-search">>, <<"text-single">>], XEl),
        ok
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

simple_text_search_request(Config) ->
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

                %% Riak YZ is async.
                %% It takes time to index docs  (usually 1 second).
                %% https://docs.riak.com/riak/kv/latest/developing/usage/search/index.html#indexing-values
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

muc_light_simple(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = <<"testroom">>,
            given_muc_light_room(Room, Alice, []),

            M1 = when_muc_light_message_is_sent(Alice, Room,
                                                <<"Msg 1">>, <<"Id1">>),
            then_muc_light_message_is_received_by([Alice], M1),

            M2 = when_muc_light_message_is_sent(Alice, Room,
                                                <<"Message 2">>, <<"MyID2">>),
            then_muc_light_message_is_received_by([Alice], M2),

            Aff = when_muc_light_affiliations_are_set(Alice, Room, [{Bob, member}]),
            then_muc_light_affiliations_are_received_by([Alice, Bob], Aff),

            maybe_wait_for_archive(Config),
            when_archive_query_is_sent(Bob, muc_light_helper:room_bin_jid(Room), Config),
            ExpectedResponse = [{create, [{Alice, owner}]},
                                {muc_message, Room, Alice, <<"Msg 1">>},
                                {muc_message, Room, Alice, <<"Message 2">>},
                                {affiliations, [{Bob, member}]}],
            then_archive_response_is(Bob, ExpectedResponse, Config)
        end).

muc_light_shouldnt_modify_pm_archive(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = <<"testroom2">>,
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
        end).

muc_light_stored_in_pm_if_allowed_to(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = <<"testroom_pm">>,
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

muc_light_chat_markers_are_archived_if_enabled(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            Room = <<"testroom_markers">>,
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
            Room = <<"testroom_no_markers">>,
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

retrieve_form_fields(ConfigIn) ->
    escalus_fresh:story(ConfigIn, [{alice, 1}], fun(Alice) ->
        P = ?config(props, ConfigIn),
        Namespace = get_prop(mam_ns, P),
        escalus:send(Alice, stanza_retrieve_form_fields(<<"q">>, Namespace)),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_with_ns, [Namespace], Res)
    end).

archived(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        Msg = escalus:wait_for_stanza(Bob),
        try
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
        catch Class:Reason:StackTrace ->
            ct:pal("Msg ~p", [Msg]),
            erlang:raise(Class, Reason, StackTrace)
        end
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
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

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
            NoStoreEl = #xmlel{name = <<"no-store">>},

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

pagination_first5_opt_count(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"first5_opt">>, RSM)),
        wait_message_range(Alice, 1, 5),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_first25_opt_count_all(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 25.
        RSM = #rsm_in{max=25},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"first25_opt_all">>, RSM)),
        wait_message_range(Alice, 1, 15),
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

pagination_last5_opt_count(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last5_opt">>, RSM)),
        wait_message_range(Alice, 11, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_last25_opt_count_all(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 25.
        RSM = #rsm_in{max=25, direction=before, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last25_opt_all">>, RSM)),
        wait_message_range(Alice, 1, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_offset5_opt_count(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Skip 5 messages, get 5 messages.
        RSM = #rsm_in{max=5, index=5, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last5_opt">>, RSM)),
        wait_message_range(Alice, 6, 10),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_offset5_opt_count_all(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Skip 5 messages, get 25 messages (only 10 are available).
        RSM = #rsm_in{max=25, index=5, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"last5_opt_all">>, RSM)),
        wait_message_range(Alice, 6, 15),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).


pagination_before10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"before10">>, RSM)),
        wait_message_range(Alice, 5, 9),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

pagination_simple_before10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config), simple=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(P, <<"before10">>, RSM)),
     %% wait_message_range(Client, TotalCount,    Offset, FromN, ToN),
        wait_message_range(Alice,   undefined, undefined,     5,   9),
        ok
        end,
    parallel_story(Config, [{alice, 1}], F).

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

server_returns_item_not_found_for_before_filter_with_nonexistent_id(Config) ->
    NonexistentID = <<"AV25E9SCO50K">>,
    RSM = #rsm_in{max = 5, direction = 'before', id = NonexistentID},
    StanzaID = <<"before-nonexistent-id">>,
    server_returns_item_not_found_for_nonexistent_id(Config, RSM, StanzaID).

server_returns_item_not_found_for_after_filter_with_nonexistent_id(Config) ->
    NonexistentID = <<"AV25E9SCO50K">>,
    RSM = #rsm_in{max = 5, direction = 'after', id = NonexistentID},
    StanzaID = <<"after-nonexistent-id">>,
    server_returns_item_not_found_for_nonexistent_id(Config, RSM, StanzaID).

server_returns_item_not_found_for_nonexistent_id(Config, RSM, StanzaID) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        rsm_send(Config, Alice,
                 stanza_page_archive_request(P, StanzaID, RSM)),
        Res = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Res),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Res),
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
    _P = ?config(props, Config),
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

        escalus:send(Alice, stanza_prefs_get_request(mam_ns_binary())),
        ReplyGet = escalus:wait_for_stanza(Alice),

        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assert_equal(ResultIQ1, ResultIQ2),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

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
    _P = ?config(props, Config),
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
                                                      {xmlcdata, <<"\n">>}, %% Put as it is
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

mam_service_discovery(Config) ->
    _P = ?config(props, Config),
    F = fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:disco_info(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        try
        escalus:assert(is_iq_result, Stanza),
        escalus:assert(has_feature, [mam_ns_binary_v04()], Stanza),
        ok
        catch Class:Reason:StackTrace ->
            ct:pal("Stanza ~p.", [Stanza]),
            erlang:raise(Class, Reason, StackTrace)
        end
        end,
    escalus_fresh:story(Config, [{alice, 1}], F).

%% Check, that MUC is supported.
muc_service_discovery(Config) ->
    _P = ?config(props, Config),
    F = fun(Alice) ->
        Domain = ct:get_config({hosts, mim, domain}),
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [muc_host()], Stanza),
        escalus:assert(is_stanza_from, [Domain], Stanza),
        ok
        end,
    escalus:fresh_story(Config, [{alice, 1}], F).

metric_incremented_on_archive_request(ConfigIn) ->
    P = ?config(props, ConfigIn),
    F = fun(Alice) ->
        escalus:send(Alice, stanza_archive_request(P, <<"metric_q1">>)),
        Res = wait_archive_respond(Alice),
        assert_respond_size(0, Res),
        assert_respond_query_id(P, <<"metric_q1">>, parse_result_iq(Res)),
        ok
        end,
    MongooseMetrics = [{[host(), backends, mod_mam, lookup], changed}],
    Config = [{mongoose_metrics, MongooseMetrics} | ConfigIn],
    escalus_fresh:story(Config, [{alice, 1}], F).

metric_incremented_when_store_message(Config) ->
    archived(Config).

messages_filtered_when_prefs_default_policy_is_always(Config) ->
    run_prefs_cases(always, Config).

messages_filtered_when_prefs_default_policy_is_never(Config) ->
    run_prefs_cases(never, Config).

messages_filtered_when_prefs_default_policy_is_roster(Config) ->
    run_prefs_cases(roster, Config).


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

%% The same as prefs_set_request case but for different configurations
run_set_and_get_prefs_cases(ConfigIn) ->
    _P = ?config(props, ConfigIn),
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
  ok = rpc(mim(), ejabberd_auth, try_register, [AdminU, AdminS, AdminP]),
  %% admin user already registered
  true = rpc(mim(), ejabberd_users, does_user_exist, [AdminU, AdminS]),
  false = rpc(mim(), ejabberd_users, does_user_exist, [<<"fake-user">>, AdminS]),
  false = rpc(mim(), ejabberd_users, does_user_exist, [AdminU, <<"fake-domain">>]),
  %% cleanup
  ok = rpc(mim(), ejabberd_auth, remove_user, [AdminU, AdminS]).

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
