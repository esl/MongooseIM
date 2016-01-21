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
-export([mam_service_discovery/1,
         muc_service_discovery/1,
         simple_archive_request/1,
         muc_archive_request/1,
         muc_archive_purge/1,
         muc_multiple_devices/1,
         muc_private_message/1,
         range_archive_request/1,
         range_archive_request_not_empty/1,
         limit_archive_request/1,
         prefs_set_request/1,
         prefs_set_cdata_request/1,
         pagination_first5/1,
         pagination_last5/1,
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
         archived/1,
         strip_archived/1,
         filter_forwarded/1,
         policy_violation/1,
         offline_message/1,
         purge_single_message/1,
         purge_multiple_messages/1,
         purge_old_single_message/1,
         querying_for_all_messages_with_jid/1,
         muc_querying_for_all_messages/1,
         muc_querying_for_all_messages_with_jid/1,
         iq_spoofing/1]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml_stream.hrl").

-define(assert_equal(E, V), (
    [ct:fail("ASSERT EQUAL~n\tExpected ~p~n\tValue ~p~n", [(E), (V)])
     || (E) =/= (V)]
    )).

-record(rsm_in, {
        max         :: non_neg_integer() | undefined,
        direction   :: before | 'after' | undefined,
        id          :: binary() | undefined,
        index       :: non_neg_integer() | undefined,
        after_id    :: binary() | undefined,
        before_id   :: binary() | undefined,
        from_id     :: binary() | undefined,
        to_id       :: binary() | undefined,
        simple = false :: boolean(),
        opt_count = false :: boolean()
        }).

-record(forwarded_message, {
    from           :: binary() | undefined,
    to             :: binary() | undefined,
    result_queryid :: binary() | undefined,
    result_id      :: binary() | undefined,
    delay_from     :: binary() | undefined,
    delay_stamp    :: binary() | undefined,
    message_to     :: binary() | undefined,
    message_type   :: binary() | undefined,
    message_body   :: binary() | undefined
}).

-record(result_iq, {
    from            :: binary(),
    to              :: binary(),
    id              :: binary(),
    first           :: binary() | undefined,
    first_index     :: non_neg_integer() | undefined,
    last            :: binary() | undefined,
    count           :: non_neg_integer()
}).

-record(error_iq, {
    id              :: binary(),
    type            :: binary(),
    error_type      :: binary(),
    condition       :: binary(),
    text            :: binary()
}).

-record(prefs_result_iq, {
    default_mode    :: binary() | undefined,
    always_jids = [] :: [binary()],
    never_jids  = [] :: [binary()]
}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

muc_host() ->
    <<"muc.localhost">>.

host() ->
    <<"localhost">>.

configurations() ->
    odbc_configs(is_odbc_enabled(host()))
    ++ riak_configs(is_riak_enabled(host())).

odbc_configs(true) ->
    [odbc,
     odbc_async_pool,
     odbc_mnesia,
     odbc_async_cache,
     odbc_cache,
     odbc_mnesia_cache,
     odbc_mnesia_muc_cache];
odbc_configs(_) ->
    [].

riak_configs(true) ->
     [riak_timed_yz_buckets];
riak_configs(_) ->
     [].

basic_group_names() ->
    [
    mam,
    mam_purge,
    muc,
    muc_with_pm,
    rsm,
    with_rsm,
    muc_rsm,
    bootstrapped,
    archived,
    policy_violation,
    offline_message
    ].

all() ->
    Reasons =
    case is_mam_possible(host())  of
        false -> [require_odbc];
        true  -> []
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

is_skipped(riak_timed_yz_buckets, G) ->
    lists:member(G, [muc, muc_with_pm, muc_rsm]);
is_skipped(_, _) ->
    false.


basic_groups() ->
    [{bootstrapped,     [], bootstrapped_cases()},
     {mam,              [], mam_cases()},
     {mam_purge,        [], mam_purge_cases()},
     {archived,         [], archived_cases()},
     {policy_violation, [], policy_violation_cases()},
     {offline_message,  [], offline_message_cases()},
     {muc,              [], muc_cases()},
     {muc_with_pm,      [], muc_cases()},
     {rsm,              [], rsm_cases()},
     {muc_rsm,          [], muc_rsm_cases()},
     {with_rsm,         [], with_rsm_cases()}].

bootstrapped_cases() ->
     [purge_old_single_message,
      querying_for_all_messages_with_jid].

mam_cases() ->
    [mam_service_discovery,
     simple_archive_request,
     range_archive_request,
     range_archive_request_not_empty,
     limit_archive_request,
     prefs_set_request,
     prefs_set_cdata_request,
     iq_spoofing
     ].

mam_purge_cases() ->
    [purge_single_message,
     purge_multiple_messages].

archived_cases() ->
    [archived,
     strip_archived,
     filter_forwarded].

policy_violation_cases() ->
    [policy_violation].

offline_message_cases() ->
    [offline_message].

muc_cases() ->
    [muc_service_discovery,
     muc_archive_request,
     muc_archive_purge,
     muc_multiple_devices,
     muc_private_message,
     muc_querying_for_all_messages,
     muc_querying_for_all_messages_with_jid
     ].

muc_rsm_cases() ->
    rsm_cases().

with_rsm_cases() ->
    rsm_cases().

rsm_cases() ->
      [pagination_first5,
       pagination_last5,
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
       pagination_offset5_opt_count_all].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    [{escalus_user_db, {module, escalus_ejabberd}}
     | escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

user_names() ->
    [alice, bob].

create_users(Config) ->
    escalus:create_users(Config, {by_name, user_names()}).

delete_users(Config) ->
    escalus:delete_users(Config, {by_name, user_names()}).

init_per_group(Group, ConfigIn) ->
   C = configuration(Group),
   B = basic_group(Group),
   case init_modules(C, B, ConfigIn) of
        skip ->
            {skip, {init_modules, C, B, ConfigIn}};
        Config0 ->
            ct:pal("Init per group ~p; configuration ~p; basic group ~p",
                   [Group, C, B]),
            Config1 = do_init_per_group(C, Config0),
            [{basic_group, B}, {configuration, C} | init_state(C, B, Config1)]
    end.

do_init_per_group(C, ConfigIn) ->
    Config0 = create_users(ConfigIn),
    case C of
        riak_timed_yz_buckets ->
            [{yz_wait, 2500} | Config0];
        _ ->
            Config0
    end.

end_per_group(Group, Config) ->
    delete_users(Config),
    C = configuration(Group),
    B = basic_group(Group),
    end_modules(C, B, Config).

init_modules(C, muc_rsm, Config) ->
    init_modules(C, muc, Config);

init_modules(ca, muc_with_pm, Config) ->
    %% TODO add mod_mam with Cassandra
    init_module(host(), mod_mam_muc_ca_arch, []),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc, muc_with_pm, Config) ->
    %% TODO test both mod_mam_muc_odbc_arch and mod_mam_odbc_arch
    init_module(host(), mod_mam_odbc_arch, [muc, pm]),
    init_module(host(), mod_mam_odbc_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_async_pool, muc_with_pm, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, [no_writer]),
    init_module(host(), mod_mam_muc_odbc_async_pool_writer, []),
    init_module(host(), mod_mam_odbc_arch, [no_writer, pm]),
    init_module(host(), mod_mam_odbc_async_pool_writer, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_mnesia, muc_with_pm, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_mnesia_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_cache, muc_with_pm, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam_cache_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_async_cache, muc_with_pm, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, [no_writer]),
    init_module(host(), mod_mam_muc_odbc_async_pool_writer, []),
    init_module(host(), mod_mam_odbc_arch, [no_writer, pm]),
    init_module(host(), mod_mam_odbc_async_pool_writer, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam_cache_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_mnesia_muc_cache, muc_with_pm, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_mnesia_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam_cache_user, [pm]),
    init_module(host(), mod_mam_muc_cache_user, []),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_mnesia_cache, muc_with_pm, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_mnesia_prefs, [muc, pm]),
    init_module(host(), mod_mam_odbc_user, [muc, pm]),
    init_module(host(), mod_mam_cache_user, [muc, pm]),
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;

init_modules(ca, muc, Config) ->
    init_module(host(), mod_mam_muc_ca_arch, []),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc, muc, Config) ->
    %% TODO test both mod_mam_muc_odbc_arch and mod_mam_odbc_arch
    init_module(host(), mod_mam_odbc_arch, [muc]),
    init_module(host(), mod_mam_odbc_prefs, [muc]),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_async_pool, muc, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, [no_writer]),
    init_module(host(), mod_mam_muc_odbc_async_pool_writer, []),
    init_module(host(), mod_mam_odbc_prefs, [muc]),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_mnesia, muc, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [muc]),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_cache, muc, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_odbc_prefs, [muc]),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_async_cache, muc, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, [no_writer]),
    init_module(host(), mod_mam_muc_odbc_async_pool_writer, []),
    init_module(host(), mod_mam_odbc_prefs, [muc]),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_mnesia_muc_cache, muc, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [muc]),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_muc_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc_mnesia_cache, muc, Config) ->
    init_module(host(), mod_mam_muc_odbc_arch, []),
    init_module(host(), mod_mam_mnesia_prefs, [muc]),
    init_module(host(), mod_mam_odbc_user, [muc]),
    init_module(host(), mod_mam_cache_user, [muc]),
    init_module(host(), mod_mam_muc, [{host, "muc.@HOST@"}]),
    Config;
init_modules(odbc, _, Config) ->
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    Config;
init_modules(ca, _, Config) ->
    init_module(host(), mod_mam_con_ca_arch, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    init_module(host(), mod_mam, []),
    Config;
init_modules(odbc_async, _, Config) ->
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_odbc_arch, [pm, no_writer]),
    init_module(host(), mod_mam_odbc_async_writer, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    Config;
init_modules(riak_timed_yz_buckets, _, Config) ->
    init_module(host(), mod_mam_riak_timed_arch_yz, [pm]),
    init_module(host(), mod_mam, []),
    Config;
init_modules(odbc_async_pool, _, Config) ->
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_odbc_arch, [pm, no_writer]),
    init_module(host(), mod_mam_odbc_async_pool_writer, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    Config;
init_modules(odbc_mnesia, _, Config) ->
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_mnesia_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    Config;
init_modules(odbc_cache, _, Config) ->
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    init_module(host(), mod_mam_cache_user, [pm]),
    Config;
init_modules(odbc_async_cache, _, Config) ->
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_odbc_arch, [pm, no_writer]),
    init_module(host(), mod_mam_odbc_async_pool_writer, [pm]),
    init_module(host(), mod_mam_odbc_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    init_module(host(), mod_mam_cache_user, [pm]),
    Config;
init_modules(odbc_mnesia_muc_cache, _, _Config) ->
    skip;
init_modules(odbc_mnesia_cache, _, Config) ->
    init_module(host(), mod_mam, []),
    init_module(host(), mod_mam_odbc_arch, [pm]),
    init_module(host(), mod_mam_mnesia_prefs, [pm]),
    init_module(host(), mod_mam_odbc_user, [pm]),
    init_module(host(), mod_mam_cache_user, [pm]),
    Config.

end_modules(_, _, Config) ->
    [stop_module(host(), M) || M <- mam_modules()],
    Config.

mam_modules() ->
    [mod_mam,
     mod_mam_muc,
     mod_mam_con_ca_arch,
     mod_mam_ca_arch,
     mod_mam_muc_ca_arch,
     mod_mam_odbc_arch,
     mod_mam_muc_odbc_arch,
     mod_mam_con_ca,
     mod_mam_odbc_async_pool_writer,
     mod_mam_muc_odbc_async_pool_writer,
     mod_mam_odbc_prefs,
     mod_mam_mnesia_prefs,
     mod_mam_mnesia_dirty_prefs,
     mod_mam_odbc_user,
     mod_mam_cache_user,
     mod_mam_muc_cache_user,
     mod_mam_riak_timed_arch_yz].

init_state(_, muc_rsm, Config) ->
    Config1 = start_alice_room(Config),
    Config2 = clean_room_archive(Config1),
    Config3 = send_muc_rsm_messages(Config2),
    [{muc_rsm, true} | Config3];
init_state(_, muc, Config) ->
    Config;
init_state(_, muc_with_pm, Config) ->
    Config;
init_state(_, rsm, Config) ->
    send_rsm_messages(clean_archives(Config));
init_state(_, with_rsm, Config) ->
    Config1 = [{with_rsm, true}|Config],
    send_rsm_messages(clean_archives(Config1));
init_state(_, _, Config) ->
    clean_archives(Config).


init_per_testcase(C=archived, ConfigIn) ->
    Config = case ?config(configuration, ConfigIn) of
                 odbc_async_pool ->
                     MongooseMetrics = [
                                        {[data, odbc, mam_async],
                                         [{recv_oct, '>'}, {send_oct, '>'}]}
                                       ],
                     [{mongoose_metrics, MongooseMetrics} | ConfigIn];
                 _ ->
                     ConfigIn
             end,
    escalus:init_per_testcase(C, clean_archives(Config));
init_per_testcase(C=strip_archived, Config) ->
    escalus:init_per_testcase(C, clean_archives(Config));
init_per_testcase(C=filter_forwarded, Config) ->
    escalus:init_per_testcase(C, clean_archives(Config));
init_per_testcase(C=purge_single_message, Config) ->
    escalus:init_per_testcase(C, clean_archives(Config));
init_per_testcase(C=purge_multiple_messages, Config) ->
    escalus:init_per_testcase(C, clean_archives(Config));
init_per_testcase(C=purge_old_single_message, Config) ->
    escalus:init_per_testcase(C,
        bootstrap_archive(clean_archives(Config)));
init_per_testcase(C=querying_for_all_messages_with_jid, Config) ->
    escalus:init_per_testcase(C,
        bootstrap_archive(clean_archives(Config)));
init_per_testcase(C=offline_message, Config) ->
    escalus:init_per_testcase(C,
        bootstrap_archive(clean_archives(Config)));
init_per_testcase(C=muc_querying_for_all_messages, Config) ->
    escalus:init_per_testcase(C,
        muc_bootstrap_archive(start_alice_room(Config)));
init_per_testcase(C=muc_querying_for_all_messages_with_jid, Config) ->
    escalus:init_per_testcase(C,
        muc_bootstrap_archive(start_alice_room(Config)));
init_per_testcase(C=muc_archive_request, Config) ->
    escalus:init_per_testcase(C, clean_room_archive(start_alice_room(Config)));
init_per_testcase(C=muc_archive_purge, Config) ->
    escalus:init_per_testcase(C, clean_room_archive(start_alice_room(Config)));
init_per_testcase(C=muc_multiple_devices, Config) ->
    escalus:init_per_testcase(C, clean_room_archive(start_alice_room(Config)));
init_per_testcase(C=muc_private_message, Config) ->
    escalus:init_per_testcase(C, start_alice_room(Config));
init_per_testcase(C=range_archive_request_not_empty, Config) ->
    escalus:init_per_testcase(C,
        bootstrap_archive(clean_archives(Config)));
init_per_testcase(C=prefs_set_request, Config) ->
    skip_if_riak(C, Config);
init_per_testcase(C=prefs_set_cdata_request, Config) ->
    skip_if_riak(C, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

skip_if_riak(C, Config) ->
    case ?config(configuration, Config) of
        riak_timed_yz_buckets ->
            {skip, "prefs not implemented for riak"};
        _ ->
            escalus:init_per_testcase(C, Config)
    end.

end_per_testcase(C=muc_archive_request, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_archive_purge, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_multiple_devices, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_private_message, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_querying_for_all_messages, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(C=muc_querying_for_all_messages_with_jid, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(C, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

init_module(Host, Mod, Args) ->
    lists:member(Mod, mam_modules())
    orelse
    ct:fail("Unknown module ~p", [Mod]),
    stop_module(Host, Mod),
    ok = start_module(Host, Mod, Args).

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
    {atomic, ok} = rpc_apply(gen_mod, stop_module, [Host, Mod]),
    ok.

rpc_apply(M, F, Args) ->
    case rpc_call(M, F, Args) of
    {badrpc, Reason} ->
        ct:fail("~p:~p/~p with arguments ~w fails with reason ~p.",
                [M, F, length(Args), Args, Reason]);
    Result ->
        Result
    end.

rpc_call(M, F, A) ->
    Node = escalus_ct:get_config(ejabberd_node),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, 10000, Cookie).

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
%% `make_greedy(odbc_mnesia_muc, [odbc, odbc_mnesia]) -> odbc'
%% `make_greedy(odbc_mnesia_muc, match_longer_first([odbc, odbc_mnesia])) -> odbc_mnesia'
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
    match_atom_prefix_1(atom_to_list(Target), Prefixes).

match_atom_prefix_1(TargetS, [PrefixA|Prefixes]) ->
    PrefixS = atom_to_list(PrefixA),
    case lists:prefix(PrefixS, TargetS) of
        true -> PrefixA;
        false -> match_atom_prefix_1(TargetS, Prefixes)
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

%% Querying the archive for messages
simple_archive_request(ConfigIn) ->
    F = fun(Alice, Bob) ->
        %% Alice sends "OH, HAI!" to Bob
        %% {xmlel,<<"message">>,
        %%  [{<<"from">>,<<"alice@localhost/res1">>},
        %%   {<<"to">>,<<"bob@localhost/res1">>},
        %%   {<<"xml:lang">>,<<"en">>},
        %%   {<<"type">>,<<"chat">>}],
        %%   [{xmlel,<<"body">>,[],[{xmlcdata,<<"OH, HAI!">>}]}]}
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        maybe_wait_for_yz(ConfigIn),
        escalus:send(Alice, stanza_archive_request(<<"q1">>)),
        assert_respond_size(1, wait_archive_respond_iq_first(Alice)),
        ok
        end,
    MongooseMetrics = [{[backends, mod_mam, archive], changed},
                       {[backends, mod_mam, lookup], changed}
                      ],
    Config = [{mongoose_metrics, MongooseMetrics} | ConfigIn],
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

querying_for_all_messages_with_jid(Config) ->
    F = fun(Alice) ->
        Pregenerated = ?config(pre_generated_msgs, Config),
        BWithJID = nick_to_jid(bob, Config),

        WithBob = [1 || {_, _, {JID, _, _}, _, _} <- Pregenerated, JID == BWithJID],

        CountWithBob = lists:sum(WithBob),
        escalus:send(Alice, stanza_filtered_by_jid_request(BWithJID)),
        assert_respond_size(CountWithBob, wait_archive_respond_iq_first(Alice)),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_querying_for_all_messages(Config) ->
    F = fun(Alice) ->
        Room = ?config(room, Config),
        MucMsgs = ?config(pre_generated_muc_msgs, Config),

        MucArchiveLen = length(MucMsgs),

        IQ = stanza_archive_request(<<>>),
        escalus:send(Alice, stanza_to_room(IQ, Room)),
        assert_respond_size(MucArchiveLen, wait_archive_respond_iq_first(Alice)),

        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

muc_querying_for_all_messages_with_jid(Config) ->
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        BWithJID = room_address(Room, nick(Bob)),

        MucMsgs = ?config(pre_generated_muc_msgs, Config),
        WithJID = [1 || {_, _, {JID, _, _}, _, _} <- MucMsgs, JID == BWithJID],
        Len = lists:sum(WithJID),

        IQ = stanza_filtered_by_jid_request(BWithJID),
        escalus:send(Alice, stanza_to_room(IQ, Room)),
        Result = wait_archive_respond_iq_first(Alice),

        assert_respond_size(Len, Result),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

archived(Config) ->
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        Msg = escalus:wait_for_stanza(Bob),
        try
        Arc = exml_query:subelement(Msg, <<"archived">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),

        ?assert_equal(By, escalus_client:short_jid(Bob)),

        %% Bob calls archive.
        maybe_wait_for_yz(Config),
        escalus:send(Bob, stanza_archive_request(<<"q1">>)),
        [_ArcIQ, ArcMsg] = assert_respond_size(1, wait_archive_respond_iq_first(Bob)),
        #forwarded_message{result_id=ArcId} = parse_forwarded_message(ArcMsg),
        ?assert_equal(Id, ArcId),
        ok
        catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            ct:pal("Msg ~p", [Msg]),
            erlang:raise(Class, Reason, Stacktrace)
        end
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

filter_forwarded(Config) ->
    F = fun(Alice, Bob) ->
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        escalus:wait_for_stanza(Bob),
        maybe_wait_for_yz(Config),
        escalus:send(Bob, stanza_archive_request(<<"q1">>)),
        [_ArcIQ1, _ArcMsg1] = assert_respond_size(1, wait_archive_respond_iq_first(Bob)),

        %% Check, that previous forwarded message was not archived.
        escalus:send(Bob, stanza_archive_request(<<"q2">>)),
        [_ArcIQ2, _ArcMsg2] = assert_respond_size(1, wait_archive_respond_iq_first(Bob)),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

strip_archived(Config) ->
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice,
                     append_subelem(escalus_stanza:chat_to(Bob, <<"OH, HAI!">>),
                                    archived_elem(escalus_client:short_jid(Bob),
                                                  <<"fake-id">>))),

        %% Bob receives a message.
        Msg = escalus:wait_for_stanza(Bob),
        Arc = exml_query:subelement(Msg, <<"archived">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),

        ?assert_equal(escalus_client:short_jid(Bob), By),

        try
        %% Bob calls archive.
        maybe_wait_for_yz(Config),
        escalus:send(Bob, stanza_archive_request(<<"q1">>)),
        [_ArcIQ, ArcMsg] = assert_respond_size(1, wait_archive_respond_iq_first(Bob)),
        #forwarded_message{result_id=ArcId} = parse_forwarded_message(ArcMsg),
        ?assert_equal(ArcId, Id),
        ok
        catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            ct:pal("Msg ~p", [Msg]),
            erlang:raise(Class, Reason, Stacktrace)
        end
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

wait_archive_respond_iq_first(User) ->
    %% rot1
    [IQ|Messages] = lists:reverse(wait_archive_respond(User)),
    [IQ|lists:reverse(Messages)].

wait_archive_respond(User) ->
    S = escalus:wait_for_stanza(User, 5000),
    case escalus_pred:is_iq_error(S) of
        true ->
            ct:pal("Stanza ~p", [S]),
            ct:fail("Unexpected error IQ.", []);
        false -> ok
    end,
    case escalus_pred:is_iq_result(S) of
        true  -> [S];
        false -> [S|wait_archive_respond(User)]
    end.

assert_respond_size(Size, Respond) when length(Respond) =:= (Size + 1) ->
    Respond;
assert_respond_size(ExpectedSize, Respond) ->
    RespondSize = length(Respond) - 1,
    ct:fail("Respond size is ~p, ~p is expected.", [RespondSize, ExpectedSize]).
    %% void()

%% To conserve resources, a server MAY place a reasonable limit on how many
%% stanzas may be pushed to a client in one request.
%% If a query returns a number of stanzas greater than this limit and
%% the client did not specify a limit using RSM then the server should
%% return a policy-violation error to the client.
policy_violation(Config) ->
    F = fun(Alice, Bob) ->
        %% Alice sends messages to Bob.
        %% WARNING: are we sending too fast?
        [escalus:send(Alice,
                      escalus_stanza:chat_to(Bob, generate_message_text(N)))
         || N <- lists:seq(1, 51)],
        %% Bob is waiting for 51 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 51, 5000),
        maybe_wait_for_yz(Config),
        %% Get whole history (queryid is "will_fail", id is random).
        escalus:send(Alice, stanza_archive_request(<<"will_fail">>)),
        ErrorIQ = escalus:wait_for_stanza(Alice, 5000),
        try
            #error_iq{condition = Condition} = parse_error_iq(ErrorIQ),
            ?assert_equal(<<"policy-violation">>, Condition),
            ok
        catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            ct:pal("ErrorIQ ~p", [ErrorIQ]),
            erlang:raise(Class, Reason, Stacktrace)
        end
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

%% Ensure, that a offline message does not stored twice when delivered.
offline_message(Config) ->
    Msg = <<"Is there anybody here?">>,
    F = fun(Alice) ->
        %% Alice sends a message to Bob while bob is offline.
        escalus:send(Alice,
                     escalus_stanza:chat_to(bob, Msg)),
        maybe_wait_for_yz(Config),
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
    escalus:send(Bob, stanza_archive_request(<<"q1">>)),
    [_ArcRes | ArcMsgs] = R = wait_archive_respond_iq_first(Bob),
    assert_only_one_of_many_is_equal(ArcMsgs, Msg),

    escalus_cleaner:clean(Config).

purge_single_message(Config) ->
    F = fun(Alice, Bob) ->
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            maybe_wait_for_yz(Config),
            escalus:send(Alice, stanza_archive_request(<<"q1">>)),
            [_IQ, Mess] = assert_respond_size(1, wait_archive_respond_iq_first(Alice)),
            ParsedMess = parse_forwarded_message(Mess),
            #forwarded_message{result_id=MessId} = ParsedMess,
            escalus:send(Alice, stanza_purge_single_message(MessId)),
            %% Waiting for ack.
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice, 5000)),
            escalus:send(Alice, stanza_archive_request(<<"q2">>)),
            assert_respond_size(0, wait_archive_respond_iq_first(Alice)),
            ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

purge_old_single_message(Config) ->
    F = fun(Alice) ->
            escalus:send(Alice, stanza_archive_request(<<"q1">>)),
            Pregenderated = ?config(pre_generated_msgs, Config),
            AliceArchSize = length(Pregenderated),
            [_IQ|AllMessages] = assert_respond_size(AliceArchSize,
                wait_archive_respond_iq_first(Alice)),
            ParsedMessages = [parse_forwarded_message(M) || M <- AllMessages],
            %% Delete fifth message.
            ParsedMess = lists:nth(5, ParsedMessages),
            #forwarded_message{result_id=MessId} = ParsedMess,
            escalus:send(Alice, stanza_purge_single_message(MessId)),
            %% Waiting for ack.
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice, 5000)),
            %% Check, that it was deleted.
            escalus:send(Alice, stanza_archive_request(<<"q2">>)),
            assert_respond_size(AliceArchSize - 1, wait_archive_respond_iq_first(Alice)),
            ok
        end,
    escalus:story(Config, [{alice, 1}], F).

purge_multiple_messages(Config) ->
    F = fun(Alice, Bob) ->
            %% Alice sends messages to Bob.
            [begin
                escalus:send(Alice,
                    escalus_stanza:chat_to(Bob, generate_message_text(N)))
             end || N <- lists:seq(1, 15)],
            maybe_wait_for_yz(Config),
            %% Bob is waiting for 15 messages for 5 seconds.
            escalus:wait_for_stanzas(Bob, 15, 5000),
            %% Bob purges all messages from his archive.
            escalus:send(Bob, stanza_purge_multiple_messages(
                    undefined, undefined, undefined)),
            %% Waiting for ack.
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob, 15000)),
            escalus:send(Bob, stanza_archive_request(<<"q2">>)),
            assert_respond_size(0, wait_archive_respond_iq_first(Bob)),
            ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_archive_request(Config) ->
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

        %% Alice sends to the chat room.
		escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by alicesroom@localhost).
        %% User's archive is disabled (i.e. bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        Arc = exml_query:subelement(BobMsg, <<"archived">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),


        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(<<"q1">>), Room)),
        [_ArcRes, ArcMsg] = assert_respond_size(1, wait_archive_respond_iq_first(Bob)),
        #forwarded_message{result_id=ArcId, message_body=ArcMsgBody} =
            parse_forwarded_message(ArcMsg),
        ?assert_equal(Text, ArcMsgBody),
        ?assert_equal(ArcId, Id),
        ?assert_equal(RoomAddr, By),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).
%% Copied from 'muc_archive_reuest' test in case to show some bug in mod_mam_muc related to issue #512
muc_archive_purge(Config) ->
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

        %% Alice sends to the chat room.
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Text)),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by alicesroom@localhost).
        %% User's archive is disabled (i.e. bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        %% Flush all msgs to Alice
        escalus:wait_for_stanzas(Alice, 6),
        %% Alice purges the room's archive.
        escalus:send(Alice, stanza_to_room(stanza_purge_multiple_messages(
           undefined, undefined, undefined), Room)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
        ok
    end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

muc_multiple_devices(Config) ->
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

        Alice1Arc = exml_query:subelement(Alice1Msg, <<"archived">>),
        Alice2Arc = exml_query:subelement(Alice2Msg, <<"archived">>),
        ?assert_equal(Alice1Arc, Alice2Arc),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by alicesroom@localhost).
        %% User's archive is disabled (i.e. bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, BobMsg),
        Arc = exml_query:subelement(BobMsg, <<"archived">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),

        ?assert_equal(Alice1Arc, Arc),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(<<"q1">>), Room)),
        [_ArcRes, ArcMsg] = assert_respond_size(1, wait_archive_respond_iq_first(Bob)),
        #forwarded_message{result_id=ArcId, message_body=ArcMsgBody} =
            parse_forwarded_message(ArcMsg),
        ?assert_equal(Text, ArcMsgBody),
        ?assert_equal(ArcId, Id),
        ?assert_equal(RoomAddr, By),
        ok
        end,
    escalus:story(Config, [{alice, 2}, {bob, 1}], F).

muc_private_message(Config) ->
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
        escalus:send(Bob, stanza_to_room(stanza_archive_request(<<"q1">>), Room)),
        [_ArcRes] = assert_respond_size(0, wait_archive_respond_iq_first(Bob)),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

%% @doc Querying the archive for all messages in a certain timespan.
range_archive_request(Config) ->
    F = fun(Alice) ->
        %% Send
        %% <iq type='get'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%     <start>2010-06-07T00:00:00Z</start>
        %%     <end>2010-07-07T13:23:54Z</end>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_date_range_archive_request()),
        IQ = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_iq_result, IQ),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

range_archive_request_not_empty(Config) ->
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
        escalus:send(Alice, stanza_date_range_archive_request_not_empty(StartTime, StopTime)),
        %% Receive two messages and IQ
        M1 = escalus:wait_for_stanza(Alice, 5000),
        M2 = escalus:wait_for_stanza(Alice, 5000),
        IQ = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_iq_result, IQ),
        #forwarded_message{delay_stamp=Stamp1} = parse_forwarded_message(M1),
        #forwarded_message{delay_stamp=Stamp2} = parse_forwarded_message(M2),
        ?assert_equal(list_to_binary(StartTime), Stamp1),
        ?assert_equal(list_to_binary(StopTime), Stamp2),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

make_iso_time(Micro) ->
    Now = usec:to_now(Micro),
    DateTime = calendar:now_to_datetime(Now),
    {Time, TimeZone} = rpc_apply(jlib, timestamp_to_iso, [DateTime, utc]),
    Time ++ TimeZone.

%% @doc A query using Result Set Management.
%% See also `#rsm_in.max'.
limit_archive_request(Config) ->
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
        escalus:send(Alice, stanza_limit_archive_request()),
        [IQ | Msgs] = wait_archive_respond_iq_first(Alice),
        escalus:assert(is_iq_result, IQ),
        10 = length(Msgs),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_empty_rset(Config) ->
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=0},

        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"empty_rset">>, RSM)),
        wait_empty_rset(Alice, 15)
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_first5(Config) ->
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"first5">>, RSM)),
        wait_message_range(Alice, 1, 5),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_first5_opt_count(Config) ->
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"first5_opt">>, RSM)),
        wait_message_range(Alice, 1, 5),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_first25_opt_count_all(Config) ->
    F = fun(Alice) ->
        %% Get the first page of size 25.
        RSM = #rsm_in{max=25},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"first25_opt_all">>, RSM)),
        wait_message_range(Alice, 1, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_last5(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"last5">>, RSM)),
        wait_message_range(Alice, 11, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_last5_opt_count(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"last5_opt">>, RSM)),
        wait_message_range(Alice, 11, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_last25_opt_count_all(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 25.
        RSM = #rsm_in{max=25, direction=before, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"last25_opt_all">>, RSM)),
        wait_message_range(Alice, 1, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_offset5_opt_count(Config) ->
    F = fun(Alice) ->
        %% Skip 5 messages, get 5 messages.
        RSM = #rsm_in{max=5, index=5, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"last5_opt">>, RSM)),
        wait_message_range(Alice, 6, 10),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_offset5_opt_count_all(Config) ->
    F = fun(Alice) ->
        %% Skip 5 messages, get 25 messages (only 10 are available).
        RSM = #rsm_in{max=25, index=5, opt_count=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"last5_opt_all">>, RSM)),
        wait_message_range(Alice, 6, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).


pagination_before10(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"before10">>, RSM)),
        wait_message_range(Alice, 5, 9),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_simple_before10(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config), simple=true},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"before10">>, RSM)),
     %% wait_message_range(Client, TotalCount,    Offset, FromN, ToN),
        wait_message_range(Alice,   undefined, undefined,     5,   9),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_after10(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction='after', id=message_id(10, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"after10">>, RSM)),
        wait_message_range(Alice, 11, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

%% Select first page of recent messages after last known id.
%% Paginating from newest messages to oldest ones.
pagination_last_after_id5(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5 after 5-th message.
        RSM = #rsm_in{max=5, direction='before',
                after_id=message_id(5, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"last_after_id5">>, RSM)),
     %% wait_message_range(Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(Alice,          10,      5,    11,  15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

%% Select second page of recent messages after last known id.
pagination_last_after_id5_before_id11(Config) ->
    F = fun(Alice) ->
        RSM = #rsm_in{max=5, direction='before',
                after_id=message_id(5, Config),
                before_id=message_id(11, Config)},
        rsm_send(Config, Alice,
            stanza_page_archive_request(<<"last_after_id5_before_id11">>, RSM)),
     %% wait_message_range(Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(Alice,           5,      0,     6,  10),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

generate_message_text(N) when is_integer(N) ->
    <<"Message #", (list_to_binary(integer_to_list(N)))/binary>>.

rsm_send(Config, User, Packet) ->
    case ?config(with_rsm, Config) of
        true ->
            BWithJID = nick_to_jid(bob, Config),
            rsm_send_1(Config, User, add_with_jid(BWithJID, Packet));
        _ ->
            rsm_send_1(Config, User, Packet)
    end.

rsm_send_1(Config, User, Packet) ->
    case ?config(muc_rsm, Config) of
        true ->
            Room = ?config(room, Config),
            escalus:send(User, stanza_to_room(Packet, Room));
        _ ->
            escalus:send(User, Packet)
    end.

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
                                                     [<<"montague@montague.net">>])),
        ReplySet = escalus:wait_for_stanza(Alice),

        escalus:send(Alice, stanza_prefs_get_request()),
        ReplyGet = escalus:wait_for_stanza(Alice),

        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assert_equal(ResultIQ1, ResultIQ2),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

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
                                                      {xmlcdata, <<"\n">>}, %% Put as it is
                                                      <<"montague@montague.net">>], [])),
        ReplySet = escalus:wait_for_stanza(Alice),

        escalus:send(Alice, stanza_prefs_get_request()),
        ReplyGet = escalus:wait_for_stanza(Alice),

        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assert_equal(ResultIQ1, ResultIQ2),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

mam_service_discovery(Config) ->
    F = fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:disco_info(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        try
        escalus:assert(is_iq_result, Stanza),
        escalus:assert(has_feature, [mam_ns_binary()], Stanza),
        ok
        catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            ct:pal("Stanza ~p.", [Stanza]),
            erlang:raise(Class, Reason, Stacktrace)
        end
        end,
    escalus:story(Config, [{alice, 1}], F).

%% Check, that MUC is supported.
muc_service_discovery(Config) ->
    F = fun(Alice) ->
        Domain = escalus_config:get_config(ejabberd_domain, Config),
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [muc_host()], Stanza),
        escalus:assert(is_stanza_from, [Domain], Stanza),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

iq_spoofing(Config) ->
    F = fun(Alice, Bob) ->
        %% Sending iqs between clients is allowed.
        %% Every client MUST check "from" and "id" attributes.
        %% This test checks, that server assign corrent "from" attribute
        %% when it is not specified.
        To = escalus_utils:get_jid(Alice),
        From = escalus_utils:get_jid(Bob),
        escalus:send(Bob, escalus_stanza:to(result_iq(), To)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus_assert:is_stanza_from(From, Stanza),
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

result_iq() ->
    #xmlel{
        name = <<"iq">>,
        attrs = [{<<"id">>,<<"xxx">>}, {<<"type">>,<<"result">>}],
        children = [#xmlel{name = <<"query">>}]}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

nick(User) -> escalus_utils:get_username(User).

mam_ns_binary() -> <<"urn:xmpp:mam:tmp">>.
muc_ns_binary() -> <<"http://jabber.org/protocol/muc">>.

stanza_purge_single_message(MessId) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"purge">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}, {<<"id">>, MessId}]
    }]).

stanza_purge_multiple_messages(BStart, BEnd, BWithJID) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"purge">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}],
       children = skip_undefined([
           maybe_start_elem(BStart),
           maybe_end_elem(BEnd),
           maybe_with_elem(BWithJID)])
    }]).


skip_undefined(Xs) ->
    [X || X <- Xs, X =/= undefined].

maybe_attr(_, undefined) ->
    [];
maybe_attr(K, V) ->
    [{K, V}].

mam_ns_attr() ->
    [{<<"xmlns">>,mam_ns_binary()}].

maybe_start_elem(undefined) ->
    undefined;
maybe_start_elem(BStart) ->
    #xmlel{
        name = <<"start">>,
        children = #xmlcdata{content = BStart}}.

maybe_end_elem(undefined) ->
    undefined;
maybe_end_elem(BEnd) ->
    #xmlel{
        name = <<"end">>,
        children = #xmlcdata{content = BEnd}}.

maybe_with_elem(undefined) ->
    undefined;
maybe_with_elem(BWithJID) ->
    #xmlel{
        name = <<"with">>,
        children = #xmlcdata{content = BWithJID}}.

%% An optional 'queryid' attribute allows the client to match results to
%% a certain query.
stanza_archive_request(QueryId) ->
    stanza_lookup_messages_iq(QueryId,
                              undefined, undefined,
                              undefined, undefined).

stanza_date_range_archive_request() ->
    stanza_lookup_messages_iq(undefined,
                              "2010-06-07T00:00:00Z", "2010-07-07T13:23:54Z",
                              undefined, undefined).

stanza_date_range_archive_request_not_empty(Start, Stop) ->
    stanza_lookup_messages_iq(undefined,
                              Start, Stop,
                              undefined, undefined).

stanza_limit_archive_request() ->
    stanza_lookup_messages_iq(undefined, "2010-08-07T00:00:00Z",
                              undefined, undefined, #rsm_in{max=10}).

stanza_page_archive_request(QueryId, RSM) ->
    stanza_lookup_messages_iq(QueryId, undefined, undefined, undefined, RSM).

stanza_filtered_by_jid_request(BWithJID) ->
    stanza_lookup_messages_iq(undefined, undefined,
                              undefined, BWithJID, undefined).

stanza_lookup_messages_iq(QueryId, BStart, BEnd, BWithJID, RSM) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"query">>,
       attrs = mam_ns_attr()
            ++ maybe_attr(<<"queryid">>, QueryId)
            ++ border_attributes(RSM),
       children = skip_undefined([
           maybe_simple_elem(RSM),
           maybe_opt_count_elem(RSM),
           maybe_start_elem(BStart),
           maybe_end_elem(BEnd),
           maybe_with_elem(BWithJID),
           maybe_rsm_elem(RSM)])
    }]).

maybe_simple_elem(#rsm_in{simple=true}) ->
    [#xmlel{name = <<"simple">>}];
maybe_simple_elem(_) ->
    [].

maybe_opt_count_elem(#rsm_in{opt_count=true}) ->
    [#xmlel{name = <<"opt_count">>}];
maybe_opt_count_elem(_) ->
    [].

border_attributes(undefined) ->
    [];
border_attributes(#rsm_in{
        before_id=BeforeId, after_id=AfterId, from_id=FromId, to_id=ToId}) ->
    maybe_attr(<<"before_id">>, BeforeId)
    ++ maybe_attr(<<"after_id">>, AfterId)
    ++ maybe_attr(<<"from_id">>, FromId)
    ++ maybe_attr(<<"to_id">>, ToId).

maybe_rsm_elem(undefined) ->
    undefined;
maybe_rsm_elem(#rsm_in{max=Max, direction=Direction, id=Id, index=Index}) ->
    #xmlel{name = <<"set">>,
           children = skip_undefined([
                maybe_rsm_max(Max),
                maybe_rsm_index(Index),
                maybe_rsm_direction(Direction, Id)])}.

maybe_rsm_id(undefined) -> [];
maybe_rsm_id(Id) -> #xmlcdata{content = Id}.

maybe_rsm_direction(undefined, undefined) ->
    undefined;
maybe_rsm_direction(Direction, Id) ->
    #xmlel{
        name = atom_to_binary(Direction, latin1),
        children = maybe_rsm_id(Id)}.

maybe_rsm_index(undefined) ->
    undefined;
maybe_rsm_index(Index) when is_integer(Index) ->
    #xmlel{
        name = <<"index">>,
        children = #xmlcdata{content = integer_to_list(Index)}}.

maybe_rsm_max(undefined) ->
    undefined;
maybe_rsm_max(Max) when is_integer(Max) ->
    #xmlel{
        name = <<"max">>,
        children = #xmlcdata{content = integer_to_list(Max)}}.

add_with_jid(BWithJID,
    IQ=#xmlel{children=[
        Query=#xmlel{children=QueryChildren}]}) ->
    IQ#xmlel{children=[
        Query#xmlel{children=[maybe_with_elem(BWithJID) | QueryChildren]}]}.

assert_only_one_of_many_is_equal(Archived, Sent) ->
    Scanned = lists:map(fun parse_forwarded_message/1, Archived),
    Same = lists:filter(fun (Stanza) -> is_same_message_text(Stanza, Sent) end, Scanned),
    ?assert_equal(1, erlang:length(Same)).

is_same_message_text(Stanza, Raw) ->
    #forwarded_message{message_body = A} = Stanza,
    A =:= Raw.

%% ----------------------------------------------------------------------
%% PREFERENCE QUERIES

stanza_prefs_set_request(DefaultMode, AlwaysJIDs, NewerJIDs) ->
    AlwaysEl = #xmlel{name = <<"always">>,
                      children = encode_jids(AlwaysJIDs)},
    NewerEl  = #xmlel{name = <<"never">>,
                      children = encode_jids(NewerJIDs)},
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"prefs">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}]
               ++ [{<<"default">>, DefaultMode} || is_def(DefaultMode)],
       children = [AlwaysEl, NewerEl]
    }]).

stanza_prefs_get_request() ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"prefs">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}]
    }]).

%% Allows to cdata to be put as it is
encode_jids(JIDs) ->
    [encode_jid_or_cdata(JID) || JID <- JIDs].

encode_jid_or_cdata({xmlcdata, Text}) ->
    {xmlcdata, Text};
encode_jid_or_cdata(JID) ->
    #xmlel{name = <<"jid">>,
           children = [#xmlcdata{content = JID}]}.

%% ----------------------------------------------------------------------
%% PARSING RESPONDS

parse_forwarded_message(#xmlel{name = <<"message">>,
                               attrs = Attrs, children = Children}) ->
    M = #forwarded_message{
        from = proplists:get_value(<<"from">>, Attrs),
        to   = proplists:get_value(<<"to">>, Attrs)},
    lists:foldl(fun 'parse_children[message]'/2, M, Children).

'parse_children[message]'(#xmlel{name = <<"result">>,
                                 attrs = Attrs,
                                 children = Children}, M) ->
    M1 = M#forwarded_message{
        result_queryid = proplists:get_value(<<"queryid">>, Attrs),
        result_id      = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun 'parse_children[message/result]'/2, M1, Children).

'parse_children[message/result]'(#xmlel{name = <<"forwarded">>,
                                        children = Children}, M) ->
    lists:foldl(fun 'parse_children[message/result/forwarded]'/2, M, Children).


'parse_children[message/result/forwarded]'(#xmlel{name = <<"delay">>,
                                                  attrs = Attrs}, M) ->
    M#forwarded_message{
        delay_from  = proplists:get_value(<<"from">>, Attrs),
        delay_stamp = proplists:get_value(<<"stamp">>, Attrs)};
'parse_children[message/result/forwarded]'(#xmlel{name = <<"message">>,
                                                  attrs = Attrs,
                                                  children = Children}, M) ->
    M1 = M#forwarded_message{
        message_to   = proplists:get_value(<<"to">>, Attrs),
        message_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun 'parse_children[message/result/forwarded/message]'/2,
                M1, Children).

'parse_children[message/result/forwarded/message]'(#xmlel{name = <<"body">>,
        children = [{xmlcdata, Body}]}, M) ->
    M#forwarded_message{message_body = Body};
%% Parse `<archived />' here.
'parse_children[message/result/forwarded/message]'(_, M) ->
    M.

%% Num is 1-based.
message_id(Num, Config) ->
    AllMessages = proplists:get_value(all_messages, Config),
    #forwarded_message{result_id=Id} = lists:nth(Num, AllMessages),
    Id.


%% @doc Result query iq.
%%
%% [{xmlel,<<"iq">>,
%%     [{<<"from">>,<<"alice@localhost">>},
%%      {<<"to">>,<<"alice@localhost/res1">>},
%%      {<<"id">>,<<"387862024ce65379b049e19751e4309e">>},
%%      {<<"type">>,<<"result">>}],
%%     []}]
%%
%%
%%  [{xmlel,<<"iq">>,
%%       [{<<"from">>,<<"alice@localhost">>},
%%        {<<"to">>,<<"alice@localhost/res1">>},
%%        {<<"id">>,<<"c256a18c4b720465e215a81362d41eb7">>},
%%        {<<"type">>,<<"result">>}],
%%       [{xmlel,<<"query">>,
%%            [{<<"xmlns">>,<<"urn:xmpp:mam:tmp">>}],
%%            [{xmlel,<<"set">>,
%%                 [{<<"xmlns">>,<<"http://jabber.org/protocol/rsm">>}],
%%                 [{xmlel,<<"first">>,
%%                      [{<<"index">>,<<"10">>}],
%%                      [{xmlcdata,<<"103439">>}]},
%%                  {xmlel,<<"last">>,[],[{xmlcdata,<<"103447">>}]},
%%                  {xmlel,<<"count">>,[],[{xmlcdata,<<"15">>}]}]}]}]}]
parse_result_iq(#xmlel{name = <<"iq">>,
                       attrs = Attrs, children = Children}) ->
    IQ = #result_iq{
        from = proplists:get_value(<<"from">>, Attrs),
        to   = proplists:get_value(<<"to">>, Attrs),
        id   = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun 'parse_children[iq]'/2, IQ, Children).

'parse_children[iq]'(#xmlel{name = <<"query">>, children = Children},
                     IQ) ->
    lists:foldl(fun 'parse_children[iq/query]'/2, IQ, Children).


'parse_children[iq/query]'(#xmlel{name = <<"set">>,
                                  children = Children},
                           IQ) ->
    lists:foldl(fun 'parse_children[iq/query/set]'/2, IQ, Children).

'parse_children[iq/query/set]'(#xmlel{name = <<"first">>,
                                      attrs = Attrs,
                                      children = [{xmlcdata, First}]},
                               IQ) ->
    Index = case proplists:get_value(<<"index">>, Attrs) of
                undefined -> undefined;
                X -> list_to_integer(binary_to_list(X))
            end,
    IQ#result_iq{first_index = Index, first = First};
'parse_children[iq/query/set]'(#xmlel{name = <<"last">>,
                                      children = [{xmlcdata, Last}]},
                               IQ) ->
    IQ#result_iq{last = Last};
'parse_children[iq/query/set]'(#xmlel{name = <<"count">>,
                                      children = [{xmlcdata, Count}]},
                               IQ) ->
    IQ#result_iq{count = list_to_integer(binary_to_list(Count))};
'parse_children[iq/query/set]'(_, IQ) -> IQ.

is_def(X) -> X =/= undefined.



parse_prefs_result_iq(#xmlel{name = <<"iq">>, children = Children}) ->
    IQ = #prefs_result_iq{},
    lists:foldl(fun 'parse_children[prefs_iq]'/2, IQ, Children).

'parse_children[prefs_iq]'(#xmlel{name = <<"prefs">>,
                                  attrs = Attrs, children = Children},
                           IQ) ->
    DefaultMode = proplists:get_value(<<"default">>, Attrs),
    IQ1 = IQ#prefs_result_iq{default_mode = DefaultMode},
    lists:foldl(fun 'parse_children[prefs_iq/prefs]'/2, IQ1, Children).


'parse_children[prefs_iq/prefs]'(#xmlel{name = <<"always">>,
                                        children = Children},
                                 IQ) ->
    IQ#prefs_result_iq{always_jids = lists:sort(parse_jids(Children))};
'parse_children[prefs_iq/prefs]'(#xmlel{name = <<"never">>,
                                        children = Children},
                                 IQ) ->
    IQ#prefs_result_iq{never_jids = lists:sort(parse_jids(Children))}.


parse_jids(Els) ->
    [JID || #xmlel{name = <<"jid">>, children = [{xmlcdata, JID}]} <- Els].

%% <iq type='error' id='q29302'>
%%   <error type='modify'>
%%     <policy-violation xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
%%     <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Too many results</text>
%%   </error>
%% </iq>
parse_error_iq(#xmlel{name = <<"iq">>,
                      attrs = Attrs, children = Children}) ->

    IQ = #error_iq{
        type = proplists:get_value(<<"type">>, Attrs),
        id   = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun 'parse_children[error_iq]'/2, IQ, Children).


'parse_children[error_iq]'(#xmlel{name = <<"error">>,
                                  attrs = Attrs, children = Children}, IQ) ->
    IQ1 = IQ#error_iq{
        error_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun 'parse_children[error_iq/error]'/2, IQ1, Children);
'parse_children[error_iq]'(_, IQ) ->
    IQ.

'parse_children[error_iq/error]'(#xmlel{name = <<"text">>,
                                 children = [{xmlcdata, Text}]}, IQ) ->
    IQ#error_iq{text = Text};
'parse_children[error_iq/error]'(#xmlel{name = Condition}, IQ) ->
    IQ#error_iq{condition = Condition};
'parse_children[error_iq/error]'(_, IQ) ->
    IQ.

%%--------------------------------------------------------------------
%% Helpers (muc)
%%--------------------------------------------------------------------

generate_rpc_jid({_,User}) ->
    {username, Username} = lists:keyfind(username, 1, User),
    {server, Server} = lists:keyfind(server, 1, User),
    LUsername = escalus_utils:jid_to_lower(Username),
    LServer = escalus_utils:jid_to_lower(Server),
    {jid, Username, Server, <<"rpc">>, LUsername, LServer, <<"rpc">>}.

start_alice_room(Config) ->
    %% TODO: ensure, that the room's archive is empty
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    [Alice | _] = ?config(escalus_users, Config),
    start_room(Config, Alice, RoomName, RoomNick, [{persistent, true}]).

start_room(Config, User, Room, Nick, Opts) ->
    From = generate_rpc_jid(User),
    rpc_apply(mod_muc, create_instant_room,
        [<<"localhost">>, Room, From, Nick, Opts]),
    [{nick, Nick}, {room, Room} | Config].

destroy_room(Config) ->
    RoomName = ?config(room, Config),
    delete_room_archive(muc_host(), RoomName),
    case rpc_apply(ets, lookup, [muc_online_room, {RoomName, muc_host()}]) of
        [{_,_,Pid}|_] -> gen_fsm:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

stanza_muc_enter_room(Room, Nick) ->
    Elem = #xmlel{ name = <<"x">>,
                   attrs=[{<<"xmlns">>, muc_ns_binary()}]},
    stanza_to_room(escalus_stanza:presence(<<"available">>, [Elem]),
                   Room, Nick).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

room_address(Room) when is_binary(Room) ->
    <<Room/binary, "@", (muc_host())/binary>>.

room_address(Room, Nick) when is_binary(Room), is_binary(Nick) ->
    <<Room/binary, "@", (muc_host())/binary, "/", Nick/binary>>.


send_muc_rsm_messages(Config) ->
    Pid = self(),
    Room = ?config(room, Config),
    RoomAddr = room_address(Room),
    F = fun(Alice, Bob) ->
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Alice, 3),

        %% Alice sends messages to Bob.
        [escalus:send(Alice,
                      escalus_stanza:groupchat_to(RoomAddr, generate_message_text(N)))
         || N <- lists:seq(1, 15)],
        %% Bob is waiting for 15 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 15, 5000),
        escalus:wait_for_stanzas(Alice, 15, 5000),
        %% Get whole history.
        escalus:send(Alice,
            stanza_to_room(stanza_archive_request(<<"all_room_messages">>), Room)),
        [_ArcIQ|AllMessages] =
            assert_respond_size(15, wait_archive_respond_iq_first(Alice)),
        ParsedMessages = [parse_forwarded_message(M) || M <- AllMessages],
        Pid ! {parsed_messages, ParsedMessages},
        ok
        end,
    Config1 = escalus:init_per_testcase(pre_rsm, Config),
    escalus:story(Config1, [{alice, 1}, {bob, 1}], F),
    ParsedMessages = receive {parsed_messages, PM} -> PM
                     after 5000 -> error(receive_timeout) end,

    escalus:end_per_testcase(pre_rsm, Config1),
    [{all_messages, ParsedMessages}|Config].

send_rsm_messages(Config) ->
    Pid = self(),
    Room = ?config(room, Config),
    F = fun(Alice, Bob) ->
        %% Alice sends messages to Bob.
        [escalus:send(Alice,
                      escalus_stanza:chat_to(Bob, generate_message_text(N)))
         || N <- lists:seq(1, 15)],
        %% Bob is waiting for 15 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 15, 5000),
        maybe_wait_for_yz(Config),
        %% Get whole history.
        rsm_send(Config, Alice, stanza_archive_request(<<"all_messages">>)),
        [_ArcIQ|AllMessages] =
            assert_respond_size(15, wait_archive_respond_iq_first(Alice)),
        ParsedMessages = [parse_forwarded_message(M) || M <- AllMessages],
        Pid ! {parsed_messages, ParsedMessages},
        ok
        end,
    Config1 = escalus:init_per_testcase(pre_rsm, Config),
    escalus:story(Config1, [{alice, 1}, {bob, 1}], F),
    ParsedMessages = receive {parsed_messages, PM} -> PM
                     after 5000 -> error(receive_timeout) end,

    escalus:end_per_testcase(pre_rsm, Config1),
    [{all_messages, ParsedMessages}|Config].

append_subelem(Elem=#xmlel{children=Cs}, SubElem) ->
    Elem#xmlel{children=Cs ++ [SubElem]}.

archived_elem(By, Id) ->
    #xmlel{name = <<"archived">>,
           attrs = [{<<"by">>, By}, {<<"id">>, Id}]}.

clean_archives(Config) ->
    SUs = serv_users(Config),
    %% It is not the best place to delete these messages.
    [ok = delete_offline_messages(S, U) || {S, U} <- SUs],
    [ok = delete_archive(S, U) || {S, U} <- SUs],
    timer:sleep(1500),
    [assert_empty_archive(S, U) || {S, U} <- SUs],
    Config.

clean_room_archive(Config) ->
    Room = ?config(room, Config),
    delete_room_archive(muc_host(), Room),
    timer:sleep(500),
    assert_empty_room_archive(muc_host(), Room),
    Config.

serv_users(Config) ->
    [serv_user(Config, UserSpec)
     || {_, UserSpec} <- escalus_users:get_users(all)].

serv_user(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    {Server, Username}.

%% @doc Check, that the archive is empty.
assert_empty_archive(Server, Username) ->
    case archive_size(Server, Username) of
       0 -> ok;
       X -> ct:fail({not_empty, Server, Username, X})
    end.

%% @doc Check, that the archive is empty.
assert_empty_room_archive(Server, Username) ->
    case room_archive_size(Server, Username) of
       0 -> ok;
       X -> ct:fail({not_empty, Server, Username, X})
    end.


archive_size(Server, Username) ->
    rpc_apply(mod_mam, archive_size, [Server, Username]).

room_archive_size(Server, Username) ->
    rpc_apply(mod_mam_muc, archive_size, [Server, Username]).

delete_archive(Server, Username) ->
    rpc_apply(mod_mam, delete_archive, [Server, Username]).

delete_room_archive(Server, Username) ->
    rpc_apply(mod_mam_muc, delete_archive, [Server, Username]).

delete_offline_messages(Server, Username) ->
    %% Do not care
    catch rpc_apply(mod_offline, remove_user, [Username, Server]),
    ok.

wait_message_range(Client, FromN, ToN) ->
    wait_message_range(Client, 15, FromN-1, FromN, ToN).

wait_message_range(Client, TotalCount, Offset, FromN, ToN) ->
    [IQ|Messages] = wait_archive_respond_iq_first(Client),
    ParsedMessages = parse_messages(Messages),
    ParsedIQ = parse_result_iq(IQ),
    try
        ?assert_equal(TotalCount, ParsedIQ#result_iq.count),
        ?assert_equal(Offset, ParsedIQ#result_iq.first_index),
        %% Compare body of the messages.
        ?assert_equal([generate_message_text(N) || N <- lists:seq(FromN, ToN)],
                      [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("IQ: ~p~n"
               "Messages: ~p~n"
               "Parsed messages: ~p~n",
               [IQ, Messages, ParsedMessages]),
        erlang:raise(Class, Reason, Stacktrace)
    end.


wait_empty_rset(Alice, TotalCount) ->
    [IQ] = wait_archive_respond_iq_first(Alice),
    ParsedIQ = parse_result_iq(IQ),
    try
        ?assert_equal(TotalCount, ParsedIQ#result_iq.count),
        ok
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("IQ: ~p~n", [IQ]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

parse_messages(Messages) ->
    try [parse_forwarded_message(M) || M <- Messages]
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("Messages: ~p~n", [Messages]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

bootstrap_archive(Config) ->
    random:seed(now()),
    Domain = escalus_ct:get_config(ejabberd_domain),
    ArcJID = {<<"alice@",Domain/binary>>, make_jid(<<"alice">> ,Domain, <<>>),
             rpc_apply(mod_mam, archive_id, [Domain, <<"alice">>])},
    OtherUsers = [{<<"bob@",Domain/binary>>, make_jid(<<"bob">>, Domain, <<>>),
                   rpc_apply(mod_mam, archive_id, [Domain, <<"bob">>])},
                  {<<"carol@",Domain/binary>>, make_jid(<<"carol">>, Domain, <<>>),
                   rpc_apply(mod_mam, archive_id, [Domain, <<"carol">>])}],
    Msgs = generate_msgs_for_days(ArcJID, OtherUsers, 16),
    put_msgs(Msgs),
    timer:sleep(1500),
    [{pre_generated_msgs, sort_msgs(Msgs)} | Config].

sort_msgs(Msgs) ->
    SortFun = fun({{ID1, _}, _, _, _, _}, {{ID2, _}, _, _, _, _}) ->
        ID1 =< ID2
    end,
    lists:sort(SortFun,Msgs).

generate_msgs_for_days(OwnerJID, OtherUsers, Days) ->
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    StartDay = Today - Days,
    lists:flatten([generate_msgs_for_day(Day, OwnerJID, OtherUsers)
                   || Day <- lists:seq(StartDay, Today)]).

generate_msgs_for_day(Day, OwnerJID, OtherUsers) ->
    Date = calendar:gregorian_days_to_date(Day),

    [generate_msg_for_date_user(OwnerJID, RemoteJID, {Date, random_time()})
     || RemoteJID <- OtherUsers].

generate_msg_for_date_user(Owner, {RemoteBin, _, _} = Remote, DateTime) ->
    MicrosecDateTime = datetime_to_microseconds(DateTime),
    NowMicro = rpc_apply(mod_mam_utils, now_to_microseconds, [rpc_apply(erlang, now, [])]),
    Microsec = min(NowMicro, MicrosecDateTime),
    MsgIdOwner = rpc_apply(mod_mam_utils, encode_compact_uuid, [Microsec, random:uniform(20)]),
    MsgIdRemote = rpc_apply(mod_mam_utils, encode_compact_uuid, [Microsec+1, random:uniform(20)]),
    Packet = escalus_stanza:chat_to(RemoteBin, base16:encode(crypto:rand_bytes(4))),
    {{MsgIdOwner, MsgIdRemote}, Owner, Remote, Owner, Packet}.

random_time() ->
    MaxSecondsInDay = 86399,
    RandSeconds = random:uniform(MaxSecondsInDay),
    calendar:seconds_to_time(RandSeconds).

datetime_to_microseconds({{_,_,_}, {_,_,_}} = DateTime) ->
    S1 = calendar:datetime_to_gregorian_seconds(DateTime),
    S0 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = S1 - S0,
    Seconds * 1000000.


put_msgs(Msgs) ->
    [put_msg(Msg) || Msg <- Msgs].

put_msg({{MsgIdOwner, MsgIdRemote},
         {_FromBin, FromJID, FromArcID},
         {_ToBin, ToJID, ToArcID},
         {_, Source, _}, Packet}) ->
    Host = escalus_ct:get_config(ejabberd_domain),
    archive_message([Host, MsgIdOwner, FromArcID, FromJID, ToJID, Source, outgoing, Packet]),
    archive_message([Host, MsgIdRemote, ToArcID, ToJID, FromJID, Source, incoming, Packet]).

archive_message(Args) ->
    rpc_apply(mod_mam, archive_message, Args).



muc_bootstrap_archive(Config) ->
    Room = ?config(room, Config),

    A = room_address(Room, nick(alice)),
    B = room_address(Room, nick(bob)),
    R = room_address(Room),

    Domain = muc_host(),
    Host = host(),
    RoomJid = make_jid(Room,Domain, <<>>),
    ArcJID = {R, RoomJid,
              rpc_apply(mod_mam_muc, archive_id, [Domain, Room])},
    Msgs = generate_msgs_for_days(ArcJID,
                                 [{B, make_jid(<<"bob">>, Host, <<"res1">>),
                                  rpc_apply(jid, replace_resource, [RoomJid, nick(bob)])},
                                  {A, make_jid(<<"alice">>, Host, <<"res1">>),
                                   rpc_apply(jid, replace_resource, [RoomJid, nick(alice)])}], 16),

    put_muc_msgs(Msgs),

    [{pre_generated_muc_msgs, sort_msgs(Msgs)} | Config].

put_muc_msgs(Msgs) ->
    Host = host(),
    [archive_muc_msg(Host, Msg) || Msg <- Msgs].

archive_muc_msg(Host, {{MsgID, _},
                {_RoomBin, RoomJID, RoomArcID},
                {_FromBin, FromJID, SrcJID}, _, Packet}) ->
    rpc_apply(mod_mam_muc, archive_message, [Host, MsgID, RoomArcID, RoomJID,
                                             SrcJID, SrcJID, incoming, Packet]).

%% @doc Get a binary jid of the user, that tagged with `UserName' in the config.
nick_to_jid(UserName, Config) when is_atom(UserName) ->
    UserSpec = escalus_users:get_userspec(Config, UserName),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

make_jid(U, S, R) ->
    rpc_apply(jid, make, [U, S, R]).


is_mam_possible(Host) ->
    is_odbc_enabled(Host) orelse is_riak_enabled(Host).

is_odbc_enabled(Host) ->
    case sql_transaction(Host, fun erlang:now/0) of
        {atomic, _} -> true;
        Other ->
            %ct:pal("ODBC disabled (check failed ~p)", [Other]),
            false
    end.

is_riak_enabled(_Host) ->
    case escalus_ejabberd:rpc(mongoose_riak, get_worker, []) of
        Pid when is_pid(Pid) ->
            true;
        _ ->
            false
    end.

sql_transaction(Host, F) ->
    escalus_ejabberd:rpc(ejabberd_odbc, sql_transaction, [Host, F]).

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.

maybe_wait_for_yz(Config) ->
    case ?config(yz_wait, Config) of
        undefined ->
            ok;
        Value ->
            timer:sleep(Value)
    end.
