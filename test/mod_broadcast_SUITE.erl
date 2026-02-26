%%%-------------------------------------------------------------------
%%% @doc Small test suite for mod_broadcast(_api) defensive/error paths.
%%%
%%% This suite covers error cases that are difficult to reproduce in normal
%%% operation, such as missing RDBMS auth, probe failures, and stop_job errors.
%%% Uses meck to mock dependencies without starting real services.
%%%-------------------------------------------------------------------
-module(mod_broadcast_SUITE).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("mongoose_config_spec.hrl").
-include_lib("mod_broadcast.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    %% sanity
    supported_features_includes_dynamic_domains/1,
    config_spec_includes_backend_rdbms_default/1,
    %% start_error_paths
    start_requires_rdbms_auth_raises_error/1,
    %% probe_error_paths
    probe_live_jobs_noproc_returns_zero/1,
    probe_live_jobs_timeout_returns_zero/1,
    %% abort_broadcast_paths
    abort_broadcast_stop_job_not_live_maps_to_broadcast_not_found/1,
    abort_broadcast_stop_job_nodedown_maps_to_cannot_reach_job_owner_node/1,
    abort_broadcast_stop_job_noproc_maps_to_cannot_reach_job_owner_node/1,
    %% api_domain_not_found_paths
    start_broadcast_domain_not_found/1,
    get_broadcast_domain_not_found/1,
    get_broadcasts_domain_not_found/1,
    abort_broadcast_domain_not_found/1,
    delete_inactive_broadcasts_by_ids_domain_not_found/1,
    delete_inactive_broadcasts_by_domain_domain_not_found/1
]).

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [{group, sanity},
     {group, start_error_paths},
     {group, probe_error_paths},
     {group, abort_broadcast_paths},
     {group, api_domain_not_found_paths}].

groups() ->
    [{sanity, [], sanity_tests()},
     {start_error_paths, [], start_error_path_tests()},
     {probe_error_paths, [], probe_error_path_tests()},
     {abort_broadcast_paths, [], abort_broadcast_path_tests()},
     {api_domain_not_found_paths, [], api_domain_not_found_path_tests()}].

sanity_tests() ->
    [supported_features_includes_dynamic_domains,
     config_spec_includes_backend_rdbms_default].

start_error_path_tests() ->
    [start_requires_rdbms_auth_raises_error].

probe_error_path_tests() ->
    [probe_live_jobs_noproc_returns_zero,
     probe_live_jobs_timeout_returns_zero].

abort_broadcast_path_tests() ->
    [abort_broadcast_stop_job_not_live_maps_to_broadcast_not_found,
     abort_broadcast_stop_job_nodedown_maps_to_cannot_reach_job_owner_node,
     abort_broadcast_stop_job_noproc_maps_to_cannot_reach_job_owner_node].

api_domain_not_found_path_tests() ->
    [start_broadcast_domain_not_found,
     get_broadcast_domain_not_found,
     get_broadcasts_domain_not_found,
     abort_broadcast_domain_not_found,
     delete_inactive_broadcasts_by_ids_domain_not_found,
     delete_inactive_broadcasts_by_domain_domain_not_found].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Group, Config) ->
    setup_mocks(),
    set_domain_api_expectation(Group),
    Config.

end_per_group(_Group, _Config) ->
    teardown_mocks(),
    ok.

init_per_testcase(_TestCase, Config) ->
    reset_meck_defaults(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%====================================================================
%% Sanity tests
%%====================================================================

supported_features_includes_dynamic_domains(_Config) ->
    Features = mod_broadcast:supported_features(),
    true = lists:member(dynamic_domains, Features).

config_spec_includes_backend_rdbms_default(_Config) ->
    #section{items = Items, defaults = #{<<"backend">> := rdbms}} = mod_broadcast:config_spec(),
    #option{type = atom, validate = {enum, Enum}} = maps:get(<<"backend">>, Items),
    true = lists:member(rdbms, Enum).

%%====================================================================
%% start/2 error path tests
%%====================================================================

start_requires_rdbms_auth_raises_error(_Config) ->
    HostType = broadcast_helper:host_type(),
    meck:expect(ejabberd_auth, auth_modules_for_host_type,
                fun(_HostType) -> [] end),
    ?assertException(error,
                     #{what := mod_broadcast_requires_rdbms_auth,
                       host_type := HostType},
                     mod_broadcast:start(HostType, #{})).

%%====================================================================
%% probe/2 error path tests
%%====================================================================

probe_live_jobs_noproc_returns_zero(_Config) ->
    meck:expect(broadcast_manager, get_live_job_count,
                fun(_HostType) -> exit({noproc, mock}) end),
    #{count := 0} = mod_broadcast:probe(mod_broadcast_live_jobs, #{host_type => broadcast_helper:host_type()}).

probe_live_jobs_timeout_returns_zero(_Config) ->
    meck:expect(broadcast_manager, get_live_job_count,
                fun(_HostType) -> exit({timeout, mock}) end),
    #{count := 0} = mod_broadcast:probe(mod_broadcast_live_jobs, #{host_type => broadcast_helper:host_type()}).

%%====================================================================
%% abort_broadcast/2 error path tests
%%====================================================================

abort_broadcast_stop_job_not_live_maps_to_broadcast_not_found(_Config) ->
    HostType = broadcast_helper:host_type(),
    Domain = broadcast_helper:domain(),
    JobId = broadcast_helper:job_id(),
    Job = broadcast_helper:sample_broadcast_job(),

    meck:expect(mongoose_domain_api, get_host_type,
                fun(_Domain) -> {ok, HostType} end),
    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _Id) -> {ok, Job} end),
    meck:expect(broadcast_manager, stop_job,
                fun(_Node, _HostType, _Id) -> {error, not_live} end),

    {broadcast_not_found, _} = mod_broadcast_api:abort_broadcast(Domain, JobId).

abort_broadcast_stop_job_nodedown_maps_to_cannot_reach_job_owner_node(_Config) ->
    HostType = broadcast_helper:host_type(),
    Domain = broadcast_helper:domain(),
    JobId = broadcast_helper:job_id(),
    FakeNode = 'nonexistent@nonexistent',
    Job = (broadcast_helper:sample_broadcast_job())#broadcast_job{owner_node = FakeNode},

    meck:expect(mongoose_domain_api, get_host_type,
                fun(_Domain) -> {ok, HostType} end),
    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _Id) -> {ok, Job} end),
    meck:expect(broadcast_manager, stop_job,
                fun(Node, HT, Id) -> meck:passthrough([Node, HT, Id]) end),

    {cannot_reach_job_owner_node, _} = mod_broadcast_api:abort_broadcast(Domain, JobId).

abort_broadcast_stop_job_noproc_maps_to_cannot_reach_job_owner_node(_Config) ->
    HostType = broadcast_helper:host_type(),
    Domain = broadcast_helper:domain(),
    JobId = broadcast_helper:job_id(),
    Job = (broadcast_helper:sample_broadcast_job())#broadcast_job{owner_node = node()},

    meck:expect(mongoose_domain_api, get_host_type,
                fun(_Domain) -> {ok, HostType} end),
    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _Id) -> {ok, Job} end),
    meck:expect(broadcast_manager, stop_job,
                fun(Node, HT, Id) -> meck:passthrough([Node, HT, Id]) end),

    %% No broadcast_manager process is running for HostType on this node;
    %% broadcast_manager:stop_job/3 will call gen_server:call({ProcName, node()}, ...)
    %% which exits with {noproc, _} because ProcName is not registered.
    {cannot_reach_job_owner_node, _} = mod_broadcast_api:abort_broadcast(Domain, JobId).

%%====================================================================
%% API domain_not_found path tests
%%====================================================================

start_broadcast_domain_not_found(_Config) ->
    JobSpec = broadcast_helper:valid_job_spec(),
    {domain_not_found, _} = mod_broadcast_api:start_broadcast(JobSpec).

get_broadcast_domain_not_found(_Config) ->
    {domain_not_found, _} = mod_broadcast_api:get_broadcast(broadcast_helper:domain(),
                                                            broadcast_helper:job_id()).

get_broadcasts_domain_not_found(_Config) ->
    {domain_not_found, _} = mod_broadcast_api:get_broadcasts(broadcast_helper:domain(), 10, 0).

abort_broadcast_domain_not_found(_Config) ->
    {domain_not_found, _} = mod_broadcast_api:abort_broadcast(broadcast_helper:domain(),
                                                              broadcast_helper:job_id()).

delete_inactive_broadcasts_by_ids_domain_not_found(_Config) ->
    {domain_not_found, _} = mod_broadcast_api:delete_inactive_broadcasts_by_ids(
                               broadcast_helper:domain(),
                               [broadcast_helper:job_id()]).

delete_inactive_broadcasts_by_domain_domain_not_found(_Config) ->
    {domain_not_found, _} = mod_broadcast_api:delete_inactive_broadcasts_by_domain(
                               broadcast_helper:domain()).

%%====================================================================
%% Helper functions
%%====================================================================

setup_mocks() ->
    meck:new(ejabberd_auth, [no_link]),
    meck:new(mongoose_domain_api, [no_link]),
    meck:new(mod_broadcast_backend, [no_link]),
    meck:new(broadcast_manager, [no_link]),
    ok.

reset_meck_defaults() ->
    meck:expect(ejabberd_auth, auth_modules_for_host_type,
                fun(_HostType) -> [ejabberd_auth_rdbms] end),
    meck:expect(mod_broadcast_backend, get_job,
                fun(_HostType, _Id) -> {error, not_found} end),
    meck:expect(broadcast_manager, stop_job,
                fun(_Node, _HostType, _Id) -> ok end),
    meck:expect(broadcast_manager, get_live_job_count,
                fun(_HostType) -> 0 end),
    ok.

teardown_mocks() ->
    catch meck:unload(ejabberd_auth),
    catch meck:unload(mongoose_domain_api),
    catch meck:unload(mod_broadcast_backend),
    catch meck:unload(broadcast_manager),
    ok.

set_domain_api_expectation(api_domain_not_found_paths) ->
    meck:expect(mongoose_domain_api, get_host_type,
                fun(_Domain) -> {error, not_found} end);
set_domain_api_expectation(_Other) ->
    meck:expect(mongoose_domain_api, get_host_type,
                fun(_Domain) -> {ok, broadcast_helper:host_type()} end).
