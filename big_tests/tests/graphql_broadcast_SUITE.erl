-module(graphql_broadcast_SUITE).

-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% admin_mutation group
-export([admin_start_broadcast_success/1,
         admin_start_broadcast_validation_error/1,
         admin_start_broadcast_coercion_error/1,
         admin_start_broadcast_sender_not_found/1,
         admin_abort_broadcast/1,
         admin_delete_inactive_broadcasts/1,
         admin_delete_inactive_broadcasts_by_domain/1]).

%% admin_query group
-export([admin_get_broadcasts_pagination/1,
         admin_get_broadcast/1,
         admin_get_broadcast_finished/1,
         admin_get_broadcast_worker_killed/1]).

%% domain_admin group
-export([domain_admin_start_broadcast_success/1]).

%% protection group
-export([admin_module_gating_checks/1,
         admin_protection_checks/1,
         domain_admin_protection_checks/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").

-import(distributed_helper, [mim/0, rpc/4, require_rpc_nodes/1]).
-import(domain_helper, [domain/0, secondary_domain/0]).
-import(graphql_helper, [execute_command/4,
                         get_ok_value/2,
                         get_err_code/1,
                         get_err_msg/1,
                         get_unauthorized/1,
                         get_not_loaded/1,
                         get_coercion_err_msg/1]).
-import(broadcast_helper, [wait_until_job_state/3]).

%%====================================================================
%% CT callbacks
%%====================================================================

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_mutation},
     {group, admin_query},
     {group, domain_admin},
     {group, protection}].

groups() ->
    [{admin_mutation, [sequence], admin_mutation_test_cases()},
     {admin_query, [sequence], admin_query_test_cases()},
     {domain_admin, [sequence], domain_admin_test_cases()},
     {protection, [sequence], protection_test_cases()}].

admin_mutation_test_cases() ->
    [admin_start_broadcast_success,
     admin_start_broadcast_validation_error,
     admin_start_broadcast_coercion_error,
     admin_start_broadcast_sender_not_found,
    admin_abort_broadcast,
    admin_delete_inactive_broadcasts,
    admin_delete_inactive_broadcasts_by_domain].

admin_query_test_cases() ->
    [admin_get_broadcasts_pagination,
     admin_get_broadcast,
     admin_get_broadcast_finished,
     admin_get_broadcast_worker_killed].

domain_admin_test_cases() ->
    [domain_admin_start_broadcast_success].

protection_test_cases() ->
    [admin_module_gating_checks,
     admin_protection_checks,
     domain_admin_protection_checks].

init_per_suite(Config) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true ->
            Config1 = dynamic_modules:save_modules(domain_helper:host_type(), Config),
            Config2 = escalus:create_users(Config1, escalus:get_users([alice])),
            broadcast_helper:create_many_users(10),
            ensure_mod_broadcast_started(domain()),
            ensure_mod_broadcast_started(secondary_domain()),
            escalus:init_per_suite(Config2);
        false ->
            {skip, require_rdbms}
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    broadcast_helper:delete_many_users(10),
    escalus:delete_users(Config, escalus:get_users([alice])),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(Group, Config) when Group == protection; Group == domain_admin ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(_Group, Config) ->
    graphql_helper:init_admin_handler(Config).

end_per_group(_Group, _Config) ->
    graphql_helper:clean().

init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    broadcast_helper:clean_broadcast_jobs(),
    escalus:end_per_testcase(TestCase, Config).

%%====================================================================
%% Admin Mutation Tests
%%====================================================================

admin_start_broadcast_success(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    Res = start_broadcast_op(domain(), Sender, ?FUNCTION_NAME, Config),

    [_] = get_ok_value([data, broadcast, startBroadcast, ids], Res),
    <<_, _/binary>> = get_ok_value([data, broadcast, startBroadcast, message], Res).

admin_start_broadcast_validation_error(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    SlowJob = broadcast_helper:slow_job_spec(domain(), Sender, ?FUNCTION_NAME),
    Vars = job_spec_to_vars(SlowJob#{message_rate => 2000}),
    Res = execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config),

    <<"bad_parameter">> = get_err_code(Res),
    Msg = get_err_msg(Res),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"message_rate">>), {got_error_message, Msg}).

admin_start_broadcast_coercion_error(Config) ->
    SlowJob = broadcast_helper:slow_job_spec(domain(), <<"u@d">>, ?FUNCTION_NAME),
    Vars = job_spec_to_vars(SlowJob#{message_rate => <<"not_a_number">>}),
    Res = execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config),
    get_coercion_err_msg(Res).

admin_start_broadcast_sender_not_found(Config) ->
    JobWithUnknownSender = broadcast_helper:slow_job_spec(domain(), <<"unknown@sender">>, ?FUNCTION_NAME),
    Vars = job_spec_to_vars(JobWithUnknownSender),
    Res = execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config),
    <<"sender_not_found">> = get_err_code(Res).

admin_abort_broadcast(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    Spec = broadcast_helper:slow_job_spec(domain(), Sender, ?FUNCTION_NAME),
    {ok, JobId} = broadcast_helper:start_broadcast(Spec),

    Vars = #{domain => domain(), id => JobId},
    Res = execute_command(<<"broadcast">>, <<"abortBroadcast">>, Vars, Config),
    [JobId] = get_ok_value([data, broadcast, abortBroadcast, ids], Res),

    GetRes = get_broadcast_op(domain(), JobId, Config),
    #{<<"id">> := JobId} = Job = get_ok_value([data, broadcast, getBroadcast], GetRes),
    validate_job_info(Job, Spec, abort_admin).

admin_delete_inactive_broadcasts(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    {ok, JobId} = broadcast_helper:start_broadcast(
        broadcast_helper:slow_job_spec(domain(), Sender, ?FUNCTION_NAME)),
    {ok, _} = broadcast_helper:abort_broadcast(domain(), JobId),

    Vars = #{domain => domain(), ids => [JobId]},
    Res = execute_command(<<"broadcast">>, <<"deleteInactiveBroadcastsByIds">>, Vars, Config),
    [JobId] = get_ok_value([data, broadcast, deleteInactiveBroadcastsByIds, ids], Res),

    {broadcast_not_found, _} = broadcast_helper:get_broadcast(domain(), JobId).

admin_delete_inactive_broadcasts_by_domain(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    {ok, JobId} = broadcast_helper:start_broadcast(
        broadcast_helper:slow_job_spec(domain(), Sender, ?FUNCTION_NAME)),
    {ok, _} = broadcast_helper:abort_broadcast(domain(), JobId),

    Vars = #{domain => domain()},
    Res = execute_command(<<"broadcast">>, <<"deleteInactiveBroadcastsByDomain">>, Vars, Config),
    [JobId] = get_ok_value([data, broadcast, deleteInactiveBroadcastsByDomain, ids], Res),

    {broadcast_not_found, _} = broadcast_helper:get_broadcast(domain(), JobId).

%%====================================================================
%% Admin Query Tests
%%====================================================================

admin_get_broadcasts_pagination(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    Spec1 = broadcast_helper:slow_job_spec(domain(), Sender, <<"Pagination 1">>),
    {ok, JobId1} = broadcast_helper:start_broadcast(Spec1),
    broadcast_helper:abort_broadcast(domain(), JobId1),

    Spec2 = broadcast_helper:slow_job_spec(domain(), Sender, <<"Pagination 2">>),
    {ok, JobId2} = broadcast_helper:start_broadcast(Spec2),

    Get1 = get_broadcasts_op(domain(), 1, 0, Config),
    [#{<<"id">> := JobId2} = Job2] = get_ok_value([data, broadcast, getBroadcasts], Get1),
    validate_job_info(Job2, Spec2, running),

    Get2 = get_broadcasts_op(domain(), 1, 1, Config),
    [#{<<"id">> := JobId1} = Job1] = get_ok_value([data, broadcast, getBroadcasts], Get2),
    validate_job_info(Job1, Spec1, abort_admin).

admin_get_broadcast(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    Spec = broadcast_helper:slow_job_spec(domain(), Sender, ?FUNCTION_NAME),
    {ok, JobId} = broadcast_helper:start_broadcast(Spec),

    Res = get_broadcast_op(domain(), JobId, Config),
    #{<<"id">> := JobId} = Job = get_ok_value([data, broadcast, getBroadcast], Res),

    validate_job_info(Job, Spec, running).

admin_get_broadcast_finished(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    Spec = broadcast_helper:fast_job_spec(domain(), Sender, ?FUNCTION_NAME),
    {ok, JobId} = broadcast_helper:start_broadcast(Spec),

    %% Wait for the fast job to complete
    broadcast_helper:wait_until_job_state(domain(), JobId, finished),

    %% Verify the job is in finished state via GraphQL
    Res = get_broadcast_op(domain(), JobId, Config),
    #{<<"id">> := JobId} = Job = get_ok_value([data, broadcast, getBroadcast], Res),

    validate_job_info(Job, Spec, finished).

admin_get_broadcast_worker_killed(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    Spec = broadcast_helper:slow_job_spec(domain(), Sender, ?FUNCTION_NAME),
    {ok, JobId} = broadcast_helper:start_broadcast(Spec),

    %% Wait for the worker process to start, then kill it
    HostType = domain_helper:host_type(),
    broadcast_helper:wait_until_worker_started(HostType, JobId),
    WorkerMap = rpc(mim(), broadcast_manager, get_worker_map, [HostType]),
    #{JobId := #{pid := WorkerPid}} = WorkerMap,
    rpc(mim(), erlang, exit, [WorkerPid, kill]),

    %% Wait for the manager to detect the crash and persist abort_error
    wait_until_job_state(domain(), JobId, abort_error),

    %% Verify the job is in abort_error state via GraphQL
    Res = get_broadcast_op(domain(), JobId, Config),
    #{<<"id">> := JobId} = Job = get_ok_value([data, broadcast, getBroadcast], Res),
    validate_job_info(Job, Spec, abort_error).

%%====================================================================
%% Domain Admin Tests
%%====================================================================

domain_admin_start_broadcast_success(Config) ->
    Sender = escalus_utils:jid_to_lower(escalus_users:get_jid(Config, alice)),
    Res = start_broadcast_op(domain(), Sender, ?FUNCTION_NAME, Config),
    [_] = get_ok_value([data, broadcast, startBroadcast, ids], Res).

%%====================================================================
%% Protection Tests
%%====================================================================

admin_module_gating_checks(Config) ->
    HostType = domain_helper:host_type(),
    stop_mod_broadcast(HostType),
    try
        Res = get_broadcasts_op(domain(), 10, 0, Config),
        get_not_loaded(Res)
    after
        ensure_mod_broadcast_started(domain())
    end.

admin_protection_checks(Config) ->
    verify_protection_for_all_ops(domain(), wrong_auth, Config).

domain_admin_protection_checks(Config) ->
    OtherDomain = secondary_domain(),
    verify_protection_for_all_ops(OtherDomain, normal_auth, Config).

%%====================================================================
%% Helpers
%%====================================================================

ensure_mod_broadcast_started(Domain) ->
    dynamic_modules:ensure_modules(domain_helper:domain_to_host_type(mim(), Domain),
                                   [{mod_broadcast, #{backend => rdbms}}]).

stop_mod_broadcast(HostType) ->
    dynamic_modules:ensure_modules(HostType, [{mod_broadcast, stopped}]).

start_broadcast_op(Domain, Sender, JobName, Config) ->
    Vars = job_spec_to_vars(broadcast_helper:slow_job_spec(Domain, Sender, JobName)),
    execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config).

get_broadcasts_op(Domain, Limit, Index, Config) ->
    Vars = #{domain => Domain, limit => Limit, index => Index},
    execute_command(<<"broadcast">>, <<"getBroadcasts">>, Vars, Config).

get_broadcast_op(Domain, Id, Config) ->
    Vars = #{domain => Domain, id => Id},
    execute_command(<<"broadcast">>, <<"getBroadcast">>, Vars, Config).

job_spec_to_vars(Spec) ->
    #{name => maps:get(name, Spec),
      domain => maps:get(domain, Spec),
      messageSubject => maps:get(subject, Spec),
      messageBody => maps:get(body, Spec),
      senderJid => jid:to_binary(maps:get(sender, Spec)),
      messageRate => maps:get(message_rate, Spec),
      recipientGroup => recipient_group_to_var(maps:get(recipient_group, Spec))}.

validate_job_info(Job, Spec, ExpectedStateAtom) ->
    ?assertEqual(maps:get(name, Spec), maps:get(<<"name">>, Job)),
    ?assertEqual(maps:get(domain, Spec), maps:get(<<"domain">>, Job)),
    ?assertEqual(maps:get(subject, Spec), maps:get(<<"messageSubject">>, Job)),
    ?assertEqual(maps:get(body, Spec), maps:get(<<"messageBody">>, Job)),
    ?assertEqual(jid:to_binary(maps:get(sender, Spec)), maps:get(<<"senderJid">>, Job)),
    ?assertEqual(maps:get(message_rate, Spec), maps:get(<<"messageRate">>, Job)),
    ?assertEqual(recipient_group_to_var(maps:get(recipient_group, Spec)), maps:get(<<"recipientGroup">>, Job)),
    ?assertEqual(execution_state_to_var(ExpectedStateAtom), maps:get(<<"executionState">>, Job)),

    assert_non_neg_integer(maps:get(<<"recipientCount">>, Job)),
    assert_non_neg_integer(maps:get(<<"recipientsProcessed">>, Job)),

    % Will throw an exception if the timestamp is not a valid RFC3339 string
    validate_timestamp(maps:get(<<"createTimestamp">>, Job)),

    % stopTimestamp should be null for running jobs, integer otherwise
    % startTimestamp is also safe to check for non-running jobs,
    % but we don't want to enforce it for running ones since they might not have been updated yet
    case ExpectedStateAtom of
        running ->
            null = maps:get(<<"stopTimestamp">>, Job);
        abort_error ->
            validate_timestamp(maps:get(<<"stopTimestamp">>, Job)),
            validate_timestamp(maps:get(<<"startTimestamp">>, Job)),
            <<_, _/binary>> = maps:get(<<"abortionReason">>, Job);
        NonErrorStop when NonErrorStop == abort_admin; NonErrorStop == finished ->
            validate_timestamp(maps:get(<<"stopTimestamp">>, Job)),
            validate_timestamp(maps:get(<<"startTimestamp">>, Job)),
            null = maps:get(<<"abortionReason">>, Job)
    end.

recipient_group_to_var(all_users_in_domain) -> <<"ALL_USERS_IN_DOMAIN">>.

execution_state_to_var(running) -> <<"RUNNING">>;
execution_state_to_var(abort_error) -> <<"ABORT_ERROR">>;
execution_state_to_var(abort_admin) -> <<"ABORT_ADMIN">>;
execution_state_to_var(finished) -> <<"FINISHED">>.

assert_non_neg_integer(I) when is_integer(I), I >= 0 -> ok.

validate_timestamp(TimestampBin) ->
    calendar:rfc3339_to_system_time(binary_to_list(TimestampBin)).

%% Iterates over all broadcast operations to ensure they are protected
verify_protection_for_all_ops(Domain, AuthMode, Config) ->
    Ops = [
        {<<"getBroadcasts">>, #{domain => Domain, limit => 1, index => 0}},
        {<<"getBroadcast">>, #{domain => Domain, id => 2137}},
        {<<"startBroadcast">>, #{name => <<"T">>, domain => Domain, messageSubject => <<"S">>,
                                 messageBody => <<"B">>, senderJid => <<"u@d">>, messageRate => 1,
                                 recipientGroup => <<"ALL_USERS_IN_DOMAIN">>}},
        {<<"abortBroadcast">>, #{domain => Domain, id => 2137}},
        {<<"deleteInactiveBroadcastsByIds">>, #{domain => Domain, ids => [2137]}},
        {<<"deleteInactiveBroadcastsByDomain">>, #{domain => Domain}}
    ],
    NewConfig = apply_auth_mode_to_config(AuthMode, Config),
    lists:foreach(fun({Cmd, Vars}) ->
        get_unauthorized(execute_command(<<"broadcast">>, Cmd, Vars, NewConfig))
    end, Ops).

apply_auth_mode_to_config(wrong_auth, Config) ->
    [{override_admin_creds, {<<"spy">>, <<"spy_password">>}} | Config];
apply_auth_mode_to_config(normal_auth, Config) ->
    Config.
