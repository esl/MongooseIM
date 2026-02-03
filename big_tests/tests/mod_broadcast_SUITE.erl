%%%-------------------------------------------------------------------
%%% @doc Integration test suite for mod_broadcast module.
%%%
%%% Tests the broadcast functionality via mod_broadcast_api module.
%%% Database correctness is validated indirectly via API calls.
%%% Requires RDBMS backend.
%%%-------------------------------------------------------------------
-module(mod_broadcast_SUITE).
-author('piotr.nosek@erlang-solutions.com').

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% lifecycle
-export([start_broadcast_ok_returns_job_id/1,
         start_broadcast_already_running/1,
         start_broadcast_two_domains_both_ok/1,
         abort_broadcast_running_ok/1,
         abort_broadcast_not_found/1,
         abort_broadcast_not_running_returns_not_running/1,
         delete_inactive_by_ids_skips_running/1,
         delete_inactive_by_domain_empty_ok/1,
         delete_inactive_by_domain_deletes_only_inactive/1]).
%% validation
-export([start_broadcast_domain_not_found/1,
         start_broadcast_sender_not_found/1,
         start_broadcast_bad_message_rate/1,
         start_broadcast_bad_name/1,
         start_broadcast_bad_subject/1,
         start_broadcast_bad_body/1,
         start_broadcast_string_limits_ok/1]).
%% retrieval
-export([get_broadcast_domain_not_found/1,
         get_broadcast_not_found/1,
         get_broadcast_ok_returns_expected_fields/1,
         get_broadcasts_empty_ok/1,
         get_broadcasts_pagination_basic/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [domain/0, secondary_domain/0]).

%%====================================================================
%% CT callbacks
%%====================================================================

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    case (not ct_helper:is_ct_running())
         orelse mongoose_helper:is_rdbms_enabled(domain()) of
        true ->
            [{group, lifecycle},
             {group, validation},
             {group, retrieval}];
        false ->
            {skip, require_rdbms}
    end.

groups() ->
    [{lifecycle, [sequence], lifecycle_tests()},
     {validation, [], validation_tests()},
     {retrieval, [], retrieval_tests()}].

lifecycle_tests() ->
    [start_broadcast_ok_returns_job_id,
     start_broadcast_already_running,
     start_broadcast_two_domains_both_ok,
     abort_broadcast_running_ok,
     abort_broadcast_not_found,
     abort_broadcast_not_running_returns_not_running,
     delete_inactive_by_ids_skips_running,
     delete_inactive_by_domain_empty_ok,
     delete_inactive_by_domain_deletes_only_inactive].

validation_tests() ->
    [start_broadcast_domain_not_found,
     start_broadcast_sender_not_found,
     start_broadcast_bad_message_rate,
     start_broadcast_bad_name,
     start_broadcast_bad_subject,
     start_broadcast_bad_body,
     start_broadcast_string_limits_ok].

retrieval_tests() ->
    [get_broadcast_domain_not_found,
     get_broadcast_not_found,
     get_broadcast_ok_returns_expected_fields,
     get_broadcasts_empty_ok,
     get_broadcasts_pagination_basic].

%%====================================================================
%% Suite setup/teardown
%%====================================================================

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules([domain(), secondary_domain()], Config),
    ensure_broadcast_started(domain()),
    ensure_broadcast_started(secondary_domain()),
    create_many_users(100, domain()),
    clean_broadcast_jobs(),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    delete_many_users(100, domain()),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

%%====================================================================
%% Group setup/teardown
%%====================================================================

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%====================================================================
%% Test case setup/teardown
%%====================================================================

init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    clean_broadcast_jobs(),
    escalus:end_per_testcase(TestCase, Config).

%%====================================================================
%% Lifecycle tests
%%====================================================================

start_broadcast_ok_returns_job_id(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = valid_job_spec(domain(), AliceJid),
        {ok, JobId} = start_broadcast(JobSpec),
        ?assert(is_integer(JobId)),
        ?assert(JobId >= 0)
    end).

start_broadcast_already_running(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Use slow job spec with 100 users created in init_per_suite
        JobSpec = slow_job_spec(domain(), AliceJid),
        {ok, _JobId1} = start_broadcast(JobSpec),
        Result = start_broadcast(JobSpec),
        ?assertMatch({already_running, _}, Result)
    end).

start_broadcast_two_domains_both_ok(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], fun(Alice, AliceBis) ->
        AliceJid = escalus_client:short_jid(Alice),
        AliceBisJid = escalus_client:short_jid(AliceBis),
        JobSpecA = valid_job_spec(domain(), AliceJid),
        JobSpecB = valid_job_spec(secondary_domain(), AliceBisJid),
        {ok, JobIdA} = start_broadcast(JobSpecA),
        {ok, JobIdB} = start_broadcast(JobSpecB),
        ?assert(is_integer(JobIdA)),
        ?assert(is_integer(JobIdB)),
        ?assertNotEqual(JobIdA, JobIdB)
    end).

abort_broadcast_running_ok(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = valid_job_spec(domain(), AliceJid),
        {ok, JobId} = start_broadcast(JobSpec),
        {ok, ReturnedId} = abort_broadcast(domain(), JobId),
        ?assertEqual(JobId, ReturnedId),
        %% Verify state changed (poll with bounded retries)
        wait_until_job_stops(domain(), JobId)
    end).

abort_broadcast_not_found(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], fun(_Alice, AliceBis) ->
        AliceBisJid = escalus_client:short_jid(AliceBis),
        %% Create a job in domain B
        JobIdB = create_job_in_domain(secondary_domain(), AliceBisJid),

        %% Table-driven: nonexistent ID and ID from other domain
        Cases = [
            {<<"nonexistent_id">>, 999999, domain()},
            {<<"cross_domain_id">>, JobIdB, domain()}
        ],
        lists:foreach(fun({Desc, Id, Domain}) ->
            Result = abort_broadcast(Domain, Id),
            ?assertMatch({broadcast_not_found, _}, Result, Desc)
        end, Cases)
    end).

abort_broadcast_not_running_returns_not_running(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = valid_job_spec(domain(), AliceJid),
        {ok, JobId} = start_broadcast(JobSpec),
        %% First abort it
        {ok, _} = abort_broadcast(domain(), JobId),
        wait_until_job_stops(domain(), JobId),
        %% Now try to abort again - it's already not running
        Result = abort_broadcast(domain(), JobId),
        ?assertMatch({not_running, _}, Result)
    end).

delete_inactive_by_ids_skips_running(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid),

        {ok, InactiveId} = start_and_abort_job(JobSpec),
        {ok, RunningId} = start_broadcast(JobSpec),

        %% Use a bogus ID to verify that non-existent IDs are ignored and don't cause failure
        BogusId = 999999,
        {ok, [InactiveId]} = delete_inactive_broadcasts_by_ids(domain(),
                                                               [RunningId, InactiveId, BogusId]),

        ?assertMatch({broadcast_not_found, _},
                     get_broadcast(domain(), InactiveId)),
        ?assertMatch({ok, _}, get_broadcast(domain(), RunningId))
    end).

delete_inactive_by_domain_empty_ok(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(_Alice) ->
        {ok, []} = get_broadcasts(domain(), 10, 0),
        {ok, []} = delete_inactive_broadcasts_by_domain(domain())
    end).

delete_inactive_by_domain_deletes_only_inactive(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid),

        {ok, InactiveId} = start_and_abort_job(JobSpec),
        {ok, RunningId} = start_broadcast(JobSpec),

        {ok, [InactiveId]} = delete_inactive_broadcasts_by_domain(domain()),
        ?assertMatch({broadcast_not_found, _},
                     get_broadcast(domain(), InactiveId)),
        ?assertMatch({ok, _}, get_broadcast(domain(), RunningId))
    end).

%%====================================================================
%% Validation tests
%%====================================================================

start_broadcast_domain_not_found(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = valid_job_spec(unknown_domain(), AliceJid),
        Result = start_broadcast(JobSpec),
        ?assertMatch({domain_not_found, _}, Result)
    end).

start_broadcast_sender_not_found(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(_Alice) ->
        %% Use a non-existent sender
        NonExistentJid = jid:make_noprep(<<"nonexistent">>, domain(), <<>>),
        JobSpec = valid_job_spec(domain(), NonExistentJid),
        Result = start_broadcast(JobSpec),
        ?assertMatch({sender_not_found, _}, Result)
    end).

start_broadcast_bad_message_rate(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Table-driven: 0, negative, >1000
        InvalidRates = [0, -1, -100, 1001, 9999],
        lists:foreach(fun(Rate) ->
            JobSpec = (valid_job_spec(domain(), AliceJid))#{message_rate := Rate},
            Result = start_broadcast(JobSpec),
            ?assertMatch({bad_parameter, _}, Result,
                         io_lib:format("message_rate ~p should fail", [Rate]))
        end, InvalidRates)
    end).

start_broadcast_bad_name(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Empty and too long
        InvalidNames = [<<>>, binary:copy(<<"a">>, 251)],
        lists:foreach(fun(Name) ->
            JobSpec = (valid_job_spec(domain(), AliceJid))#{name := Name},
            Result = start_broadcast(JobSpec),
            ?assertMatch({bad_parameter, _}, Result,
                         io_lib:format("name length ~p should fail", [byte_size(Name)]))
        end, InvalidNames)
    end).

start_broadcast_bad_subject(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Too long (>1024)
        TooLongSubject = binary:copy(<<"a">>, 1025),
        JobSpec = (valid_job_spec(domain(), AliceJid))#{subject := TooLongSubject},
        Result = start_broadcast(JobSpec),
        ?assertMatch({bad_parameter, _}, Result)
    end).

start_broadcast_bad_body(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Empty and too long
        InvalidBodies = [<<>>, binary:copy(<<"a">>, 16001)],
        lists:foreach(fun(Body) ->
            JobSpec = (valid_job_spec(domain(), AliceJid))#{body := Body},
            Result = start_broadcast(JobSpec),
            ?assertMatch({bad_parameter, _}, Result,
                         io_lib:format("body length ~p should fail", [byte_size(Body)]))
        end, InvalidBodies)
    end).

start_broadcast_string_limits_ok(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Test boundary values that should be accepted
        BoundaryCases = [
            %% {field, min_value, max_value}
            {name, binary:copy(<<"a">>, 1), binary:copy(<<"a">>, 250)},
            {subject, <<>>, binary:copy(<<"a">>, 1024)},
            {body, binary:copy(<<"a">>, 1), binary:copy(<<"a">>, 16000)}
        ],
        lists:foreach(fun({Field, MinVal, MaxVal}) ->
            %% Test minimum
            JobSpecMin = maps:put(Field, MinVal, valid_job_spec(domain(), AliceJid)),
            ResultMin = start_broadcast(JobSpecMin),
            ?assertMatch({ok, _}, ResultMin,
                         io_lib:format("~p min value should succeed", [Field])),
            %% Abort to allow next test
            {ok, JobIdMin} = ResultMin,
            abort_broadcast(domain(), JobIdMin),
            wait_until_job_stops(domain(), JobIdMin),

            %% Test maximum
            JobSpecMax = maps:put(Field, MaxVal,
                                  valid_job_spec_named(domain(), AliceJid, <<"maxtest">>)),
            ResultMax = start_broadcast(JobSpecMax),
            ?assertMatch({ok, _}, ResultMax,
                         io_lib:format("~p max value should succeed", [Field])),
            %% Abort for cleanup
            {ok, JobIdMax} = ResultMax,
            abort_broadcast(domain(), JobIdMax),
            wait_until_job_stops(domain(), JobIdMax)
        end, BoundaryCases)
    end).

%%====================================================================
%% Retrieval tests
%%====================================================================

get_broadcast_domain_not_found(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(_Alice) ->
        Result = get_broadcast(unknown_domain(), 1),
        ?assertMatch({domain_not_found, _}, Result)
    end).

get_broadcast_not_found(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {alice_bis, 1}], fun(_Alice, AliceBis) ->
        %% First create a job in domain B
        AliceBisJid = escalus_client:short_jid(AliceBis),
        JobIdB = create_job_in_domain(secondary_domain(), AliceBisJid),

        %% Table-driven: nonexistent ID and ID from other domain
        Cases = [
            {<<"nonexistent_id">>, 999999, domain()},
            {<<"cross_domain_id">>, JobIdB, domain()}
        ],
        lists:foreach(fun({Desc, Id, Domain}) ->
            Result = get_broadcast(Domain, Id),
            ?assertMatch({broadcast_not_found, _}, Result, Desc)
        end, Cases)
    end).

get_broadcast_ok_returns_expected_fields(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        JobSpec = slow_job_spec(domain(), AliceJid),
        #{name := ExpectedName,
          domain := ExpectedDomain,
          sender := ExpectedSender,
          subject := ExpectedSubject,
          body := ExpectedBody,
          message_rate := ExpectedRate,
          recipient_group := ExpectedRecipientGroup} = JobSpec,
        ExpectedNode = maps:get(node, distributed_helper:mim()),

        {ok, JobId} = start_broadcast(JobSpec),
        Job = broadcast_job_to_map(get_broadcast(domain(), JobId)),

        %% Verify expected fields are present and match
        #{id := JobId,
          domain := ExpectedDomain,
          name := ExpectedName,
          sender := ExpectedSender,
          subject := ExpectedSubject,
          body := ExpectedBody,
          message_rate := ExpectedRate,
          execution_state := running,
          owner_node := ExpectedNode,
          create_timestamp := {{_, _, _}, {_, _, _}},
          stop_timestamp := undefined,
          abortion_reason := undefined,
          recipient_group := ExpectedRecipientGroup,
          recipient_count := RecipientCount,
          recipients_processed := RecipientsProcessed} = Job,

        assert_non_neg_integer(RecipientCount),
        assert_non_neg_integer(RecipientsProcessed),

        abort_broadcast(domain(), JobId),
        wait_until_job_stops(domain(), JobId),

        AbortedJob = broadcast_job_to_map(get_broadcast(domain(), JobId)),
        #{execution_state := abort_admin,
          start_timestamp := {{_, _, _}, {_, _, _}},
          stop_timestamp := {{_, _, _}, {_, _, _}}} = AbortedJob
    end).

get_broadcasts_empty_ok(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(_Alice) ->
        {ok, Jobs} = get_broadcasts(domain(), 10, 0),
        ?assertEqual([], Jobs)
    end).

get_broadcasts_pagination_basic(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJid = escalus_client:short_jid(Alice),
        %% Create and abort multiple jobs for pagination testing
        %% (only one job can run per domain at a time, so we need inactive jobs)
        {ok, _JobId1} = start_and_abort_job(valid_job_spec_named(domain(), AliceJid, <<"pag1">>)),
        {ok, _JobId2} = start_and_abort_job(valid_job_spec_named(domain(), AliceJid, <<"pag2">>)),
        {ok, _JobId3} = start_and_abort_job(valid_job_spec_named(domain(), AliceJid, <<"pag3">>)),

        %% Get all with high limit
        {ok, AllJobs} = get_broadcasts(domain(), 100, 0),
        ?assertEqual(3, length(AllJobs)),

        %% Get with limit 2
        {ok, LimitedJobs} = get_broadcasts(domain(), 2, 0),
        ?assert(length(LimitedJobs) =< 2),

        %% Get with offset
        {ok, OffsetJobs} = get_broadcasts(domain(), 100, 1),
        ?assertEqual(2, length(OffsetJobs))
    end).

%%====================================================================
%% Test helpers
%%====================================================================

assert_non_neg_integer(Value) when is_integer(Value), Value >= 0 ->
    ok.

valid_job_spec(Domain, SenderJid) ->
    valid_job_spec_named(Domain, SenderJid, <<"test_broadcast">>).

valid_job_spec_named(Domain, SenderJid, Name) ->
    #{
        name => Name,
        domain => Domain,
        sender => jid:from_binary(SenderJid),
        subject => <<"Test Subject">>,
        body => <<"Test Body">>,
        message_rate => 100,
        recipient_group => all_users_in_domain
    }.

slow_job_spec(Domain, SenderJid) ->
    #{
        name => <<"slow_broadcast">>,
        domain => Domain,
        sender => jid:from_binary(SenderJid),
        subject => <<"Slow Test Subject">>,
        body => <<"Slow Test Body">>,
        message_rate => 1,  %% Low rate to make it slow
        recipient_group => all_users_in_domain
    }.

create_job_in_domain(Domain, SenderJid) ->
    JobSpec = valid_job_spec(Domain, SenderJid),
    {ok, JobId} = start_broadcast(JobSpec),
    JobId.

start_and_abort_job(JobSpec) ->
    Domain = maps:get(domain, JobSpec),
    {ok, JobId} = start_broadcast(JobSpec),
    {ok, _} = abort_broadcast(Domain, JobId),
    wait_until_job_stops(Domain, JobId),
    {ok, JobId}.

wait_until_job_stops(Domain, JobId) ->
    wait_helper:wait_until(
        fun() ->
            case get_broadcast(Domain, JobId) of
                {ok, JobRecord} ->
                    case broadcast_job_to_map(JobRecord) of
                        #{execution_state := running} ->
                            {error, still_running};
                        _ ->
                            finished
                    end;
                {broadcast_not_found, _} ->
                    {error, not_found}
            end
        end,
        finished,
        #{name => wait_for_abort_state}).

%%====================================================================
%% RPC helpers
%%====================================================================

call_api(Function, Args) ->
    rpc(mim(), mod_broadcast_api, Function, Args).

start_broadcast(JobSpec) ->
    call_api(start_broadcast, [JobSpec]).

abort_broadcast(Domain, JobId) ->
    call_api(abort_broadcast, [Domain, JobId]).

get_broadcast(Domain, JobId) ->
    call_api(get_broadcast, [Domain, JobId]).

get_broadcasts(Domain, Limit, Offset) ->
    call_api(get_broadcasts, [Domain, Limit, Offset]).

delete_inactive_broadcasts_by_ids(Domain, JobIds) ->
    call_api(delete_inactive_broadcasts_by_ids, [Domain, JobIds]).

delete_inactive_broadcasts_by_domain(Domain) ->
    call_api(delete_inactive_broadcasts_by_domain, [Domain]).

broadcast_job_to_map({ok, JobRecord}) ->
    broadcast_job_to_map(JobRecord);
broadcast_job_to_map(JobRecord) ->
    call_api(broadcast_job_to_map, [JobRecord]).

call_manager(Function, Args) ->
    rpc(mim(), broadcast_manager, Function, Args).

ensure_broadcast_started(HostType) ->
    dynamic_modules:ensure_modules(HostType, [{mod_broadcast, #{backend => rdbms}}]).

clean_broadcast_jobs() ->
    ok = call_manager(abort_running_jobs_for_domain, [domain(), domain()]),
    ok = call_manager(abort_running_jobs_for_domain, [secondary_domain(), secondary_domain()]),
    {ok, _} = delete_inactive_broadcasts_by_domain(domain()),
    {ok, _} = delete_inactive_broadcasts_by_domain(secondary_domain()),
    ok.

%%====================================================================
%% Other
%%====================================================================

unknown_domain() ->
    <<"unknown.test">>.

%%====================================================================
%% User management helpers
%%====================================================================

create_many_users(Count, Domain) ->
    lists:foreach(fun(N) ->
        Username = <<"testuser", (integer_to_binary(N))/binary>>,
        Password = <<"password123">>,
        JID = jid:make_noprep(Username, Domain, <<>>),
        rpc(mim(), ejabberd_auth, try_register, [JID, Password])
    end, lists:seq(1, Count)).

delete_many_users(Count, Domain) ->
    lists:foreach(fun(N) ->
        Username = <<"testuser", (integer_to_binary(N))/binary>>,
        JID = jid:make_noprep(Username, Domain, <<>>),
        rpc(mim(), ejabberd_auth, remove_user, [JID])
    end, lists:seq(1, Count)).
