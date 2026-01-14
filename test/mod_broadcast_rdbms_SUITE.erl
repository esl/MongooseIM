-module(mod_broadcast_rdbms_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(HT, <<"localhost">>).

all() ->
    [list_running_jobs_binds_host_type_test,
     list_jobs_domain_param_null_test,
     next_recipients_uses_default_limit_test,
     claim_job_ok_test,
     claim_job_not_claimed_test,
     get_job_not_found_test].

list_running_jobs_binds_host_type_test(_Config) ->
    OwnerNode = <<"node@host">>,
    meck:new(mongoose_rdbms, [passthrough]),
    meck:expect(mongoose_rdbms, execute,
        fun(?HT, broadcast_list_running_owned, [OwnerNodeArg, ?HT]) when OwnerNodeArg =:= OwnerNode ->
                        {selected, [{10}, {20}]}
                end),

    ?assertEqual({ok, [10, 20]}, mod_broadcast_rdbms:list_running_jobs(?HT, OwnerNode)),

    meck:unload(mongoose_rdbms).

list_jobs_domain_param_null_test(_Config) ->
    Limit = 25,
    Index = 50,
    meck:new(mongoose_rdbms, [passthrough]),

    %% Return one row, then total count.
    meck:expect(mongoose_rdbms, execute,
            fun(?HT, broadcast_list_jobs, [null, null, LimitArg, IndexArg])
                  when LimitArg =:= Limit, IndexArg =:= Index ->
                        {selected, [job_row(101)]};
                   (?HT, broadcast_count_jobs, [null, null]) ->
                        {selected, [{1}]}
                end),

    {ok, Payload} = mod_broadcast_rdbms:list_jobs(?HT, undefined, Limit, Index),
    ?assertMatch(#{items := [_], limit := Limit, index := Index, total_count := 1}, Payload),

    meck:unload(mongoose_rdbms).

next_recipients_uses_default_limit_test(_Config) ->
    JobId = 123,
    meck:new(mongoose_rdbms, [passthrough]),
    meck:expect(mongoose_rdbms, execute,
        fun(?HT, broadcast_select_next_recipients, [JobIdArg, 200]) when JobIdArg =:= JobId ->
                        {selected, [{<<"alice">>}, {<<"bob">>}]}
                end),

    ?assertEqual({ok, [<<"alice">>, <<"bob">>]}, mod_broadcast_rdbms:next_recipients(?HT, JobId, undefined)),

    meck:unload(mongoose_rdbms).

claim_job_ok_test(_Config) ->
    JobId = 777,
    meck:new(mongoose_rdbms, [passthrough]),
    meck:expect(mongoose_rdbms, execute,
        fun(?HT, broadcast_claim_job, [_Owner, _Now, JobIdArg, _Owner2]) when JobIdArg =:= JobId ->
                        {updated, 1}
                end),

    ?assertEqual(ok, mod_broadcast_rdbms:claim_job(?HT, JobId)),

    meck:unload(mongoose_rdbms).

claim_job_not_claimed_test(_Config) ->
    JobId = 888,
    meck:new(mongoose_rdbms, [passthrough]),
    meck:expect(mongoose_rdbms, execute,
        fun(?HT, broadcast_claim_job, [_Owner, _Now, JobIdArg, _Owner2]) when JobIdArg =:= JobId ->
                        {updated, 0}
                end),

    ?assertEqual(not_claimed, mod_broadcast_rdbms:claim_job(?HT, JobId)),

    meck:unload(mongoose_rdbms).

get_job_not_found_test(_Config) ->
    JobId = 999,
    meck:new(mongoose_rdbms, [passthrough]),
    meck:expect(mongoose_rdbms, execute,
        fun(?HT, broadcast_select_job, [JobIdArg]) when JobIdArg =:= JobId ->
                        {selected, []}
                end),

    ?assertEqual(not_found, mod_broadcast_rdbms:get_job(?HT, JobId)),

    meck:unload(mongoose_rdbms).

%% Helpers
job_row(Id) ->
    {Id,
     ?HT,
     <<"example.com">>,
     <<"name">>,
     <<"sender@example.com">>,
     <<"subject">>,
     <<"body">>,
     <<"running">>,
     1000,
     null,
     10,
     2,
     1,
     <<"node@host">>,
     1000,
     null}.
