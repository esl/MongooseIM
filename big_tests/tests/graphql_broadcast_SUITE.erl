-module(graphql_broadcast_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1, mim/0]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_command/4,
                         get_ok_value/2,
                         get_err_code/1,
                         get_unauthorized/1,
                         get_not_loaded/1]).
-import(mongooseimctl_helper, [rpc_call/3]).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_http},
     {group, admin_cli},
     {group, domain_admin}].

groups() ->
    [{admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {domain_admin, [], domain_admin_groups()},

     {admin_broadcast_configured, [], admin_tests()},
     {admin_broadcast_not_configured, [], not_configured_tests()},

     {domain_admin_broadcast_configured, [], domain_admin_tests()},
     {domain_admin_broadcast_not_configured, [], not_configured_tests()}].

admin_groups() ->
    [{group, admin_broadcast_configured},
     {group, admin_broadcast_not_configured}].

domain_admin_groups() ->
    [{group, domain_admin_broadcast_configured},
     {group, domain_admin_broadcast_not_configured}].

admin_tests() ->
    [admin_start_get_delete_broadcast_test,
     admin_get_broadcasts_pagination_test,
     admin_abort_broadcast_test].

domain_admin_tests() ->
    [domain_admin_get_broadcasts_own_domain_test,
     domain_admin_get_broadcasts_no_domain_no_permission_test,
     domain_admin_start_broadcast_other_domain_no_permission_test,
     domain_admin_get_broadcast_no_permission_test,
     domain_admin_abort_broadcast_no_permission_test,
     domain_admin_delete_broadcasts_no_permission_test].

not_configured_tests() ->
    [broadcast_not_configured_test].

init_per_suite(Config0) ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true ->
            HostType = domain_helper:host_type(),
            Config = dynamic_modules:save_modules(HostType, Config0),
            Config1 = escalus:init_per_suite(Config),
            ejabberd_node_utils:init(mim(), Config1);
        false ->
            {skip, "RDBMS not available"}
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(Group, Config) when Group =:= admin_broadcast_configured;
                                   Group =:= domain_admin_broadcast_configured ->
    ensure_broadcast_started(Config);
init_per_group(Group, Config) when Group =:= admin_broadcast_not_configured;
                                   Group =:= domain_admin_broadcast_not_configured ->
    ensure_broadcast_stopped(Config).

end_per_group(GroupName, _Config) when GroupName =:= admin_http;
                                       GroupName =:= admin_cli;
                                       GroupName =:= domain_admin ->
    graphql_helper:clean();
end_per_group(GroupName, _Config) when GroupName =:= admin_broadcast_configured;
                                       GroupName =:= domain_admin_broadcast_configured ->
    ok = cleanup_stopped_jobs(),
    escalus_fresh:clean();
end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(_CaseName, _Config) ->
    ok.

ensure_broadcast_started(Config) ->
    HostType = host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_broadcast, #{}}]),
    ok = cleanup_stopped_jobs(),
    Config.

ensure_broadcast_stopped(Config) ->
    HostType = host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_broadcast, stopped}]),
    Config.

cleanup_stopped_jobs() ->
    %% Keep DB clean between tests/suites. We only delete stopped jobs.
    _ = rpc_call(mod_broadcast_rdbms, delete_jobs, [global, undefined]),
    ok.

%% Admin tests

admin_start_get_delete_broadcast_test(Config) ->
    Id = start_empty_broadcast(domain(), Config),
    Job = wait_until_stopped(Id, Config),
    ?assertMatch(#{<<"id">> := _, <<"status">> := _}, Job),

    Res = delete_broadcasts([Id], Config),
    DeletedCount = get_ok_value([data, broadcast, deleteBroadcasts, deletedCount], Res),
    ?assertEqual(1, DeletedCount),

    Res2 = get_broadcast(Id, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Res2)).

admin_get_broadcasts_pagination_test(Config) ->
    Id1 = start_empty_broadcast(domain(), Config),
    Id2 = start_empty_broadcast(domain(), Config),
    Id3 = start_empty_broadcast(domain(), Config),
    _ = wait_until_stopped(Id1, Config),
    _ = wait_until_stopped(Id2, Config),
    _ = wait_until_stopped(Id3, Config),

    Res1 = get_broadcasts(null, 2, 0, Config),
    Items1 = get_ok_value([data, broadcast, getBroadcasts, items], Res1),
    Total1 = get_ok_value([data, broadcast, getBroadcasts, totalCount], Res1),
    ?assertEqual(2, length(Items1)),
    ?assert(Total1 >= 3),

    Res2 = get_broadcasts(null, 2, 2, Config),
    Items2 = get_ok_value([data, broadcast, getBroadcasts, items], Res2),
    Total2 = get_ok_value([data, broadcast, getBroadcasts, totalCount], Res2),
    ?assertEqual(1, length(Items2)),
    ?assertEqual(Total1, Total2).

admin_abort_broadcast_test(Config) ->
    Id = start_slow_broadcast(domain(), 50, 1, Config),
    Res = abort_broadcast(Id, Config),
    Status = get_ok_value([data, broadcast, abortBroadcast, status], Res),
    ?assertEqual('ABORTED_ADMIN', Status),

    %% Clean up: job is stopped so can be deleted.
    _ = delete_broadcasts([Id], Config),
    ok.

%% Domain admin tests

domain_admin_get_broadcasts_own_domain_test(Config) ->
    %% Domain admins must scope to their domain.
    Res = get_broadcasts(domain(), 10, 0, Config),
    _ = get_ok_value([data, broadcast, getBroadcasts, items], Res),
    ok.

domain_admin_get_broadcasts_no_domain_no_permission_test(Config) ->
    Res = get_broadcasts(null, 10, 0, Config),
    get_unauthorized(Res).

domain_admin_start_broadcast_other_domain_no_permission_test(Config) ->
    Vars = start_broadcast_vars(<<"otherhost">>, <<"other">>, sender_jid_for_domain(<<"otherhost">>), 1,
                               #{<<"type">> => <<"USERNAMES">>, <<"usernames">> => []}),
    Res = execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config),
    get_unauthorized(Res).

domain_admin_get_broadcast_no_permission_test(Config) ->
    Res = get_broadcast(1, Config),
    get_unauthorized(Res).

domain_admin_abort_broadcast_no_permission_test(Config) ->
    Res = abort_broadcast(1, Config),
    get_unauthorized(Res).

domain_admin_delete_broadcasts_no_permission_test(Config) ->
    Res = delete_broadcasts(null, Config),
    get_unauthorized(Res).

%% Not configured

broadcast_not_configured_test(Config) ->
    Vars = start_broadcast_vars(domain(), <<"name">>, sender_jid_for_domain(domain()), 1,
                               #{<<"type">> => <<"USERNAMES">>, <<"usernames">> => []}),
    Res = execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config),
    get_not_loaded(Res).

%% Commands

start_empty_broadcast(Domain, Config) ->
    Vars = start_broadcast_vars(Domain,
                               unique_name(<<"empty">>),
                               sender_jid_for_domain(Domain),
                               1000,
                               #{<<"type">> => <<"USERNAMES">>, <<"usernames">> => []}),
    Res = execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config),
    get_ok_value([data, broadcast, startBroadcast, id], Res).

start_slow_broadcast(Domain, N, Rate, Config) ->
    Vars = start_broadcast_vars(Domain,
                               unique_name(<<"slow">>),
                               sender_jid_for_domain(Domain),
                               Rate,
                               #{<<"type">> => <<"USERNAMES">>, <<"usernames">> => gen_usernames(N)}),
    Res = execute_command(<<"broadcast">>, <<"startBroadcast">>, Vars, Config),
    get_ok_value([data, broadcast, startBroadcast, id], Res).

get_broadcast(Id, Config) ->
    execute_command(<<"broadcast">>, <<"getBroadcast">>, #{id => Id}, Config).

get_broadcasts(Domain, Limit, Index, Config) ->
    execute_command(<<"broadcast">>, <<"getBroadcasts">>,
                    #{domain => Domain, limit => Limit, index => Index}, Config).

abort_broadcast(Id, Config) ->
    execute_command(<<"broadcast">>, <<"abortBroadcast">>, #{id => Id}, Config).

delete_broadcasts(Ids, Config) ->
    execute_command(<<"broadcast">>, <<"deleteBroadcasts">>, #{ids => Ids}, Config).

wait_until_stopped(Id, Config) ->
    wait_until_stopped(Id, Config, 50).

wait_until_stopped(Id, Config, 0) ->
    %% Last attempt: return what we have.
    get_ok_value([data, broadcast, getBroadcast], get_broadcast(Id, Config));
wait_until_stopped(Id, Config, N) ->
    Res = get_broadcast(Id, Config),
    Job = get_ok_value([data, broadcast, getBroadcast], Res),
    case maps:get(<<"status">>, Job) of
        'RUNNING' ->
            timer:sleep(100),
            wait_until_stopped(Id, Config, N - 1);
        _ ->
            Job
    end.

unique_name(Prefix) ->
    Suffix = integer_to_binary(erlang:unique_integer([positive, monotonic])),
    <<Prefix/binary, "-", Suffix/binary>>.

sender_jid_for_domain(Domain) ->
    <<"admin@", Domain/binary>>.

gen_usernames(N) when N > 0 ->
    [<<"user", (integer_to_binary(I))/binary>> || I <- lists:seq(1, N)].

atom_to_bin(B) when is_binary(B) ->
    B;
atom_to_bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
atom_to_bin(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

start_broadcast_vars(Domain, Name, SenderJid, Rate, Recipients) ->
    #{domain => Domain,
      name => Name,
      senderJid => SenderJid,
      subject => null,
      body => <<"hello">>,
      ratePerSecond => Rate,
      recipients => Recipients}.
