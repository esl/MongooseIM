-module(start_node_id_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, all}].

groups() ->
    [{all, [], cases()}].

cases() ->
    [cleaning_works].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

cleaning_works(_Config) ->
    Id = <<"someid139455">>,
    Pid = spawn_link(fun() -> receive stop -> ok end end),
    ok = rpc(mim(), mongoose_start_node_id, register_on_remote_node_rpc, [node(), Id, Pid]),
    GetF = fun() -> rpc(mim(), mongoose_start_node_id, node_id_to_name, [Id]) end,
    wait_helper:wait_until(GetF, {ok, node()}),
    Pid ! stop,
    wait_helper:wait_until(GetF, {error, unknown_id}).
