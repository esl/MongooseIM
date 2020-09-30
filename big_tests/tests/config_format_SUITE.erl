-module(config_format_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-define(eq(Expected, Actual),
        ?assertEqual(Expected, Actual)).

all() ->
    [equivalence].

suite() ->
    distributed_helper:require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    ejabberd_node_utils:init(Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config0) ->
    Config1 = escalus:init_per_testcase(CaseName, Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    Config1.

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config),
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(mongooseim).

%% Test cases

equivalence(Config) ->
    TOMLOpts = get_config_tables(),
    ejabberd_node_utils:modify_config_file(mim, [], Config, cfg),
    ejabberd_node_utils:restart_application(mongooseim),
    CfgOpts = get_config_tables(),
    compare_unordered_lists(CfgOpts, TOMLOpts, fun handle_config_option/2).

%% Test case helpers

get_config_tables() ->
    Tables = [config, local_config, acl],
    Opts = lists:flatmap(fun(Tab) -> rpc(mim(), ets, tab2list, [Tab]) end, Tables),
    lists:filter(fun filter_config/1, Opts).

filter_config({config, required_files, _}) -> false; % not supported in TOML
filter_config({local_config, node_start, _}) -> false;
filter_config(_) -> true.

handle_config_option({Config, K1, V1}, {Config, K2, V2}) when Config =:= config;
                                                              Config =:= local_config ->
    ?eq(K1, K2),
    compare_values(K1, V1, V2);
handle_config_option(Opt1, Opt2) ->
    ?eq(Opt1, Opt2).

compare_values(listen, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_listener/2);
compare_values({auth_opts, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_auth_opt/2);
compare_values(outgoing_pools, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_conn_pool/2);
compare_values({modules, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_item_with_opts/2);
compare_values({services, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_item_with_opts/2);
compare_values({auth_method, _}, V1, V2) when is_atom(V1) ->
    ?eq([V1], V2);
compare_values({s2s_addr, _}, {_, _, _, _} = IP1, IP2) ->
    ?eq(inet:ntoa(IP1), IP2);
compare_values(K, V1, V2) ->
    ?eq({K, V1}, {K, V2}).

handle_listener({P1, M1, O1}, {P2, M2, O2}) ->
    ?eq(P1, P2),
    ?eq(M1, M2),
    compare_unordered_lists(O1, O2, fun handle_listener_option/2).

handle_listener_option({modules, M1}, {modules, M2}) ->
    compare_unordered_lists(M1, M2, fun handle_listener_module/2);
handle_listener_option({transport_options, O1}, {transport_options, O2}) ->
    compare_unordered_lists(O1, O2);
handle_listener_option(V1, V2) -> ?eq(V1, V2).

handle_listener_module({H1, P1, M1}, M2) ->
    handle_listener_module({H1, P1, M1, []}, M2);
handle_listener_module({H1, P1, M1, O1}, {H2, P2, M2, O2}) ->
    ?eq(H1, H2),
    ?eq(P1, P2),
    ?eq(M1, M2),
    compare_listener_module_options(M1, O1, O2).

compare_listener_module_options(mod_websockets,
                                [{ejabberd_service, S1}], [{ejabberd_service, S2}]) ->
    compare_unordered_lists(S1, S2);
compare_listener_module_options(_, O1, O2) ->
    ?eq(O1, O2).

handle_auth_opt({cyrsasl_external, M}, {cyrsasl_external, [M]}) -> ok;
handle_auth_opt(V1, V2) -> ?eq(V1, V2).

handle_item_with_opts({M1, O1}, {M2, O2}) ->
    ?eq(M1, M2),
    compare_unordered_lists(O1, O2).

handle_conn_pool({Type1, Scope1, Tag1, POpts1, COpts1},
                 {Type2, Scope2, Tag2, POpts2, COpts2}) ->
    ?eq(Type1, Type2),
    ?eq(Scope1, Scope2),
    ?eq(Tag1, Tag2),
    compare_unordered_lists(POpts1, POpts2),
    compare_unordered_lists(COpts1, COpts2, fun handle_conn_opt/2).

handle_conn_opt({server, {D1, H1, DB1, U1, P1, O1}},
                {server, {D2, H2, DB2, U2, P2, O2}}) ->
    ?eq(D1, D2),
    ?eq(H1, H2),
    ?eq(DB1, DB2),
    ?eq(U1, U2),
    ?eq(P1, P2),
    compare_unordered_lists(O1, O2, fun handle_db_server_opt/2);
handle_conn_opt({tls_options, Opts1}, {tls_options, Opts2}) ->
    compare_unordered_lists(Opts1, Opts2);
handle_conn_opt(V1, V2) -> ?eq(V1, V2).

handle_db_server_opt({ssl_opts, O1}, {ssl_opts, O2}) ->
    compare_unordered_lists(O1, O2);
handle_db_server_opt(V1, V2) -> ?eq(V1, V2).

compare_unordered_lists(L1, L2) ->
    compare_unordered_lists(L1, L2, fun(V1, V2) -> ?eq(V1, V2) end).

compare_unordered_lists(L1, L2, F) ->
    SL1 = lists:sort(L1),
    SL2 = lists:sort(L2),
    compare_ordered_lists(SL1, SL2, F).

compare_ordered_lists([H1|T1], [H1|T2], F) ->
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([H1|T1], [H2|T2], F) ->
    try F(H1, H2)
    catch C:R:S ->
            ct:fail({C, R, S})
    end,
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([], [], _) ->
    ok.
