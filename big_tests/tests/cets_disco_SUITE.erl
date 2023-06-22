-module(cets_disco_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, file}, {group, rdbms}].

groups() ->
    [{file, [], file_cases()},
     {rdbms, [], rdbms_cases()}].

file_cases() ->
    [file_backend].

rdbms_cases() ->
    [rdbms_backend].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(rdbms, Config) ->
    case not ct_helper:is_ct_running()
         orelse mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        false -> {skip, rdbms_or_ct_not_running};
        true -> Config
    end;
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

file_backend(Config) ->
    Path = filename:join(?config(mim_data_dir, Config), "nodes.txt"),
    Opts = #{disco_file => Path},
    State = rpc(mim(), cets_discovery_file, init, [Opts]),
    {{ok, Nodes}, _} = rpc(mim(), cets_discovery_file, get_nodes, [State]),
    ['node1@localhost', 'node2@otherhost'] = lists:sort(Nodes).

rdbms_backend(_Config) ->
    Opts1 = #{cluster_name => <<"big_test">>, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => <<"big_test">>, node_name_to_insert => <<"test2">>},
    State1 = rpc(mim(), mongoose_cets_discovery_rdbms, init, [Opts1]),
    rpc(mim(), mongoose_cets_discovery_rdbms, get_nodes, [State1]),
    State2 = rpc(mim(), mongoose_cets_discovery_rdbms, init, [Opts2]),
    {{ok, Nodes}, _} = rpc(mim(), mongoose_cets_discovery_rdbms, get_nodes, [State2]),
    %% "test2" node can see "test1"
    lists:member(test1, Nodes).
