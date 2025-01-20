-module(local_iq_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4, subhost_pattern/1]).
-import(domain_helper, [host_type/0, secondary_host_type/0]).

suite() ->
    require_rpc_nodes([mim, mim2]).

all() ->
    [{group, iq_group}].

groups() ->
    [{iq_group, [], cases()}].

cases() ->
    [process_iq_works_across_multiple_nodes,
     process_iq_timeout].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    distributed_helper:add_node_to_cluster(mim2(), Config),
    mongoose_helper:inject_module(mim(), ?MODULE, reload),
    mongoose_helper:inject_module(mim2(), ?MODULE, reload),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(iq_group, Config) ->
    Config.

end_per_group(iq_group, Config) ->
    escalus_fresh:clean(),
    Config.

init_per_testcase(Testcase, Config) ->
    escalus:init_per_testcase(Testcase, Config).

end_per_testcase(Testcase, Config) ->
    escalus:end_per_testcase(Testcase, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

process_iq_works_across_multiple_nodes(Config) ->
    %% Alice connects to node1
    F = fun(Alice) ->
          route_iq(Alice, undefined),
          Stanza = escalus:wait_for_stanza(Alice),
          escalus:assert(is_iq_get, [], Stanza),
          escalus_client:send(Alice, escalus_stanza:iq_result(Stanza)),
          receive
              {result, _CbFrom, _CbTo, _CbAcc, _CbIQ} -> ok
          after 5000 -> ct:fail(timeout_waiting_for_result)
          end
        end,
    escalus:fresh_story(Config, [{alice, 1}], F).

process_iq_timeout(Config) ->
    F = fun(Alice) ->
          route_iq(Alice, 1),
          Stanza = escalus:wait_for_stanza(Alice),
          escalus:assert(is_iq_get, [], Stanza),
          receive
              {result, _CbFrom, _CbTo, _CbAcc, CbIQ} -> timeout = CbIQ
          after 5000 -> ct:fail(timeout_waiting_for_result)
          end
        end,
    escalus:fresh_story(Config, [{alice, 1}], F).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

route_iq(Alice, Timeout) ->
    Jid = escalus_client:full_jid(Alice),
    Server = escalus_client:server(Alice),
    To = jid:from_binary(Jid),
    From = jid:from_binary(Server),
    Query = #xmlel{name = <<"query">>,
                   attrs = #{<<"xmlns">> => <<"cooltestns">>}},
    Xml = #xmlel{name = <<"iq">>,
                 attrs = #{<<"type">> => <<"get">>,
                           <<"from">> => Server, <<"to">> => Jid},
                 children = [Query]},
    Acc = rpc(mim(), mongoose_acc, new,
              [#{location => #{}, lserver => Server, element => Xml}]),
    IQ = rpc(mim(), jlib, iq_query_info, [Xml]),
    Self = self(),
    Callback = fun(CbFrom, CbTo, CbAcc, CbIQ) ->
                       Self ! {result, CbFrom, CbTo, CbAcc, CbIQ}, CbAcc end,
    Args = [From, To, Acc, IQ, Callback, Timeout],
    rpc(mim2(), ejabberd_local, route_iq, Args).
