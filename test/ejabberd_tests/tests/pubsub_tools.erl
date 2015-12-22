%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @Tools module - pubsub specific tools and high level
%%% @               wrappers for the escalus tool.
%%% @end
%%%===================================================================

-module(pubsub_tools).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
         create_node/3,
         delete_node/3
        ]).

%%-----------------------------------------------------------------------------
%% API: pubsub tools
%%-----------------------------------------------------------------------------

create_node(User, NodeAddr, NodeName) ->
    Id = <<"create1">>,
    CreateNodeIq = escalus_pubsub_stanza:create_node_stanza(
                       User, Id, NodeAddr, NodeName),
    log_stanza("REQUEST create node", CreateNodeIq),
    escalus:send(User, CreateNodeIq),
    {true, _ResultStanza} = wait_for_stanza_and_match_result_iq(User, Id, NodeAddr).

delete_node(User, NodeAddr, NodeName) ->
    Id = <<"delete1">>,
    DeleteNode = escalus_pubsub_stanza:delete_node_stanza(NodeName),
    DeleteNodeIq = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [DeleteNode]),
    log_stanza("REQUEST delete node", DeleteNodeIq),
    escalus:send(User, DeleteNodeIq),
    {true, _ResultStanza} = wait_for_stanza_and_match_result_iq(User, Id, NodeAddr).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

%% Checks that:
%% - IQ from server matches the sent id
%% - there is "result"
%% - sender is correct.
wait_for_stanza_and_match_result_iq(User, Id, DestinationNode) ->
    ResultStanza = escalus:wait_for_stanza(User),
    log_stanza("RESPONSE", ResultStanza),

    QueryStanza = escalus_stanza:iq_with_type_id_from(<<"result">>, Id, DestinationNode),
    Result = escalus_pred:is_iq_result(QueryStanza, ResultStanza),

    {Result, ResultStanza}.

log_stanza(ReportString, Stanza) ->
    PrettyStanza = binary:list_to_bin(exml:to_pretty_iolist(Stanza)),
    ct:print("~s~n~s", [ReportString, PrettyStanza]),
    ct:log("~s~n~s", [ReportString, exml:escape_attr(PrettyStanza)]).
