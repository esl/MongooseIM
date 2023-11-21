-module(mongoose_node_address).
-behaviour(gen_server).

-export([start_link/0, lookup/1, remember_addresses/1, get_pairs/0]).

%% gen_server hooks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ignore_xref([
get_pairs/0,
start_link/0
]).

-include_lib("kernel/include/logger.hrl").

-type state() :: #{
    waiting_for_nodes := [{node(), reference()}]
}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(term()) -> {ok, state()}.
init(_Opts) ->
    ets:new(?MODULE, [named_table, public]),
    net_kernel:monitor_nodes(true),
    {ok, #{waiting_for_nodes => []}}.

handle_call(Msg, From, State) ->
    ?LOG_ERROR(#{what => unexpected_call, msg => Msg, from => From}),
    {reply, {error, unexpected_call}, State}.

handle_cast(Msg, State) ->
    ?LOG_ERROR(#{what => unexpected_cast, msg => Msg}),
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    {noreply, handle_nodeup(Node, State)};
handle_info({nodedown, Node}, State) ->
    {noreply, handle_nodedown(Node, State)};
handle_info({get_pairs_result, Ref, Node, Res}, State) ->
    {noreply, handle_get_pairs_result(Ref, Node, Res, State)};
handle_info(Msg, State) ->
    ?LOG_ERROR(#{what => unexpected_info, msg => Msg}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Store addresses extracted by CETS disco from RDBMS
%% We also would send our IPs to other nodes on nodeup
-spec remember_addresses([{node(), binary()}]) -> ok.
remember_addresses(Pairs) ->
    try
        ets:insert(?MODULE, Pairs),
        ok
    catch _:Error ->
        ?LOG_ERROR(#{what => remember_addresses_failed, reason => Error, pairs => Pairs}),
        ok
    end.

can_connect(IP) ->
    Timeout = 1000,
    case open(IP, Timeout) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        _ ->
            false
    end.

get_epmd_port() ->
    case init:get_argument(epmd_port) of
        {ok, [[PortStr|_]|_]} when is_list(PortStr) ->
            list_to_integer(PortStr);
        error ->
            4369
    end.

open({_, _, _, _} = IP, Timeout) ->
    %% That would block
    gen_tcp:connect(IP, get_epmd_port(), [inet], Timeout);
open({_, _, _, _, _, _, _, _} = IP, Timeout) ->
    gen_tcp:connect(IP, get_epmd_port(), [inet6], Timeout).

%% This function waits for the IP address to appear.
%% It only works well in case net_kernel does not contact a lot of
%% dead nodes.
%% Generally, we only expect calls for nodes that are alive
%% and the connect is issued by CETS (but CETS checks that node is
%% reachable first) or by connect_all feature in the global module of OTP.
lookup(Node) ->
    Start = os:system_time(millisecond),
    Timeout = 3000,
    Sleep = 300,
    lookup_loop(Node, Start, Timeout, Sleep).

%% We have to check that we could use the IP
%% (i.e. we can at least to connect to it).
%% Because a newer IP could appear in DB.
%% There is no easy way to find if the ETS table is up to date.
%% We also do retries because:
%% - IP could be from the previous MongooseIM container.
%% - The remote node is not accessable yet for some networking reasons.
lookup_loop(Node, Start, Timeout, Sleep) ->
    case ets:lookup(?MODULE, Node) of
        [{Node, <<>>}] ->
            %% The caller should try DNS.
            {error, no_ip_in_db};
        [{Node, Bin}] ->
            case inet:parse_address(binary_to_list(Bin)) of
                {ok, IP} ->
                    case can_connect(IP) of
                        true ->
                            {ok, IP};
                        false ->
                            maybe_retry(Node, Start, Timeout, Sleep,
                                       {cannot_connect_to_epmd, Node, IP})
                    end
            end;
        [] ->
            maybe_retry(Node, Start, Timeout, Sleep,
                        {no_record_for_node, Node})
    end.

maybe_retry(Node, Start, Timeout, Sleep, Reason) ->
    Time = os:system_time(millisecond),
    case (Time - Start + Sleep) < Timeout of
        true ->
            lookup_loop(Node, Start, Timeout, Sleep);
        false ->
            {error, Reason}
    end.

%% There is a chance nodeup could come from a non-mongooseim node
handle_nodeup(Node, State = #{waiting_for_nodes := Waiting}) ->
    Server = self(),
    Ref = make_ref(),
    %% There could be some time spent between nodeup and us getting the result
    %% from the RPC.
    %% We could use erpc:send_request/4 and avoid spawn, but the return result is
    %% undocumented (so, we cannot match in in handle_info)
    spawn_link(fun() ->
            %% We have to ignore our node name when processing result
            Server ! {get_pairs_result, Ref, Node, rpc:call(Node, ?MODULE, get_pairs, [])},
            ok
        end),
    %% Only one get_pairs per node is allowed, ignore the old result, if it comes.
    State#{waiting_for_nodes := [{Node, Ref} | lists:keydelete(Node, 1, Waiting)]}.

handle_nodedown(Node, State = #{waiting_for_nodes := Waiting}) ->
    %% Stop waiting for get_pairs_result for the node
    Waiting2 = [Res || {WaitingNode, _Ref} = Res <- Waiting, WaitingNode =/= Node],
    State#{waiting_for_nodes := Waiting2}.

get_pairs() ->
    Nodes = [node() | nodes()],
    [Pair || {Node, _} = Pair <- ets:tab2list(?MODULE), lists:member(Node, Nodes)].

handle_get_pairs_result(Ref, Node, Res, State = #{waiting_for_nodes := Waiting}) ->
    case lists:member({Node, Ref}, Waiting) of
        true ->
            process_get_pairs_result(Node, Res),
            State#{waiting_for_nodes := lists:delete({Node, Ref}, Waiting)};
        false ->
            ?LOG_WARNING(#{what => unknown_ref_in_get_pairs_result,
                           text => <<"Could be a late response, if the remote nodes reconnects fast">>,
                           node => Node, reference => Ref, result => Res}),
            State
    end.

process_get_pairs_result(_Node, Pairs) when is_list(Pairs) ->
    %% Ignore our node name in the result
    Pairs2 = lists:keydelete(node(), 1, Pairs),
    remember_addresses(Pairs2);
process_get_pairs_result(Node, Other) ->
    ?LOG_WARNING(#{what => get_pairs_failed_on_node,
                   text => <<"We asked the remote node for the node list and addresses, but got an error.">>,
                   remote_node => Node, reason => Other}),
    ok.
