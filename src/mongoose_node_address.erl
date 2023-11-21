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
-ignore_xref([get_pairs/0, start_link/0]).

%% For testing only
-export([lookup_loop/4]).
-ignore_xref([lookup_loop/4]).

-include_lib("kernel/include/logger.hrl").

-type state() :: #{}.

-type lookup_error() ::
    {no_ip_in_db, node()}
  | {cannot_connect_to_epmd, node(), inet:ip_address()}
  | {no_record_for_node, node()}.

-type pairs() :: [{node(), Address :: binary()}].
-type pairs_result() :: pairs() | {badrpc, Reason :: term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Store addresses extracted by CETS disco from RDBMS
%% We also would send our IPs to other nodes on nodeup
-spec remember_addresses(pairs()) -> ok.
remember_addresses(Pairs) ->
    try
        ets:insert(?MODULE, Pairs),
        ok
    catch _:Error ->
        ?LOG_ERROR(#{what => remember_addresses_failed, reason => Error, pairs => Pairs}),
        ok
    end.

%% This function waits for the IP address to appear.
%% It only works well in case net_kernel does not contact a lot of
%% dead nodes.
%% Generally, we only expect calls for nodes that are alive
%% and the connect is issued by CETS (but CETS checks that node is
%% reachable first) or by connect_all feature in the global module of OTP.
-spec lookup(node()) -> {ok, inet:ip_address()} | {error, lookup_error()}.
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
            {error, {no_ip_in_db, Node}};
        [{Node, Bin}] ->
            {ok, IP} = inet:parse_address(binary_to_list(Bin)),
            case can_connect(IP) of
                true ->
                    {ok, IP};
                false ->
                    maybe_retry(Node, Start, Timeout, Sleep,
                               {cannot_connect_to_epmd, Node, IP})
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

%% gen_server callbacks
-spec init(term()) -> {ok, state()}.
init(_Opts) ->
    ets:new(?MODULE, [named_table, public]),
    net_kernel:monitor_nodes(true),
    {ok, #{}}.

handle_call(Msg, From, State) ->
    ?LOG_ERROR(#{what => unexpected_call, msg => Msg, from => From}),
    {reply, {error, unexpected_call}, State}.

handle_cast(Msg, State) ->
    ?LOG_ERROR(#{what => unexpected_cast, msg => Msg}),
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    {noreply, handle_nodeup(Node, State)};
handle_info({nodedown, _Node}, State) ->
    {noreply, State};
handle_info({get_pairs_result, Node, Res}, State) ->
    {noreply, handle_get_pairs_result(Node, Res, State)};
handle_info(Msg, State) ->
    ?LOG_ERROR(#{what => unexpected_info, msg => Msg}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% There is a chance nodeup could come from a non-mongooseim node
-spec handle_nodeup(node(), state()) -> state().
handle_nodeup(Node, State = #{}) ->
    Server = self(),
    %% There could be some time spent between nodeup and us getting the result
    %% from the RPC.
    %% We could use erpc:send_request/4 and avoid spawn, but the return result is
    %% undocumented (so, we cannot match in in handle_info)
    spawn_link(fun() ->
            %% We have to ignore our node name when processing result
            Server ! {get_pairs_result, Node, rpc:call(Node, ?MODULE, get_pairs, [])},
            ok
        end),
    State.

-spec get_pairs() -> pairs().
get_pairs() ->
    Nodes = [node() | nodes()],
    [Pair || {Node, _} = Pair <- ets:tab2list(?MODULE), lists:member(Node, Nodes)].

-spec handle_get_pairs_result(node(), pairs_result(), state()) -> state().
handle_get_pairs_result(_RemoteNode, Pairs, State) when is_list(Pairs) ->
    %% Ignore our node name in the result
    Pairs2 = lists:keydelete(node(), 1, Pairs),
    remember_addresses(Pairs2),
    State;
handle_get_pairs_result(_Node, _Res, State) ->
    State.

-spec can_connect(inet:ip_address()) -> boolean().
can_connect(IP) ->
    Timeout = 1000,
    case open(IP, Timeout) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        _ ->
            false
    end.

-spec get_epmd_port() -> inet:port_number().
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
