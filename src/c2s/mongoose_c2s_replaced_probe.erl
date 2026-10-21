%%% @doc Diagnostics for sessions that were replaced by a new connection.
%%%
%%% When a new session replaces older ones, {@link ejabberd_sm} asks the old
%%% processes to exit and returns their pids. `mongoose_c2s' arms a
%%% `replaced_wait_timeout' timer with that list and, once it fires, checks
%%% whether those processes really went away. That check exists only to tell an
%%% operator that some old session is frozen - it is not part of authentication,
%%% authorisation or session replacement, and it must never influence the new
%%% connection in any way.
%%%
%%% It used to be `rpc:call(node(Pid), erlang, is_process_alive, [Pid])' matched
%%% against `true' and `false' only. During a rolling shutdown the node hosting
%%% the old session is already gone by the time the timer fires, the call
%%% answers `{badrpc, nodedown}', and the missing clause raised `case_clause'
%%% inside the `gen_statem' callback - killing a brand new, fully authenticated
%%% connection that had nothing to do with the old node.
%%%
%%% Rules this module keeps:
%%% <ul>
%%%  <li>every answer is classified, there is no unmatched clause;</li>
%%%  <li>"the node cannot be reached" means "unknown", never "the process is
%%%      dead" and never "the process is alive";</li>
%%%  <li>remote RPC waits share one timeout budget (`?BATCH_BUDGET_MS'),
%%%      rather than a fresh timeout per pid; disconnected nodes are not called;</li>
%%%  <li>nothing escapes: {@link verify/2} catches its own errors, so a bug here
%%%      can never take a connection down;</li>
%%%  <li>logs carry the event, the remote nodes, the outcome categories and the
%%%      counts - never `c2s_data', acc, packets, tokens or any other runtime
%%%      payload - and the outcome that a mass shutdown produces (`unreachable')
%%%      is logged at debug level, avoiding per-session info or warning logs
%%%      during a rolling deploy when debug logging is disabled.</li>
%%% </ul>
-module(mongoose_c2s_replaced_probe).

-include("mongoose_logger.hrl").

-export([verify/2, probe/1, probe/2]).

-ignore_xref([probe/1, probe/2]).

%% Shared timeout budget for remote RPC waits in one batch. As with erpc
%% timeouts generally, scheduling and distribution backpressure can delay the
%% return; this is not a hard wall-clock limit on the calling process.
-define(BATCH_BUDGET_MS, 1000).
%% Replacement normally yields a single pid. Cap the batch anyway so that a
%% pathological session table can never turn one timer into unbounded work.
-define(MAX_REMOTE_PIDS, 16).

-type status() :: alive        % confirmed still running
                | dead         % confirmed gone, the expected outcome
                | unreachable  % node is not connected or dropped: unknown
                | timeout      % node did not answer within the budget: unknown
                | error        % unexpected answer or remote exception: unknown
                | skipped.     % batch cap reached, deliberately not asked
-type result() :: {pid(), status()}.
-type opts() :: #{budget => non_neg_integer(), max_remote => non_neg_integer()}.

-export_type([status/0, result/0, opts/0]).

%% @doc Probe the replaced pids and log the outcome. Always returns `ok'.
-spec verify([pid()], mongoose_c2s:state()) -> ok.
verify([], _C2SState) ->
    ok;
verify(ReplacedPids, C2SState) ->
    try
        log_results(probe(ReplacedPids), C2SState)
    catch
        Class:Reason:Stacktrace ->
            %% This is a diagnostic path: if the diagnostic itself is broken we
            %% report it and carry on. The connection is never affected.
            ?LOG_INFO(#{what => c2s_replaced_probe_failed,
                        text => <<"Failed to verify replaced sessions">>,
                        class => Class, reason => Reason, stacktrace => Stacktrace})
    end,
    ok.

%% @doc Classify replaced pids, sharing one timeout budget across remote RPCs.
%% Invalid input or internal failures are contained by verify/2. Only explicit
%% remote answers are classified as alive or dead.
-spec probe([pid()]) -> [result()].
probe(Pids) ->
    probe(Pids, #{}).

-spec probe([pid()], opts()) -> [result()].
probe(Pids, Opts) ->
    Budget = maps:get(budget, Opts, ?BATCH_BUDGET_MS),
    MaxRemote = maps:get(max_remote, Opts, ?MAX_REMOTE_PIDS),
    {Local, Remote} = lists:partition(fun(Pid) -> node(Pid) =:= node() end, lists:usort(Pids)),
    [{Pid, local_status(Pid)} || Pid <- Local] ++ probe_remote(Remote, Budget, MaxRemote).

-spec local_status(pid()) -> alive | dead.
local_status(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> alive;
        false -> dead
    end.

-spec probe_remote([pid()], non_neg_integer(), non_neg_integer()) -> [result()].
probe_remote([], _Budget, _MaxRemote) ->
    [];
probe_remote(Pids, Budget, MaxRemote) ->
    {Probed, Skipped} = lists:split(min(MaxRemote, length(Pids)), Pids),
    Deadline = erlang:monotonic_time(millisecond) + Budget,
    ByNode = maps:to_list(maps:groups_from_list(fun erlang:node/1, Probed)),
    probe_nodes(ByNode, Deadline, [{Pid, skipped} || Pid <- Skipped]).

-spec probe_nodes([{node(), [pid()]}], integer(), [result()]) -> [result()].
probe_nodes([], _Deadline, Acc) ->
    Acc;
probe_nodes([{Node, Pids} | Rest], Deadline, Acc) ->
    TimeLeft = Deadline - erlang:monotonic_time(millisecond),
    probe_nodes(Rest, Deadline, probe_node(Node, Pids, TimeLeft) ++ Acc).

-spec probe_node(node(), [pid()], integer()) -> [result()].
probe_node(_Node, Pids, TimeLeft) when TimeLeft =< 0 ->
    %% The batch budget is spent. We do not know, and we will not block to find out.
    [{Pid, timeout} || Pid <- Pids];
probe_node(Node, Pids, TimeLeft) when TimeLeft > 0 ->
    case lists:member(Node, nodes(connected)) of
        false ->
            %% The common case during a rolling shutdown: the node that hosted the
            %% old session is already gone. Skipping the call keeps this free and
            %% keeps a c2s event loop out of distribution connection setup, and
            %% "not connected" is not evidence about the process either way.
            [{Pid, unreachable} || Pid <- Pids];
        true ->
            %% One round trip per node, one shared deadline, results in pid order.
            %% `fun erlang:is_process_alive/1' is an external fun, so it resolves
            %% on the remote node and works across mixed code versions.
            %% One node in, so one answer out - but even that is matched, not assumed.
            case erpc:multicall([Node], lists, map,
                                [fun erlang:is_process_alive/1, Pids], TimeLeft) of
                [Answer] -> interpret(Answer, Pids);
                Unexpected -> interpret(Unexpected, Pids)
            end
    end.

%% Total by construction: anything that is not a well formed answer is `error'.
-spec interpret(term(), [pid()]) -> [result()].
interpret({ok, Alive}, Pids) when is_list(Alive), length(Alive) =:= length(Pids) ->
    lists:zipwith(fun(Pid, true) -> {Pid, alive};
                     (Pid, false) -> {Pid, dead};
                     (Pid, _) -> {Pid, error}
                  end, Pids, Alive);
interpret({error, {erpc, noconnection}}, Pids) ->
    [{Pid, unreachable} || Pid <- Pids];
interpret({error, {erpc, timeout}}, Pids) ->
    [{Pid, timeout} || Pid <- Pids];
interpret(_Other, Pids) ->
    %% Remote exception, remote exit, an erpc reason we do not know, or a badly
    %% shaped answer. Unknown is unknown - it is never reported as dead.
    [{Pid, error} || Pid <- Pids].

-spec log_results([result()], mongoose_c2s:state()) -> ok.
log_results(Results, C2SState) ->
    Counts = log_counts(Results),
    log_alive(pids_with_status(alive, Results), Counts, C2SState),
    log_unknown(unknown_results(Results), Counts, C2SState).

%% The original diagnostic: some old session did not react to being replaced.
%% Rare, and `replaced_wait_timeout' is the phrase documented for operators.
-spec log_alive([pid()], map(), mongoose_c2s:state()) -> ok.
log_alive([], _Counts, _C2SState) ->
    ok;
log_alive(AlivePids, Counts, C2SState) ->
    ?LOG_WARNING(Counts#{what => c2s_replaced_wait_timeout,
                         text => <<"Some processes are not responding when handling replace messages">>,
                         replaced_pids => AlivePids,
                         replaced_nodes => nodes_of(AlivePids),
                         state_name => C2SState}),
    ok.

%% We asked but got no answer. `unreachable' is the expected outcome of a rolling
%% shutdown, so it stays at debug level and cannot storm the logs; a connected
%% node that times out or misbehaves is rare and worth an info line.
-spec log_unknown([result()], map(), mongoose_c2s:state()) -> ok.
log_unknown([], _Counts, _C2SState) ->
    ok;
log_unknown(Unknown, Counts, C2SState) ->
    Report = Counts#{what => c2s_replaced_probe_inconclusive,
                     text => <<"Could not confirm whether replaced sessions exited">>,
                     replaced_nodes => nodes_of([Pid || {Pid, _} <- Unknown]),
                     state_name => C2SState},
    case [Pid || {Pid, Status} <- Unknown, Status =/= unreachable] of
        [] -> ?LOG_DEBUG(Report);
        _ -> ?LOG_INFO(Report)
    end,
    ok.

%% Everything we could not confirm one way or the other.
-spec unknown_results([result()]) -> [result()].
unknown_results(Results) ->
    [R || {_Pid, Status} = R <- Results, Status =/= alive, Status =/= dead].

-spec pids_with_status(status(), [result()]) -> [pid()].
pids_with_status(Status, Results) ->
    [Pid || {Pid, S} <- Results, S =:= Status].

-spec nodes_of([pid()]) -> [node()].
nodes_of(Pids) ->
    lists:usort([node(Pid) || Pid <- Pids]).

%% Fixed set of keys, so the shape of the log line never depends on the outcome.
-spec log_counts([result()]) -> map().
log_counts(Results) ->
    Zeroes = #{alive_count => 0, dead_count => 0, unreachable_count => 0,
               timeout_count => 0, error_count => 0, skipped_count => 0},
    lists:foldl(fun({_Pid, Status}, Acc) ->
                        Key = count_key(Status),
                        maps:update_with(Key, fun(N) -> N + 1 end, 1, Acc)
                end, Zeroes, Results).

-spec count_key(status()) -> atom().
count_key(alive) -> alive_count;
count_key(dead) -> dead_count;
count_key(unreachable) -> unreachable_count;
count_key(timeout) -> timeout_count;
count_key(error) -> error_count;
count_key(skipped) -> skipped_count.
