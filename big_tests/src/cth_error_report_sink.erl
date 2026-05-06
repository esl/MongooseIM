-module(cth_error_report_sink).
-moduledoc """
Test-runner-side sink for error log entries pushed from MIM nodes.

Owns one ETS table keyed by a global monotonic sequence. Receives
`{log_entry, Node, Level, Msg, Meta}` messages from `log_error_collector`
handlers running on MIM nodes.

Survives across all suites in a CT run; cleared at each
`pre_init_per_suite` via `clear/0`.
""".
-behaviour(gen_server).

-export([start_link/0, stop/0,
         mark/0, get_after/1, clear/0,
         watch_nodes/2, inject/1, unwatch/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

-define(NAME, ?MODULE).

-type spec() :: distributed_helper:rpc_spec().
-type level() :: log_error_collector:level().
%% Storage shape: a single log event captured from a MIM node.
%% Same arity as the wire format `{log_entry, Node, Level, Msg, Meta}'
%% but without the leading tag.
-type log_entry() :: {Node :: node(),
                      Level :: log_error_collector:level(),
                      Msg :: log_error_collector:msg(),
                      Meta :: log_error_collector:meta()}.

-export_type([log_entry/0]).

-record(state, {
    seq = 0 :: non_neg_integer(),
    table :: ets:tid() | atom(),
    %% Nodes where a handler has been (re-)injected. Used by
    %% unwatch/0 for best-effort handler removal at suite end.
    watched = #{} :: #{node() => spec()},
    levels = [error] :: [level()]
}).

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?NAME).

-doc """
Returns the current sequence number. Entries inserted after this call
have a strictly greater sequence.
""".
-spec mark() -> non_neg_integer().
mark() ->
    gen_server:call(?NAME, mark).

-doc """
Returns `{Entries, NewMark}` for entries with sequence strictly greater
than `Mark`. Entries are returned in sequence order (oldest first).
""".
-spec get_after(non_neg_integer()) -> {[log_entry()], non_neg_integer()}.
get_after(Mark) ->
    gen_server:call(?NAME, {get_after, Mark}).

-doc "Drops all stored entries and resets the sequence to 0.".
-spec clear() -> ok.
clear() ->
    gen_server:call(?NAME, clear).

-doc """
Declares which nodes the sink should watch and injects the
`log_error_collector` handler on each of them. Re-injection after
a node restart is the responsibility of the restart caller — see
`inject/1`; `distributed_helper:start_node/2` calls it.
""".
-spec watch_nodes([{atom(), spec()}], [level()]) -> ok.
watch_nodes(Specs, Levels) ->
    gen_server:call(?NAME, {watch_nodes, Specs, Levels}).

-doc """
(Re-)injects the `log_error_collector` handler on the given node.
Called by node-restart helpers (e.g. `distributed_helper:start_node/2`)
after a node has come back up so error collection resumes.
""".
-spec inject(spec()) -> ok.
inject(Spec) ->
    gen_server:call(?NAME, {inject, Spec}).

-doc """
Stops watching all nodes; performs best-effort handler removal on
nodes that are still up.
""".
-spec unwatch() -> ok.
unwatch() ->
    gen_server:call(?NAME, unwatch).

%% gen_server

init([]) ->
    Tab = ets:new(?MODULE, [ordered_set, private]),
    {ok, #state{table = Tab}}.

handle_call(mark, _From, #state{seq = Seq} = State) ->
    {reply, Seq, State};
handle_call({get_after, Mark}, _From, #state{table = T, seq = Seq} = State) ->
    Entries = ets:select(T, [{ {'$1', '$2'}, [{'>', '$1', {const, Mark}}], ['$2'] }]),
    {reply, {Entries, Seq}, State};
handle_call(clear, _From, #state{table = T} = State) ->
    %% NOTE: in-flight {log_entry, ...} messages already in the
    %% mailbox at this point are still drained after the clear and
    %% will land under sequence numbers >0. Per-suite boundaries are
    %% quiet (no triggered work between the previous post_end_per_suite
    %% and this call), so the practical risk of an entry being
    %% mis-attributed to the next suite is negligible. If that ever
    %% becomes a problem, switch to a generation counter checked in
    %% the {log_entry, ...} handler.
    ets:delete_all_objects(T),
    {reply, ok, State#state{seq = 0}};
handle_call({watch_nodes, Specs, Levels}, _From, State) ->
    lists:foreach(fun({_Key, Spec}) -> do_inject(Spec, Levels) end, Specs),
    Watched = #{Node => Spec || {_Key, #{node := Node} = Spec} <- Specs},
    {reply, ok, State#state{watched = Watched, levels = Levels}};
handle_call({inject, #{node := Node} = Spec}, _From,
            #state{watched = Watched, levels = Levels} = State) ->
    do_inject(Spec, Levels),
    {reply, ok, State#state{watched = Watched#{Node => Spec}}};
handle_call(unwatch, _From, #state{watched = Watched} = State) ->
    maps:foreach(fun(_Node, Spec) -> do_remove(Spec) end, Watched),
    {reply, ok, State#state{watched = #{}}};
handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({log_entry, Node, Level, Msg, Meta},
            #state{table = T, seq = Seq} = State) ->
    NewSeq = Seq + 1,
    Entry = {Node, Level, Msg, Meta},
    ets:insert(T, {NewSeq, Entry}),
    {noreply, State#state{seq = NewSeq}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal: injection / removal happen synchronously inside the
%% gen_server call so the caller is guaranteed the handler is
%% attached (or failure logged) before inject/1 or watch_nodes/2
%% returns. Test setup is sequential -- there is no concurrency to
%% serve here, and correctness beats parallelism.

-spec do_inject(spec(), [level()]) -> ok.
do_inject(#{node := Node} = Spec, Levels) ->
    try
        mongoose_helper:inject_module(Spec, log_error_collector, no_reload),
        SinkRef = {?NAME, node()},
        _ = distributed_helper:rpc(Spec, log_error_collector, start, [Levels, SinkRef]),
        ok
    catch Class:Reason:Stack ->
        ct:pal("cth_error_report_sink: failed to inject handler on ~p: ~p:~p~n~p",
               [Node, Class, Reason, Stack]),
        ok
    end.

-spec do_remove(spec()) -> ok.
do_remove(Spec) ->
    try distributed_helper:rpc(Spec, log_error_collector, stop, [])
    catch _:_ -> ok
    end,
    ok.
