%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%%
%%% Some backends allow ArcID can be not set (riak?).
%%% This means, that there are no "jid => integer" mapping.
%%%
%%% But this module requires ArcID to be an integer.
%%% ArcID is set by mod_mam_rdbms_user.
%%% If ArcID is undefined, it means that there can be issues
%%% in mod_mam_rdbms_user.
%%%
%%% We have `is_integer(ArcID)' check on each hook handler, so cases when
%%% `ArcID' is undefined would fail at the module entrance.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_rdbms_async_pool_writer).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1, supported_features/0]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([archive_message/3]).

%% Helpers for debugging
-export([queue_length/1,
         queue_lengths/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([code_change/3, handle_cast/2, handle_info/2, init/1, queue_length/1,
              queue_lengths/1, terminate/2]).

-define(PER_MESSAGE_FLUSH_TIME, [?MODULE, per_message_flush_time]).
-define(FLUSH_TIME, [?MODULE, flush_time]).
-define(DEFAULT_POOL_SIZE, 32).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {
    flush_interval, %% milliseconds
    max_batch_size,
    host_type :: mongooseim:host_type(),
    number,
    acc = [],
    flush_interval_tref}).

worker_prefix() ->
    "ejabberd_mod_mam_writer".

%% Ensure, that:
%% `worker_count(_) = Int * mod_mam_rdbms_arch:partition_count()'
%%
%% For example,
%% `worker_count(_) = 32, partition_count() = 16'.
%% or
%% `worker_count(_) = 16, partition_count() = 16'.
worker_count(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, pool_size, ?DEFAULT_POOL_SIZE).

worker_names(HostType) ->
    [{N, worker_name(HostType, N)} || N <- lists:seq(0, worker_count(HostType) - 1)].

worker_name(HostType, N) ->
    list_to_atom(worker_prefix() ++ "_" ++ binary_to_list(HostType) ++ "_" ++ integer_to_list(N)).

select_worker(HostType, ArcID) ->
    N = worker_number(HostType, ArcID),
    worker_name(HostType, N).

worker_number(HostType, ArcID) when is_integer(ArcID) ->
    ArcID rem worker_count(HostType).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(HostType, Opts) ->
    mongoose_metrics:ensure_metric(HostType, ?PER_MESSAGE_FLUSH_TIME, histogram),
    mongoose_metrics:ensure_metric(HostType, ?FLUSH_TIME, histogram),
    MaxSize = gen_mod:get_module_opt(HostType, ?MODULE, max_batch_size, 30),
    mod_mam_rdbms_arch:prepare_insert(insert_mam_message, 1),
    mod_mam_rdbms_arch:prepare_insert(insert_mam_messages, MaxSize),

    start_workers(HostType, MaxSize),
    case gen_mod:get_module_opt(HostType, ?MODULE, pm, false) of
        true ->
            start_pm(HostType, Opts);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(HostType, ?MODULE, muc, false) of
        true ->
            start_muc(HostType, Opts);
        false ->
            ok
    end.

stop(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, pm, false) of
        true ->
            stop_pm(HostType);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(HostType, ?MODULE, muc, false) of
        true ->
            stop_muc(HostType);
        false ->
            ok
    end,
    stop_workers(HostType).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

start_pm(HostType, _Opts) ->
    ejabberd_hooks:add(mam_archive_message, HostType, ?MODULE, archive_message, 50),
    ok.

stop_pm(HostType) ->
    ejabberd_hooks:delete(mam_archive_message, HostType, ?MODULE, archive_message, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(HostType, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_message, HostType, ?MODULE, archive_message, 50),
    ok.

stop_muc(HostType) ->
    ejabberd_hooks:delete(mam_muc_archive_message, HostType, ?MODULE, archive_message, 50),
    ok.


%%====================================================================
%% API
%%====================================================================

start_workers(HostType, MaxSize) ->
    [start_worker(WriterProc, N, HostType, MaxSize)
     || {N, WriterProc} <- worker_names(HostType)].

stop_workers(HostType) ->
    [stop_worker(WriterProc) ||  {_, WriterProc} <- worker_names(HostType)].

start_worker(WriterProc, N, HostType, MaxSize) ->
    WriterChildSpec = worker_spec(WriterProc, N, HostType, MaxSize),
    supervisor:start_child(mod_mam_sup, WriterChildSpec).

worker_spec(WriterProc, N, HostType, MaxSize) ->
    %% Store incoming messages of the heap
    Opts = [{message_queue_data, off_heap}],
    Args = [{local, WriterProc}, ?MODULE, [HostType, N, MaxSize], Opts],
    {WriterProc,
     {gen_server, start_link, Args},
     permanent,
     5000,
     worker,
     [mod_mam_rdbms_async_pool_writer]}.

stop_worker(Proc) ->
    supervisor:terminate_child(mod_mam_sup, Proc),
    supervisor:delete_child(mod_mam_sup, Proc).


-spec archive_message(_Result, mongooseim:host_type(), mod_mam:archive_message_params()) ->
          ok | {error, timeout}.
archive_message(_Result, HostType, Params = #{archive_id := ArcID}) ->
    Worker = select_worker(HostType, ArcID),
    gen_server:cast(Worker, {archive_message, Params}).

%% For metrics.
queue_length(HostType) ->
    Len = lists:sum(queue_lengths(HostType)),
    {ok, Len}.

queue_lengths(HostType) ->
    [worker_queue_length(SrvName) || {_, SrvName} <- worker_names(HostType)].

worker_queue_length(SrvName) ->
    case whereis(SrvName) of
        undefined ->
            0;
        Pid ->
            {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
            Len
    end.

%%====================================================================
%% Internal functions
%%====================================================================

handle_archive_message(Params, State=#state{acc=Acc,
                         flush_interval_tref=TRef, flush_interval=Int,
                         max_batch_size=Max}) ->
    TRef2 = case {Acc, TRef} of
                {[], undefined} -> erlang:send_after(Int, self(), flush);
                {_, _} -> TRef
            end,
    State2 = State#state{acc=[Params|Acc], flush_interval_tref=TRef2},
    case length(Acc) + 1 >= Max of
        true -> run_flush(State2);
        false -> State2
    end.

run_flush(State = #state{acc = []}) ->
    State;
run_flush(State = #state{host_type = HostType, acc = Acc}) ->
    MessageCount = length(Acc),
    {FlushTime, NewState} = timer:tc(fun do_run_flush/2, [MessageCount, State]),
    mongoose_metrics:update(HostType, ?PER_MESSAGE_FLUSH_TIME, round(FlushTime / MessageCount)),
    mongoose_metrics:update(HostType, ?FLUSH_TIME, FlushTime),
    NewState.

do_run_flush(MessageCount, State = #state{host_type = HostType, max_batch_size = MaxSize,
                                          flush_interval_tref = TRef, acc = Acc}) ->
    cancel_and_flush_timer(TRef),
    ?LOG_DEBUG(#{what => mam_flush, message_count => MessageCount}),
    Rows = [mod_mam_rdbms_arch:prepare_message(HostType, Params) || Params <- Acc],
    InsertResult =
        case MessageCount of
            MaxSize ->
                mongoose_rdbms:execute(HostType, insert_mam_messages, lists:append(Rows));
            OtherSize ->
                Results = [mongoose_rdbms:execute(HostType, insert_mam_message, Row) || Row <- Rows],
                case lists:keyfind(error, 1, Results) of
                    false -> {updated, OtherSize};
                    Error -> Error
                end
        end,
    [mod_mam_rdbms_arch:retract_message(HostType, Params) || Params <- Acc],
    case InsertResult of
        {updated, _Count} -> ok;
        {error, Reason} ->
            mongoose_metrics:update(HostType, modMamDropped, MessageCount),
            ?LOG_ERROR(#{what => archive_message_failed,
                         text => <<"archive_message query failed">>,
                         message_count => MessageCount, reason => Reason}),
            ok
    end,
    mongoose_metrics:update(HostType, modMamFlushed, MessageCount),
    erlang:garbage_collect(),
    State#state{acc=[], flush_interval_tref=undefined}.

cancel_and_flush_timer(undefined) ->
    ok;
cancel_and_flush_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
        false ->
            receive
                flush -> ok
            after 0 -> ok
            end;
        _ -> ok
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([HostType, N, MaxSize]) ->
    %% Use a private RDBMS-connection.
    Int = gen_mod:get_module_opt(HostType, ?MODULE, flush_interval, 2000),
    {ok, #state{host_type = HostType, number = N,
                flush_interval = Int, max_batch_size = MaxSize}}.

handle_cast({archive_message, Params}, State) ->
    {noreply, handle_archive_message(Params, State)};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(flush, State) ->
    {noreply, run_flush(State#state{flush_interval_tref=undefined})};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
