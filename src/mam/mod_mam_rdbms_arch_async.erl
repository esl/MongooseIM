-module(mod_mam_rdbms_arch_async).

-behaviour(mongoose_batch_worker).

-include("mongoose_logger.hrl").

-define(PER_MESSAGE_FLUSH_TIME, [mod_mam_rdbms_async_pool_writer, per_message_flush_time]).
-define(FLUSH_TIME, [mod_mam_rdbms_async_pool_writer, flush_time]).

-behaviour(gen_mod).

-export([start/2, stop/1, hooks/1, supported_features/0]).
-export([archive_pm_message/3, mam_archive_sync/3]).
-export([flush/2]).

-export([make_pool_opts/2, prepare_insert_queries/2]).

-spec archive_pm_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: mod_mam:archive_message_params(),
    Extra :: gen_hook:extra().
archive_pm_message(_Result, #{archive_id := ArcID} = Params, #{host_type := HostType}) ->
    {ok, mongoose_async_pools:put_task(HostType, pm_mam, ArcID, Params)}.

-spec mam_archive_sync(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: map(),
    Extra :: gen_hook:extra().
mam_archive_sync(Result, _Params, #{host_type := HostType}) ->
    mongoose_async_pools:sync(HostType, pm_mam),
    {ok, Result}.

%%% gen_mod callbacks
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    {PoolOpts, Extra} = make_pool_opts(pm, Opts),
    prepare_insert_queries(pm, Extra),
    mongoose_metrics:ensure_metric(HostType, ?PER_MESSAGE_FLUSH_TIME, histogram),
    mongoose_metrics:ensure_metric(HostType, ?FLUSH_TIME, histogram),
    mongoose_async_pools:start_pool(HostType, pm_mam, PoolOpts).

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    mongoose_async_pools:stop_pool(HostType, pm_mam).

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
        {mam_archive_sync, HostType, fun ?MODULE:mam_archive_sync/3, #{}, 50},
        {mam_archive_message, HostType, fun ?MODULE:archive_pm_message/3, #{}, 50}
    ].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%% internal callbacks
-spec make_pool_opts(mod_mam:mam_type(), gen_mod:module_opts()) ->
          {mongoose_async_pools:pool_opts(), mongoose_async_pools:pool_extra()}.
make_pool_opts(Type, Opts) ->
    Extra = add_batch_name(Type, Opts),
    PoolOpts = Extra#{pool_type => batch,
                      flush_callback => flush_callback(Type),
                      flush_extra => Extra},
    {PoolOpts, Extra}.

%% Put batch_size into a statement name, so we could survive the module restarts
%% with different batch sizes
add_batch_name(pm, #{batch_size := MaxSize} = Opts) ->
    Opts#{batch_name => multi_name(insert_mam_messages, MaxSize)};
add_batch_name(muc, #{batch_size := MaxSize} = Opts) ->
    Opts#{batch_name => multi_name(insert_mam_muc_messages, MaxSize)}.

flush_callback(pm) -> fun ?MODULE:flush/2;
flush_callback(muc) -> fun mod_mam_muc_rdbms_arch_async:flush/2.

prepare_insert_queries(pm, #{batch_size := MaxSize, batch_name := BatchName}) ->
    mod_mam_rdbms_arch:prepare_insert(insert_mam_message, 1),
    mod_mam_rdbms_arch:prepare_insert(BatchName, MaxSize);
prepare_insert_queries(muc, #{batch_size := MaxSize, batch_name := BatchName}) ->
    mod_mam_muc_rdbms_arch:prepare_insert(insert_mam_muc_message, 1),
    mod_mam_muc_rdbms_arch:prepare_insert(BatchName, MaxSize).

multi_name(Name, Times) ->
    list_to_atom(atom_to_list(Name) ++ integer_to_list(Times)).

%%% flush callbacks
flush(Acc, Extra = #{host_type := HostType, queue_length := MessageCount}) ->
    {FlushTime, Result} = timer:tc(fun do_flush_pm/2, [Acc, Extra]),
    mongoose_metrics:update(HostType, ?PER_MESSAGE_FLUSH_TIME, round(FlushTime / MessageCount)),
    mongoose_metrics:update(HostType, ?FLUSH_TIME, FlushTime),
    Result.

%% mam workers callbacks
do_flush_pm(Acc, #{host_type := HostType, queue_length := MessageCount,
                   batch_size := MaxSize, batch_name := BatchName}) ->
    Rows = [mod_mam_rdbms_arch:prepare_message(HostType, Params) || Params <- Acc],
    IsFullBuffer = MessageCount =:= MaxSize,
    case IsFullBuffer of
        true ->
            Result = mongoose_rdbms:execute(HostType, BatchName, lists:append(Rows)),
            process_batch_result(Result, Acc, HostType, MessageCount);
        false ->
            Results = [mongoose_rdbms:execute(HostType, insert_mam_message, Row) || Row <- Rows],
            Process = lists:zip(Results, Acc),
            process_list_results(Process, HostType)
    end,
    [mod_mam_rdbms_arch:retract_message(HostType, Params) || Params <- Acc],
    mongoose_hooks:mam_flush_messages(HostType, MessageCount),
    ok.

process_batch_result({updated, _Count}, _, _, _) ->
    ok;
process_batch_result({error, Reason}, Rows, HostType, MessageCount) ->
    mongoose_metrics:update(HostType, modMamDropped, MessageCount),
    Keys = [ maps:with([message_id, archive_id], Row) || Row <- Rows ],
    ?LOG_ERROR(#{what => archive_message_failed,
                 text => <<"archive_message batch query failed">>,
                 keys => Keys, message_count => MessageCount, reason => Reason}),
    ok.

process_list_results(Results, HostType) ->
    lists:foreach(fun(R) -> process_single_result(R, HostType) end, Results).

process_single_result({{updated, _Count}, _}, _HostType) ->
    ok;
process_single_result({{error, Reason}, #{message_id := MsgId, archive_id := ArcId}}, HostType) ->
    mongoose_metrics:update(HostType, modMamDropped, 1),
    ?LOG_ERROR(#{what => archive_message_failed,
                 text => <<"archive_message batch query failed">>,
                 message_id => MsgId, archive_id => ArcId, reason => Reason}).
