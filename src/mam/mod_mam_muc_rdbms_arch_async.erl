-module(mod_mam_muc_rdbms_arch_async).

-behaviour(mongoose_batch_worker).

-include("mongoose_logger.hrl").

-define(PER_MESSAGE_FLUSH_TIME, [mod_mam_muc_rdbms_async_pool_writer, per_message_flush_time]).
-define(FLUSH_TIME, [mod_mam_muc_rdbms_async_pool_writer, flush_time]).

-behaviour(gen_mod).
-export([start/2, stop/1, hooks/1, supported_features/0]).
-export([archive_muc_message/3, mam_muc_archive_sync/3]).
-export([flush/2]).
-ignore_xref([flush/2]).

-spec archive_muc_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {ok, mod_mam:lookup_result()},
    Params :: map(),
    Extra :: gen_hook:extra().
archive_muc_message(Result,
                    #{archive_id := RoomID} = Params0,
                    #{host_type := HostType}) ->
    Params = mod_mam_muc_rdbms_arch:extend_params_with_sender_id(HostType, Params0),
    mongoose_async_pools:put_task(HostType, muc_mam, RoomID, Params),
    {ok, Result}.

-spec mam_muc_archive_sync(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: map(),
    Extra :: gen_hook:extra().
mam_muc_archive_sync(Result, _Params, #{host_type := HostType}) ->
    mongoose_async_pools:sync(HostType, muc_mam),
    {ok, Result}.

%%% gen_mod callbacks
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    {PoolOpts, Extra} = mod_mam_rdbms_arch_async:make_pool_opts(muc, Opts),
    mod_mam_rdbms_arch_async:prepare_insert_queries(muc, Extra),
    mongoose_metrics:ensure_metric(HostType, ?PER_MESSAGE_FLUSH_TIME, histogram),
    mongoose_metrics:ensure_metric(HostType, ?FLUSH_TIME, histogram),
    mongoose_async_pools:start_pool(HostType, muc_mam, PoolOpts).

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    mongoose_async_pools:stop_pool(HostType, muc_mam).

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
        {mam_muc_archive_sync, HostType, fun ?MODULE:mam_muc_archive_sync/3, #{}, 50},
        {mam_muc_archive_message, HostType, fun ?MODULE:archive_muc_message/3, #{}, 50}
    ].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%% flush callbacks
flush(Acc, Extra = #{host_type := HostType, queue_length := MessageCount}) ->
    {FlushTime, Result} = timer:tc(fun do_flush_muc/2, [Acc, Extra]),
    mongoose_metrics:update(HostType, ?PER_MESSAGE_FLUSH_TIME, round(FlushTime / MessageCount)),
    mongoose_metrics:update(HostType, ?FLUSH_TIME, FlushTime),
    Result.

%% mam workers callbacks
do_flush_muc(Acc, #{host_type := HostType, queue_length := MessageCount,
                    batch_size := MaxSize, batch_name := BatchName}) ->
    Rows = [mod_mam_muc_rdbms_arch:prepare_message(HostType, Params) || Params <- Acc],
    InsertResult =
        case MessageCount of
            MaxSize ->
                mongoose_rdbms:execute(HostType, BatchName, lists:append(Rows));
            OtherSize ->
                Results = [mongoose_rdbms:execute(HostType, insert_mam_muc_message, Row) || Row <- Rows],
                case lists:keyfind(error, 1, Results) of
                    false -> {updated, OtherSize};
                    Error -> Error
                end
        end,
    case InsertResult of
        {updated, _Count} -> ok;
        {error, Reason} ->
            mongoose_metrics:update(HostType, modMucMamDropped, MessageCount),
            ?LOG_ERROR(#{what => archive_message_query_failed,
                         text => <<"archive_message query failed, modMucMamDropped metric updated">>,
                         message_count => MessageCount, reason => Reason}),
            ok
    end,
    [mod_mam_muc_rdbms_arch:retract_message(HostType, Params) || Params <- Acc],
    mongoose_hooks:mam_muc_flush_messages(HostType, MessageCount),
    ok.
