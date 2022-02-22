-module(mod_mam_muc_rdbms_arch_async).

-behaviour(mongoose_batch_worker).

-include("mongoose_logger.hrl").

-define(MUC_PER_MESSAGE_FLUSH_TIME, [mod_mam_muc_rdbms_async_pool_writer, per_message_flush_time]).
-define(MUC_FLUSH_TIME, [mod_mam_muc_rdbms_async_pool_writer, flush_time]).

-export([archive_muc_message/3, mam_muc_archive_sync/2, flush/2]).
-ignore_xref([archive_muc_message/3, mam_muc_archive_sync/2, flush/2]).

-spec archive_muc_message(_Result, mongooseim:host_type(), mod_mam:archive_message_params()) -> ok.
archive_muc_message(_Result, HostType, Params0 = #{archive_id := RoomID}) ->
    Params = mod_mam_muc_rdbms_arch:extend_params_with_sender_id(HostType, Params0),
    mongoose_async_pools:put_task(HostType, muc_mam, RoomID, Params).

-spec mam_muc_archive_sync(term(), mongooseim:host_type()) -> term().
mam_muc_archive_sync(Result, HostType) ->
    mongoose_async_pools:sync(HostType, muc_mam),
    Result.

%%% flush callbacks
flush(Acc, Extra = #{host_type := HostType, queue_length := MessageCount}) ->
    {FlushTime, Result} = timer:tc(fun do_flush_muc/2, [Acc, Extra]),
    mongoose_metrics:update(HostType, ?MUC_PER_MESSAGE_FLUSH_TIME, round(FlushTime / MessageCount)),
    mongoose_metrics:update(HostType, ?MUC_FLUSH_TIME, FlushTime),
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
