-module(mod_mam_muc_rdbms_arch_async).

-behaviour(mongoose_batch_worker).

-include("mongoose_logger.hrl").

-behaviour(gen_mod).
-export([start/2, stop/1, hooks/1, instrumentation/1, supported_features/0]).
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

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_mam_muc_flushed, #{host_type => HostType},
      #{metrics => #{time_per_message => histogram, time => histogram, count => spiral}}}].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%% flush callbacks
flush(Acc, Extra = #{host_type := HostType, queue_length := MessageCount}) ->
    mongoose_instrument:span(mod_mam_muc_flushed, #{host_type => HostType},
                             fun do_flush_muc/2, [Acc, Extra],
                             fun(Time, _Result) ->
                                     #{time => Time,
                                       time_per_message => round(Time / MessageCount),
                                       count => MessageCount}
                             end).

%% mam workers callbacks
do_flush_muc(Acc, #{host_type := HostType, queue_length := MessageCount,
                    batch_size := MaxSize, batch_name := BatchName}) ->
    Rows = [mod_mam_muc_rdbms_arch:prepare_message(HostType, Params) || Params <- Acc],
    IsFullBuffer = MessageCount =:= MaxSize,
    case IsFullBuffer of
        true ->
            Result = mongoose_rdbms:execute(HostType, BatchName, lists:append(Rows)),
            process_batch_result(Result, Acc, HostType, MessageCount);
        _ ->
            Results = [mongoose_rdbms:execute(HostType, insert_mam_muc_message, Row) || Row <- Rows],
            Process = lists:zip(Results, Acc),
            process_list_results(Process, HostType)
    end,
    [mod_mam_muc_rdbms_arch:retract_message(HostType, Params) || Params <- Acc],
    mongoose_hooks:mam_muc_flush_messages(HostType, MessageCount),
    ok.

process_batch_result({updated, _Count}, _, _, _) ->
    ok;
process_batch_result({error, Reason}, Rows, HostType, MessageCount) ->
    mongoose_instrument:execute(mod_mam_muc_dropped, #{host_type => HostType}, #{count => MessageCount}),
    Keys = [ maps:with([message_id, archive_id], Row) || Row <- Rows ],
    ?LOG_ERROR(#{what => archive_muc_batch_messages_failed,
                 text => <<"A batch of muc messages failed to archive, modMucMamDropped metric updated">>,
                 keys => Keys, message_count => MessageCount, reason => Reason}),
    ok.

process_list_results(Results, HostType) ->
    lists:foreach(fun(R) -> process_single_result(R, HostType) end, Results).

process_single_result({{updated, _Count}, _}, _HostType) ->
    ok;
process_single_result({{error, Reason}, #{message_id := MsgId, archive_id := ArcId}}, HostType) ->
    mongoose_instrument:execute(mod_mam_muc_dropped, #{host_type => HostType}, #{count => 1}),
    ?LOG_ERROR(#{what => archive_muc_single_message_failed,
                 text => <<"Single muc message failed to archive, modMucMamDropped metric updated">>,
                 message_id => MsgId, archive_id => ArcId, reason => Reason}).
