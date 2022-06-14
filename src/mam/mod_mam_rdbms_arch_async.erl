-module(mod_mam_rdbms_arch_async).

-behaviour(mongoose_batch_worker).

-include("mongoose_logger.hrl").

-define(PER_MESSAGE_FLUSH_TIME, [mod_mam_rdbms_async_pool_writer, per_message_flush_time]).
-define(FLUSH_TIME, [mod_mam_rdbms_async_pool_writer, flush_time]).

-behaviour(gen_mod).
-export([start/2, stop/1, supported_features/0]).
-export([archive_pm_message/3, mam_archive_sync/2, flush/2]).
-ignore_xref([archive_pm_message/3, mam_archive_sync/2]).

-export([make_pool_opts/2, prepare_insert_queries/2]).

-spec archive_pm_message(_Result, mongooseim:host_type(), mod_mam_pm:archive_message_params()) -> ok.
archive_pm_message(_Result, HostType, Params = #{archive_id := ArcID}) ->
    mongoose_async_pools:put_task(HostType, pm_mam, ArcID, Params).

-spec mam_archive_sync(term(), mongooseim:host_type()) -> term().
mam_archive_sync(Result, HostType) ->
    mongoose_async_pools:sync(HostType, pm_mam),
    Result.

%%% gen_mod callbacks
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    {PoolOpts, Extra} = make_pool_opts(pm, Opts),
    prepare_insert_queries(pm, Extra),
    mongoose_metrics:ensure_metric(HostType, ?PER_MESSAGE_FLUSH_TIME, histogram),
    mongoose_metrics:ensure_metric(HostType, ?FLUSH_TIME, histogram),
    ejabberd_hooks:add(mam_archive_sync, HostType, ?MODULE, mam_archive_sync, 50),
    ejabberd_hooks:add(mam_archive_message, HostType, ?MODULE, archive_pm_message, 50),
    mongoose_async_pools:start_pool(HostType, pm_mam, PoolOpts).

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    ejabberd_hooks:delete(mam_archive_message, HostType, ?MODULE, archive_pm_message, 50),
    ejabberd_hooks:delete(mam_archive_sync, HostType, ?MODULE, mam_archive_sync, 50),
    mongoose_async_pools:stop_pool(HostType, pm_mam).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%% internal callbacks
-spec make_pool_opts(mod_mam_meta:mam_type(), gen_mod:module_opts()) ->
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
    InsertResult =
        case MessageCount of
            MaxSize ->
                mongoose_rdbms:execute(HostType, BatchName, lists:append(Rows));
            OtherSize ->
                Results = [mongoose_rdbms:execute(HostType, insert_mam_message, Row) || Row <- Rows],
                case lists:keyfind(error, 1, Results) of
                    false -> {updated, OtherSize};
                    Error -> Error
                end
        end,
    case InsertResult of
        {updated, _Count} -> ok;
        {error, Reason} ->
            mongoose_metrics:update(HostType, modMamDropped, MessageCount),
            ?LOG_ERROR(#{what => archive_message_failed,
                         text => <<"archive_message query failed">>,
                         message_count => MessageCount, reason => Reason}),
            ok
    end,
    [mod_mam_rdbms_arch:retract_message(HostType, Params) || Params <- Acc],
    mongoose_hooks:mam_flush_messages(HostType, MessageCount),
    ok.
