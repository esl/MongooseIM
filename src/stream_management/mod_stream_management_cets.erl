-module(mod_stream_management_cets).
-behaviour(mod_stream_management_backend).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/2,
         stop/1,
         register_smid/3,
         unregister_smid/2,
         get_sid/2]).

-export([read_stale_h/2,
         write_stale_h/3,
         delete_stale_h/2]).

-export([clear_table/2]).

-ignore_xref([start_link/1]).

-define(TABLE, cets_stream_management_session).
-define(TABLE_H, cets_stream_management_stale_h).

init(HostType, #{stale_h := StaleOpts}) ->
    cets:start(?TABLE, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE),
    maybe_init_stale_h(HostType, StaleOpts),
    ok.

stop(HostType) ->
    stop_cleaner(HostType).

maybe_init_stale_h(HostType, StaleOpts = #{enabled := true}) ->
    cets:start(?TABLE_H, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE_H),
    start_cleaner(HostType, StaleOpts);
maybe_init_stale_h(_, _) -> ok.

-spec register_smid(HostType, SMID, SID) -> ok when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    SID :: ejabberd_sm:sid().
register_smid(_HostType, SMID, SID) ->
    cets:insert_many(?TABLE, [{{sid, SID}, SMID}, {{smid, SMID}, SID}]).

-spec unregister_smid(mongooseim:host_type(), ejabberd_sm:sid()) ->
    {ok, SMID :: mod_stream_management:smid()} | {error, smid_not_found}.
unregister_smid(_HostType, SID) ->
    case ets:lookup(?TABLE, {sid, SID}) of
        [] ->
            {error, smid_not_found};
        [{_, SMID}] ->
            cets:delete_many(?TABLE, [{sid, SID}, {smid, SMID}]),
            {ok, SMID}
    end.

-spec get_sid(mongooseim:host_type(), mod_stream_management:smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.
get_sid(_HostType, SMID) ->
    case ets:lookup(?TABLE, {smid, SMID}) of
        [] ->
            {error, smid_not_found};
        [{_, SID}] ->
            {sid, SID}
    end.

%% stale_h functions

-spec read_stale_h(HostType, SMID) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid().
read_stale_h(_HostType, SMID) ->
    case ets:lookup(?TABLE_H, SMID) of
        [] ->
            {error, smid_not_found};
        [{_, H, _}] ->
            {stale_h, H}
    end.

-spec write_stale_h(HostType, SMID, H) -> ok when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    H :: non_neg_integer().
write_stale_h(_HostType, SMID, H) ->
    Stamp = erlang:monotonic_time(second),
    cets:insert(?TABLE_H, {SMID, H, Stamp}).

-spec delete_stale_h(HostType, SMID) -> ok when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid().
delete_stale_h(_HostType, SMID) ->
    cets:delete(?TABLE_H, SMID).

%% stale_h cleaning logic

start_cleaner(HostType, #{repeat_after := Interval, geriatric := TTL}) ->
    %% TODO cleaner should be a service
    WOpts = #{host_type => HostType, action => fun ?MODULE:clear_table/2,
              opts => TTL, interval => timer:seconds(Interval)},
    mongoose_collector:start_common(?MODULE, HostType, WOpts).

stop_cleaner(HostType) ->
    mongoose_collector:stop_common(?MODULE, HostType).

clear_table(_HostType, GeriatricAge) ->
    TimeToDie = erlang:monotonic_time(second) - GeriatricAge,
    MS = ets:fun2ms(fun({_, _, S}) when S < TimeToDie -> true end),
    ets:select_delete(?TABLE_H, MS).
