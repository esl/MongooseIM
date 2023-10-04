-module(mod_stream_management_mnesia).
-behaviour(mod_stream_management_backend).

-include("mongoose.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/2,
         stop/1,
         register_smid/3,
         unregister_smid/2,
         get_sid/2]).

-export([read_stale_h/2,
         write_stale_h/3,
         delete_stale_h/2]).

%% Internal exports
-export([clear_table/2]).

-record(stream_mgmt_stale_h,
        {smid :: mod_stream_management:smid(),
         h :: non_neg_integer(),
         stamp :: non_neg_integer() }).

-record(sm_session,
        {smid :: mod_stream_management:smid(),
         sid :: ejabberd_sm:sid() }).

init(HostType, #{stale_h := StaleOpts}) ->
    mongoose_mnesia:create_table(sm_session,
        [{ram_copies, [node()]},
         {attributes, record_info(fields, sm_session)}]),
    mnesia:add_table_index(sm_session, sid),
    maybe_init_stale_h(HostType, StaleOpts),
    ok.

stop(HostType) ->
    stop_cleaner(HostType).

maybe_init_stale_h(HostType, StaleOpts = #{enabled := true}) ->
    ?LOG_INFO(#{what => stream_mgmt_stale_h_start}),
    mongoose_mnesia:create_table(stream_mgmt_stale_h,
        [{ram_copies, [node()]},
         {attributes, record_info(fields, stream_mgmt_stale_h)}]),
    start_cleaner(HostType, StaleOpts);
maybe_init_stale_h(_, _) -> ok.

-spec register_smid(HostType, SMID, SID) ->
    ok | {error, term()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    SID :: ejabberd_sm:sid().
register_smid(_HostType, SMID, SID) ->
    try
        mnesia:sync_dirty(fun mnesia:write/1,
                          [#sm_session{smid = SMID, sid = SID}]),
        ok
    catch exit:Reason ->
              {error, Reason}
    end.

-spec unregister_smid(mongooseim:host_type(), ejabberd_sm:sid()) ->
    {ok, SMID :: mod_stream_management:smid()} | {error, smid_not_found}.
unregister_smid(_HostType, SID) ->
    case mnesia:dirty_index_read(sm_session, SID, #sm_session.sid) of
        [] ->
            {error, smid_not_found};
        [#sm_session{smid = SMID}] ->
            mnesia:dirty_delete(sm_session, SMID),
            {ok, SMID}
    end.

-spec get_sid(mongooseim:host_type(), mod_stream_management:smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.
get_sid(_HostType, SMID) ->
    case mnesia:dirty_read(sm_session, SMID) of
        [#sm_session{sid = SID}] -> {sid, SID};
        [] -> {error, smid_not_found}
    end.

%% stale_h functions

-spec read_stale_h(HostType, SMID) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid().
read_stale_h(_HostType, SMID) ->
    try
        case mnesia:dirty_read(stream_mgmt_stale_h, SMID) of
            [#stream_mgmt_stale_h{h = H}] -> {stale_h, H};
            [] -> {error, smid_not_found}
        end
    catch exit:_Reason ->
              {error, smid_not_found}
    end.

-spec write_stale_h(HostType, SMID, H) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    H :: non_neg_integer().
write_stale_h(_HostType, SMID, H) ->
    try
        Stamp = erlang:monotonic_time(second),
        mnesia:dirty_write(#stream_mgmt_stale_h{smid = SMID, h = H, stamp = Stamp})
    catch exit:Reason ->
              {error, Reason}
    end.

-spec delete_stale_h(HostType, SMID) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid().
delete_stale_h(_HostType, SMID) ->
    try
        mnesia:dirty_delete(stream_mgmt_stale_h, SMID)
    catch exit:Reason ->
              {error, Reason}
    end.

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
    MS = ets:fun2ms(fun(#stream_mgmt_stale_h{stamp = S}) when S < TimeToDie -> true end),
    ets:select_delete(stream_mgmt_stale_h, MS).
