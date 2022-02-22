-module(mod_stream_management_cets).
-behaviour(mod_stream_management_backend).
-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/2,
         register_smid/3,
         unregister_smid/2,
         get_sid/2]).

-export([read_stale_h/2,
         write_stale_h/3,
         delete_stale_h/2]).

%% Internal exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-ignore_xref([start_link/1]).

-record(smgc_state,
        {gc_repeat_after :: non_neg_integer(),
         gc_geriatric :: non_neg_integer() }).

-define(TABLE, cets_strm_man).
-define(TABLE_H, cets_strm_man_h).

init(_HostType, Opts = #{stale_h := StaleOpts}) ->
    cets:start(?TABLE, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE),
    maybe_init_stale_h(StaleOpts),
    ok.

maybe_init_stale_h(StaleOpts = #{enabled := true}) ->
    cets:start(?TABLE_H, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE_H),
    start_cleaner(StaleOpts);
maybe_init_stale_h(_) -> ok.

-spec register_smid(HostType, SMID, SID) ->
    ok | {error, term()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    SID :: ejabberd_sm:sid().
register_smid(_HostType, SMID, SID) ->
    cets:insert(?TABLE, [{{sid, SID}, SMID}, {{smid, SMID}, SID}]).

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

-spec write_stale_h(HostType, SMID, H) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    H :: non_neg_integer().
write_stale_h(_HostType, SMID, H) ->
    Stamp = erlang:monotonic_time(second),
    cets:insert(?TABLE_H, {SMID, H, Stamp}).

-spec delete_stale_h(HostType, SMID) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid().
delete_stale_h(_HostType, SMID) ->
    cets:delete(?TABLE_H, SMID).

%% stale_h cleaning logic

start_cleaner(Opts) ->
    MFA = {?MODULE, start_link, [Opts]},
    ChildSpec = {stream_management_stale_h, MFA, permanent, 5000, worker, [?MODULE]},
    %% TODO cleaner should be a service
    ejabberd_sup:start_child(ChildSpec).

start_link(Opts) ->
    gen_server:start_link({local, stream_management_stale_h}, ?MODULE, Opts, []).

init(#{repeat_after := RepeatAfter, geriatric := GeriatricAge}) ->
    State = #smgc_state{gc_repeat_after = RepeatAfter,
                        gc_geriatric = GeriatricAge},
    schedule_check(State),
    {ok, State}.

handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(check, #smgc_state{gc_geriatric = GeriatricAge} = State) ->
    clear_table(GeriatricAge),
    schedule_check(State),
    {noreply, State};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

schedule_check(#smgc_state{gc_repeat_after = RepeatAfter}) ->
    erlang:send_after(RepeatAfter * 1000, self(), check).

clear_table(GeriatricAge) ->
    TimeToDie = erlang:monotonic_time(second) - GeriatricAge,
    MS = ets:fun2ms(fun({_, _, S}) when S < TimeToDie -> true end),
    ets:select_delete(?TABLE_H, MS).
