-module(stream_management_stale_h).
-behaviour(gen_server).

-include("mongoose.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(smgc_state,
        {gc_repeat_after :: non_neg_integer(),
         gc_geriatric :: non_neg_integer()
        }).

-record(stream_mgmt_stale_h,
        {smid :: mod_stream_management:smid(),
         h :: non_neg_integer(),
         stamp :: non_neg_integer()
        }).

-export([read_stale_h/1,
         write_stale_h/2,
         delete_stale_h/1,
         clear_table/1
        ]).

-export([maybe_start/1,
         stop/0
        ]).

%% Internal exports
-export([start_link/1]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-spec read_stale_h(SMID :: mod_stream_management:smid()) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found}.
read_stale_h(SMID) ->
    try
        case mnesia:dirty_read(stream_mgmt_stale_h, SMID) of
            [#stream_mgmt_stale_h{h = H}] -> {stale_h, H};
            [] -> {error, smid_not_found}
        end
    catch exit:_Reason ->
              {error, smid_not_found}
    end.

-spec write_stale_h(SMID :: mod_stream_management:smid(), H :: non_neg_integer()) ->
    ok | {error, any()}.
write_stale_h(SMID, H) ->
    try
        Stamp = erlang:monotonic_time(second),
        mnesia:dirty_write(#stream_mgmt_stale_h{smid = SMID, h = H, stamp = Stamp})
    catch exit:Reason ->
              {error, Reason}
    end.

-spec delete_stale_h(SMID :: mod_stream_management:smid()) ->
    ok | {error, any()}.
delete_stale_h(SMID) ->
    try
        mnesia:dirty_delete(stream_mgmt_stale_h, SMID)
    catch exit:Reason ->
              {error, Reason}
    end.


%%
%% gen_server
maybe_start(Opts) ->
    StaleOpts = gen_mod:get_opt(stale_h, Opts, [{enabled, false}]),
    case proplists:get_value(enabled, StaleOpts, false) of
        false ->
            ok;
        true ->
            ?INFO_MSG("event=stream_mgmt_stale_h_start", []),
            mnesia:create_table(stream_mgmt_stale_h,
                                [{ram_copies, [node()]},
                                 {attributes, record_info(fields, stream_mgmt_stale_h)}]),
            mnesia:add_table_copy(stream_mgmt_stale_h, node(), ram_copies),
            start_cleaner(StaleOpts)
    end.

start_cleaner(Opts) ->
    ChildSpec = {?MODULE,
                 {?MODULE, start_link, [Opts]},
                 permanent, 5000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

stop() ->
    ok.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

init([GCOpts]) ->
    RepeatAfter = proplists:get_value(stale_h_repeat_after, GCOpts, 1800),
    GeriatricAge = proplists:get_value(stale_h_geriatric, GCOpts, 3600),
    State = #smgc_state{gc_repeat_after = RepeatAfter,
                        gc_geriatric = GeriatricAge},
    {ok, State, RepeatAfter}.

handle_call(Msg, _From, State) ->
    ?WARNING_MSG("event=unexpected_handle_call message=~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("event=unexpected_handle_cast message=~p", [Msg]),
    {noreply, State}.

handle_info(timeout, #smgc_state{gc_repeat_after = RepeatAfter,
                                 gc_geriatric = GeriatricAge} = State) ->
    clear_table(GeriatricAge),
    {noreply, State, RepeatAfter};
handle_info(Info, #smgc_state{gc_repeat_after = RepeatAfter,
                              gc_geriatric = _GeriatricAge} = State) ->
    ?WARNING_MSG("event=unexpected_handle_info info=~p", [Info]),
    {noreply, State, RepeatAfter}.

clear_table(GeriatricAge) ->
    TimeToDie = erlang:monotonic_time(second) - GeriatricAge,
    MS = ets:fun2ms(fun(#stream_mgmt_stale_h{stamp=S}) when S < TimeToDie -> true end),
    ets:select_delete(stream_mgmt_stale_h, MS).
