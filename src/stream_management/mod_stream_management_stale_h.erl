-module(mod_stream_management_stale_h).
-behaviour(gen_server).

-include("mongoose.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state,
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
         delete_stale_h/1
        ]).

-export([maybe_start/2,
         stop/0
        ]).

%% Internal exports
-export([start_link/2]).
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
        Stamp = timestamp_to_seconds(erlang:timestamp()),
        mnesia:dirty_write(
          #stream_mgmt_stale_h{
             smid = SMID, h = H, stamp = Stamp})
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
maybe_start(Host, Opts) ->
    StaleOpts = gen_mod:get_opt(stale_h, Opts, {false, []}),
    case StaleOpts of
        {false, []} ->
            ok;
        {true, GCOpts} ->
            ?INFO_MSG("stream_mgmt_stale_h starting", []),
            mnesia:create_table(stream_mgmt_stale_h,
                                [{ram_copies, [node()]},
                                 {attributes, record_info(fields, stream_mgmt_stale_h)}]),
            mnesia:add_table_copy(stream_mgmt_stale_h, node(), ram_copies),
            start_cleaner(Host, GCOpts)
    end.

start_cleaner(Host, Opts) ->
    ChildSpec = {?MODULE,
                 {?MODULE, start_link, [Host, Opts]},
                 permanent, 5000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

stop() ->
    ejabberd_sup:stop_child(?MODULE).


start_link(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Opts], []).

init([_Host, GCOpts]) ->
    RepeatAfter = proplists:get_value(gc_repeat_after, GCOpts, 1800),
    GeriatricAge = proplists:get_value(gc_geriatric, GCOpts, 3600),
    State = #state{gc_repeat_after = RepeatAfter,
                   gc_geriatric = GeriatricAge},
    {ok, State, RepeatAfter}.

handle_call(Msg, _From, State) ->
    ?WARNING_MSG("unexpected message ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected message ~p", [Msg]),
    {noreply, State}.

handle_info(timeout, #state{gc_repeat_after = RepeatAfter,
                            gc_geriatric = GeriatricAge} = State) ->
    TimeToDie = timestamp_to_seconds(erlang:timestamp()) + GeriatricAge,
    ets:select_delete(
      stream_mgmt_stale_h,
      ets:fun2ms(
        fun(#stream_mgmt_stale_h{stamp=S}) when S < TimeToDie -> true end
       )
     ),
    {noreply, State, RepeatAfter};
handle_info(Info, #state{gc_repeat_after = RepeatAfter,
                          gc_geriatric = _GeriatricAge} = State) ->
    ?WARNING_MSG("unexpected message ~p", [Info]),
    {noreply, State, RepeatAfter}.

%%
%% Helpers
%%
-spec timestamp_to_seconds(erlang:timestamp()) -> non_neg_integer().
timestamp_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.


