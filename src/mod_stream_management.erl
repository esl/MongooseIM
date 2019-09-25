-module(mod_stream_management).
-xep([{xep, 198}, {version, "1.6"}]).
-behaviour(gen_mod).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% `gen_mod' callbacks
-export([start/2,
         stop/1]).

%% Internal exports
-export([start_link/2]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

%% `ejabberd_hooks' handlers
-export([add_sm_feature/2,
         remove_smid/5,
         session_cleanup/5]).

%% `mongooseim.cfg' options (don't use outside of tests)
-export([get_buffer_max/1,
         set_buffer_max/1,
         get_ack_freq/1,
         set_ack_freq/1,
         get_resume_timeout/1,
         set_resume_timeout/1,
         get_gc_repeat_after/1,
         set_gc_repeat_after/1,
         get_gc_geriatric/1,
         set_gc_geriatric/1
        ]).

%% API for `ejabberd_c2s'
-export([
         make_smid/0,
         get_session_from_smid/1,
         get_sid/1,
         get_stale_h/1,
         register_smid/2,
         register_stale_smid_h/2,
         remove_stale_smid_h/1
        ]).

-type smid() :: base64:ascii_binary().

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state,
        {gc_repeat_after :: non_neg_integer(),
         gc_geriatric :: non_neg_integer()
        }).

-record(sm_session,
        {smid :: smid(),
         sid :: ejabberd_sm:sid()
        }).

-record(stream_mgmt_stale_h,
        {smid :: smid(),
         h :: non_neg_integer(),
         stamp :: non_neg_integer()
        }).

%%
%% `gen_mod' callbacks
%%

start(Host, Opts) ->
    ?INFO_MSG("mod_stream_management starting", []),
    ejabberd_hooks:add(c2s_stream_features, Host, ?MODULE, add_sm_feature, 50),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, remove_smid, 50),
    ejabberd_hooks:add(session_cleanup, Host, ?MODULE, session_cleanup, 50),
    mnesia:create_table(sm_session, [{ram_copies, [node()]},
                                     {attributes, record_info(fields, sm_session)}]),
    mnesia:add_table_index(sm_session, sid),
    mnesia:add_table_copy(sm_session, node(), ram_copies),
    maybe_start_stale_h(Host, Opts),
    ok.

maybe_start_stale_h(Host, Opts) ->
    StaleOpts = gen_mod:get_opt(stale_h, Opts, {false, []}),
    case StaleOpts of
        {false, []} ->
            ok;
        {true, GCOpts} ->
            mnesia:create_table(stream_mgmt_stale_h,
                                [{ram_copies, [node()]},
                                 {attributes, record_info(fields, stream_mgmt_stale_h)}]),
            mnesia:add_table_copy(stream_mgmt_stale_h, node(), ram_copies),
            start_cleaner(Host, GCOpts)
    end.

stop(Host) ->
    ?INFO_MSG("mod_stream_management stopping", []),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, remove_smid, 50),
    ejabberd_hooks:delete(c2s_stream_features, Host, ?MODULE, add_sm_feature, 50),
    ejabberd_hooks:delete(session_cleanup, Host, ?MODULE, session_cleanup, 50),
    StaleOpts = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, {false, []}),
    case StaleOpts of
        {false, []} ->
            ok;
        {true, _GCOpts} ->
            stop_cleaner(?MODULE)
    end.

%%
%% `ejabberd_hooks' handlers
%%

add_sm_feature(Acc, _Server) ->
    lists:keystore(<<"sm">>, #xmlel.name, Acc, sm()).

sm() ->
    #xmlel{name = <<"sm">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

-spec remove_smid(Acc, SID, JID, Info, Reason) -> Acc1 when
      Acc :: mongoose_acc:t(),
      SID :: ejabberd_sm:sid(),
      JID :: undefined | jid:jid(),
      Info :: undefined | [any()],
      Reason :: undefined | ejabberd_sm:close_reason(),
      Acc1 :: mongoose_acc:t().
remove_smid(Acc, SID, _JID, _Info, _Reason) ->
    do_remove_smid(Acc, SID).

-spec session_cleanup(Acc :: map(), LUser :: jid:luser(), LServer :: jid:lserver(),
                      LResource :: jid:lresource(), SID :: ejabberd_sm:sid()) -> any().
session_cleanup(Acc, _LUser, _LServer, _LResource, SID) ->
    Acc1 = do_remove_smid(Acc, SID),
    case mongoose_acc:get(stream_mgmt, smid, Acc1) of
        {error, smid_not_found} -> ok;
        {ok, SMID} -> remove_stale_smid_h(SMID)
    end,
    Acc.

-spec do_remove_smid(Acc, SID) -> Acc1 when
      Acc :: mongoose_acc:t(),
      SID :: ejabberd_sm:sid(),
      Acc1 :: mongoose_acc:t().
do_remove_smid(Acc, SID) ->
    H = mongoose_acc:get(stream_mgmt, h, undefined, Acc),
    MaybeSMID = case mnesia:dirty_index_read(sm_session, SID, #sm_session.sid) of
        [] -> {error, smid_not_found};
        [#sm_session{smid = SMID}] ->
            mnesia:dirty_delete(sm_session, SMID),
            case H of
                undefined -> ok;
                _ -> register_stale_smid_h(SMID, H)
            end,
            {ok, SMID}
    end,
    mongoose_acc:set(stream_mgmt, smid, MaybeSMID, Acc).

%%
%% `mongooseim.cfg' options (don't use outside of tests)
%%

-spec get_buffer_max(pos_integer() | infinity | no_buffer)
    -> pos_integer() | infinity | no_buffer.
get_buffer_max(Default) ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, buffer_max, Default).

%% Return true if succeeded, false otherwise.
-spec set_buffer_max(pos_integer() | infinity | no_buffer | undefined)
    -> boolean().
set_buffer_max(undefined) ->
    del_module_opt(?MYNAME, ?MODULE, buffer_max);
set_buffer_max(infinity) ->
    set_module_opt(?MYNAME, ?MODULE, buffer_max, infinity);
set_buffer_max(no_buffer) ->
    set_module_opt(?MYNAME, ?MODULE, buffer_max, no_buffer);
set_buffer_max(Seconds) when is_integer(Seconds), Seconds > 0 ->
    set_module_opt(?MYNAME, ?MODULE, buffer_max, Seconds).

-spec get_ack_freq(pos_integer() | never) -> pos_integer() | never.
get_ack_freq(Default) ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, ack_freq, Default).

%% Return true if succeeded, false otherwise.
-spec set_ack_freq(pos_integer() | never | undefined) -> boolean().
set_ack_freq(undefined) ->
    del_module_opt(?MYNAME, ?MODULE, ack_freq);
set_ack_freq(never) ->
    set_module_opt(?MYNAME, ?MODULE, ack_freq, never);
set_ack_freq(Freq) when is_integer(Freq), Freq > 0 ->
    set_module_opt(?MYNAME, ?MODULE, ack_freq, Freq).

-spec get_resume_timeout(pos_integer()) -> pos_integer().
get_resume_timeout(Default) ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, resume_timeout, Default).

-spec set_resume_timeout(pos_integer()) -> boolean().
set_resume_timeout(ResumeTimeout) ->
    set_module_opt(?MYNAME, ?MODULE, resume_timeout, ResumeTimeout).


-spec get_gc_repeat_after(pos_integer()) -> pos_integer().
get_gc_repeat_after(Default) ->
    S = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, undefined),
    case S of
        {true, GCOpts} ->
            proplists:get_value(gc_repeat_after, GCOpts, Default);
        _ -> Default
    end.

-spec set_gc_repeat_after(pos_integer()) -> boolean().
set_gc_repeat_after(ResumeTimeout) ->
    S = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, undefined),
    case S of
        {true, GCOpts} ->
            NewGCOpts = lists:keystore(gc_repeat_after, 1, GCOpts,
                                       {gc_repeat_after, ResumeTimeout}),
            NewStaleOpts = {true, NewGCOpts},
            set_module_opt(?MYNAME, ?MODULE, stale_h, NewStaleOpts);
        _ -> false
    end.

-spec get_gc_geriatric(pos_integer()) -> pos_integer().
get_gc_geriatric(Default) ->
    S = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, undefined),
    case S of
        {true, GCOpts} ->
            proplists:get_value(gc_geriatric, GCOpts, Default);
        _ -> Default
    end.

-spec set_gc_geriatric(pos_integer()) -> boolean().
set_gc_geriatric(ResumeTimeout) ->
    S = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, undefined),
    case S of
        {true, GCOpts} ->
            NewGCOpts = lists:keystore(gc_geriatric, 1, GCOpts,
                                       {gc_geriatric, ResumeTimeout}),
            NewStaleOpts = {true, NewGCOpts},
            set_module_opt(?MYNAME, ?MODULE, stale_h, NewStaleOpts);
        _ -> false
    end.


%%
%% API for `ejabberd_c2s'
%%

-spec make_smid() -> smid().
make_smid() ->
    base64:encode(crypto:strong_rand_bytes(21)).

%% Getters
-spec get_session_from_smid(SMID :: smid()) ->
    {sid, ejabberd_sm:sid()} | {stale_h, non_neg_integer()} | {error, smid_not_found}.
get_session_from_smid(SMID) ->
    case get_sid(SMID) of
        {sid, SID} -> {sid, SID};
        {error, smid_not_found} -> get_stale_h(SMID)
    end.

-spec get_sid(SMID :: smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.
get_sid(SMID) ->
    case mnesia:dirty_read(sm_session, SMID) of
        [#sm_session{sid = SID}] -> {sid, SID};
        [] -> {error, smid_not_found}
    end.

-spec get_stale_h(SMID :: smid()) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found}.
get_stale_h(SMID) ->
    case gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, {false, []}) of
        {false, []} -> {error, smid_not_found};
        {true, _} ->
            case mnesia:dirty_read(stream_mgmt_stale_h, SMID) of
                [#stream_mgmt_stale_h{h = H}] -> {stale_h, H};
                [] -> {error, smid_not_found}
            end
    end.

%% Setters
register_smid(SMID, SID) ->
    try
        mnesia:sync_dirty(fun mnesia:write/1,
                          [#sm_session{smid = SMID, sid = SID}]),
        ok
    catch exit:Reason ->
              {error, Reason}
    end.

register_stale_smid_h(SMID, H) ->
    case gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, {false, []}) of
        {false, []} -> ok;
        {true, _} ->
            try
                Stamp = now_to_seconds(erlang:timestamp()),
                mnesia:sync_dirty(fun mnesia:write/1,
                                  [#stream_mgmt_stale_h{
                                      smid = SMID, h = H, stamp = Stamp}])
            catch exit:Reason ->
                      {error, Reason}
            end
    end.

remove_stale_smid_h(SMID) ->
    case gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, {false, []}) of
        {false, []} -> ok;
        {true, _} -> mnesia:dirty_delete(stream_mgmt_stale_h, SMID)
    end.

%%
%% gen_server
start_cleaner(Host, Opts) ->
    ChildSpec = {?MODULE,
                 {?MODULE, start_link, [Host, Opts]},
                 permanent, 5000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

stop_cleaner(Proc) ->
    ejabberd_sup:stop_child(Proc).

start_link(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Opts], []).

init([_Host, GCOpts]) ->
    RepeatAfter = proplists:get_value(gc_repeat_after, GCOpts, 30000),
    GeriatricAge = proplists:get_value(gc_geriatric, GCOpts, 60000),
    State = #state{gc_repeat_after = RepeatAfter,
                   gc_geriatric = GeriatricAge},
     case RepeatAfter of
         never -> ignore;
         _ when is_integer(RepeatAfter) ->  {ok, State, RepeatAfter}
     end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{gc_repeat_after = RepeatAfter,
                            gc_geriatric = GeriatricAge} = State) ->
    TimeToDie = now_to_seconds(erlang:timestamp()) + GeriatricAge,
    ets:select_delete(
      stream_mgmt_stale_h,
      ets:fun2ms(
        fun(#stream_mgmt_stale_h{stamp=S}) when S < TimeToDie -> true end
       )
     ),
    {noreply, State, RepeatAfter};
handle_info(_Info, #state{gc_repeat_after = RepeatAfter,
                          gc_geriatric = _GeriatricAge} = State) ->
    {noreply, State, RepeatAfter}.

%%
%% Helpers
%%
-spec now_to_seconds(erlang:timestamp()) -> non_neg_integer().
now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.


%% copy-n-paste from gen_mod.erl
-record(ejabberd_module, {module_host, opts}).

set_module_opt(Host, Module, Opt, Value) ->
    mod_module_opt(Host, Module, Opt, Value, fun set_opt/3).

del_module_opt(Host, Module, Opt) ->
    mod_module_opt(Host, Module, Opt, undefined, fun del_opt/3).

-spec mod_module_opt(_Host, _Module, _Opt, _Value, _Modify) -> boolean().
mod_module_opt(Host, Module, Opt, Value, Modify) ->
    Key = {Module, Host},
    OptsList = ets:lookup(ejabberd_modules, Key),
    case OptsList of
        [] ->
            false;
        [#ejabberd_module{opts = Opts}] ->
            Updated = Modify(Opt, Opts, Value),
            ets:update_element(ejabberd_modules, Key,
                               {#ejabberd_module.opts, Updated})
    end.

set_opt(Opt, Opts, Value) ->
    lists:keystore(Opt, 1, Opts, {Opt, Value}).

del_opt(Opt, Opts, _) ->
    lists:keydelete(Opt, 1, Opts).
