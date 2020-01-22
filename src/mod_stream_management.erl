-module(mod_stream_management).
-xep([{xep, 198}, {version, "1.6"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% `gen_mod' callbacks
-export([start/2,
         stop/1]).

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
         get_stale_h_repeat_after/1,
         set_stale_h_repeat_after/1,
         get_stale_h_geriatric/1,
         set_stale_h_geriatric/1
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

-record(sm_session,
        {smid :: smid(),
         sid :: ejabberd_sm:sid()
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
    stream_management_stale_h:maybe_start(Opts),
    ok.

stop(Host) ->
    ?INFO_MSG("mod_stream_management stopping", []),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, remove_smid, 50),
    ejabberd_hooks:delete(c2s_stream_features, Host, ?MODULE, add_sm_feature, 50),
    ejabberd_hooks:delete(session_cleanup, Host, ?MODULE, session_cleanup, 50),
    StaleOpts = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, []),
    case proplists:get_value(enabled, StaleOpts, false) of
        false -> ok;
        true -> stream_management_stale_h:stop()
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
    do_remove_smid(Acc, SID).

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


-spec get_stale_h_opt(Opt :: atom(), Def :: pos_integer()) -> pos_integer().
get_stale_h_opt(Option, Default) ->
    MaybeModOpts = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, []),
    proplists:get_value(Option, MaybeModOpts, Default).

-spec get_stale_h_repeat_after(pos_integer()) -> pos_integer().
get_stale_h_repeat_after(Default) ->
    get_stale_h_opt(stale_h_repeat_after, Default).

-spec get_stale_h_geriatric(pos_integer()) -> pos_integer().
get_stale_h_geriatric(Default) ->
    get_stale_h_opt(stale_h_geriatric, Default).

-spec set_stale_h_opt(Option :: atom(), Value :: pos_integer()) -> boolean().
set_stale_h_opt(Option, Value) ->
    MaybeModOpts = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, []),
    case MaybeModOpts of
        [] -> false;
        GCOpts ->
            NewGCOpts = lists:keystore(Option, 1, GCOpts, {Option, Value}),
            set_module_opt(?MYNAME, ?MODULE, stale_h, NewGCOpts)
    end.

-spec set_stale_h_repeat_after(pos_integer()) -> boolean().
set_stale_h_repeat_after(ResumeTimeout) ->
    set_stale_h_opt(stale_h_repeat_after, ResumeTimeout).

-spec set_stale_h_geriatric(pos_integer()) -> boolean().
set_stale_h_geriatric(GeriatricAge) ->
    set_stale_h_opt(stale_h_geriatric, GeriatricAge).

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
    MaybeModOpts = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, []),
    case proplists:get_value(enabled, MaybeModOpts, false) of
        false -> {error, smid_not_found};
        true -> stream_management_stale_h:read_stale_h(SMID)
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
    MaybeModOpts = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, []),
    case proplists:get_value(enabled, MaybeModOpts, false) of
        false -> ok;
        true -> stream_management_stale_h:write_stale_h(SMID, H)
    end.

remove_stale_smid_h(SMID) ->
    MaybeModOpts = gen_mod:get_module_opt(?MYNAME, ?MODULE, stale_h, []),
    case proplists:get_value(enabled, MaybeModOpts, false) of
        false -> ok;
        true -> stream_management_stale_h:delete_stale_h(SMID)
    end.

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
