-module(mod_stream_management).
-xep([{xep, 198}, {version, "1.6"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% `gen_mod' callbacks
-export([start/2,
         stop/1,
         config_spec/0,
         supported_features/0,
         process_buffer_and_ack/1]).

%% hooks handlers
-export([c2s_stream_features/3,
         remove_smid/5,
         session_cleanup/5]).

%% API for `ejabberd_c2s'
-export([make_smid/0,
         get_session_from_smid/2,
         get_buffer_max/2,
         get_ack_freq/2,
         get_resume_timeout/2,
         register_smid/2]).

%% API for inspection and tests
-export([get_sid/1,
         get_stale_h/2,
         register_stale_smid_h/3,
         remove_stale_smid_h/2]).

-ignore_xref([c2s_stream_features/3, get_sid/1, get_stale_h/2, remove_smid/5,
              register_stale_smid_h/3, remove_stale_smid_h/2, session_cleanup/5]).

-type smid() :: base64:ascii_binary().

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-record(sm_session,
        {smid :: smid(),
         sid :: ejabberd_sm:sid()
        }).

-type buffer_max() :: pos_integer() | infinity | no_buffer.
-type ack_freq() :: pos_integer() | never.
%%
%% `gen_mod' callbacks
%%

start(HostType, Opts) ->
    ?LOG_INFO(#{what => stream_management_starting}),
    ejabberd_hooks:add(hooks(HostType)),
    mnesia:create_table(sm_session, [{ram_copies, [node()]},
                                     {attributes, record_info(fields, sm_session)}]),
    mnesia:add_table_index(sm_session, sid),
    mnesia:add_table_copy(sm_session, node(), ram_copies),
    stream_management_stale_h:maybe_start(Opts),
    ok.

stop(HostType) ->
    ?LOG_INFO(#{what => stream_management_stopping}),
    ejabberd_hooks:delete(hooks(HostType)),
    ok.

hooks(HostType) ->
    [{sm_remove_connection_hook, HostType, ?MODULE, remove_smid, 50},
     {c2s_stream_features, HostType, ?MODULE, c2s_stream_features, 50},
     {session_cleanup, HostType, ?MODULE, session_cleanup, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"buffer">> => #option{type = boolean},
                  <<"buffer_max">> => #option{type = int_or_infinity,
                                              validate = positive},
                  <<"ack">> => #option{type = boolean},
                  <<"ack_freq">> => #option{type = integer,
                                            validate = positive},
                  <<"resume_timeout">> => #option{type = integer,
                                                  validate = positive},
                  <<"stale_h">> => stale_h_config_spec()
                 },
        process = fun ?MODULE:process_buffer_and_ack/1
      }.

supported_features() -> [dynamic_domains].

process_buffer_and_ack(KVs) ->
    {[Buffer, Ack], Opts} = proplists:split(KVs, [buffer, ack]),
    OptsWithBuffer = check_buffer(Buffer, Opts),
    check_ack(Ack, OptsWithBuffer).

check_buffer([{buffer, false}], Opts) ->
    lists:ukeysort(1, [{buffer_max, no_buffer}] ++ Opts);
check_buffer(_, Opts) ->
    Opts.

check_ack([{ack, false}], Opts) ->
    lists:ukeysort(1, [{ack_freq, never}] ++ Opts);
check_ack(_, Opts) ->
    Opts.

stale_h_config_spec() ->
    #section{
        items = #{<<"enabled">> => #option{type = boolean},
                  <<"repeat_after">> => #option{type = integer,
                                                validate = positive,
                                                format = {kv, stale_h_repeat_after}},
                  <<"geriatric">> => #option{type = integer,
                                             validate = positive,
                                             format = {kv, stale_h_geriatric}}
        }
    }.

%%
%% hooks handlers
%%

-spec c2s_stream_features([exml:element()], mongooseim:host_type(), jid:lserver()) ->
          [exml:element()].
c2s_stream_features(Acc, _HostType, _Lserver) ->
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
    HostType = mongoose_acc:host_type(Acc),
    do_remove_smid(Acc, HostType, SID).

-spec session_cleanup(Acc :: map(), LUser :: jid:luser(), LServer :: jid:lserver(),
                      LResource :: jid:lresource(), SID :: ejabberd_sm:sid()) -> any().
session_cleanup(Acc, _LUser, _LServer, _LResource, SID) ->
    HostType = mongoose_acc:host_type(Acc),
    do_remove_smid(Acc, HostType, SID).

-spec do_remove_smid(mongoose_acc:t(), mongooseim:host_type(), ejabberd_sm:sid()) ->
    mongoose_acc:t().
do_remove_smid(Acc, HostType, SID) ->
    H = mongoose_acc:get(stream_mgmt, h, undefined, Acc),
    MaybeSMID = case mnesia:dirty_index_read(sm_session, SID, #sm_session.sid) of
        [] -> {error, smid_not_found};
        [#sm_session{smid = SMID}] ->
            mnesia:dirty_delete(sm_session, SMID),
            case H of
                undefined -> ok;
                _ -> register_stale_smid_h(HostType, SMID, H)
            end,
            {ok, SMID}
    end,
    mongoose_acc:set(stream_mgmt, smid, MaybeSMID, Acc).

%%
%% API for `ejabberd_c2s'
%%

-spec make_smid() -> smid().
make_smid() ->
    base64:encode(crypto:strong_rand_bytes(21)).

%% Getters
-spec get_session_from_smid(mongooseim:host_type(), smid()) ->
    {sid, ejabberd_sm:sid()} | {stale_h, non_neg_integer()} | {error, smid_not_found}.
get_session_from_smid(HostType, SMID) ->
    case get_sid(SMID) of
        {sid, SID} -> {sid, SID};
        {error, smid_not_found} -> get_stale_h(HostType, SMID)
    end.

-spec get_sid(smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.
get_sid(SMID) ->
    case mnesia:dirty_read(sm_session, SMID) of
        [#sm_session{sid = SID}] -> {sid, SID};
        [] -> {error, smid_not_found}
    end.

-spec get_stale_h(mongooseim:host_type(), SMID :: smid()) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found}.
get_stale_h(HostType, SMID) ->
    MaybeModOpts = gen_mod:get_module_opt(HostType, ?MODULE, stale_h, []),
    case proplists:get_value(enabled, MaybeModOpts, false) of
        false -> {error, smid_not_found};
        true -> stream_management_stale_h:read_stale_h(SMID)
    end.

-spec get_buffer_max(mongooseim:host_type(), buffer_max()) -> buffer_max().
get_buffer_max(HostType, Default) ->
    gen_mod:get_module_opt(HostType, ?MODULE, buffer_max, Default).

-spec get_ack_freq(mongooseim:host_type(), ack_freq()) -> ack_freq().
get_ack_freq(HostType, Default) ->
    gen_mod:get_module_opt(HostType, ?MODULE, ack_freq, Default).

-spec get_resume_timeout(mongooseim:host_type(), pos_integer()) -> pos_integer().
get_resume_timeout(HostType, Default) ->
    gen_mod:get_module_opt(HostType, ?MODULE, resume_timeout, Default).

%% Setters
register_smid(SMID, SID) ->
    try
        mnesia:sync_dirty(fun mnesia:write/1,
                          [#sm_session{smid = SMID, sid = SID}]),
        ok
    catch exit:Reason ->
              {error, Reason}
    end.

register_stale_smid_h(HostType, SMID, H) ->
    MaybeModOpts = gen_mod:get_module_opt(HostType, ?MODULE, stale_h, []),
    case proplists:get_value(enabled, MaybeModOpts, false) of
        false -> ok;
        true -> stream_management_stale_h:write_stale_h(SMID, H)
    end.

remove_stale_smid_h(HostType, SMID) ->
    MaybeModOpts = gen_mod:get_module_opt(HostType, ?MODULE, stale_h, []),
    case proplists:get_value(enabled, MaybeModOpts, false) of
        false -> ok;
        true -> stream_management_stale_h:delete_stale_h(SMID)
    end.
