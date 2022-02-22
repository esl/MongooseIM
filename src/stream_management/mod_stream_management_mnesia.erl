-module(mod_stream_management_mnesia).
-behaviour(mod_stream_management_backend).

-export([init/2,
         register_smid/3,
         unregister_smid/2,
         get_sid/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(sm_session,
        {smid :: mod_stream_management:smid(),
         sid :: ejabberd_sm:sid()
        }).

init(_HostType, _Opts) ->
    mnesia:create_table(sm_session, [{ram_copies, [node()]},
                                     {attributes, record_info(fields, sm_session)}]),
    mnesia:add_table_index(sm_session, sid),
    mnesia:add_table_copy(sm_session, node(), ram_copies),
    ok.

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
