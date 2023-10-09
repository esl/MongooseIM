%% Mnesia backend module for mod_jingle_sip module
-module(mod_jingle_sip_mnesia).
-behaviour(mod_jingle_sip_backend).

-include("mongoose.hrl").
-include("mod_jingle_sip_session.hrl").

-export([init/2]).
-export([read_session/1]).
-export([write_new_session/2]).
-export([update_session/2]).
-export([remove_session/1]).

-type call_id() :: mod_jingle_sip_session:call_id().
-type session() :: mod_jingle_sip_session:session().
-type update_fun() :: mod_jingle_sip_session:update_fun().

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_Host, _Opts) ->
    mongoose_mnesia:create_table(jingle_sip_session,
        [{ram_copies, [node()]},
         {attributes, record_info(fields, jingle_sip_session)}]),
    ok.

-spec read_session(call_id()) -> [session()].
read_session(CallID) ->
    mnesia:dirty_read(jingle_sip_session, CallID).

-spec write_new_session(call_id(), session()) ->
    ok | {error, conflict}.
write_new_session(CallID, Session) ->
    run_transaction(fun() -> write_new_session_tr(CallID, Session) end).

write_new_session_tr(CallID, Session) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [_] ->
            {error, conflict};
        _ ->
            mnesia:write(Session)
    end.

-spec update_session(call_id(), update_fun()) -> ok | {error, _}.
update_session(CallID, F) ->
    run_transaction(fun() -> update_session_tr(CallID, F) end).

update_session_tr(CallID, F) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [Session] ->
            case F(Session) of
                {error, _} = Err -> Err;
                Session2 -> mnesia:write(Session2)
            end;
       _ ->
            {error, not_found}
    end.

run_transaction(TFun) ->
    case mnesia:transaction(TFun) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec remove_session(call_id()) -> ok.
remove_session(CallID) ->
    mnesia:dirty_delete(jingle_sip_session, CallID).
