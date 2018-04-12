-module(mod_jingle_sip_backend).

-include("mongoose.hrl").

-type call_id() :: binary().
-type incoming_request() :: binary().
-type outgoing_handle() :: binary().

-export([init/2]).
-export([set_incoming_request/5]).
-export([set_incoming_handle/2]).
-export([set_outgoing_request/4]).
-export([set_outgoing_handle/4]).
-export([set_outgoing_accepted/1]).
-export([set_incoming_accepted/1]).
-export([get_incoming_request/1]).
-export([get_outgoing_handle/1]).
-export([get_session_info/1]).
-export([remove_session/1]).

-record(jingle_sip_session, {sid,
                             dialog,
                             state,
                             direction,
                             request,
                             node,
                             meta}).

init(_Host, _Opts) ->
    mnesia:create_table(jingle_sip_session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, jingle_sip_session)}]).

set_incoming_request(CallID, ReqID, From, To, JingleEl) ->
    TFun = pa:bind(fun set_incoming_request_tr/5, CallID, ReqID, From, To, JingleEl),
    run_transaction(TFun).

set_incoming_request_tr(CallID, ReqID, _From, _To, JingleEl) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [#jingle_sip_session{request = undefined, direction = in, meta = Meta} = Session] ->
            NewMeta = Meta#{init_stanza => JingleEl},
            Session2 = Session#jingle_sip_session{request = ReqID,
                                                  node = node(),
                                                  meta = NewMeta
                                                  },
            mnesia:write(Session2);
        [_] ->
            {error, sid_already_exists};
        _ ->
            Meta = #{init_stanza => JingleEl},
            Session = #jingle_sip_session{sid = CallID,
                                          request = ReqID,
                                          dialog = undefined,
                                          state = undefined,
                                          direction = in,
                                          node = node(),
                                          meta = Meta},
            mnesia:write(Session)
    end.

set_outgoing_request(CallID, ReqID, From, To) ->
    TFun = pa:bind(fun set_outgoing_request_tr/4, CallID, ReqID, From, To),
    run_transaction(TFun).

set_outgoing_request_tr(CallID, ReqID, _From, _To) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [#jingle_sip_session{request = undefined, direction = out} = Session] ->
            Session2 = Session#jingle_sip_session{request = ReqID,
                                                  node = node()},
            mnesia:write(Session2);
        [_] ->
            {error, sid_already_exists};
        _ ->
            Session = #jingle_sip_session{sid = CallID,
                                          request = ReqID,
                                          dialog = undefined,
                                          state = undefined,
                                          direction = out,
                                          node = node(),
                                          meta = #{}},
            mnesia:write(Session)
    end.

run_transaction(TFun) ->
    case mnesia:transaction(TFun) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.


set_incoming_handle(CallID, DialogHandle) ->
    TFun = pa:bind(fun set_incoming_handle_tr/2, CallID, DialogHandle),
    run_transaction(TFun).

set_incoming_handle_tr(CallID, DialogHandle) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [#jingle_sip_session{dialog = undefined, direction = in} = Session] ->
            Session2 = Session#jingle_sip_session{dialog = DialogHandle,
                                                  node = node()},
            mnesia:write(Session2);
        [_] ->
            {error, incoming_handle_exists};
        _ ->
            {error, not_found}
    end.

set_outgoing_handle(CallID, DialogHandle, From, To) ->
    TFun = pa:bind(fun set_outgoing_handle_tr/4, CallID, DialogHandle, From, To),
    run_transaction(TFun).

set_outgoing_handle_tr(CallID, DialogHandle, _From, _To) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [#jingle_sip_session{dialog = undefined, direction = out} = Session] ->
            Session2 = Session#jingle_sip_session{dialog = DialogHandle,
                                                  node = node()},
            mnesia:write(Session2);
        [_] ->
            {error, outgoing_handle_exists};
        _ ->
            Session = #jingle_sip_session{sid = CallID,
                                          dialog = DialogHandle,
                                          node = node(),
                                          direction = out,
                                          state = ringing},
            mnesia:write(Session)
    end.

set_incoming_accepted(CallID) ->
    TFun = pa:bind(fun set_incoming_accepted_tr/1, CallID),
    run_transaction(TFun).

set_incoming_accepted_tr(CallID) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [#jingle_sip_session{direction = in, meta = Meta} = Session] ->
            MetaWithoutInitStanza = maps:without([init_stanza], Meta),
            Session2 = Session#jingle_sip_session{state = accepted,
                                                  meta = MetaWithoutInitStanza},
            mnesia:write(Session2);
        _ ->
            {error, not_found}
    end.

set_outgoing_accepted(CallID) ->
    TFun = pa:bind(fun set_outgoing_accepted_tr/1, CallID),
    run_transaction(TFun).

set_outgoing_accepted_tr(CallID) ->
    case mnesia:wread({jingle_sip_session, CallID}) of
        [#jingle_sip_session{direction = out} = Session] ->
            Session2 = Session#jingle_sip_session{state = accepted},
            mnesia:write(Session2);
        _ ->
            {error, not_found}
    end.

-spec get_incoming_request(call_id()) -> {ok, undefined | incoming_request()} |
                                        {error, not_found}.
get_incoming_request(CallID) ->
    case mnesia:dirty_read(jingle_sip_session, CallID) of
         [#jingle_sip_session{request = ReqID}] ->
            {ok, ReqID};
         _ ->
            {error, not_found}
    end.

-spec get_outgoing_handle(call_id()) -> {ok, undefined | outgoing_handle()} |
                                        {error, not_found}.
get_outgoing_handle(SID) ->
    case mnesia:dirty_read(jingle_sip_session, SID) of
         [#jingle_sip_session{dialog = Handle}] ->
            {ok, Handle};
         _ ->
            {error, not_found}
    end.

get_session_info(SID) ->
    case mnesia:dirty_read(jingle_sip_session, SID) of
         [#jingle_sip_session{sid = SID,
                              dialog = Handle,
                              request = Request,
                              state = State,
                              direction = Dir,
                              node = ONode,
                              meta = Meta}] ->
            {ok, #{sid => SID,
                   dialog => Handle,
                   request => Request,
                   state => State,
                   direction => Dir,
                   node => ONode,
                   meta => Meta}};
         _ ->
            {error, not_found}
    end.

remove_session(CallID) ->
    mnesia:dirty_delete(jingle_sip_session, CallID).
