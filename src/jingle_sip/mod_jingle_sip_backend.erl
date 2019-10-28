%% @doc Backend module for mod_jingle_sip
%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%
%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mod_jingle_sip_backend).

-include("mongoose.hrl").

-type call_id() :: binary().
-type incoming_request() :: {node(), binary()}.
-type outgoing_handle() :: binary().

-export([init/2]).
-export([set_incoming_request/5]).
-export([set_incoming_handle/2]).
-export([set_outgoing_request/4]).
-export([set_outgoing_handle/4]).
-export([set_outgoing_accepted/1]).
-export([set_incoming_accepted/1]).
-export([get_incoming_request/2]).
-export([get_outgoing_handle/2]).
-export([get_session_info/2]).
-export([remove_session/1]).

-record(jingle_sip_session, {sid,
                             dialog,
                             state,
                             direction,
                             request,
                             node,
                             owner,
                             from,
                             to,
                             now,
                             meta}).

init(_Host, _Opts) ->
    mnesia:create_table(jingle_sip_session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, jingle_sip_session)}]).

-spec set_incoming_request(CallID :: call_id(), ReqID :: binary(),
                           From :: jid:jid(), To :: jid:jid(), exml:element()) ->
    ok | {error, any()}.
set_incoming_request(CallID, ReqID, From, To, JingleEl) ->
    TFun = pa:bind(fun set_incoming_request_tr/5, CallID, ReqID, From, To, JingleEl),
    run_transaction(TFun).

set_incoming_request_tr(CallID, ReqID, From, To, JingleEl) ->
    Owner = jid:to_lus(To),
    case mnesia:wread({jingle_sip_session, CallID}) of
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
                                          from = jid:to_lus(From),
                                          to = Owner,
                                          owner = Owner,
                                          now = os:system_time(microsecond),
                                          meta = Meta},
            mnesia:write(Session)
    end.

-spec set_outgoing_request(CallID :: call_id(), ReqID :: binary(),
                           From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.
set_outgoing_request(CallID, ReqID, From, To) ->
    TFun = pa:bind(fun set_outgoing_request_tr/4, CallID, ReqID, From, To),
    run_transaction(TFun).

set_outgoing_request_tr(CallID, ReqID, From, To) ->
    Owner = jid:to_lus(From),
    case mnesia:wread({jingle_sip_session, CallID}) of
        [_] ->
            {error, sid_already_exists};
        _ ->
            Session = #jingle_sip_session{sid = CallID,
                                          request = ReqID,
                                          dialog = undefined,
                                          state = undefined,
                                          direction = out,
                                          node = node(),
                                          from = Owner,
                                          to = jid:to_lus(To),
                                          owner = Owner,
                                          now = os:system_time(microsecond),
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

-spec get_incoming_request(call_id(), jid:jid()) -> {ok, undefined | incoming_request()} |
                                                    {error, not_found}.
get_incoming_request(CallID, User) ->
    UserUS = jid:to_lus(User),
    case mnesia:dirty_read(jingle_sip_session, CallID) of
         [#jingle_sip_session{request = ReqID, node = Node, owner = UserUS}] ->
            {ok, {Node, ReqID}};
         _ ->
            {error, not_found}
    end.

-spec get_outgoing_handle(call_id(), jid:jid()) -> {ok, undefined | outgoing_handle()} |
                                                   {error, not_found}.
get_outgoing_handle(SID, User) ->
    UserUS = jid:to_lus(User),
    case mnesia:dirty_read(jingle_sip_session, SID) of
         [#jingle_sip_session{dialog = Handle, owner = UserUS}] ->
            {ok, Handle};
         _ ->
            {error, not_found}
    end.

-spec get_session_info(binary(), jid:jid()) ->
    {ok, map()} | {error, any()}.
get_session_info(SID, User) ->
    UserUS = jid:to_lus(User),
    case mnesia:dirty_read(jingle_sip_session, SID) of
         [#jingle_sip_session{sid = SID,
                              dialog = Handle,
                              request = Request,
                              state = State,
                              direction = Dir,
                              node = ONode,
                              owner = UserUS,
                              to = To,
                              from = From,
                              meta = Meta}] ->
            {ok, #{sid => SID,
                   dialog => Handle,
                   request => Request,
                   state => State,
                   direction => Dir,
                   node => ONode,
                   from => From,
                   to => To,
                   meta => Meta}};
         _ ->
            {error, not_found}
    end.

remove_session(CallID) ->
    mnesia:dirty_delete(jingle_sip_session, CallID).
