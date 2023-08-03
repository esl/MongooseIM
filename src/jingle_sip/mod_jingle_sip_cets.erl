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
-module(mod_jingle_sip_cets).
-behaviour(mod_jingle_sip_backend).

-include("mongoose.hrl").

-type call_id() :: binary().
-type incoming_request() :: {node(), binary()}.
-type outgoing_handle() :: binary().
-type dialog_hangle() :: term().

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

-ignore_xref([remove_session/1]).

-record(jingle_sip_session, {sid, %% CallID
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

-define(TABLE, cets_jingle_sip_session).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_Host, _Opts) ->
    cets:start(?TABLE, #{keypos => 2}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE).

-spec set_incoming_request(CallID :: call_id(), ReqID :: binary(),
                           From :: jid:jid(), To :: jid:jid(), exml:element()) ->
    ok | {error, any()}.
set_incoming_request(CallID, ReqID, From, To, JingleEl) ->
    Owner = jid:to_lus(To),
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
    write_new_session(CallID, Session).

-spec set_outgoing_request(CallID :: call_id(), ReqID :: binary(),
                           From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.
set_outgoing_request(CallID, ReqID, From, To) ->
    Owner = jid:to_lus(From),
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
    write_new_session(CallID, Session).

-spec set_incoming_handle(CallID :: call_id(), DialogHandle :: dialog_hangle()) ->
    ok | {error, any()}.
set_incoming_handle(CallID, DialogHandle) ->
    case read_session(CallID) of
        [#jingle_sip_session{dialog = undefined, direction = in} = Session] ->
            Session2 = Session#jingle_sip_session{dialog = DialogHandle,
                                                  node = node()},
            update_session(Session2);
        [_] ->
            {error, incoming_handle_exists};
        _ ->
            {error, not_found}
    end.

-spec set_outgoing_handle(CallID :: call_id(), DialogHandle :: dialog_hangle(),
                          From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.
set_outgoing_handle(CallID, DialogHandle, _From, _To) ->
    case read_session(CallID) of
        [#jingle_sip_session{dialog = undefined, direction = out} = Session] ->
            Session2 = Session#jingle_sip_session{dialog = DialogHandle,
                                                  node = node()},
            update_session(Session2);
        [_] ->
            {error, outgoing_handle_exists};
        _ ->
            Session = #jingle_sip_session{sid = CallID,
                                          dialog = DialogHandle,
                                          node = node(),
                                          direction = out,
                                          state = ringing},
            cets:insert_new(?TABLE, Session)
    end.

-spec set_incoming_accepted(CallID :: call_id()) ->
    ok | {error, any()}.
set_incoming_accepted(CallID) ->
    case read_session(CallID) of
        [#jingle_sip_session{direction = in, meta = Meta} = Session] ->
            MetaWithoutInitStanza = maps:without([init_stanza], Meta),
            Session2 = Session#jingle_sip_session{state = accepted,
                                                  meta = MetaWithoutInitStanza},
            update_session(Session2);
        _ ->
            {error, not_found}
    end.

-spec set_outgoing_accepted(CallID :: call_id()) ->
    ok | {error, any()}.
set_outgoing_accepted(CallID) ->
    case read_session(CallID) of
        [#jingle_sip_session{direction = out} = Session] ->
            Session2 = Session#jingle_sip_session{state = accepted},
            update_session(Session2);
        _ ->
            {error, not_found}
    end.

-spec get_incoming_request(call_id(), jid:jid()) -> {ok, undefined | incoming_request()} |
                                                    {error, not_found}.
get_incoming_request(CallID, User) ->
    UserUS = jid:to_lus(User),
    case read_session(CallID) of
         [#jingle_sip_session{request = ReqID, node = Node, owner = UserUS}] ->
            {ok, {Node, ReqID}};
         _ ->
            {error, not_found}
    end.

-spec get_outgoing_handle(call_id(), jid:jid()) -> {ok, undefined | outgoing_handle()} |
                                                   {error, not_found}.
get_outgoing_handle(SID, User) ->
    UserUS = jid:to_lus(User),
    case read_session(SID) of
         [#jingle_sip_session{dialog = Handle, owner = UserUS}] ->
            {ok, Handle};
         _ ->
            {error, not_found}
    end.

-spec get_session_info(binary(), jid:jid()) ->
    {ok, map()} | {error, any()}.
get_session_info(SID, User) ->
    UserUS = jid:to_lus(User),
    case read_session(SID) of
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

-spec remove_session(call_id()) -> ok.
remove_session(CallID) ->
    cets:delete(?TABLE, CallID).

-spec read_session(call_id()) -> [#jingle_sip_session{}].
read_session(CallID) ->
    ets:lookup(?TABLE, CallID).

-spec write_new_session(call_id(), #jingle_sip_session{}) ->
    ok | {error, sid_already_exists}.
write_new_session(CallID, Session) ->
    case read_session(CallID) of
        [_] ->
            {error, sid_already_exists};
        _ ->
            case cets:insert_new(?TABLE, Session) of
                true ->
                    ok;
                false ->
                    {error, sid_already_exists}
            end
    end.

-spec update_session(#jingle_sip_session{}) -> ok.
update_session(Session) ->
    cets:insert(?TABLE, Session).
