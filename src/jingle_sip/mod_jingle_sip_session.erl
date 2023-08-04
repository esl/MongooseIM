%% @doc Handles operations with #jingle_sip_session{} record
-module(mod_jingle_sip_session).

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

-include("mongoose.hrl").
-include("mod_jingle_sip_session.hrl").

-type call_id() :: binary().
-type incoming_request() :: {node(), binary()}.
-type outgoing_handle() :: binary().
-type dialog_handle() :: nksip:handle().
-type request_id() :: binary().
-type session() :: #jingle_sip_session{
        sid :: call_id(),
        dialog :: dialog_handle() | undefined,
        state :: accepted | ringing | undefined,
        direction :: in | out,
        request :: request_id() | undefined,
        node :: node(),
        owner :: jid:simple_bare_jid(),
        from :: jid:simple_bare_jid(),
        to :: jid:simple_bare_jid(),
        now :: integer(),
        meta :: #{init_stanza => exml:element()}
    }.
-type update_fun() :: fun((session()) -> session() | {error, term()}).
-export_type([call_id/0, session/0, update_fun/0]).

-spec make_simple_bare_jid(jid:jid()) -> jid:simple_bare_jid().
make_simple_bare_jid(Jid) ->
    {_, _} = jid:to_lus(Jid).

-spec set_incoming_request(CallID :: call_id(), ReqID :: request_id(),
                           From :: jid:jid(), To :: jid:jid(), exml:element()) ->
    ok | {error, any()}.
set_incoming_request(CallID, ReqID, From, To, JingleEl) ->
    Owner = make_simple_bare_jid(To),
    Meta = #{init_stanza => JingleEl},
    Session = #jingle_sip_session{sid = CallID,
                                  request = ReqID,
                                  dialog = undefined,
                                  state = undefined,
                                  direction = in,
                                  node = node(),
                                  from = make_simple_bare_jid(From),
                                  to = Owner,
                                  owner = Owner,
                                  now = os:system_time(microsecond),
                                  meta = Meta},
    mod_jingle_sip_backend:write_new_session(CallID, Session).

-spec set_outgoing_request(CallID :: call_id(), ReqID :: request_id(),
                           From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.
set_outgoing_request(CallID, ReqID, From, To) ->
    Owner = make_simple_bare_jid(From),
    Session = #jingle_sip_session{sid = CallID,
                                  request = ReqID,
                                  dialog = undefined,
                                  state = undefined,
                                  direction = out,
                                  node = node(),
                                  from = Owner,
                                  to = make_simple_bare_jid(To),
                                  owner = Owner,
                                  now = os:system_time(microsecond),
                                  meta = #{}},
    mod_jingle_sip_backend:write_new_session(CallID, Session).

-spec set_incoming_handle(CallID :: call_id(), DialogHandle :: dialog_handle()) ->
    ok | {error, any()}.
set_incoming_handle(CallID, DialogHandle) ->
    F = fun(Session) -> do_set_incoming_handle(DialogHandle, Session) end,
    mod_jingle_sip_backend:update_session(CallID, F).

do_set_incoming_handle(DialogHandle, Session = #jingle_sip_session{dialog = undefined, direction = in}) ->
    Session#jingle_sip_session{dialog = DialogHandle,
                                 node = node()};
do_set_incoming_handle(_, _) ->
    {error, incoming_handle_exists}.

-spec set_outgoing_handle(CallID :: call_id(), DialogHandle :: dialog_handle(),
                          From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.
set_outgoing_handle(CallID, DialogHandle, From, To) ->
    F = fun(Session) -> do_set_outgoing_handle(DialogHandle, Session) end,
    case mod_jingle_sip_backend:update_session(CallID, F) of
        {error, not_found} ->
            Owner = make_simple_bare_jid(From),
            Session = #jingle_sip_session{sid = CallID,
                                          dialog = DialogHandle,
                                          node = node(),
                                          direction = out,
                                          state = ringing,
                                          from = Owner,
                                          to = make_simple_bare_jid(To),
                                          owner = Owner,
                                          now = os:system_time(microsecond),
                                          meta = #{}},
            mod_jingle_sip_backend:write_new_session(CallID, Session);
        Res ->
             Res
    end.

do_set_outgoing_handle(DialogHandle, Session = #jingle_sip_session{dialog = undefined, direction = out}) ->
    Session#jingle_sip_session{dialog = DialogHandle,
                                 node = node()};
do_set_outgoing_handle(_, _) ->
    {error, outgoing_handle_exists}.

-spec set_incoming_accepted(CallID :: call_id()) ->
    ok | {error, any()}.
set_incoming_accepted(CallID) ->
    mod_jingle_sip_backend:update_session(CallID, fun do_set_incoming_accepted/1).

do_set_incoming_accepted(Session = #jingle_sip_session{direction = in, meta = Meta}) ->
    MetaWithoutInitStanza = maps:without([init_stanza], Meta),
    Session#jingle_sip_session{state = accepted,
                                meta = MetaWithoutInitStanza};
do_set_incoming_accepted(_) ->
    {error, not_found}.

-spec set_outgoing_accepted(CallID :: call_id()) ->
    ok | {error, any()}.
set_outgoing_accepted(CallID) ->
    mod_jingle_sip_backend:update_session(CallID, fun do_set_outgoing_accepted/1).

do_set_outgoing_accepted(Session = #jingle_sip_session{direction = out}) ->
    Session#jingle_sip_session{state = accepted};
do_set_outgoing_accepted(_) ->
    {error, not_found}.

-spec get_incoming_request(call_id(), jid:jid()) -> {ok, undefined | incoming_request()} |
                                                    {error, not_found}.
get_incoming_request(CallID, User) ->
    UserUS = make_simple_bare_jid(User),
    case mod_jingle_sip_backend:read_session(CallID) of
         [#jingle_sip_session{request = ReqID, node = Node, owner = UserUS}] ->
            {ok, {Node, ReqID}};
         _ ->
            {error, not_found}
    end.

-spec get_outgoing_handle(call_id(), jid:jid()) -> {ok, undefined | outgoing_handle()} |
                                                   {error, not_found}.
get_outgoing_handle(SID, User) ->
    UserUS = make_simple_bare_jid(User),
    case mod_jingle_sip_backend:read_session(SID) of
         [#jingle_sip_session{dialog = Handle, owner = UserUS}] ->
            {ok, Handle};
         _ ->
            {error, not_found}
    end.

-spec get_session_info(binary(), jid:jid()) ->
    {ok, map()} | {error, any()}.
get_session_info(SID, User) ->
    UserUS = make_simple_bare_jid(User),
    case mod_jingle_sip_backend:read_session(SID) of
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
    mod_jingle_sip_backend:remove_session(CallID).
