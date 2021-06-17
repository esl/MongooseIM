-module(ejabberd_c2s_state).

-include("ejabberd_c2s.hrl").

-export([server/1, jid/1, host_type/1]).

-spec server(state()) -> jid:lserver().
server(#state{ server = Server }) ->
    Server.

-spec host_type(state()) -> mongooseim:host_type().
host_type(#state{ host_type = HostType }) ->
    HostType.

-spec jid(state()) -> jid:jid().
jid(#state{ jid = JID }) ->
    JID.
