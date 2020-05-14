-module(ejabberd_c2s_state).

-include("ejabberd_c2s.hrl").

-export([server/1, jid/1]).

server(#state{ server = Server }) ->
    Server.

jid(#state{ jid = JID }) ->
    JID.

