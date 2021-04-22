-module(ejabberd_c2s_state).

-include("ejabberd_c2s.hrl").

-export([server/1, jid/1, host_type/1]).

server(#state{ server = Server }) ->
    Server.

host_type(#state{ host_type = HostType }) ->
    HostType.

jid(#state{ jid = JID }) ->
    JID.

