%%% @doc Encoding and decoding using encoding described in XMPP RFC specification
-module(mam_jid_rfc).
-export([encode/2,
         decode/2]).

-behaviour(mam_jid).

encode(_UserJID, JID) ->
    jid:to_binary(jid:to_lower(JID)).

decode(_UserJID, BSrcJID) ->
    jid:from_binary(BSrcJID).
