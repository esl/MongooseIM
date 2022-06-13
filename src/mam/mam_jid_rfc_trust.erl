%%% @doc Encoding and decoding using encoding described in XMPP RFC specification
-module(mam_jid_rfc_trust).
-behaviour(mam_jid).

-export([encode/2, decode/2]).

encode(_UserJID, JID) ->
    jid:to_binary(JID).

decode(_UserJID, BSrcJID) ->
    jid:from_binary_noprep(BSrcJID).
