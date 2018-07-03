%%% @doc Encoding and decoding using user's JID for compressing
-module(mam_jid_mini).
-export([encode/2,
         decode/2]).

-behaviour(mam_jid).

encode(UserJID, JID) ->
    mod_mam_utils:jid_to_opt_binary(UserJID, JID).

decode(UserJID, BSrcJID) ->
    jid:from_binary(mod_mam_utils:expand_minified_jid(UserJID, BSrcJID)).
