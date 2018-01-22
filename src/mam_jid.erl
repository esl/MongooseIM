%%% @doc Encoder and decoder for MAM JIDs
%%%
%%% Default implementations are:
%%% - mam_jid_mini
%%% - mam_jid_rfc
-module(mam_jid).

-callback encode(jid:jid(), jid:jid()) -> binary().
-callback decode(jid:jid(), binary()) -> jid:jid().

-export([encode/3, decode/3]).

-spec encode(module(), jid:jid(), jid:jid()) -> binary().
encode(Module, UserJID, JID) ->
    Module:encode(UserJID, JID).

-spec decode(module(), jid:jid(), binary()) -> jid:jid().
decode(Module, UserJID, Bin) ->
    Module:decode(UserJID, Bin).

