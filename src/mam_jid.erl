%%% @doc Encoder and decoder for MAM JIDs
%%%
%%% Default implementations are:
%%% - mam_jid_mini
%%% - mam_jid_rfc
-module(mam_jid).

-callback encode(jlib:jid(), jlib:jid()) -> binary().
-callback decode(jlib:jid(), binary()) -> jlib:jid().

-export([encode/3, decode/3]).

-spec encode(module(), jlib:jid(), jlib:jid()) -> binary().
encode(Module, UserJID, JID) ->
    Module:encode(UserJID, JID).

-spec decode(module(), jlib:jid(), binary()) -> jlib:jid().
decode(Module, UserJID, Bin) ->
    Module:decode(UserJID, Bin).

