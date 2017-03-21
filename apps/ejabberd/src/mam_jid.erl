%%% @doc Encoder and decoder for MAM JIDs
%%%
%%% Default implementations are:
%%% - mam_jid_mini
%%% - mam_jid_rfc
-module(mam_jid).

-callback encode(ejabberd:jid(), ejabberd:jid()) -> binary().
-callback decode(ejabberd:jid(), binary()) -> ejabberd:jid().

-export([encode/3, decode/3]).

-spec encode(module(), ejabberd:jid(), ejabberd:jid()) -> binary().
encode(Module, UserJID, JID) ->
    Module:encode(UserJID, JID).

-spec decode(module(), ejabberd:jid(), binary()) -> ejabberd:jid().
decode(Module, UserJID, Bin) ->
    Module:decode(UserJID, Bin).

