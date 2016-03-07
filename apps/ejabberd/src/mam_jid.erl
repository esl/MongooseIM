%%% @doc Encoder and decoder for MAM JIDs
%%%
%%% Default implementations are:
%%% - mam_jid_mini
%%% - mam_jid_rfc
-module(mam_jid).

-callback encode(ejabberd:jid(), ejabberd:jid()) -> binary().
-callback decode(ejabberd:jid(), binary()) -> ejabberd:jid().
