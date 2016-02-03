%%% @doc Encoder and decoder for MAM JIDs
%%%
%%% Default implementations are:
%%% - mam_jid_mini
%%% - mam_jid_rfc
-module(mam_jid).

-callback encode(jlib:xmlel()) -> binary().
-callback decode(binary()) -> jlib:xmlel().
