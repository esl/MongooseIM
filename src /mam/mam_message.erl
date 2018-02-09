%%% @doc Encoder and decoder for MAM messages
%%%
%%% Default implementations are:
%%% - mam_message_xml
%%% - mam_message_eterm
%%% - mam_message_compressed_eterm
-module(mam_message).

-callback encode(exml:element()) -> binary().
-callback decode(binary()) -> exml:element().

-export([encode/2, decode/2]).

-spec encode(module(), exml:element()) -> binary().
encode(Mod, Packet) -> Mod:encode(Packet).

-spec decode(module(), binary()) -> exml:element().
decode(Mod, Bin) -> Mod:decode(Bin).

