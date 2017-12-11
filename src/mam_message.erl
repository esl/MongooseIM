%%% @doc Encoder and decoder for MAM messages
%%%
%%% Default implementations are:
%%% - mam_message_xml
%%% - mam_message_eterm
%%% - mam_message_compressed_eterm
-module(mam_message).

-callback encode(jlib:xmlel()) -> binary().
-callback decode(binary()) -> jlib:xmlel().

-export([encode/2, decode/2]).

-spec encode(module(), jlib:xmlel()) -> binary().
encode(Mod, Packet) -> Mod:encode(Packet).

-spec decode(module(), binary()) -> jlib:xmlel().
decode(Mod, Bin) -> Mod:decode(Bin).

