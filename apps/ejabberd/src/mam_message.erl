%%% @doc Encoder and decoder for MAM messages
%%%
%%% Default implementations are:
%%% - mam_message_xml
%%% - mam_message_eterm
%%% - mam_message_compressed_eterm
-module(mam_message).

-callback encode(jlib:xmlel()) -> binary().
-callback decode(binary()) -> jlib:xmlel().
