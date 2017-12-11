%%% @doc Encoding and decoding messages using internal erlang format and zlib
-module(mam_message_compressed_eterm).
-export([encode/1,
         decode/1]).

-behaviour(mam_message).

encode(Packet) ->
    term_to_binary(Packet, [compressed]).

decode(Bin) ->
    binary_to_term(Bin).
