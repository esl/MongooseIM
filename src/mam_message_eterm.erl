%%% @doc Encoding and decoding messages using internal erlang format
-module(mam_message_eterm).
-export([encode/1,
         decode/1]).

-behaviour(mam_message).

encode(Packet) ->
    term_to_binary(Packet).

decode(Bin) ->
    binary_to_term(Bin).
