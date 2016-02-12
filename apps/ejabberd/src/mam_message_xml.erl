%%% @doc Encoding and decoding messages using exml library
-module(mam_message_xml).
-export([encode/1,
         decode/1]).

-behaviour(mam_message).

encode(Packet) ->
    exml:to_binary(Packet).

decode(Bin) ->
    {ok, Packet} = exml:parse(Bin),
    Packet.
