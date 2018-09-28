%%%----------------------------------------------------------------------
%%% File    : mod_event_pusher_http_defaults.erl
%%% Author  : Baibossynv Valery <baibossynov.valery@gmail.com>
%%% Purpose : Message passing via http
%%% Created : 23 Feb 2075 by Piotr Nosek
%%%----------------------------------------------------------------------

-module(mod_event_pusher_http_defaults).
-author("baibossynov.valery@gmail.com").

-behaviour(mod_event_pusher_http).

%% API
-export([should_make_req/4, prepare_body/5, prepare_headers/5]).

%% @doc This function determines whether to send http notification or not.
%% Can be reconfigured by creating a custom module implementing should_make_req/3
%% and adding it to mod_event_pusher_http settings as {callback_module}
%% Default behaviour is to send all chat messages with non-empty body.
should_make_req(Acc, Packet, From, To) ->
    Type = exml_query:attr(Packet, <<"type">>, <<>>),
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    should_make_req(Acc, Type, Body, From, To).

should_make_req(_Acc, <<"chat">>, Body, _From, _To) when Body /= <<"">> ->
    true;
should_make_req(_Acc, _, _, _, _) ->
    false.

prepare_body(_Acc, Host, Message, Sender, Receiver) ->
    cow_qs:qs([{<<"author">>, Sender},
        {<<"server">>, Host}, {<<"receiver">>, Receiver}, {<<"message">>, Message}]).

prepare_headers(_Acc, _Host, _Sender, _Receiver, _Message) ->
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}].
