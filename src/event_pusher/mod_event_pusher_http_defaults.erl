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
-export([should_make_req/6, prepare_body/7, prepare_headers/7]).

%% @doc This function determines whether to send http notification or not.
%% Can be reconfigured by creating a custom module implementing should_make_req/3
%% and adding it to mod_event_pusher_http settings as {callback_module}
%% Default behaviour is to send all chat messages with non-empty body.
should_make_req(_Acc, out, _Packet, _From, _To, _Opts) ->
    false;
should_make_req(Acc, in, Packet, From, To, _Opts) ->
    Type = exml_query:attr(Packet, <<"type">>, <<>>),
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    should_make_req_type(Acc, Type, Body, From, To).

should_make_req_type(_Acc, <<"chat">>, Body, _From, _To) when Body /= <<"">> ->
    true;
should_make_req_type(_Acc, _, _, _, _) ->
    false.

prepare_body(_Acc, _Dir, Host, Message, Sender, Receiver, _Opts) ->
    cow_qs:qs([{<<"author">>, Sender},
        {<<"server">>, Host}, {<<"receiver">>, Receiver}, {<<"message">>, Message}]).

prepare_headers(_Acc, _Dir, _Host, _Sender, _Receiver, _Message, _Opts) ->
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}].
