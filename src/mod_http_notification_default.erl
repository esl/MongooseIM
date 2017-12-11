%%%----------------------------------------------------------------------
%%% File    : mod_http_notification_default.erl
%%% Author  : Baibossynv Valery <baibossynov.valery@gmail.com>
%%% Purpose : Message passing via http
%%% Created : 23 Feb 2075 by Piotr Nosek
%%%----------------------------------------------------------------------

-module(mod_http_notification_default).
-author("baibossynov.valery@gmail.com").

-behaviour(mod_http_notification).

%% API
-export([should_make_req/3]).

%% @doc This function determines whether to send http notification or not.
%% Can be reconfigured by creating a custom module implementing should_make_req/3
%% and adding it to mod_http_notification settings as {callback_module}
%% Default behaviour is to send all chat messages with non-empty body.
should_make_req(Packet, From, To) ->
    Type = exml_query:attr(Packet, <<"type">>, <<>>),
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    should_make_req(Type, Body, From, To).

should_make_req(<<"chat">>, Body, _From, _To) when Body /= <<"">> ->
    true;
should_make_req(_, _, _, _) ->
    false.

