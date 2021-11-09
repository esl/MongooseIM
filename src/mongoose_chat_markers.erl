%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Erlang-Solutions
%%% @doc
%%% This module implements [XEP-0333: Chat Markers] helper functions
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_chat_markers).

-include("mongoose_ns.hrl").
-include_lib("exml/include/exml.hrl").

%% Markers
-export([has_chat_markers/1,
         list_chat_markers/1,
         chat_marker_names/0]).

-type id() :: binary().
-type chat_marker_type() :: received | displayed | acknowledged.
-type chat_marker_element() :: {chat_marker_type(), id()}.
-export_type([chat_marker_type/0]).
-export_type([chat_marker_element/0]).

%% API

-spec has_chat_markers(exml:element()) -> boolean().
has_chat_markers(Packet) ->
    [] =/= list_chat_markers(Packet).

-spec list_chat_markers(exml:element()) -> [chat_marker_element()].
list_chat_markers(#xmlel{children = Children}) ->
    lists:filtermap(fun is_chat_marker_element/1, Children).

-spec chat_marker_names() -> [binary()].
chat_marker_names() ->
    [<<"received">>, <<"displayed">>, <<"acknowledged">>].

%% Internal functions

-spec is_chat_marker_element(exml:element()) ->
    false | {true, {chat_marker_type(), Id :: binary}}.
is_chat_marker_element(#xmlel{name = <<"received">>} = El) ->
    check_chat_marker_attributes(received, El);
is_chat_marker_element(#xmlel{name = <<"displayed">>} = El) ->
    check_chat_marker_attributes(displayed, El);
is_chat_marker_element(#xmlel{name = <<"acknowledged">>} = El) ->
    check_chat_marker_attributes(acknowledged, El);
is_chat_marker_element(_) ->
    false.

-spec check_chat_marker_attributes(chat_marker_type(), exml:element()) ->
    false | {true, chat_marker_element()}.
check_chat_marker_attributes(Type, El) ->
    NS = exml_query:attr(El, <<"xmlns">>),
    Id = exml_query:attr(El, <<"id">>),
    ?NS_CHAT_MARKERS =:= NS andalso Id =/= undefined andalso {true, {Type, Id}}.
