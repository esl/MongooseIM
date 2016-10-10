-module(mongoose_client_api_sse).

-behaviour(lasse_handler).

-export([init/3]).
-export([handle_notify/2]).
-export([handle_info/2]).
-export([handle_error/3]).
-export([terminate/3]).

init(_InitArgs, _LastEvtId, Req) ->
    {ok, Req, #{}}.

handle_notify(_Msg, State) ->
    {nosend, State}.

handle_info(_Msg, State) ->
    {nosend, State}.

handle_error(_Msg, _Reson, State) ->
    {nosend, State}.

terminate(_Reson, _Req, State) ->
    State.
