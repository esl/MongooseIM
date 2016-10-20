-module(mongoose_client_api_sse).

-behaviour(lasse_handler).

-export([init/3]).
-export([handle_notify/2]).
-export([handle_info/2]).
-export([handle_error/3]).
-export([terminate/3]).

init(_InitArgs, _LastEvtId, Req) ->
    {Authorization, Req2, State} = mongoose_client_api:is_authorized(Req, #{}),
    maybe_init(Authorization, Req2, State).

maybe_init(true, Req, State) ->
    {ok, Req, State};
maybe_init({false, Value}, Req, State) ->
    Headers = [{<<"www-authenticate">>, Value}],
    {shutdown, 401, Headers, <<>>, Req, State}.

handle_notify(_Msg, State) ->
    {nosend, State}.

handle_info(_Msg, State) ->
    {nosend, State}.

handle_error(_Msg, _Reson, State) ->
    {nosend, State}.

terminate(_Reson, _Req, State) ->
    State.
