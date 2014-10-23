-module(revproxy_handler).

-export([init/3,
         handle/2,
         terminate/3]).

-record(state, {}).

init(_Type, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {Host, Req2} = cowboy_req:host(Req),
    {PathInfo, Req3} = cowboy_req:path_info(Req2),
    {Method, Req4} = cowboy_req:method(Req3),
    {Headers, Req5} = cowboy_req:headers(Req4),
    ContentType = [{<<"content-type">>, <<"text/plain">>}],
    {Body, Req7} = case cowboy_req:has_body(Req5) of
        false ->
            {<<>>, Req5};
        true ->
            {ok, Body1, Req6} = cowboy_req:body(Req5),
            {Body1, Req6}
    end,
    Response = io_lib:format("~p~n~p~n~p~n~p~n~p",
                             [Host, Method, PathInfo, Body, Headers]),
    {ok, Req8} = cowboy_req:reply(200, ContentType, Response, Req7),
    {ok, Req8, State}.

terminate(_Reason, _Req, _State) ->
    ok.
