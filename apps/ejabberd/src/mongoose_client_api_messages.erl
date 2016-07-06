-module(mongoose_client_api_messages).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).

-export([to_json/2]).
-export([send_message/2]).

-include("ejabberd.hrl").

%% yes, there is no other option, this API has to run over encrypted connection
init({ssl, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, send_message}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

to_json(Req, User) ->
    {<<"{messages}">>, Req, User}.

send_message(Req, User) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    #{<<"to">> := To, <<"body">> := MsgBody} = jiffy:decode(Body, [return_maps]),

    ?WARNING_MSG("User ~p will send message ~p to ~p", [User, MsgBody, To]),
    Req3 = cowboy_req:set_resp_body(<<"{'id':'hjklkjh'}">>, Req2),
    {true, Req3, User}.
