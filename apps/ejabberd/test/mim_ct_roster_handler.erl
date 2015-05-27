%%%==============================================================================
%%% File    : mim_ct_rest_handler.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Description : Rest handler simulating authentication REST API
%%% Created : 6 Aug 2014 by <piotr.nosek@erlang-solutions.com>
%%%==============================================================================

-module(mim_ct_roster_handler).

-behaviour(cowboy_http_handler).

-define(JSON_ROSTER_DIR, <<"roster/">>).

-include_lib("eunit/include/eunit.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    ?debugFmt("Req = ~p~n", [Req]),
    {Method, Req2} = cowboy_req:method(Req),
    ?debugMsg("1"),
    %% {JID, Req3} = cowboy_req:qs_val(<<"JID">>, Req2),
    ?debugMsg("2"),
    {JID, Req3} = cowboy_req:binding(from_jid, Req2),
    ?debugMsg("3"),

    {ok, Req4} = roster_get(Method, JID, Req3),
    ?debugMsg("4"),
    {ok, Req4, State}.

roster_get(<<"GET">>, undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing JID.">>, Req);
roster_get(<<"GET">>, <<"alice@localhost">>, Req) ->
    ?debugFmt("Req = ~p~n", [Req]),
    Response = read_JSON_roster(<<"alice.json">>),
    reply_JSON(Req, Response);
roster_get(<<"GET">>, <<"bob@localhost">>, Req) ->
    Response = read_JSON_roster(<<"bob.json">>),
    reply_JSON(Req, Response);
roster_get(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.

reply_JSON(Req, Response) ->
    ?debugMsg("dupa"),
    cowboy_req:reply(
      200,
      [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
      Response,
      Req).

read_JSON_roster(Name) ->
    ?debugFmt("DIRECTORY = ~p~n", [file:get_cwd()]),
    Directory = ?JSON_ROSTER_DIR,
    {ok, Bin} = file:read_file(<<Directory/binary, Name/binary>>),
    Bin.

