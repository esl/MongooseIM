%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2016 21:59
%%%-------------------------------------------------------------------
-module(packet).
-author("bartekgorny").

-include("jlib.hrl").
-include("ejabberd.hrl").

%% API
-export([initialise/1, initialise/2, to_binary/1, pass/2, enter/1, exit/1]).

initialise(#xmlel{name = <<"message">>, uid = none} = Xi) ->
    Uid = make_ref(),
    initialise(Xi, Uid);
initialise(X) ->
    X.

initialise(#xmlel{uid = none} = Xi, Uid) ->
    X = Xi#xmlel{uid = Uid},
    X;
initialise(X, _) ->
    X.

to_binary(X) ->
    exml:to_binary(X).

pass(#xmlel{name = <<"message">>} = X, Marker) ->
    Nx = X#xmlel{meta = [Marker|X#xmlel.meta]},
    passmsg(Nx, Marker),
    Nx;
pass(X, _) ->
    X.

enter(#xmlel{name = <<"message">>} = X) ->
    entermsg(X);
enter(_) ->
    ok.

exit(#xmlel{name = <<"message">>} = X) ->
    exitmsg(X);
exit(_) ->
    ok.

exitmsg(X) ->
    ?ERROR_MSG("======> ~p (~p)", [X#xmlel.name, X#xmlel.uid]),
    ?ERROR_MSG("History: ~p~n", [X#xmlel.meta]).
entermsg(X) ->
    ?ERROR_MSG("~p (~p) ======>", [X#xmlel.name, X#xmlel.uid]).
passmsg(X, Marker) ->
    ?ERROR_MSG("=> ~p (~p) [~p, ~p] =>", [X#xmlel.name, X#xmlel.uid, Marker, self()]).
