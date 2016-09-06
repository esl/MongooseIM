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
-export([initialise/1, initialise/2, to_binary/1, pass/2, clone/2, clone/3]).

initialise(#xmlel{uid = none} = Xi) ->
    Uid = make_ref(),
    initialise(Xi, Uid);
initialise(X) ->
    X.

initialise(#xmlel{uid = none} = Xi, Uid) ->
    X = Xi#xmlel{uid = Uid},
    notifye(X),
    X;
initialise(X, _) ->
    X.

clone(X, none) ->
    X;
clone(X, Name) ->
    X#xmlel{name = Name, meta = [cloned|X#xmlel.meta]}.

clone(X, Name, Attrs) ->
    Nx = clone(X, Name),
    Nx#xmlel{attrs = Attrs}.

to_binary(X) ->
    notify(X),
    exml:to_binary(X).

pass(#xmlel{name = <<"presence">>} = X, Marker) ->
    Nx = X#xmlel{meta = [Marker|X#xmlel.meta]},
    passmsg(Nx, Marker),
    Nx;
pass(X, _) ->
    X.

notifye(#xmlel{name = <<"presence">>} = X) ->
    entermsg(X);
notifye(X) ->
    ok.

%%notify(#xmlel{name = <<"iq">>} = X) ->
%%    exitmsg(X);
notify(#xmlel{name = <<"presence">>} = X) ->
    exitmsg(X);
%%notify(#xmlel{name = <<"presence">>} = X) ->
%%    exitmsg(X);
notify(_) ->
    ok.

exitmsg(X) ->
    ?ERROR_MSG("======> ~p (~p)", [X#xmlel.name, X#xmlel.uid]),
    ?ERROR_MSG("History: ~p~n", [X#xmlel.meta]).
entermsg(X) ->
    ?ERROR_MSG("~p (~p) ======>", [X#xmlel.name, X#xmlel.uid]).
passmsg(X, Marker) ->
    ?ERROR_MSG("=> ~p (~p) [~p, ~p] =>", [X#xmlel.name, X#xmlel.uid, Marker, self()]).
