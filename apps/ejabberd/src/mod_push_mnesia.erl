%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @todo: write me!
%%% @end
%%%-------------------------------------------------------------------
-module(mod_push_mnesia).
-author("Rafal Slota").
-behavior(mod_push).

-export([init/2, enable/4, disable/3, get_publish_services/1]).


-spec init(Host :: ejabberd:server(), Opts :: list()) -> ok.
init(Host, Opts) ->
    ok.


-spec enable(User :: ejabberd:jid(), PubSub :: ejabberd:jid(),
                 Node :: binary(), Forms :: any()) -> ok.
enable(User, PubSub, Node, Forms) ->
    ok.


-spec disable(User :: ejabberd:jid(), PubSub :: ejabberd:jid(), Node :: binary()) -> ok.
disable(User, PubSub, Node) ->
    ok.


-spec get_publish_services(User :: ejabberd:jid()) ->
    [{PubSub :: ejabberd:jid(), Node :: binary()}].
get_publish_services(User) ->
    [].