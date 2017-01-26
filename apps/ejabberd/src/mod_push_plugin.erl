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
-module(mod_push_plugin).
-behavior(mod_push_plugin).
-author('rafal.slota@erlang-solutions.com').

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

%% API
-export([should_publish/3]).

-callback should_publish(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) ->
    boolean().


-spec should_publish(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) ->
    boolean().
should_publish(_From, To = #jid{luser = LUser, lserver = LServer}, _Packet) ->
    try ejabberd_users:does_user_exist(LUser, LServer) of
        false ->
            false;
        true ->
            case catch lists:max(ejabberd_sm:get_user_present_pids(LUser, LServer)) of
                {Priority, _} when Priority >= 0 ->
                    false;
                _ ->
                    is_offline(To)
            end
    catch
        _:_ ->
            is_offline(To)
    end.


is_offline(#jid{luser = LUser, lserver = LServer}) ->
    case catch lists:max(ejabberd_sm:get_user_present_pids(LUser, LServer)) of
        {Priority, _} when is_integer(Priority), Priority >= 0 ->
            false;
        _ ->
            true
    end.