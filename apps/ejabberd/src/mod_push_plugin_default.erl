%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Default plugin module for mod_push. This module allows for some dynamic customizations.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_push_plugin_default).
-behavior(mod_push_plugin).
-author('rafal.slota@erlang-solutions.com').

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

%% Callback API
-export([should_publish/3, sender_id/2]).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

%% Callback 'should_publish'
-spec should_publish(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) ->
                            boolean().
should_publish(_From, To = #jid{luser = LUser, lserver = LServer}, _Packet) ->
    try ejabberd_users:does_user_exist(LUser, LServer) of
        false ->
            false;
        true ->
            is_offline(To)
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

%% Callback 'sender_id'
-spec sender_id(From :: ejabberd:jid(), Packet :: jlib:xmlel()) -> SenderId :: binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> ->
            jid:to_binary(jid:to_bare(jid:to_lower(From)));
        <<"groupchat">> ->
            jid:to_binary(jid:to_lower(From))
    end.
