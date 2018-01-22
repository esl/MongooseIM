%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin module defining default custom behaviour for AWS SNS notifications.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_sns_defaults).
-author("Rafal Slota").

-behavior(mod_event_pusher_sns).

-include("jlib.hrl").

%%%===================================================================
%%% Exports
%%%===================================================================

%% Callbacks
-export([user_guid/1, message_attributes/3, message_attributes/5]).

%% -------------------------------------------------------------------
%% @doc Returns user's GUID for SNS notification based on his JID.
%% -------------------------------------------------------------------
-spec user_guid(UserJID :: jid:jid()) -> mod_event_pusher_sns:user_guid().
user_guid(#jid{} = UserJID) ->
    jid:to_binary(jid:to_lower(jid:to_bare(UserJID))).

%% -------------------------------------------------------------------
%% @doc Returns SNS Message Attributes for presence change notification.
%% -------------------------------------------------------------------
-spec message_attributes(TopicARN :: mod_event_pusher_sns:topic_arn(), UserJID :: jid:jid(),
                         IsOnline :: boolean()) -> mod_event_pusher_sns:attributes().
message_attributes(_TopicARN, _UserJID, _IsOnline) ->
    #{}.

%% -------------------------------------------------------------------
%% @doc Returns SNS Message Attributes for message notification.
%% -------------------------------------------------------------------
-spec message_attributes(TopicARN :: mod_event_pusher_sns:topic_arn(), From :: jid:jid(),
                         To :: jid:jid(), MessageType :: pm | muc, Packet :: exml:element()) ->
    mod_event_pusher_sns:attributes().
message_attributes(_TopicARN, _From, _To, _MessageType, _Packet) ->
    #{}.
