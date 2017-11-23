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
-module(mod_aws_sns_defaults).
-author("Rafal Slota").

-behavior(mod_aws_sns).

-include_lib("ejabberd/include/jlib.hrl").

%%%===================================================================
%%% Exports
%%%===================================================================

%% Callbacks
-export([user_guid/1, message_attributes/3, message_attributes/5]).

%% -------------------------------------------------------------------
%% @doc Returns user's GUID for SNS notification based on his JID.
%% -------------------------------------------------------------------
-spec user_guid(UserJID :: ejabberd:jid()) -> mod_aws_sns:user_guid().
user_guid(#jid{} = UserJID) ->
    jid:to_binary(jid:to_lower(jid:to_bare(UserJID))).

%% -------------------------------------------------------------------
%% @doc Returns SNS Message Attributes for presence change notification.
%% -------------------------------------------------------------------
-spec message_attributes(TopicARN :: mod_aws_sns:topic_arn(), UserJID :: ejabberd:jid(),
                         IsOnline :: boolean()) -> mod_aws_sns:attributes().
message_attributes(_TopicARN, _UserJID, _IsOnline) ->
    #{}.

%% -------------------------------------------------------------------
%% @doc Returns SNS Message Attributes for message notification.
%% -------------------------------------------------------------------
-spec message_attributes(TopicARN :: mod_aws_sns:topic_arn(), From :: ejabberd:jid(),
                         To :: ejabberd:jid(), MessageType :: pm | muc, Packet :: jlib:xmlel()) ->
    mod_aws_sns:attributes().
message_attributes(_TopicARN, _From, _To, _MessageType, _Packet) ->
    #{}.
