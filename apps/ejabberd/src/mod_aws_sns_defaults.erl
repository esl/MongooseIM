-module(mod_aws_sns_defaults).

-include_lib("ejabberd/include/jlib.hrl").

-export([user_guid/1, message_attributes/3, message_attributes/5]).

-type attributes() :: #{term() => term()}.

user_guid(#jid{} = UserJID) ->
    jid:to_binary(jid:to_bare(UserJID)).


-spec message_attributes(TopicARN :: string(), UserJID :: ejabberd:jid(), IsOnline :: boolean()) ->
    attributes().
message_attributes(TopicARN, UserJID, IsOnline) ->
    #{}.

-spec message_attributes(TopicARN :: string(), From :: ejabberd:jid(), To :: ejabberd:jid(),
                         MessageType :: pm | muc, Packet :: binary()) -> attributes().
message_attributes(TopicARN, From, To, MessageType, Packet) ->
    #{}.