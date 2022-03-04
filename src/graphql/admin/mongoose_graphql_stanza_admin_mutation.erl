-module(mongoose_graphql_stanza_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

execute(_Ctx, _Obj, <<"sendMessage">>, Args) ->
    send_message(Args);
execute(_Ctx, _Obj, <<"sendMessageHeadLine">>, Args) ->
    send_message_headline(Args);
execute(_Ctx, _Obj, <<"sendStanza">>, Args) ->
    send_stanza(Args).

send_message(#{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Packet = mongoose_stanza_helper:build_message(
               jid:to_binary(From), jid:to_binary(To), Body),
    mongoose_stanza_helper:route(From, To, Packet, true).

send_message_headline(Args = #{<<"from">> := From, <<"to">> := To}) ->
    Packet = mongoose_stanza_helper:build_message_with_headline(
               jid:to_binary(From), jid:to_binary(To), Args),
    mongoose_stanza_helper:route(From, To, Packet, true).

send_stanza(#{<<"stanza">> := Packet}) ->
    From = jid:from_binary(exml_query:attr(Packet, <<"from">>)),
    To = jid:from_binary(exml_query:attr(Packet, <<"to">>)),
    mongoose_stanza_helper:route(From, To, Packet, true).
