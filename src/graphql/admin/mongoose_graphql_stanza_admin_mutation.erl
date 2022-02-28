-module(mongoose_graphql_stanza_admin_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

-type result() :: {ok, map()} | {error, term()}.

-spec execute(graphql:endpoint_context(), graphql:ast(), binary(), map()) ->
    result().
execute(_Ctx, _Obj, <<"sendMessage">>, Opts) ->
    send_message(Opts);
execute(_Ctx, _Obj, <<"sendMessageHeadLine">>, Opts) ->
    send_message_headline(Opts);
execute(_Ctx, _Obj, <<"sendStanza">>, Opts) ->
    send_stanza(Opts).

send_message(#{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Packet = mongoose_stanza_helper:build_message(
               jid:to_binary(From), jid:to_binary(To), Body),
    mongoose_stanza_helper:route(From, To, Packet, true).

send_message_headline(Opts = #{<<"from">> := From, <<"to">> := To}) ->
    Packet = mongoose_stanza_helper:build_message_with_headline(
               jid:to_binary(From), jid:to_binary(To), Opts),
    mongoose_stanza_helper:route(From, To, Packet, true).

send_stanza(#{<<"stanza">> := Packet}) ->
    From = jid:from_binary(exml_query:attr(Packet, <<"from">>)),
    To = jid:from_binary(exml_query:attr(Packet, <<"to">>)),
    mongoose_stanza_helper:route(From, To, Packet, true).
