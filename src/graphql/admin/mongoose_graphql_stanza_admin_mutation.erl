-module(mongoose_graphql_stanza_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [null_to_undefined/1, format_result/2]).

execute(_Ctx, _Obj, <<"sendMessage">>, Args) ->
    send_message(Args);
execute(_Ctx, _Obj, <<"sendMessageHeadLine">>, Args) ->
    send_message_headline(Args);
execute(_Ctx, _Obj, <<"sendStanza">>, Args) ->
    send_stanza(Args).

send_message(#{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Res = mongoose_stanza_api:send_chat_message(undefined, From, To, Body),
    format_result(Res, #{from => jid:to_binary(From)}).

send_message_headline(#{<<"from">> := From, <<"to">> := To,
                        <<"body">> := Body, <<"subject">> := Subject}) ->
    Res = mongoose_stanza_api:send_headline_message(
            undefined, From, To, null_to_undefined(Body), null_to_undefined(Subject)),
    format_result(Res, #{from => jid:to_binary(From)}).

send_stanza(#{<<"stanza">> := Packet}) ->
    Res = mongoose_stanza_api:send_stanza(undefined, Packet),
    format_result(Res, #{}).
