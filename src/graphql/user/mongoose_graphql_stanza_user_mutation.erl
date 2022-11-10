-module(mongoose_graphql_stanza_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [null_to_undefined/1, format_result/2]).

execute(Ctx, _Obj, <<"sendMessage">>, Args) ->
    send_message(Ctx, Args);
execute(Ctx, _Obj, <<"sendMessageHeadLine">>, Args) ->
    send_message_headline(Ctx, Args);
execute(Ctx, _Obj, <<"sendStanza">>, Args) ->
    send_stanza(Ctx, Args).

send_message(#{user := User}, #{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Res = mongoose_stanza_api:send_chat_message(User, null_to_undefined(From), To, Body),
    format_result(Res, #{}).

send_message_headline(#{user := User}, #{<<"from">> := From, <<"to">> := To, <<"body">> := Body,
                                         <<"subject">> := Subject}) ->
    Res = mongoose_stanza_api:send_headline_message(
            User, null_to_undefined(From), To, null_to_undefined(Body), null_to_undefined(Subject)),
    format_result(Res, #{}).

send_stanza(#{user := User}, #{<<"stanza">> := Packet}) ->
    Res = mongoose_stanza_api:send_stanza(User, Packet),
    format_result(Res, #{}).
