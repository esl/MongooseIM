-module(mongoose_graphql_stanza_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

execute(Ctx, _Obj, <<"sendMessage">>, Args) ->
    send_message(Ctx, Args);
execute(Ctx, _Obj, <<"sendMessageHeadLine">>, Args) ->
    send_message_headline(Ctx, Args);
execute(Ctx, _Obj, <<"sendStanza">>, Args) ->
    send_stanza(Ctx, Args).

send_message(Ctx, Args) ->
    with_from(Ctx, Args, fun send_message2/1).

send_message_headline(Ctx, Args) ->
    with_from(Ctx, Args, fun send_message_headline2/1).

send_message2(#{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Packet = mongoose_stanza_helper:build_message(jid:to_binary(From), jid:to_binary(To), Body),
    %% SkipAuth = false, because we already checked if From exists
    mongoose_stanza_helper:route(From, To, Packet, false).

send_message_headline2(Args = #{<<"from">> := From, <<"to">> := To}) ->
    Packet = mongoose_stanza_helper:build_message_with_headline(
               jid:to_binary(From), jid:to_binary(To), Args),
    mongoose_stanza_helper:route(From, To, Packet, false).

send_stanza(#{user := User}, #{<<"stanza">> := Packet}) ->
    From = jid:from_binary(exml_query:attr(Packet, <<"from">>)),
    To = jid:from_binary(exml_query:attr(Packet, <<"to">>)),
    case compare_bare_jids(User, From) of
        true ->
            mongoose_stanza_helper:route(From, To, Packet, false);
        false ->
            {error, #{what => bad_from_jid}}
    end.

compare_bare_jids(#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S}) -> true;
compare_bare_jids(_, _) -> false.

with_from(_Ctx = #{user := User}, Args, Next) ->
    case maps:get(<<"from">>, Args, null) of
        null ->
            Next(Args#{<<"from">> => User});
        From ->
            case compare_bare_jids(User, From) of
                true ->
                    %% We still can allow a custom resource
                    Next(Args#{<<"from">> => From});
                false ->
                    ?LOG_ERROR(#{what => bad_from_jid,
                                 user_jid => User, from_jid => From}),
                    {error, #{what => bad_from_jid}}
            end
    end.
