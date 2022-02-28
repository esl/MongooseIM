-module(mongoose_graphql_stanza_user_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

-type result() :: {ok, map()} | {error, term()}.

-spec execute(graphql:endpoint_context(), graphql:ast(), binary(), map()) ->
    result().
execute(Ctx, _Obj, <<"sendMessage">>, Opts) ->
    send_message(Ctx, Opts);
execute(Ctx, _Obj, <<"sendMessageHeadLine">>, Opts) ->
    send_message_headline(Ctx, Opts);
execute(Ctx, _Obj, <<"sendStanza">>, Opts) ->
    send_stanza(Ctx, Opts).

send_message(Ctx, Opts) ->
    with_from(Ctx, Opts, fun send_message2/1).

send_message_headline(Ctx, Opts) ->
    with_from(Ctx, Opts, fun send_message_headline2/1).

send_message2(#{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Packet = mongoose_stanza_helper:build_message(jid:to_binary(From), jid:to_binary(To), Body),
    %% SkipAuth = false, because we already checked if From exists
    mongoose_stanza_helper:route(From, To, Packet, false).

send_message_headline2(Opts = #{<<"from">> := From, <<"to">> := To}) ->
    Packet = mongoose_stanza_helper:build_message_with_headline(
               jid:to_binary(From), jid:to_binary(To), Opts),
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

with_from(_Ctx = #{user := User}, Opts, Next) ->
    case maps:get(<<"from">>, Opts, null) of
        null ->
            Next(Opts#{<<"from">> => User});
        From ->
            case compare_bare_jids(User, From) of
                true ->
                    %% We still can allow a custom resource
                    Next(Opts#{<<"from">> => From});
                false ->
                    ?LOG_ERROR(#{what => bad_from_jid,
                                 user_jid => User, from_jid => From}),
                    {error, #{what => bad_from_jid}}
            end
    end.
