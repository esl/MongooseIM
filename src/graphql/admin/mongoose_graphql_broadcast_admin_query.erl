-module(mongoose_graphql_broadcast_admin_query).
-behaviour(mongoose_graphql).
-author('piotr.nosek@erlang-solutions.com').

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("../../broadcast/mod_broadcast.hrl").

-import(mongoose_graphql_helper, [undefined_to_null/1, make_error/2]).

execute(_Ctx, broadcast, <<"getBroadcasts">>, Args) ->
    get_broadcasts(Args);
execute(_Ctx, broadcast, <<"getBroadcast">>, Args) ->
    get_broadcast(Args).

-spec get_broadcasts(map()) -> {ok, [map()]} | {error, resolver_error()}.
get_broadcasts(#{<<"domain">> := Domain, <<"limit">> := Limit, <<"index">> := Index}) ->
    case mod_broadcast_api:get_broadcasts(Domain, Limit, Index) of
        {ok, Broadcasts} ->
            {ok, [{ok, format_broadcast(B)} || B <- Broadcasts]};
        Error ->
            make_error(Error, #{domain => Domain})
    end.

-spec get_broadcast(map()) -> {ok, map()} | {error, resolver_error()}.
get_broadcast(#{<<"domain">> := Domain, <<"id">> := Id}) ->
    case mod_broadcast_api:get_broadcast(Domain, Id) of
        {ok, Broadcast} ->
            {ok, format_broadcast(Broadcast)};
        Error ->
            make_error(Error, #{domain => Domain, id => Id})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec format_broadcast(broadcast_job()) -> map().
format_broadcast(#broadcast_job{
    id = Id,
    name = Name,
    domain = Domain,
    subject = MessageSubject,
    body = MessageBody,
    sender = SenderJid,
    message_rate = MessageRate,
    owner_node = OwnerNode,
    created_at = CreateTimestamp,
    started_at = StartTimestamp,
    stopped_at = StopTimestamp,
    execution_state = ExecutionState,
    abortion_reason = AbortionReason,
    recipient_group = RecipientGroup,
    recipient_count = RecipientCount,
    recipients_processed = RecipientsProcessed
}) ->
    #{<<"id">> => Id,
      <<"name">> => Name,
      <<"domain">> => Domain,
      <<"messageSubject">> => MessageSubject,
      <<"messageBody">> => MessageBody,
      <<"senderJid">> => SenderJid,
      <<"messageRate">> => MessageRate,
      <<"ownerNode">> => atom_to_binary(OwnerNode, utf8),
      <<"createTimestamp">> => datetime_to_microseconds(CreateTimestamp),
      <<"startTimestamp">> => datetime_to_microseconds(StartTimestamp),
      <<"stopTimestamp">> => datetime_to_microseconds(StopTimestamp),
      <<"executionState">> => ExecutionState,
      <<"abortionReason">> => undefined_to_null(AbortionReason),
      <<"recipientGroup">> => RecipientGroup,
      <<"recipientCount">> => RecipientCount,
      <<"recipientsProcessed">> => RecipientsProcessed}.

-spec datetime_to_microseconds(calendar:datetime() | undefined) -> integer() | null.
datetime_to_microseconds(undefined) -> null;
datetime_to_microseconds(DateTime) ->
    calendar:universal_time_to_system_time(DateTime, [{unit, microsecond}]).
