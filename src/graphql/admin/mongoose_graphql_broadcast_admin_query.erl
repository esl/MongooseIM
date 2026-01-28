%% @doc GraphQL resolver for broadcast admin queries.
%% @author piotr.nosek@erlang-solutions.com
-module(mongoose_graphql_broadcast_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("../../broadcast/mod_broadcast.hrl").

-import(mongoose_graphql_helper, [make_error/2]).

execute(_Ctx, broadcast, <<"getBroadcasts">>, Args) ->
    get_broadcasts(Args);
execute(_Ctx, broadcast, <<"getBroadcast">>, Args) ->
    get_broadcast(Args).

-spec get_broadcasts(map()) -> {ok, [map()]} | {error, resolver_error()}.
get_broadcasts(#{<<"domain">> := Domain, <<"limit">> := Limit, <<"index">> := Index}) ->
    case mod_broadcast_api:get_broadcasts(Domain, Limit, Index) of
        {ok, Broadcasts} ->
            {ok, [format_broadcast(B) || B <- Broadcasts]};
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
      <<"executionState">> => format_execution_state(ExecutionState),
      <<"abortionReason">> => undefined_to_null(AbortionReason),
      <<"recipientGroup">> => format_recipient_group(RecipientGroup),
      <<"recipientCount">> => RecipientCount,
      <<"recipientsProcessed">> => RecipientsProcessed}.

-spec format_execution_state(execution_state()) -> binary().
format_execution_state(running) -> <<"RUNNING">>;
format_execution_state(finished) -> <<"FINISHED">>;
format_execution_state(abort_error) -> <<"ABORT_ERROR">>;
format_execution_state(abort_admin) -> <<"ABORT_ADMIN">>.

-spec format_recipient_group(recipient_group()) -> binary().
format_recipient_group(all_users_in_domain) -> <<"ALL_USERS_IN_DOMAIN">>.

-spec datetime_to_microseconds(calendar:datetime() | undefined) -> integer() | null.
datetime_to_microseconds(undefined) -> null;
datetime_to_microseconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) * 1000000.

-spec undefined_to_null(binary() | undefined) -> binary() | null.
undefined_to_null(undefined) -> null;
undefined_to_null(Value) -> Value.
