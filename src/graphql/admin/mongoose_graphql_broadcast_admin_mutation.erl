%% @doc GraphQL resolver for broadcast admin mutations.
%% @author piotr.nosek@erlang-solutions.com
-module(mongoose_graphql_broadcast_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("../../broadcast/mod_broadcast.hrl").

-import(mongoose_graphql_helper, [make_error/2]).

execute(_Ctx, broadcast, <<"startBroadcast">>, Args) ->
    start_broadcast(Args);
execute(_Ctx, broadcast, <<"abortBroadcast">>, Args) ->
    abort_broadcast(Args);
execute(_Ctx, broadcast, <<"deleteInactiveBroadcastsByIds">>, Args) ->
    delete_inactive_broadcasts_by_ids(Args);
execute(_Ctx, broadcast, <<"deleteInactiveBroadcastsByDomain">>, Args) ->
    delete_inactive_broadcasts_by_domain(Args).

-spec start_broadcast(map()) -> {ok, map()} | {error, resolver_error()}.
start_broadcast(#{<<"name">> := Name,
                  <<"domain">> := Domain,
                  <<"messageSubject">> := MessageSubject,
                  <<"messageBody">> := MessageBody,
                  <<"senderJid">> := SenderJid,
                  <<"messageRate">> := MessageRate,
                  <<"recipientGroup">> := RecipientGroup}) ->
    RecipientGroupAtom = parse_recipient_group(RecipientGroup),
    JobSpec = #{name => Name,
                domain => Domain,
                sender => SenderJid,
                subject => MessageSubject,
                body => MessageBody,
                message_rate => MessageRate,
                recipient_group => RecipientGroupAtom},
    case mod_broadcast_api:start_broadcast(JobSpec) of
        {ok, Id} ->
            {ok, make_mutation_result(<<"Broadcast job started">>, [Id])};
        Error ->
            make_error(Error, #{domain => Domain, name => Name})
    end.

-spec abort_broadcast(map()) -> {ok, map()} | {error, resolver_error()}.
abort_broadcast(#{<<"domain">> := Domain, <<"id">> := Id}) ->
    case mod_broadcast_api:abort_broadcast(Domain, Id) of
        {ok, Id} ->
            {ok, make_mutation_result(<<"Broadcast job aborted">>, [Id])};
        Error ->
            make_error(Error, #{domain => Domain, id => Id})
    end.

-spec delete_inactive_broadcasts_by_ids(map()) -> {ok, map()} | {error, resolver_error()}.
delete_inactive_broadcasts_by_ids(#{<<"domain">> := Domain, <<"ids">> := Ids}) ->
    case mod_broadcast_api:delete_inactive_broadcasts_by_ids(Domain, Ids) of
        {ok, DeletedIds} ->
            {ok, make_mutation_result(<<"Inactive broadcasts deleted">>, DeletedIds)};
        Error ->
            make_error(Error, #{domain => Domain, ids => Ids})
    end.

-spec delete_inactive_broadcasts_by_domain(map()) -> {ok, map()} | {error, resolver_error()}.
delete_inactive_broadcasts_by_domain(#{<<"domain">> := Domain}) ->
    case mod_broadcast_api:delete_inactive_broadcasts_by_domain(Domain) of
        {ok, DeletedIds} ->
            {ok, make_mutation_result(<<"Inactive broadcasts deleted">>, DeletedIds)};
        Error ->
            make_error(Error, #{domain => Domain})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec make_mutation_result(binary(), [integer()]) -> map().
make_mutation_result(Message, Ids) ->
    #{<<"message">> => Message, <<"ids">> => Ids}.

-spec parse_recipient_group(binary()) -> recipient_group().
parse_recipient_group(<<"ALL_USERS_IN_DOMAIN">>) -> all_users_in_domain.
