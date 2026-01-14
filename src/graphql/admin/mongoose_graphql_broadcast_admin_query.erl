-module(mongoose_graphql_broadcast_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4, job_to_gql/1]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, null_to_undefined/1, undefined_to_null/1]).

execute(_Ctx, _Obj, <<"getBroadcasts">>, Args) ->
    get_broadcasts(Args);
execute(_Ctx, _Obj, <<"getBroadcast">>, Args) ->
    get_broadcast(Args).

get_broadcasts(#{<<"domain">> := Domain, <<"limit">> := Limit, <<"index">> := Index}) ->
    Domain2 = null_to_undefined(Domain),
    Limit2 = null_to_undefined(Limit),
    Index2 = null_to_undefined(Index),
    case mod_broadcast_api:list_broadcasts(#{domain => Domain2, limit => Limit2, index => Index2}) of
        {ok, #{items := Items, limit := L, index := I, total_count := C}} ->
            {ok, #{<<"items">> => [job_to_gql(J) || J <- Items],
                   <<"limit">> => L,
                   <<"index">> => I,
                   <<"totalCount">> => C}};
        Error ->
            make_error(Error, #{domain => Domain2})
    end.

get_broadcast(#{<<"id">> := Id}) ->
    case mod_broadcast_api:get_broadcast(Id) of
        {ok, Job} -> {ok, job_to_gql(Job)};
        Error -> make_error(Error, #{id => Id})
    end.

job_to_gql(Job = #{}) ->
    #{<<"id">> => maps:get(id, Job),
      <<"domain">> => maps:get(domain, Job),
      <<"name">> => maps:get(name, Job),
      <<"senderJid">> => maps:get(sender_jid, Job),
      <<"subject">> => undefined_to_null(maps:get(subject, Job, undefined)),
      <<"body">> => maps:get(body, Job),
      <<"status">> => status_to_enum(maps:get(status, Job)),
      <<"startTs">> => maps:get(start_ts, Job),
      <<"stopTs">> => undefined_to_null(maps:get(stop_ts, Job, undefined)),
      <<"ratePerSecond">> => maps:get(rate_per_second, Job),
      <<"recipientCount">> => maps:get(recipient_count, Job),
      <<"progressCount">> => maps:get(progress_count, Job),
      <<"ownerNode">> => undefined_to_null(maps:get(owner_node, Job, undefined)),
      <<"lastError">> => undefined_to_null(maps:get(last_error, Job, undefined))}.

status_to_enum(running) -> 'RUNNING';
status_to_enum(success) -> 'SUCCESS';
status_to_enum(aborted_admin) -> 'ABORTED_ADMIN';
status_to_enum(aborted_errors) -> 'ABORTED_ERRORS';
status_to_enum(_) -> 'ABORTED_ERRORS'.
