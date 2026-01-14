%% @doc Public API for managing broadcast jobs (used by GraphQL/admin tooling).
-module(mod_broadcast_api).

-export([start_broadcast/1,
         get_broadcast/1,
         list_broadcasts/1,
         abort_broadcast/1,
         delete_broadcasts/1]).

-include("mongoose_logger.hrl").

-type api_error() :: {domain_not_found | not_found | not_allowed | bad_request | server_error, iodata()}.

%% Input maps are used to keep API stable for GraphQL additions.
-spec start_broadcast(map()) -> {ok, map()} | api_error().
start_broadcast(#{domain := Domain,
                 name := Name,
                 sender_jid := SenderJid,
                 subject := Subject,
                 body := Body,
                 rate_per_second := Rate,
                 recipients := Recipients}) ->
    PrepDomain = jid:nameprep(Domain),
    case mongoose_domain_api:get_domain_host_type(PrepDomain) of
        {ok, HostType} ->
            do_start_broadcast(HostType, PrepDomain, Name, SenderJid, Subject, Body, Rate, Recipients);
        {error, not_found} ->
            {domain_not_found, <<"Unknown domain">>}
    end;
start_broadcast(_) ->
    {bad_request, <<"Invalid arguments">>}.

-spec get_broadcast(pos_integer()) -> {ok, map()} | api_error().
get_broadcast(Id) when is_integer(Id), Id > 0 ->
    case mod_broadcast_rdbms:get_job(global, Id) of
        {ok, Job} -> {ok, Job};
        not_found -> {not_found, <<"Broadcast not found">>};
        {error, Reason} ->
            ?LOG_ERROR(#{what => mod_broadcast_get_failed, id => Id, reason => Reason}),
            {server_error, <<"Database error">>}
    end;
get_broadcast(_) ->
    {bad_request, <<"Invalid broadcast id">>}.

-spec list_broadcasts(map()) -> {ok, map()} | api_error().
list_broadcasts(#{domain := Domain, limit := Limit, index := Index}) ->
    Domain2 = case Domain of
                  undefined -> undefined;
                  D -> jid:nameprep(D)
              end,
    Limit2 = normalize_limit(Limit),
    Index2 = normalize_index(Index),
    case mod_broadcast_rdbms:list_jobs(global, Domain2, Limit2, Index2) of
        {ok, Payload} -> {ok, Payload};
        {error, Reason} ->
            ?LOG_ERROR(#{what => mod_broadcast_list_failed, reason => Reason}),
            {server_error, <<"Database error">>}
    end;
list_broadcasts(_) ->
    {bad_request, <<"Invalid arguments">>}.

-spec abort_broadcast(pos_integer()) -> {ok, map()} | api_error().
abort_broadcast(Id) when is_integer(Id), Id > 0 ->
    case mod_broadcast_rdbms:abort_job(global, Id) of
        {ok, Job} ->
            maybe_notify_manager(Job),
            {ok, Job};
        not_found -> {not_found, <<"Broadcast not found">>};
        not_running -> {not_allowed, <<"Broadcast is not running">>};
        {error, Reason} ->
            ?LOG_ERROR(#{what => mod_broadcast_abort_failed, id => Id, reason => Reason}),
            {server_error, <<"Database error">>}
    end;
abort_broadcast(_) ->
    {bad_request, <<"Invalid broadcast id">>}.

-spec delete_broadcasts(map()) -> {ok, map()} | api_error().
delete_broadcasts(#{ids := Ids}) ->
    case mod_broadcast_rdbms:delete_jobs(global, Ids) of
        {ok, DeletedCount} -> {ok, #{deleted_count => DeletedCount}};
        {error, Reason} ->
            ?LOG_ERROR(#{what => mod_broadcast_delete_failed, reason => Reason}),
            {server_error, <<"Database error">>}
    end;
delete_broadcasts(_) ->
    {bad_request, <<"Invalid arguments">>}.

%% Internal

do_start_broadcast(HostType, Domain, Name, SenderJid, Subject, Body, Rate, Recipients) ->
    case normalize_recipients(Domain, Recipients) of
        {ok, RecipientUsers} ->
            JobId = make_job_id(),
            StartTS = erlang:system_time(second),
            SenderBin = jid:to_binary(jid:to_bare(SenderJid)),
            SubjectBin = maybe_bin(Subject),
            BodyBin = iolist_to_binary(Body),
            Rate2 = erlang:max(1, Rate),
            Job0 = #{id => JobId,
                    host_type => HostType,
                    domain => Domain,
                    name => iolist_to_binary(Name),
                    sender_jid => SenderBin,
                    subject => SubjectBin,
                    body => BodyBin,
                    status => running,
                    start_ts => StartTS,
                    stop_ts => undefined,
                    rate_per_second => Rate2,
                    recipient_count => length(RecipientUsers),
                    progress_count => 0,
                    owner_node => atom_to_binary(node(), utf8),
                    heartbeat_ts => StartTS,
                    last_error => undefined},
            case mod_broadcast_rdbms:create_job(global, Job0, RecipientUsers) of
                ok ->
                    ok = mod_broadcast_manager:ensure_started(HostType),
                    ok = mod_broadcast_manager:start_job(HostType, JobId),
                    {ok, Job0};
                {error, Reason} ->
                    ?LOG_ERROR(#{what => mod_broadcast_create_failed,
                                 host_type => HostType,
                                 domain => Domain,
                                 reason => Reason}),
                    {server_error, <<"Database error">>}
            end;
        {error, Msg} ->
            {bad_request, Msg}
    end.

maybe_notify_manager(#{host_type := HostType, id := JobId}) ->
    ok = mod_broadcast_manager:ensure_started(HostType),
    ok = mod_broadcast_manager:abort_job(HostType, JobId);
maybe_notify_manager(_) ->
    ok.

make_job_id() ->
    %% 16-bit node hash + 48-bit unique int.
    NodePart = erlang:phash2(node(), 1 bsl 16),
    UniquePart = erlang:unique_integer([monotonic, positive]) band ((1 bsl 48) - 1),
    (NodePart bsl 48) bor UniquePart.

normalize_limit(undefined) -> 50;
normalize_limit(L) when is_integer(L), L > 0 -> erlang:min(L, 500);
normalize_limit(_) -> 50.

normalize_index(undefined) -> 0;
normalize_index(I) when is_integer(I), I >= 0 -> I;
normalize_index(_) -> 0.

maybe_bin(undefined) -> undefined;
maybe_bin(null) -> undefined;
maybe_bin(B) when is_binary(B) -> B;
maybe_bin(Io) -> iolist_to_binary(Io).

normalize_recipients(Domain, #{type := all_users}) ->
    Users = ejabberd_auth:get_vh_registered_users(Domain),
    {ok, [LU || {LU, _LS} <- Users]};
normalize_recipients(_Domain, #{type := usernames, usernames := Usernames}) when is_list(Usernames) ->
    %% Usernames are expected to be binaries (UserName scalar).
    {ok, lists:usort([U || U <- Usernames, is_binary(U), byte_size(U) > 0])};
normalize_recipients(_, _) ->
    {error, <<"Invalid recipients input">>}.
