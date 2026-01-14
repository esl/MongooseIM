-module(mod_broadcast_rdbms).

-export([init/2,
         create_job/3,
         get_job/2,
         list_jobs/4,
         claim_job/2,
         next_recipients/3,
         mark_recipient_sent/4,
         increment_progress/3,
         finish_job/3,
         fail_job/4,
         abort_job/2,
         delete_jobs/2,
         cleanup_stale_jobs/2]).

-import(rdbms_queries, [limit_offset/0]).

-define(BATCH_LIMIT_DEFAULT, 200).

init(_HostType, _Opts) ->
    LimitOffsetSQL = limit_offset(),
    LimitSQL = rdbms_queries:limit(),

    mongoose_rdbms:prepare(broadcast_insert_job, broadcast_jobs,
                           [id, host_type, domain, name, sender_jid, subject, body,
                            status, start_ts, rate_per_second, recipient_count,
                            progress_count, owner_node, heartbeat_ts, last_error],
                           <<"INSERT INTO broadcast_jobs (id, host_type, domain, name, sender_jid, subject, body, "
                             "status, start_ts, rate_per_second, recipient_count, progress_count, owner_node, heartbeat_ts, last_error) "
                             "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>),

    mongoose_rdbms:prepare(broadcast_insert_recipient, broadcast_recipients,
                           [job_id, luser],
                           <<"INSERT INTO broadcast_recipients (job_id, luser) VALUES (?, ?)">>),

    mongoose_rdbms:prepare(broadcast_select_job, broadcast_jobs,
                           [id],
                           <<"SELECT id, host_type, domain, name, sender_jid, subject, body, status, start_ts, stop_ts, "
                             "rate_per_second, recipient_count, progress_count, owner_node, heartbeat_ts, last_error "
                             "FROM broadcast_jobs WHERE id = ?">>),

        mongoose_rdbms:prepare(broadcast_list_jobs, broadcast_jobs,
                                                     [domain, domain, limit, offset],
                           <<"SELECT id, host_type, domain, name, sender_jid, subject, body, status, start_ts, stop_ts, "
                             "rate_per_second, recipient_count, progress_count, owner_node, heartbeat_ts, last_error "
                             "FROM broadcast_jobs WHERE (? IS NULL OR domain = ?) "
                             "ORDER BY start_ts DESC, id DESC ", LimitOffsetSQL/binary>>),

    mongoose_rdbms:prepare(broadcast_count_jobs, broadcast_jobs,
                           [domain, domain],
                           <<"SELECT COUNT(*) FROM broadcast_jobs WHERE (? IS NULL OR domain = ?)">>),

    mongoose_rdbms:prepare(broadcast_claim_job, broadcast_jobs,
                           [owner_node, heartbeat_ts, id, owner_node],
                           <<"UPDATE broadcast_jobs SET owner_node = ?, heartbeat_ts = ? "
                             "WHERE id = ? AND status = 'running' AND (owner_node IS NULL OR owner_node = ?) ">>),

    mongoose_rdbms:prepare(broadcast_select_next_recipients, broadcast_recipients,
                           [job_id, limit],
                           <<"SELECT luser FROM broadcast_recipients "
                             "WHERE job_id = ? AND sent_ts IS NULL "
                                                         "ORDER BY luser ", LimitSQL/binary>>),

    mongoose_rdbms:prepare(broadcast_mark_recipient_sent, broadcast_recipients,
                           [sent_ts, job_id, luser],
                           <<"UPDATE broadcast_recipients SET sent_ts = ? WHERE job_id = ? AND luser = ? AND sent_ts IS NULL">>),

    mongoose_rdbms:prepare(broadcast_increment_progress, broadcast_jobs,
                           [progress_count, heartbeat_ts, id],
                           <<"UPDATE broadcast_jobs SET progress_count = progress_count + ?, heartbeat_ts = ? WHERE id = ?">>),

    mongoose_rdbms:prepare(broadcast_finish_job, broadcast_jobs,
                           [status, stop_ts, heartbeat_ts, id],
                           <<"UPDATE broadcast_jobs SET status = ?, stop_ts = ?, heartbeat_ts = ? WHERE id = ?">>),

    mongoose_rdbms:prepare(broadcast_fail_job, broadcast_jobs,
                           [status, stop_ts, heartbeat_ts, last_error, id],
                           <<"UPDATE broadcast_jobs SET status = ?, stop_ts = ?, heartbeat_ts = ?, last_error = ? WHERE id = ?">>),

    mongoose_rdbms:prepare(broadcast_abort_job, broadcast_jobs,
                           [stop_ts, heartbeat_ts, id],
                           <<"UPDATE broadcast_jobs SET status = 'aborted_admin', stop_ts = ?, heartbeat_ts = ? "
                             "WHERE id = ? AND status = 'running'">>),

    mongoose_rdbms:prepare(broadcast_delete_jobs_all_stopped, broadcast_jobs,
                           [],
                           <<"DELETE FROM broadcast_jobs WHERE status <> 'running'">>),

    mongoose_rdbms:prepare(broadcast_delete_job, broadcast_jobs,
                           [id],
                           <<"DELETE FROM broadcast_jobs WHERE id = ? AND status <> 'running'">>),

    mongoose_rdbms:prepare(broadcast_cleanup_stale, broadcast_jobs,
                           [stale_threshold],
                           <<"UPDATE broadcast_jobs SET status = 'aborted_errors', "
                             "stop_ts = heartbeat_ts, last_error = 'Stale job (no heartbeat)' "
                             "WHERE status = 'running' AND heartbeat_ts < ?">>),

    ok.

-spec create_job(mongooseim:host_type_or_global(), map(), [binary()]) -> ok | {error, term()}.
create_job(HostType, Job = #{id := JobId}, RecipientUsers) ->
    F = fun() ->
                ok = insert_job_t(HostType, Job),
                ok = insert_recipients_t(HostType, JobId, RecipientUsers),
                ok
        end,
    case rdbms_queries:sql_transaction(HostType, F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

insert_job_t(HostType, #{id := Id,
                        host_type := HostType2,
                        domain := Domain,
                        name := Name,
                        sender_jid := SenderJid,
                        subject := Subject,
                        body := Body,
                        status := Status,
                        start_ts := StartTS,
                        rate_per_second := Rate,
                        recipient_count := RecipientCount,
                        progress_count := Progress,
                        owner_node := OwnerNode,
                        heartbeat_ts := Heartbeat,
                        last_error := LastError}) ->
    StatusBin = status_to_bin(Status),
    Params = [Id, HostType2, Domain, Name, SenderJid, Subject, Body,
              StatusBin, StartTS, Rate, RecipientCount, Progress,
              OwnerNode, Heartbeat, LastError],
    {updated, 1} = mongoose_rdbms:execute(HostType, broadcast_insert_job, Params),
    ok.

insert_recipients_t(_HostType, _JobId, []) ->
    ok;
insert_recipients_t(HostType, JobId, Users) ->
    batch_insert_recipients(HostType, JobId, Users, 1000).

batch_insert_recipients(_HostType, _JobId, [], _BatchSize) ->
    ok;
batch_insert_recipients(HostType, JobId, Users, BatchSize) ->
    {Batch, Rest} = lists:split(erlang:min(BatchSize, length(Users)), Users),
    _ = [mongoose_rdbms:execute(HostType, broadcast_insert_recipient, [JobId, LUser]) || LUser <- Batch],
    batch_insert_recipients(HostType, JobId, Rest, BatchSize).

-spec get_job(mongooseim:host_type_or_global(), pos_integer()) -> {ok, map()} | not_found | {error, term()}.
get_job(HostType, Id) ->
    case mongoose_rdbms:execute(HostType, broadcast_select_job, [Id]) of
        {selected, []} -> not_found;
        {selected, [Row]} -> {ok, row_to_job(Row)};
        Other -> {error, Other}
    end.

-spec list_jobs(mongooseim:host_type_or_global(), undefined | binary(), pos_integer(), non_neg_integer()) ->
          {ok, map()} | {error, term()}.
list_jobs(HostType, Domain, Limit, Index) ->
    Offset = Index,
    DomainParam = domain_param(Domain),
    case mongoose_rdbms:execute(HostType, broadcast_list_jobs, [DomainParam, DomainParam, Limit, Offset]) of
        {selected, Rows} ->
            {selected, [{Count}]} = mongoose_rdbms:execute(HostType, broadcast_count_jobs, [DomainParam, DomainParam]),
            Jobs = [row_to_job(R) || R <- Rows],
            {ok, #{items => Jobs, limit => Limit, index => Index, total_count => Count}};
        Other ->
            {error, Other}
    end.

domain_param(undefined) -> null;
domain_param(Domain) -> Domain.

-spec claim_job(mongooseim:host_type_or_global(), pos_integer()) -> ok | not_claimed | {error, term()}.
claim_job(HostType, Id) ->
    Now = erlang:system_time(second),
    Owner = atom_to_binary(node(), utf8),
    case mongoose_rdbms:execute(HostType, broadcast_claim_job, [Owner, Now, Id, Owner]) of
        {updated, 1} -> ok;
        {updated, 0} -> not_claimed;
        Other -> {error, Other}
    end.

-spec next_recipients(mongooseim:host_type_or_global(), pos_integer(), pos_integer()) -> {ok, [binary()]} | {error, term()}.
next_recipients(HostType, JobId, Limit) ->
    Limit2 = case Limit of
                 undefined -> ?BATCH_LIMIT_DEFAULT;
                 L when is_integer(L), L > 0 -> L;
                 _ -> ?BATCH_LIMIT_DEFAULT
             end,
    case mongoose_rdbms:execute(HostType, broadcast_select_next_recipients, [JobId, Limit2]) of
        {selected, Rows} -> {ok, [U || {U} <- Rows]};
        Other -> {error, Other}
    end.

-spec mark_recipient_sent(mongooseim:host_type_or_global(), pos_integer(), binary(), non_neg_integer()) -> ok | already_sent | {error, term()}.
mark_recipient_sent(HostType, JobId, LUser, SentTS) ->
    case mongoose_rdbms:execute(HostType, broadcast_mark_recipient_sent, [SentTS, JobId, LUser]) of
        {updated, 1} -> ok;
        {updated, 0} -> already_sent;
        Other -> {error, Other}
    end.

-spec increment_progress(mongooseim:host_type_or_global(), pos_integer(), pos_integer()) -> ok | {error, term()}.
increment_progress(HostType, JobId, Inc) ->
    Now = erlang:system_time(second),
    case mongoose_rdbms:execute(HostType, broadcast_increment_progress, [Inc, Now, JobId]) of
        {updated, 1} -> ok;
        Other -> {error, Other}
    end.

-spec finish_job(mongooseim:host_type_or_global(), pos_integer(), atom()) -> ok | {error, term()}.
finish_job(HostType, JobId, Status) ->
    Now = erlang:system_time(second),
    StatusBin = status_to_bin(Status),
    case mongoose_rdbms:execute(HostType, broadcast_finish_job, [StatusBin, Now, Now, JobId]) of
        {updated, 1} -> ok;
        Other -> {error, Other}
    end.

-spec fail_job(mongooseim:host_type_or_global(), pos_integer(), atom(), binary()) -> ok | {error, term()}.
fail_job(HostType, JobId, Status, LastError) ->
    Now = erlang:system_time(second),
    StatusBin = status_to_bin(Status),
    case mongoose_rdbms:execute(HostType, broadcast_fail_job, [StatusBin, Now, Now, LastError, JobId]) of
        {updated, 1} -> ok;
        Other -> {error, Other}
    end.

-spec abort_job(mongooseim:host_type_or_global(), pos_integer()) -> {ok, map()} | not_found | not_running | {error, term()}.
abort_job(HostType, JobId) ->
    Now = erlang:system_time(second),
    case get_job(HostType, JobId) of
        not_found -> not_found;
        {ok, Job = #{status := running}} ->
            case mongoose_rdbms:execute(HostType, broadcast_abort_job, [Now, Now, JobId]) of
                {updated, 1} -> {ok, Job#{status => aborted_admin, stop_ts => Now}};
                {updated, 0} -> not_running;
                Other -> {error, Other}
            end;
        {ok, _Job} ->
            not_running;
        {error, _} = Err ->
            Err
    end.

-spec delete_jobs(mongooseim:host_type_or_global(), undefined | [pos_integer()]) -> {ok, non_neg_integer()} | {error, term()}.
delete_jobs(HostType, undefined) ->
    case mongoose_rdbms:execute(HostType, broadcast_delete_jobs_all_stopped, []) of
        {updated, Count} -> {ok, Count};
        Other -> {error, Other}
    end;
delete_jobs(HostType, Ids) when is_list(Ids) ->
    try
        Counts = [case mongoose_rdbms:execute(HostType, broadcast_delete_job, [Id]) of
                      {updated, C} -> C;
                      Other -> throw({db_error, Other})
                  end || Id <- Ids],
        {ok, lists:sum(Counts)}
    catch
        throw:{db_error, Reason} -> {error, Reason}
    end;
delete_jobs(_, _) ->
    {error, badarg}.

status_to_bin(running) -> <<"running">>;
status_to_bin(success) -> <<"success">>;
status_to_bin(aborted_admin) -> <<"aborted_admin">>;
status_to_bin(aborted_errors) -> <<"aborted_errors">>;
status_to_bin(Other) when is_atom(Other) -> atom_to_binary(Other, utf8).

row_to_job({Id, HostType, Domain, Name, SenderJid, Subject, Body,
            StatusBin, StartTS, StopTS, Rate, RecipientCount, Progress,
            OwnerNode, Heartbeat, LastError}) ->
    #{id => Id,
      host_type => HostType,
      domain => Domain,
      name => Name,
      sender_jid => SenderJid,
      subject => Subject,
      body => Body,
      status => bin_to_status(StatusBin),
      start_ts => StartTS,
      stop_ts => StopTS,
      rate_per_second => Rate,
      recipient_count => RecipientCount,
      progress_count => Progress,
      owner_node => OwnerNode,
      heartbeat_ts => Heartbeat,
      last_error => LastError}.

-spec cleanup_stale_jobs(mongooseim:host_type_or_global(), non_neg_integer()) -> ok | {error, term()}.
cleanup_stale_jobs(HostType, StaleThreshold) ->
    case mongoose_rdbms:execute(HostType, broadcast_cleanup_stale, [StaleThreshold]) of
        {updated, _Count} -> ok;
        Other -> {error, Other}
    end.

bin_to_status(<<"running">>) -> running;
bin_to_status(<<"success">>) -> success;
bin_to_status(<<"aborted_admin">>) -> aborted_admin;
bin_to_status(<<"aborted_errors">>) -> aborted_errors;
bin_to_status(B) when is_binary(B) ->
    try binary_to_existing_atom(B, utf8)
    catch _:_ -> unknown
    end.
