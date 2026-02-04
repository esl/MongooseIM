%%%-------------------------------------------------------------------
%%% @doc RDBMS backend for mod_broadcast.
%%%-------------------------------------------------------------------

-module(mod_broadcast_rdbms).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(mod_broadcast_backend).

-export([init/2,
         create_job/2,
         get_job/2,
         get_jobs_by_domain/4,
         get_running_jobs/1,
         get_worker_state/2,
         set_job_started/2,
         update_worker_state/3,
         set_job_finished/2,
         set_job_aborted_error/3,
         set_job_aborted_admin/2,
         delete_job/2,
         delete_inactive_jobs_by_domain/2]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

-include("mongoose_logger.hrl").
-include("mod_broadcast.hrl").

%%====================================================================
%% API
%%====================================================================

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

-spec create_job(mongooseim:host_type(), mod_broadcast_backend:job_spec()) ->
    {ok, JobId :: broadcast_job_id()} | {error, already_running}.
create_job(HostType, JobSpec) ->
    #{name := Name, domain := Domain, sender := Sender,
      subject := Subject, body := Body, message_rate := Rate,
      recipient_group := RecipientGroup, recipient_count := RecipientCount} = JobSpec,
    OwnerNode = atom_to_binary(node(), utf8),
    SenderBin = jid:to_binary(Sender),
    RecipientGroupBin = encode_recipient_group(RecipientGroup),
    try execute_successfully(HostType, broadcast_create_job,
                             [Name, Domain, SenderBin, Subject, Body, Rate,
                              RecipientGroupBin, OwnerNode, RecipientCount]) of
        {updated, 1, [{JobId}]} ->
            {ok, JobId}
    catch
        error:#{reason := {error, duplicate_key}} ->
            {error, already_running}
    end.

-spec get_job(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_job()} | {error, not_found}.
get_job(HostType, JobId) ->
    case execute_successfully(HostType, broadcast_get_job, [JobId]) of
        {selected, [Row]} ->
            {ok, row_to_job(HostType, Row)};
        {selected, []} ->
            {error, not_found}
    end.

-spec get_running_jobs(mongooseim:host_type()) -> {ok, [broadcast_job()]}.
get_running_jobs(HostType) ->
    OwnerNode = atom_to_binary(node(), utf8),
    {selected, Rows} = execute_successfully(HostType, broadcast_get_running_jobs, [OwnerNode]),
    Jobs = [row_to_job(HostType, R) || R <- Rows],
    {ok, Jobs}.

-spec get_jobs_by_domain(mongooseim:host_type(), jid:lserver(),
                         Limit :: pos_integer(), Offset :: non_neg_integer()) ->
    {ok, [broadcast_job()]}.
get_jobs_by_domain(HostType, Domain, Limit, Offset) ->
    {selected, Rows} = execute_successfully(HostType, broadcast_get_jobs_by_domain,
                                            [Domain, Limit, Offset]),
    Jobs = [row_to_job(HostType, R) || R <- Rows],
    {ok, Jobs}.

-spec get_worker_state(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_worker_state()} | {error, not_found}.
get_worker_state(HostType, JobId) ->
    case execute_successfully(HostType, broadcast_get_worker_state, [JobId]) of
        {selected, [{Cursor, RecipientsProcessed}]} ->
            {ok, #broadcast_worker_state{cursor = maybe_null(Cursor),
                                         recipients_processed = RecipientsProcessed}};
        {selected, []} ->
            {error, not_found}
    end.

-spec set_job_started(mongooseim:host_type(), JobId :: broadcast_job_id()) -> ok.
set_job_started(HostType, JobId) ->
    {updated, 1} = execute_successfully(HostType, broadcast_set_job_started, [JobId]),
    ok.

-spec update_worker_state(mongooseim:host_type(), JobId :: broadcast_job_id(),
                          WorkerState :: broadcast_worker_state()) ->
    ok.
update_worker_state(HostType, JobId, WorkerState) ->
    Cursor = null_if_undefined(WorkerState#broadcast_worker_state.cursor),
    RecipientsProcessed = WorkerState#broadcast_worker_state.recipients_processed,
    {updated, _} = execute_successfully(HostType, broadcast_upsert_worker_state,
                                        [JobId, Cursor, RecipientsProcessed]),
    ok.

-spec set_job_finished(mongooseim:host_type(), JobId :: broadcast_job_id()) -> ok.
set_job_finished(HostType, JobId) ->
    {updated, 1} = execute_successfully(HostType, broadcast_set_job_finished, [JobId]),
    ok.

-spec set_job_aborted_error(mongooseim:host_type(), JobId :: broadcast_job_id(), Reason :: binary()) ->
    ok.
set_job_aborted_error(HostType, JobId, Reason) ->
    {updated, 1} = execute_successfully(HostType, broadcast_set_job_aborted_error, [Reason, JobId]),
    ok.

-spec set_job_aborted_admin(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok.
set_job_aborted_admin(HostType, JobId) ->
    {updated, 1} = execute_successfully(HostType, broadcast_set_job_aborted_admin, [JobId]),
    ok.

-spec delete_job(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok.
delete_job(HostType, JobId) ->
    {updated, 1} = execute_successfully(HostType, broadcast_delete_job, [JobId]),
    ok.

-spec delete_inactive_jobs_by_domain(mongooseim:host_type(), jid:lserver()) ->
    {ok, [integer()]}.
delete_inactive_jobs_by_domain(HostType, Domain) ->
    %% MySQL doesn't support RETURNING in DELETE, so we must SELECT first
    {selected, Rows} = execute_successfully(HostType,
                                            broadcast_get_inactive_job_ids_by_domain,
                                            [Domain]),
    JobIds = [mongoose_rdbms:result_to_integer(Id) || {Id} <- Rows],
    {updated, _} = execute_successfully(HostType,
                                        broadcast_delete_inactive_jobs_by_domain,
                                        [Domain]),
    {ok, JobIds}.

%%====================================================================
%% Internal functions
%%====================================================================

prepare_queries(HostType) ->
    prepare(broadcast_create_job, broadcast_jobs,
            [name, server, from_jid, subject, message, rate,
             recipient_group, owner_node, recipient_count],
            <<"INSERT INTO broadcast_jobs "
              "(name, server, from_jid, subject, message, rate, "
              "recipient_group, owner_node, recipient_count) "
              "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) "
              "RETURNING id">>),

    prepare(broadcast_get_job, broadcast_jobs,
            [id],
            <<"SELECT j.id, j.name, j.server, j.from_jid, j.subject, j.message, j.rate, "
              "j.recipient_group, j.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at "
              "FROM broadcast_jobs j "
              "LEFT JOIN broadcast_worker_state ws ON j.id = ws.broadcast_id "
              "WHERE j.id = ?">>),

    prepare(broadcast_get_running_jobs, broadcast_jobs,
            [owner_node],
            <<"SELECT j.id, j.name, j.server, j.from_jid, j.subject, j.message, j.rate, "
              "j.recipient_group, j.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at "
              "FROM broadcast_jobs j "
              "LEFT JOIN broadcast_worker_state ws ON j.id = ws.broadcast_id "
              "WHERE j.owner_node = ? AND j.execution_state = 'running'">>),

    prepare(broadcast_get_worker_state, broadcast_worker_state,
            [broadcast_id],
            <<"SELECT cursor_user, recipients_processed FROM broadcast_worker_state "
              "WHERE broadcast_id = ?">>),

    prepare(broadcast_set_job_started, broadcast_jobs,
            [id],
            <<"UPDATE broadcast_jobs SET started_at = CURRENT_TIMESTAMP"
              " WHERE id = ?">>),

    prepare(broadcast_upsert_worker_state, broadcast_worker_state,
            [broadcast_id, cursor_user, recipients_processed],
            upsert_worker_state_query(HostType)),

    prepare(broadcast_set_job_finished, broadcast_jobs,
            [id],
            <<"UPDATE broadcast_jobs "
              "SET execution_state = 'finished', stopped_at = CURRENT_TIMESTAMP "
              "WHERE id = ? AND execution_state = 'running'">>),

    prepare(broadcast_set_job_aborted_error, broadcast_jobs,
            [abortion_reason, id],
            <<"UPDATE broadcast_jobs "
              "SET execution_state = 'abort_error', abortion_reason = ?, "
              "stopped_at = CURRENT_TIMESTAMP "
              "WHERE id = ? AND execution_state = 'running'">>),

    prepare(broadcast_set_job_aborted_admin, broadcast_jobs,
            [id],
            <<"UPDATE broadcast_jobs "
              "SET execution_state = 'abort_admin', stopped_at = CURRENT_TIMESTAMP "
              "WHERE id = ? AND execution_state = 'running'">>),

    prepare(broadcast_get_jobs_by_domain, broadcast_jobs,
            [server, limit, offset],
            <<"SELECT j.id, j.name, j.server, j.from_jid, j.subject, j.message, j.rate, "
              "j.recipient_group, j.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at "
              "FROM broadcast_jobs j "
              "LEFT JOIN broadcast_worker_state ws ON j.id = ws.broadcast_id "
              "WHERE j.server = ? "
              "ORDER BY j.id DESC LIMIT ? OFFSET ?">>),

    prepare(broadcast_delete_job, broadcast_jobs,
            [id],
            <<"DELETE FROM broadcast_jobs WHERE id = ?">>),

    prepare(broadcast_get_inactive_job_ids_by_domain, broadcast_jobs,
            [server],
            <<"SELECT id FROM broadcast_jobs "
              "WHERE server = ? AND execution_state != 'running'">>),

    prepare(broadcast_delete_inactive_jobs_by_domain, broadcast_jobs,
            [server],
            <<"DELETE FROM broadcast_jobs "
              "WHERE server = ? AND execution_state != 'running'">>),
    ok.

upsert_worker_state_query(HostType) ->
    case mongoose_rdbms:db_engine(HostType) of
        pgsql ->
            <<"INSERT INTO broadcast_worker_state (broadcast_id, cursor_user, recipients_processed) "
              "VALUES (?, ?, ?) "
              "ON CONFLICT (broadcast_id) DO UPDATE "
              "SET cursor_user = EXCLUDED.cursor_user, "
              "recipients_processed = EXCLUDED.recipients_processed">>;
        mysql ->
            <<"INSERT INTO broadcast_worker_state (broadcast_id, cursor_user, recipients_processed) "
              "VALUES (?, ?, ?) "
              "ON DUPLICATE KEY UPDATE cursor_user = VALUES(cursor_user), "
              "recipients_processed = VALUES(recipients_processed)">>
    end.

row_to_job(HostType, {JobId, Name, Server, FromJid, Subject, Message, Rate,
                      RecipientGroup, OwnerNode, RecipientCount, RecipientsProcessed,
                      ExecutionState, AbortionReason, CreatedAt, StartedAt, StoppedAt}) ->
    #broadcast_job{id = JobId,
                   name = Name,
                   host_type = HostType,
                   domain = Server,
                   sender = jid:from_binary(FromJid),
                   subject = Subject,
                   body = Message,
                   message_rate = Rate,
                   recipient_group = decode_recipient_group(RecipientGroup),
                   owner_node = binary_to_atom(OwnerNode, utf8),
                   recipient_count = RecipientCount,
                   recipients_processed = RecipientsProcessed,
                   execution_state = decode_execution_state(ExecutionState),
                   abortion_reason = maybe_null(AbortionReason),
                   created_at = postprocess_timestamp(CreatedAt),
                   started_at = postprocess_timestamp(StartedAt),
                   stopped_at = postprocess_timestamp(StoppedAt)}.

encode_recipient_group(all_users_in_domain) -> <<"all_users_in_domain">>.

decode_execution_state(<<"running">>) -> running;
decode_execution_state(<<"finished">>) -> finished;
decode_execution_state(<<"abort_error">>) -> abort_error;
decode_execution_state(<<"abort_admin">>) -> abort_admin.

decode_recipient_group(<<"all_users_in_domain">>) -> all_users_in_domain.

postprocess_timestamp({Date, {Hour, Min, Sec}}) ->
    {Date, {Hour, Min, round(Sec)}};
postprocess_timestamp(null) ->
    undefined.

maybe_null(null) -> undefined;
maybe_null(Value) -> Value.

null_if_undefined(undefined) -> null;
null_if_undefined(Value) -> Value.
