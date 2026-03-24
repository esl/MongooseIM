%%%-------------------------------------------------------------------
%%% @doc RDBMS backend for mod_broadcast.
%%%-------------------------------------------------------------------

-module(mod_broadcast_rdbms).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(mod_broadcast_backend).

-export([init/2,
         create_job/3,
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
         delete_inactive_jobs_by_domain/2,
         renew_ownership/2,
         take_expired_jobs/2,
         remove_ownership/2]).

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

-spec create_job(mongooseim:host_type(), job_spec(), ExpiresAt :: non_neg_integer()) ->
    {ok, JobId :: broadcast_job_id()} | {error, running_job_limit_exceeded}.
create_job(HostType, JobSpec, ExpiresAt) ->
    #{name := Name, domain := Domain, sender := Sender,
      subject := Subject, body := Body, message_rate := Rate,
      recipient_group := RecipientGroup, recipient_count := RecipientCount} = JobSpec,
    OwnerNode = atom_to_binary(node(), utf8),
    SenderBin = jid:to_binary(Sender),
    RecipientGroupBin = encode_recipient_group(RecipientGroup),
    T = fun() ->
        %% Limit is hardcoded to 1 for now
        %% TODO: Make it configurable
        case execute_successfully(HostType, broadcast_count_running_jobs, [Domain]) of
            {selected, [{RunningJobsCount}]} when RunningJobsCount >= 1 ->
                {error, running_job_limit_exceeded};
            _Else ->
                {ok, JobId} = create_job_query(HostType, mongoose_rdbms:db_engine(HostType),
                                 [Name, Domain, HostType, SenderBin, Subject, Body, Rate,
                                  RecipientGroupBin, RecipientCount]),
                {updated, 1} = execute_successfully(HostType, broadcast_create_ownership,
                                                    [JobId, OwnerNode, ExpiresAt]),
                {ok, JobId}
        end
    end,
    case mongoose_rdbms:sql_transaction(HostType, T) of
        {atomic, {ok, JobId}} ->
            {ok, JobId};
        {atomic, {error, running_job_limit_exceeded} = E}->
            E
    end.

-spec get_job(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_job()} | {error, not_found}.
get_job(HostType, JobId) ->
    case execute_successfully(HostType, broadcast_get_job, [JobId]) of
        {selected, [Row]} ->
            {ok, row_to_job(Row)};
        {selected, []} ->
            {error, not_found}
    end.

-spec get_running_jobs(mongooseim:host_type()) -> {ok, [broadcast_job()]}.
get_running_jobs(HostType) ->
    OwnerNode = atom_to_binary(node(), utf8),
    {selected, Rows} = execute_successfully(HostType, broadcast_get_running_jobs, [OwnerNode, HostType]),
    Jobs = lists:map(fun row_to_job/1, Rows),
    {ok, Jobs}.

-spec get_jobs_by_domain(mongooseim:host_type(), jid:lserver(),
                         Limit :: pos_integer(), Offset :: non_neg_integer()) ->
    {ok, [broadcast_job()]}.
get_jobs_by_domain(HostType, Domain, Limit, Offset) ->
    {selected, Rows} = execute_successfully(HostType, broadcast_get_jobs_by_domain,
                                            [Domain, Limit, Offset]),
    Jobs = lists:map(fun row_to_job/1, Rows),
    {ok, Jobs}.

-spec get_worker_state(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_worker_state()} | {error, not_found}.
get_worker_state(HostType, JobId) ->
    case execute_successfully(HostType, broadcast_get_worker_state, [JobId]) of
        {selected, [{Cursor, RecipientsProcessed, Finished}]} ->
            {ok, #broadcast_worker_state{cursor = maybe_null(Cursor),
                                         recipients_processed = RecipientsProcessed,
                                         finished = mongoose_rdbms:to_bool(Finished)}};
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
    Finished = WorkerState#broadcast_worker_state.finished,
    {updated, _} = execute_successfully(HostType, broadcast_upsert_worker_state,
                                        [JobId, Cursor, RecipientsProcessed, Finished]),
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

-spec renew_ownership(mongooseim:host_type(), ExpiresAt :: non_neg_integer()) ->
    {ok, [broadcast_job_id()]}.
renew_ownership(HostType, ExpiresAt) ->
    OwnerNode = atom_to_binary(node(), utf8),
    case mongoose_rdbms:db_engine(HostType) of
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            renew_ownership_pgsql(HostType, ExpiresAt, OwnerNode);
        mysql ->
            renew_ownership_mysql(HostType, ExpiresAt, OwnerNode)
    end.

-spec take_expired_jobs(mongooseim:host_type(), NewExpiresAt :: non_neg_integer()) ->
    {ok, [broadcast_job()]}.
take_expired_jobs(HostType, NewExpiresAt) ->
    OwnerNode = atom_to_binary(node(), utf8),
    case mongoose_rdbms:db_engine(HostType) of
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            take_expired_jobs_pgsql(HostType, NewExpiresAt, OwnerNode);
        mysql ->
            take_expired_jobs_mysql(HostType, NewExpiresAt, OwnerNode)
    end.

-spec remove_ownership(mongooseim:host_type(), JobId :: broadcast_job_id()) -> ok.
remove_ownership(HostType, JobId) ->
    {updated, _} = execute_successfully(HostType, broadcast_remove_ownership, [JobId]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

prepare_queries(HostType) ->
    prepare_create_job_queries(HostType),
    prepare_ownership_queries(HostType),

    prepare(broadcast_count_running_jobs, broadcast_jobs,
            [server],
            <<"SELECT COUNT(*) FROM broadcast_jobs "
              "WHERE server = ? AND execution_state = 'running'">>),

    prepare(broadcast_get_job, broadcast_jobs,
            [id],
            <<"SELECT j.id, j.name, j.server, j.host_type, j.from_jid, j.subject, j.body, j.rate, "
              "j.recipient_group, o.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at "
              "FROM broadcast_jobs j "
              "LEFT JOIN broadcast_jobs_ownership o ON j.id = o.broadcast_id "
              "LEFT JOIN broadcast_worker_state ws ON j.id = ws.broadcast_id "
              "WHERE j.id = ?">>),

    prepare(broadcast_get_running_jobs, broadcast_jobs,
            [owner_node, host_type],
            <<"SELECT j.id, j.name, j.server, j.host_type, j.from_jid, j.subject, j.body, j.rate, "
              "j.recipient_group, o.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at "
              "FROM broadcast_jobs j "
              "JOIN broadcast_jobs_ownership o ON j.id = o.broadcast_id "
              "LEFT JOIN broadcast_worker_state ws ON j.id = ws.broadcast_id "
              "WHERE o.owner_node = ? AND j.host_type = ? AND j.execution_state = 'running'">>),

    prepare(broadcast_get_worker_state, broadcast_worker_state,
            [broadcast_id],
            <<"SELECT cursor_user, recipients_processed, finished FROM broadcast_worker_state "
              "WHERE broadcast_id = ?">>),

    prepare(broadcast_upsert_worker_state, broadcast_worker_state,
            [broadcast_id, cursor_user, recipients_processed, finished],
            upsert_worker_state_query(HostType)),

    prepare(broadcast_set_job_started, broadcast_jobs,
            [id],
            <<"UPDATE broadcast_jobs SET started_at = CURRENT_TIMESTAMP"
              " WHERE id = ?">>),

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
            <<"SELECT j.id, j.name, j.server, j.host_type, j.from_jid, j.subject, j.body, j.rate, "
              "j.recipient_group, o.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at "
              "FROM broadcast_jobs j "
              "LEFT JOIN broadcast_jobs_ownership o ON j.id = o.broadcast_id "
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

prepare_create_job_queries(HostType) ->
    case mongoose_rdbms:db_engine(HostType) of
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            prepare(broadcast_create_job_returning, broadcast_jobs,
                    [name, server, host_type, from_jid, subject, body, rate,
                     recipient_group, recipient_count],
                    <<"INSERT INTO broadcast_jobs "
                      "(name, server, host_type, from_jid, subject, body, rate, "
                      "recipient_group, recipient_count) "
                      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) "
                      "RETURNING id">>);
        mysql ->
            prepare(broadcast_create_job_mysql, broadcast_jobs,
                    [name, server, host_type, from_jid, subject, body, rate,
                     recipient_group, recipient_count],
                    <<"INSERT INTO broadcast_jobs "
                      "(name, server, host_type, from_jid, subject, body, rate, "
                      "recipient_group, recipient_count) "
                      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)">>),
            prepare(broadcast_get_last_insert_id, broadcast_jobs,
                    [],
                    <<"SELECT LAST_INSERT_ID()">>)
    end.

create_job_query(HostType, Engine, Params) when Engine == pgsql; Engine == cockroachdb ->
    {updated, 1, [{JobId}]} = execute_successfully(
                                HostType, broadcast_create_job_returning, Params),
    {ok, JobId};
create_job_query(HostType, Engine, Params) when Engine == mysql ->
    {updated, 1} = execute_successfully(
                    HostType, broadcast_create_job_mysql, Params),
    {selected, [{JobId}]} = execute_successfully(
                                HostType, broadcast_get_last_insert_id, []),
    {ok, mongoose_rdbms:result_to_integer(JobId)}.

upsert_worker_state_query(HostType) ->
    case mongoose_rdbms:db_engine(HostType) of
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            <<"INSERT INTO broadcast_worker_state (broadcast_id, cursor_user, recipients_processed, finished) "
              "VALUES (?, ?, ?, ?) "
              "ON CONFLICT (broadcast_id) DO UPDATE "
              "SET cursor_user = EXCLUDED.cursor_user, "
              "recipients_processed = EXCLUDED.recipients_processed, "
              "finished = EXCLUDED.finished">>;
        mysql ->
            <<"INSERT INTO broadcast_worker_state (broadcast_id, cursor_user, recipients_processed, finished) "
              "VALUES (?, ?, ?, ?) "
              "ON DUPLICATE KEY UPDATE cursor_user = VALUES(cursor_user), "
              "recipients_processed = VALUES(recipients_processed), "
              "finished = VALUES(finished)">>
    end.

prepare_ownership_queries(HostType) ->
    prepare(broadcast_remove_ownership, broadcast_jobs_ownership,
            [broadcast_id],
            <<"DELETE FROM broadcast_jobs_ownership WHERE broadcast_id = ?">>),
    case mongoose_rdbms:db_engine(HostType) of
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            prepare_ownership_queries_pgsql();
        mysql ->
            prepare_ownership_queries_mysql()
    end.

prepare_ownership_queries_pgsql() ->
    prepare(broadcast_create_ownership, broadcast_jobs_ownership,
            [broadcast_id, owner_node, expires_at],
            <<"INSERT INTO broadcast_jobs_ownership "
              "(broadcast_id, owner_node, updated_at, expires_at) "
              "VALUES (?, ?, now(), to_timestamp(?))">>),

    prepare(broadcast_renew_ownership, broadcast_jobs_ownership,
            [expires_at, owner_node, host_type],
            <<"UPDATE broadcast_jobs_ownership o "
              "SET expires_at = to_timestamp(?), updated_at = now() "
              "FROM broadcast_jobs j "
              "WHERE o.broadcast_id = j.id "
              "AND o.owner_node = ? "
              "AND j.host_type = ? "
              "AND j.execution_state = 'running' "
              "RETURNING o.broadcast_id">>),

    prepare(broadcast_take_expired_jobs, broadcast_jobs_ownership,
            [owner_node, expires_at],
            <<"UPDATE broadcast_jobs_ownership o "
              "SET owner_node = ?, expires_at = to_timestamp(?), updated_at = now() "
              "FROM broadcast_jobs j "
              "LEFT JOIN broadcast_worker_state ws ON j.id = ws.broadcast_id "
              "WHERE o.broadcast_id = j.id "
              "AND o.expires_at < now() "
              "AND j.execution_state = 'running' "
              "RETURNING j.id, j.name, j.server, j.host_type, j.from_jid, j.subject, j.body, j.rate, "
              "j.recipient_group, o.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at">>),
    ok.

prepare_ownership_queries_mysql() ->
    prepare(broadcast_create_ownership, broadcast_jobs_ownership,
            [broadcast_id, owner_node, expires_at],
            <<"INSERT INTO broadcast_jobs_ownership "
              "(broadcast_id, owner_node, updated_at, expires_at) "
              "VALUES (?, ?, CURRENT_TIMESTAMP, FROM_UNIXTIME(?))">>),

    %% Renew ownership: SELECT FOR UPDATE, then UPDATE
    prepare(broadcast_renew_ownership_select, broadcast_jobs_ownership,
            [owner_node, host_type],
            <<"SELECT o.broadcast_id "
              "FROM broadcast_jobs_ownership o "
              "JOIN broadcast_jobs j ON o.broadcast_id = j.id "
              "WHERE o.owner_node = ? AND j.host_type = ? "
              "AND j.execution_state = 'running' "
              "FOR UPDATE">>),

    prepare(broadcast_renew_ownership_update, broadcast_jobs_ownership,
            [expires_at, owner_node, host_type],
            <<"UPDATE broadcast_jobs_ownership "
              "SET expires_at = FROM_UNIXTIME(?), updated_at = CURRENT_TIMESTAMP "
              "WHERE owner_node = ? "
              "AND broadcast_id IN ("
              "SELECT id FROM broadcast_jobs "
              "WHERE host_type = ? AND execution_state = 'running')">>),

    %% Take expired jobs: SELECT FOR UPDATE, then UPDATE
    prepare(broadcast_take_expired_select, broadcast_jobs_ownership,
            [],
            <<"SELECT j.id, j.name, j.server, j.host_type, j.from_jid, j.subject, j.body, j.rate, "
              "j.recipient_group, o.owner_node, j.recipient_count, "
              "COALESCE(ws.recipients_processed, 0), "
              "j.execution_state, j.abortion_reason, j.created_at, j.started_at, j.stopped_at "
              "FROM broadcast_jobs j "
              "JOIN broadcast_jobs_ownership o ON j.id = o.broadcast_id "
              "LEFT JOIN broadcast_worker_state ws ON j.id = ws.broadcast_id "
              "WHERE o.expires_at < CURRENT_TIMESTAMP "
              "AND j.execution_state = 'running' "
              "FOR UPDATE">>),

    prepare(broadcast_take_expired_update, broadcast_jobs_ownership,
            [owner_node, expires_at],
            <<"UPDATE broadcast_jobs_ownership "
              "SET owner_node = ?, expires_at = FROM_UNIXTIME(?), updated_at = CURRENT_TIMESTAMP "
              "WHERE expires_at < CURRENT_TIMESTAMP "
              "AND broadcast_id IN ("
              "SELECT id FROM broadcast_jobs "
              "WHERE execution_state = 'running')">>),
    ok.

%% ===================================================================
%% Ownership operations - DB specific
%% ===================================================================

renew_ownership_pgsql(HostType, ExpiresAt, OwnerNode) ->
    case execute_successfully(HostType, broadcast_renew_ownership,
                              [ExpiresAt, OwnerNode, HostType]) of
        {updated, _, Rows} ->
            {ok, [mongoose_rdbms:result_to_integer(Id) || {Id} <- Rows]};
        {updated, 0} ->
            {ok, []}
    end.

renew_ownership_mysql(HostType, ExpiresAt, OwnerNode) ->
    T = fun() ->
        {selected, IdRows} = execute_successfully(
            HostType, broadcast_renew_ownership_select, [OwnerNode, HostType]),
        case IdRows of
            [] ->
                {ok, []};
            _ ->
                {updated, _} = execute_successfully(
                    HostType, broadcast_renew_ownership_update,
                    [ExpiresAt, OwnerNode, HostType]),
                {ok, [mongoose_rdbms:result_to_integer(Id) || {Id} <- IdRows]}
        end
    end,
    {atomic, Result} = mongoose_rdbms:sql_transaction(HostType, T),
    Result.

take_expired_jobs_pgsql(HostType, NewExpiresAt, OwnerNode) ->
    case execute_successfully(HostType, broadcast_take_expired_jobs,
                              [OwnerNode, NewExpiresAt]) of
        {updated, _, Rows} ->
            {ok, lists:map(fun row_to_job/1, Rows)};
        {updated, 0} ->
            {ok, []}
    end.

take_expired_jobs_mysql(HostType, NewExpiresAt, OwnerNode) ->
    T = fun() ->
        {selected, Rows} = execute_successfully(
            HostType, broadcast_take_expired_select, []),
        case Rows of
            [] ->
                {ok, []};
            _ ->
                {updated, _} = execute_successfully(
                    HostType, broadcast_take_expired_update,
                    [OwnerNode, NewExpiresAt]),
                %% Rows have old owner_node; replace with our node
                Jobs = [Job#broadcast_job{owner_node = node()}
                        || Job <- lists:map(fun row_to_job/1, Rows)],
                {ok, Jobs}
        end
    end,
    {atomic, Result} = mongoose_rdbms:sql_transaction(HostType, T),
    Result.

row_to_job({JobId, Name, Server, HostType, FromJid, Subject, Message, Rate,
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
                   owner_node = decode_owner_node(OwnerNode),
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

decode_owner_node(null) -> undefined;
decode_owner_node(Bin) -> binary_to_atom(Bin, utf8).

postprocess_timestamp({Date, {Hour, Min, Sec}}) ->
    {Date, {Hour, Min, round(Sec)}};
postprocess_timestamp(null) ->
    undefined.

maybe_null(null) -> undefined;
maybe_null(Value) -> Value.

null_if_undefined(undefined) -> null;
null_if_undefined(Value) -> Value.
