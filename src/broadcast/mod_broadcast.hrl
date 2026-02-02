%%%-------------------------------------------------------------------
%%% @doc Common types and records for mod_broadcast.
%%%-------------------------------------------------------------------

-type broadcast_job_id() :: non_neg_integer().
-type execution_state() :: running | finished | abort_error | abort_admin.
-type recipient_group() :: all_users_in_domain.
-type recipients_processed() :: non_neg_integer().

-record(broadcast_job, {
    id :: broadcast_job_id(),
    name :: binary(),
    host_type :: mongooseim:host_type(),
    domain :: jid:lserver(),
    sender :: jid:jid(),
    subject :: binary(),
    body :: binary(),
    message_rate :: pos_integer(),
    recipient_group :: recipient_group(),
    owner_node :: node(),
    recipient_count :: non_neg_integer(),
    recipients_processed :: recipients_processed(),
    execution_state :: execution_state(),
    abortion_reason :: binary() | undefined,
    created_at :: calendar:datetime(),
    started_at :: calendar:datetime() | undefined,
    stopped_at :: calendar:datetime() | undefined
}).

-type broadcast_job() :: #broadcast_job{}.

-record(broadcast_worker_state, {
    cursor :: binary() | undefined,
    recipients_processed :: recipients_processed()
}).

-type broadcast_worker_state() :: #broadcast_worker_state{}.

-type job_spec() :: #{
    name := binary(),
    domain := jid:lserver(),
    sender := jid:jid(),
    subject := binary(),
    body := binary(),
    message_rate := pos_integer(),
    recipient_group := recipient_group()
}.
