%% @doc Provide an interface for frontends (like graphql or ctl) to manage message broadcasts.

-module(mod_broadcast_api).
-author('piotr.nosek@erlang-solutions.com').

-export([get_broadcasts/3,
         get_broadcast/2,
         start_broadcast/1,
         abort_broadcast/2,
         delete_inactive_broadcasts_by_ids/2,
         delete_inactive_broadcasts_by_domain/1]).

%% For now exported only for tests
-export([broadcast_job_to_map/1]).

-ignore_xref([broadcast_job_to_map/1]).

-include("jlib.hrl").
-include("mod_broadcast.hrl").
-include("mongoose.hrl").

-type broadcast_job_map() :: #{
    id := broadcast_job_id(),
    name := binary(),
    domain := jid:lserver(),
    subject := binary(),
    body := binary(),
    sender := jid:jid(),
    message_rate := pos_integer(),
    owner_node := node(),
    create_timestamp := calendar:datetime(),
    start_timestamp := calendar:datetime() | undefined,
    stop_timestamp := calendar:datetime() | undefined,
    execution_state := execution_state(),
    abortion_reason := binary() | undefined,
    recipient_group := recipient_group(),
    recipient_count := non_neg_integer(),
    recipients_processed := recipients_processed()
}.

-export_type([broadcast_job_map/0]).

%%====================================================================
%% API
%%====================================================================

-spec get_broadcasts(jid:lserver(), Limit :: pos_integer(), Index :: non_neg_integer()) ->
    {ok, [broadcast_job()]} | {domain_not_found, binary()}.
get_broadcasts(Domain, Limit, Index) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            mod_broadcast_backend:get_jobs_by_domain(HostType, Domain, Limit, Index);
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec get_broadcast(jid:lserver(), broadcast_job_id()) ->
    {ok, broadcast_job()} | {domain_not_found | broadcast_not_found, binary()}.
get_broadcast(Domain, Id) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            get_broadcast(HostType, Domain, Id);
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec start_broadcast(job_spec()) ->
    {ok, broadcast_job_id()} | {atom(), binary()}.
start_broadcast(#{domain := Domain} = JobSpec) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            start_broadcast(HostType, JobSpec);
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec abort_broadcast(jid:lserver(), broadcast_job_id()) ->
    {ok, broadcast_job_id()} | {atom(), binary()}.
abort_broadcast(Domain, Id) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            abort_broadcast(HostType, Domain, Id);
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec delete_inactive_broadcasts_by_ids(jid:lserver(), [broadcast_job_id()]) ->
    {ok, [broadcast_job_id()]} | {atom(), binary()}.
delete_inactive_broadcasts_by_ids(Domain, Ids) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            {ok, delete_inactive_broadcasts_by_ids(HostType, Domain, Ids)};
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec delete_inactive_broadcasts_by_domain(jid:lserver()) ->
    {ok, [broadcast_job_id()]} | {atom(), binary()}.
delete_inactive_broadcasts_by_domain(Domain) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            mod_broadcast_backend:delete_inactive_jobs_by_domain(HostType, Domain);
        {error, not_found} ->
            error_result(domain_not_found)
    end.

%%====================================================================
%% Delegated API functions, after host type check
%%====================================================================

-spec get_broadcast(mongooseim:host_type(), jid:lserver(), broadcast_job_id()) ->
    {ok, broadcast_job()} | {broadcast_not_found, binary()}.
get_broadcast(HostType, Domain, Id) ->
    case mod_broadcast_backend:get_job(HostType, Id) of
        {ok, #broadcast_job{domain = Domain} = Job} ->
            {ok, Job};
        {ok, #broadcast_job{}} ->
            %% Job exists but belongs to different domain
            error_result(broadcast_not_found);
        {error, not_found} ->
            error_result(broadcast_not_found)
    end.

-spec start_broadcast(mongooseim:host_type(), job_spec()) ->
    {ok, broadcast_job_id()} | {atom(), binary()}.
start_broadcast(HostType, JobSpec) ->
    case broadcast_manager:start_job(HostType, JobSpec) of
        {ok, JobId} ->
            {ok, JobId};
        {error, running_job_limit_exceeded} ->
            error_result(running_job_limit_exceeded);
        {error, sender_not_found} ->
            error_result(sender_not_found);
        {error, no_recipients} ->
            error_result(no_recipients);
        {error, {bad_parameter, ParameterName}} ->
            error_result({bad_parameter, ParameterName})
    end.

-spec abort_broadcast(mongooseim:host_type(), jid:lserver(), broadcast_job_id()) ->
    {ok, broadcast_job_id()} | {atom(), binary()}.
abort_broadcast(HostType, Domain, Id) ->
    %% First verify the job exists and belongs to this domain
    case mod_broadcast_backend:get_job(HostType, Id) of
        {ok, #broadcast_job{domain = Domain,
                            owner_node = OwnerNode,
                            execution_state = running}} ->
            try broadcast_manager:stop_job(OwnerNode, HostType, Id) of
                ok ->
                    {ok, Id};
                {error, not_live} ->
                    error_result(broadcast_not_found)
            catch
                exit:{{nodedown, _}, _} ->
                    error_result(cannot_reach_job_owner_node);
                exit:{noproc, _} ->
                    %% Technically the node is reachable but manager is not running
                    %% Not relevant for the caller, as ultimately the job cannot be aborted anyway
                    error_result(cannot_reach_job_owner_node)
            end;
        {ok, #broadcast_job{domain = Domain}} ->
            error_result(not_running);
        {ok, #broadcast_job{}} ->
            %% Job exists but belongs to different domain
            error_result(broadcast_not_found);
        {error, not_found} ->
            error_result(broadcast_not_found)
    end.

-spec delete_inactive_broadcasts_by_ids(mongooseim:host_type(), jid:lserver(), [broadcast_job_id()]) ->
    [broadcast_job_id()].
delete_inactive_broadcasts_by_ids(HostType, Domain, Ids) ->
    lists:filtermap(
        fun(Id) ->
            case delete_inactive_job(HostType, Domain, Id) of
                ok ->
                    {true, Id};
                {error, Reason} ->
                    ?LOG_INFO(#{what => cannot_delete_broadcast_job,
                                domain => Domain,
                                id => Id,
                                reason => Reason}),
                    false
            end
        end, Ids).

%%====================================================================
%% Internal functions
%%====================================================================

-spec delete_inactive_job(mongooseim:host_type(), jid:lserver(), broadcast_job_id()) ->
    ok | {error, term()}.
delete_inactive_job(HostType, Domain, Id) ->
    case mod_broadcast_backend:get_job(HostType, Id) of
        {ok, #broadcast_job{domain = Domain, execution_state = State}}
          when State =/= running ->
            mod_broadcast_backend:delete_job(HostType, Id);
        {ok, #broadcast_job{domain = Domain}} ->
            {error, still_running};
        {error, not_found} ->
            {error, not_found}
    end.

-spec error_result(atom() | {bad_parameter, atom()}) -> {atom(), binary()}.
error_result(domain_not_found) ->
    {domain_not_found, <<"Domain not found">>};
error_result(broadcast_not_found) ->
    {broadcast_not_found, <<"Broadcast job not found">>};
error_result(cannot_reach_job_owner_node) ->
    {cannot_reach_job_owner_node, <<"Cannot reach the node managing the broadcast job">>};
error_result(sender_not_found) ->
    {sender_not_found, <<"Sender account does not exist">>};
error_result(no_recipients) ->
    {no_recipients, <<"Recipient group is empty">>};
error_result(running_job_limit_exceeded) ->
    {running_job_limit_exceeded, <<"Cannot start new broadcast job: running job limit exceeded">>};
error_result(not_running) ->
    {not_running, <<"The broadcast job is not currently running">>};
error_result({bad_parameter, ParameterName}) ->
    Message = iolist_to_binary(["Invalid parameter: ", atom_to_binary(ParameterName, utf8)]),
    {bad_parameter, Message}.

%%====================================================================
%% Broadcast job record to map conversion
%%====================================================================

-spec broadcast_job_to_map(broadcast_job()) -> broadcast_job_map().
broadcast_job_to_map(#broadcast_job{
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
    #{id => Id,
      name => Name,
      domain => Domain,
      subject => MessageSubject,
      body => MessageBody,
      sender => SenderJid,
      message_rate => MessageRate,
      owner_node => OwnerNode,
      create_timestamp => CreateTimestamp,
      start_timestamp => StartTimestamp,
      stop_timestamp => StopTimestamp,
      execution_state => ExecutionState,
      abortion_reason => AbortionReason,
      recipient_group => RecipientGroup,
      recipient_count => RecipientCount,
      recipients_processed => RecipientsProcessed}.
