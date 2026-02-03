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

-include("jlib.hrl").
-include("mod_broadcast.hrl").
-include("mongoose.hrl").

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
            case mod_broadcast_backend:get_job(HostType, Id) of
                {ok, #broadcast_job{domain = Domain} = Job} ->
                    {ok, Job};
                {ok, #broadcast_job{}} ->
                    %% Job exists but belongs to different domain
                    error_result(broadcast_not_found);
                {error, not_found} ->
                    error_result(broadcast_not_found)
            end;
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec start_broadcast(job_spec()) ->
    {ok, broadcast_job_id()} | {atom(), binary()}.
start_broadcast(#{domain := Domain} = JobSpec) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            case broadcast_manager:start_job(HostType, JobSpec) of
                {ok, JobId} ->
                    {ok, JobId};
                {error, already_running} ->
                    error_result(already_running);
                {error, sender_not_found} ->
                    error_result(sender_not_found);
                {error, {bad_parameter, ParameterName}} ->
                    error_result({bad_parameter, ParameterName});
                {error, Reason} ->
                    error_result({internal_error, Reason})
            end;
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec abort_broadcast(jid:lserver(), broadcast_job_id()) ->
    {ok, broadcast_job_id()} | {atom(), binary()}.
abort_broadcast(Domain, Id) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            %% First verify the job exists and belongs to this domain
            case mod_broadcast_backend:get_job(HostType, Id) of
                {ok, #broadcast_job{domain = Domain, execution_state = running}} ->
                    case broadcast_manager:stop_job(HostType, Id) of
                        ok ->
                            {ok, Id};
                        {error, not_found} ->
                            error_result(broadcast_not_found)
                    end;
                {ok, #broadcast_job{domain = Domain}} ->
                    error_result(not_running);
                {ok, #broadcast_job{}} ->
                    %% Job exists but belongs to different domain
                    error_result(broadcast_not_found);
                {error, not_found} ->
                    error_result(broadcast_not_found)
            end;
        {error, not_found} ->
            error_result(domain_not_found)
    end.

-spec delete_inactive_broadcasts_by_ids(jid:lserver(), [broadcast_job_id()]) ->
    {ok, [broadcast_job_id()]} | {atom(), binary()}.
delete_inactive_broadcasts_by_ids(Domain, Ids) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            DeletedIds = lists:filtermap(
                fun(Id) ->
                    case try_delete_inactive_job(HostType, Domain, Id) of
                        ok ->
                            {true, Id};
                        {error, Reason} ->
                            ?LOG_INFO(#{what => cannot_delete_broadcast_job,
                                        domain => Domain,
                                        id => Id,
                                        reason => Reason}),
                            false
                    end
                end, Ids),
            {ok, DeletedIds};
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
%% Internal functions
%%====================================================================

-spec try_delete_inactive_job(mongooseim:host_type(), jid:lserver(), broadcast_job_id()) ->
    ok | {error, term()}.
try_delete_inactive_job(HostType, Domain, Id) ->
    case mod_broadcast_backend:get_job(HostType, Id) of
        {ok, #broadcast_job{domain = Domain, execution_state = State}}
          when State =/= running ->
            mod_broadcast_backend:delete_job(HostType, Id);
        {ok, #broadcast_job{domain = Domain}} ->
            {error, still_running};
        {error, not_found} ->
            {error, not_found}
    end.

-spec error_result(atom()) -> {atom(), binary()}.
error_result(domain_not_found) ->
    {domain_not_found, <<"Domain not found">>};
error_result(broadcast_not_found) ->
    {broadcast_not_found, <<"Broadcast job not found">>};
error_result(sender_not_found) ->
    {sender_not_found, <<"Sender account does not exist">>};
error_result(already_running) ->
    {already_running, <<"A broadcast job is already running for this domain">>};
error_result(not_running) ->
    {not_running, <<"The broadcast job is not currently running">>};
error_result({bad_parameter, ParameterName}) ->
    Message = iolist_to_binary(["Invalid parameter: ", atom_to_binary(ParameterName, utf8)]),
    {bad_parameter, Message};
error_result({internal_error, Reason}) ->
    {internal_error, format_error(Reason)}.

-spec format_error(term()) -> binary().
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%====================================================================
%% Broadcast job properties conversion, for now used only in tests
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
