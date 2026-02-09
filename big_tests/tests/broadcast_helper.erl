%%%-------------------------------------------------------------------
%%% @doc
%%% Helper functions for broadcast-related tests.
%%% @end
%%%-------------------------------------------------------------------
-module(broadcast_helper).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([slow_job_spec/3,
         fast_job_spec/3,
         start_broadcast/1,
         get_broadcast/2,
         get_broadcasts/3,
         broadcast_job_to_map/1,
         abort_broadcast/2,
         delete_inactive_broadcasts_by_ids/2,
         delete_inactive_broadcasts_by_domain/1,
         does_worker_for_job_exist/2,
         wait_until_worker_started/2,
         clean_broadcast_jobs/0,
         % User management helpers
         create_many_users/1,
         delete_many_users/1
        ]).

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [domain/0, secondary_domain/0]).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% API
%%====================================================================

-spec slow_job_spec(Domain :: jid:lserver(),
                    SenderJid :: binary(),
                    TestName :: atom() | binary()) -> mod_broadcast:job_spec().
slow_job_spec(Domain, SenderJid, AtomName) when is_atom(AtomName) ->
    slow_job_spec(Domain, SenderJid, atom_to_binary(AtomName, utf8));
slow_job_spec(Domain, SenderJid, Name) ->
    #{
        name => Name,
        domain => Domain,
        sender => jid:from_binary(SenderJid),
        subject => <<"Slow Test Subject">>,
        body => <<"Slow Test Body">>,
        message_rate => 1,  %% Low rate to make it slow
        recipient_group => all_users_in_domain
    }.

-spec fast_job_spec(Domain :: jid:lserver(),
                    SenderJid :: binary(),
                    AtomName :: atom()) -> mod_broadcast:job_spec().
fast_job_spec(Domain, SenderJid, AtomName) ->
    #{
        name => atom_to_binary(AtomName, utf8),
        domain => Domain,
        sender => jid:from_binary(SenderJid),
        subject => <<"Fast Test Subject">>,
        body => <<"Fast Test Body">>,
        message_rate => 1000,  %% High rate to finish quickly
        recipient_group => all_users_in_domain
    }.

-spec start_broadcast(JobSpec :: mod_broadcast:job_spec()) ->
    {ok, JobId :: mod_broadcast:broadcast_job_id()} | {atom(), binary()}.
start_broadcast(JobSpec) ->
    call_api(start_broadcast, [JobSpec]).

-spec get_broadcast(Domain :: jid:lserver(),
                    JobId :: mod_broadcast:broadcast_job_id()) ->
    {ok, mod_broadcast_api:broadcast_job()} | {atom(), binary()}.
get_broadcast(Domain, JobId) ->
    call_api(get_broadcast, [Domain, JobId]).

-spec get_broadcasts(Domain :: jid:lserver(),
                     Limit :: pos_integer(),
                     Offset :: non_neg_integer()) ->
    {ok, [mod_broadcast_api:broadcast_job()]} | {atom(), binary()}.
get_broadcasts(Domain, Limit, Offset) ->
    call_api(get_broadcasts, [Domain, Limit, Offset]).

-spec broadcast_job_to_map(
    BroadcastJobRecord :: mod_broadcast:broadcast_job() |
                          {ok, mod_broadcast:broadcast_job()}) ->
    mod_broadcast_api:broadcast_job_map().
broadcast_job_to_map({ok, JobRecord}) ->
    broadcast_job_to_map(JobRecord);
broadcast_job_to_map(JobRecord) ->
    call_api(broadcast_job_to_map, [JobRecord]).

-spec abort_broadcast(Domain :: jid:lserver(),
                      JobId :: mod_broadcast:broadcast_job_id()) ->
    {ok, mod_broadcast:broadcast_job_id()} | {atom(), binary()}.
abort_broadcast(Domain, JobId) ->
    call_api(abort_broadcast, [Domain, JobId]).

-spec delete_inactive_broadcasts_by_ids(Domain :: jid:lserver(),
                                        JobIds :: [mod_broadcast:broadcast_job_id()]) ->
    {ok, [mod_broadcast:broadcast_job_id()]} | {atom(), binary()}.
delete_inactive_broadcasts_by_ids(Domain, JobIds) ->
    call_api(delete_inactive_broadcasts_by_ids, [Domain, JobIds]).

-spec delete_inactive_broadcasts_by_domain(Domain :: jid:lserver()) ->
    {ok, [mod_broadcast:broadcast_job_id()]} | {atom(), binary()}.
delete_inactive_broadcasts_by_domain(Domain) ->
    call_api(delete_inactive_broadcasts_by_domain, [Domain]).

-spec does_worker_for_job_exist(HostType :: mongooseim:host_type(),
                                JobId :: mod_broadcast:broadcast_job_id()) ->
    boolean().
does_worker_for_job_exist(HostType, JobId) ->
    rpc(mim(), broadcast_manager, does_worker_for_job_exist, [HostType, JobId]).

-spec wait_until_worker_started(HostType :: mongooseim:host_type(),
                                JobId :: mod_broadcast:broadcast_job_id()) ->
    any().
wait_until_worker_started(HostType, JobId) ->
    wait_helper:wait_until(fun() ->
        does_worker_for_job_exist(HostType, JobId)
    end, true).

-spec clean_broadcast_jobs() -> ok.
clean_broadcast_jobs() ->
    Node = maps:get(node, mim()),
    ok = rpc(mim(), broadcast_manager, abort_running_jobs_for_domain,
             [Node, domain(), domain()]),
    ok = rpc(mim(), broadcast_manager, abort_running_jobs_for_domain,
             [Node, secondary_domain(), secondary_domain()]),
    {ok, _} = delete_inactive_broadcasts_by_domain(domain()),
    {ok, _} = delete_inactive_broadcasts_by_domain(secondary_domain()),
    ok.

%%====================================================================
%% User management helpers
%%====================================================================

-spec create_many_users(Count :: pos_integer()) -> ok.
create_many_users(Count) ->
    lists:foreach(fun(N) ->
        Username = <<"testuser", (integer_to_binary(N))/binary>>,
        Password = <<"password123">>,
        JIDPrimary = jid:make_noprep(Username, domain(), <<>>),
        rpc(mim(), ejabberd_auth, try_register, [JIDPrimary, Password]),
        JIDSecondary = jid:make_noprep(Username, secondary_domain(), <<>>),
        rpc(mim(), ejabberd_auth, try_register, [JIDSecondary, Password])
    end, lists:seq(1, Count)).

-spec delete_many_users(Count :: pos_integer()) -> ok.
delete_many_users(Count) ->
    lists:foreach(fun(N) ->
        Username = <<"testuser", (integer_to_binary(N))/binary>>,
        JIDPrimary = jid:make_noprep(Username, domain(), <<>>),
        rpc(mim(), ejabberd_auth, remove_user, [JIDPrimary]),
        JIDSecondary = jid:make_noprep(Username, secondary_domain(), <<>>),
        rpc(mim(), ejabberd_auth, remove_user, [JIDSecondary])
    end, lists:seq(1, Count)).

%%====================================================================
%% Internal functions
%%====================================================================

call_api(Function, Args) ->
    rpc(mim(), mod_broadcast_api, Function, Args).