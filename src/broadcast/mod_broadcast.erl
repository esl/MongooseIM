%%%-------------------------------------------------------------------
%%% @doc Message Broadcast feature
%%%-------------------------------------------------------------------

-module(mod_broadcast).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
-behaviour(mongoose_instrument_probe).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-export([start/2,
         stop/1,
         hooks/1,
         supported_features/0,
         config_spec/0,
         instrumentation/1,
         remove_domain/3]).

%% mongoose_instrument_probe callback
-export([probe/2]).

-include("mongoose_config_spec.hrl").
-include("mod_broadcast.hrl").
-include("mongoose.hrl").

-export_type([broadcast_job_id/0,
              broadcast_job/0,
              broadcast_worker_state/0,
              execution_state/0,
              recipient_group/0,
              job_spec/0]).

-spec start(HostType :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    %% This will be refined/relaxed in future iterations, when more backends are supported
    %% or when more recipient sources are added.
    ensure_rdbms_auth_enabled(HostType),
    mod_broadcast_backend:init(HostType, Opts),
    start_supervisor(HostType),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    stop_supervisor(HostType),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50}].

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_domain_api:remove_domain_acc(),
      Params :: #{domain := jid:lserver()},
      Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    Nodes = mongoose_cluster:all_cluster_nodes(),
    lists:foreach(fun(Node) ->
                        broadcast_manager:abort_running_jobs_for_domain(Node, HostType, Domain)
                  end, Nodes),
    mod_broadcast_backend:delete_inactive_jobs_by_domain(HostType, Domain),
    {ok, Acc}.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"backend">> => #option{type = atom,
                                           validate = {enum, [rdbms]}}},
        defaults = #{<<"backend">> => rdbms}
    }.

ensure_rdbms_auth_enabled(HostType) ->
    case lists:member(ejabberd_auth_rdbms, ejabberd_auth:auth_modules_for_host_type(HostType)) of
        true -> ok;
        false ->
            error(#{what => mod_broadcast_requires_rdbms_auth,
                    text => <<"mod_broadcast requires ejabberd_auth_rdbms to be enabled">>,
                    host_type => HostType})
    end.

start_supervisor(HostType) ->
    SupName = gen_mod:get_module_proc(HostType, broadcast_sup),
    ChildSpec = #{id => SupName,
                  start => {broadcast_sup, start_link, [HostType]},
                  restart => permanent,
                  shutdown => infinity,
                  type => supervisor,
                  modules => [broadcast_sup]},
    ejabberd_sup:start_child(ChildSpec).

stop_supervisor(HostType) ->
    SupName = gen_mod:get_module_proc(HostType, broadcast_sup),
    ejabberd_sup:stop_child(SupName).

%%====================================================================
%% Instrumentation
%%====================================================================

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [
     %% Running jobs gauge (polled via probe)
     {mod_broadcast_live_jobs, #{host_type => HostType},
      #{probe => #{module => ?MODULE}, metrics => #{count => gauge}}},
     %% Job lifecycle counters
     {mod_broadcast_jobs_started, #{host_type => HostType},
      #{metrics => #{count => counter}}},
     {mod_broadcast_jobs_finished, #{host_type => HostType},
      #{metrics => #{count => counter}}},
     {mod_broadcast_jobs_aborted_admin, #{host_type => HostType},
      #{metrics => #{count => counter}}},
     {mod_broadcast_jobs_aborted_error, #{host_type => HostType},
      #{metrics => #{count => counter}}},
     %% Recipient processing spirals
     {mod_broadcast_recipients_processed, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_recipients_success, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_recipients_skipped, #{host_type => HostType},
      #{metrics => #{count => spiral}}}
    ].

-spec probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
          mongoose_instrument:measurements().
probe(mod_broadcast_live_jobs, #{host_type := HostType}) ->
    try broadcast_manager:get_live_job_count(HostType) of
        Count ->
            ?LOG_DEBUG(#{what => broadcast_live_jobs_probe, host_type => HostType, count => Count}),
            #{count => Count}
    catch
        exit:{noproc, _} ->
            #{count => 0};
        exit:{timeout, _} ->
            #{count => 0}
    end.
