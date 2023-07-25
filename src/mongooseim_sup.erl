-module(mongooseim_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/1, stop_child/1]).

-include("mongoose_logger.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hooks = worker_spec(gen_hook),
    Cleaner = worker_spec(mongoose_cleaner),
    Router = worker_spec(ejabberd_router),
    S2S = worker_spec(ejabberd_s2s),
    Local = worker_spec(ejabberd_local),
    MucIQ = worker_spec(mod_muc_iq),
    SMBackendSupervisor = supervisor_spec(ejabberd_sm_backend_sup),
    OutgoingPoolsSupervisor = supervisor_spec(mongoose_wpool_sup),
    Listener = supervisor_spec(mongoose_listener_sup),
    ShaperSup = supervisor_spec(mongoose_shaper_sup),
    DomainSup = supervisor_spec(mongoose_domain_sup),
    ReceiverSupervisor =
        ejabberd_tmp_sup_spec(mongoose_transport_sup, [mongoose_transport_sup, mongoose_transport]),
    C2SSupervisor =
        ejabberd_tmp_sup_spec(mongoose_c2s_sup, [mongoose_c2s_sup, mongoose_c2s]),
    S2SInSupervisor =
        ejabberd_tmp_sup_spec(ejabberd_s2s_in_sup, [ejabberd_s2s_in_sup, ejabberd_s2s_in]),
    S2SOutSupervisor =
        ejabberd_tmp_sup_spec(ejabberd_s2s_out_sup, [ejabberd_s2s_out_sup, ejabberd_s2s_out]),
    ServiceSupervisor =
        ejabberd_tmp_sup_spec(ejabberd_service_sup, [ejabberd_service_sup, ejabberd_service]),
    IQSupervisor =
        ejabberd_tmp_sup_spec(ejabberd_iq_sup, [ejabberd_iq_sup, mongoose_iq_worker]),
    PG =
        {pg,
          {pg, start_link, [mim_scope]},
          permanent, infinity, supervisor, [pg]},
    {ok, {{one_for_one, 10, 1},
          [PG,
           Hooks,
           Cleaner,
           SMBackendSupervisor,
           Router,
           S2S,
           Local,
           ReceiverSupervisor,
           C2SSupervisor,
           S2SInSupervisor,
           S2SOutSupervisor,
           ServiceSupervisor,
           OutgoingPoolsSupervisor,
           IQSupervisor,
           Listener,
           MucIQ,
           ShaperSup,
           DomainSup
          ]}}.

start_child(ChildSpec) ->
    case supervisor:start_child(mongooseim_sup, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            Stacktrace = element(2, erlang:process_info(self(), current_stacktrace)),
            ?LOG_ERROR(#{what => start_child_failed, spec => ChildSpec,
                         reason => Other, stacktrace => Stacktrace}),
            erlang:error({start_child_failed, Other, ChildSpec})
    end.

stop_child(Proc) ->
    supervisor:terminate_child(mongooseim_sup, Proc),
    supervisor:delete_child(mongooseim_sup, Proc),
    ok.

ejabberd_tmp_sup_spec(Name, Args) ->
    {Name,
     {ejabberd_tmp_sup, start_link, Args},
     permanent, infinity, supervisor, [ejabberd_tmp_sup]}.

supervisor_spec(Mod) ->
    {Mod, {Mod, start_link, []}, permanent, infinity, supervisor, [Mod]}.

worker_spec(Mod) ->
    {Mod, {Mod, start_link, []}, permanent, timer:seconds(5), worker, [Mod]}.
