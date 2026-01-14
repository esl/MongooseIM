-module(mod_broadcast).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0, instrumentation/1]).

%% Internal API (used by GraphQL/API layer)
-export([start_broadcast_worker/2, stop_broadcast_worker/1]).

-include("mongoose_logger.hrl").
-include("mongoose_config_spec.hrl").

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    ok = mod_broadcast_rdbms:init(HostType, Opts),
    ok = start_broadcast_worker(HostType, Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ok = stop_broadcast_worker(HostType),
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(_HostType) ->
    [].

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_broadcast_started, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_aborted, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_completed, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_failed, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_recipients, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_messages_sent, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_broadcast_messages_failed, #{host_type => HostType},
      #{metrics => #{count => spiral}}}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{}, defaults = #{}}.

%% Internal

start_broadcast_worker(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, mod_broadcast_manager),
    ChildSpec = {Proc, {mod_broadcast_manager, start_link, [HostType, Opts]},
                 permanent, 5000, worker, [mod_broadcast_manager]},
    case ejabberd_sup:start_child(ChildSpec) of
        {ok, _Pid} -> ok
    end.

stop_broadcast_worker(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, mod_broadcast_manager),
    _ = ejabberd_sup:stop_child(Proc),
    ok.
