%%%-------------------------------------------------------------------
%%% @doc Message Broadcast feature
%%%-------------------------------------------------------------------

-module(mod_broadcast).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-export([start/2,
         stop/1,
         supported_features/0,
         config_spec/0]).

-include("mongoose_config_spec.hrl").
-include("mod_broadcast.hrl").

-export_type([broadcast_job/0, broadcast_worker_state/0, execution_state/0, recipient_group/0]).

-spec start(HostType :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
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

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"backend">> => #option{type = atom,
                                           validate = {enum, [rdbms]}}},
        defaults = #{<<"backend">> => rdbms}
    }.

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
