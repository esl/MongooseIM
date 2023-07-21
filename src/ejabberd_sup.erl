%%%----------------------------------------------------------------------
%%% File    : ejabberd_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Erlang/OTP supervisor
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_sup).
-author('alexey@process-one.net').

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/1, start_child/2, stop_child/1]).

-include("mongoose_logger.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hooks =
        {gen_hook,
         {gen_hook, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [gen_hook]},
    Cleaner =
        {mongoose_cleaner,
         {mongoose_cleaner, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [mongoose_cleaner]},
    Router =
        {ejabberd_router,
         {ejabberd_router, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [ejabberd_router]},
    S2S =
        {ejabberd_s2s,
         {ejabberd_s2s, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [ejabberd_s2s]},
    Local =
        {ejabberd_local,
         {ejabberd_local, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [ejabberd_local]},
    Listener =
        {mongoose_listener_sup,
         {mongoose_listener_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [mongoose_listener_sup]},
    ReceiverSupervisor =
        {mongoose_transport_sup,
         {ejabberd_tmp_sup, start_link,
          [mongoose_transport_sup, mongoose_transport]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    C2SSupervisor =
        {mongoose_c2s_sup,
         {ejabberd_tmp_sup, start_link, [mongoose_c2s_sup, mongoose_c2s]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    S2SInSupervisor =
        {ejabberd_s2s_in_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_s2s_in_sup, ejabberd_s2s_in]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    S2SOutSupervisor =
        {ejabberd_s2s_out_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_s2s_out_sup, ejabberd_s2s_out]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    ServiceSupervisor =
        {ejabberd_service_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_service_sup, ejabberd_service]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    OutgoingPoolsSupervisor =
        {mongoose_wpool_sup,
         {mongoose_wpool_sup, start_link, []},
         permanent, infinity,
         supervisor, [mongoose_wpool_sup]},
    IQSupervisor =
        {ejabberd_iq_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_iq_sup, mongoose_iq_worker]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    SMBackendSupervisor =
        {ejabberd_sm_backend_sup,
         {ejabberd_sm_backend_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [ejabberd_sm_backend_sup]},
    MucIQ =
        {mod_muc_iq,
         {mod_muc_iq, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [mod_muc_iq]},
    ShaperSup =
        {mongoose_shaper_sup,
          {mongoose_shaper_sup, start_link, []},
          permanent, infinity, supervisor, [mongoose_shaper_sup]},
    PG =
        {pg,
          {pg, start_link, [mim_scope]},
          permanent, infinity, worker, [pg]},
    StartIdServer =
        {mongoose_start_node_id,
          {mongoose_start_node_id, start_link, []},
          permanent, infinity, worker, [mongoose_start_node_id]},
    {ok, {{one_for_one, 10, 1},
          [StartIdServer,
           PG,
           Hooks,
           Cleaner,
           SMBackendSupervisor,
           OutgoingPoolsSupervisor
           ] ++ cets_specs() ++ [
           Router,
           S2S,
           Local,
           ReceiverSupervisor,
           C2SSupervisor,
           S2SInSupervisor,
           S2SOutSupervisor,
           ServiceSupervisor,
           IQSupervisor,
           Listener,
           MucIQ,
           ShaperSup]}}.

start_child(ChildSpec) ->
    start_child(ejabberd_sup, ChildSpec).

%% This function handles error results from supervisor:start_child
%% It does some logging
start_child(SupName, ChildSpec) ->
    case supervisor:start_child(SupName, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            Stacktrace = element(2, erlang:process_info(self(), current_stacktrace)),
            ?LOG_ERROR(#{what => start_child_failed, spec => ChildSpec,
                         supervisor_name => SupName,
                         reason => Other, stacktrace => Stacktrace}),
            erlang:error({start_child_failed, Other, ChildSpec})
    end.

stop_child(Proc) ->
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

cets_specs() ->
    cets_specs(mongoose_config:get_opt([internal_databases, cets], disabled)).

cets_specs(disabled) ->
    [];
cets_specs(#{backend := DiscoBackend, cluster_name := ClusterName} = Opts) ->
    DiscoFile =
        case {DiscoBackend, Opts} of
            {file, #{node_list_file := NodeFile}} ->
                NodeFile;
            {file, _} ->
                ?LOG_CRITICAL(#{what => node_list_file_option_is_required,
                                text => <<"Specify internal_databases.cets.node_list_file option">>}),
                error(node_list_file_option_is_required);
            _ ->
                undefined
        end,
    DiscoOpts = #{
        backend_module => disco_backend_to_module(DiscoBackend),
        cluster_name => atom_to_binary(ClusterName),
        node_name_to_insert => atom_to_binary(node(), latin1),
        name => mongoose_cets_discovery, disco_file => DiscoFile},
    CetsDisco =
        {cets_discovery,
          {cets_discovery, start_link, [DiscoOpts]},
          permanent, infinity, supervisor, [cets_discovery]},
    [CetsDisco].

disco_backend_to_module(rdbms) -> mongoose_cets_discovery_rdbms;
disco_backend_to_module(file) -> cets_discovery_file.
