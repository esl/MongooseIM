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
-export([create_ets_table/2]).

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
    StartIdServer = worker_spec(mongoose_start_node_id),
    PG = worker_spec(pg, [mim_scope]),
    SMBackendSupervisor = supervisor_spec(ejabberd_sm_backend_sup),
    OutgoingPoolsSupervisor = supervisor_spec(mongoose_wpool_sup),
    Listener = supervisor_spec(mongoose_listener_sup),
    ShaperSup = mongoose_shaper:child_spec(),
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
    {ok, {{one_for_one, 10, 1},
          [StartIdServer,
           PG,
           Hooks,
           Cleaner,
           SMBackendSupervisor,
           OutgoingPoolsSupervisor
           ] ++ mongoose_cets_discovery:supervisor_specs() ++ [
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
           ShaperSup,
           DomainSup]}}.

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

ejabberd_tmp_sup_spec(Name, Args) ->
    {Name,
     {ejabberd_tmp_sup, start_link, Args},
     permanent, infinity, supervisor, [ejabberd_tmp_sup]}.

supervisor_spec(Mod) ->
    {Mod, {Mod, start_link, []}, permanent, infinity, supervisor, [Mod]}.

worker_spec(Mod) ->
    worker_spec(Mod, []).

worker_spec(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, timer:seconds(5), worker, [Mod]}.

-spec create_ets_table(atom(), list()) -> ok.
create_ets_table(TableName, TableOpts) ->
    case does_table_exist(TableName) of
        true -> ok;
        false ->
            Opts = maybe_add_heir(whereis(?MODULE), self(), TableOpts),
            ets:new(TableName, Opts),
            ok
    end.

does_table_exist(TableName) ->
    undefined =/= ets:info(TableName, name).

%% In tests or when module is started in run-time, we need to set heir to the
%% ETS table, otherwise it will be destroyed when the creator's process finishes.
%% When started normally during node start up, self() =:= EjdSupPid and there
%% is no need for setting heir
maybe_add_heir(EjdSupPid, EjdSupPid, BaseOpts) when is_pid(EjdSupPid) ->
    BaseOpts;
maybe_add_heir(EjdSupPid, _Self, BaseOpts) when is_pid(EjdSupPid) ->
    case lists:keymember(heir, 1, BaseOpts) of
        true -> BaseOpts;
        false -> [{heir, EjdSupPid, testing} | BaseOpts]
    end;
maybe_add_heir(_, _, BaseOpts) ->
    BaseOpts.
