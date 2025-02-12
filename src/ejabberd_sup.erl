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
-export([create_ets_table/2, template_supervisor_spec/2]).

-export([start_linked_child/2]).
-ignore_xref([start_linked_child/2]).

-include("mongoose_logger.hrl").

-spec start_link() -> supervisor:startlink_err().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec init(noargs) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(noargs) ->
    Hooks = worker_spec(gen_hook),
    Instrument = worker_spec(mongoose_instrument),
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
    S2SReceiverSupervisor =
        template_supervisor_spec(mongoose_s2s_socket_out_sup, mongoose_s2s_socket_out),
    C2SSupervisor =
        template_supervisor_spec(mongoose_c2s_sup, mongoose_c2s),
    S2SOutSupervisor =
        template_supervisor_spec(mongoose_s2s_out_sup, mongoose_s2s_out),
    IQSupervisor =
        template_supervisor_spec(ejabberd_iq_sup, mongoose_iq_worker),
    {ok, {{one_for_one, 10, 1},
          [StartIdServer,
           PG,
           Hooks,
           Instrument,
           Cleaner,
           SMBackendSupervisor,
           OutgoingPoolsSupervisor
           ] ++ mongoose_cets_discovery:supervisor_specs() ++ [
           Router,
           S2S,
           S2SReceiverSupervisor,
           S2SOutSupervisor,
           Local,
           C2SSupervisor,
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

-spec template_supervisor_spec(atom(), module()) -> supervisor:child_spec().
template_supervisor_spec(Name, Module) ->
    #{
        id => Name,
        start => {mongoose_template_sup, start_link, [Name, Module]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [mongoose_template_sup]
    }.

supervisor_spec(Mod) ->
    {Mod, {Mod, start_link, []}, permanent, infinity, supervisor, [Mod]}.

worker_spec(Mod) ->
    worker_spec(Mod, []).

worker_spec(Mod, Args) ->
    %% We use `start_linked_child' wrapper to log delays
    %% in the slow init worker functions.
    MFA = {?MODULE, start_linked_child, [Mod, Args]},
    {Mod, MFA, permanent, timer:seconds(5), worker, [Mod]}.

%% In case one of the workers takes long time to start
%% we want the logging progress (to know which child got stuck).
%% This could happend on CI during the node restarts.
start_linked_child(Mod, Args) ->
    F = fun() -> erlang:apply(Mod, start_link, Args) end,
    mongoose_task:run_tracked(#{task => start_linked_child, child_module => Mod}, F).

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
