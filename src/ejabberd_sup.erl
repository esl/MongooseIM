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
-export([start_child/1, stop_child/1]).

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
        {ejabberd_receiver_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_receiver_sup, ejabberd_receiver]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    C2SSupervisor =
        {ejabberd_c2s_sup,
         {ejabberd_tmp_sup, start_link, [ejabberd_c2s_sup, ejabberd_c2s]},
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
        {ejabberd_shaper_sup,
          {ejabberd_shaper_sup, start_link, []},
          permanent, infinity, supervisor, [ejabberd_shaper_sup]},
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
           ShaperSup]}}.

start_child(ChildSpec) ->
    case supervisor:start_child(ejabberd_sup, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            Stacktrace = element(2, erlang:process_info(self(), current_stacktrace)),
            ?LOG_ERROR(#{what => start_child_failed, spec => ChildSpec,
                         reason => Other, stacktrace => Stacktrace}),
            erlang:error({start_child_failed, Other, ChildSpec})
    end.

stop_child(Proc) ->
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.
