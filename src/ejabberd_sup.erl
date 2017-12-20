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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_sup).
-author('alexey@process-one.net').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hooks =
        {ejabberd_hooks,
         {ejabberd_hooks, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [ejabberd_hooks]},
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
    SM =
        {ejabberd_sm,
         {ejabberd_sm, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [ejabberd_sm]},
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
        {ejabberd_listener,
         {ejabberd_listener, start_link, []},
         permanent,
         infinity,
         supervisor,
         [ejabberd_listener]},
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
    HTTPSupervisor =
        {ejabberd_http_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_http_sup, ejabberd_http]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    HTTPPollSupervisor =
        {ejabberd_http_poll_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_http_poll_sup, ejabberd_http_poll]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    IQSupervisor =
        {ejabberd_iq_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_iq_sup, gen_iq_handler]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    STUNSupervisor =
        {ejabberd_stun_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_stun_sup, ejabberd_stun]},
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
    MAM =
        {mod_mam_sup,
         {mod_mam_sup, start_link, []},
         permanent, infinity, supervisor, [mod_mam_sup]},
    ShaperSpecs = shaper_srv:child_specs(),

    {ok, {{one_for_one, 10, 1},
          ShaperSpecs ++
          [Hooks,
           Cleaner,
           SMBackendSupervisor,
           Router,
           SM,
           S2S,
           Local,
           ReceiverSupervisor,
           C2SSupervisor,
           S2SInSupervisor,
           S2SOutSupervisor,
           ServiceSupervisor,
           HTTPSupervisor,
           HTTPPollSupervisor,
           IQSupervisor,
           STUNSupervisor,
           Listener,
           MucIQ,
           MAM]}}.
