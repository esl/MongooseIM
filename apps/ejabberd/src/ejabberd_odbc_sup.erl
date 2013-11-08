%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ODBC connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_odbc_sup).
-author('alexey@process-one.net').
-behaviour(supervisor).

%% API
-export([start_link/1,
         with_connection/2]).

%% supervisor's callbacks
-export([init/1]).

-include("ejabberd.hrl").

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_ODBC_START_INTERVAL, 30). % 30 seconds

% time to wait for the supervisor to start its child before returning
% a timeout error to the request
-define(CONNECT_TIMEOUT, 500). % milliseconds

start_link(Host) ->
    supervisor:start_link(?MODULE, [Host]).

with_connection(Host, TransF) ->
    poolboy:transaction(pool_name(Host), TransF).

init([Host]) ->
    StartInterval = start_interval(Host) * 1000,
    PoolName = pool_name(Host),
    PoolSize = pool_size(Host),
    PoolArgs =
        [{name, {local, PoolName}},
         {size, PoolSize},
         {max_overflow, 0},
         {worker_module, ejabberd_odbc}],
    WorkerArgs = [Host, StartInterval],
    PoolWorkersSpecs = poolboy:child_spec(
        PoolName, PoolArgs, WorkerArgs),
    {ok, {{one_for_all, 10, 10}, [PoolWorkersSpecs]}}.

pool_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

pool_size(Host) ->
    MaybeSize = ejabberd_config:get_local_option({odbc_pool_size, Host}),
    maybe_pool_size(Host, MaybeSize).

maybe_pool_size(_, undefined) ->
   ?DEFAULT_POOL_SIZE;
maybe_pool_size(_, Size) when is_integer(Size) ->
    Size;
maybe_pool_size(Host, InvalidSize) ->
    Size = ?DEFAULT_POOL_SIZE,
    ?ERROR_MSG("Wrong odbc_pool_size definition '~p' "
               "for host ~p, default to ~p~n",
               [InvalidSize, Host, Size]),
    Size.

start_interval(Host) ->
    MaybeSize =
        ejabberd_config:get_local_option({odbc_start_interval, Host}),
    maybe_start_interval(Host, MaybeSize).

maybe_start_interval(_, undefined) ->
   ?DEFAULT_ODBC_START_INTERVAL;
maybe_start_interval(_, Interval) when is_integer(Interval) ->
    Interval;
maybe_start_interval(Host, InvalidInterval) ->
    Interval = ?DEFAULT_ODBC_START_INTERVAL,
    ?ERROR_MSG("Wrong odbc_start_interval "
               "definition '~p' for host ~p, "
               "defaulting to ~p~n",
               [InvalidInterval, Host, Interval]),
    Interval.
