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

%% API
-export([start_link/1,
         get_dedicated_connection/1,
         init/1,
         add_pid/2,
         remove_pid/2,
         get_pids/1,
         get_random_pid/1
        ]).

-include("ejabberd.hrl").

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_ODBC_START_INTERVAL, 30). % 30 seconds

% time to wait for the supervisor to start its child before returning
% a timeout error to the request
-define(CONNECT_TIMEOUT, 500). % milliseconds

-record(sql_pool, {host :: ejabberd_odbc:odbc_server(),
                   pid :: pid()
                  }).

-spec start_link(binary() | string()) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Host) ->
    mnesia:create_table(sql_pool,
                        [{ram_copies, [node()]},
                         {type, bag},
                         {local_content, true},
                         {attributes, record_info(fields, sql_pool)}]),
    mnesia:add_table_copy(sql_pool, node(), ram_copies),
    F = fun() ->
                mnesia:delete({sql_pool, Host})
        end,
    mnesia:ets(F),
    supervisor:start_link({local, gen_mod:get_module_proc(Host, ?MODULE)},
                          ?MODULE, [Host]).

-spec get_dedicated_connection(Host :: ejabberd:server())
      -> 'ignore' | {'error',_} | {'ok',{Host :: atom(), pid()}}.
get_dedicated_connection(Host) ->
    StartInterval = start_interval(Host)*1000,
    case ejabberd_odbc:start_link(Host, StartInterval, true) of
        {ok, Pid} ->
            {ok, {Host, Pid}};
        Other ->
            Other
    end.

-spec init([ejabberd:server(),...]) -> {'ok',{{_,_,_},[any()]}}.
init([Host]) ->
    PoolSize = pool_size(Host),
    StartInterval = start_interval(Host)*1000,
    {ok, {{one_for_one, PoolSize*10, 1},
          lists:map(
            fun(I) ->
                    {I,
                     {ejabberd_odbc, start_link, [Host, StartInterval, false]},
                     transient,
                     2000,
                     worker,
                     [?MODULE]}
            end, lists:seq(1, PoolSize))}}.

-spec get_pids(_) -> [pid()].
get_pids(Host) ->
    Rs = mnesia:dirty_read(sql_pool, Host),
    [R#sql_pool.pid || R <- Rs].

-spec get_random_pid(ejabberd:server()) -> pid().
get_random_pid(Host) ->
    Pids = get_pids(Host),
    Pids == [] andalso erlang:error({empty_sql_pool, Host}),
    lists:nth(erlang:phash(now(), length(Pids)), Pids).

-spec add_pid(_, pid()) -> any().
add_pid(Host, Pid) ->
    F = fun() ->
                mnesia:write(#sql_pool{host = Host, pid = Pid})
        end,
    spawn(fun() ->
            MonRef = monitor(process, Pid),
            receive
                {'DOWN', MonRef, process, Pid, _} ->
                    remove_pid(Host, Pid);
                _ ->
                    error
            end
        end),
    mnesia:ets(F).

remove_pid(Host, Pid) ->
    F = fun() ->
                mnesia:delete_object(#sql_pool{host = Host, pid = Pid})
        end,
    mnesia:ets(F).

-spec pool_size(ejabberd:server()) -> integer().
pool_size(Host) ->
    MaybeSize = ejabberd_config:get_local_option({odbc_pool_size, Host}),
    maybe_pool_size(Host, MaybeSize).

-spec maybe_pool_size(ejabberd:server(), integer() | undefined | term()) -> integer().
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

-spec start_interval(ejabberd:server()) -> integer().
start_interval(Host) ->
    MaybeSize =
        ejabberd_config:get_local_option({odbc_start_interval, Host}),
    maybe_start_interval(Host, MaybeSize).

-spec maybe_start_interval(ejabberd:server(), integer() | undefined | term()) -> integer().
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

