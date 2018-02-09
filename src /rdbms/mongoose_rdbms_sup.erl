%%%----------------------------------------------------------------------
%%% File    : mongoose_rdbms_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ODBC connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%% Copyright 2016 Erlang Solutions Ltd.
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

-module(mongoose_rdbms_sup).
-author('alexey@process-one.net').
-author('konrad.zemek@erlang-solutions.com').

%% API
-export([start_link/0,
         init/1,
         get_pids/1,
         pool_proc/1,
         pool/1,
         add_pool/1,
         remove_pool/1,
         get_option/2
        ]).

-include("mongoose.hrl").

-define(DEFAULT_POOL_NAME, default).
-define(DEFAULT_POOL_SIZE, 10).

-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ok = application:ensure_started(worker_pool),
    catch ets:new(prepared_statements, [public, named_table, {read_concurrency, true}]),
    {ok, {{one_for_one, 5, 60}, []}}.

-spec add_pool(Pool :: mongoose_rdbms:pool()) -> supervisor:startchild_ret().
add_pool(Pool) ->
    Size = pool_size(Pool),
    ChildSpec = pool_spec(Pool, Size),
    mongoose_metrics:ensure_db_pool_metric(Pool),
    supervisor:start_child(?MODULE, ChildSpec).

-spec remove_pool(Pool :: mongoose_rdbms:pool()) -> ok.
remove_pool(Pool) ->
    ok = supervisor:terminate_child(?MODULE, Pool),
    ok = supervisor:delete_child(?MODULE, Pool).

-spec pool_spec(Pool :: mongoose_rdbms:pool(), Size :: pos_integer()) -> supervisor:child_spec().
pool_spec(Pool, Size) ->
    Opts = [{workers, Size}, {worker, {mongoose_rdbms, Pool}}, {pool_sup_shutdown, infinity}],
    {Pool, {wpool, start_pool, [pool_proc(Pool), Opts]}, transient, 2000, supervisor, dynamic}.


-spec get_pids(jid:server() | atom()) -> [pid()].
get_pids(HostOrPool) ->
    PoolProc = pool_proc(HostOrPool),
    case whereis(PoolProc) of
        undefined -> [];
        _ ->
            Stats = wpool:stats(PoolProc),
            {workers, Workers} = lists:keyfind(workers, 1, Stats),
            [whereis(wpool_pool:worker_name(PoolProc, WorkerId)) || {WorkerId, _} <- Workers]
    end.

-spec pool_proc(mongoose_rdbms:server()) -> atom().
pool_proc(Host) when is_binary(Host) ->
    pool_proc(pool(Host));
pool_proc(Pool) when is_atom(Pool) ->
    gen_mod:get_module_proc(atom_to_list(Pool), mongoose_rdbms_pool).

-spec pool(mongoose_rdbms:server()) -> mongoose_rdbms:pool().
pool(Host) when is_binary(Host) ->
    ejabberd_config:get_local_option_or_default({odbc_pool, Host}, ?DEFAULT_POOL_NAME);
pool(Pool) when is_atom(Pool) ->
    Pool.

-spec pool_size(mongoose_rdbms:pool()) -> integer().
pool_size(Pool) ->
    case get_option(Pool, odbc_pool_size) of
        undefined ->
            ?DEFAULT_POOL_SIZE;
        Size when is_integer(Size) ->
            Size;
        InvalidSize ->
            Size = ?DEFAULT_POOL_SIZE,
            ?ERROR_MSG("Wrong odbc_pool_size definition '~p' "
                       "for pool ~p, default to ~p~n",
                       [InvalidSize, Pool, Size]),
            Size
    end.

-spec get_option(mongoose_rdbms:pool(), atom()) -> term().
get_option(Pool, Option) ->
    ejabberd_config:get_local_option(config_key(Pool, Option)).

config_key(Pool, Option) ->
    {Option, odbc_pool, Pool}.
