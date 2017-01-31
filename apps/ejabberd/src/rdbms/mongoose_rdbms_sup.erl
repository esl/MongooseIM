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
-export([start_link/1,
         init/1,
         get_pids/1,
         default_pool/1,
         add_pool/4,
         remove_pool/2
        ]).

-include("ejabberd.hrl").

-define(POOL_NAME, odbc_pool).
-define(DEFAULT_POOL_SIZE, 10).

-spec start_link(binary() | string()) -> ignore | {error, term()} | {ok, pid()}.
start_link(Host) ->
    supervisor:start_link({local, gen_mod:get_module_proc(Host, ?MODULE)}, ?MODULE, Host).


-spec init(ejabberd:server()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Host) ->
    ok = application:ensure_started(worker_pool),
    catch ets:new(prepared_statements, [public, named_table, {read_concurrency, true}]),
    PoolSize = pool_size(Host),
    PoolName = default_pool(Host),
    ChildrenSpec = [pool_spec(Host, default_pool, PoolName, PoolSize)],
    {ok, {{one_for_one, 5, 60}, ChildrenSpec}}.


-spec add_pool(Host :: ejabberd:server(), Id :: supervisor:child_id(),
               Name :: atom(), Size :: pos_integer()) -> supervisor:startchild_ret().
add_pool(Host, Id, Name, Size) ->
    Sup = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = pool_spec(Host, Id, Name, Size),
    supervisor:start_child(Sup, ChildSpec).


-spec remove_pool(Host :: ejabberd:server(), Id :: supervisor:child_id()) -> ok.
remove_pool(Host, Id) ->
    Sup = gen_mod:get_module_proc(Host, ?MODULE),
    ok = supervisor:terminate_child(Sup, Id),
    ok = supervisor:delete_child(Sup, Id).


-spec pool_spec(Host :: ejabberd:server(), Id :: supervisor:child_id(),
                Name :: atom(), Size :: pos_integer()) -> supervisor:child_spec().
pool_spec(Host, Id, Name, Size) ->
    Opts = [{workers, Size}, {worker, {mongoose_rdbms, Host}}, {pool_sup_shutdown, infinity}],
    {Id, {wpool, start_pool, [Name, Opts]}, transient, 2000, supervisor, dynamic}.


-spec get_pids(ejabberd:server() | atom()) -> [pid()].
get_pids(Host) when is_binary(Host) ->
    get_pids(default_pool(Host));
get_pids(PoolName) ->
    case whereis(PoolName) of
        undefined -> [];
        _ ->
            Stats = wpool:stats(PoolName),
            {workers, Workers} = lists:keyfind(workers, 1, Stats),
            [whereis(wpool_pool:worker_name(PoolName, WorkerId)) || {WorkerId, _} <- Workers]
    end.


-spec default_pool(Host :: ejabberd:server()) -> atom().
default_pool(Host) ->
    gen_mod:get_module_proc(Host, ?POOL_NAME).


-spec pool_size(ejabberd:server()) -> integer().
pool_size(Host) ->
    case ejabberd_config:get_local_option({odbc_pool_size, Host}) of
        undefined ->
            ?DEFAULT_POOL_SIZE;
        Size when is_integer(Size) ->
            Size;
        InvalidSize ->
            Size = ?DEFAULT_POOL_SIZE,
            ?ERROR_MSG("Wrong odbc_pool_size definition '~p' "
                       "for host ~p, default to ~p~n",
                       [InvalidSize, Host, Size]),
            Size
    end.
