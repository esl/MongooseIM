%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd's application callback module
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

-module(ejabberd_app).
-author('alexey@process-one.net').

-behaviour(application).

-export([start_modules/0, start/2, prep_stop/1, stop/1]).

-include("ejabberd.hrl").


%%%
%%% Application API
%%%

start(normal, _Args) ->
    init_log(),
    mongoose_fips:notify(),
    write_pid_file(),
    db_init(),
    application:start(cache_tab),

    translate:start(),
    acl:start(),
    ejabberd_node_id:start(),
    ejabberd_ctl:init(),
    ejabberd_commands:init(),
    mongoose_commands:init(),
    mongoose_subhosts:init(),
    gen_mod:start(),
    ejabberd_config:start(),
    ejabberd_check:config(),
    connect_nodes(),
    {ok, _} = Sup = ejabberd_sup:start_link(),
    ejabberd_rdbms:start(),
    mongoose_riak:start(),
    mongoose_cassandra:start(),
    mongoose_http_client:start(),
    ejabberd_auth:start(),
    cyrsasl:start(),
    %% Profiling
    %%ejabberd_debug:eprof_start(),
    %%ejabberd_debug:fprof_start(),
    start_modules(),
    mongoose_metrics:init(),
    ejabberd_listener:start_listeners(),
    ejabberd_admin:start(),
    ?INFO_MSG("ejabberd ~s is started in the node ~p", [?VERSION, node()]),
    Sup;
start(_, _) ->
    {error, badarg}.

%% @doc Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
    ejabberd_listener:stop_listeners(),
    stop_modules(),
    mongoose_subhosts:stop(),
    broadcast_c2s_shutdown(),
    timer:sleep(5000),
    mongoose_metrics:remove_all_metrics(),
    State.

%% All the processes were killed when this function is called
stop(_State) ->
    ?INFO_MSG("ejabberd ~s is stopped in the node ~p", [?VERSION, node()]),
    delete_pid_file(),
    %%ejabberd_debug:stop(),
    ok.


%%%
%%% Internal functions
%%%
db_init() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            application:stop(mnesia),
            mnesia:create_schema([node()]),
            application:start(mnesia, permanent);
        _ ->
            ok
    end,
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

%% @doc Start all the modules in all the hosts
-spec start_modules() -> 'ok'.
start_modules() ->
    lists:foreach(
      fun(Host) ->
              case ejabberd_config:get_local_option({modules, Host}) of
                  undefined ->
                      ok;
                  Modules ->
                      gen_mod_deps:start_modules(Host, Modules)
              end
      end, ?MYHOSTS).

%% Stop all the modules in all the hosts
-spec stop_modules() -> 'ok'.
stop_modules() ->
    lists:foreach(
      fun(Host) ->
          StopModuleFun =
              fun({Module, _Args}) ->
                  gen_mod:stop_module_keep_config(Host, Module)
              end,
          case ejabberd_config:get_local_option({modules, Host}) of
              undefined ->
                  ok;
              Modules ->
                  lists:foreach(StopModuleFun, Modules)
          end
      end, ?MYHOSTS).

-spec connect_nodes() -> 'ok'.
connect_nodes() ->
    case ejabberd_config:get_local_option(cluster_nodes) of
        undefined ->
            ok;
        Nodes when is_list(Nodes) ->
            lists:foreach(fun(Node) ->
                              net_kernel:connect_node(Node)
                          end, Nodes)
    end.

-spec broadcast_c2s_shutdown() -> 'ok'.
broadcast_c2s_shutdown() ->
    Children = supervisor:which_children(ejabberd_c2s_sup),
    lists:foreach(
      fun({_, C2SPid, _, _}) ->
          C2SPid ! system_shutdown
      end, Children).

%%%
%%% PID file
%%%

-spec write_pid_file() -> 'ok' | {'error', atom()}.
write_pid_file() ->
    case ejabberd:get_pid_file() of
        false ->
            ok;
        PidFilename ->
            write_pid_file(os:getpid(), PidFilename)
    end.

-spec write_pid_file(Pid :: string(),
                     PidFilename :: nonempty_string()
                    ) -> 'ok' | {'error', atom()}.
write_pid_file(Pid, PidFilename) ->
    case file:open(PidFilename, [write]) of
        {ok, Fd} ->
            io:format(Fd, "~s~n", [Pid]),
            file:close(Fd);
        {error, Reason} ->
            ?ERROR_MSG("Cannot write PID file ~s~nReason: ~p", [PidFilename, Reason]),
            throw({cannot_write_pid_file, PidFilename, Reason})
    end.

-spec delete_pid_file() -> 'ok' | {'error', atom()}.
delete_pid_file() ->
    case ejabberd:get_pid_file() of
        false ->
            ok;
        PidFilename ->
            file:delete(PidFilename)
    end.

init_log() ->
    ejabberd_loglevel:init(),
    case application:get_env(ejabberd, keep_lager_intact, false) of
        true ->
            skip;
        false ->
            ejabberd_loglevel:set(4)
    end.
