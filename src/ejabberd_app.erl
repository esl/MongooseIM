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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_app).
-author('alexey@process-one.net').

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

-ignore_xref([prep_stop/1]).

-include("mongoose.hrl").


%%%
%%% Application API
%%%

start(normal, _Args) ->
    mongoose_fips:notify(),
    write_pid_file(),
    update_status_file(starting),
    db_init(),
    application:start(cache_tab),

    translate:start(),
    ejabberd_node_id:start(),
    ejabberd_ctl:init(),
    ejabberd_commands:init(),
    mongoose_commands:init(),
    mongoose_config:start(),
    mongoose_router:start(),
    mongoose_logs:set_global_loglevel(mongoose_config:get_opt(loglevel)),
    mongoose_deprecations:start(),
    {ok, _} = Sup = ejabberd_sup:start_link(),
    mongoose_domain_api:init(),
    mongoose_wpool:ensure_started(),
    mongoose_wpool:start_configured_pools(),
    %% ejabberd_sm is started separately because it may use one of the outgoing_pools
    %% but some outgoing_pools should be started only with ejabberd_sup already running
    ejabberd_sm:start(),
    ejabberd_auth:start(),
    mongoose_cluster_id:start(),
    mongoose_service:start(),
    mongoose_modules:start(),
    service_mongoose_system_metrics:verify_if_configured(),
    mongoose_metrics:init(),
    mongoose_listener:start(),
    ejabberd_admin:start(),
    update_status_file(started),
    ?LOG_NOTICE(#{what => mongooseim_node_started, version => ?MONGOOSE_VERSION, node => node()}),
    Sup;
start(_, _) ->
    {error, badarg}.

%% @doc Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
    mongoose_deprecations:stop(),
    mongoose_listener:stop(),
    mongoose_modules:stop(),
    mongoose_service:stop(),
    broadcast_c2s_shutdown(),
    mongoose_wpool:stop(),
    mongoose_metrics:remove_all_metrics(),
    mongoose_config:stop(),
    State.

%% All the processes were killed when this function is called
stop(_State) ->
    ?LOG_NOTICE(#{what => mongooseim_node_stopped, version => ?MONGOOSE_VERSION, node => node()}),
    delete_pid_file(),
    update_status_file(stopped),
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

-spec broadcast_c2s_shutdown() -> 'ok'.
broadcast_c2s_shutdown() ->
    Children = supervisor:which_children(ejabberd_c2s_sup),
    lists:foreach(
      fun({_, C2SPid, _, _}) ->
          C2SPid ! system_shutdown
      end, Children),
    mongoose_lib:wait_until(
      fun() ->
              Res = supervisor:count_children(ejabberd_c2s_sup),
              proplists:get_value(active, Res)
      end, 0).

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
            ?LOG_ERROR(#{what => cannot_write_to_pid_file,
                         pid_file => PidFilename, reason => Reason}),
            throw({cannot_write_pid_file, PidFilename, Reason})
    end.

update_status_file(Status) ->
    case ejabberd:get_status_file() of
        false ->
            ok;
        StatusFilename ->
            file:write_file(StatusFilename, atom_to_list(Status))
    end.

-spec delete_pid_file() -> 'ok' | {'error', atom()}.
delete_pid_file() ->
    case ejabberd:get_pid_file() of
        false ->
            ok;
        PidFilename ->
            file:delete(PidFilename)
    end.
