%%%----------------------------------------------------------------------
%%% File    : ejabberd_rdbms.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Manage the start of the database modules when needed
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

-module(ejabberd_rdbms).
-author('alexey@process-one.net').

-export([start/0, start/1, stop/1, stop_odbc/1]).
-include("ejabberd.hrl").

-spec start() -> 'ok' | {'error','lager_not_running'}.
start() ->
    compile_odbc_type_helper(),
    %% Check if ejabberd has been compiled with ODBC
    case catch ejabberd_odbc_sup:module_info() of
        {'EXIT',{undef,_}} ->
            ?INFO_MSG("ejabberd has not been compiled with relational database support. Skipping database startup.", []);
        _ ->
            %% If compiled with ODBC, start ODBC on the needed host
            start_hosts()
    end.

%% @doc Start relationnal DB module on the nodes where it is needed
-spec start_hosts() -> 'ok'.
start_hosts() ->
    lists:foreach(fun start/1, ?MYHOSTS).

-spec stop(Host :: ejabberd:server()) -> 'ok'.
stop(Host) ->
    case needs_odbc(Host) of
        true -> stop_odbc(Host);
        false -> ok
    end.

-spec start(Host :: ejabberd:server()) -> 'ok'.
start(Host) ->
    case needs_odbc(Host) of
        true  -> start_odbc(Host);
        false -> ok
    end.

%% @doc Start the ODBC module on the given host
-spec start_odbc(binary() | string()) -> 'ok'.
start_odbc(Host) ->
    Supervisor_name = gen_mod:get_module_proc(Host, ejabberd_odbc_sup),
    ChildSpec =
        {Supervisor_name,
         {ejabberd_odbc_sup, start_link, [Host]},
         transient,
         infinity,
         supervisor,
         [ejabberd_odbc_sup]},
    case supervisor:start_child(ejabberd_sup, ChildSpec) of
        {ok, _PID} ->
            ok;
        _Error ->
            ?ERROR_MSG("Start of supervisor ~p failed:~n~p~nRetrying...~n",
                      [Supervisor_name, _Error]),
            start_odbc(Host)
    end.

-spec stop_odbc(binary() | string()) -> 'ok'.
stop_odbc(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_odbc_sup),
    supervisor:terminate_child(ejabberd_sup, Proc).

%% @doc Returns true if we have configured odbc_server for the given host
-spec needs_odbc(_) -> boolean().
needs_odbc(Host) ->
    LHost = jid:nameprep(Host),
    case ejabberd_config:get_local_option({odbc_server, LHost}) of
        undefined ->
            false;
        _ -> true
    end.

compile_odbc_type_helper() ->
    Key = {odbc_server_type, ?MYNAME},
    Type = ejabberd_config:get_local_option(Key),
    CodeStr = odbc_type_helper(Type),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "ejabberd_odbc_type.erl", Code).

odbc_type_helper(Type) ->
    lists:flatten(
        ["-module(ejabberd_odbc_type).
         -export([get/0]).
         -spec get() -> atom().
         get() -> ", normalize_type(Type), ".\n"]
    ).

normalize_type(mssql) ->
    "mssql";
normalize_type(_) ->
    "generic".
