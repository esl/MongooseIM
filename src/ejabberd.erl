%%%----------------------------------------------------------------------
%%% File    : ejabberd.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd wrapper: start / stop
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd).
-author('alexey@process-one.net').
-export([start/0,
         stop/0,
         write_pid_file/0,
         update_status_file/1,
         delete_pid_file/0,
         get_so_path/0]).

-ignore_xref([start/0, stop/0]).

-include("mongoose.hrl").

-type lang() :: binary().

%% Incoming event from XML stream. Used everywhere in xmlstream fsm modules
-type xml_stream_item() :: 'closed'
                          | 'timeout'
                          | {'xmlstreamelement', exml:element()}
                          | {'xmlstreamend', _}
                          | {'xmlstreamerror', _}
                          | {'xmlstreamstart', Name :: any(), Attrs :: list()}.

-export_type([lang/0, xml_stream_item/0]).

start() ->
    mongooseim:start().

stop() ->
    mongooseim:stop().

-spec write_pid_file() -> 'ok' | {'error', atom()}.
write_pid_file() ->
    case get_pid_file() of
        false ->
            ok;
        PidFilename ->
            write_pid_file(os:getpid(), PidFilename)
    end.

-spec write_pid_file(Pid :: string(), PidFilename :: nonempty_string()) ->
    'ok' | {'error', atom()}.
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

-spec update_status_file(term()) -> 'ok' | {'error', atom()}.
update_status_file(Status) ->
    case get_status_file() of
        false ->
            ok;
        StatusFilename ->
            file:write_file(StatusFilename, atom_to_list(Status))
    end.

-spec delete_pid_file() -> 'ok' | {'error', atom()}.
delete_pid_file() ->
    case get_pid_file() of
        false ->
            ok;
        PidFilename ->
            file:delete(PidFilename)
    end.

-spec get_so_path() -> binary() | string().
get_so_path() ->
    case os:getenv("EJABBERD_SO_PATH") of
        false ->
            case code:priv_dir(mongooseim) of
                {error, _} ->
                    ".";
                Path ->
                    filename:join([Path, "lib"])
            end;
        Path ->
            Path
    end.

-spec get_pid_file() -> 'false' | nonempty_string().
get_pid_file() ->
    case os:getenv("EJABBERD_PID_PATH") of
        false ->
            false;
        "" ->
            false;
        Path ->
            Path
    end.

-spec get_status_file() -> 'false' | nonempty_string().
get_status_file() ->
    case os:getenv("EJABBERD_STATUS_PATH") of
        false ->
            false;
        "" ->
            false;
        Path ->
            Path
    end.
