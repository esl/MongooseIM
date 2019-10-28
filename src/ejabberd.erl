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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd).
-author('alexey@process-one.net').
-xep([{xep, 212}, {version, "1.0"}]).
-export([start/0,
         stop/0,
         get_pid_file/0,
         get_status_file/0,
         get_so_path/0,
         get_bin_path/0]).

-include("jlib.hrl").

-type lang() :: binary().

-type sockmod() :: gen_tcp
                 | ejabberd_socket
                 | mod_bosh_socket
                 | mod_websockets
                 | ejabberd_tls
                 | ejabberd_zlib.

%% Incoming event from XML stream. Used everywhere in xmlstream fsm modules
-type xml_stream_item() :: 'closed'
                          | 'timeout'
                          | {'xmlstreamelement', exml:element()}
                          | {'xmlstreamend', _}
                          | {'xmlstreamerror', _}
                          | {'xmlstreamstart', Name :: any(), Attrs :: list()}.

-export_type([lang/0,
              sockmod/0,
              xml_stream_item/0
             ]).

start() ->
    application:ensure_all_started(mongooseim).

stop() ->
    application:stop(mongooseim).

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

-spec get_bin_path() -> binary() | string().
get_bin_path() ->
    case os:getenv("EJABBERD_BIN_PATH") of
        false ->
            case code:priv_dir(ejabberd) of
                {error, _} ->
                    ".";
                Path ->
                    filename:join([Path, "bin"])
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
