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
-export([get_pid_file/0, get_status_file/0]).

-type lang() :: binary().

%% Incoming event from XML stream. Used everywhere in xmlstream fsm modules
-type xml_stream_item() :: 'closed'
                          | 'timeout'
                          | {'xmlstreamelement', exml:element()}
                          | exml_stream:start()
                          | exml_stream:stop()
                          | jlib:xmlstreamerror().

-export_type([lang/0, xml_stream_item/0]).

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
