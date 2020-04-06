%%%----------------------------------------------------------------------
%%% File    : ejabberd_zlib.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to zlib
%%% Created : 19 Jan 2006 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_zlib).
-author('alexey@process-one.net').
-xep([{xep, 138}, {version, "2.0"}]).
-export([enable_zlib/3, disable_zlib/1,
         send/2,
         recv_data/2,
         setopts/2,
         sockname/1, peername/1,
         get_sockmod/1,
         controlling_process/2,
         close/1]).

-define(DEFLATE, 1).
-define(INFLATE, 2).

-record(zlibsock, {sockmod, socket, zlibport, inflate_size_limit = 0}).
-type zlibsock() :: #zlibsock{}.

-spec enable_zlib(ejabberd:sockmod(), zlibsock(), integer()) -> {ok, zlibsock()}.
enable_zlib(SockMod, Socket, InflateSizeLimit) ->
    case erl_ddll:load_driver(ejabberd:get_so_path(), ejabberd_zlib_drv) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, OtherError} ->
            erlang:error({cannot_load_ejabberd_zlib_drv, erl_ddll:format_error(OtherError)})
    end,
    Port = open_port({spawn_driver, "ejabberd_zlib_drv"}, [binary]),
    {ok, #zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port,
                   inflate_size_limit = InflateSizeLimit}}.

-spec disable_zlib(zlibsock()) -> {ejabberd:sockmod(), inet:socket()}.
disable_zlib(#zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port}) ->
    port_close(Port),
    {SockMod, Socket}.

-spec recv_data(zlibsock(), Packet :: string()|binary()) -> {ok, _} | {error, _}.
recv_data(#zlibsock{sockmod = SockMod, socket = Socket} = ZlibSock, Packet) ->
    case SockMod of
        gen_tcp ->
            recv_data2(ZlibSock, Packet);
        _ ->
            case SockMod:recv_data(Socket, Packet) of
                {ok, Packet2} ->
                    recv_data2(ZlibSock, Packet2);
                Error ->
                    Error
            end
    end.

recv_data2(ZlibSock, Packet) ->
    case catch recv_data1(ZlibSock, Packet) of
        {'EXIT', Reason} ->
            {error, Reason};
        Res ->
            Res
    end.

-spec recv_data1(zlibsock(), iolist()) -> {'error', atom()} | {'ok', binary()}.
recv_data1(#zlibsock{zlibport = Port, inflate_size_limit = SizeLimit} = _ZlibSock, Packet) ->
    case port_control(Port, SizeLimit bsl 2 + ?INFLATE, Packet) of
        <<0, In/binary>> ->
            {ok, In};
        <<1, Error/binary>> ->
            {error, erlang:binary_to_existing_atom(Error, utf8)}
    end.

-spec send(zlibsock(), iolist()) -> ok | {error, atom()}.
send(#zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port},
     Packet) ->
    case port_control(Port, ?DEFLATE, Packet) of
        <<0, Out/binary>> ->
        mongoose_metrics:update(global, [data, xmpp, sent, compressed_size], size(Out)),
            SockMod:send(Socket, Out);
        <<1, Error/binary>> ->
            {error, erlang:binary_to_existing_atom(Error, utf8)};
        _ ->
            {error, deflate_error}
    end.


setopts(#zlibsock{sockmod = SockMod, socket = Socket}, Opts) ->
    case SockMod of
        gen_tcp ->
            inet:setopts(Socket, Opts);
        _ ->
            SockMod:setopts(Socket, Opts)
    end.

sockname(#zlibsock{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
        gen_tcp ->
            inet:sockname(Socket);
        _ ->
            SockMod:sockname(Socket)
    end.

get_sockmod(#zlibsock{sockmod = SockMod}) ->
    SockMod.

peername(#zlibsock{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
        gen_tcp ->
            inet:peername(Socket);
        _ ->
            SockMod:peername(Socket)
    end.

controlling_process(#zlibsock{sockmod = SockMod, socket = Socket}, Pid) ->
    SockMod:controlling_process(Socket, Pid).

close(#zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port}) ->
    SockMod:close(Socket),
    port_close(Port).
