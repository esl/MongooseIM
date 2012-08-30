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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------
-module(ejabberd_zlib).
-author('alexey@process-one.net').

-export([enable_zlib/3, disable_zlib/1,
         send/2,
         recv/2, recv/3, recv_data/2,
         setopts/2,
         sockname/1, peername/1,
         get_sockmod/1,
         controlling_process/2,
         close/1]).

-record(zlibsock, {sockmod, socket, inflate, deflate}).

enable_zlib(SockMod, Socket, Pid) ->
    Inflate = zlib:open(),
    ok = zlib:inflateInit(Inflate),
    erlang:port_connect(Inflate, Pid),
    Deflate = zlib:open(),
    ok = zlib:deflateInit(Deflate),
    %%erlang:port_connect(Deflate, Pid),
    {ok, #zlibsock{sockmod = SockMod, socket = Socket,
                   inflate = Inflate, deflate = Deflate}}.

disable_zlib(#zlibsock{sockmod = SockMod, socket = Socket,
                       inflate = Inflate, deflate = Deflate}) ->
    zlib:close(Inflate),
    zlib:close(Deflate),
    {SockMod, Socket}.

recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#zlibsock{sockmod = SockMod, socket = Socket} = ZlibSock,
     Length, Timeout) ->
    case SockMod:recv(Socket, Length, Timeout) of
	{ok, Packet} ->
	    recv_data(ZlibSock, Packet);
	{error, _Reason} = Error ->
	    Error
    end.

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

recv_data1(#zlibsock{inflate = Inflate} = _ZlibSock, Packet) ->
    Data = zlib:inflate(Inflate, Packet),
    {ok, list_to_binary(Data)}.

send(#zlibsock{sockmod = SockMod, socket = Socket, deflate = Deflate},
     Packet) ->
    Compressed = zlib:deflate(Deflate, Packet, full),
    SockMod:send(Socket, Compressed).


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

close(#zlibsock{sockmod = SockMod, socket = Socket,
                inflate = Inflate, deflate = Deflate}) ->
    SockMod:close(Socket),

    zlib:close(Inflate),
    try
        port_connect(Deflate, self()),
        zlib:close(Deflate)
    catch _:_ ->
            ok
    end.
