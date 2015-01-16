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

-export([start/0,
         stop/0,
         ensure_started/1,
         get_pid_file/0,
         get_so_path/0,
         get_bin_path/0]).

-include("jlib.hrl").

-type lang() :: binary() | nonempty_string().

-type sockmod() :: gen_tcp
                 | ejabberd_socket
                 | mod_bosh_socket
                 | mod_websockets
                 | ejabberd_tls
                 | ejabberd_zlib.

-type user()      :: binary().
-type server()    :: binary().
-type resource()  :: binary().
-type luser()     :: binary().
-type lserver()   :: binary().
-type lresource() :: binary().

%% A tuple-style JID
-type simple_jid() :: {user(), server(), resource()}.

%% A tuple-style JID without resource part
-type simple_bare_jid() :: {LUser :: luser(), LServer :: lserver()}.
-type literal_jid() :: binary().

%% Incoming event from XML stream. Used everywhere in xmlstream fsm modules
-type xml_stream_item() :: 'closed'
                          | 'timeout'
                          | {'xmlstreamelement', jlib:xmlel()}
                          | {'xmlstreamend',_}
                          | {'xmlstreamerror',_}
                          | {'xmlstreamstart', Name :: any(), Attrs :: list()}.

-export_type([lang/0,
              sockmod/0,
              jid/0, simple_jid/0, simple_bare_jid/0,
              iq/0,
              xml_stream_item/0,
              user/0, server/0, resource/0,
              luser/0, lserver/0, lresource/0,
              literal_jid/0
             ]).

-ifdef(only_builtin_types).
-type dict_t() :: dict().
-type queue_t() :: queue().
-type set_t() :: set().
-else.
-type dict_t() :: dict:dict().
-type queue_t() :: queue:queue().
-type set_t() :: set:set().
-endif.

-export_type([dict_t/0, queue_t/0, set_t/0]).

start() ->
    ensure_started(ejabberd).

ensure_started(AppName) ->
    case application:start(AppName) of
        ok ->
            ok;
        {error, {already_started, AppName}} ->
            ok;
        {error, {not_started, AppName2}} ->
            ok = ensure_started(AppName2),
            ensure_started(AppName)
    end.

stop() ->
    application:stop(ejabberd).

-spec get_so_path() -> binary() | string().
get_so_path() ->
    case os:getenv("EJABBERD_SO_PATH") of
        false ->
            case code:priv_dir(ejabberd) of
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
