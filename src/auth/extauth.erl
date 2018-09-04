%%%----------------------------------------------------------------------
%%% File    : extauth.erl
%%% Author  : Leif Johansson <leifj@it.su.se>
%%% Purpose : External authentication using a simple port-driver
%%% Created : 30 Jul 2004 by Leif Johansson <leifj@it.su.se>
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

-module(extauth).
-author('leifj@it.su.se').

-export([start/2,
         stop/1,
         init/2,
         check_password/3,
         set_password/3,
         try_register/3,
         remove_user/2,
         remove_user/3,
         is_user_exists/2]).

-include("mongoose.hrl").

-define(INIT_TIMEOUT, 60000). % Timeout is in milliseconds: 60 seconds == 60000
-define(CALL_TIMEOUT, 10000). % Timeout is in milliseconds: 10 seconds == 10000


-spec start(atom() | binary(), _) -> 'ok'.
start(Host, ExtPrg) ->
    lists:foreach(
        fun(This) ->
            start_instance(get_process_name(Host, This), ExtPrg)
        end,
        lists:seq(0, get_instances(Host)-1)
    ).


-spec start_instance(atom(), _) -> pid().
start_instance(ProcessName, ExtPrg) ->
    spawn(?MODULE, init, [ProcessName, ExtPrg]).


-spec restart_instance(atom(), _) -> pid().
restart_instance(ProcessName, ExtPrg) ->
    unregister(ProcessName),
    start_instance(ProcessName, ExtPrg).


-spec init(atom(), string()) -> no_return().
init(ProcessName, ExtPrg) ->
    register(ProcessName, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port, ?INIT_TIMEOUT, ProcessName, ExtPrg).


-spec stop(atom() | binary()) -> 'ok'.
stop(Host) ->
    lists:foreach(
        fun(This) ->
            get_process_name(Host, This) ! stop
        end,
        lists:seq(0, get_instances(Host)-1)
    ).


-spec get_process_name(binary(), integer()) -> atom().
get_process_name(Host, Integer) ->
    gen_mod:get_module_proc(lists:append([erlang:binary_to_list(Host), integer_to_list(Integer)]), eauth).


-spec check_password(jid:user(), jid:server(), binary()) -> boolean().
check_password(User, Server, Password) ->
    call_port(Server, [<<"auth">>, User, Server, Password]).


-spec is_user_exists(jid:user(), jid:server()) -> boolean().
is_user_exists(User, Server) ->
    call_port(Server, [<<"isuser">>, User, Server]).


-spec set_password(jid:user(), jid:server(), binary()) -> any().
set_password(User, Server, Password) ->
    call_port(Server, [<<"setpass">>, User, Server, Password]).


-spec try_register(jid:user(), jid:server(), binary()
                  ) -> ok | {error, not_allowed}.
try_register(User, Server, Password) ->
    case call_port(Server, [<<"tryregister">>, User, Server, Password]) of
        true -> ok;
        false -> {error, not_allowed}
    end.


-spec remove_user(jid:user(), jid:server()) -> any().
remove_user(User, Server) ->
    call_port(Server, [<<"removeuser">>, User, Server]).


-spec remove_user(jid:user(), jid:server(), binary()) -> any().
remove_user(User, Server, Password) ->
    call_port(Server, [<<"removeuser3">>, User, Server, Password]).


-spec call_port(jid:server(), [any(), ...]) -> any().
call_port(Server, Msg) ->
    LServer = jid:nameprep(Server),
    ProcessName = get_process_name(LServer, random_instance(get_instances(LServer))),
    ProcessName ! {call, self(), Msg},
    receive
        {eauth, Result} ->
            Result
    end.


-spec random_instance(pos_integer()) -> non_neg_integer().
random_instance(MaxNum) ->
    rand:uniform(MaxNum) - 1.


-spec get_instances(atom() | binary()) -> integer().
get_instances(Server) ->
    case ejabberd_config:get_local_option({extauth_instances, Server}) of
        Num when is_integer(Num) -> Num;
        _ -> 1
    end.


-spec loop(port(), integer(), atom(), any()) -> no_return().
loop(Port, Timeout, ProcessName, ExtPrg) ->
    receive
        {call, Caller, Msg} ->
            port_command(Port, encode(Msg)),
            receive
                {Port, {data, Data}} ->
                    ?DEBUG("extauth call '~p' received data response:~n~p", [Msg, Data]),
                    Caller ! {eauth, decode(Data)},
                    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg);
                {Port, Other} ->
                    ?ERROR_MSG("extauth call '~p' received strange response:~n~p", [Msg, Other]),
                    Caller ! {eauth, false},
                    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg)
            after
                Timeout ->
                    ?ERROR_MSG("extauth call '~p' didn't receive response", [Msg]),
                    Caller ! {eauth, false},
                    Pid = restart_instance(ProcessName, ExtPrg),
                    flush_buffer_and_forward_messages(Pid),
                    exit(port_terminated)
            end;
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            ?CRITICAL_MSG("extauth script has exitted abruptly with reason '~p'", [Reason]),
            Pid = restart_instance(ProcessName, ExtPrg),
            flush_buffer_and_forward_messages(Pid),
            exit(port_terminated)
    end.


-spec flush_buffer_and_forward_messages(pid()) -> 'true'.
flush_buffer_and_forward_messages(Pid) ->
    receive
        Message ->
            Pid ! Message,
            flush_buffer_and_forward_messages(Pid)
    after 0 ->
            true
    end.


-spec join([binary()], binary()) -> binary().
join(List, Sep) ->
    lists:foldl(fun(A, <<"">>) -> A;
                   (A, Acc) -> <<Acc/bitstring, Sep/bitstring, A/bitstring>>
        end, <<"">>, List).


-spec encode([binary()]) -> [byte()].
encode(L) ->
    erlang:binary_to_list(join(L, <<":">>)).


-spec decode([0 | 1, ...]) -> boolean().
decode([0, 0]) -> false;
decode([0, 1]) -> true.

