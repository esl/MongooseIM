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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(extauth).
-author('leifj@it.su.se').

-export([start/2,
         stop/1,
         init/2,
         check_password/4,
         set_password/4,
         try_register/4,
         remove_user/3,
         does_user_exist/3]).

-include("mongoose.hrl").

-define(INIT_TIMEOUT, 60000). % Timeout is in milliseconds: 60 seconds == 60000
-define(CALL_TIMEOUT, 10000). % Timeout is in milliseconds: 10 seconds == 10000


-spec start(mongooseim:host_type(), _) -> 'ok'.
start(HostType, ExtPrg) ->
    lists:foreach(
        fun(This) ->
            start_instance(get_process_name(HostType, This), ExtPrg)
        end,
        lists:seq(0, get_instances(HostType) - 1)
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
stop(HostType) ->
    lists:foreach(
        fun(This) ->
            get_process_name(HostType, This) ! stop
        end,
        lists:seq(0, get_instances(HostType) - 1)
    ).


-spec get_process_name(binary(), integer()) -> atom().
get_process_name(HostType, Integer) ->
    gen_mod:get_module_proc(lists:append([erlang:binary_to_list(HostType),
                                          integer_to_list(Integer)]), eauth).


-spec check_password(mongooseim:host_type(), jid:user(), jid:server(), binary()) -> boolean().
check_password(HostType, User, Server, Password) ->
    call_port(HostType, [<<"auth">>, User, Server, Password]).


-spec does_user_exist(mongooseim:host_type(), jid:user(), jid:server()) -> boolean().
does_user_exist(HostType, User, Server) ->
    call_port(HostType, [<<"isuser">>, User, Server]).


-spec set_password(mongooseim:host_type(), jid:user(), jid:server(), binary()) -> any().
set_password(HostType, User, Server, Password) ->
    call_port(HostType, [<<"setpass">>, User, Server, Password]).


-spec try_register(mongooseim:host_type(), jid:user(), jid:server(), binary()) ->
          ok | {error, not_allowed}.
try_register(HostType, User, Server, Password) ->
    case call_port(HostType, [<<"tryregister">>, User, Server, Password]) of
        true -> ok;
        false -> {error, not_allowed}
    end.


-spec remove_user(mongooseim:host_type(), jid:user(), jid:server()) -> any().
remove_user(HostType, User, Server) ->
    call_port(HostType, [<<"removeuser">>, User, Server]).

-spec call_port(mongooseim:host_type(), [any(), ...]) -> any().
call_port(HostType, Msg) ->
    ProcessName = get_process_name(HostType, random_instance(get_instances(HostType))),
    ProcessName ! {call, self(), Msg},
    receive
        {eauth, Result} ->
            Result
    end.


-spec random_instance(pos_integer()) -> non_neg_integer().
random_instance(MaxNum) ->
    rand:uniform(MaxNum) - 1.


-spec get_instances(mongooseim:host_type()) -> integer().
get_instances(HostType) ->
    mongoose_config:get_opt([{auth, HostType}, external, instances]).


-spec loop(port(), integer(), atom(), any()) -> no_return().
loop(Port, Timeout, ProcessName, ExtPrg) ->
    receive
        {call, Caller, Msg} ->
            port_command(Port, encode(Msg)),
            receive
                {Port, {data, Data}} ->
                    ?LOG_DEBUG(#{what => extauth_result,
                                 extauth_call => Msg, result => Data,
                                 text => <<"extauth call received data response">>}),
                    Caller ! {eauth, decode(Data)},
                    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg);
                {Port, Other} ->
                    ?LOG_ERROR(#{what => extauth_unexpected_message,
                                 extauth_call => Msg, unexpected_message => Other,
                                 text => <<"extauth call received strange response">>}),
                    Caller ! {eauth, false},
                    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg)
            after
                Timeout ->
                    ?LOG_ERROR(#{what => extauth_timeout, extauth_call => Msg,
                                 text => <<"extauth call didn't receive response, restarting instance">>}),
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
            ?LOG_CRITICAL(#{what => extauth_crash,
                            text => <<"extauth script has exitted abruptly, restarting instance">>,
                            reason => Reason}),
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

