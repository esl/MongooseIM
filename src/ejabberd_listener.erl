%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage socket listener
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

-module(ejabberd_listener).
-author('alexey@process-one.net').

-export([start_link/0,
         init/1,
         start_listeners/0,
         start_listener/1,
         stop_listeners/0,
         stop_listener/1
        ]).

%% Internal
-export([format_error/1, socket_error/6]).

-ignore_xref([start_link/0, init/1, start_listener/1, stop_listener/1]).

-include("mongoose.hrl").

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ejabberd_listeners}, ?MODULE, []).


init(_) ->
    %bind_tcp_ports(),
    {ok, {{one_for_one, 10, 1}, []}}.


-spec start_listeners() -> 'ignore' | {'ok', {{_, _, _}, [any()]}}.
start_listeners() ->
    case mongoose_config:get_opt(listen) of
        [] ->
            ignore;
        Ls ->
            Ls2 = lists:map(
                fun(Listener) ->
                        case start_listener(Listener) of
                            {ok, _Pid} = R -> R;
                            {error, Error} ->
                                throw(Error)
                        end
                end, Ls),
            {ok, {{one_for_one, 10, 1}, Ls2}}
    end.

-spec start_listener(mongoose_listenet_config:listener()) -> {'error', pid()} | {'ok', _}.
start_listener(Opts = #{module := Module}) ->
    try
        %% It is only required to start the supervisor in some cases.
        %% But it doesn't hurt to attempt to start it for any listener.
        %% So, it's normal (and harmless) that in most cases this call returns:
        %% {error, {already_started, pid()}}
        start_module_sup(Module),
        start_listener_sup(Module, Opts)
    of
        {ok, _Pid} = R -> R;
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, Reason} = R ->
            ?LOG_CRITICAL(#{what => listener_failed_to_start, reason => Reason,
                            text => <<"Failed to start a listener">>,
                            module => Module, opts => Opts}),
            R
    catch Class:Reason:Stacktrace ->
            ?LOG_CRITICAL(#{what => listener_failed_to_start,
                            text => <<"Failed to start a listener">>,
                            module => Module, opts => Opts,
                            class => Class, reason => Reason, stacktrace => Stacktrace}),
            {error, Reason}
    end.

-spec start_module_sup(Module :: module())
      -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start_module_sup(Module) ->
    Proc = gen_mod:get_module_proc("sup", Module),
    ChildSpec =
        {Proc,
         {ejabberd_tmp_sup, start_link, [Proc, Module]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    %% TODO Rewrite using ejabberd_sup:start_child
    %% This function is called more than once
    supervisor:start_child(ejabberd_sup, ChildSpec).

-spec start_listener_sup(module(), mongoose_listener_config:listener())
      -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start_listener_sup(Module, Listener = #{ip_address := IPAddr, port := Port, proto := Proto}) ->
    ListenerId = mongoose_listener_config:listener_id(Listener),
    Opts = mongoose_listener_config:prepare_opts(Listener),
    case Module:socket_type() of
        independent ->
            Module:start_listener(ListenerId, Opts);
        _ ->
            SockOpts = mongoose_listener_config:filter_socket_opts(Opts),
            {SupModule, Kill, Type} =
                case Proto of
                    udp -> {mongoose_udp_listener, brutal_kill, worker};
                    _ -> {mongoose_tcp_listener, 1000, supervisor}
                end,
            ChildSpec = {ListenerId,
                         {SupModule, start_link,
                          [ListenerId, Module, Opts, SockOpts, Port, IPAddr]},
                         transient, Kill, Type, [SupModule]},
            supervisor:start_child(ejabberd_listeners, ChildSpec)
    end.

-spec stop_listeners() -> 'ok'.
stop_listeners() ->
    Listeners = mongoose_config:get_opt(listen),
    lists:foreach(fun stop_listener/1, Listeners).

-spec stop_listener(mongoose_listener_config:listener())
      -> 'ok' | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_listener(Listener) ->
    ListenerId = mongoose_listener_config:listener_id(Listener),
    supervisor:terminate_child(ejabberd_listeners, ListenerId),
    supervisor:delete_child(ejabberd_listeners, ListenerId).

%%%
%%% Check options
%%%

socket_error(Reason, PortIP, Module, SockOpts, Port, IPS) ->
    ReasonT = case Reason of
                  eaddrnotavail ->
                      "IP address not available: " ++ IPS;
                  eaddrinuse ->
                      "IP address and port number already used: "
                          ++IPS++" "++integer_to_list(Port);
                  _ ->
                      format_error(Reason)
              end,
    ?LOG_ERROR(#{what => failed_to_open_socket, reason => ReasonT,
                 port => Port, module => Module, socket_option => SockOpts}),
    throw({Reason, PortIP}).

-spec format_error(atom()) -> string().
format_error(Reason) ->
    case inet:format_error(Reason) of
        "unknown POSIX error" ->
            atom_to_list(Reason);
        ReasonStr ->
            ReasonStr
    end.
