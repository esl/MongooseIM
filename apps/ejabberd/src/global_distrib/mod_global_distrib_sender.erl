-module(mod_global_distrib_sender).

-behaviour(gen_mod).
-behaviour(supervisor).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, send/2, start_link/0, init/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts) ->
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []}, transient, 1000, supervisor, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop() ->
    ok.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

send(Server, {_From, _To, _Acc} = Packet) ->
    Pid = get_process_for(Server),
    BinPacket = term_to_binary(Packet),
    ok = mod_global_distrib_utils:cast_or_call(gen_server, Pid, {data, BinPacket}).

get_process_for(Server) ->
    Name = server_to_atom(Server),
    case whereis(Name) of
        Pid when is_pid(Pid) -> Pid;
        undefined ->
            case supervisor:start_child(?MODULE, [Name, Server]) of
                {ok, Pid} -> Pid;
                {error, {already_started, Pid}} -> Pid
            end
    end.

init(_) ->
    Module = mod_global_distrib_sender_worker,
    Child = {Module, {Module, start_link, []}, temporary, 5000, worker, [Module]},
    {ok, {{simple_one_for_one, 1000000, 1}, [Child]}}.

server_to_atom(Server) ->
    binary_to_atom(base64:encode(Server), latin1).
