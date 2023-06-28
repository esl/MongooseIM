-module(hook_helper).
%% API
-export([start/4,
         receive_all/0]).

-export([handle_hook/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Register hook on a node.
%% Sends messages to the caller proceess.
%% Hook is automatically unregistered if the caller dies.
%% Example:
%% hook_helper:start(distributed_helper:mim(), filter_packet, global, 60).
%% Sends:
%% {hook_called, #{hook_name := HookName, acc := Acc, params := Params}}
start(Node, HookName, HostType, Priority) ->
    ensure_server_started(),
    gen_server:call(?MODULE, {register_hook, Node, HookName, HostType, Priority}).

receive_all() ->
    receive
        {hook_called, Map} ->
            [Map | receive_all()]
    after 0 ->
        []
    end.

handle_register_hook(Node, HookName, HostType, Priority, {ClientPid, _} = _From, State = #{mons := Mons}) ->
    HookExtra = #{dest_pid => ClientPid, hook_name => HookName},
    H = {HookName, HostType, fun ?MODULE:handle_hook/3, HookExtra, Priority},
    ok = distributed_helper:rpc(Node, gen_hook, add_handlers, [[H]]),
    MonRef = erlang:monitor(process, ClientPid),
    {reply, ok, State#{mons => maps:put(MonRef, {Node, H}, Mons)}}.

handle_hook(Acc, Params, #{hook_name := HookName, dest_pid := DestPid} = _Extra) ->
    DestPid ! {hook_called, #{hook_name => HookName, acc => Acc, params => Params}},
    {ok, Acc}.

%% We need a server to serialize all register/unregister calls to
%% avoid the race conditions.
%% The server also monitors the caller pid.
ensure_server_started() ->
    %% Will start only one server.
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    mongoose_helper:inject_module(?MODULE),
    {ok, #{mons => #{}}}.

handle_call({register_hook, Node, HookName, HostType, Priority}, From, State) ->
    handle_register_hook(Node, HookName, HostType, Priority, From, State).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, _Type, _Pid, _Info}, State = #{mons := Mons}) ->
    case maps:get(MonRef, Mons, unknown) of
        unknown ->
            ok;
        {Node, H} ->
            ok = distributed_helper:rpc(Node, gen_hook, delete_handlers, [[H]])
    end,
    {noreply, State#{mons => maps:delete(MonRef, Mons)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
