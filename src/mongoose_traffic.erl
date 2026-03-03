-module(mongoose_traffic).

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([register/1]).
%% gen_mod API
-export([start/2, stop/1]).
-export([supported_features/0]).
%% hook handler
-export([trace_traffic/3]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).


start(HostType, _Opts) ->
    gen_hook:add_handlers(hooks(HostType)),
    case whereis(?MODULE) of
        undefined ->
            Traffic = {mongoose_traffic,
                         {gen_server, start_link, [?MODULE, [], []]},
                         permanent, 1000, supervisor, [?MODULE]},
            ejabberd_sup:start_child(Traffic);
        _ ->
            ok
    end,
    ok.

stop(Host) ->
    gen_hook:delete_handlers(hooks(Host)),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

hooks(_HostType) ->
    [{c2s_debug, global, fun ?MODULE:trace_traffic/3, #{}, 50}].

supported_features() -> [dynamic_domains].

register(Limit) ->
    Pid = self(),
    gen_server:call(?MODULE, {register, {Pid, Limit}}).

trace_traffic(Acc, #{arg := {client_to_server, From, El}}, _) ->
    traffic(client_to_server, From, El),
    {ok, Acc};
trace_traffic(Acc, #{arg := {server_to_client, To, El}}, _) ->
    traffic(server_to_client, To, El),
    {ok, Acc}.

traffic(Dir, Jid, El) ->
    St = iolist_to_binary(fix_and_format(El)),
    UserPid = self(),
    gen_server:cast(?MODULE, {message, Dir, {UserPid, Jid}, St}),
    ok.


init([]) ->
    register(?MODULE, self()),
    {ok, #{}}.

handle_call({register, {Pid, Limit}}, _From, State) ->
    monitor(process, Pid),
    {reply, ok, register_pid(Pid, Limit, State)};
handle_call({unregister, Pid}, _From, State) ->
    {reply, ok, unregister_pid(Pid, State)};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({message, _, _, _} = Msg, State) ->
    State1 = maps:fold(fun(Pid, Limit, NewState) ->
                           send_and_count(Msg, Pid, Limit, NewState)
                       end, #{},
                       State),
    {noreply, State1}.

send_and_count(_Msg, Pid, 0, NewState) ->
    Pid ! limit_exceeded,
    NewState;
send_and_count(Msg, Pid, Limit, NewState) ->
    Pid ! Msg,
    NewState#{Pid => Limit - 1}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
    {noreply, unregister_pid(Pid, State)}.

fix_and_format(El) when is_binary(El) ->
    El;
fix_and_format({xmlstreamend, _}) ->
    <<"</stream:stream>">>;
fix_and_format({Tag, Name}) ->
    exml:to_pretty_iolist({Tag, Name, #{}});
fix_and_format({Tag, Name, Attrs}) ->
    exml:to_pretty_iolist({Tag, Name, fix_attrs(Attrs)});
fix_and_format({Tag, Name, Attrs, Children}) ->
    exml:to_pretty_iolist({Tag, Name, fix_attrs(Attrs), Children}).

register_pid(Pid, Limit, State) ->
    State#{Pid => Limit}.

unregister_pid(Pid, State) ->
    maps:remove(Pid, State).

fix_attrs(Attrs) when is_map(Attrs) ->
    maps:filter(fun is_defined/2, Attrs).

is_defined(_, undefined) -> false;
is_defined(_, _) -> true.
