%%%===================================================================
%%% @doc Common listener/router for modules that use Cowboy.
%%%
%%% The `modules' configuration option should be a list of
%%% {Host, BasePath, Module} or {Host, BasePath, Module, Opts} tuples,
%%% where a Host of "_" will match any host.
%%%
%%% A `middlewares' configuration option may be specified to configure
%%% Cowboy middlewares.
%%%
%%% Modules may export the following function to configure Cowboy
%%% routing for sub-paths:
%%% cowboy_router_paths(BasePath, Opts) ->
%%%   [{PathMatch, Handler, NewOpts}]
%%% If not implemented, [{BasePath, Module, []|Opts}] is assumed.
%%% @end
%%%===================================================================
-module(ejabberd_cowboy).
-behaviour(gen_server).
-behaviour(cowboy_middleware).
-behaviour(mongoose_listener).

%% mongoose_listener API
-export([listener_spec/1,
         instrumentation/1]).

%% cowboy_middleware API
-export([execute/2]).

%% gen_server API
-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% helper for internal use
-export([ref/1, reload_dispatch/1]).
-export([start_cowboy/4, start_cowboy/2, stop_cowboy/1]).

-ignore_xref([behaviour_info/1, process/1, ref/1, reload_dispatch/1, start_cowboy/2,
              start_cowboy/4, start_link/1, stop/0, stop_cowboy/1]).

-include("mongoose.hrl").

-record(cowboy_state, {ref :: atom(), opts :: mongoose_listener:options()}).

%%--------------------------------------------------------------------
%% mongoose_listener API
%%--------------------------------------------------------------------

-spec instrumentation(mongoose_listener:options()) -> [mongoose_instrument:spec()].
instrumentation(#{handlers := Handlers}) ->
    [Spec || #{module := Module} <- Handlers,
             Spec <- mongoose_http_handler:instrumentation(Module)].

-spec listener_spec(mongoose_listener:options()) -> supervisor:child_spec().
listener_spec(Opts) ->
    ListenerId = mongoose_listener:listener_id(Opts),
    Ref = ref(ListenerId),
    ChildSpec = #{id => ListenerId,
                  start => {?MODULE, start_link, [#cowboy_state{ref = Ref, opts = Opts}]},
                  restart => transient,
                  shutdown => infinity,
                  modules => [?MODULE]},
    ChildSpec.

reload_dispatch(Ref) ->
    gen_server:call(Ref, reload_dispatch).

%% @doc gen_server for handling shutdown when started via mongoose_listener
-spec start_link(_) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(State) ->
    gen_server:start_link(?MODULE, State, []).

init(#cowboy_state{ref = Ref, opts = Opts} = State) ->
    process_flag(trap_exit, true),
    {ok, _} = start_cowboy(Ref, Opts),
    {ok, State}.

handle_call(reload_dispatch, _From, #cowboy_state{ref = Ref, opts = Opts} = State) ->
    reload_dispatch(Ref, Opts),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    stop_cowboy(State#cowboy_state.ref).

-spec handler({integer(), inet:ip_address(), tcp}) -> list().
handler({Port, IP, tcp}) ->
    [inet_parse:ntoa(IP), <<"_">>, integer_to_list(Port)].

%%--------------------------------------------------------------------
%% cowboy_middleware callback
%%--------------------------------------------------------------------

-spec execute(cowboy_req:req(), cowboy_middleware:env()) ->
    {ok, cowboy_req:req(), cowboy_middleware:env()}.
execute(Req, Env) ->
    case mongoose_config:lookup_opt(http_server_name) of
        {error, not_found} ->
            {ok, Req, Env};
        {ok, ServerName} ->
            {ok, cowboy_req:set_resp_header(<<"server">>, ServerName, Req), Env}
    end.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

-spec start_cowboy(atom(), mongoose_listener:options()) -> {ok, pid()} | {error, any()}.
start_cowboy(Ref, Opts) ->
    start_cowboy(Ref, Opts, 20, 50).

-spec start_cowboy(atom(), mongoose_listener:options(),
                   Retries :: non_neg_integer(), SleepTime :: non_neg_integer()) ->
          {ok, pid()} | {error, any()}.
start_cowboy(Ref, Opts, 0, _) ->
    do_start_cowboy(Ref, Opts);
start_cowboy(Ref, Opts, Retries, SleepTime) ->
    case do_start_cowboy(Ref, Opts) of
        {error, eaddrinuse} ->
            timer:sleep(SleepTime),
            start_cowboy(Ref, Opts, Retries - 1, SleepTime);
        Other ->
            Other
    end.

-spec do_start_cowboy(atom(), mongoose_listener:options()) -> {ok, pid()} | {error, any()}.
do_start_cowboy(Ref, Opts) ->
    #{ip_tuple := IPTuple, port := Port, handlers := Handlers0,
      transport := TransportOpts0, protocol := ProtocolOpts0} = Opts,
    Handlers = [ Handler#{ip_tuple => IPTuple, port => Port, proto => tcp} || Handler <- Handlers0 ],
    Routes = mongoose_http_handler:get_routes(Handlers),
    Dispatch = cowboy_router:compile(Routes),
    ProtocolOpts = ProtocolOpts0#{env => #{dispatch => Dispatch}},
    TransportOpts = TransportOpts0#{socket_opts => [{port, Port}, {ip, IPTuple}]},
    store_trails(Routes),
    case catch start_http_or_https(Opts, Ref, TransportOpts, ProtocolOpts) of
        {error, {{shutdown,
                  {failed_to_start_child, ranch_acceptors_sup,
                   {{badmatch, {error, eaddrinuse}}, _ }}}, _}} ->
            {error, eaddrinuse};
        Result ->
            Result
    end.

start_http_or_https(#{tls := TLSOpts, hibernate_after := HibernateAfter},
                    Ref, TransportOpts, ProtocolOpts) ->
    SocketOpts = maps:get(socket_opts, TransportOpts),
    SSLOpts = just_tls:make_server_opts(TLSOpts),
    SocketOptsWithSSL = SocketOpts ++ SSLOpts ++ [{hibernate_after, HibernateAfter}],
    cowboy_start_https(Ref, TransportOpts#{socket_opts := SocketOptsWithSSL}, ProtocolOpts);
start_http_or_https(#{}, Ref, TransportOpts, ProtocolOpts) ->
    cowboy_start_http(Ref, TransportOpts, ProtocolOpts).

cowboy_start_http(Ref, TransportOpts, ProtocolOpts) ->
    ProtoOpts = add_common_middleware(ProtocolOpts),
    cowboy:start_clear(Ref, TransportOpts, ProtoOpts).

cowboy_start_https(Ref, TransportOpts, ProtocolOpts) ->
    ProtoOpts = add_common_middleware(ProtocolOpts),
    cowboy:start_tls(Ref, TransportOpts, ProtoOpts).

% We need to insert our middleware just before `cowboy_handler`,
% so the injected response header is taken into account.
add_common_middleware(Map = #{ middlewares := Middlewares }) ->
    {Ms1, Ms2} = lists:splitwith(fun(Middleware) -> Middleware /= cowboy_handler end, Middlewares),
    Map#{ middlewares := Ms1 ++ [?MODULE | Ms2] };
add_common_middleware(Map) ->
    Map#{ middlewares => [cowboy_router, ?MODULE, cowboy_handler] }.

reload_dispatch(Ref, #{handlers := Handlers}) ->
    Dispatch = cowboy_router:compile(mongoose_http_handler:get_routes(Handlers)),
    cowboy:set_env(Ref, dispatch, Dispatch).

stop_cowboy(Ref) ->
    cowboy:stop_listener(Ref).

ref(Listener) ->
    Ref = handler(Listener),
    ModRef = [?MODULE_STRING, <<"_">>, Ref],
    binary_to_atom(iolist_to_binary(ModRef)).

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Store trails, this is needed to generate swagger documentation.
%% Add to Trails each of modules where the trails behaviour is used.
%% The modules must be added into `mongooseim.toml' in the `swagger' section.
%% @end
%% -------------------------------------------------------------------
store_trails(Routes) ->
    AllModules = lists:usort(lists:flatmap(fun({_Host, HostRoutes}) ->
                                                   [Module || {_Path, Module, _Opts} <- HostRoutes]
                                           end, Routes)),
    TrailModules = lists:filter(fun(Module) ->
                                        mongoose_lib:is_exported(Module, trails, 0)
                                end, AllModules),
    try
        trails:store(trails:trails(TrailModules))
    catch Class:Reason:Stacktrace ->
              ?LOG_WARNING(#{what => store_trails_failed,
                             class => Class, reason => Reason, stacktrace => Stacktrace})
    end.
