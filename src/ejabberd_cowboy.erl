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
-export([socket_type/0,
         start_listener/1]).

%% cowboy_middleware API
-export([execute/2]).

%% gen_server API
-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% helper for internal use
-export([ref/1, reload_dispatch/1]).
-export([start_cowboy/4, start_cowboy/2, stop_cowboy/1]).

-ignore_xref([behaviour_info/1, process/1, ref/1, socket_type/0, start_cowboy/2,
              start_cowboy/4, start_link/1, start_listener/2, start_listener/1, stop_cowboy/1]).

-include("mongoose.hrl").
-type options() :: [any()].
-type path() :: binary().
-type paths() :: [path()].
-type route() :: {path() | paths(), module(), options()}.
-type implemented_result() :: [route()].

-type listener_options() :: #{port := inet:port_number(),
                              ip_tuple := inet:ip_address(),
                              ip_address := string(),
                              ip_version := 4 | 6,
                              proto := tcp,
                              handlers := list(),
                              transport := ranch:opts(),
                              protocol := cowboy:opts(),
                              atom() => any()}.

-export_type([options/0]).
-export_type([path/0]).
-export_type([route/0]).
-export_type([implemented_result/0]).

-callback cowboy_router_paths(path(), options()) -> implemented_result().

-record(cowboy_state, {ref :: atom(), opts :: listener_options()}).

%%--------------------------------------------------------------------
%% mongoose_listener API
%%--------------------------------------------------------------------

-spec socket_type() -> mongoose_listener:socket_type().
socket_type() ->
    independent.

-spec start_listener(listener_options()) -> ok.
start_listener(Opts = #{proto := tcp}) ->
    ListenerId = mongoose_listener_config:listener_id(Opts),
    Ref = ref(ListenerId),
    ChildSpec = #{id => ListenerId,
                  start => {?MODULE, start_link, [#cowboy_state{ref = Ref, opts = Opts}]},
                  restart => transient,
                  shutdown => infinity,
                  modules => [?MODULE]},
    mongoose_listener_sup:start_child(ChildSpec),
    {ok, _} = start_cowboy(Ref, Opts),
    ok.

reload_dispatch(Ref) ->
    gen_server:call(Ref, reload_dispatch).

%% @doc gen_server for handling shutdown when started via mongoose_listener
-spec start_link(_) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(State) ->
    gen_server:start_link(?MODULE, State, []).

init(State) ->
    process_flag(trap_exit, true),
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

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    case mongoose_config:lookup_opt(cowboy_server_name) of
        {error, not_found} ->
            {ok, Req, Env};
        {ok, ServerName} ->
            {ok, cowboy_req:set_resp_header(<<"server">>, ServerName, Req), Env}
    end.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

-spec start_cowboy(atom(), listener_options()) -> {ok, pid()} | {error, any()}.
start_cowboy(Ref, Opts) ->
    start_cowboy(Ref, Opts, 20, 50).

-spec start_cowboy(atom(), listener_options(),
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

-spec do_start_cowboy(atom(), listener_options()) -> {ok, pid()} | {error, any()}.
do_start_cowboy(Ref, Opts) ->
    #{ip_tuple := IPTuple, port := Port, handlers := Modules,
      transport := TransportOpts0, protocol := ProtocolOpts0} = Opts,
    Dispatch = cowboy_router:compile(get_routes(Modules)),
    ProtocolOpts = ProtocolOpts0#{env => #{dispatch => Dispatch}},
    TransportOpts = TransportOpts0#{socket_opts => [{port, Port}, {ip, IPTuple}]},
    ok = trails_store(Modules),
    case catch start_http_or_https(Opts, Ref, TransportOpts, ProtocolOpts) of
        {error, {{shutdown,
                  {failed_to_start_child, ranch_acceptors_sup,
                   {{badmatch, {error, eaddrinuse}}, _ }}}, _}} ->
            {error, eaddrinuse};
        Result ->
            Result
    end.

start_http_or_https(#{tls := SSLOpts}, Ref, TransportOpts, ProtocolOpts) ->
    SSLOptsWithVerifyFun = maybe_set_verify_fun(SSLOpts),
    SocketOptsWithSSL = maps:get(socket_opts, TransportOpts) ++ SSLOptsWithVerifyFun,
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

reload_dispatch(Ref, #{handlers := Modules}) ->
    Dispatch = cowboy_router:compile(get_routes(Modules)),
    cowboy:set_env(Ref, dispatch, Dispatch).

stop_cowboy(Ref) ->
    cowboy:stop_listener(Ref).


ref(Listener) ->
    Ref = handler(Listener),
    ModRef = [?MODULE_STRING, <<"_">>, Ref],
    list_to_atom(binary_to_list(iolist_to_binary(ModRef))).

%% @doc Cowboy will search for a matching Host, then for a matching Path.  If no
%% Path matches, Cowboy will not search for another matching Host.  So, we must
%% merge all Paths for each Host, add any wildcard Paths to each Host, and
%% ensure that the wildcard Host is listed last.  A dict would be slightly
%% easier to use here, but a proplist ensures that the user can influence Host
%% ordering if other wildcards like "[...]" are used.
get_routes(Modules) ->
    Routes = get_routes(Modules, []),
    WildcardPaths = proplists:get_value('_', Routes, []),
    Merge = fun(Paths) -> Paths ++ WildcardPaths end,
    Merged = lists:keymap(Merge, 2, proplists:delete('_', Routes)),
    Final = Merged ++ [{'_', WildcardPaths}],
    ?LOG_DEBUG(#{what => configured_cowboy_routes, routes => Final}),
    Final.

get_routes([], Routes) ->
    Routes;
get_routes([{Host, BasePath, Module} | Tail], Routes) ->
    get_routes([{Host, BasePath, Module, []} | Tail], Routes);
get_routes([{Host, BasePath, Module, Opts} | Tail], Routes) ->
    %% "_" is used in TOML and translated to '_' here.
    CowboyHost = case Host of
        "_" -> '_';
        _ -> Host
    end,
    ensure_loaded_module(Module),
    Paths = proplists:get_value(CowboyHost, Routes, []) ++
    case erlang:function_exported(Module, cowboy_router_paths, 2) of
        true -> Module:cowboy_router_paths(BasePath, Opts);
        _ -> [{BasePath, Module, Opts}]
    end,
    get_routes(Tail, lists:keystore(CowboyHost, 1, Routes, {CowboyHost, Paths})).

ensure_loaded_module(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            ok;
        Other ->
            erlang:error(#{issue => ensure_loaded_module_failed,
                           modue => Module,
                           reason => Other})
    end.

maybe_set_verify_fun(SSLOptions) ->
    case lists:keytake(verify_mode, 1, SSLOptions) of
        false ->
            SSLOptions;
        {value, {_, Mode}, SSLOptions1} ->
            Fun = just_tls:verify_fun(Mode),
            [{verify_fun, Fun} | SSLOptions1]
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Store trails, this is needed to generate swagger documentation.
%% Add to Trails each of modules where the trails behaviour is used.
%% The modules must be added into `mongooseim.toml' in the `swagger' section.
%% @end
%% -------------------------------------------------------------------
trails_store(Modules) ->
    try
        trails:store(trails:trails(collect_trails(Modules, [])))
    catch Class:Reason ->
              ?LOG_WARNING(#{what => caught_exception, class => Class, reason => Reason})
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper of store trails for collect trails modules
%% @end
%% -------------------------------------------------------------------
collect_trails([], Acc) ->
    Acc;
collect_trails([{Host, BasePath, Module} | T], Acc) ->
    collect_trails([{Host, BasePath, Module, []} | T], Acc);
collect_trails([{_, _, Module, _} | T], Acc) ->
    case erlang:function_exported(Module, trails, 0) of
      true ->
          collect_trails(T, [Module | Acc]);
      _ ->
          collect_trails(T, Acc)
  end.
