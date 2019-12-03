%%%===================================================================
%%% @doc Common listener/router for modules that use Cowboy.
%%%
%%% The 'modules' configuration option should be a list of
%%% {Host, BasePath, Module} or {Host, BasePath, Module, Opts} tuples,
%%% where a Host of "_" will match any host.
%%%
%%% A 'middlewares' configuration option may be specified to configure
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
-behavior(gen_server).
-behavior(cowboy_middleware).

%% ejabberd_listener API
-export([socket_type/0,
         start_listener/2]).

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
-export([start_cowboy/2, stop_cowboy/1]).

-include("mongoose.hrl").
-type options()  :: [any()].
-type path() :: iodata().
-type paths() :: [path()].
-type route() :: {path() | paths(), module(), options()}.
-type implemented_result() :: [route()].

-export_type([options/0]).
-export_type([path/0]).
-export_type([route/0]).
-export_type([implemented_result/0]).

-callback cowboy_router_paths(path(), options()) -> implemented_result().

-record(cowboy_state, {ref, opts = []}).

%%--------------------------------------------------------------------
%% ejabberd_listener API
%%--------------------------------------------------------------------

socket_type() ->
    independent.

start_listener({Port, IP, tcp}=Listener, Opts) ->
    Ref = ref(Listener),
    ChildSpec = {Listener, {?MODULE, start_link,
                            [#cowboy_state{ref = Ref, opts = Opts}]},
                 transient, infinity, worker, [?MODULE]},
    {ok, Pid} = supervisor:start_child(ejabberd_listeners, ChildSpec),
    TransportOpts = gen_mod:get_opt(transport_options, Opts, []),
    TransportOptsMap = maps:from_list(TransportOpts),
    TransportOptsMap2 = TransportOptsMap#{socket_opts => [{port, Port}, {ip, IP}]},
    TransportOptsMap3 = maybe_insert_max_connections(TransportOptsMap2, Opts),
    Opts2 = gen_mod:set_opt(transport_options, Opts, TransportOptsMap3),
    {ok, _} = start_cowboy(Ref, Opts2),
    {ok, Pid}.

reload_dispatch(Ref) ->
    gen_server:call(Ref, reload_dispatch).

%% @doc gen_server for handling shutdown when started via ejabberd_listener
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
    case ejabberd_config:get_local_option(cowboy_server_name) of
        undefined ->
            {ok, Req, Env};
        ServerName ->
            {ok, cowboy_req:set_resp_header(<<"server">>, ServerName, Req), Env}
    end.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

start_cowboy(Ref, Opts) ->
    {Retries, SleepTime} = gen_mod:get_opt(retries, Opts, {20, 50}),
    do_start_cowboy(Ref, Opts, Retries, SleepTime).


do_start_cowboy(Ref, Opts, 0, _) ->
    do_start_cowboy(Ref, Opts);
do_start_cowboy(Ref, Opts, Retries, SleepTime) ->
    case do_start_cowboy(Ref, Opts) of
        {error, eaddrinuse} ->
            timer:sleep(SleepTime),
            do_start_cowboy(Ref, Opts, Retries - 1, SleepTime);
        Other ->
            Other
    end.

do_start_cowboy(Ref, Opts) ->
    SSLOpts = gen_mod:get_opt(ssl, Opts, undefined),
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    TransportOpts0 = gen_mod:get_opt(transport_options, Opts, #{}),
    TransportOpts = TransportOpts0#{num_acceptros => NumAcceptors},
    Modules = gen_mod:get_opt(modules, Opts),
    Dispatch = cowboy_router:compile(get_routes(Modules)),
    ProtocolOpts = [{env, [{dispatch, Dispatch}]} |
                    gen_mod:get_opt(protocol_options, Opts, [])],
    ok = trails_store(Modules),
    case catch start_http_or_https(SSLOpts, Ref, TransportOpts, ProtocolOpts) of
        {error, {{shutdown,
                  {failed_to_start_child, ranch_acceptors_sup,
                   {{badmatch, {error, eaddrinuse}}, _ }}}, _}} ->
            {error, eaddrinuse};
        Result ->
            Result
    end.

start_http_or_https(undefined, Ref, TransportOpts, ProtocolOpts) ->
    cowboy_start_http(Ref, TransportOpts, ProtocolOpts);
start_http_or_https(SSLOpts, Ref, TransportOpts, ProtocolOpts) ->
    SSLOptsWithVerifyFun = maybe_set_verify_fun(SSLOpts),
    FilteredSSLOptions = filter_options(ignored_ssl_options(), SSLOptsWithVerifyFun),
    SocketOptsWithSSL = maps:get(socket_opts, TransportOpts) ++ FilteredSSLOptions,
    cowboy_start_https(Ref, TransportOpts#{socket_opts => SocketOptsWithSSL}, ProtocolOpts).

cowboy_start_http(Ref, TransportOpts, ProtocolOpts) ->
    ProtoOpts = add_common_middleware(make_env_map(maps:from_list(ProtocolOpts))),
    cowboy:start_clear(Ref, TransportOpts, ProtoOpts).

cowboy_start_https(Ref, TransportOpts, ProtocolOpts) ->
    ProtoOpts = add_common_middleware(make_env_map(maps:from_list(ProtocolOpts))),
    cowboy:start_tls(Ref, TransportOpts, ProtoOpts).

make_env_map(Map = #{ env := Env }) ->
    Map#{ env => maps:from_list(Env) }.

% We need to insert our middleware just before `cowboy_handler`,
% so the injected response header is taken into account.
add_common_middleware(Map = #{ middlewares := Middlewares }) ->
    {Ms1, Ms2} = lists:splitwith(fun(Middleware) -> Middleware /= cowboy_handler end, Middlewares),
    Map#{ middlewares := Ms1 ++ [?MODULE | Ms2] };
add_common_middleware(Map) ->
    Map#{ middlewares => [cowboy_router, ?MODULE, cowboy_handler] }.

reload_dispatch(Ref, Opts) ->
    Dispatch = cowboy_router:compile(get_routes(gen_mod:get_opt(modules, Opts))),
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
    ?DEBUG("Configured Cowboy Routes: ~p", [Final]),
    Final.

get_routes([], Routes) ->
    Routes;
get_routes([{Host, BasePath, Module} | Tail], Routes) ->
    get_routes([{Host, BasePath, Module, []} | Tail], Routes);
get_routes([{Host, BasePath, Module, Opts} | Tail], Routes) ->
    %% ejabberd_config tries to expand the atom '_' as a Macro, which fails.
    %% To work around that, use "_" instead and translate it to '_' here.
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

ignored_ssl_options() ->
    %% these options should be ignored if they creep into ssl section
    [port, ip, max_connections, verify_mode].

filter_options(IgnoreOpts, [Opt | Opts]) when is_atom(Opt) ->
    case lists:member(Opt, IgnoreOpts) of
        true  -> filter_options(IgnoreOpts, Opts);
        false -> [Opt | filter_options(IgnoreOpts, Opts)]
    end;
filter_options(IgnoreOpts, [Opt | Opts]) when tuple_size(Opt) >= 1 ->
    case lists:member(element(1, Opt), IgnoreOpts) of
        true  -> filter_options(IgnoreOpts, Opts);
        false -> [Opt | filter_options(IgnoreOpts, Opts)]
    end;
filter_options(_, []) ->
    [].

maybe_set_verify_fun(SSLOptions) ->
    case proplists:get_value(verify_mode, SSLOptions, undefined) of
        undefined ->
            SSLOptions;
        Mode ->
            Fun = just_tls:verify_fun(Mode),
            lists:keystore(verify_fun, 1, SSLOptions, {verify_fun, Fun})
    end.

% This functions is for backward compatibility, as previous default config
% used max_connections tuple for all ejabberd_cowboy listeners
maybe_insert_max_connections(TransportOpts, Opts) ->
    Key = max_connections,
    case gen_mod:get_opt(Key, Opts, undefined) of
        undefined ->
            TransportOpts;
        Value ->
            TransportOpts#{Key => Value}
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Store trails, this need for generate swagger documentation
%% Add to Trails each of modules where used trails behaviour
%% The modules must be added into `mongooseim.cfg`in `swagger` section
%% @end
%% -------------------------------------------------------------------
trails_store(Modules) ->
    try
        trails:store(trails:trails(collect_trails(Modules, [])))
    catch Class:Exception ->
        ?WARNING_MSG("Trails Call: [~p:~p/0] catched ~p:~p~n", [?MODULE, trails_store, Class, Exception])
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
