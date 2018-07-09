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

%% ejabberd_listener API
-export([socket_type/0,
         start_listener/2]).

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
    TransportOpts2 = [{port, Port}, {ip, IP} | TransportOpts],
    TransportOpts3 = maybe_insert_max_connections(TransportOpts2, Opts),
    Opts2 = gen_mod:set_opt(transport_options, Opts, TransportOpts3),
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
    TransportOpts = gen_mod:get_opt(transport_options, Opts, []),
    Modules = gen_mod:get_opt(modules, Opts),
    Dispatch = cowboy_router:compile(get_routes(Modules)),
    {MetricsEnv, MetricsProtoOpts} = maybe_init_metrics(Opts),
    ProtocolOpts = [{env, [{dispatch, Dispatch} | MetricsEnv]} |
                    gen_mod:get_opt(protocol_options, Opts, [])] ++ MetricsProtoOpts,
    case catch start_http_or_https(SSLOpts, Ref, NumAcceptors, TransportOpts, ProtocolOpts) of
        {error, {{shutdown,
                  {failed_to_start_child, ranch_acceptors_sup,
                   {{badmatch, {error, eaddrinuse}}, _ }}}, _}} ->
            {error, eaddrinuse};
        Result ->
            Result
    end.

start_http_or_https(undefined, Ref, NumAcceptors, TransportOpts, ProtocolOpts) ->
    cowboy:start_http(Ref, NumAcceptors, TransportOpts, ProtocolOpts);
start_http_or_https(SSLOpts, Ref, NumAcceptors, TransportOpts, ProtocolOpts) ->
    FilteredSSLOptions = filter_options(ignored_ssl_options(), SSLOpts),
    TransportOptsWithSSL = TransportOpts ++ FilteredSSLOptions,
    cowboy:start_https(Ref, NumAcceptors, TransportOptsWithSSL, ProtocolOpts).

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
get_routes([{Host, BasePath, Module, HandlerOpts} | Tail], Routes) ->
    get_routes([{Host, BasePath, Module, HandlerOpts, []} | Tail], Routes);
get_routes([{Host, BasePath, Module, HandlerOpts, _Opts} | Tail], Routes) ->
    %% ejabberd_config tries to expand the atom '_' as a Macro, which fails.
    %% To work around that, use "_" instead and translate it to '_' here.
    CowboyHost = case Host of
        "_" -> '_';
        _ -> Host
    end,
    {module, Module} = code:ensure_loaded(Module),
    Paths = proplists:get_value(CowboyHost, Routes, []) ++
    case erlang:function_exported(Module, cowboy_router_paths, 2) of
        true -> Module:cowboy_router_paths(BasePath, HandlerOpts);
        _ -> [{BasePath, Module, HandlerOpts}]
    end,
    get_routes(Tail, lists:keystore(CowboyHost, 1, Routes, {CowboyHost, Paths})).

ignored_ssl_options() ->
    %% these options are specified in the listener section
    %% and should be ignored if they creep into ssl section
    [port, ip, max_connections].

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

% This functions is for backward compatibility, as previous default config
% used max_connections tuple for all ejabberd_cowboy listeners
maybe_insert_max_connections(TransportOpts, Opts) ->
    Key = max_connections,
    case gen_mod:get_opt(Key, Opts, undefined) of
        undefined ->
            TransportOpts;
        Value ->
            NewTuple = {Key, Value},
            lists:keystore(Key, 1, TransportOpts, NewTuple)
    end.

-spec measured_methods() -> [mongoose_cowboy_metrics:method()].
measured_methods() ->
    [<<"GET">>,
     <<"HEAD">>,
     <<"POST">>,
     <<"PUT">>,
     <<"DELETE">>,
     <<"OPTIONS">>,
     <<"PATCH">>].

-spec measured_classes() -> [mongoose_cowboy_metrics:status_class()].
measured_classes() ->
    [<<"1XX">>, <<"2XX">>, <<"3XX">>, <<"4XX">>, <<"5XX">>].

base_metrics_prefix() ->
    [http].

middlewares_with_metrics() ->
    [mongoose_cowboy_metrics_mw_before,
     cowboy_router,
     mongoose_cowboy_metrics_mw_after,
     cowboy_handler].

-spec maybe_init_metrics(list()) -> {MetricsEnv :: list(), MetricsProtocolOpts :: list()}.
maybe_init_metrics(Opts) ->
    case proplists:get_value(metrics, Opts, false) of
        true ->
            BasePrefix = base_metrics_prefix(),
            HandlerToPrefixMappings = build_metric_prefixes(
                                          BasePrefix, proplists:get_value(modules, Opts, []), #{}),
            [create_metrics(Prefix) || Prefix <- maps:values(HandlerToPrefixMappings)],
            {[{record_metrics, true}, {handler_to_metric_prefix, HandlerToPrefixMappings}],
             [{middlewares, middlewares_with_metrics()}]};
        false ->
            {[], []}
    end.

-spec build_metric_prefixes(BasePrefix :: list(), Modules :: [tuple()], Acc) -> Acc
    when Acc :: #{module() => mongoose_cowboy_metrics:prefix()}.
build_metric_prefixes(_BasePrefix, [], Acc) ->
    Acc;
build_metric_prefixes(BasePrefix, [{_Host, _Path, Handler, _HandlerOpts, Opts} | Tail], Acc) ->
    case proplists:get_value(metrics, Opts, []) of
        MetricsOpts when is_list(MetricsOpts) ->
            HandlerPrefix = proplists:get_value(prefix, MetricsOpts, Handler),
            Prefix = BasePrefix ++ lists:flatten([HandlerPrefix]),
            build_metric_prefixes(BasePrefix, Tail, maps:put(Handler, Prefix, Acc));
        false ->
            build_metric_prefixes(BasePrefix, Tail, Acc)
    end;
build_metric_prefixes(BasePrefix, [{Host, Path, Handler, HandlerOpts} | Tail], Acc) ->
    build_metric_prefixes(BasePrefix, [{Host, Path, Handler, HandlerOpts, []} | Tail], Acc);
build_metric_prefixes(BasePrefix, [{Host, Path, Handler} | Tail], Acc) ->
    build_metric_prefixes(BasePrefix, [{Host, Path, Handler, [], []} | Tail], Acc).

-spec create_metrics(mongoose_cowboy_metrics:prefix()) -> any().
create_metrics(Prefix) ->
    CountMetrics = [mongoose_cowboy_metrics:request_count_metric(Prefix, M) || M <- measured_methods()] ++
                   [mongoose_cowboy_metrics:response_count_metric(Prefix, M, C)
                    || M <- measured_methods(), C <- measured_classes()],
    LatencyMetrics = [mongoose_cowboy_metrics:response_latency_metric(Prefix, M, C)
                      || M <- measured_methods(), C <- measured_classes()],
    [mongoose_metrics:ensure_metric(global, M, spiral) || M <- CountMetrics],
    [mongoose_metrics:ensure_metric(global, M, histogram) || M <- LatencyMetrics].
