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
-behaviour(gen_mod).
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

%% gen_mod API
-export([start/2,
         stop/1]).

%% helper for internal use
-export([handler/1]).

-include("ejabberd.hrl").

%%--------------------------------------------------------------------
%% ejabberd_listener API
%%--------------------------------------------------------------------

socket_type() ->
    independent.

start_listener({Port, IP, tcp}=Listener, Opts) ->
    IPPort = handler(Listener),
    ChildSpec = {Listener, {?MODULE, start_link, [IPPort]}, transient,
                 infinity, worker, [?MODULE]},
    {ok, Pid} = supervisor:start_child(ejabberd_listeners, ChildSpec),
    {ok, _} = start_cowboy(IPPort, [{port, Port}, {ip, IP} | Opts]),
    {ok, Pid}.

%% @doc gen_server for handling shutdown when started via ejabberd_listener
-spec start_link(_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Ref) ->
    gen_server:start_link(?MODULE, [Ref], []).
init(Ref) ->
    process_flag(trap_exit, true),
    {ok, Ref}.
handle_call(_Request, _From, Ref) ->
    {noreply, Ref}.
handle_cast(_Request, Ref) ->
    {noreply, Ref}.
handle_info(_Info, Ref) ->
    {noreply, Ref}.
code_change(_OldVsn, Ref, _Extra) ->
    {ok, Ref}.
terminate(_Reason, Ref) ->
    stop_cowboy(Ref).

-spec handler({integer(), inet:ip_address(), tcp}) -> list().
handler({Port, IP, tcp}) ->
    [inet_parse:ntoa(IP), <<"_">>, integer_to_list(Port)].

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

start(Host, Opts) ->
    start_cowboy(Host, Opts).

stop(Host) ->
    stop_cowboy(Host).

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

start_cowboy(Ref, Opts) ->
    %% Port and Dispatch are required
    Port = gen_mod:get_opt(port, Opts),
    IP = gen_mod:get_opt(ip, Opts, {0,0,0,0}),
    SSLCert = gen_mod:get_opt(cert, Opts, undefined),
    SSLKey = gen_mod:get_opt(key, Opts, undefined),
    SSLKeyPass = gen_mod:get_opt(key_pass, Opts, ""),
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    MaxConns = gen_mod:get_opt(max_connections, Opts, 1024),
    Middlewares = case gen_mod:get_opt(middlewares, Opts, undefined) of
        undefined -> [];
        M -> [{middlewares, M}]
    end,
    Dispatch = cowboy_router:compile(get_routes(gen_mod:get_opt(modules, Opts))),
    case {SSLCert, SSLKey} of
        {undefined, undefined} ->
            cowboy:start_http(cowboy_ref(Ref), NumAcceptors,
                              [{port, Port}, {ip, IP}, {max_connections, MaxConns}],
                              [{env, [{dispatch, Dispatch}]} | Middlewares]);
        _ ->
            cowboy:start_https(cowboy_ref(Ref), NumAcceptors,
                               [{port, Port}, {ip, IP}, {max_connections, MaxConns},
                                {certfile, SSLCert}, {keyfile, SSLKey},
                                {password, SSLKeyPass}],
                               [{env, [{dispatch, Dispatch}]} | Middlewares])
    end.

stop_cowboy(Ref) ->
    cowboy:stop_listener(cowboy_ref(Ref)),
    ok.


cowboy_ref(Ref) ->
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
    {module, Module} = code:ensure_loaded(Module),
    Paths = proplists:get_value(CowboyHost, Routes, []) ++
    case erlang:function_exported(Module, cowboy_router_paths, 2) of
        true -> Module:cowboy_router_paths(BasePath, Opts);
        _ -> [{BasePath, Module, Opts}]
    end,
    get_routes(Tail, lists:keystore(CowboyHost, 1, Routes, {CowboyHost, Paths})).
