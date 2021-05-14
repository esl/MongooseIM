%% Generally, you should not call anything from this module.
%% Use mongoose_domain_api module instead.
-module(mongoose_lazy_routing).
-behaviour(gen_server).

-include("mongoose_logger.hrl").

%% API
-export([start/0, stop/0]).
-export([start_link/0]).

-export([maybe_add_domain_or_subdomain/1,
         maybe_remove_domain/2,
         maybe_remove_subdomain/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-ifdef(TEST).

%% required for unit tests
start() ->
    just_ok(gen_server:start({local, ?MODULE}, ?MODULE, [], [])).

stop() ->
    gen_server:stop(?MODULE).

-else.

start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []},
                 permanent, infinity, worker, [?MODULE]},
    just_ok(supervisor:start_child(ejabberd_sup, ChildSpec)).

%% required for integration tests
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec maybe_add_domain_or_subdomain(mongooseim:domain()) -> boolean().
maybe_add_domain_or_subdomain(Domain) ->
    %% it can be domain or subdomain name
    gen_server:call(?MODULE, {maybe_add_domain_or_subdomain, Domain}).

-spec maybe_remove_domain(mongooseim:host_type(), mongooseim:domain()) -> ok.
maybe_remove_domain(HostType, Domain) ->
    gen_server:cast(?MODULE, {maybe_remove_domain, HostType, Domain}).

-spec maybe_remove_subdomain(mongoose_subdomain_core:subdomain_info()) -> ok.
maybe_remove_subdomain(SubdomainInfo) ->
    gen_server:cast(?MODULE, {maybe_remove_subdomain, SubdomainInfo}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init(_) ->
    ets:new(?MODULE, [set, named_table, protected]),
    {ok,ok}.

handle_call({maybe_add_domain_or_subdomain, Domain}, _From, State) ->
    RetValue = handle_maybe_add_domain_or_subdomain(Domain),
    {reply, RetValue, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast({maybe_remove_domain, HostType, Domain}, State) ->
    handle_maybe_remove_domain(HostType, Domain),
    {noreply, State};
handle_cast({maybe_remove_subdomain, SubdomainInfo}, State) ->
    handle_maybe_remove_subdomain(SubdomainInfo),
    {noreply, State};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------
just_ok({ok, _}) -> ok;
just_ok(Other) -> Other.

handle_maybe_add_domain_or_subdomain(Domain) ->
    case ets:lookup(?MODULE, Domain) of
        [_] ->
            %% It's absolutely normal situation. We can receive
            %% a couple of maybe_add_domain_or_subdomain requests
            %% from different processes, so this domain can be
            %% already added.
            true;
        [] ->
            try_to_add_domain_or_subdomain(Domain)
    end.

try_to_add_domain_or_subdomain(Domain) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, HostType} ->
            add_domain(HostType, Domain),
            true;
        {error, not_found} ->
            case mongoose_domain_api:get_subdomain_info(Domain) of
                {ok, Info} ->
                    add_subdomain(Info),
                    true;
                {error, not_found} ->
                    false
            end
    end.

add_domain(HostType, Domain) ->
    case ets:insert_new(?MODULE, {Domain, HostType}) of
        true ->
            %% TODO: register IQ handlers before domain registration
            ejabberd_local:register_host(Domain);
        false ->
            %% we should never get here, but it's ok to just ignore this.
            ok
    end.

add_subdomain(#{subdomain := Subdomain, host_type := HostType,
                subdomain_pattern := SubdomainPattern,
                packet_handler := PacketHandler}) ->
    case ets:insert_new(?MODULE, {Subdomain, {HostType, SubdomainPattern}}) of
        true ->
            %% TODO: register IQ handlers before subdomain registration
            ejabberd_router:register_route(Subdomain, PacketHandler);
        false ->
            %% we should never get here, but it's ok to just ignore this.
            ok
    end.

handle_maybe_remove_domain(HostType, Domain) ->
    case ets:lookup(?MODULE, Domain) of
        [{Domain, HostType}] ->
            ejabberd_local:unregister_host(Domain),
            %% TODO: unregister IQ handlers after domain
            ets:delete(?MODULE, Domain);
        _ -> ok
    end.

handle_maybe_remove_subdomain(#{subdomain := Subdomain, host_type := HostType,
                                subdomain_pattern := SubdomainPattern}) ->
    case ets:lookup(?MODULE, Subdomain) of
        [{Domain, {HostType, SubdomainPattern}}] ->
            ejabberd_router:unregister_route(Domain),
            %% TODO: unregister IQ handlers after domain
            ets:delete(?MODULE, Domain);
        _ -> ok
    end.
