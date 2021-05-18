%% Generally, you should not call anything from this module directly.
-module(mongoose_lazy_routing).
-behaviour(gen_server).

-include("mongoose_logger.hrl").

%% start/stop API
-export([start/0, stop/0]).
-export([start_link/0]).

%% domain/subdomain API
-export([maybe_add_domain_or_subdomain/1,
         maybe_remove_domain/2,
         maybe_remove_subdomain/1]).

%% IQ handling API
-export([register_iq_handler_for_domain/4,
         register_iq_handler_for_subdomain/5,
         unregister_iq_handler_for_domain/3,
         unregister_iq_handler_for_subdomain/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(IQ_TABLE, mongoose_lazy_routing_iqs).
-define(ROUTING_TABLE, mongoose_lazy_routing).

-record(iq_table_key, {host_type :: host_type(),
                       %% subdomain_pattern is 'undefined' for domains
                       subdomain_pattern :: subdomain_pattern() | undefined,
                       namespace = '_' :: binary() | '_',
                       component = '_' :: module() | '_'}).

-type subdomain_pattern() :: mongoose_subdomain_utils:subdomain_pattern().
-type subdomain_info() :: mongoose_subdomain_core:subdomain_info().
-type host_type() :: mongooseim:host_type().
-type domain() :: mongooseim:domain_name().

-type iq_record() :: {#iq_table_key{}, mongoose_iq_handler:t()}.
-type domain_record() :: {domain(), host_type() | {host_type(), subdomain_pattern()}}.


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

-spec maybe_remove_subdomain(subdomain_info()) -> ok.
maybe_remove_subdomain(SubdomainInfo) ->
    gen_server:cast(?MODULE, {maybe_remove_subdomain, SubdomainInfo}).

-spec register_iq_handler_for_domain(HostType :: mongooseim:host_type(),
                                     Namespace :: binary(),
                                     Component :: module(),
                                     IQHandler :: mongoose_iq_handler:t()) ->
    ok | {error, atom()}.
register_iq_handler_for_domain(HostType, Namespace, Component, IQHandler) ->
    gen_server:call(?MODULE, {register_iq_handler_for_domain, HostType,
                              Namespace, Component, IQHandler}).

-spec register_iq_handler_for_subdomain(HostType :: mongooseim:host_type(),
                                        SubdomainPattern :: subdomain_pattern(),
                                        Namespace :: binary(),
                                        Component :: module(),
                                        IQHandler :: mongoose_iq_handler:t()) ->
    ok | {error, atom()}.
register_iq_handler_for_subdomain(HostType, SubdomainPattern,
                                  Namespace, Component, IQHandler) ->
    gen_server:call(?MODULE, {register_iq_handler_for_subdomain, HostType,
                              SubdomainPattern, Namespace, Component, IQHandler}).

-spec unregister_iq_handler_for_domain(HostType :: mongooseim:host_type(),
                                       Namespace :: binary(),
                                       Component :: module()) ->
    {ok, mongoose_iq_handler:t()} | {error, not_found}.
unregister_iq_handler_for_domain(HostType, Namespace, Component) ->
    gen_server:call(?MODULE, {unregister_iq_handler_for_domain,
                              HostType, Namespace, Component}).

-spec unregister_iq_handler_for_subdomain(HostType :: mongooseim:host_type(),
                                          SubdomainPattern :: subdomain_pattern(),
                                          Namespace :: binary(),
                                          Component :: module()) ->
    {ok, mongoose_iq_handler:t()} | {error, not_found}.
unregister_iq_handler_for_subdomain(HostType, SubdomainPattern,
                                    Namespace, Component) ->
    gen_server:call(?MODULE, {unregister_iq_handler_for_subdomain, HostType,
                              SubdomainPattern, Namespace, Component}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init(_) ->
    %% ?IQ_TABLE contains tuples of following format:
    %%    * {#iq_table_key{}, IQHandler}
    ets:new(?IQ_TABLE, [set, named_table, protected]),
    %% ?ROUTING_TABLE contains tuples of one of the following formats:
    %%    * {DomainName, HostType}
    %%    * {SubdomainName, {HostType, SubdomainPattern}}
    ets:new(?ROUTING_TABLE, [set, named_table, protected]),
    %% no state, all required data is stored in ETS tables
    {ok, ok}.

handle_call({maybe_add_domain_or_subdomain, Domain}, _From, State) ->
    RetValue = handle_maybe_add_domain_or_subdomain(Domain),
    {reply, RetValue, State};
handle_call({register_iq_handler_for_domain,
             HostType, Namespace, Component, IQHandler},
            _From, State) ->
    RetValue = handle_register_iq_handler_for_domain(HostType, Namespace,
                                                     Component, IQHandler),
    {reply, RetValue, State};
handle_call({register_iq_handler_for_subdomain, HostType,
             SubdomainPattern, Namespace, Component, IQHandler},
            _From, State) ->
    RetValue = handle_register_iq_handler_for_subdomain(HostType, SubdomainPattern,
                                                        Namespace, Component, IQHandler),
    {reply, RetValue, State};
handle_call({unregister_iq_handler_for_domain, HostType, Namespace, Component},
            _From, State) ->
    RetValue = handle_unregister_iq_handler_for_domain(HostType, Namespace, Component),
    {reply, RetValue, State};
handle_call({unregister_iq_handler_for_subdomain, HostType, SubdomainPattern,
             Namespace, Component},
            _From, State) ->
    RetValue = handle_unregister_iq_handler_for_subdomain(HostType, SubdomainPattern,
                                                          Namespace, Component),
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

-spec handle_register_iq_handler_for_domain(HostType :: mongooseim:host_type(),
                                            Namespace :: binary(),
                                            Component :: module(),
                                            IQHandler :: mongoose_iq_handler:t()) ->
    ok | {error, atom()}.
handle_register_iq_handler_for_domain(HostType, Namespace, Component, IQHandler) ->
    IQKey = #iq_table_key{host_type = HostType, namespace = Namespace,
                          component = Component},
    NewIQHandler = mongoose_iq_handler:add_extra(IQHandler, #{host_type => HostType}),
    IQ = {IQKey, NewIQHandler},
    case ets:insert_new(?IQ_TABLE, IQ) of
        false -> {error, already_registered};
        true ->
            Domains = ets:match_object(?ROUTING_TABLE, {'_', HostType}),
            register_iqs([IQ], Domains)
    end.

-spec handle_register_iq_handler_for_subdomain(HostType :: mongooseim:host_type(),
                                               SubdomainPattern :: subdomain_pattern(),
                                               Namespace :: binary(),
                                               Component :: module(),
                                               IQHandler :: mongoose_iq_handler:t()) ->
    ok | {error, atom()}.
handle_register_iq_handler_for_subdomain(HostType, SubdomainPattern, Namespace,
                                         Component, IQHandler) ->
    IQKey = #iq_table_key{host_type = HostType, subdomain_pattern = SubdomainPattern,
                          namespace = Namespace, component = Component},
    NewIQHandler = mongoose_iq_handler:add_extra(IQHandler, #{host_type => HostType}),
    IQ = {IQKey, NewIQHandler},
    case ets:insert_new(?IQ_TABLE, IQ) of
        false -> {error, already_registered};
        true ->
            Domains = ets:match_object(?ROUTING_TABLE,
                                       {'_', {HostType, SubdomainPattern}}),
            register_iqs([IQ], Domains)
    end.

-spec handle_unregister_iq_handler_for_domain(HostType :: mongooseim:host_type(),
                                              Namespace :: binary(),
                                              Component :: module()) ->
    {ok, mongoose_iq_handler:t()} | {error, not_found}.
handle_unregister_iq_handler_for_domain(HostType, Namespace, Component) ->
    IQKey = #iq_table_key{host_type = HostType, namespace = Namespace,
                          component = Component},
    case ets:lookup(?IQ_TABLE, IQKey) of
        [] -> {error, not_found};
        [{_, IQHandler} = IQ] ->
            Domains = ets:match_object(?ROUTING_TABLE, {'_', HostType}),
            unregister_iqs([IQ], Domains),
            {ok, IQHandler}
    end.

-spec handle_unregister_iq_handler_for_subdomain(HostType :: mongooseim:host_type(),
                                                 SubdomainPattern :: subdomain_pattern(),
                                                 Namespace :: binary(),
                                                 Component :: module()) ->
    {ok, mongoose_iq_handler:t()} | {error, not_found}.
handle_unregister_iq_handler_for_subdomain(HostType, SubdomainPattern,
                                           Namespace, Component) ->
    IQKey = #iq_table_key{host_type = HostType, subdomain_pattern = SubdomainPattern,
                          namespace = Namespace, component = Component},
    case ets:lookup(?IQ_TABLE, IQKey) of
        [] -> {error, not_found};
        [{_, IQHandler} = IQ] ->
            Domains = ets:match_object(?ROUTING_TABLE, {'_', HostType}),
            unregister_iqs([IQ], Domains),
            {ok, IQHandler}
    end.

-spec handle_maybe_add_domain_or_subdomain(domain()) -> boolean().
handle_maybe_add_domain_or_subdomain(Domain) ->
    case ets:member(?ROUTING_TABLE, Domain) of
        true ->
            %% It's absolutely normal situation. We can receive
            %% a couple of maybe_add_domain_or_subdomain requests
            %% from different processes, so this domain can be
            %% already added.
            true;
        false ->
            try_to_add_domain_or_subdomain(Domain)
    end.

-spec try_to_add_domain_or_subdomain(domain()) -> boolean().
try_to_add_domain_or_subdomain(Domain) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
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

-spec add_domain(host_type(), domain()) -> ok.
add_domain(HostType, Domain) ->
    DomainElement = {Domain, HostType},
    case ets:insert_new(?ROUTING_TABLE, DomainElement) of
        true ->
            IQs = get_iqs(#iq_table_key{host_type = HostType}),
            register_iqs(IQs, [DomainElement]),
            ejabberd_local:register_host(Domain),
            ok;
        false ->
            %% we should never get here, but it's ok to just ignore this.
            ok
    end.

-spec add_subdomain(subdomain_info()) -> ok.
add_subdomain(#{subdomain := Subdomain, host_type := HostType,
                subdomain_pattern := SubdomainPattern,
                packet_handler := PacketHandler}) ->
    SubdomainElement = {Subdomain, {HostType, SubdomainPattern}},
    case ets:insert_new(?ROUTING_TABLE, SubdomainElement) of
        true ->
            IQs = get_iqs(#iq_table_key{host_type = HostType,
                                        subdomain_pattern = SubdomainPattern}),
            register_iqs(IQs, [SubdomainElement]),
            ejabberd_router:register_route(Subdomain, PacketHandler);
        false ->
            %% we should never get here, but it's ok to just ignore this.
            ok
    end.

-spec handle_maybe_remove_domain(host_type(), domain()) -> ok.
handle_maybe_remove_domain(HostType, Domain) ->
    case ets:lookup(?ROUTING_TABLE, Domain) of
        [{Domain, HostType} = DomainElement] ->
            ejabberd_local:unregister_host(Domain),
            IQs = get_iqs(#iq_table_key{host_type = HostType}),
            unregister_iqs(IQs, [DomainElement]),
            ets:delete(?ROUTING_TABLE, Domain);
        _ -> ok
    end.

-spec handle_maybe_remove_subdomain(subdomain_info()) -> ok.
handle_maybe_remove_subdomain(#{subdomain := Subdomain, host_type := HostType,
                                subdomain_pattern := SubdomainPattern}) ->
    case ets:lookup(?ROUTING_TABLE, Subdomain) of
        [{Domain, {HostType, SubdomainPattern}} = SubdomainElement] ->
            ejabberd_router:unregister_route(Domain),
            IQs = get_iqs(#iq_table_key{host_type = HostType,
                                        subdomain_pattern = SubdomainPattern}),
            unregister_iqs(IQs, [SubdomainElement]),
            ets:delete(?ROUTING_TABLE, Domain);
        _ -> ok
    end.

-spec get_iqs(#iq_table_key{}) -> [iq_record()].
get_iqs(KeyMatchPattern) ->
    ets:match_object(?IQ_TABLE, {KeyMatchPattern, '_'}).

-spec register_iqs([iq_record()], [domain_record()]) -> ok.
register_iqs(IQList, DomainList) ->
    [register_iq(IQ, Domain) || IQ <- IQList, Domain <- DomainList,
                                check_that_domain_and_iq_match(IQ, Domain)],
    ok.

-spec unregister_iqs([iq_record()], [domain_record()]) -> ok.
unregister_iqs(IQList, DomainList) ->
    [unregister_iq(IQ, Domain) || IQ <- IQList, Domain <- DomainList,
                                  check_that_domain_and_iq_match(IQ, Domain)],
    ok.

-spec check_that_domain_and_iq_match(iq_record(), domain_record()) -> boolean().
check_that_domain_and_iq_match({#iq_table_key{host_type = HostType,
                                              subdomain_pattern = undefined,
                                              namespace = Namespace,
                                              component = Component}, _},
                               {_, HostType}) when is_binary(Namespace),
                                                   Component =/= '_' ->
    true;
check_that_domain_and_iq_match({#iq_table_key{host_type = HostType,
                                              subdomain_pattern = Pattern,
                                              namespace = Namespace,
                                              component = Component}, _},
                               {_, {HostType, Pattern}}) when is_binary(Namespace),
                                                              Component =/= '_' ->
    true;
check_that_domain_and_iq_match(IQ, Domain) ->
    %% we should not get here, log error
    ?LOG_ERROR(#{what => domain_and_iq_doesnt_match, domain => Domain, iq => IQ}),
    false.

-spec register_iq(iq_record(), domain_record()) -> ok.
register_iq({#iq_table_key{namespace = Namespace,
                           component = Component}, IQHandler},
            {Domain, _})->
    gen_iq_component:register_iq_handler(Component, Domain, Namespace, IQHandler).

-spec unregister_iq(iq_record(), domain_record()) -> ok.
unregister_iq({#iq_table_key{namespace = Namespace,
                             component = Component}, _},
              {Domain, _}) ->
    gen_iq_component:unregister_iq_handler(Component, Domain, Namespace).
