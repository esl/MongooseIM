%% Generally, you should not call anything from this module.
%% Use mongoose_domain_api module instead.
-module(mongoose_subdomain_core).
-behaviour(gen_server).

-include("mongoose_logger.hrl").

%% API
-export([start_link/0]).

-export([register_subdomain/3,
         unregister_subdomain/2,
         add_domain/2,
         remove_domain/2,
         sync/0]).

-export([get_host_type/1,
         get_subdomain_info/1,
         get_all_subdomains_for_domain/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-ignore_xref([start_link/0, sync/0]).

-ifdef(TEST).

-undef(LOG_ERROR).
-export([log_error/2]).
-define(LOG_ERROR(Error), ?MODULE:log_error(?FUNCTION_NAME, Error)).

-endif.

-define(SUBDOMAINS_TABLE, ?MODULE).
-define(REGISTRATION_TABLE, mongoose_subdomain_reg).

-type host_type() :: mongooseim:host_type().
-type domain() :: mongooseim:domain_name().
-type subdomain_pattern() :: mongoose_subdomain_utils:subdomain_pattern().
-type maybe_parent_domain() :: domain() | no_parent_domain.

-type reg_item() :: {{host_type(), subdomain_pattern()}, %% table key
                     Type :: fqdn | subdomain,
                     mongoose_packet_handler:t()}.

-record(subdomain_item, {host_type :: host_type() | '_',
                         subdomain :: domain() | '_', %% table key
                         subdomain_pattern :: subdomain_pattern() | '_',
                         parent_domain :: maybe_parent_domain() | '_',
                         packet_handler :: mongoose_packet_handler:t() | '_'}).

-type subdomain_item() :: #subdomain_item{}.

%% corresponds to the fields in #subdomain_item{} record
-type subdomain_info() :: #{host_type := host_type(),
                            subdomain := domain(),
                            subdomain_pattern := subdomain_pattern(),
                            parent_domain := maybe_parent_domain(),
                            packet_handler := mongoose_packet_handler:t()}.

-export_type([subdomain_info/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-ifdef(TEST).
%% this interface is required only to detect errors in unit tests
log_error(_Function, _Error) -> ok.
-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_subdomain(host_type(), subdomain_pattern(),
                         mongoose_packet_handler:t()) ->
    ok | {error, already_registered | subdomain_already_exists}.
register_subdomain(HostType, SubdomainPattern, PacketHandler) ->
    NewPacketHandler = mongoose_packet_handler:add_extra(PacketHandler,
                                                         #{host_type => HostType}),
    gen_server:call(?MODULE, {register, HostType, SubdomainPattern, NewPacketHandler}).

-spec unregister_subdomain(host_type(), subdomain_pattern()) -> ok.
unregister_subdomain(HostType, SubdomainPattern) ->
    gen_server:call(?MODULE, {unregister, HostType, SubdomainPattern}).

-spec sync() -> ok.
sync() ->
    gen_server:call(?MODULE, sync).

-spec add_domain(host_type(), domain()) -> ok.
add_domain(HostType, Domain) ->
    gen_server:cast(?MODULE, {add_domain, HostType, Domain}).

-spec remove_domain(host_type(), domain()) -> ok.
remove_domain(HostType, Domain) ->
    gen_server:cast(?MODULE, {remove_domain, HostType, Domain}).

-spec get_host_type(Subdomain :: domain()) -> {ok, host_type()} | {error, not_found}.
get_host_type(Subdomain) ->
    case ets:lookup(?SUBDOMAINS_TABLE, Subdomain) of
        [] ->
            {error, not_found};
        [#subdomain_item{host_type = HostType}] ->
            {ok, HostType}
    end.

-spec get_subdomain_info(Subdomain :: domain()) -> {ok, subdomain_info()} | {error, not_found}.
get_subdomain_info(Subdomain) ->
    case ets:lookup(?SUBDOMAINS_TABLE, Subdomain) of
        [] ->
            {error, not_found};
        [Item] ->
            {ok, convert_subdomain_item_to_map(Item)}
    end.

-spec get_all_subdomains_for_domain(Domain :: maybe_parent_domain()) -> [subdomain_info()].
%% if Domain param is set to no_parent_domain,
%% this function returns all the FQDN "subdomains".
get_all_subdomains_for_domain(Domain) ->
    Pattern = #subdomain_item{parent_domain = Domain, _ = '_'},
    Match = ets:match_object(?SUBDOMAINS_TABLE, Pattern),
    [convert_subdomain_item_to_map(Item) || Item <- Match].

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?SUBDOMAINS_TABLE, [set, named_table, protected,
                                {keypos, #subdomain_item.subdomain},
                                {read_concurrency, true}]),
    %% ?REGISTRATION_TABLE is protected only for traceability purposes
    ets:new(?REGISTRATION_TABLE, [set, named_table, protected]),
    %% no need for state, everything is kept in ETS tables
    {ok, ok}.

handle_call({register, HostType, SubdomainPattern, PacketHandler}, From, State) ->
    %% handle_register/4 must reply to the caller using gen_server:reply/2 interface
    handle_register(HostType, SubdomainPattern, PacketHandler, From),
    {noreply, State};
handle_call({unregister, HostType, SubdomainPattern}, _From, State) ->
    Result = handle_unregister(HostType, SubdomainPattern),
    {reply, Result, State};
handle_call(sync, _From, State) ->
    {reply, ok, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast({add_domain, HostType, Domain}, State) ->
    handle_add_domain(HostType, Domain),
    {noreply, State};
handle_cast({remove_domain, HostType, Domain}, State) ->
    handle_remove_domain(HostType, Domain),
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

-spec handle_register(host_type(), subdomain_pattern(),
                      mongoose_packet_handler:t(), any()) -> ok.
handle_register(HostType, SubdomainPattern, PacketHandler, From) ->
    SubdomainType = mongoose_subdomain_utils:subdomain_type(SubdomainPattern),
    RegItem = {{HostType, SubdomainPattern}, SubdomainType, PacketHandler},
    case ets:insert_new(?REGISTRATION_TABLE, RegItem) of
        true ->
            case SubdomainType of
                subdomain ->
                    Fn = fun(_HostType, Subdomain) ->
                             add_subdomain(RegItem, Subdomain)
                         end,
                    %% mongoose_domain_core:for_each_domain/2 can take quite long,
                    %% reply before running it.
                    gen_server:reply(From, ok),
                    mongoose_domain_core:for_each_domain(HostType, Fn);
                fqdn ->
                    Result = add_subdomain(RegItem, no_parent_domain),
                    gen_server:reply(From, Result)
            end;
        false ->
            gen_server:reply(From, {error, already_registered})
    end.

-spec handle_unregister(host_type(), subdomain_pattern()) -> ok.
handle_unregister(HostType, SubdomainPattern) ->
    Pattern = #subdomain_item{subdomain_pattern = SubdomainPattern,
                              host_type = HostType, _ = '_'},
    Match = ets:match_object(?SUBDOMAINS_TABLE, Pattern),
    remove_subdomains(Match),
    ets:delete(?REGISTRATION_TABLE, {HostType, SubdomainPattern}),
    ok.

-spec handle_add_domain(host_type(), domain()) -> ok.
handle_add_domain(HostType, Domain) ->
    check_domain_name(HostType, Domain),
    %% even if the domain name check fails, it's not easy to solve this
    %% collision. so the best thing we can do is to report it and just keep
    %% the data in both ETS tables (domains and subdomains) for further
    %% troubleshooting.
    Match = ets:match_object(?REGISTRATION_TABLE, {{HostType, '_'}, subdomain, '_'}),
    add_subdomains(Match, Domain).

-spec handle_remove_domain(host_type(), domain()) -> ok.
handle_remove_domain(HostType, Domain) ->
    Pattern = #subdomain_item{parent_domain = Domain, host_type = HostType, _ = '_'},
    Match = ets:match_object(?SUBDOMAINS_TABLE, Pattern),
    remove_subdomains(Match).

-spec remove_subdomains([subdomain_item()]) -> ok.
remove_subdomains(SubdomainItems) ->
    Fn = fun(SubdomainItem) ->
             remove_subdomain(SubdomainItem)
         end,
    lists:foreach(Fn, SubdomainItems).

-spec remove_subdomain(subdomain_item()) -> ok.
remove_subdomain(#subdomain_item{subdomain = Subdomain} = SubdomainItem) ->
    mongoose_hooks:unregister_subhost(Subdomain),
    ets:delete(?SUBDOMAINS_TABLE, Subdomain),
    SubdomainInfo = convert_subdomain_item_to_map(SubdomainItem),
    mongoose_lazy_routing:maybe_remove_subdomain(SubdomainInfo).

-spec add_subdomains([reg_item()], domain()) -> ok.
add_subdomains(RegItems, Domain) ->
    Fn = fun(RegItem) ->
             add_subdomain(RegItem, Domain)
         end,
    lists:foreach(Fn, RegItems).

-spec add_subdomain(reg_item(), maybe_parent_domain()) -> ok | {error, already_registered}.
add_subdomain(RegItem, Domain) ->
    #subdomain_item{subdomain = Subdomain} = Item = make_subdomain_item(RegItem, Domain),
    case ets:insert_new(?SUBDOMAINS_TABLE, Item) of
        true ->
            mongoose_hooks:register_subhost(Subdomain, false),
            check_subdomain_name(Item),
            %% even if the subdomain name check fails, it's not easy to solve this
            %% collision. so the best thing we can do is to report it and just keep
            %% the data in both ETS tables (domains and subdomains) for further
            %% troubleshooting.
            ok;
        false ->
            case ets:lookup(?SUBDOMAINS_TABLE, Subdomain) of
                [Item] ->
                    ok; %% exactly the same item is already inserted, it's fine.
                [ExistingItem] ->
                    report_subdomains_collision(ExistingItem, Item),
                    {error, subdomain_already_exists}
            end
    end.

-spec make_subdomain_item(reg_item(), maybe_parent_domain()) -> subdomain_item().
make_subdomain_item({{HostType, SubdomainPattern}, Type, PacketHandler}, Domain) ->
    Subdomain = case {Type, Domain} of
                    {fqdn, no_parent_domain} ->
                        %% not a subdomain, but FQDN
                        mongoose_subdomain_utils:get_fqdn(SubdomainPattern, <<"">>);
                    {subdomain, Domain} when is_binary(Domain) ->
                        mongoose_subdomain_utils:get_fqdn(SubdomainPattern, Domain)
                end,
    #subdomain_item{host_type = HostType, subdomain = Subdomain, parent_domain = Domain,
                    subdomain_pattern = SubdomainPattern, packet_handler = PacketHandler}.

-spec convert_subdomain_item_to_map(subdomain_item()) -> subdomain_info().
convert_subdomain_item_to_map(#subdomain_item{} = Item) ->
    Fields = record_info(fields, subdomain_item),
    [_ | Values] = tuple_to_list(Item),
    KVList = lists:zip(Fields, Values),
    maps:from_list(KVList).

-spec check_domain_name(mongooseim:host_type(), mongooseim:domain_name()) ->
    boolean().
check_domain_name(_HostType, Domain) ->
    case mongoose_subdomain_core:get_subdomain_info(Domain) of
        {error, not_found} -> true;
        {ok, SubdomainInfo} ->
            %% TODO: this is critical collision, and it must be reported properly
            %% think about adding some metric, so devops can set some alarm for it
            ?LOG_ERROR(#{what => check_domain_name_failed, domain => Domain}),
            %% in case of domain/subdomain name conflicts, mongoose_lazy_routing
            %% configures routing and IQ handling for a top level domain.
            %% So to keep configuration consistent on all of the nodes in the cluster,
            %% we must unregister subdomain and let mongoose_lazy_routing register top
            %% level domain on the next routing.
            mongoose_lazy_routing:maybe_remove_subdomain(SubdomainInfo),
            false
    end.

-spec check_subdomain_name(subdomain_item()) -> boolean().
check_subdomain_name(#subdomain_item{subdomain = Subdomain} = _SubdomainItem) ->
    case mongoose_domain_core:get_host_type(Subdomain) of
        {error, not_found} -> true;
        {ok, _HostType} ->
            %% TODO: this is critical collision, and it must be reported properly
            %% think about adding some metric, so devops can set some alarm for it
            ?LOG_ERROR(#{what => check_subdomain_name_failed, subdomain => Subdomain}),
            false
    end.

-spec report_subdomains_collision(subdomain_item(), subdomain_item()) -> ok.
report_subdomains_collision(ExistingSubdomainItem, _NewSubdomainItem) ->
    #subdomain_item{subdomain = Subdomain} = ExistingSubdomainItem,
    %% TODO: this is critical collision, and it must be reported properly
    %% think about adding some metric, so devops can set some alarm for it
    ?LOG_ERROR(#{what => subdomains_collision, subdomain => Subdomain}),
    ok.
