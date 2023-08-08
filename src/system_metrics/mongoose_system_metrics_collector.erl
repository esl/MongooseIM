-module(mongoose_system_metrics_collector).

-include("mongoose.hrl").

-type report_struct() ::
    #{
        name := term(),
        params := #{term() := term()}
    }.

-export_type([report_struct/0]).

-export([collect/1]).

collect(PrevReport) ->
    ReportResults = [ get_reports(RGetter) || RGetter <- report_getters()],
    StanzasCount = get_xmpp_stanzas_count(PrevReport),
    lists:flatten(ReportResults ++ StanzasCount).

-spec get_reports(fun(() -> [report_struct()])) -> [report_struct()].
get_reports(Fun) ->
    Fun().

-spec report_getters() -> [fun(() -> [report_struct()])].
report_getters() ->
    [
        fun get_hosts_count/0,
        fun get_domains_count/0,
        fun get_modules/0,
        fun get_number_of_custom_modules/0,
        fun get_cluster_data/0,
        fun get_api/0,
        fun get_transport_mechanisms/0,
        fun get_tls_options/0,
        fun get_outgoing_pools/0
    ].

get_cluster_data() ->
    Steps = [
        fun get_uptime/0,
        fun get_version/0,
        fun get_components/0,
        fun get_cluster_size/0,
        fun get_config_type/0
    ],
    Params = lists:foldl(fun(Step, Acc) ->
                             maps:merge(Acc, Step())
                         end, #{}, Steps),
    [#{name => cluster, params => Params}].

get_hosts_count() ->
    HostTypes = ?ALL_HOST_TYPES,
    NumberOfHosts = length(HostTypes),
    [#{name => host_count, params => #{value => NumberOfHosts}}].

get_domains_count() ->
    DomainsCount = mongoose_domain_core:domains_count(),
    [#{name => domain_count, params => #{value => DomainsCount}}].

get_modules() ->
    HostTypes = ?ALL_HOST_TYPES,
    AllModules = lists:flatten([gen_mod:loaded_modules(H) || H <- HostTypes]),
    ModulesToReport = filter_behaviour_implementations(lists:usort(AllModules),
                                                       mongoose_module_metrics),
    ModsWithOpts = [get_modules_metrics(Host, ModulesToReport) || Host <- HostTypes],
    [report_module_with_opts(Mod, Opt) || {Mod, Opt} <- lists:flatten(ModsWithOpts)].

filter_behaviour_implementations(Modules, Behaviour) ->
    lists:filter(
        fun(M) ->
             try lists:keyfind([Behaviour], 2, M:module_info(attributes)) of
                 {behavior, _} -> true;
                 {behaviour, _} -> true;
                 _ -> false
             catch
                 _:_ -> false
             end
         end, Modules).

get_modules_metrics(Host, Modules) ->
    lists:map(
        fun(M) ->
            case erlang:function_exported(M, config_metrics, 1) of
                true -> {M, M:config_metrics(Host)};
                false -> {M, [{none, none}]}
            end
        end, Modules).

report_module_with_opts(Module, Opts) ->
    #{name => module_with_opt, params =>
        lists:foldl(
            fun
                ({none, _}, Acc) -> 
                    Acc;
                ({OptKey, OptValue}, Acc) ->
                    maps:put(OptKey, OptValue, Acc)
            end, #{module => Module}, Opts)
    }.

get_number_of_custom_modules() ->
    HostTypes = ?ALL_HOST_TYPES,
    AllModules = lists:flatten(
                    lists:map(fun gen_mod:loaded_modules/1, HostTypes)),
    GenMods = filter_behaviour_implementations(AllModules, gen_mod),
    GenModsSet = sets:from_list(GenMods),
    MetricsModule = filter_behaviour_implementations(AllModules,
                                                     mongoose_module_metrics),
    MetricsModuleSet = sets:from_list(MetricsModule),
    CountCustomMods= sets:size(sets:subtract(GenModsSet, MetricsModuleSet)),
    [#{name => custom_module_count, params => #{value => CountCustomMods}}].

get_uptime() ->
    {Uptime, _} = statistics(wall_clock),
    UptimeSeconds = Uptime div 1000,
    {D, {H, M, S}} = calendar:seconds_to_daystime(UptimeSeconds),
    Formatted = io_lib:format("~4..0B-~2..0B:~2..0B:~2..0B", [D,H,M,S]),
    #{uptime => list_to_binary(Formatted)}.

get_cluster_size() ->
    NodesNo = length(nodes()) + 1,
    #{node_number => NodesNo}.

get_version() ->
    case lists:keyfind(mongooseim, 1, application:which_applications()) of
        {_, _, Version} ->
            #{version => list_to_binary(Version)};
        _ ->
            []
    end.

get_components() ->
    Domains = mongoose_router:get_all_domains() ++ mongoose_component:dirty_get_all_components(all),
    Components = [mongoose_component:lookup_component(D, node()) || D <- Domains],
    LenComponents = length(lists:flatten(Components)),
    #{component => LenComponents}.

get_api() ->
    ApiList = filter_unknown_api(get_http_handler_modules()),
    [#{name => http_api,
       params => lists:foldl(fun(Element, Acc) ->
                                 maps:put(Element, enabled, Acc)
                             end, #{}, ApiList)}].

filter_unknown_api(ApiList) ->
    AllowedToReport = [mongoose_client_api, mongoose_admin_api, mod_bosh, mod_websockets],
    [Api || Api <- ApiList, lists:member(Api, AllowedToReport)].

get_transport_mechanisms() ->
    HTTP = [Mod || Mod <- get_http_handler_modules(),
                   Mod =:= mod_bosh orelse Mod =:= mod_websockets],
    TCP = lists:usort([tcp || #{proto := tcp} <- get_listeners(mongoose_c2s_listener)]),
    [#{name => transport_mechanism,
       params => lists:foldl(fun(Element, Acc) ->
                                 maps:put(Element, enabled, Acc)
                             end, #{}, HTTP ++ TCP)}].

get_http_handler_modules() ->
    Listeners = get_listeners(ejabberd_cowboy),
    lists:usort(lists:flatmap(fun get_http_handler_modules/1, Listeners)).

get_listeners(Module) ->
    Listeners = mongoose_config:get_opt(listen),
    lists:filter(fun(#{module := Mod}) -> Mod =:= Module end, Listeners).

get_http_handler_modules(#{handlers := Handlers}) ->
    [Module || #{module := Module} <- Handlers].

get_tls_options() ->
    TLSOptions = lists:flatmap(fun extract_tls_options/1, get_listeners(mongoose_c2s_listener)),
    lists:foldl(fun({Mode, Module}, Acc) ->
                    [#{name => tls_option, params => #{mode => Mode, module => Module}}] ++ Acc
                end, [], lists:usort(TLSOptions)).

extract_tls_options(#{tls := #{mode := TLSMode, module := TLSModule}}) ->
    [{TLSMode, TLSModule}];
extract_tls_options(_) -> [].

get_outgoing_pools() ->
    OutgoingPools = mongoose_config:get_opt(outgoing_pools),
    [#{name => outgoing_pool,
       params => #{value => Type}} || #{type := Type} <- OutgoingPools].

get_xmpp_stanzas_count(PrevReport) ->
    StanzaTypes = [xmppMessageSent, xmppMessageReceived, xmppIqSent,
                   xmppIqReceived, xmppPresenceSent, xmppPresenceReceived],
    NewCount = [count_stanzas(StanzaType) || StanzaType <- StanzaTypes],
    StanzasCount = calculate_stanza_rate(PrevReport, NewCount),
    [#{name => xmpp_stanza_count,
       params => #{
        stanza_type => StanzaType,
        total => Total,
        increment => Increment
       }} || {StanzaType, Total, Increment} <- StanzasCount].

count_stanzas(StanzaType) ->
    ExometerResults = exometer:get_values(['_', StanzaType]),
    StanzaCount = lists:foldl(fun({ _, [{count,Count}, {one, _}]}, Sum) ->
                            Count + Sum end, 0, ExometerResults),
    {StanzaType, StanzaCount}.

calculate_stanza_rate([], NewCount) ->
    [{Type, Count, Count} || {Type, Count} <- NewCount];
calculate_stanza_rate(PrevReport, NewCount) ->
    ReportProplist = [{Name, Key} ||
        #{name := xmpp_stanza_count,
          params := #{stanza_type := Name, total := Key}}  <- PrevReport],
    [{Type, Count,
        case proplists:get_value(Type, ReportProplist) of
            undefined -> Count;
            Total -> Count-Total
        end} || {Type, Count} <- NewCount].

get_config_type() ->
    ConfigPath = mongoose_config:get_config_path(),
    ConfigType = case filename:extension(ConfigPath) of
        ".toml" -> toml;
        ".cfg" -> cfg;
        _ -> unknown_config_type
    end,
    #{config_type => ConfigType}.
