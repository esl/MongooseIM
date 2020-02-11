-module(mongoose_system_metrics_collector).

-type report_struct() ::
    #{
        report_name := term(),
        key := term(),
        value := term()
    }.

-export_type([report_struct/0]).

-export([collect/0]).

collect() ->
    ReportResults = [ get_reports(RGetter) || RGetter <- report_getters()],
    lists:flatten(ReportResults).

-spec get_reports(fun(() -> [report_struct()])) -> [report_struct()].
get_reports(Fun) ->
    Fun().

-spec report_getters() -> [fun(() -> [report_struct()])].
report_getters() ->
    [
        fun get_hosts_count/0,
        fun get_modules/0,
        fun get_number_of_custom_modules/0,
        fun get_uptime/0,
        fun get_cluster_size/0,
        fun get_version/0,
        fun get_components/0,
        fun get_api/0,
        fun get_transport_mechanisms/0,
        fun get_tls_options/0
    ].

get_hosts_count() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    NumberOfHosts = length(Hosts),
    [#{report_name => hosts, key => count, value => NumberOfHosts}].

get_modules() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    AllModules = lists:flatten([gen_mod:loaded_modules(H) || H <- Hosts]),
    ModulesToReport = filter_behaviour_implementations(lists:usort(AllModules),
                                                       mongoose_module_metrics),
    ModsWithOpts = [get_modules_metrics(Host, ModulesToReport) || Host <- Hosts],
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
                false -> {M ,[{none, none}]}
            end
        end, Modules).

report_module_with_opts(Module, Opts) ->
    lists:map(
        fun({OptKey, OptValue}) ->
            #{report_name => Module, key => OptKey, value => OptValue}
        end,Opts).

get_number_of_custom_modules() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    AllModules = lists:flatten(
                    lists:map(fun gen_mod:loaded_modules/1, Hosts)),
    GenMods = filter_behaviour_implementations(AllModules, gen_mod),
    GenModsSet = sets:from_list(GenMods),
    MetricsModule = filter_behaviour_implementations(AllModules,
                                                     mongoose_module_metrics),
    MetricsModuleSet = sets:from_list(MetricsModule),
    CountCustomMods= sets:size(sets:subtract(GenModsSet, MetricsModuleSet)),
    #{report_name => custom_modules, key => count, value => CountCustomMods}.

get_uptime() ->
    {Uptime, _} = statistics(wall_clock),
    UptimeSeconds = Uptime div 1000,
    {D, {H, M, S}} = calendar:seconds_to_daystime(UptimeSeconds),
    Formatted = io_lib:format("~4..0B-~2..0B:~2..0B:~2..0B", [D,H,M,S]),
    [#{report_name => cluster, key => uptime, value => list_to_binary(Formatted)}].

get_cluster_size() ->
    NodesNo = length(nodes()) + 1,
    [#{report_name => cluster, key => number_of_nodes, value => NodesNo}].

get_version() ->
    case lists:keyfind(mongooseim, 1, application:which_applications()) of
        {_, _, Version} ->
            #{report_name => cluster, key => mim_version, value => list_to_binary(Version)};
        _ ->
            []
    end.

get_components() ->
    Domains = ejabberd_router:dirty_get_all_domains(),
    Components = [ejabberd_router:lookup_component(D, node()) || D <- Domains],
    LenComponents = length(lists:flatten(Components)),
    #{report_name => cluster, key => number_of_components, value => LenComponents}.

get_api() ->
    ServiceOptions = get_service_option(ejabberd_cowboy),
    ModulesOptions = lists:flatten([ Mod || {modules, Mod} <- ServiceOptions]),
    % Modules Option can have variable number of elements. To be more
    % error-proof, extracting 3rd element instead of pattern matching.
    AllApi = lists:map(fun(Module)-> element(3, Module) end, ModulesOptions),
    ApiList = filter_unknown_api(lists:usort(AllApi)),
    [#{report_name => http_api, key => Api, value => enabled} || Api <- ApiList].

filter_unknown_api(ApiList) ->
    AllowedToReport = [ mongoose_api, mongoose_client_api_rooms_messages,
                        mongoose_client_api_rooms_users, mongoose_client_api_rooms_config,
                        mongoose_client_api_rooms ,mongoose_client_api_contacts,
                        mongoose_client_api_messages, lasse_handler, mongoose_api_admin,
                        mod_bosh, mod_websockets, mod_revproxy],
    [Api || Api <- ApiList, lists:member(Api, AllowedToReport)].

get_service_option(Service) ->
    Listen = ejabberd_config:get_local_option(listen),
    Result = [ Option || {_, S, Option} <- Listen, S == Service],
    lists:flatten(Result).

get_transport_mechanisms() ->
    ServiceOptions = get_service_option(ejabberd_cowboy),
    MaybeBosh  = maybe_api_configured(mod_bosh, ServiceOptions),
    MaybeWebsockets = maybe_api_configured(mod_websockets, ServiceOptions),
    MaybeTCP = is_tcp_configured(),
    ReturnList = lists:flatten([MaybeBosh, MaybeWebsockets, MaybeTCP]),
    [#{report_name => transport_mechanism,
       key => Transport,
       value => enabled} || Transport <- lists:usort(ReturnList)].

maybe_api_configured(Api, ServiceOptions) ->
    Modules = proplists:get_value(modules, ServiceOptions, []),
    case lists:keyfind(Api, 3, Modules) of
        false -> [];
        _Return -> Api
    end.

get_tls_options() ->
    TcpConfigured = is_tcp_configured(),
    TlsOption = check_tls_option([starttls, starttls_required, tls]),
    case {TcpConfigured, TlsOption} of
        {tcp, {Option, SpecificOption}} ->
            #{report_name => tls_option, key => Option, value => SpecificOption};
        { _, _} ->
            []
    end.

is_tcp_configured() ->
    case [] =/= lists:usort(get_service_option(ejabberd_c2s)) of
        true -> tcp;
        false -> []
    end.

check_tls_option([]) ->
    {none, none};
check_tls_option([TlsOption | Tail]) ->
    ServiceOptions = get_service_option(ejabberd_c2s),
    case lists:member(TlsOption, ServiceOptions) of
        true -> {TlsOption, check_tls_specific_option()};
        _ ->  check_tls_option(Tail)
    end.

check_tls_specific_option() ->
    ServiceOptions = get_service_option(ejabberd_c2s),
    case lists:member({tls_module, just_tls}, ServiceOptions) of
        true -> just_tls;
        _ -> fast_tls
    end.
