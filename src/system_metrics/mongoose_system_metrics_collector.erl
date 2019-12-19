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
        fun get_version/0
    ].

get_hosts_count() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    NumberOfHosts = length(Hosts),
    [#{report_name => hosts, key => count, value => NumberOfHosts}].

get_modules() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    AllModules = lists:flatten(
                    lists:map(fun gen_mod:loaded_modules/1, Hosts)),
    ModulesToReport = filter_behaviour_implementations(AllModules,
                                                       mongoose_module_metrics),
    ModulesWithOpts = lists:flatten(
                        lists:map(
                            fun(Host) ->
                                get_modules_metrics(Host, ModulesToReport)
                            end, Hosts)),
    lists:map(
        fun({Module, Opts}) ->
            report_module_with_opts(Module, Opts)
        end, ModulesWithOpts).

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
    [#{report_name => cluster, key => uptime, value => Formatted}].

get_cluster_size() ->
    NodesNo = length(nodes()) + 1,
    [#{report_name => cluster, key => number_of_nodes, value => NodesNo}].

get_version() ->
    ok.
%%get_loglevel() ->
%%    Loglevels = ejabberd_loglevel:get(),
%%    [{_Module, {_N, Loglevel}} | _T ] = Loglevels, % TODO which loglevel should be chosen?
%%    [#{report_name => cluster, key => log_level_configured, value => Loglevel}].
