-module(mongoose_system_stats_gatherer).

-type report_struct() :: 
    #{
        report_name := term(),
        key := term(),
        value := term()
    }.

-export_type([report_struct/0]).

-export([gather/0]).

gather() ->
    ReportResults = [ get_reports(RGetter) || RGetter <- report_getters()],
    FlatReportResults = lists:flatten(ReportResults),
    spawn(mongoose_system_stats_sender, send, [FlatReportResults]).

-spec get_reports(fun(() -> [report_struct()])) -> [report_struct()].
get_reports(Fun) ->
    Fun().

-spec report_getters() -> [fun(() -> [report_struct()])].
report_getters() ->
    [
        fun get_hosts_count/0,
        fun get_modules/0
    ].

get_hosts_count() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    NumberOfHosts = length(Hosts),
    [#{report_name => hosts, key => count, value => NumberOfHosts}].

get_modules() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    ModulesWithOpts = lists:flatten(
        lists:map(fun gen_mod:loaded_modules_with_opts/1, Hosts)),
    lists:map(
        fun({Module, Opts}) ->
            Backend = proplists:get_value(backend, Opts, none),
            #{report_name => Module, key => backend, value => Backend}
        end, ModulesWithOpts).


