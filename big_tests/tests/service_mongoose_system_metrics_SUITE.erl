-module(service_mongoose_system_metrics_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER_URL, "http://localhost:8765").
-define(ETS_TABLE, qs).
-define(TRACKING_ID, "UA-151671255-2").
-define(TRACKING_ID_CI, "UA-151671255-1").
-define(TRACKING_ID_EXTRA, "UA-EXTRA-TRACKING-ID").

-record(event, {
    cid = "",
    tid = "",
    ec = "",
    ea = "",
    ev = "",
    el = "" }).

%% API
-export([
         all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         system_metrics_are_not_reported_when_not_allowed/1,
         periodic_report_available/1,
         all_clustered_mongooses_report_the_same_client_id/1,
         system_metrics_are_reported_to_google_analytics_when_mim_starts/1,
         system_metrics_are_reported_to_additional_google_analytics/1,
         system_metrics_are_reported_to_configurable_google_analytics/1,
         system_metrics_are_reported_to_a_json_file/1,
         tracking_id_is_correctly_configured/1,
         module_backend_is_reported/1,
         mongoose_version_is_reported/1,
         cluster_uptime_is_reported/1,
         xmpp_components_are_reported/1,
         api_are_reported/1,
         transport_mechanisms_are_reported/1
        ]).

-export([
         just_removed_from_config_logs_question/1,
         in_config_unmodified_logs_request_for_agreement/1,
         in_config_with_explicit_no_report_goes_off_silently/1,
         in_config_with_explicit_reporting_goes_on_silently/1
        ]).

-import(distributed_helper, [mim/0, mim2/0, mim3/0,
                             require_rpc_nodes/1
                            ]).

-import(component_helper, [connect_component/1,
                           disconnect_component/2,
                           spec/2,
                           common/1]).

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
     system_metrics_are_not_reported_when_not_allowed,
     periodic_report_available,
     all_clustered_mongooses_report_the_same_client_id,
     system_metrics_are_reported_to_google_analytics_when_mim_starts,
     system_metrics_are_reported_to_additional_google_analytics,
     system_metrics_are_reported_to_configurable_google_analytics,
     system_metrics_are_reported_to_a_json_file,
     tracking_id_is_correctly_configured,
     module_backend_is_reported,
     mongoose_version_is_reported,
     cluster_uptime_is_reported,
     xmpp_components_are_reported,
     api_are_reported,
     transport_mechanisms_are_reported,
     {group, log_transparency}
    ].

groups() ->
    [
     {log_transparency, [], [
                             just_removed_from_config_logs_question,
                             in_config_unmodified_logs_request_for_agreement,
                             in_config_with_explicit_no_report_goes_off_silently,
                             in_config_with_explicit_reporting_goes_on_silently
                            ]}
    ].

-define(APPS, [inets, crypto, ssl, ranch, cowlib, cowboy]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->

    case system_metrics_service_is_enabled(mim()) of
        false ->
            ct:fail("service_mongoose_system_metrics is not running");
        true ->
            [ {ok, _} = application:ensure_all_started(App) || App <- ?APPS ],
            http_helper:start(8765, "/[...]", fun handler_init/1),
            Config1 = escalus:init_per_suite(Config),
            ejabberd_node_utils:init(Config1)
    end.

end_per_suite(Config) ->
    http_helper:stop(),
    Args = [{initial_report, timer:seconds(20)}, {periodic_report, timer:minutes(5)}],
    [start_system_metrics_module(Node, Args) || Node <- [mim(), mim2()]],
    escalus:end_per_suite(Config).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_group(log_transparency, Config) ->
    lager_ct_backend:start(),
    lager_ct_backend:capture(warning),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(log_transparency, Config) ->
    lager_ct_backend:stop_capture(),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(periodic_report_available, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config;
init_per_testcase(system_metrics_are_reported_to_google_analytics_when_mim_starts, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config;
init_per_testcase(system_metrics_are_not_reported_when_not_allowed, Config) ->
    create_events_collection(),
    disable_system_metrics(mim()),
    delete_prev_client_id(mim()),
    Config;
init_per_testcase(all_clustered_mongooses_report_the_same_client_id, Config) ->
    create_events_collection(),
    distributed_helper:add_node_to_cluster(mim2(), Config),
    enable_system_metrics(mim()),
    enable_system_metrics(mim2()),
    Config;
init_per_testcase(system_metrics_are_reported_to_additional_google_analytics, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    configure_additional_tracking_id(mim()),
    Config;
init_per_testcase(system_metrics_are_reported_to_configurable_google_analytics, Config) ->
    create_events_collection(),
    enable_system_metrics_with_configurable_tracking_id(mim()),
    Config;
init_per_testcase(xmpp_components_are_reported, Config) ->
    create_events_collection(),
    Config1 = get_components(common(Config), Config),
    enable_system_metrics(mim()),
    Config1;
init_per_testcase(module_backend_is_reported, Config) ->
    create_events_collection(),
    maybe_start_module(mod_vcard),
    enable_system_metrics(mim()),
    Config;
init_per_testcase(_TestcaseName, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config.


end_per_testcase(periodic_report_available, Config) ->
    clear_events_collection(),
    disable_system_metrics(mim()),
    delete_prev_client_id(mim()),
    Config;
end_per_testcase(system_metrics_are_reported_to_google_analytics_when_mim_starts, Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    disable_system_metrics(mim()),
    Config;
end_per_testcase(system_metrics_are_not_reported_when_not_allowed, Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    Config;
end_per_testcase(all_clustered_mongooses_report_the_same_client_id , Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    Nodes = [mim(), mim2()],
    [ begin delete_prev_client_id(Node), disable_system_metrics(Node) end || Node <- Nodes ],
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    Config;
end_per_testcase(system_metrics_are_reported_to_additional_google_analytics, Config) ->
    clear_events_collection(),
    remove_additional_tracking_id(mim()),
    disable_system_metrics(mim()),
    Config;
end_per_testcase(_TestcaseName, Config) ->
    clear_events_collection(),
    disable_system_metrics(mim()),
    Config.


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
system_metrics_are_not_reported_when_not_allowed(_Config) ->
    true = system_metrics_service_is_disabled(mim()).

periodic_report_available(_Config) ->
    ReportsNumber = get_events_collection_size(),
    mongoose_helper:wait_until(
        fun() ->
                NewReportsNumber = get_events_collection_size(),
                NewReportsNumber > ReportsNumber + 1
        end,
        true).

all_clustered_mongooses_report_the_same_client_id(_Config) ->
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    all_event_have_the_same_client_id().


system_metrics_are_reported_to_google_analytics_when_mim_starts(_Config) ->
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    mongoose_helper:wait_until(fun modules_are_reported/0, true),
    all_event_have_the_same_client_id().

tracking_id_is_correctly_configured(_Config) ->
    TrackingId = distributed_helper:rpc(mim(), ejabberd_config, get_local_option, [google_analytics_tracking_id]),
    case os:getenv("CI") of
        "true" ->
            ?assertEqual(?TRACKING_ID_CI, TrackingId);
        _ ->
            ?assertEqual(?TRACKING_ID, TrackingId)
    end.

system_metrics_are_reported_to_additional_google_analytics(_Config) ->
    mongoose_helper:wait_until(fun events_are_reported_to_additional_tracking_id/0, true).

system_metrics_are_reported_to_configurable_google_analytics(_Config) ->
    mongoose_helper:wait_until(fun events_are_reported_to_additional_tracking_id/0, true),
    mongoose_helper:wait_until(fun events_are_reported_to_configurable_tracking_id/0, true).

system_metrics_are_reported_to_a_json_file(_Config) ->
    ReportFilePath = distributed_helper:rpc(mim(), mongoose_system_metrics_file, location, []),
    ReportLastModified = distributed_helper:rpc(mim(), filelib, last_modified, [ReportFilePath]),
    Fun = fun() ->
        ReportLastModified < distributed_helper:rpc(mim(), filelib, last_modified, [ReportFilePath])
    end,
    mongoose_helper:wait_until(Fun, true),
    %% now we read the content of the file and check if it's a valid JSON
    {ok, File} = distributed_helper:rpc(mim(), file, read_file, [ReportFilePath]),
    jiffy:decode(File).

module_backend_is_reported(_Config) ->
    mongoose_helper:wait_until(fun modules_are_reported/0, true),
    mongoose_helper:wait_until(fun mod_vcard_backend_is_reported/0, true).

mongoose_version_is_reported(_Config) ->
    mongoose_helper:wait_until(fun mongoose_version_is_reported/0, true).

cluster_uptime_is_reported(_Config) ->
    mongoose_helper:wait_until(fun cluster_uptime_is_reported/0, true).

xmpp_components_are_reported(Config) ->
    CompOpts = ?config(component1, Config),
    {Component, Addr, _} = connect_component(CompOpts),
    mongoose_helper:wait_until(fun xmpp_components_are_reported/0, true),
    mongoose_helper:wait_until(fun more_than_one_component_is_reported/0, true),
    disconnect_component(Component, Addr).

api_are_reported(_Config) ->
    mongoose_helper:wait_until(fun api_are_reported/0, true).

transport_mechanisms_are_reported(_Config) ->
    mongoose_helper:wait_until(fun transport_mechanisms_are_reported/0, true).

just_removed_from_config_logs_question(_Config) ->
    disable_system_metrics(mim3()),
    remove_service_from_config(service_mongoose_system_metrics),
    %% WHEN
    Result = distributed_helper:rpc(
               mim3(), service_mongoose_system_metrics, verify_if_configured, []),
    %% THEN
    ?assertEqual(ignore, Result).

in_config_unmodified_logs_request_for_agreement(_Config) ->
    %% WHEN
    disable_system_metrics(mim()),
    lager_ct_backend:capture(warning),
    enable_system_metrics(mim()),
    %% THEN
    FilterFun = fun(_, Msg) ->
                        re:run(Msg, "MongooseIM docs", [global]) /= nomatch
                end,
    mongoose_helper:wait_until(fun() -> length(lager_ct_backend:recv(FilterFun)) end, 1),
    %% CLEAN
    lager_ct_backend:stop_capture(),
    disable_system_metrics(mim()).

in_config_with_explicit_no_report_goes_off_silently(_Config) ->
    %% WHEN
    lager_ct_backend:capture(warning),
    start_system_metrics_module(mim(), [no_report]),
    lager_ct_backend:stop_capture(),
    %% THEN
    FilterFun = fun(warning, Msg) ->
                        re:run(Msg, "MongooseIM docs", [global]) /= nomatch;
                   (_,_) -> false
                end,
    [] = lager_ct_backend:recv(FilterFun),
    %% CLEAN
    disable_system_metrics(mim()).

in_config_with_explicit_reporting_goes_on_silently(_Config) ->
    %% WHEN
    lager_ct_backend:capture(warning),
    start_system_metrics_module(mim(), [report]),
    lager_ct_backend:stop_capture(),
    %% THEN
    FilterFun = fun(warning, Msg) ->
                        re:run(Msg, "MongooseIM docs", [global]) /= nomatch;
                   (_,_) -> false
                end,
    [] = lager_ct_backend:recv(FilterFun),
    %% CLEAN
    disable_system_metrics(mim()).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

all_event_have_the_same_client_id() ->
    Tab = ets:tab2list(?ETS_TABLE),
    UniqueSortedTab = lists:usort([Cid ||#event{cid = Cid} <- Tab]),
    1 = length(UniqueSortedTab).

hosts_count_is_reported() ->
    is_in_table(<<"hosts">>).

hosts_count_is_not_reported() ->
    is_not_in_table(<<"hosts">>).

modules_are_reported() ->
    is_in_table(<<"module">>).

modules_are_not_reported() ->
    is_not_in_table(<<"module">>).

is_in_table(EventCategory) ->
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{ec = EC}) ->
            verify_category(EC, EventCategory)
        end, Tab).

is_not_in_table(EventCategory) ->
    not is_in_table(EventCategory).

verify_category(EC, <<"module">>) ->
    Result = re:run(EC, "^mod_.*"),
    case Result of
        {match, _Captured} -> true;
        nomatch -> false
    end;
verify_category(EC, EC) ->
    true;
verify_category(_EC, _EventCategory) ->
    false.

get_events_collection_size() ->
    length(ets:tab2list(?ETS_TABLE)).

enable_system_metrics(Node) ->
    enable_system_metrics(Node, [{initial_report, 100}, {periodic_report, 100}]).

enable_system_metrics(Node, Timers) ->
    UrlArgs = [google_analytics_url, ?SERVER_URL],
    {atomic, ok} = mongoose_helper:successful_rpc(Node, ejabberd_config, add_local_option, UrlArgs),
    start_system_metrics_module(Node, Timers).

enable_system_metrics_with_configurable_tracking_id(Node) ->
    enable_system_metrics(Node, [{initial_report, 100}, {periodic_report, 100}, {tracking_id, ?TRACKING_ID_EXTRA}]).

start_system_metrics_module(Node, Args) ->
    distributed_helper:rpc(
      Node, mongoose_service, start_service, [service_mongoose_system_metrics, Args]).

disable_system_metrics(Node) ->
    distributed_helper:rpc(Node, mongoose_service, stop_service, [service_mongoose_system_metrics]),
    mongoose_helper:successful_rpc(Node, ejabberd_config, del_local_option, [ google_analytics_url ]).

delete_prev_client_id(Node) ->
    mongoose_helper:successful_rpc(Node, mnesia, delete_table, [service_mongoose_system_metrics]).

create_events_collection() ->
    ets:new(?ETS_TABLE, [duplicate_bag, named_table, public]).

clear_events_collection() ->
    ets:delete_all_objects(?ETS_TABLE).

system_metrics_service_is_enabled(Node) ->
    Pid = distributed_helper:rpc(Node, erlang, whereis, [service_mongoose_system_metrics]),
    erlang:is_pid(Pid).

system_metrics_service_is_disabled(Node) ->
    not system_metrics_service_is_enabled(Node).

configure_additional_tracking_id(Node) ->
    TrackingIdArgs = [extra_google_analytics_tracking_id, ?TRACKING_ID_EXTRA],
    {atomic, ok} = mongoose_helper:successful_rpc(Node, ejabberd_config, add_local_option, TrackingIdArgs).

remove_additional_tracking_id(Node) ->
    mongoose_helper:successful_rpc(
        Node, ejabberd_config, del_local_option, [ extra_google_analytics_tracking_id ]).

remove_service_from_config(Service) ->
        Services = distributed_helper:rpc(
                       mim3(), ejabberd_config, get_local_option_or_default, [services, []]),
        NewServices = proplists:delete(Service, Services),
        distributed_helper:rpc(mim3(), ejabberd_config, add_local_option, [services, NewServices]).

events_are_reported_to_additional_tracking_id() ->
    Tab = ets:tab2list(?ETS_TABLE),
    SetTab = sets:from_list([Tid ||#event{tid = Tid} <- Tab]),
    2 >= sets:size(SetTab).

events_are_reported_to_configurable_tracking_id() ->
    ConfigurableTrackingId = <<?TRACKING_ID_EXTRA>>,
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{tid = TrackingId}) ->
            TrackingId == ConfigurableTrackingId
        end, Tab).

maybe_start_module(Module) ->
    Host = <<"localhost">>,
    Options = [],
    distributed_helper:rpc(mim(), gen_mod, start_module, [Host, Module, Options]).

feature_is_reported(EventCategory, EventAction) ->
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{ec = EC, ea = EA}) ->
            EC == EventCategory andalso EA == EventAction
        end, Tab).

mod_vcard_backend_is_reported() ->
    feature_is_reported(<<"mod_vcard">>, <<"backend">>).

mongoose_version_is_reported() ->
    feature_is_reported(<<"cluster">>, <<"mim_version">>).

cluster_uptime_is_reported() ->
    feature_is_reported(<<"cluster">>, <<"uptime">>).

xmpp_components_are_reported() ->
    feature_is_reported(<<"cluster">>, <<"number_of_components">>).

api_are_reported() ->
    is_in_table(<<"http_api">>).

transport_mechanisms_are_reported() ->
    is_in_table(<<"transport_mechanism">>).

more_than_one_component_is_reported() ->
    Tab = ets:tab2list(?ETS_TABLE),
    EventCategory = <<"cluster">>,
    EventAction = <<"number_of_components">>,
    lists:any(
        fun(#event{ec = EC, ea = EA, el = EL}) ->
             EC == EventCategory andalso EA == EventAction andalso EL > <<"0">>
        end, Tab).

%%--------------------------------------------------------------------
%% Cowboy handlers
%%--------------------------------------------------------------------
handler_init(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    StrEvents = string:split(Body, "\n", all),
    lists:map(
        fun(StrEvent) ->
            Event = str_to_event(StrEvent),
            ets:insert(?ETS_TABLE, Event)
        end, StrEvents),
    Req1 = cowboy_req:reply(200, #{}, <<"">>, Req),
    {ok, Req1, no_state}.

str_to_event(Qs) ->
    StrParams = string:split(Qs, "&", all),
    Params = lists:map(
        fun(StrParam) ->
            [StrKey, StrVal] = string:split(StrParam, "="),
            {binary_to_atom(StrKey, utf8), StrVal}
        end, StrParams),
    #event{
        cid = get_el(cid, Params),
        tid = get_el(tid, Params),
        ec = get_el(ec, Params),
        ea = get_el(ea, Params),
        el = get_el(el, Params),
        ev = get_el(ev, Params)
    }.

get_el(Key, Proplist) ->
    proplists:get_value(Key, Proplist, undef).

get_components(Opts, Config) ->
    Components = [component1, component2, vjud_component],
    [ {C, Opts ++ spec(C, Config)} || C <- Components ] ++ Config.
