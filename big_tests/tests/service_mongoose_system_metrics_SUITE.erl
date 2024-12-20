-module(service_mongoose_system_metrics_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER_URL, "http://localhost:8765").
-define(ETS_TABLE, qs).
-define(TRACKING_ID, #{id => "G-7KQE4W9SVJ", secret => "Secret"}).
-define(TRACKING_ID_CI, #{id => "G-VB91V60SKT", secret => "Secret2"}).
-define(TRACKING_ID_EXTRA, #{id => "UA-EXTRA-TRACKING-ID", secret => "Secret3"}).

-record(event, {
    name = <<>>,
    params = #{},
    client_id = <<>>,
    instance_id = <<>>,
    app_secret = <<>>}).

-import(distributed_helper, [mim/0, mim2/0, mim3/0, rpc/4, require_rpc_nodes/1]).

-import(domain_helper, [host_type/0]).
-import(config_parser_helper, [mod_config/2, config/2]).

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
     system_metrics_are_not_reported_when_not_allowed,
     periodic_report_available,
     all_clustered_mongooses_report_the_same_client_id,
     system_metrics_are_reported_to_google_analytics_when_mim_starts,
     system_metrics_are_reported_to_configurable_google_analytics,
     system_metrics_are_reported_to_a_json_file,
     mongoose_version_is_reported,
     cluster_uptime_is_reported,
     xmpp_components_are_reported,
     api_are_reported,
     transport_mechanisms_are_reported,
     outgoing_pools_are_reported,
     xmpp_stanzas_counts_are_reported,
     config_type_is_reported,
     {group, module_opts},
     {group, log_transparency}
    ].

groups() ->
    [
     {module_opts, [], [
                        module_opts_are_reported,
                        rdbms_module_opts_are_reported
                       ]},
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
    [ {ok, _} = application:ensure_all_started(App) || App <- ?APPS ],
    http_helper:start(8765, "/[...]", fun handler_init/1),
    Config1 = escalus:init_per_suite(Config),
    Config2 = dynamic_services:save_services([mim(), mim2()], Config1),
    ejabberd_node_utils:init(Config2).

end_per_suite(Config) ->
    http_helper:stop(),
    dynamic_services:restore_services(Config),
    escalus:end_per_suite(Config).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_group(module_opts, Config) ->
    dynamic_modules:save_modules(host_type(), Config);
init_per_group(log_transparency, Config) ->
    logger_ct_backend:start(),
    logger_ct_backend:capture(warning),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(module_opts, Config) ->
    dynamic_modules:restore_modules(Config);
end_per_group(log_transparency, Config) ->
    logger_ct_backend:stop_capture(),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

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
init_per_testcase(system_metrics_are_reported_to_configurable_google_analytics, Config) ->
    create_events_collection(),
    enable_system_metrics_with_configurable_tracking_id(mim()),
    Config;
init_per_testcase(xmpp_components_are_reported, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config;
init_per_testcase(xmpp_stanzas_counts_are_reported = CN, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config1 = escalus:create_users(Config, escalus:get_users([alice, bob])),
    escalus:init_per_testcase(CN, Config1);
init_per_testcase(rdbms_module_opts_are_reported = CN, Config) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        false ->
            {skip, "RDBMS is not available"};
        true ->
            create_events_collection(),
            dynamic_modules:ensure_modules(host_type(), required_modules(CN)),
            enable_system_metrics(mim()),
            Config
    end;
init_per_testcase(module_opts_are_reported = CN, Config) ->
    create_events_collection(),
    dynamic_modules:ensure_modules(host_type(), required_modules(CN)),
    enable_system_metrics(mim()),
    Config;
init_per_testcase(_TestcaseName, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config.

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
end_per_testcase(xmpp_stanzas_counts_are_reported = CN, Config) ->
    clear_events_collection(),
    disable_system_metrics(mim()),
    escalus:delete_users(Config, escalus:get_users([alice, bob])),
    escalus:end_per_testcase(CN, Config);
end_per_testcase(_TestcaseName, Config) ->
    clear_events_collection(),
    disable_system_metrics(mim()),
    delete_prev_client_id(mim()),
    Config.


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
system_metrics_are_not_reported_when_not_allowed(_Config) ->
    true = system_metrics_service_is_disabled(mim()).

periodic_report_available(_Config) ->
    ReportsNumber = get_events_collection_size(),
    wait_helper:wait_until(
        fun() ->
                NewReportsNumber = get_events_collection_size(),
                NewReportsNumber > ReportsNumber + 1
        end,
        true).

all_clustered_mongooses_report_the_same_client_id(_Config) ->
    wait_helper:wait_until(fun is_host_count_reported/0, true),
    all_event_have_the_same_client_id().

system_metrics_are_reported_to_google_analytics_when_mim_starts(_Config) ->
    wait_helper:wait_until(fun is_host_count_reported/0, true),
    wait_helper:wait_until(fun are_modules_reported/0, true),
    wait_helper:wait_until(fun events_are_reported_to_primary_tracking_id/0, true),
    all_event_have_the_same_client_id().

system_metrics_are_reported_to_configurable_google_analytics(_Config) ->
    wait_helper:wait_until(fun is_host_count_reported/0, true),
    wait_helper:wait_until(fun are_modules_reported/0, true),
    wait_helper:wait_until(fun events_are_reported_to_both_tracking_ids/0, true),
    all_event_have_the_same_client_id().

system_metrics_are_reported_to_a_json_file(_Config) ->
    ReportFilePath = rpc(mim(), mongoose_system_metrics_file, location, []),
    ReportLastModified = rpc(mim(), filelib, last_modified, [ReportFilePath]),
    Fun = fun() ->
        ReportLastModified < rpc(mim(), filelib, last_modified, [ReportFilePath])
    end,
    wait_helper:wait_until(Fun, true),
    %% now we read the content of the file and check if it's a valid JSON
    {ok, File} = rpc(mim(), file, read_file, [ReportFilePath]),
    jiffy:decode(File).

module_opts_are_reported(_Config) ->
    wait_helper:wait_until(fun are_modules_reported/0, true),
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    MemBackend = ct_helper:get_internal_database(),
    check_module_backend(mod_bosh, MemBackend),
    check_module_backend(mod_event_pusher, push),
    check_module_backend(mod_event_pusher_push, Backend),
    check_module_backend(mod_http_upload, s3),
    check_module_backend(mod_last, Backend),
    check_module_backend(mod_muc, Backend),
    check_module_opt(mod_muc, <<"online_backend">>, atom_to_binary(MemBackend)),
    check_module_backend(mod_muc_light, Backend),
    check_module_backend(mod_offline, Backend),
    check_module_backend(mod_privacy, Backend),
    check_module_backend(mod_private, Backend),
    check_module_backend(mod_pubsub, Backend),
    check_module_opt(mod_push_service_mongoosepush, <<"api_version">>, <<"v3">>),
    check_module_backend(mod_roster, Backend),
    check_module_backend(mod_vcard, Backend).

rdbms_module_opts_are_reported(_Config) ->
    wait_helper:wait_until(fun are_modules_reported/0, true),
    check_module_backend(mod_auth_token, rdbms),
    check_module_backend(mod_inbox, rdbms),
    check_module_backend(mod_mam, rdbms).

check_module_backend(Module, Backend) ->
    check_module_opt(Module, <<"backend">>, atom_to_binary(Backend)).

mongoose_version_is_reported(_Config) ->
    wait_helper:wait_until(fun is_mongoose_version_reported/0, true).

cluster_uptime_is_reported(_Config) ->
    wait_helper:wait_until(fun is_cluster_uptime_reported/0, true).

xmpp_components_are_reported(_Config) ->
    CompOpts = component_helper:spec(component1),
    {Component, Addr, _} = component_helper:connect_component(CompOpts),
    wait_helper:wait_until(fun are_xmpp_components_reported/0, true),
    wait_helper:wait_until(fun more_than_one_component_is_reported/0, true),
    component_helper:disconnect_component(Component, Addr).

api_are_reported(_Config) ->
    wait_helper:wait_until(fun is_api_reported/0, true).

transport_mechanisms_are_reported(_Config) ->
    wait_helper:wait_until(fun are_transport_mechanisms_reported/0, true).

outgoing_pools_are_reported(_Config) ->
    wait_helper:wait_until(fun are_outgoing_pools_reported/0, true).

xmpp_stanzas_counts_are_reported(Config) ->
    escalus:story(Config, [{alice,1}, {bob,1}], fun(Alice, Bob) ->
        wait_helper:wait_until(fun is_message_count_reported/0, true),
        wait_helper:wait_until(fun is_iq_count_reported/0, true),
        Sent = get_metric_value(<<"xmppMessageSent">>),
        Received = get_metric_value(<<"xmppMessageReceived">>),
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi">>)),
        escalus:assert(is_chat_message, [<<"Hi">>], escalus:wait_for_stanza(Bob)),
        F = fun() -> assert_message_count_is_incremented(Sent, Received) end,
        wait_helper:wait_until(F, ok, #{sleep_time => 500, time_left => timer:seconds(20)})
    end).

config_type_is_reported(_Config) ->
    wait_helper:wait_until(fun is_config_type_reported/0, true).

just_removed_from_config_logs_question(_Config) ->
    disable_system_metrics(mim3()),
    %% WHEN
    Result = distributed_helper:rpc(
               mim3(), service_mongoose_system_metrics, verify_if_configured, []),
    %% THEN
    ?assertEqual(ignore, Result).

in_config_unmodified_logs_request_for_agreement(_Config) ->
    %% WHEN
    disable_system_metrics(mim()),
    logger_ct_backend:capture(warning),
    enable_system_metrics(mim()),
    %% THEN
    FilterFun = fun(_, Msg) ->
                        re:run(Msg, "MongooseIM docs", [global]) /= nomatch
                end,
    wait_helper:wait_until(fun() -> length(logger_ct_backend:recv(FilterFun)) end, 1),
    %% CLEAN
    logger_ct_backend:stop_capture(),
    disable_system_metrics(mim()).

in_config_with_explicit_no_report_goes_off_silently(_Config) ->
    %% WHEN
    logger_ct_backend:capture(warning),
    start_system_metrics_service(mim(), #{report => false}),
    logger_ct_backend:stop_capture(),
    %% THEN
    FilterFun = fun(warning, Msg) ->
                        re:run(Msg, "MongooseIM docs", [global]) /= nomatch;
                   (_,_) -> false
                end,
    [] = logger_ct_backend:recv(FilterFun),
    %% CLEAN
    disable_system_metrics(mim()).

in_config_with_explicit_reporting_goes_on_silently(_Config) ->
    %% WHEN
    logger_ct_backend:capture(warning),
    start_system_metrics_service(mim(), #{report => true}),
    logger_ct_backend:stop_capture(),
    %% THEN
    FilterFun = fun(warning, Msg) ->
                        re:run(Msg, "MongooseIM docs", [global]) /= nomatch;
                   (_,_) -> false
                end,
    [] = logger_ct_backend:recv(FilterFun),
    %% CLEAN
    disable_system_metrics(mim()).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

required_modules(CaseName) ->
    lists:filter(fun({Module, _Opts}) -> is_module_supported(Module) end,
                 modules_to_test(CaseName)).

modules_to_test(module_opts_are_reported) ->
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    MemBackend = ct_helper:get_internal_database(),
    [required_module(mod_bosh, #{backend => MemBackend}),
     required_module(mod_event_pusher,
                     #{push => config([modules, mod_event_pusher, push], #{backend => Backend})}),
     required_module(mod_http_upload, s3),
     required_module(mod_last, Backend),
     required_module(mod_muc, #{backend => Backend, online_backend => MemBackend}),
     required_module(mod_muc_light, Backend),
     required_module(mod_offline, Backend),
     required_module(mod_privacy, Backend),
     required_module(mod_private, Backend),
     required_module(mod_pubsub, Backend),
     required_module(mod_push_service_mongoosepush),
     required_module(mod_roster, Backend),
     required_module(mod_vcard, Backend)];
modules_to_test(rdbms_module_opts_are_reported) ->
    [required_module(mod_auth_token),
     required_module(mod_inbox),
     required_module(mod_mam)].

required_module(Module) ->
    required_module(Module, #{}).

required_module(Module, Backend) when is_atom(Backend) ->
    {Module, mod_config(Module, #{backend => Backend})};
required_module(Module, Opts) ->
    {Module, mod_config(Module, Opts)}.

check_module_opt(Module, Key, Value) when is_binary(Key), is_binary(Value) ->
    case is_module_supported(Module) of
        true ->
            ?assert(is_module_opt_reported(atom_to_binary(Module), Key, Value), {Module, Key, Value});
        false ->
            ct:log("Skipping unsupported module ~p", [Module])
    end.

is_module_supported(Module) ->
    is_host_type_static() orelse supports_dynamic_domains(Module).

is_host_type_static() ->
    rpc(mim(), mongoose_domain_core, is_static, [host_type()]).

supports_dynamic_domains(Module) ->
    rpc(mim(), gen_mod, does_module_support, [Module, dynamic_domains]).

all_event_have_the_same_client_id() ->
    Tab = ets:tab2list(?ETS_TABLE),
    UniqueSortedTab = lists:usort([Cid || #event{client_id = Cid} <- Tab]),
    1 = length(UniqueSortedTab).

is_host_count_reported() ->
    is_in_table(<<"host_count">>).

are_modules_reported() ->
    is_in_table(<<"module">>).

is_in_table(EventName) ->
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{name = Name, params = Params}) ->
            verify_name(Name, EventName, Params)
        end, Tab).

verify_name(<<"module_with_opt">>, <<"module">>, Params) ->
    Module = maps:get(<<"module">>, Params),
    Result = re:run(Module, "^mod_.*"),
    case Result of
        {match, _Captured} -> true;
        nomatch -> false
    end;
verify_name(Name, Name, _) ->
    true;
verify_name(_, _, _) ->
    false.

get_events_collection_size() ->
    ets:info(?ETS_TABLE, size).

enable_system_metrics(Node) ->
    enable_system_metrics(Node, #{initial_report => 100, periodic_report => 100}).

enable_system_metrics_with_configurable_tracking_id(Node) ->
    enable_system_metrics(Node, #{initial_report => 100, periodic_report => 100,
                                  tracking_id => ?TRACKING_ID_EXTRA}).

enable_system_metrics(Node, Opts) ->
    UrlArgs = [google_analytics_url, ?SERVER_URL],
    ok = mongoose_helper:successful_rpc(Node, mongoose_config, set_opt, UrlArgs),
    start_system_metrics_service(Node, Opts).

start_system_metrics_service(Node, ExtraOpts) ->
    Opts = config([services, service_mongoose_system_metrics], ExtraOpts),
    dynamic_services:ensure_started(Node, service_mongoose_system_metrics, Opts).

disable_system_metrics(Node) ->
    dynamic_services:ensure_stopped(Node, service_mongoose_system_metrics),
    mongoose_helper:successful_rpc(Node, mongoose_config, unset_opt, [ google_analytics_url ]).

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

events_are_reported_to_primary_tracking_id() ->
    events_are_reported_to_tracking_ids([primary_tracking_id()]).

events_are_reported_to_both_tracking_ids() ->
    events_are_reported_to_tracking_ids([primary_tracking_id(), ?TRACKING_ID_EXTRA]).

primary_tracking_id() ->
    case os:getenv("CI") of
        "true" -> ?TRACKING_ID_CI;
        _ -> ?TRACKING_ID
    end.

events_are_reported_to_tracking_ids(ConfiguredTrackingIds) ->
    Tab = ets:tab2list(?ETS_TABLE),
    ActualTrackingIds = lists:usort([InstanceId || #event{instance_id = InstanceId} <- Tab]),
    ExpectedTrackingIds = lists:sort([list_to_binary(Tid) || #{id := Tid} <- ConfiguredTrackingIds]),
    ExpectedTrackingIds =:= ActualTrackingIds.

is_feature_reported(EventName, Key) ->
    length(match_events(EventName, Key)) > 0.

is_feature_reported(EventName, Key, Value) ->
    length(match_events(EventName, Key, Value)) > 0.

is_module_opt_reported(Module, Key, Value) ->
    length(get_matched_events_for_module(Module, Key, Value)) > 0.

is_mongoose_version_reported() ->
    is_feature_reported(<<"cluster">>, <<"version">>).

is_cluster_uptime_reported() ->
    is_feature_reported(<<"cluster">>, <<"uptime">>).

are_xmpp_components_reported() ->
    is_feature_reported(<<"cluster">>, <<"component">>).

is_config_type_reported() ->
    IsToml = is_feature_reported(<<"cluster">>, <<"config_type">>, <<"toml">>),
    IsCfg = is_feature_reported(<<"cluster">>, <<"config_type">>, <<"cfg">>),
    IsToml orelse IsCfg.

is_api_reported() ->
    is_in_table(<<"http_api">>).

are_transport_mechanisms_reported() ->
    is_in_table(<<"transport_mechanism">>).

are_outgoing_pools_reported() ->
    is_in_table(<<"outgoing_pool">>).

is_iq_count_reported() ->
    is_feature_reported(<<"xmpp_stanza_count">>,
                        <<"stanza_type">>,
                        <<"xmppIqSent">>).

is_message_count_reported() ->
    XmppMessageSent = is_feature_reported(<<"xmpp_stanza_count">>,
                                          <<"stanza_type">>,
                                          <<"xmppMessageSent">>),
    XmppMessageReceived = is_feature_reported(<<"xmpp_stanza_count">>,
                                               <<"stanza_type">>,
                                               <<"xmppMessageReceived">>),
    XmppMessageSent andalso XmppMessageReceived. 

assert_message_count_is_incremented(Sent, Received) ->
    assert_increment(<<"xmppMessageSent">>, Sent),
    assert_increment(<<"xmppMessageReceived">>, Received).

assert_increment(EventCategory, InitialValue) ->
    Events = match_events(<<"xmpp_stanza_count">>, <<"stanza_type">>, EventCategory),
    % expect exactly one event with an increment of 1
    SeekedEvent = [Event || Event = #event{params = #{<<"total">> := Total, <<"increment">> := 1}}
        <- Events, Total == InitialValue + 1],
    ?assertMatch([_], SeekedEvent).

get_metric_value(EventCategory) ->
    [#event{params = #{<<"total">> := Value}} | _] = match_events(<<"xmpp_stanza_count">>, <<"stanza_type">>, EventCategory),
    Value.

more_than_one_component_is_reported() ->
    Events = match_events(<<"cluster">>),
    lists:any(fun(#event{params = Params}) ->
                       maps:get(<<"component">>, Params) > 0
              end, Events).

match_events(EventName) ->
    ets:match_object(?ETS_TABLE, #event{name = EventName, _ = '_'}).

match_events(EventName, ParamKey) ->
    Res = ets:match_object(?ETS_TABLE, #event{name = EventName, _ = '_'}),
    [Event || Event = #event{params = #{ParamKey := _}} <- Res].

match_events(EventName, ParamKey, ParamValue) ->
    Res = ets:match_object(?ETS_TABLE, #event{name = EventName, _ = '_'}),
    [Event || Event = #event{params = #{ParamKey := Value}} <- Res, Value == ParamValue].

get_matched_events_for_module(ParamModule, Key, ParamValue) ->
    Res = ets:match_object(?ETS_TABLE, #event{name = <<"module_with_opt">>, _ = '_'}),
    [Event || Event = #event{params = #{<<"module">> := Module, Key := Value}} <- Res,
         Value == ParamValue, Module == ParamModule].

%%--------------------------------------------------------------------
%% Cowboy handlers
%%--------------------------------------------------------------------
handler_init(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    #{measurement_id := InstanceId, api_secret := AppSecret} = cowboy_req:match_qs([measurement_id, api_secret], Req0),
    BodyMap = jiffy:decode(Body, [return_maps]),
    EventTab = maps:get(<<"events">>, BodyMap), 
    ClientID = maps:get(<<"client_id">>, BodyMap),
    lists:map(
        fun(Event) ->
            EventRecord = #event{name = maps:get(<<"name">>, Event),
                                 params = maps:get(<<"params">>, Event),
                                 client_id = ClientID,
                                 instance_id = InstanceId,
                                 app_secret = AppSecret},
            %% TODO there is a race condition when table is not available
            ets:insert(?ETS_TABLE, EventRecord)
        end, EventTab),
    Req1 = cowboy_req:reply(200, #{}, <<>>, Req),
    {ok, Req1, no_state}.
