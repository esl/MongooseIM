-module(gen_server_unexpected_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log_helper.hrl").

all() ->
    [{group, Name} || Name <- servers()].

groups() ->
    [{Name, [], cases()} || Name <- servers()].

servers() ->
    [mongoose_ets_heir,
     gen_hook,
     mongoose_instrument,
     mongoose_collector,
     mongoose_subdomain_core,
     mongoose_lazy_routing,
     mongoose_domain_core,
     mongoose_domain_db_cleaner,
     mod_global_distrib_hosts_refresher,
     mod_offline,
     mongoose_batch_worker,
     mongoose_aggregator_worker].

cases() ->
    [unexpected_call,
     unexpected_cast,
     unexpected_info].

init_per_suite(Config) ->
    log_helper:set_up(),
    mongoose_config:set_opts(opts()),
    meck:new(mongoose_domain_sql, [no_link]),
    meck:expect(mongoose_domain_sql, get_minmax_event_id, fun() -> {1, 1} end),
    Config.

end_per_suite(_Config) ->
    meck:unload(),
    mongoose_config:erase_opts(),
    log_helper:tear_down().

opts() ->
    #{instrumentation => config_parser_helper:default_config([instrumentation])}.

init_per_group(Name, Config) ->
    [{server, Name} | Config].

end_per_group(_Name, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    {ok, _Pid} = start_server(?config(server, Config)),
    log_helper:subscribe(),
    Config.

end_per_testcase(_Case, Config) ->
    log_helper:unsubscribe(),
    gen_server:stop(?config(server, Config)).

start_server(mongoose_ets_heir) ->
    mongoose_ets_heir:start_link();
start_server(gen_hook) ->
    gen_hook:start_link();
start_server(mongoose_instrument) ->
    mongoose_instrument:start_link();
start_server(mongoose_collector) ->
    mongoose_collector:start_link(mongoose_collector, #{host_type => <<"type">>,
                                                       action => fun(_HostType, _Opts) -> ok end,
                                                       opts => #{},
                                                       interval => timer:hours(1)});
start_server(mongoose_subdomain_core) ->
    mongoose_subdomain_core:start_link();
start_server(mongoose_lazy_routing) ->
    mongoose_lazy_routing:start_link();
start_server(mongoose_domain_core) ->
    mongoose_domain_core:start_link([], []);
start_server(mongoose_domain_db_cleaner) ->
    mongoose_domain_db_cleaner:start_link(#{event_cleaning_interval => 3600,
                                            event_max_age => 3600});
start_server(mod_global_distrib_hosts_refresher) ->
    mod_global_distrib_hosts_refresher:start_link(#{local_host => <<"localhost">>,
                                                    hosts_refresh_interval => timer:hours(1)});
start_server(mod_offline) ->
    mod_offline:start_link(mod_offline, <<"type">>, max_user_offline_messages);
start_server(mongoose_batch_worker) ->
    gen_server:start_link({local, mongoose_batch_worker}, mongoose_batch_worker,
                          #{host_type => <<"type">>,
                            pool_id => test_pool,
                            batch_size => 10,
                            flush_interval => timer:hours(1),
                            flush_callback => fun(_Tasks, _Extra) -> ok end,
                            flush_extra => #{}}, []);
start_server(mongoose_aggregator_worker) ->
    gen_server:start_link({local, mongoose_aggregator_worker}, mongoose_aggregator_worker,
                          #{host_type => <<"type">>,
                            pool_id => test_pool,
                            request_callback => fun(_Task, _Extra) -> ok end,
                            aggregate_callback => fun(_Old, New, _Extra) -> {ok, New} end,
                            flush_extra => #{}}, []).

%% Test cases

unexpected_call(Config) ->
    % GIVEN
    Name = ?config(server, Config),
    Pid = whereis(Name),

    % WHEN
    _ = gen_server:call(Name, bad_call),

    % THEN
    ?assertLog(warning, #{what := unexpected_call, msg := bad_call}, 1000),
    ?assertEqual(Pid, whereis(Name)).

unexpected_cast(Config) ->
    % GIVEN
    Name = ?config(server, Config),
    Pid = whereis(Name),

    % WHEN
    gen_server:cast(Name, bad_cast),

    % THEN
    ?assertLog(warning, #{what := unexpected_cast, msg := bad_cast}, 1000),
    ?assertEqual(Pid, whereis(Name)).

unexpected_info(Config) ->
    % GIVEN
    Name = ?config(server, Config),
    Pid = whereis(Name),

    % WHEN
    Name ! bad_info,

    % THEN
    ?assertLog(warning, #{what := unexpected_info, msg := bad_info}, 1000),
    ?assertEqual(Pid, whereis(Name)).
