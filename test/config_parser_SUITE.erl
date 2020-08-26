-module(config_parser_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("ejabberd_config.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-define(err(Expr), ?assertError(_, Expr)).

-define(HOST, <<"myhost">>).

-import(mongoose_config_parser_toml, [parse/1]).

all() ->
    [{group, equivalence},
     {group, general}].

groups() ->
    [{equivalence, [parallel], [sample_pgsql,
                                miscellaneous,
                                s2s]},
     {general, [parallel], [loglevel,
                            hosts,
                            registration_timeout,
                            language,
                            all_metrics_are_global,
                            sm_backend,
                            max_fsm_queue,
                            rdbms_server_type,
                            override,
                            pgsql_users_number_estimate,
                            route_subdomain,
                            mongooseimctl_access_commands,
                            routing_modules,
                            replaced_wait_timeout,
                            hide_service_name]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    ok.

sample_pgsql(Config) ->
    CfgPath = ejabberd_helper:data(Config, "mongooseim-pgsql.cfg"),
    State1 = mongoose_config_parser_cfg:parse_file(CfgPath),
    Hosts1 = mongoose_config_parser:state_to_host_opts(State1),
    Opts1 = mongoose_config_parser:state_to_opts(State1),

    TOMLPath = ejabberd_helper:data(Config, "mongooseim-pgsql.toml"),
    State2 = mongoose_config_parser_toml:parse_file(TOMLPath),
    Hosts2 = mongoose_config_parser:state_to_host_opts(State2),
    Opts2 = mongoose_config_parser:state_to_opts(State2),
    ?eq(Hosts1, Hosts2),
    compare_unordered_lists(lists:filter(fun filter_config/1, Opts1), Opts2,
                            fun handle_config_option/2).

miscellaneous(Config) ->
    CfgPath = ejabberd_helper:data(Config, "miscellaneous.cfg"),
    State1 = mongoose_config_parser_cfg:parse_file(CfgPath),
    Hosts1 = mongoose_config_parser:state_to_host_opts(State1),
    Opts1 = mongoose_config_parser:state_to_opts(State1),

    TOMLPath = ejabberd_helper:data(Config, "miscellaneous.toml"),
    State2 = mongoose_config_parser_toml:parse_file(TOMLPath),
    Hosts2 = mongoose_config_parser:state_to_host_opts(State2),
    Opts2 = mongoose_config_parser:state_to_opts(State2),

    ?eq(Hosts1, Hosts2),
    compare_unordered_lists(lists:filter(fun filter_config/1, Opts1), Opts2,
                        fun handle_config_option/2).

s2s(Config) ->
    Cfg_Path = ejabberd_helper:data(Config, "s2s_only.cfg"),
    State1 = mongoose_config_parser_cfg:parse_file(Cfg_Path),
    Opts1 = mongoose_config_parser:state_to_opts(State1),

    TOML_path = ejabberd_helper:data(Config, "s2s_only.toml"),
    State2 = mongoose_config_parser_toml:parse_file(TOML_path),
    Opts2 = mongoose_config_parser:state_to_opts(State2),

    compare_unordered_lists(lists:filter(fun filter_config/1, Opts1), Opts2,
                            fun handle_config_option/2).

loglevel(_Config) ->
    ?eq([#local_config{key = loglevel, value = debug}],
        parse(#{<<"general">> => #{<<"loglevel">> => <<"debug">>}})),
    ?err(parse(#{<<"general">> => #{<<"loglevel">> => <<"bebug">>}})).

hosts(_Config) ->
    ?eq([#config{key = hosts, value = [<<"host1">>, <<"host2">>]}],
        parse(#{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host2">>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<"what is this?">>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<>>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => []}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host1">>]}})).

registration_timeout(_Config) ->
    ?eq([#local_config{key = registration_timeout, value = infinity}],
        parse(#{<<"general">> => #{<<"registration_timeout">> => <<"infinity">>}})),
    ?eq([#local_config{key = registration_timeout, value = 300}],
        parse(#{<<"general">> => #{<<"registration_timeout">> => 300}})),
    ?err(parse(#{<<"general">> => #{<<"registration_timeout">> => 0}})).

language(_Config) ->
    ?eq([#config{key = language, value = <<"en">>}],
        parse(#{<<"general">> => #{<<"language">> => <<"en">>}})),
    ?err(parse(#{<<"general">> => #{<<"language">> => <<>>}})).

all_metrics_are_global(_Config) ->
    ?eq([#local_config{key = all_metrics_are_global, value = true}],
        parse(#{<<"general">> => #{<<"all_metrics_are_global">> => true}})),
    ?err(parse(#{<<"general">> => #{<<"all_metrics_are_global">> => <<"true">>}})).

sm_backend(_Config) ->
    ?eq([#config{key = sm_backend, value = {mnesia, []}}],
        parse(#{<<"general">> => #{<<"sm_backend">> => <<"mnesia">>}})),
    ?eq([#config{key = sm_backend, value = {redis, []}}],
        parse(#{<<"general">> => #{<<"sm_backend">> => <<"redis">>}})),
    ?err(parse(#{<<"general">> => #{<<"sm_backend">> => <<"amnesia">>}})).

max_fsm_queue(_Config) ->
    ?eq([#local_config{key = max_fsm_queue, value = 100}],
        parse(#{<<"general">> => #{<<"max_fsm_queue">> => 100}})),
    ?err(parse(#{<<"general">> => #{<<"max_fsm_queue">> => -10}})).

http_server_name(_Config) ->
    ?eq([#local_config{key = cowqboy_server_name, value = "myserver"}],
        parse(#{<<"general">> => #{<<"http_server_name">> => <<"my server">>}})),
    ?err(parse(#{<<"general">> => #{<<"http_server_name">> => #{}}})).

rdbms_server_type(_Config) ->
    ?eq([#local_config{key = rdbms_server_type, value = mssql}],
        parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"mssql">>}})),
    ?eq([#local_config{key = rdbms_server_type, value = pgsql}],
        parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"pgsql">>}})),
    ?err(parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"nosql">>}})).

override(_Config) ->
    ?eq([{override, local}, {override, global}, {override, acls}],
        parse(#{<<"general">> => #{<<"override">> => [<<"local">>, <<"global">>, <<"acls">>]}})),
    ?err(parse(#{<<"general">> => #{<<"override">> => [<<"local">>, <<"global">>, <<"local">>]}})),
    ?err(parse(#{<<"general">> => #{<<"override">> => [<<"pingpong">>]}})).

pgsql_users_number_estimate(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"pgsql_users_number_estimate">> => true}}),
    ?eq([#local_config{key = {pgsql_users_number_estimate, ?HOST}, value = true}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"pgsql_users_number_estimate">> => 1200}})).

route_subdomain(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"route_subdomain">> => <<"s2s">>}}),
    ?eq([#local_config{key = {route_subdomain, ?HOST}, value = s2s}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"route_subdomain">> => <<"c2s">>}})).

mongooseimctl_access_commands(_Config) ->
    AccessRule = #{<<"access_rule">> => <<"local">>,
                   <<"commands">> => [<<"join_cluster">>],
                   <<"argument_restrictions">> => #{<<"node">> => <<"mim1@host1">>}},
    ?eq([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, ["join_cluster"], [{node, "mim1@host1"}]}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> => [AccessRule]}})),
    ?err(parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                        [AccessRule#{<<"cat">> => <<"meow">>}]
                                   }})).

routing_modules(_Config) ->
    ?eq([#local_config{key = routing_modules, value = [mongoose_router_global,
                                                       mongoose_router_localdomain]}],
        parse(#{<<"general">> => #{<<"routing_modules">> => [<<"mongoose_router_global">>,
                                                             <<"mongoose_router_localdomain">>]}})),
    ?err(parse(#{<<"general">> => #{<<"routing_modules">> => [<<"moongoose_router_global">>]}})).

replaced_wait_timeout(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"replaced_wait_timeout">> => 1000}}),
    ?eq([#local_config{key = {replaced_wait_timeout, ?HOST}, value = 1000}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"replaced_wait_timeout">> => 0}})).

hide_service_name(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"hide_service_name">> => false}}),
    ?eq([#local_config{key = {hide_service_name, ?HOST}, value = false}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"hide_service_name">> => []}})).

filter_config(#config{key = required_files}) ->
    false; % not supported yet in TOML
filter_config(_) -> true.

handle_config_option(#config{key = K1, value = V1},
                     #config{key = K2, value = V2}) ->
    ?eq(K1, K2),
    compare_values(K1, V1, V2);
handle_config_option(#local_config{key = K1, value = V1},
                     #local_config{key = K2, value = V2}) ->
    ?eq(K1, K2),
    compare_values(K1, V1, V2);
handle_config_option(Opt1, Opt2) ->
    ?eq(Opt1, Opt2).

compare_values(listen, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_listener/2);
compare_values({auth_opts, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_auth_opt/2);
compare_values(outgoing_pools, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_conn_pool/2);
compare_values({modules, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_item_with_opts/2);
compare_values({services, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_item_with_opts/2);
compare_values({auth_method, _}, V1, V2) when is_atom(V1) ->
    ?eq([V1], V2);
compare_values({s2s_addr, _}, {_, _, _, _} = IP1, IP2) ->
    ?eq(inet:ntoa(IP1), IP2);
compare_values(services, V1, V2) ->
    MetricsOpts1 = proplists:get_value(service_mongoose_system_metrics, V1),
    MetricsOpts2 = proplists:get_value(service_mongoose_system_metrics, V2),
    compare_unordered_lists(MetricsOpts1, MetricsOpts2);
compare_values(K, V1, V2) ->
    ?eq({K, V1}, {K, V2}).

handle_listener({P1, M1, O1}, {P2, M2, O2}) ->
    ?eq(P1, P2),
    ?eq(M1, M2),
    compare_unordered_lists(O1, O2, fun handle_listener_option/2).

handle_listener_option({modules, M1}, {modules, M2}) ->
    compare_unordered_lists(M1, M2, fun handle_listener_module/2);
handle_listener_option({transport_options, O1}, {transport_options, O2}) ->
    compare_unordered_lists(O1, O2);
handle_listener_option(V1, V2) -> ?eq(V1, V2).

handle_listener_module({H1, P1, M1}, M2) ->
    handle_listener_module({H1, P1, M1, []}, M2);
handle_listener_module({H1, P1, M1, O1}, {H2, P2, M2, O2}) ->
    ?eq(H1, H2),
    ?eq(P1, P2),
    ?eq(M1, M2),
    compare_listener_module_options(M1, O1, O2).

compare_listener_module_options(mod_websockets,
                                [{ejabberd_service, S1}], [{ejabberd_service, S2}]) ->
    compare_unordered_lists(S1, S2);
compare_listener_module_options(_, O1, O2) ->
    ?eq(O1, O2).

handle_auth_opt({cyrsasl_external, M}, {cyrsasl_external, [M]}) -> ok;
handle_auth_opt(V1, V2) -> ?eq(V1, V2).

handle_item_with_opts({M1, O1}, {M2, O2}) ->
    ?eq(M1, M2),
    compare_unordered_lists(O1, O2).

handle_conn_pool({Type1, Scope1, Tag1, POpts1, COpts1},
                 {Type2, Scope2, Tag2, POpts2, COpts2}) ->
    ?eq(Type1, Type2),
    ?eq(Scope1, Scope2),
    ?eq(Tag1, Tag2),
    compare_unordered_lists(POpts1, POpts2),
    compare_unordered_lists(COpts1, COpts2, fun handle_conn_opt/2).

handle_conn_opt({server, {D1, H1, DB1, U1, P1, O1}},
                {server, {D2, H2, DB2, U2, P2, O2}}) ->
    ?eq(D1, D2),
    ?eq(H1, H2),
    ?eq(DB1, DB2),
    ?eq(U1, U2),
    ?eq(P1, P2),
    compare_unordered_lists(O1, O2, fun handle_db_server_opt/2);
handle_conn_opt(V1, V2) -> ?eq(V1, V2).

handle_db_server_opt({ssl_opts, O1}, {ssl_opts, O2}) ->
    compare_unordered_lists(O1, O2);
handle_db_server_opt(V1, V2) -> ?eq(V1, V2).

compare_unordered_lists(L1, L2) ->
    compare_unordered_lists(L1, L2, fun(V1, V2) -> ?eq(V1, V2) end).

compare_unordered_lists(L1, L2, F) ->
    SL1 = lists:sort(L1),
    SL2 = lists:sort(L2),
    compare_ordered_lists(SL1, SL2, F).

compare_ordered_lists([H1|T1], [H1|T2], F) ->
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([H1|T1], [H2|T2], F) ->
    try F(H1, H2)
    catch C:R:S ->
            ct:fail({C, R, S})
    end,
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([], [], _) ->
    ok.
