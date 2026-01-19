-module(mongoose_graphql_admin_api_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-type listener_opts() :: #{endpoint_schema := binary(),
                           atom() => any()}.

all() ->
    [{group, admin_api_listener}].

groups() ->
    [{admin_api_listener, [parallel], [admin_server_get_host_types,
                                       admin_server_get_global_info]}].

init_per_suite(Config) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(admin_api_listener, Config) ->
    meck:new(mongoose_config, [passthrough, no_link]),
    meck:expect(mongoose_config, get_opt,
                fun(hosts) -> [<<"localhost">>];
                    (host_types) -> [];
                    (internal_databases) -> #{mnesia => #{}};
                    ([{auth, <<"localhost">>}, methods]) -> [internal];
                    (Key) -> meck:passthrough([Key])
                end),
    meck:new(gen_mod, [passthrough, no_link]),
    meck:expect(gen_mod, loaded_modules_with_opts,
                fun(<<"localhost">>) ->
                    #{mod_roster => #{backend => rdbms},
                      mod_ping => #{}};
                   (_) -> #{}
                end),
    meck:new(mongoose_domain_api, [passthrough, no_link]),
    meck:expect(mongoose_domain_api, get_domains_by_host_type,
                fun(<<"localhost">>) -> [<<"localhost">>, <<"example.com">>];
                   (_) -> []
                end),
     meck:expect(mongoose_domain_api, get_host_type,
                     fun(<<"localhost">>) -> {ok, <<"localhost">>};
                         (_) -> {error, not_found}
                     end),
    meck:new(mongoose_service, [passthrough, no_link]),
    meck:expect(mongoose_service, loaded_services_with_opts,
                fun() -> #{service_domain_db => #{}} end),
    ListenerOpts = #{username => <<"admin">>,
                     password => <<"secret">>,
                     schema_endpoint => admin},
    init_listener_with_init(5562, admin_api_listener, ListenerOpts, fun mongoose_graphql:init/0, Config);
init_per_group(_G, Config) ->
    Config.

end_per_group(admin_api_listener, Config) ->
    meck:unload(mongoose_config),
    meck:unload(gen_mod),
    meck:unload(mongoose_domain_api),
    meck:unload(mongoose_service),
    ?config(test_process, Config) ! stop,
    Config;
end_per_group(_, Config) ->
    Config.

admin_server_get_host_types(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Query = <<"query { server { hostTypes { name domains authMethods modules { name backend } } } }">>,
    Body = #{query => Query},
    {Status, Data} = execute(Ep, Body, {<<"admin">>, <<"secret">>}),
    ?assertEqual({<<"200">>, <<"OK">>}, Status),
    ?assertMatch(#{<<"data">> := #{<<"server">> := #{<<"hostTypes">> := HostTypes}}} when is_list(HostTypes), Data),
    #{<<"data">> := #{<<"server">> := #{<<"hostTypes">> := HostTypes}}} = Data,
    % Validate structure of first host type (if any)
    case HostTypes of
        [FirstHostType | _] ->
            ?assertMatch(#{<<"name">> := _}, FirstHostType),
            ?assertMatch(#{<<"domains">> := Domains} when is_list(Domains), FirstHostType),
            ?assertMatch(#{<<"modules">> := Modules} when is_list(Modules), FirstHostType),
            ?assertMatch(#{<<"authMethods">> := AuthMethods} when is_list(AuthMethods), FirstHostType),
            #{<<"modules">> := Modules} = FirstHostType,
            % Validate module structure
            lists:foreach(
                fun(Module) ->
                    ?assertMatch(#{<<"name">> := _}, Module),
                    % Backend may be null or a string
                    case maps:get(<<"backend">>, Module, undefined) of
                        null -> ok;
                        undefined -> ok;
                        Backend when is_binary(Backend) -> ok
                    end
                end,
                Modules
            );
        [] ->
            ct:pal("No host types returned (empty list)")
    end.

admin_server_get_global_info(Config) ->
    Ep = ?config(endpoint_addr, Config),
    Query = <<"query { server { globalInfo { services { name } internalDatabases } } }">>,
    Body = #{query => Query},
    {Status, Data} = execute(Ep, Body, {<<"admin">>, <<"secret">>}),
    ?assertEqual({<<"200">>, <<"OK">>}, Status),
    #{<<"data">> := #{<<"server">> := #{<<"globalInfo">> := GlobalInfo}}} = Data,
    ?assertMatch(#{<<"services">> := Services, <<"internalDatabases">> := DBs}
                   when is_list(Services) andalso is_list(DBs), GlobalInfo).

%% Helpers

-spec init_listener_with_init(integer(), atom(), listener_opts(), fun(() -> any()), [{atom(), term()}]) ->
    [{atom(), term()}].
init_listener_with_init(Port, Ref, ListenerOpts, InitFun, Config) ->
    Parent = self(),
    Pid = spawn(fun() ->
                    _ = InitFun(),
                    Name = list_to_atom("gql_listener_" ++ atom_to_list(Ref)),
                    ok = start_listener(Name, Port, ListenerOpts),
                    Parent ! {listener_started, self()},
                    receive
                        stop ->
                            ok
                    end
                end),
    receive
        {listener_started, Pid} -> ok
    after 5000 ->
        error(timeout_starting_listener)
    end,
    [{test_process, Pid}, {endpoint_addr, "http://localhost:" ++ integer_to_list(Port)} | Config].

-spec start_listener(atom(), integer(), listener_opts()) -> ok.
start_listener(Ref, Port, Opts) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/graphql", mongoose_graphql_handler, Opts}]}
    ]),
    {ok, _} = cowboy:start_clear(Ref,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

-spec execute(binary(), map(), undefined | {binary(), binary()}) -> {{binary(), binary()}, map()}.
execute(EpAddr, Body, undefined) ->
    post_request(EpAddr, [], Body);
execute(EpAddr, Body, {Username, Password}) ->
    Creds = base64:encode(<<Username/binary, ":", Password/binary>>),
    Headers = [{<<"Authorization">>, <<"Basic ", Creds/binary>>}],
    post_request(EpAddr, Headers, Body).

post_request(EpAddr, HeadersIn, Body) when is_binary(Body) ->
    {ok, Client} = fusco:start(EpAddr, []),
    Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"Request-Id">>, random_request_id()} | HeadersIn],
    {ok, {ResStatus, _, ResBody, _, _}} = Res =
        fusco:request(Client, <<"/graphql">>, <<"POST">>, Headers, Body, 5000),
    fusco:disconnect(Client),
    ct:log("~p", [Res]),
    {ResStatus, jiffy:decode(ResBody, [return_maps])};
post_request(Ep, HeadersIn, Body) ->
    post_request(Ep, HeadersIn, jiffy:encode(Body)).

random_request_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(8), lowercase).
