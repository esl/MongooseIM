-module(graphql_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [load_test_schema/2]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, cowboy_handler},
     {group, admin_handler},
     {group, user_handler}].

groups() ->
    [{cowboy_handler, [parallel], [can_connect_to_admin,
                           can_connect_to_user
                          ]},
     {user_handler, [parallel], [wrong_creds_cannot_access_protected_types,
                         unauth_cannot_access_protected_types,
                         unauth_can_access_unprotected_types,
                         can_execute_query_with_variables,
                         auth_user_can_access_protected_types
                        ]},
     {admin_handler, [parallel], [wrong_creds_cannot_access_protected_types,
                          unauth_cannot_access_protected_types,
                          unauth_can_access_unprotected_types,
                          can_execute_query_with_variables,
                          auth_admin_can_access_protected_types]}].

init_per_suite(Config) ->
    % reset endpoints and load test schema
    ok = load_test_schema(admin, Config),
    ok = load_test_schema(user, Config),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    % reinit endpoints with original schemas
    ok = rpc(mim(), mongoose_graphql, init, []),
    escalus:end_per_suite(Config).

init_per_group(admin_handler, Config) ->
    Endpoint = admin,
    Opts = get_listener_opts(Endpoint),
    case proplists:is_defined(username, Opts) of
        true ->
            [{schema_endpoint, Endpoint} | Config];
        false ->
            {skipped, <<"Admin credentials are not defined in config">>}
    end;
init_per_group(user_handler, Config) ->
    Config1 = escalus:create_users(Config, escalus:get_users([alice])),
    [{schema_endpoint, user} | Config1];
init_per_group(_, Config) ->
    Config.

end_per_group(user_handler, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice]));
end_per_group(_, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

can_connect_to_admin(_Config) ->
    ?assertMatch({{<<"400">>,<<"Bad Request">>}, _}, execute(admin, #{}, undefined)).

can_connect_to_user(_Config) ->
    ?assertMatch({{<<"400">>,<<"Bad Request">>}, _}, execute(user, #{}, undefined)).

unauth_cannot_access_protected_types(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, undefined),
    assert_no_permissions(Status, Data).

unauth_can_access_unprotected_types(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Body = #{query => "mutation { field }"},
    {Status, Data} = execute(Ep, Body, undefined),
    assert_access_granted(Status, Data).

wrong_creds_cannot_access_protected_types(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {<<"user">>, <<"wrong_password">>}),
    assert_no_permissions(Status, Data).

auth_user_can_access_protected_types(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Password = user_password(alice),
            AliceJID = escalus_client:short_jid(Alice),
            Ep = ?config(schema_endpoint, Config),
            Body = #{query => "{ field }"},
            {Status, Data} = execute(Ep, Body, {AliceJID, Password}),
            assert_access_granted(Status, Data)
        end).

auth_admin_can_access_protected_types(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Opts = get_listener_opts(Ep),
    User = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    Body = #{query => "{ field }"},
    {Status, Data} = execute(Ep, Body, {User, Password}),
    assert_access_granted(Status, Data).

can_execute_query_with_variables(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Body = #{query => "mutation M1($value: String!){ id(value: $value) } query Q1{ field }",
             variables => #{value => <<"Hello">>},
             operationName => <<"M1">>
            },
    {Status, Data} = execute(Ep, Body, undefined),
    ?assertEqual({<<"200">>,<<"OK">>}, Status),
    % operation M1 was executed, because id is in path
    % access was granted, an error was returned because valid resolver was not defined
    ?assertMatch(#{<<"data">> := #{<<"id">> := null},
                   <<"errors">> :=
                       [#{<<"extensions">> := #{<<"code">> := <<"resolver_crash">>},
                          <<"path">> := [<<"id">>]}]},
                 Data).

%% Helpers

assert_no_permissions(Status, Data) ->
    ?assertEqual({<<"400">>,<<"Bad Request">>}, Status),
    ?assertMatch(#{<<"errors">> := [#{<<"message">> := <<"no_permissions">>}]}, Data).

assert_access_granted(Status, Data) ->
    ?assertEqual({<<"200">>,<<"OK">>}, Status),
    % access was granted, an error was returned because valid resolver was not defined
    ?assertMatch(#{<<"errors">> :=
                   [#{<<"extensions">> :=
                     #{<<"code">> := <<"resolver_crash">>}}]}, Data).

user_password(User) ->
    [{User, Props}] = escalus:get_users([User]),
    proplists:get_value(password, Props).

get_port(EpName) ->
    {PortIpNet, ejabberd_cowboy, _Opts} = get_listener_config(EpName),
    element(1, PortIpNet).

get_listener_opts(EpName) ->
    {_, ejabberd_cowboy, Opts} = get_listener_config(EpName),
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Opts),
    [Opts2] = lists:filtermap(
        fun
            ({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                {true, Args};
            (_) ->
                false
        end, Modules),
    Opts2.

get_listener_config(EpName) ->
    Listeners = rpc(mim(), mongoose_config, get_opt, [listen]),
    [{_, ejabberd_cowboy, _} = Config] =
        lists:filter(fun(Config) -> is_graphql_config(Config, EpName) end, Listeners),
    Config.

is_graphql_config({_PortIpNet, ejabberd_cowboy, Opts}, EpName) ->
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Opts),
    lists:any(fun({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                      atom_to_binary(EpName) == proplists:get_value(schema_endpoint, Args);
                 (_) -> false
              end, Modules);
is_graphql_config(_, _EpName) ->
    false.

execute(EpName, Body, Creds) ->
    Request =
      #{port => get_port(EpName),
        role => {graphql, atom_to_binary(EpName)},
        method => <<"POST">>,
        return_maps => true,
        creds => Creds,
        path => "/graphql",
        body => Body},
    rest_helper:make_request(Request).
