-module(graphql_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute/3]).

-define(assertAdminAuth(Auth, Data), assert_auth(atom_to_binary(Auth), Data)).
-define(assertUserAuth(Username, Auth, Data),
        assert_auth(#{<<"username">> => Username,
                      <<"authStatus">> => atom_to_binary(Auth)}, Data)).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, cowboy_handler},
     {group, admin_handler},
     {group, user_handler}].

groups() ->
    [{cowboy_handler, [parallel], cowboy_handler()},
     {user_handler, [parallel], user_handler()},
     {admin_handler, [parallel], admin_handler()}].

cowboy_handler() ->
    [can_connect_to_admin,
     can_connect_to_user].

user_handler() ->
    [user_checks_auth,
     auth_user_checks_auth | common_tests()].
admin_handler() ->
    [admin_checks_auth,
     auth_admin_checks_auth | common_tests()].

common_tests() ->
    [can_load_graphiql].

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    dynamic_modules:save_modules(domain_helper:host_type(), Config1).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(admin_handler, Config) ->
    graphql_helper:init_admin_handler(Config);
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
    ?assertMatch({{<<"400">>, <<"Bad Request">>}, _}, execute(admin, #{}, undefined)).

can_connect_to_user(_Config) ->
    ?assertMatch({{<<"400">>, <<"Bad Request">>}, _}, execute(user, #{}, undefined)).

can_load_graphiql(Config) ->
    Ep = ?config(schema_endpoint, Config),
    {Status, Html} = get_graphiql_website(Ep),
    ?assertEqual({<<"200">>, <<"OK">>}, Status),
    ?assertNotEqual(nomatch, binary:match(Html, <<"Loading...">>)).

user_checks_auth(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Body = #{query => "{ checkAuth { username authStatus } }"},
    StatusData = execute(Ep, Body, undefined),
    ?assertUserAuth(null, 'UNAUTHORIZED', StatusData).

auth_user_checks_auth(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Password = user_password(alice),
            AliceJID = escalus_client:short_jid(Alice),
            Ep = ?config(schema_endpoint, Config),
            Body = #{query => "{ checkAuth { username authStatus } }"},
            StatusData = execute(Ep, Body, {AliceJID, Password}),
            ?assertUserAuth(AliceJID, 'AUTHORIZED', StatusData)
        end).

admin_checks_auth(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Body = #{query => "{ checkAuth }"},
    StatusData = execute(Ep, Body, undefined),
    ?assertAdminAuth('UNAUTHORIZED', StatusData).

auth_admin_checks_auth(Config) ->
    Ep = ?config(schema_endpoint, Config),
    Opts = ?config(listener_opts, Config),
    User = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    Body = #{query => "{ checkAuth }"},
    StatusData = execute(Ep, Body, {User, Password}),
    ?assertAdminAuth('AUTHORIZED', StatusData).

%% Helpers

assert_auth(Auth, {Status, Data}) ->
    ?assertEqual({<<"200">>, <<"OK">>}, Status),
    ?assertMatch(#{<<"data">> := #{<<"checkAuth">> := Auth}}, Data).

user_password(User) ->
    [{User, Props}] = escalus:get_users([User]),
    proplists:get_value(password, Props).

get_graphiql_website(EpName) ->
    Request =
      #{port => graphql_helper:get_listener_port(EpName),
        role => {graphql, atom_to_binary(EpName)},
        method => <<"GET">>,
        headers => [{<<"Accept">>, <<"text/html">>}],
        return_maps => true,
        path => "/graphql"},
    rest_helper:make_request(Request).
