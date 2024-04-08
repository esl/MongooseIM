%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongooseimctl_SUITE).
-compile([export_all, nowarn_export_all, nowarn_shadow_vars]).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(mongooseimctl_helper, [mongooseimctl/3]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1]).
-import(domain_helper, [domain/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, graphql},
     {group, help},
     {group, server}
    ].

groups() ->
    [{graphql, [], graphql()},
     {help, [], help()},
     {server, [], server()}].

graphql() ->
    [graphql_wrong_arguments_number,
     can_execute_admin_queries_with_permissions,
     can_handle_execution_error,
     graphql_error_unknown_command_with_args,
     graphql_error_unknown_command_without_args,
     graphql_error_unknown_category_with_args,
     graphql_error_unknown_category_without_args,
     graphql_no_command,
     graphql_error_invalid_args,
     graphql_error_invalid_arg_value,
     graphql_error_no_arg_value,
     graphql_error_missing_args,
     graphql_error_unknown_arg,
     graphql_arg_help,
     graphql_command].

help() ->
    [default_help].

server() ->
    [server_status,
     server_is_started].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config) ->
    Node = mim(),
    Config1 = ejabberd_node_utils:init(Node, Config),
    Config1.

end_per_suite(_Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mongoose_graphql tests
%%--------------------------------------------------------------------

can_execute_admin_queries_with_permissions(Config) ->
    Query = "query { checkAuth { authStatus } }",
    Res = mongooseimctl("graphql", [Query], Config),
    ?assertMatch({_, 0}, Res),
    Data = element(1, Res),
    ?assertNotEqual(nomatch, string:find(Data, "AUTHORIZED")).

can_handle_execution_error(Config) ->
    Query = "{}",
    Res = mongooseimctl("graphql", [Query], Config),
    ?assertMatch({_, 1}, Res),
    Data = element(1, Res),
    ?assertNotEqual(nomatch, string:find(Data, "parser_error")).

graphql_wrong_arguments_number(Config) ->
    ExpectedFragment = "This command requires",
    ResNoArgs = mongooseimctl("graphql", [], Config),
    ?assertMatch({_, 1}, ResNoArgs),
    Data1 = element(1, ResNoArgs),
    ?assertNotEqual(nomatch, string:find(Data1, ExpectedFragment)),

    ResTooManyArgs = mongooseimctl("graphql", ["{}", "{}"], Config),
    ?assertMatch({_, 1}, ResTooManyArgs),
    Data2 = element(1, ResTooManyArgs),
    ?assertNotEqual(nomatch, string:find(Data2, ExpectedFragment)).

%% Generic GraphQL command tests
%% Specific commands are tested in graphql_*_SUITE

graphql_error_unknown_command_with_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["makeCoffee", "--strength", "medium"], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown command")),
    expect_existing_commands(Res).

graphql_error_unknown_command_without_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["makeCoffee"], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown command")),
    expect_existing_commands(Res).

graphql_error_unknown_category_with_args(Config) ->
    {Res, 1} = mongooseimctl("cafe", ["makeCoffee"], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown category")),
    expect_category_list(Res).

graphql_error_unknown_category_without_args(Config) ->
    {Res, 1} = mongooseimctl("cafe", [], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown category")),
    expect_category_list(Res).

graphql_no_command(Config) ->
    %% Not an error - lists commands in the given category
    {Res, 0} = mongooseimctl("account", [], Config),
    expect_existing_commands(Res).

graphql_error_invalid_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers", "now"], Config),
    ?assertMatch({match, _}, re:run(Res, "Could not parse")),
    expect_command_arguments(Res).

graphql_error_invalid_arg_value(Config) ->
    {Res, 1} = mongooseimctl("vcard", ["setVcard", "--user", "user@host", "--vcard", "x"], Config),
    %% vCard should be provided in JSON
    ?assertMatch({match, _}, re:run(Res, "Invalid value 'x' of argument 'vcard'")),
    ?assertMatch({match, _}, re:run(Res, "vcard\s+VcardInput!")).

graphql_error_no_arg_value(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers", "--domain"], Config),
    ?assertMatch({match, _}, re:run(Res, "Could not parse")),
    expect_command_arguments(Res).

graphql_error_missing_args(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers"], Config),
    ?assertMatch({match, _}, re:run(Res, "Missing mandatory arguments")),
    expect_command_arguments(Res).

graphql_error_unknown_arg(Config) ->
    {Res, 1} = mongooseimctl("account", ["countUsers", "--domain", "localhost",
                                         "--x", "y"], Config),
    ?assertMatch({match, _}, re:run(Res, "Unknown argument")),
    expect_command_arguments(Res).

graphql_arg_help(Config) ->
    {Res, 0} = mongooseimctl("account", ["countUsers", "--help"], Config),
    expect_command_arguments(Res).

graphql_command(Config) ->
    {ResJSON, 0} = mongooseimctl("account", ["countUsers", "--domain", "localhost"], Config),
    #{<<"data">> := Data} = rest_helper:decode(ResJSON, #{return_maps => true}),
    ?assertMatch(#{<<"account">> := #{<<"countUsers">> := _}}, Data).

expect_existing_commands(Res) ->
    ?assertMatch({match, _}, re:run(Res, "countUsers")).

expect_command_arguments(Res) ->
    ?assertMatch({match, _}, re:run(Res, "domain\s+DomainName!")).

%%-----------------------------------------------------------------
%% Help tests
%%-----------------------------------------------------------------

default_help(Config) ->
    #{node := Node} = mim(),
    CtlCmd = distributed_helper:ctl_path(Node, Config),
    {Res, 2} = mongooseimctl_helper:run(CtlCmd, []),
    expect_category_list(Res).

%%-----------------------------------------------------------------
%% Server management tests
%%-----------------------------------------------------------------

server_status(Config) ->
    {Res, 0} = mongooseimctl("status", [], Config),
    ?assertMatch({match, _}, re:run(Res, "Erlang VM status: started")).

server_is_started(Config) ->
    %% Wait for the server to start, but it is already running
    {Res, 0} = mongooseimctl("started", [], Config),
    %% Expect only whitespace
    ?assertMatch(nomatch, re:run(Res, "\S")).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

expect_category_list(Res) ->
    ?assertMatch({match, _}, re:run(Res, "Usage")),
    ?assertMatch({match, _}, re:run(Res, "account\s+Account management")).
