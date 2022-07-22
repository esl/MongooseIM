-module(graphql_private_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1]).
-import(graphql_helper, [execute_user_command/5, execute_command/4, get_ok_value/2, get_err_code/1,
                         user_to_bin/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_private}, {group, admin_private_http}, {group, admin_private_cli}].

groups() ->
    [{user_private, [], user_private_tests()},
     {admin_private_http, [], admin_private_tests()},
     {admin_private_cli, [], admin_private_tests()}].

user_private_tests() ->
    [user_set_private,
     user_get_private,
     parse_xml_error].

admin_private_tests() ->
    [admin_set_private,
     admin_get_private,
     no_user_error_set,
     no_user_error_get].

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    Config2 = ejabberd_node_utils:init(mim(), Config1),
    Backend = mongoose_helper:get_backend_mnesia_rdbms_riak(HostType),
    ModConfig = create_config(Backend),
    dynamic_modules:ensure_modules(HostType, ModConfig),
    escalus:init_per_suite([{backend, Backend} | Config2]).

create_config(riak) ->
    [{mod_private, #{backend => riak,
                     iqdisc => one_queue,
                     riak => #{bucket_type => <<"private">>}}}];
create_config(Backend) ->
    [{mod_private, #{backend => Backend, iqdisc => one_queue}}].

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_private_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_private_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(user_private, Config) ->
    graphql_helper:init_user(Config).

end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean(),
    graphql_helper:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

% User tests

user_set_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_set_private/2).

user_set_private(Config, Alice) ->
    ElemStr = exml:to_binary(private_input()),
    ResultSet = user_set_private(Alice, ElemStr, Config),
    ParsedResultSet = get_ok_value([data, private, setPrivate], ResultSet),
    ?assertEqual(<<"[]">>, ParsedResultSet),
    ResultGet = user_get_private(Alice, <<"my_element">>, <<"alice:private:ns">>, Config),
    ParsedResultGet = get_ok_value([data, private, getPrivate], ResultGet),
    ?assertEqual(ElemStr, ParsedResultGet).

user_get_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_get_private/2).

user_get_private(Config, Alice) ->
    ElemStr = exml:to_binary(private_input()),
    IQ = escalus_stanza:to(escalus_stanza:private_set(private_input()),
                           escalus_users:get_jid(Config, alice)),
    escalus_client:send_and_wait(Alice, IQ),
    ResultGet = user_get_private(Alice, <<"my_element">>, <<"alice:private:ns">>, Config),
    ParsedResultGet = get_ok_value([data, private, getPrivate], ResultGet),
    ?assertEqual(ElemStr, ParsedResultGet).

parse_xml_error(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun parse_xml_error/2).

parse_xml_error(Config, Alice) ->
    ResultSet = user_set_private(Alice, <<"AAAABBBB">>, Config),
    ?assertEqual(<<"parse_error">>, get_err_code(ResultSet)).

% Admin tests

admin_set_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_set_private/2).

admin_set_private(Config, Alice) ->
    AliceBin = user_to_bin(Alice),
    ElemStr = exml:to_binary(private_input()),
    ResultSet = admin_set_private(AliceBin, ElemStr, Config),
    ParsedResultSet = get_ok_value([data, private, setPrivate], ResultSet),
    ?assertEqual(<<"[]">>, ParsedResultSet),
    ResultGet = admin_get_private(AliceBin, <<"my_element">>, <<"alice:private:ns">>, Config),
    ParsedResultGet = get_ok_value([data, private, getPrivate], ResultGet),
    ?assertEqual(ElemStr, ParsedResultGet).

admin_get_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_private/2).

admin_get_private(Config, Alice) ->
    AliceBin = user_to_bin(Alice),
    ElemStr = exml:to_binary(private_input()),
    IQ = escalus_stanza:to(escalus_stanza:private_set(private_input()),
                           escalus_users:get_jid(Config, alice)),
    escalus_client:send_and_wait(Alice, IQ),
    ResultGet = admin_get_private(AliceBin, <<"my_element">>, <<"alice:private:ns">>, Config),
    ParsedResultGet = get_ok_value([data, private, getPrivate], ResultGet),
    ?assertEqual(ElemStr, ParsedResultGet).

no_user_error_set(Config) ->
    ElemStr = exml:to_binary(private_input()),
    Result = admin_set_private(<<"AAAAA">>, ElemStr, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Result)).

no_user_error_get(Config) ->
    Result = admin_get_private(<<"AAAAA">>, <<"my_element">>, <<"alice:private:ns">>, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Result)).

private_input() ->
    #xmlel{name = <<"my_element">>,
           attrs = [{<<"xmlns">>, "alice:private:ns"}],
           children = [{xmlcdata, <<"DATA">>}]}.

%% Commands

user_set_private(User, ElementString, Config) ->
    Vars = #{elementString => ElementString},
    execute_user_command(<<"private">>, <<"setPrivate">>, User, Vars, Config).

user_get_private(User, Element, NameSpace, Config) ->
    Vars = #{element => Element, nameSpace => NameSpace},
    execute_user_command(<<"private">>, <<"getPrivate">>, User, Vars, Config).

admin_set_private(User, ElementString, Config) ->
    Vars = #{user => User, elementString => ElementString},
    execute_command(<<"private">>, <<"setPrivate">>, Vars, Config).

admin_get_private(User, Element, NameSpace, Config) ->
    Vars = #{user => User, element => Element, nameSpace => NameSpace},
    execute_command(<<"private">>, <<"getPrivate">>, Vars, Config).
