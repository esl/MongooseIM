-module(graphql_private_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1]).
-import(graphql_helper, [execute_user_command/5, execute_command/4, get_ok_value/2, get_err_code/1,
                         user_to_bin/1, get_unauthorized/1, get_not_loaded/1, get_coercion_err_msg/1]).
-import(config_parser_helper, [mod_config_with_auto_backend/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, domain_admin_private},
     {group, admin_http},
     {group, admin_cli}].

groups() ->
    [{user, [], user_groups()},
     {domain_admin_private, [], domain_admin_private_tests()},
     {admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {admin_private_configured, [], admin_private_tests()},
     {user_private_configured, [], user_private_tests()},
     {admin_private_not_configured, [], admin_private_not_configured_tests()},
     {user_private_not_configured, [], user_private_not_configured_tests()}].

admin_groups() ->
    [{group, admin_private_configured},
     {group, admin_private_not_configured}].

user_groups() ->
    [{group, user_private_configured},
     {group, user_private_not_configured}].

user_private_tests() ->
    [user_set_private,
     user_get_private,
     user_get_private_empty_namespace,
     parse_xml_error].

user_private_not_configured_tests() ->
    [user_set_private_not_configured,
     user_get_private_not_configured].

domain_admin_private_tests() ->
    [admin_set_private,
     admin_get_private,
     domain_admin_user_set_private_no_permission,
     domain_admin_user_get_private_no_permission].

admin_private_tests() ->
    [admin_set_private,
     admin_get_private,
     admin_get_private_empty_namespace,
     no_user_error_set,
     no_user_error_get].

admin_private_not_configured_tests() ->
    [admin_set_private_not_configured,
     admin_get_private_not_configured].

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    Config2 = ejabberd_node_utils:init(mim(), Config1),
    escalus:init_per_suite(Config2).

create_config() ->
    [{mod_private,
      mod_config_with_auto_backend(mod_private, #{iqdisc => one_queue})}].

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(user, Config) ->
    graphql_helper:init_user(Config);
init_per_group(domain_admin_private, Config) ->
    Config1 = ensure_private_started(Config),
    graphql_helper:init_domain_admin_handler(Config1);
init_per_group(Group, Config) when Group =:= admin_private_configured;
                                   Group =:= user_private_configured ->
    ensure_private_started(Config);
init_per_group(Group, Config) when Group =:= admin_private_not_configured;
                                   Group =:= user_private_not_configured ->
    ensure_private_stopped(Config).

ensure_private_started(Config) ->
    HostType = domain_helper:host_type(),
    dynamic_modules:ensure_modules(HostType, create_config()),
    Config.

ensure_private_stopped(Config) ->
    HostType = domain_helper:host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_private, stopped}]),
    Config.

end_per_group(GroupName, _Config) when GroupName =:= admin_http;
                                       GroupName =:= admin_cli;
                                       GroupName =:= user;
                                       GroupName =:= domain_admin_private ->
    graphql_helper:clean();
end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean().

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
    ?assertEqual(<<"<my_element xmlns='alice:private:ns'>DATA</my_element>">>, ParsedResultSet),
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

user_get_private_empty_namespace(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_get_private_empty_namespace/2).

user_get_private_empty_namespace(Config, Alice) ->
    ResultGet = user_get_private(Alice, <<"">>, <<"">>, Config),
    ?assertEqual(<<"Input coercion failed for type NonEmptyString with value <<>>."
                   " The reason it failed is: \"Given string is empty\"">>,
                 get_coercion_err_msg(ResultGet)).

parse_xml_error(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun parse_xml_error/2).

parse_xml_error(Config, Alice) ->
    ResultSet = user_set_private(Alice, <<"AAAABBBB">>, Config),
    ?assertEqual(<<"Input coercion failed for type XmlElement with value <<\"AAAABBBB\">>."
                   " The reason it failed is: <<\"expected <\">>">>,
                 get_coercion_err_msg(ResultSet)).

% User private not configured test cases

user_set_private_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_set_private_not_configured/2).

user_set_private_not_configured(Config, Alice) ->
    ElemStr = exml:to_binary(private_input()),
    Res = user_set_private(Alice, ElemStr, Config),
    get_not_loaded(Res).

user_get_private_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_get_private_not_configured/2).

user_get_private_not_configured(Config, Alice) ->
    Res = user_get_private(Alice, <<"my_element">>, <<"alice:private:ns">>, Config),
    get_not_loaded(Res).

% Admin tests

admin_set_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_set_private/2).

admin_set_private(Config, Alice) ->
    AliceBin = user_to_bin(Alice),
    ElemStr = exml:to_binary(private_input()),
    ResultSet = admin_set_private(AliceBin, ElemStr, Config),
    ParsedResultSet = get_ok_value([data, private, setPrivate], ResultSet),
    ?assertEqual(<<"<my_element xmlns='alice:private:ns'>DATA</my_element>">>, ParsedResultSet),
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

admin_get_private_empty_namespace(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_private_empty_namespace/2).

admin_get_private_empty_namespace(Config, Alice) ->
    AliceBin = user_to_bin(Alice),
    ResultGet = admin_get_private(AliceBin, <<"">>, <<"">>, Config),
    ?assertEqual(<<"Input coercion failed for type NonEmptyString with value <<>>."
                   " The reason it failed is: \"Given string is empty\"">>,
                 get_coercion_err_msg(ResultGet)).

no_user_error_set(Config) ->
    ElemStr = exml:to_binary(private_input()),
    Result = admin_set_private(<<"eddie@otherhost">>, ElemStr, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Result)).

no_user_error_get(Config) ->
    Result = admin_get_private(<<"eddie@otherhost">>, <<"my_element">>, <<"alice:private:ns">>, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Result)).

private_input() ->
    #xmlel{name = <<"my_element">>,
           attrs = #{<<"xmlns">> => "alice:private:ns"},
           children = [#xmlcdata{content = <<"DATA">>}]}.

% Admin private not configured test cases

admin_set_private_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_set_private_not_configured/2).

admin_set_private_not_configured(Config, Alice) ->
    AliceBin = user_to_bin(Alice),
    ElemStr = exml:to_binary(private_input()),
    Res = admin_set_private(AliceBin, ElemStr, Config),
    get_not_loaded(Res).

admin_get_private_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_private_not_configured/2).

admin_get_private_not_configured(Config, Alice) ->
    AliceBin = user_to_bin(Alice),
    Res = admin_get_private(AliceBin, <<"my_element">>, <<"alice:private:ns">>, Config),
    get_not_loaded(Res).

% Domain admin tests

domain_admin_user_set_private_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_user_set_private_no_permission/2).
domain_admin_user_set_private_no_permission(Config, AliceBis) ->
    ElemStr = exml:to_binary(private_input()),
    Result = admin_set_private(user_to_bin(AliceBis), ElemStr, Config),
    get_unauthorized(Result),
    Result2 = admin_set_private(<<"eddie@otherhost">>, ElemStr, Config),
    get_unauthorized(Result2).

domain_admin_user_get_private_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_user_get_private_no_permission/2).

domain_admin_user_get_private_no_permission(Config, AliceBis) ->
    AliceBisBin = user_to_bin(AliceBis),
    Result = admin_get_private(AliceBisBin, <<"my_element">>, <<"alice:private:ns">>, Config),
    get_unauthorized(Result),
    Result2 = admin_get_private(<<"eddie@otherhost">>, <<"my_element">>, <<"alice:private:ns">>, Config),
    get_unauthorized(Result2).

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
