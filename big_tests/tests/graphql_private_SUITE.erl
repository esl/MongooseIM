-module(graphql_private_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("../../include/mod_roster.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_private}, {group, admin_private}].

groups() ->
    [{user_private, [], user_private_handler()},
     {admin_private, [], admin_private_handler()}].

user_private_handler() ->
    [user_set_private,
     user_get_private,
     parse_xml_error].

admin_private_handler() ->
    [admin_set_private,
     admin_get_private,
     no_user_error_set,
     no_user_error_get].

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    Backend = mongoose_helper:get_backend_mnesia_rdbms_riak(HostType),
    ModConfig = create_config(Backend),
    dynamic_modules:ensure_modules(HostType, ModConfig),
    escalus:init_per_suite([{backend, Backend} | Config1]).

create_config(riak) ->
    [{mod_private, #{backend => riak,
                     iqdisc => one_queue,
                     riak => #{bucket_type => <<"private">>}}}];
create_config(Backend) ->
    [{mod_private, #{backend => Backend, iqdisc => one_queue}}].

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_private, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(user_private, Config) ->
    [{schema_endpoint, user} | Config].

end_per_group(admin_private, _Config) ->
    escalus_fresh:clean();
end_per_group(user_private, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

% User tests

user_set_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_set_private/2).

user_set_private(Config, Alice) ->
    QuerySet = user_private_mutation(),
    Expected = exml:to_binary(private_input()),
    BodySet = #{query => QuerySet, operationName => <<"M1">>,
             variables => #{privateString => Expected}},
    GraphQlRequestSet = execute_user(BodySet, Alice, Config),
    ParsedResultSet = ok_result(<<"private">>, <<"setPrivate">>, GraphQlRequestSet),
    ?assertEqual(<<"[]">>, ParsedResultSet),
    Vars = #{element => <<"my_element">>, subElement => <<"alice:private:ns">>},
    QueryGet = user_private_query(),
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => Vars},
    GraphQlRequestGet = execute_user(BodyGet, Alice, Config),
    ParsedResultGet = ok_result(<<"private">>, <<"getPrivate">>, GraphQlRequestGet),
    ?assertEqual(Expected, ParsedResultGet).

user_get_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_private/2).

user_get_private(Config, Alice) ->
    Expected = exml:to_binary(private_input()),
    IQ = escalus_stanza:to(escalus_stanza:private_set(private_input()),
                           escalus_users:get_jid(Config, alice)),
    escalus_client:send_and_wait(Alice, IQ),
    Vars = #{element => <<"my_element">>, subElement => <<"alice:private:ns">>},
    QueryGet = user_private_query(),
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => Vars},
    GraphQlRequestGet = execute_user(BodyGet, Alice, Config),
    ParsedResultGet = ok_result(<<"private">>, <<"getPrivate">>, GraphQlRequestGet),
    ?assertEqual(Expected, ParsedResultGet).

parse_xml_error(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun parse_xml_error/2).

parse_xml_error(Config, Alice) ->
    QuerySet = user_private_mutation(),
    Input = <<"AAAABBBB">>,
    BodySet = #{query => QuerySet, operationName => <<"M1">>,
             variables => #{privateString => Input}},
    GraphQlRequestSet = execute_user(BodySet, Alice, Config),
    ParsedResultSet = error_result2(<<"extensions">>, <<"code">>, GraphQlRequestSet),
    ?assertEqual(<<"parse_error">>, ParsedResultSet).

% Admin tests

admin_set_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_set_private/2).

admin_set_private(Config, Alice) ->
    QuerySet = admin_private_mutation(),
    Expected = exml:to_binary(private_input()),
    BodySet = #{query => QuerySet, operationName => <<"M1">>,
             variables => #{privateString => Expected, user => user_to_bin(Alice)}},
    GraphQlRequestSet = execute_auth(BodySet, Config),
    ParsedResultSet = ok_result(<<"private">>, <<"setPrivate">>, GraphQlRequestSet),
    ?assertEqual(<<"[]">>, ParsedResultSet),
    Vars = #{element => <<"my_element">>, subElement => <<"alice:private:ns">>,
             user => user_to_bin(Alice)},
    QueryGet = admin_private_query(),
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => Vars},
    GraphQlRequestGet = execute_auth(BodyGet, Config),
    ParsedResultGet = ok_result(<<"private">>, <<"getPrivate">>, GraphQlRequestGet),
    ?assertEqual(Expected, ParsedResultGet).

admin_get_private(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_private/2).

admin_get_private(Config, Alice) ->
    Expected = exml:to_binary(private_input()),
    IQ = escalus_stanza:to(escalus_stanza:private_set(private_input()),
                           escalus_users:get_jid(Config, alice)),
    escalus_client:send_and_wait(Alice, IQ),
    Vars = #{element => <<"my_element">>, subElement => <<"alice:private:ns">>,
             user => user_to_bin(Alice)},
    QueryGet = admin_private_query(),
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => Vars},
    GraphQlRequestGet = execute_auth(BodyGet, Config),
    ParsedResultGet = ok_result(<<"private">>, <<"getPrivate">>, GraphQlRequestGet),
    ?assertEqual(Expected, ParsedResultGet).

no_user_error_get(Config) ->
    Vars = #{element => <<"my_element">>, subElement => <<"alice:private:ns">>,
             user => <<"AAAAA">>},
    QueryGet = admin_private_query(),
    BodyGet = #{query => QueryGet, operationName => <<"Q1">>, variables => Vars},
    GraphQlRequestGet = execute_auth(BodyGet, Config),
    ParsedResultGet = error_result2(<<"extensions">>, <<"code">>, GraphQlRequestGet),
    ?assertEqual(<<"not_found">>, ParsedResultGet).

no_user_error_set(Config) ->
    QuerySet = admin_private_mutation(),
    Expected = exml:to_binary(private_input()),
    BodySet = #{query => QuerySet, operationName => <<"M1">>,
             variables => #{privateString => Expected, user => <<"AAAAA">>}},
    GraphQlRequestSet = execute_auth(BodySet, Config),
    ParsedResultSet = error_result2(<<"extensions">>, <<"code">>, GraphQlRequestSet),
    ?assertEqual(<<"not_found">>, ParsedResultSet).

private_input() ->
    #xmlel{
        name = <<"my_element">>,
        attrs = [{<<"xmlns">>, "alice:private:ns"}],
        children = [{xmlcdata, <<"DATA">>}]}.

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

error_result(What, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What, Data).

error_result2(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What2, maps:get(What1, Data)).

user_private_mutation() ->
    <<"mutation M1($privateString: String!)
       {
        private
           {
               setPrivate(privateString: $privateString)
           }
       }">>.

user_private_query() ->
    <<"query Q1($element: String! $subElement: String!)
       {
        private
           {
               getPrivate(element: $element, subElement: $subElement)
           }
       }">>.

admin_private_mutation() ->
    <<"mutation M1($privateString: String!, $user: JID!)
       {
        private
           {
               setPrivate(user: $user, privateString: $privateString)
           }
       }">>.

admin_private_query() ->
    <<"query Q1($user: JID!, $element: String! $subElement: String!)
       {
        private
           {
               getPrivate(user: $user, element: $element, subElement: $subElement)
           }
       }">>.
