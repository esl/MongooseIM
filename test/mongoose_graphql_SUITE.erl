-module(mongoose_graphql_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("graphql/src/graphql_schema.hrl").

all() ->
    [can_create_endpoint, 
     can_load_splitted_schema,
     {group, unprotected_graphql}, 
     {group, protected_graphql},
     {group, errors_handling}].

groups() ->
    [{protected_graphql, [], 
     [auth_can_execute_protected_query, 
      auth_can_execute_protected_mutation,
      unauth_cannot_execute_protected_query,
      unauth_cannot_execute_protected_mutation,
      unauth_can_access_introspection]},
     {unprotected_graphql, [], 
      [can_execute_query_with_vars,
       auth_can_execute_query,
       auth_can_execute_mutation,
       unauth_can_execute_query,
       unauth_can_execute_mutation]},
     {errors_handling, [], 
      [should_catch_parsing_errors,
       should_catch_type_check_params_errors,
       should_catch_type_check_errors
      ]}].

init_per_testcase(C, Config) when C =:= auth_can_execute_protected_query;
                                  C =:= auth_can_execute_protected_mutation;
                                  C =:= unauth_cannot_execute_protected_query;
                                  C =:= unauth_cannot_execute_protected_mutation;
                                  C =:= unauth_can_access_introspection ->
    {Mapping, Pattern} = example_schema_protected_data(Config),
    {ok, _} = mongoose_graphql:create_endpoint(C, Mapping, Pattern),
    Ep = mongoose_graphql:get_endpoint(C),
    [{endpoint, Ep} | Config];
init_per_testcase(C, Config) when C =:= can_execute_query_with_vars;
                                  C =:= auth_can_execute_query;
                                  C =:= auth_can_execute_mutation;
                                  C =:= unauth_can_execute_query;
                                  C =:= unauth_can_execute_mutation;
                                  C =:= should_catch_type_check_params_errors;
                                  C =:= should_catch_type_check_errors;
                                  C =:= should_catch_parsing_errors ->
    {Mapping, Pattern} = example_schema_data(Config),
    {ok, _} = mongoose_graphql:create_endpoint(C, Mapping, Pattern),
    Ep = mongoose_graphql:get_endpoint(C),
    [{endpoint, Ep} | Config];
init_per_testcase(C, Config) ->
    [{endpoint_name, C} | Config].

end_per_testcase(_, _Config) ->
    ok.

can_create_endpoint(Config) ->
    Name = ?config(endpoint_name, Config),
    {Mapping, Pattern} = example_schema_protected_data(Config),
    {ok, Pid} = mongoose_graphql:create_endpoint(Name, Mapping, Pattern),

    Ep = mongoose_graphql:get_endpoint(Name),
    ?assertMatch({endpoint_context, Name, Pid, _, _}, Ep),
    ?assertMatch(#root_schema{id = 'ROOT', query = <<"UserQuery">>, 
                              mutation = <<"UserMutation">>},
                  graphql_schema:get(Ep, 'ROOT')).

can_load_splitted_schema(Config) ->
    Name = ?config(endpoint_name, Config),
    {Mapping, Pattern} = example_splitted_schema_data(Config),
    Pattern2 = filelib:wildcard(Pattern),
    {ok, Pid} = mongoose_graphql:create_endpoint(Name, Mapping, Pattern),

    Ep = mongoose_graphql:get_endpoint(Name),
    ?assertMatch({endpoint_context, Name, Pid, _, _}, Ep),
    ?assertMatch(#root_schema{id = 'ROOT', query = <<"Query">>, 
                              mutation = <<"Mutation">>},
                  graphql_schema:get(Ep, 'ROOT')),
    ?assertMatch(#object_type{id = <<"Query">>}, 
                 graphql_schema:get(Ep, <<"Query">>)),
    ?assertMatch(#object_type{id = <<"Mutation">>}, 
                 graphql_schema:get(Ep, <<"Mutation">>)).
    

auth_can_execute_protected_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"{ field }">>,
    Res = mongoose_graphql:execute(Ep, undefined, Doc),
    ?assertEqual({ok,#{data => #{<<"field">> => <<"Test field">>}}}, Res).

auth_can_execute_protected_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, undefined, Doc),
    ?assertEqual({ok,#{data => #{<<"field">> => <<"Test field">>}}}, Res).

unauth_cannot_execute_protected_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"{ field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertEqual({error, no_permissions}, Res).

unauth_cannot_execute_protected_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertEqual({error, no_permissions}, Res).

unauth_can_access_introspection(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"{ __schema { queryType { name } } __type(name: \"UserQuery\") { name } }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    Expected = 
        {ok,
            #{data =>
                #{<<"__schema">> =>
                    #{<<"queryType">> => 
                        #{<<"name">> => <<"UserQuery">>}
                },
                <<"__type">> => 
                    #{<<"name">> => 
                        <<"UserQuery">>
                     }
                 }
             }
        },
    ?assertEqual(Expected, Res).

can_execute_query_with_vars(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query Q1($value: String!) { id(value: $value)}">>,
    Req = 
        #{document => Doc, 
          operation_name => <<"Q1">>, 
          vars => #{<<"value">> => <<"Hello">>}, 
          authorized => false, 
          ctx => #{}},
    Res = mongoose_graphql:execute(Ep, Req),
    ?assertEqual({ok,#{data => #{<<"id">> => <<"Hello">>}}}, Res).

unauth_can_execute_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertEqual({ok,#{data => #{<<"field">> => <<"Test field">>}}}, Res).

unauth_can_execute_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertEqual({ok,#{data => #{<<"field">> => <<"Test field">>}}}, Res).

auth_can_execute_query(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, true)),
    ?assertEqual({ok,#{data => #{<<"field">> => <<"Test field">>}}}, Res).

auth_can_execute_mutation(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"mutation { field }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, true)),
    ?assertEqual({ok,#{data => #{<<"field">> => <<"Test field">>}}}, Res).

should_catch_parsing_errors(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { field ">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, _}, Res).

should_catch_type_check_errors(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { notExistingField(value: \"Hello\") }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, _}, Res).

should_catch_type_check_params_errors(Config) ->
    Ep = ?config(endpoint, Config),
    Doc = <<"query { id(value: 12) }">>,
    Res = mongoose_graphql:execute(Ep, request(Doc, false)),
    ?assertMatch({error, _}, Res).

%% Helpers

request(Doc, Authorized) ->
    #{document => Doc, 
      operation_name => undefined, 
      vars => #{}, 
      authorized => Authorized, 
      ctx => #{}}.

example_splitted_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), 
                             "splitted_schema", "*.gql"]),
    Mapping =
        #{objects =>
              #{'Query' => mongoose_graphql_default_resolver,
                'Mutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_schema_protected_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "protected_schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.

example_schema_data(Config) ->
    Pattern = filename:join([proplists:get_value(data_dir, Config), "schema.gql"]),
    Mapping =
        #{objects =>
              #{'UserQuery' => mongoose_graphql_default_resolver,
                'UserMutation' => mongoose_graphql_default_resolver,
                default => mongoose_graphql_default_resolver}},
    {Mapping, Pattern}.
