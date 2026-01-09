-module(account_admin_SUITE).
-compile([export_all, nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [api_list_users_pagination,
     resolver_list_users_pagination,
     pagination_utils].

init_per_suite(Config) ->
    application:ensure_all_started(jid),
    mnesia:create_schema([node()]),
    mnesia:start(),

    mongoose_config:set_opts(#{
        {auth, host_type()} => #{methods => [internal], internal => #{}}
    }),
    ejabberd_auth_internal:start(host_type()),
    Config.

end_per_suite(_Config) ->
    ejabberd_auth_internal:stop(host_type()),
    mongoose_config:erase_opts(),
    mnesia:stop(),
    mnesia:delete_schema([node()]).

api_list_users_pagination(_C) ->
    Domain = <<"api.com">>,
    mongoose_domain_core:start_link([{Domain, host_type()}], []),

    Users = [<<"u1">>, <<"u2">>, <<"u3">>],
    lists:foreach(fun(U) ->
        mnesia:dirty_write({passwd, {U, Domain}, <<"pass">>})
    end, Users),

    %% Test API with options
    {ok, Result1} = mongoose_account_api:list_users(Domain, #{limit => 1}),
    ?assertEqual([<<"u1@api.com">>], Result1),

    {ok, Result2} = mongoose_account_api:list_users(Domain, #{limit => 1, offset => 1}),
    ?assertEqual([<<"u2@api.com">>], Result2),

    {ok, Result3} = mongoose_account_api:list_users(Domain, #{offset => 1}),
    ?assertEqual([<<"u2@api.com">>, <<"u3@api.com">>], Result3),

    %% Compatibility: accept proplist opts at the public ejabberd_auth boundary
    Res4 = ejabberd_auth:get_vh_registered_users(Domain, [{limit, 1}, {offset, 1}]),
    ?assertEqual([{<<"u2">>, Domain}], Res4),

    %% Cleanup
    lists:foreach(fun(U) -> mnesia:dirty_delete({passwd, {U, Domain}}) end, Users).

resolver_list_users_pagination(_C) ->
    Domain = <<"resolver.com">>,
    mongoose_domain_core:start_link([{Domain, host_type()}], []),

    Users = [<<"u1">>, <<"u2">>, <<"u3">>],
    lists:foreach(fun(U) ->
        mnesia:dirty_write({passwd, {U, Domain}, <<"pass">>})
    end, Users),

    %% Test Resolver
    %% Case 1: No pagination
    {ok, Res1} = mongoose_graphql_account_admin_query:execute(#{}, #{}, <<"listUsers">>, #{<<"domain">> => Domain}),
    ?assertEqual([{ok, <<"u1@resolver.com">>}, {ok, <<"u2@resolver.com">>}, {ok, <<"u3@resolver.com">>}], Res1),

    %% Case 2: Limit
    {ok, Res2} = mongoose_graphql_account_admin_query:execute(#{}, #{}, <<"listUsers">>,
                                                              #{<<"domain">> => Domain, <<"limit">> => 2}),
    ?assertEqual([{ok, <<"u1@resolver.com">>}, {ok, <<"u2@resolver.com">>}], Res2),

    %% Case 3: Index (Offset)
    {ok, Res3} = mongoose_graphql_account_admin_query:execute(#{}, #{}, <<"listUsers">>,
                                                              #{<<"domain">> => Domain, <<"index">> => 1}),
    ?assertEqual([{ok, <<"u2@resolver.com">>}, {ok, <<"u3@resolver.com">>}], Res3),

    %% Case 4: Both
    {ok, Res4} = mongoose_graphql_account_admin_query:execute(#{}, #{}, <<"listUsers">>,
                                                              #{<<"domain">> => Domain, <<"limit">> => 1, <<"index">> => 2}),
    ?assertEqual([{ok, <<"u3@resolver.com">>}], Res4),

    %% Cleanup
    lists:foreach(fun(U) -> mnesia:dirty_delete({passwd, {U, Domain}}) end, Users).

pagination_utils(_C) ->
    Users = [<<"u3">>, <<"u1">>, <<"u2">>],

    %% No pagination (returns as is)
    ?assertEqual(Users, mongoose_pagination_utils:slice(Users, undefined, 0)),

    %% Limit (returns first N)
    ?assertEqual([<<"u3">>, <<"u1">>], mongoose_pagination_utils:slice(Users, 2, 0)),

    %% Offset (skips first N)
    ?assertEqual([<<"u1">>, <<"u2">>], mongoose_pagination_utils:slice(Users, undefined, 1)),

    %% Both (skip 1, take 1)
    ?assertEqual([<<"u1">>], mongoose_pagination_utils:slice(Users, 1, 1)).

host_type() -> <<"test_host_type">>.
