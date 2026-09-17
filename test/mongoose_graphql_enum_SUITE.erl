-module(mongoose_graphql_enum_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [
        loglevel_valid_values_return_atoms,
        loglevel_case_insensitive
    ].

loglevel_valid_values_return_atoms(_Config) ->
    ValidLevels = [
        {<<"DEBUG">>, debug},
        {<<"INFO">>, info},
        {<<"NOTICE">>, notice},
        {<<"WARNING">>, warning},
        {<<"ERROR">>, error},
        {<<"CRITICAL">>, critical},
        {<<"ALERT">>, alert},
        {<<"EMERGENCY">>, emergency}
    ],
    lists:foreach(fun({Input, Expected}) ->
        {ok, Result} = mongoose_graphql_enum:input(<<"LogLevel">>, Input),
        ?assertEqual(Expected, Result)
    end, ValidLevels).

loglevel_case_insensitive(_Config) ->
    {ok, debug} = mongoose_graphql_enum:input(<<"LogLevel">>, <<"DEBUG">>),
    {ok, debug} = mongoose_graphql_enum:input(<<"LogLevel">>, <<"debug">>),
    {ok, debug} = mongoose_graphql_enum:input(<<"LogLevel">>, <<"Debug">>),
    {ok, info} = mongoose_graphql_enum:input(<<"LogLevel">>, <<"INFO">>),
    {ok, info} = mongoose_graphql_enum:input(<<"LogLevel">>, <<"info">>).
