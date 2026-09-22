-module(mongoose_graphql_enum_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [
        loglevel_valid_values_return_atoms
    ].

loglevel_valid_values_return_atoms(_Config) ->
    ValidLevels = [
        {<<"ALL">>, all},
        {<<"DEBUG">>, debug},
        {<<"INFO">>, info},
        {<<"NOTICE">>, notice},
        {<<"WARNING">>, warning},
        {<<"ERROR">>, error},
        {<<"CRITICAL">>, critical},
        {<<"ALERT">>, alert},
        {<<"EMERGENCY">>, emergency},
        {<<"NONE">>, none}
    ],
    lists:foreach(fun({Input, Expected}) ->
        {ok, Result} = mongoose_graphql_enum:input(<<"LogLevel">>, Input),
        ?assertEqual(Expected, Result)
    end, ValidLevels).
