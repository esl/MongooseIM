-module(json_SUITE).
-compile(export_all).
-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, json}
    ].

groups() ->
    [
     {json, [], [mochijson_test, jsx_test, j2m_test]} 
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, _Config) ->
    ok.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, _Config) ->
    ok.

% Tests

mochijson_test(_Config) ->
    A = "{\"a\":[{\"b\": 10},2]}",
    {struct,[{<<"a">>,[{struct,[{<<"b">>,10}]},2]}]} = mochijson2:decode(A).

jsx_test(_Config) ->
    A = "{\"a\":[{\"b\": 10},2]}",
    [{<<"a">>,[[{<<"b">>,10}],2]}] = jsx:decode(list_to_binary(A)).
 
j2m_test(_Config) ->
    check_j2m("{}"),
    check_j2m("{\"a\": 1}"),
    check_j2m("{\"a\": [1,2,3]}"),
    check_j2m("{\"a\" : [1,2, {\"b\" : 10}]}"),
    check_j2m("{\"a\" : [1,2, {\"b\" : 10}], \"b\" : \"string\", \"c\" : 1.22}"),
    check_j2m("{\"user\" : { \"password\" : \"password\"}}"). 

m2j_test(_Config) ->
    check_m2j("{}"),
    check_m2j("{\"a\": 1}"),
    check_m2j("{\"a\": [1,2,3]}"),
    check_m2j("{\"a\" : [1,2, {\"b\" : 10}]}"),
    check_m2j("{\"a\" : [1,2, {\"b\" : 10}], \"b\" : \"string\", \"c\" : 1.22}"),
    check_m2j("{\"user\" : { \"password\" : \"password\"}}").

check_j2m(Json) ->
    A = jsx:decode(list_to_binary(Json)),
    ?eq(mochijson2:decode(Json), mochi2jsx:j2m(A)).

check_m2j(Json) ->
    A = mochijson2:decode(Json),
    ?eq(jsx:decode(list_to_binary(Json)), mochi2jsx:m2j(A)).
    
