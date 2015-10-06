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
     {json, [], [jsx_test, j2m_test]} 
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

jsx_test(_Config) ->
    A = "{\"a\":[{\"b\": 10},2]}",
    [{<<"a">>,[[{<<"b">>,10}],2]}] = jsx:decode(list_to_binary(A)).
 
j2m_test(_Config) ->
    j2m("{}",
        {struct,[]}),
    j2m("{\"a\": 1}",
        {struct,[{<<"a">>,1}]}),
    j2m("{\"a\": [1,2,3]}",
        {struct,[{<<"a">>,[1,2,3]}]}),
    j2m("{\"a\" : [1,2, {\"b\" : 10}]}",
        {struct,[{<<"a">>,[1,2,{struct,[{<<"b">>,10}]}]}]}),
    j2m("{\"a\" : [1,2, {\"b\" : 10}], \"b\" : \"string\", \"c\" : 1.22}",
        {struct,[{<<"a">>,[1,2,{struct,[{<<"b">>,10}]}]},
                 {<<"b">>,<<"string">>},
                 {<<"c">>,1.22}]}),
    j2m("{\"user\" : { \"password\" : \"password\"}}",
        {struct, [{<<"user">>,
                   {struct,[{<<"password">>,<<"password">>}]}}]}). 

j2m(Json, Struct) ->
    A = jsx:decode(list_to_binary(Json)),
    ?eq(Struct, mochi2jsx:j2m(A)),
    ?eq(mochi2jsx:m2j(Struct), A). 
