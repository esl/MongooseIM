-module(mod_foreign_http_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").

-define(OPTS,
        [
         {backends, [
                     {http, [{pool_size, 100}]}
                    ]}
        ]).
-define(REQUEST,  <<"<request xmlns='", ?NS_FOREIGN_EVENT_HTTP/binary, "' "
                              "type='http' "
                              "url='http://localhost:8080' "
                              "method='get'>"
                       "<header name='content-type'>application-json</header>"
                       "<header name='token'>token</header>"
                       "<payload>'{\"key\":\"value\"}'</payload>"
                     "</request>">>).
-define(RESPONSE,  <<"<response xmlns='", ?NS_FOREIGN_EVENT_HTTP/binary, "' "
                                "type='http' "
                                "status='200'>"
                        "<header name='content-type'>application-json</header>"
                        "<header name='token'>token</header>"
                        "<payload>'{\"key\":\"value\"}'</payload>"
                      "</response>">>).



all() -> [
          makes_http_request
         ].

init_per_suite(Config) ->
    http_helper:start(8080, '_', fun process_request/1),
    Config.

end_per_suite(_Config) ->
    http_helper:stop().

%% Tests

makes_http_request(_Config) ->
    %% GIVEN
    {ok, Request} = exml:parse(?REQUEST),
    {ok, Response} = exml:parse(?RESPONSE),
    Self = self(),
    OnResponse = fun(Resp) -> Self ! Resp end,

    %% WHEN
    ok = mod_foreign_http:make_request(Request, OnResponse),

    %% THEN
    ActualResponse = receive R -> R after 5000 -> timeout end,
    E = Response#xmlel{children = []},
    A = ActualResponse#xmlel{children = []},

    Headers = parse_headers(Response),
    ActualHeaders = parse_headers(ActualResponse),
    ?assertMatch(A, E),
    [?assert(lists:any(fun(H) -> H =:= Header end, ActualHeaders)) ||
     Header <- Headers].

%% Helpers

process_request(Req0) ->
    {Headers, Req1} = cowboy_req:headers(Req0),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    {ok, Req3} = cowboy_req:reply(200, Headers, Body, Req2),
    Req3.

parse_headers(#xmlel{} = XML) ->
    Headers = exml_query:paths(XML, [{element, <<"header">>}]),
    lists:foldl(fun(#xmlel{attrs = [{<<"name">>, Name}],
                           children = [#xmlcdata{content = Value}]},
                    Acc) ->
                        [{Name, Value} | Acc]
                end, [], Headers).