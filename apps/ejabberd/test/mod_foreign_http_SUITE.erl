-module(mod_foreign_http_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").

-define(HOST, <<"localhost">>).
-define(HTTP_OPTS,
        [
         {http, [
                 {pool_size, 100},
                 {timeout, 5000}
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

-define(REQUEST_NO_URL,  <<"<request xmlns='", ?NS_FOREIGN_EVENT_HTTP/binary, "' "
                           "type='http' "
                           "method='get'>"
                           "<header name='content-type'>application-json</header>"
                           "<header name='token'>token</header>"
                           "<payload>'{\"key\":\"value\"}'</payload>"
                           "</request>">>).
-define(REQUEST_BAD_METHOD,  <<"<request xmlns='", ?NS_FOREIGN_EVENT_HTTP/binary, "' "
                               "type='http' "
                               "url='http://localhost:8080' "
                               "method='set'>"
                               "<header name='content-type'>application-json</header>"
                               "<header name='token'>token</header>"
                               "<payload>'{\"key\":\"value\"}'</payload>"
                               "</request>">>).
-define(REQUEST_BAD_URL,  <<"<request xmlns='", ?NS_FOREIGN_EVENT_HTTP/binary, "' "
                            "type='http' "
                            "url='smb://localhost:8080' "
                            "method='get'>"
                            "<header name='content-type'>application-json</header>"
                            "<header name='token'>token</header>"
                            "<payload>'{\"key\":\"value\"}'</payload>"
                            "</request>">>).



all() -> [
          makes_http_request,
          responds_with_error_on_malformed_request
         ].

init_per_suite(Config) ->
    given_module(ejabberd_config, get_local_option,
                 fun(all_metrics_are_global) -> false end),
    application:ensure_all_started(exometer),
    mod_foreign_http:start(?HOST, ?HTTP_OPTS),
    http_helper:start(8080, '_', fun process_request/1),
    Config.

end_per_suite(_Config) ->
    http_helper:stop(),
    mod_foreign_http:stop(?HOST),
    application:stop(exometer),
    application:stop(exometer_core).

%% Tests

responds_with_error_on_malformed_request(_Config) ->
    %% GIVEN
    given_module(gen_mod, get_module_opt,
                 fun(<<"localhost">>, mod_foreign, backends, _) -> ?HTTP_OPTS end),
    {ok, Request1} = exml:parse(?REQUEST_NO_URL),
    {ok, Request2} = exml:parse(?REQUEST_BAD_METHOD),
    {ok, Request3} = exml:parse(?REQUEST_BAD_URL),
    Self = self(),
    OnResponse = fun(Resp) -> Self ! Resp end,

    [begin
         %% WHEN
         Result = mod_foreign_http:make_request(?HOST, R, OnResponse),

         %% THEN
         ?assertMatch(error, Result),
         ?assertEqual(no_message, receive X -> X after 100 -> no_message end)
     end || R <- [Request1, Request2, Request3]].

makes_http_request(_Config) ->
    %% GIVEN
    given_module(gen_mod, get_module_opt,
                 fun(<<"localhost">>, mod_foreign, backends, _) -> ?HTTP_OPTS end),
    {ok, Request} = exml:parse(?REQUEST),
    {ok, Response} = exml:parse(?RESPONSE),
    Self = self(),
    OnResponse = fun(Resp) -> Self ! Resp end,

    %% WHEN
    ok = mod_foreign_http:make_request(?HOST, Request, OnResponse),

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

given_module(ModName, FunName, Fun) ->
    catch meck:unload(ModName),
    ok = meck:new(ModName, [non_strict, passthrough]),
    ok = meck:expect(ModName, FunName, Fun).
