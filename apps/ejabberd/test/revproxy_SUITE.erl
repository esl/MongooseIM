%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(revproxy_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include_lib("ejabberd/include/mod_revproxy.hrl").

-import(ejabberd_helper, [start_ejabberd/1,
                          stop_ejabberd/0,
                          use_config_file/2,
                          start_ejabberd_with_config/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, compile_routes},
     {group, match_routes},
     {group, generate_upstream},
     {group, requests_http},
     conf_reload].

groups() ->
    [{compile_routes, [sequence], [compile_example_routes,
                                   example_dynamic_compile]},
     {match_routes, [sequence], [exact_path_match,
                                 remainder_match,
                                 capture_subdomain_match,
                                 method_match,
                                 qs_match,
                                 slash_ending_match]},
     {generate_upstream, [sequence], [upstream_uri,
                                      upstream_host,
                                      upstream_bindings,
                                      upstream_qs,
                                      upstream_slash_path,
                                      upstream_slash_remainder]},
     {requests_http, [sequence], [no_upstreams,
                                  http_upstream,
                                  nomatch_upstream,
                                  https_upstream]}].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

-define(APPS, [crypto, ssl, fusco, ranch, cowlib, cowboy]).

init_per_suite(Config) ->
    [application:start(App) || App <- ?APPS],
    {ok, Pid} = create_handler(),
    [{meck_pid, Pid}|Config].

end_per_suite(Config) ->
    remove_handler(Config),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.

init_per_group(requests_http, Config) ->
    start_revproxy(),
    Config;
init_per_group(match_routes, Config) ->
    Rules = mod_revproxy:compile_routes(example_routes()),
    [{rules, Rules}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(requests_http, Config) ->
    stop_revproxy(),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(http_upstream, Config) ->
    meck:new(inet, [unstick, passthrough]),
    meck:expect(inet, getaddr, fun mock_getaddr/2),
    meck:expect(inet, getaddrs_tm, fun mock_getaddrs_tm/3),

    start_http_upstream(),

    Config;
init_per_testcase(https_upstream, Config) ->
    start_https_upstream(Config),
    Config;
init_per_testcase(conf_reload, Config) ->
    start_http_upstream(),
    Config;
init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(http_upstream, Config) ->
    stop_upstream(http_listener),
    meck:unload(inet),
    Config;
end_per_testcase(https_upstream, Config) ->
    stop_upstream(https_listener),
    Config;
end_per_testcase(conf_reload, Config) ->
    stop_upstream(http_listener),
    Config;
end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Routes compile tests
%%--------------------------------------------------------------------
compile_example_routes(_Config) ->
    %% Given
    Expected = compiled_example_routes(),

    %% When
    Compiled = mod_revproxy:compile_routes(example_routes()),

    %% Then
    Expected = Compiled.

example_dynamic_compile(_Config) ->
    %% Given
    Expected = compiled_example_routes(),

    %% When
    ok = mod_revproxy:compile(example_routes()),

    %% Then
    Expected = mod_revproxy_dynamic:rules().

%%--------------------------------------------------------------------
%% Configuration reload test
%%--------------------------------------------------------------------
conf_reload(Config) ->
    %% Given initial configuration
    Host = "http://localhost:5280",
    Path = <<"/">>,
    Method = "GET",
    Headers = [],
    Body = [],

    copy(data("ejabberd.onerule.cfg", Config), data("ejabberd.cfg", Config)),
    start_ejabberd_with_config(Config, "ejabberd.cfg"),

    %% When making request for http
    Response1 = execute_request(Host, Path, Method, Headers, Body),

    %% Then it returns 200
    true = is_status_code(Response1, 200),

    %% Given new configuration
    copy(data("ejabberd.norules.cfg", Config), data("ejabberd.cfg", Config)),
    ejabberd_config:reload_local(),

    %% When request is replayed
    Response2 = execute_request(Host, Path, Method, Headers, Body),

    %% Then it returns 404
    true = is_status_code(Response2, 404),

    ok = stop_ejabberd().

%%--------------------------------------------------------------------
%% HTTP requests tests
%%--------------------------------------------------------------------
no_upstreams(_Config) ->
    %% Given
    Host = "http://localhost:8080",
    Path = <<"/abc/index.html">>,
    Method = "GET",
    Headers = [{<<"host">>, <<"qwerty.com">>}],
    Body = [],

    %% When
    Response = execute_request(Host, Path, Method, Headers, Body),

    %% Then
    true = is_status_code(Response, 502).

http_upstream(_Config) ->
    %% Given
    Host = "http://localhost:8080",
    QS = <<"ts=1231231232222&st=223232&page=32">>,
    Path = <<"/abc/login.htm?", QS/binary>>,
    Method = "GET",
    Headers = [{<<"host">>, <<"qwerty.com">>}],
    Body = "some example body :)",

    %% When
    Response = execute_request(Host, Path, Method, Headers, Body),

    %% Then
    true = is_status_code(Response, 200),
    true = does_response_match(Response,
                               <<"qwerty.com">>,
                               <<"domain/qwerty/path/abc/login.htm">>,
                               Method,
                               Body,
                               QS),

    true = does_contain_header(Response, <<"custom-header-1">>, <<"value">>),
    true = does_contain_header(Response, <<"custom-header-2">>,
                               <<"some other value">>).

nomatch_upstream(_Config) ->
    %% Given
    Host = "http://localhost:8080",
    Path = <<"/abc/def">>,
    Method = "GET",
    Headers = [{<<"host">>, <<"domain.net">>}],
    Body = [],

    %% When
    Response = execute_request(Host, Path, Method, Headers, Body),

    %% Then
    true = is_status_code(Response, 404).

https_upstream(_Config) ->
    %% Given
    Host = "http://localhost:8080",
    Path = <<"/admin/index.html">>,
    Method = "POST",
    Headers = [{<<"host">>, <<"otherdomain.com">>}],
    Body = [],

    %% When
    Response = execute_request(Host, Path, Method, Headers, Body),

    %% Then
    true = is_status_code(Response, 200),
    true = does_response_match(Response,
                               <<"otherdomain.com">>,
                               <<"secret_admin/otherdomain/index.html">>,
                               Method,
                               Body,
                               <<>>).

%%--------------------------------------------------------------------
%% Routes matching tests
%%--------------------------------------------------------------------
exact_path_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"domain.com">>,
    Path = <<"/abc">>,
    Method1 = <<"GET">>,
    Method2 = <<"POST">>,

    %% When
    Match1 = mod_revproxy:match(Rules, Host, Path, Method1),
    Match2 = mod_revproxy:match(Rules, Host, Path, Method2),

    %% Then
    Upstream = #upstream{type = uri,
                         protocol = <<"http://">>,
                         host = [<<"localhost:8080">>]},
    #match{upstream = Upstream} = Match1
                                = Match2.

remainder_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"domain.com">>,
    Path1 = <<"/abc/def/ghi/index.html">>,
    Path2 = <<"/def/ghi/index.html">>,
    Method = <<"GET">>,

    %% When
    Match1 = mod_revproxy:match(Rules, Host, Path1, Method),
    Match2 = mod_revproxy:match(Rules, Host, Path2, Method),

    %% Then
    Upstream1 = #upstream{type = uri,
                          protocol = <<"http://">>,
                          host = [<<"localhost:8080">>]},
    #match{upstream = Upstream1,
           remainder = [<<"def">>, <<"ghi">>, <<"index.html">>],
           path = [<<"abc">>]} = Match1,

    Upstream2 = #upstream{type = host,
                          protocol = <<"http://">>,
                          host = [<<"localhost:1234">>]},
    #match{upstream = Upstream2,
           remainder = [<<"def">>, <<"ghi">>, <<"index.html">>],
           path = '_'} = Match2.

capture_subdomain_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host1 = <<"static.domain.com">>,
    Host2 = <<"nonstatic.domain.com">>,
    Path = <<"/a/b/c">>,
    Method = <<"GET">>,

    %% When
    Match1 = mod_revproxy:match(Rules, Host1, Path, Method),
    Match2 = mod_revproxy:match(Rules, Host2, Path, Method),

    %% Then
    Upstream1 = #upstream{type = uri,
                          protocol = <<"http://">>,
                          host = [<<"localhost:9999">>]},
    #match{upstream = Upstream1,
           remainder = [<<"a">>, <<"b">>, <<"c">>],
           path = '_'} = Match1,

    Upstream2 = #upstream{type = uri,
                          protocol = <<"http://">>,
                          host = [<<"localhost:8888">>],
                          path = [whatever, <<>>]},
    #match{upstream = Upstream2,
           remainder = [<<"a">>, <<"b">>, <<"c">>],
           bindings = [{whatever, <<"nonstatic">>}],
           path = []} = Match2.

method_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"domain.com">>,
    Path = <<"/path/a/b/c">>,
    Method1 = <<"GET">>,
    Method2 = <<"POST">>,

    %% When
    Match1 = mod_revproxy:match(Rules, Host, Path, Method1),
    Match2 = mod_revproxy:match(Rules, Host, Path, Method2),

    %% Then
    Upstream1 = #upstream{type = host,
                          protocol = <<"http://">>,
                          host = [<<"localhost:1234">>]},
    #match{upstream = Upstream1,
           remainder = [<<"path">>, <<"a">>, <<"b">>, <<"c">>],
           path = '_'} = Match1,

    Upstream2 = #upstream{type = uri,
                          protocol = <<"http://">>,
                          host = [<<"localhost:6543">>],
                          path = [<<"detailed_path">>, host, path]},
    #match{upstream = Upstream2,
           remainder = [<<"b">>, <<"c">>],
           bindings = Bindings,
           path = [<<"path">>, path, <<>>]} = Match2,
    <<"domain">> = proplists:get_value(host, Bindings),
    <<"a">> = proplists:get_value(path, Bindings).

qs_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"dummydomain.com">>,
    QS = <<"login.htm?ts=1231231232222&st=223232&page=32&ap=123442"
            "&whatever=somewordshere">>,
    Path = <<"/a/b/c/", QS/binary>>,
    Method = <<"GET">>,

    %% When
    Match = mod_revproxy:match(Rules, Host, Path, Method),

    %% Then
    Upstream = #upstream{type = uri,
                         protocol = <<"http://">>,
                         host = [<<"localhost:5678">>],
                         path = [placeholder]},
    #match{upstream = Upstream,
           remainder = [<<"a">>, <<"b">>, <<"c">>, QS],
           bindings = [{placeholder, <<"dummydomain">>}],
           path = '_'} = Match.


slash_ending_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"dummydomain.com">>,
    Path = <<"/a/b/c/">>,
    Method = <<"GET">>,

    %% When
    Match = mod_revproxy:match(Rules, Host, Path, Method),

    %% Then
    Upstream = #upstream{type = uri,
                         protocol = <<"http://">>,
                         host = [<<"localhost:5678">>],
                         path = [placeholder]},
    #match{upstream = Upstream,
           remainder = [<<"a">>, <<"b">>, <<"c">>, <<>>],
           bindings = [{placeholder, <<"dummydomain">>}],
           path = '_'} = Match.

%%--------------------------------------------------------------------
%% Upstream URI generation
%%--------------------------------------------------------------------
upstream_uri(_Config) ->
    %% Given
    Upstream = #upstream{type = uri,
                         protocol = <<"http://">>,
                         host = [<<"localhost:8080">>]},
    Remainder = [<<"def">>, <<"index.html">>],
    Bindings = [{host, <<"domain">>}],
    Path = [<<"host">>, host],
    Match = #match{upstream = Upstream,
                   remainder = Remainder,
                   bindings = Bindings,
                   path = Path},

    %% When
    URI = mod_revproxy:upstream_uri(Match),

    %% Then
    {"http://localhost:8080", <<"/def/index.html">>} = URI.

upstream_host(_Config) ->
    %% Given
    Upstream = #upstream{type = host,
                         protocol = <<"http://">>,
                         host = [<<"localhost:8080">>]},
    Remainder = [<<"def">>, <<"index.html">>],
    Bindings = [],
    Path = [<<"host">>],
    Match = #match{upstream = Upstream,
                   remainder = Remainder,
                   bindings = Bindings,
                   path = Path},

    %% When
    URI = mod_revproxy:upstream_uri(Match),

    %% Then
    {"http://localhost:8080", <<"/host/def/index.html">>} = URI.

upstream_bindings(_Config) ->
    %% Given
    Upstream = #upstream{type = uri,
                         protocol = <<"https://">>,
                         host = [domain, host, <<"localhost:8080">>],
                         path = [<<"host">>, host, <<"domain">>, domain]},
    Remainder = [<<"dir">>, <<"index.html">>],
    Bindings = [{host, <<"test_host">>}, {domain, <<"test_domain">>}],
    Path = '_',
    Match = #match{upstream = Upstream,
                   remainder = Remainder,
                   bindings = Bindings,
                   path = Path},

    %% When
    URI = mod_revproxy:upstream_uri(Match),

    %% Then
    {"https://test_domain.test_host.localhost:8080",
     <<"/host/test_host/domain/test_domain/dir/index.html">>} = URI.

upstream_qs(_Config) ->
    %% Given
    Upstream = #upstream{type = uri,
                         protocol = <<"http://">>,
                         host = [<<"localhost:1234">>],
                         path = [<<"ghi">>]},
    Path = [<<"abc">>],
    QS = <<"login.htm?ts=1231231232222&st=223232&page=32&ap=123442"
            "&whatever=somewordshere">>,
    Remainder = [<<"def">>, QS],
    Match = #match{upstream = Upstream,
                   remainder = Remainder,
                   bindings = [],
                   path = Path},

    %% When
    URI = mod_revproxy:upstream_uri(Match),

    %% Then
    Resource = <<"/ghi/def/", QS/binary>>,
    {"http://localhost:1234", Resource} = URI.

upstream_slash_path(_Config) ->
    %% Given
    Upstream = #upstream{type = host,
                         protocol = <<"http://">>,
                         host = [<<"localhost:1234">>],
                         path = [<<"abc">>]},
    Path = [<<"abc">>, <<"def">>, <<>>],
    Match = #match{upstream = Upstream,
                   remainder = [],
                   bindings = [],
                   path = Path},

    %% When
    URI = mod_revproxy:upstream_uri(Match),

    %% Then
    {"http://localhost:1234", <<"/abc/abc/def/">>} = URI.

upstream_slash_remainder(_Config) ->
    %% Given
    Upstream = #upstream{type = host,
                         protocol = <<"http://">>,
                         host = [<<"localhost:1234">>],
                         path = [<<"abc">>]},
    Path = [<<"abc">>, <<"def">>, <<>>],
    Remainder = [<<"ghi">>, <<"jkl">>, <<>>],
    Match = #match{upstream = Upstream,
                   remainder = Remainder,
                   bindings = [],
                   path = Path},

    %% When
    URI = mod_revproxy:upstream_uri(Match),

    %% Then
    {"http://localhost:1234", <<"/abc/abc/def/ghi/jkl/">>} = URI.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
copy(Src, Dst) ->
    {ok, _} = file:copy(Src, Dst).

data(File, Config) ->
    filename:join([?config(data_dir, Config), File]).

example_routes() ->
    [{"domain.com", "/abc", "_", "http://localhost:8080/"},
     {"domain.com", get, "http://localhost:1234"},
     {"static.domain.com", get, "http://localhost:9999/"},
     {":host.com", "/path/:path/", "_",
      "http://localhost:6543/detailed_path/:host/:path"},
     {":placeholder.com", get, "http://localhost:5678/:placeholder"},
     {":whatever.domain.com", "/", "_", "http://localhost:8888/:whatever/"}].

compiled_example_routes() ->
    [{[<<"com">>, <<"domain">>], [<<"abc">>], '_',
      #upstream{type = uri,
                protocol = <<"http://">>,
                host = [<<"localhost:8080">>]}},
     {[<<"com">>, <<"domain">>], '_', <<"GET">>,
      #upstream{type = host,
                protocol = <<"http://">>,
                host = [<<"localhost:1234">>]}},
     {[<<"com">>,<<"domain">>,<<"static">>], '_', <<"GET">>,
      #upstream{type = uri,
                protocol = <<"http://">>,
                host = [<<"localhost:9999">>]}},
     {[<<"com">>, host], [<<"path">>, path, <<>>], '_',
      #upstream{type = uri,
                protocol = <<"http://">>,
                host = [<<"localhost:6543">>],
                path = [<<"detailed_path">>, host, path]}},
     {[<<"com">>, placeholder], '_', <<"GET">>,
      #upstream{type = uri,
                protocol = <<"http://">>,
                host = [<<"localhost:5678">>],
                path = [placeholder]}},
     {[<<"com">>, <<"domain">>, whatever], [], '_',
      #upstream{type = uri,
                protocol = <<"http://">>,
                host = [<<"localhost:8888">>],
                path = [whatever, <<>>]}}].

start_revproxy() ->
    Routes = {routes, [{":domain.com", "/admin", "_",
                        "https://localhost:5678/secret_admin/:domain/"},
                       {":domain.com", "/:path/", get,
                        "http://:domain.localhost:1234/domain/:domain/path/:path/"}]},
    CustomHeaders = [{<<"custom-header-1">>, <<"value">>},
                     {<<"custom-header-2">>, <<"some other value">>}],
    Dispatch = cowboy_router:compile([
                {'_',
                 [{"/[...]", mod_revproxy, [{custom_headers, CustomHeaders}]}]}
                ]),
    mod_revproxy:start(nvm, [Routes]),
    cowboy:start_http(revproxy_listener, 20, [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]).

stop_revproxy() ->
    ok = cowboy:stop_listener(revproxy_listener).

start_http_upstream() ->
    Dispatch = cowboy_router:compile([
                {'_', [{"/[...]", revproxy_handler, []}]}
                ]),
    cowboy:start_http(http_listener, 20, [{port, 1234}],
                      [{env, [{dispatch, Dispatch}]}]).

start_https_upstream(Config) ->
    Dispatch = cowboy_router:compile([
                {'_', [{"/[...]", revproxy_handler, []}]}
                ]),
    Opts = [{port, 5678},
            {keyfile, data("server.key", Config)},
            {certfile, data("server.crt", Config)}],
    cowboy:start_https(https_listener, 20, Opts,
                       [{env, [{dispatch, Dispatch}]}]).

stop_upstream(Upstream) ->
    ok = cowboy:stop_listener(Upstream).

execute_request(Host, Path, Method, Headers, Body) ->
    {ok, Pid} = fusco:start_link(Host, []),
    Response = fusco:request(Pid, Path, Method, Headers, Body, 5000),
    fusco:disconnect(Pid),
    Response.

is_status_code({ok, {{CodeBin, _}, _, _, _, _}}, Code) ->
    case binary_to_integer(CodeBin) of
        Code -> true;
        _    -> false
    end.

does_response_match({ok, {{_, _}, _, Response, _, _}},
                    Host, Path, Method, Body, QS) ->
    ResponseEls = binary:split(Response, <<"\n">>, [global]),
    [RHost,RMethod,RPath,RBody,RQS|_] = ResponseEls,
    PathSegments = binary:split(Path, <<"/">>, [global, trim]),
    RPath = to_formatted_binary(PathSegments),
    RHost = to_formatted_binary(Host),
    RMethod = to_formatted_binary(list_to_binary(Method)),
    RBody = to_formatted_binary(list_to_binary(Body)),
    RQS = to_formatted_binary(QS),
    true.

to_formatted_binary(Subject) ->
    iolist_to_binary(io_lib:format("~p", [Subject])).

does_contain_header({ok, {{_, _}, _, Response, _, _}}, Header, Value) ->
    HeaderL = cowboy_bstr:to_lower(Header),
    ValueL = cowboy_bstr:to_lower(Value),
    Match = iolist_to_binary(io_lib:format("~p", [{HeaderL, ValueL}])),
    case binary:match(Response, Match) of
        nomatch -> false;
        _       -> true
    end.

%%--------------------------------------------------------------------
%% revproxy handler mock
%%--------------------------------------------------------------------
create_handler() ->
    Owner = self(),
    F = fun() ->
            ok = meck:new(revproxy_handler, [non_strict]),
            ok = meck:expect(revproxy_handler, init, fun handler_init/3),
            ok = meck:expect(revproxy_handler, handle, fun handler_handle/2),
            ok = meck:expect(revproxy_handler, terminate, fun handler_terminate/3),
            Owner ! ok,
            timer:sleep(infinity)
    end,
    Pid = spawn(F),
    receive
        ok ->
            {ok, Pid}
    after 5000 ->
            {error, timeout}
    end.

remove_handler(Config) ->
    meck:unload(revproxy_handler),
    exit(?config(meck_pid, Config), kill).

handler_init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

handler_handle(Req, State) ->
    {Host, Req2} = cowboy_req:host(Req),
    {PathInfo, Req3} = cowboy_req:path_info(Req2),
    {Method, Req4} = cowboy_req:method(Req3),
    {Headers, Req5} = cowboy_req:headers(Req4),
    ContentType = [{<<"content-type">>, <<"text/plain">>}],
    {Body, Req7} = case cowboy_req:has_body(Req5) of
        false ->
            {<<>>, Req5};
        true ->
            {ok, Body1, Req6} = cowboy_req:body(Req5),
            {Body1, Req6}
    end,
    {QS, Req8} = cowboy_req:qs(Req7),
    Response = io_lib:format("~p~n~p~n~p~n~p~n~p~n~p",
                             [Host, Method, PathInfo, Body, QS, Headers]),
    {ok, Req9} = cowboy_req:reply(200, ContentType, Response, Req8),
    {ok, Req9, State}.

handler_terminate(_Reason, _Req, _State) ->
    ok.

mock_getaddr("qwerty.localhost",inet) ->
    {ok, {127,0,0,1}};
mock_getaddr("qwerty.localhost",inet6) ->
    {ok,{0,0,0,0,0,0,0,1}};
mock_getaddr(Host, Opt) ->
    meck:passthrough([Host, Opt]).


mock_getaddrs_tm("qwerty.localhost",inet,_) ->
    {ok, [{127,0,0,1}]};
mock_getaddrs_tm("qwerty.localhost",inet6,_) ->
    {ok,[{0,0,0,0,0,0,0,1}]};
mock_getaddrs_tm(Host, Opt, Timer) ->
    meck:passthrough([Host, Opt, Timer]).
