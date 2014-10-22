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

-include("mod_revproxy.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, compile_routes},
     {group, match_routes},
     {group, generate_upstream}].

groups() ->
    [{compile_routes, [sequence], [compile_example_routes,
                                   example_dynamic_compile]},
     {match_routes, [sequence], [exact_path_match,
                                 remainder_match,
                                 capture_subdomain_match,
                                 method_match,
                                 slash_ending_match]},
     {generate_upstream, [sequence], [upstream_uri,
                                      upstream_host,
                                      upstream_bindings,
                                      upstream_slash_path,
                                      upstream_slash_remainder]}].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(match_routes, Config) ->
    Rules = mod_revproxy:compile_routes(example_routes()),
    [{rules, Rules}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    Config.

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
    Upstream1 = mod_revproxy:match(Rules, Host, Path, Method1),
    Upstream2 = mod_revproxy:match(Rules, Host, Path, Method2),

    %% Then
    #match{upstream = {uri, [<<"http://localhost:8080">>]}} = Upstream1
                                                            = Upstream2.

remainder_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"domain.com">>,
    Path1 = <<"/abc/def/ghi/index.html">>,
    Path2 = <<"/def/ghi/index.html">>,
    Method = <<"GET">>,

    %% When
    Upstream1 = mod_revproxy:match(Rules, Host, Path1, Method),
    Upstream2 = mod_revproxy:match(Rules, Host, Path2, Method),

    %% Then
    #match{upstream = {uri, [<<"http://localhost:8080">>]},
           remainder = [<<"def">>, <<"ghi">>, <<"index.html">>],
           path = [<<"abc">>]} = Upstream1,

    #match{upstream = {host, [<<"http://localhost:1234">>]},
           remainder = [<<"def">>, <<"ghi">>, <<"index.html">>],
           path = '_'} = Upstream2.

capture_subdomain_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host1 = <<"static.domain.com">>,
    Host2 = <<"nonstatic.domain.com">>,
    Path = <<"/a/b/c">>,
    Method = <<"GET">>,

    %% When
    Upstream1 = mod_revproxy:match(Rules, Host1, Path, Method),
    Upstream2 = mod_revproxy:match(Rules, Host2, Path, Method),

    %% Then
    #match{upstream = {uri, [<<"http://localhost:9999">>]},
           remainder = [<<"a">>, <<"b">>, <<"c">>],
           path = '_'} = Upstream1,

    #match{upstream = {uri, [<<"http://localhost:8888">>, whatever]},
           remainder = [<<"a">>, <<"b">>, <<"c">>],
           bindings = [{whatever, <<"nonstatic">>}],
           path = []} = Upstream2.

method_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"domain.com">>,
    Path = <<"/path/a/b/c">>,
    Method1 = <<"GET">>,
    Method2 = <<"POST">>,

    %% When
    Upstream1 = mod_revproxy:match(Rules, Host, Path, Method1),
    Upstream2 = mod_revproxy:match(Rules, Host, Path, Method2),

    %% Then
    #match{upstream = {host, [<<"http://localhost:1234">>]},
           remainder = [<<"path">>, <<"a">>, <<"b">>, <<"c">>],
           path = '_'} = Upstream1,    

    #match{upstream = {host, [<<"http://localhost:6543">>,<<"detailed_path">>,
                              host, path]},
           remainder = [<<"b">>, <<"c">>],
           bindings = Bindings,
           path = [<<"path">>, path, <<>>]} = Upstream2,
    <<"domain">> = proplists:get_value(host, Bindings),
    <<"a">> = proplists:get_value(path, Bindings).

slash_ending_match(Config) ->
    %% Given
    Rules = ?config(rules, Config),
    Host = <<"dummydomain.com">>,
    Path = <<"/a/b/c/">>,
    Method = <<"GET">>,

    %% When
    Upstream = mod_revproxy:match(Rules, Host, Path, Method),

    %% Then
    #match{upstream = {host, [<<"http://localhost:5678">>, placeholder]},
           remainder = [<<"a">>, <<"b">>, <<"c">>, <<>>],
           bindings = [{placeholder, <<"dummydomain">>}],
           path = '_'} = Upstream.

%%--------------------------------------------------------------------
%% Upstream URI generation
%%--------------------------------------------------------------------
upstream_uri(_Config) ->
    %% Given
    Upstream = {uri, [<<"http://localhost:8080">>]},
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
    Upstream = {host, [<<"http://localhost:8080">>]},
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
    Upstream = {uri, [<<"https://localhost:8080">>, <<"host">>, host, 
                      <<"domain">>, domain]},
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
    {"https://localhost:8080",
     <<"/host/test_host/domain/test_domain/dir/index.html">>} = URI.

upstream_slash_path(_Config) ->
    %% Given
    Upstream = {host, [<<"http://localhost:1234">>, <<"abc">>]},
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
    Upstream = {host, [<<"http://localhost:1234">>, <<"abc">>]},
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
      {uri, [<<"http://localhost:8080">>]}},
     {[<<"com">>, <<"domain">>], '_', <<"GET">>,
      {host, [<<"http://localhost:1234">>]}},
     {[<<"com">>,<<"domain">>,<<"static">>], '_', <<"GET">>,
      {uri, [<<"http://localhost:9999">>]}},
     {[<<"com">>, host], [<<"path">>, path, <<>>], '_',
      {host, [<<"http://localhost:6543">>, <<"detailed_path">>, host, path]}},
     {[<<"com">>, placeholder], '_', <<"GET">>,
      {host, [<<"http://localhost:5678">>, placeholder]}},
     {[<<"com">>, <<"domain">>, whatever], [], '_',
      {uri, [<<"http://localhost:8888">>, whatever]}}].
