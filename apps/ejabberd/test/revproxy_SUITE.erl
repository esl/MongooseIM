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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [compile_routes,
     match_routes,
     upstream_uri].

groups() ->
    [].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Reverse proxy tests
%%--------------------------------------------------------------------
compile_routes(_Config) ->
    %% Given
    Expected = [{[<<"domain">>,<<"com">>],
                 [<<"abc">>],
                 '_',
                 {uri,<<"http://localhost:8080/">>}},
                {[<<"domain">>,<<"com">>],
                 '_',
                 <<"GET">>,
                 {host,<<"http://localhost:1234">>}},
                {[<<"static">>,<<"domain">>,<<"com">>],
                 '_',
                 <<"GET">>,
                 {uri,<<"http://localhost:9999/">>}},
                {[<<"abc">>,<<"domain">>,<<"com">>],
                 [],
                 '_',
                 {host,<<"http://localhost:8888">>}}],

    %% When
    Compiled = mod_revproxy:compile_routes(example_routes()),

    %% Then
    Expected = Compiled.

match_routes(_Config) ->
    %% Given
    Upstreams = [{uri, <<"http://localhost:8080/">>},
                 {host, <<"http://localhost:1234">>},
                 false,
                 {uri, <<"http://localhost:9999/">>},
                 false,
                 false,
                 {host, <<"http://localhost:8888">>},
                 false],
    %% {Host, Path, Method}
    Requests = [{<<"domain.com">>,        <<"/abc">>,       <<"GET">>},
                {<<"domain.com">>,        <<"/abc/def">>,   <<"GET">>},
                {<<"unknown.com">>,      <<"/somepath">>,   <<"GET">>},
                {<<"static.domain.com">>, <<"/file.html">>, <<"GET">>},
                {<<"static.domain.com">>, <<"/file.html">>, <<"PUT">>},
                {<<"domain.com">>,        <<"/def">>,       <<"POST">>},
                {<<"abc.domain.com">>,    <<"/">>,          <<"OPTIONS">>},
                {<<"abc.domain.com">>,    <<"/a">>,         <<"DELETE">>}],
    Rules = mod_revproxy:compile_routes(example_routes()),

    %% When
    Matched = [mod_revproxy:match(Rules, Host, Path, Method)
               || {Host, Path, Method} <- Requests],

    %% Then
    Matched = Upstreams.

upstream_uri(_Config) ->
    %% Given
    Upstreams = [{host, <<"http://localhost:9999">>},
                 {uri, <<"http://localhost:8888/">>},
                 {host, <<"https://localhost:1234">>}],
    Paths = [<<"/def/ghi/jkl/index.html">>,
             <<"/abc">>,
             <<"/">>],
    %% {Host, Path},
    Expected = [{"http://localhost:9999", <<"/def/ghi/jkl/index.html">>},
                {"http://localhost:8888", <<"/">>},
                {"https://localhost:1234", <<"/">>}],

    %% When
    Responses = [mod_revproxy:upstream_uri(Upstream, Path)
                 || {Upstream, Path} <- lists:zip(Upstreams, Paths)],

    %% Then
    Expected = Responses.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

example_routes() ->
    [{"domain.com", "/abc", "_", "http://localhost:8080/"},
     {"domain.com", get, "http://localhost:1234"},
     {"static.domain.com", get, "http://localhost:9999/"},
     {"abc.domain.com", "/", "_", "http://localhost:8888"}].
