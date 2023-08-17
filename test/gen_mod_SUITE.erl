%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
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

-module(gen_mod_SUITE).
-compile([export_all, nowarn_export_all]).
-author('bartlomiej.gorny@erlang-solutions.com').

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [start_and_stop,
     start_error,
     start_with_service_deps,
     stop_error,
     loaded_modules,
     loaded_modules_with_opts,
     get_module_opt,
     lookup_module_opt,
     hosts_with_module,
     hosts_and_opts_with_module].

init_per_testcase(_, Config) ->
    mongoose_config:set_opts(opts()),
    [setup_meck(Module) || Module <- [a_module, b_module]],
    Config.

end_per_testcase(_, Config) ->
    mongoose_config:erase_opts(),
    [meck:unload(Module) || Module <- [a_module, b_module]],
    Config.

start_and_stop(_Config) ->
    ?assertEqual({ok, ok}, gen_mod:start_module(host(a), a_module, #{})),
    ?assertError(#{what := module_not_loaded}, gen_mod:start_module(host(a), b_module, #{k => v})),
    ?assertError(#{what := module_not_loaded}, gen_mod:start_module(host(b), a_module, #{})),
    ?assertEqual({ok, ok}, gen_mod:start_module(host(b), b_module, #{k => v})),
    ?assertEqual(ok, gen_mod:stop_module(host(a), a_module)),
    ?assertError(#{what := module_not_loaded}, gen_mod:stop_module(host(a), b_module)),
    ?assertError(#{what := module_not_loaded}, gen_mod:stop_module(host(b), a_module)),
    ?assertEqual(ok, gen_mod:stop_module(host(b), b_module)).

start_error(_Config) ->
    meck:expect(a_module, start, fun(_, _) -> error(bad_weather) end),
    ?assertError(bad_weather, gen_mod:start_module(host(a), a_module, #{})).

start_with_service_deps(_Config) ->
    meck:expect(a_module, deps, fun(_, _) -> [{service, a_service}] end),
    ?assertError(#{what := service_not_loaded}, gen_mod:start_module(host(a), a_module, #{})),
    mongoose_config:set_opt(services, #{a_service => #{}}),
    ?assertEqual({ok, ok}, gen_mod:start_module(host(a), a_module, #{})).

stop_error(_Config) ->
    meck:expect(a_module, stop, fun(_) -> error(bad_mood) end),
    ?assertError(bad_mood, gen_mod:stop_module(host(a), a_module)).

loaded_modules(_Config) ->
    ?assertEqual([a_module], gen_mod:loaded_modules(host(a))),
    ?assertEqual([b_module], gen_mod:loaded_modules(host(b))),
    ?assertEqual([a_module, b_module], gen_mod:loaded_modules()).

loaded_modules_with_opts(_Config) ->
    MA = #{a_module => #{}},
    MB = #{b_module => #{k => v}},
    ?assertEqual(MA, gen_mod:loaded_modules_with_opts(host(a))),
    ?assertEqual(MB, gen_mod:loaded_modules_with_opts(host(b))),
    ?assertEqual(#{host(a) => MA, host(b) => MB}, gen_mod:loaded_modules_with_opts()).

get_module_opt(_Config) ->
    ?assertEqual(v, gen_mod:get_module_opt(host(b), b_module, k)),
    ?assertError({badkey, k}, gen_mod:get_module_opt(host(a), a_module, k)),
    ?assertError({badkey, b_module}, gen_mod:get_module_opt(host(a), b_module, k)),
    ?assertEqual(default, gen_mod:get_module_opt(host(a), a_module, k, default)),
    ?assertEqual(default, gen_mod:get_module_opt(host(a), b_module, k, default)).

lookup_module_opt(_Config) ->
    ?assertEqual({ok, v}, gen_mod:lookup_module_opt(host(b), b_module, k)),
    ?assertEqual({error, not_found}, gen_mod:lookup_module_opt(host(a), a_module, k)),
    ?assertEqual({error, not_found}, gen_mod:lookup_module_opt(host(a), b_module, k)).

hosts_with_module(_Config) ->
    ?assertEqual([host(a)], gen_mod:hosts_with_module(a_module)),
    ?assertEqual([host(b)], gen_mod:hosts_with_module(b_module)).

hosts_and_opts_with_module(_Config) ->
    ?assertEqual(#{host(a) => #{}}, gen_mod:hosts_and_opts_with_module(a_module)),
    ?assertEqual(#{host(b) => #{k => v}}, gen_mod:hosts_and_opts_with_module(b_module)).

host(a) ->
    <<"localhost">>;
host(b) ->
    <<"localhost.bis">>.

setup_meck(Module) ->
    meck:new(Module, [non_strict]),
    meck:expect(Module, start, fun(_, _) -> ok end),
    meck:expect(Module, stop, fun(_) -> ok end).

opts() ->
    #{hosts => [host(a), host(b)],
      host_types => [],
      services => #{},
      {modules, host(a)} => #{a_module => #{}},
      {modules, host(b)} => #{b_module => #{k => v}}}.
