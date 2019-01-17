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
-compile(export_all).
-author('bartlomiej.gorny@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [start_and_stop].

init_per_testcase(_, Config) ->
    meck:new(ejabberd_config, [passthrough]),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    meck:expect(ejabberd_config, add_local_option, fun(_, _) -> {atomic, ok} end),
    meck:expect(ejabberd_config, del_local_option, fun(_) -> {atomic, ok} end),
    meck:new(a_module, [non_strict]),
    meck:expect(a_module, start, fun(_, _) -> ok end),
    meck:expect(a_module, stop, fun(_) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(ejabberd_config),
    meck:unload(a_module),
    Config.

start_and_stop(_Config) ->
    gen_mod:start(),
    gen_mod:stop_module(host(a), a_module),
    gen_mod:stop_module(host(b), a_module),
    {ok, _} = gen_mod:start_module(host(a), a_module, []),
    {ok, _} = gen_mod:start_module(host(b), a_module, []),
    {error, already_started} = gen_mod:start_module(host(a), a_module, []),
    {error, already_started} = gen_mod:start_module(host(b), a_module, []),
    {ok, []} = gen_mod:stop_module(host(a), a_module),
    {ok, []} = gen_mod:stop_module(host(b), a_module),
    {error, not_loaded} = gen_mod:stop_module(host(a), a_module),
    {error, not_loaded} = gen_mod:stop_module(host(b), a_module),
    ok.

host(a) ->
    <<"localhost">>;
host(b) ->
    <<"localhost.bis">>.
