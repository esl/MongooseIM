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

init_per_suite(Config) ->
    dynamic_modules:stop(host(a), mod_vcard),
    dynamic_modules:stop(host(b), mod_vcard),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

start_and_stop(_Config) ->
    {ok, _} = escalus_ejabberd:rpc(gen_mod, start_module, [host(a), mod_vcard, []]),
    {ok, _} = escalus_ejabberd:rpc(gen_mod, start_module, [host(b), mod_vcard, []]),
    {error, already_started} = escalus_ejabberd:rpc(gen_mod, start_module, [host(a), mod_vcard, []]),
    {error, already_started} = escalus_ejabberd:rpc(gen_mod, start_module, [host(b), mod_vcard, []]),
    ok = escalus_ejabberd:rpc(gen_mod, stop_module, [host(a), mod_vcard]),
    ok = escalus_ejabberd:rpc(gen_mod, stop_module, [host(b), mod_vcard]),
    {error, not_loaded} = escalus_ejabberd:rpc(gen_mod, stop_module, [host(a), mod_vcard]),
    {error, not_loaded} = escalus_ejabberd:rpc(gen_mod, stop_module, [host(b), mod_vcard]),
    ok.

host(a) ->
    <<"localhost">>;
host(b) ->
    <<"localhost.bis">>.
