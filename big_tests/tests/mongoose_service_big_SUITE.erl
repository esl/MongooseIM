%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
-module(mongoose_service_big_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [start_stop_mod_admin_extra].

all_tests() ->
    [ping,
     active,
     active_keep_alive,
     server_ping_pong,
     server_ping_pang].
suite() ->
    escalus:suite().

start_stop_mod_admin_extra(_Config) ->
    WasLoaded = rpc(mongoose_service, is_loaded, [mod()]),
    case WasLoaded of
        true ->
            ok;
        false ->
            {ok, _} = rpc(mongoose_service, start_service, [mod(), []])
    end,
    {error, already_started} = rpc(mongoose_service, start_service, [mod(), []]),
    ok = rpc(mongoose_service, stop_service, [mod()]),
    {error, not_running} = rpc(mongoose_service, stop_service, [mod()]),
    case WasLoaded of
        false ->
            ok;
        true ->
            {ok, _} = rpc(mongoose_service, start_service, [mod(), []])
    end,
    ok.

rpc(Mod, Fun, Args) ->
    escalus_ejabberd:rpc(Mod, Fun, Args).

mod() -> mod_admin_extra.
