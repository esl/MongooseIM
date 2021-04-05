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

-module(auth_dummy_SUITE).
-compile(export_all).
-author('kacper.mentel@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").

-define(DOMAIN, <<"localhost">>).
-define(HOST_TYPE, ?DOMAIN).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [authorize].

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

authorize(_Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Creds = mongoose_credentials:new(?DOMAIN, ?HOST_TYPE),
    {ok, _Creds2} = ejabberd_auth_dummy:authorize(Creds).
