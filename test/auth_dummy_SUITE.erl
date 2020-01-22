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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     check_password_3,
     check_password_5,
     authorize,
     plain_password_required
    ].

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

check_password_3(_Config) ->
    true = ejabberd_auth_dummy:check_password(<<"alice">>, ?DOMAIN, <<"makota">>),
    true = ejabberd_auth_dummy:check_password(<<"alice">>, ?DOMAIN, <<"niemakota">>),
    true = ejabberd_auth_dummy:check_password(<<"kate">>, ?DOMAIN, <<"mapsa">>).

check_password_5(_Config) ->
    false = ejabberd_auth_dummy:check_password(<<"alice">>, ?DOMAIN, <<"makota">>,
                                               <<"digest">>, fun() -> true end),
    false = ejabberd_auth_dummy:check_password(<<"alice">>, ?DOMAIN, <<"niemakota">>,
                                               <<"digest">>, fun() -> true end),
    false = ejabberd_auth_dummy:check_password(<<"kate">>, ?DOMAIN, <<"mapsa">>,
                                               <<"digest">>, fun() -> true end).

authorize(_Config) ->
    stringprep:start(),
    Creds = mongoose_credentials:new(?DOMAIN),
    {ok, _Creds2} = ejabberd_auth_dummy:authorize(Creds).

plain_password_required(_Config) ->
    true = ejabberd_auth_dummy:plain_password_required().
