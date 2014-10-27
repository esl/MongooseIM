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
-module(connect_SUITE).

-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SECURE_USER, secure_joe).
-define(TLS_VERSIONS, [tlsv1, 'tlsv1.1', 'tlsv1.2']).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------


all() ->
    [should_fail_with_sslv3, should_pass_with_all_tls_versions_up_to_12].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1, {by_name, [?SECURE_USER]}).

end_per_suite(Config0) ->
    Config1 = escalus:delete_users(Config0, {by_name, [?SECURE_USER]}),
    escalus:end_per_suite(Config1).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

should_fail_with_sslv3(Config) ->
    %% GIVEN
    UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, sslv3),

    %% WHEN
    try escalus_connection:start(UserSpec1) of
    %% THEN
        _ ->
            error(client_connected_using_sslv3)
    catch
        error:closed ->
            ok
    end.

should_pass_with_all_tls_versions_up_to_12(Config) ->
    [begin
         UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
         UserSpec1 = set_secure_connection_protocol(UserSpec0, Version),

         %% WHEN
         Result = escalus_connection:start(UserSpec1),

         %% THEN
         ?assertMatch({ok, _, _, _}, Result)
     end || Version <- ?TLS_VERSIONS].

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

set_secure_connection_protocol(UserSpec, Version) ->
    [{ssl_opts, [{versions, [Version]}]} | UserSpec].
