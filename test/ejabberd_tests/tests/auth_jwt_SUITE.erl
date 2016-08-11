%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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
-module(auth_jwt_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
-define(USERNAME, <<"10857839">>).
-define(JWT_KEY, <<"testtesttest">>).

all() ->
    [{group, jwt}].

groups() ->
    [{jwt, [parallel], [auth_ok, auth_fail]}].

suite() ->
    escalus:suite().

init_per_suite(Config1) ->
    Config2 = setup_ejabberd_node(Config1),
    escalus:init_per_suite(Config2).

setup_ejabberd_node(Config0) ->
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),

    ejabberd_node_utils:modify_config_file(
      [{auth_method, jwt},
       {jwt_auth_opts, "{jwt_key, \"" ++ binary_to_list(?JWT_KEY) ++ "\"},
                        {token_user_key, bookingNumber}"}
      ], Config1),

    ejabberd_node_utils:restart_application(ejabberd),
    Config1.

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    restore_ejabberd_node(Config).

restore_ejabberd_node(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(ejabberd).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% JSON Web Token tests
%%--------------------------------------------------------------------

auth_ok(_Config) ->
    Password = generate_token(0),
    ClientProps0 = [{username, ?USERNAME},
                    {server, <<"localhost">>},
                    {password, Password},
                    {ssl, false}
                   ],
    {ok, _Conn, _ClientProps, _} = escalus_connection:start(ClientProps0,
                                                            [start_stream,
                                                             stream_features,
                                                             authenticate
                                                            ]),
    ok.

auth_fail(_Config) ->
    Password = generate_token(60),
    ClientProps0 = [{username, ?USERNAME},
                    {server, <<"localhost">>},
                    {password, Password},
                    {ssl, false}
                   ],
    {error, _} = escalus_connection:start(ClientProps0,
                                          [start_stream,
                                           stream_features,
                                           authenticate
                                          ]),
    ok.

generate_token(NbfDelta) ->
    Now = os:system_time(seconds),
    Data = #{bookingNumber => ?USERNAME, exp => Now + 60, nbf => Now + NbfDelta, iat => Now},
    Token = jwerl:sign(Data, #{alg => <<"HS256">>, key => ?JWT_KEY}),
    Token.
