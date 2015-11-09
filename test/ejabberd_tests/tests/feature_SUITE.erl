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

-module(feature_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, order}].

groups() ->
    [{order, [], [stream_compression_after_SASL,
                  stream_compression_before_SASL]}].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1, {by_name, [alice]}).

end_per_suite(Config0) ->
    Config1 = escalus:delete_users(Config0, {by_name, [alice]}),
    escalus:end_per_suite(Config1).

init_per_group(order, Config) ->
    Config.

end_per_group(order, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Time request test
%%--------------------------------------------------------------------

stream_compression_before_SASL(Config) ->
    Alice = escalus_users:get_options(Config, alice),
    {ok, _, _, _} = escalus_connection:start(Alice, [start_stream,
                                                     stream_features,

                                                     maybe_use_compression,

                                                     maybe_use_ssl,
                                                     authenticate,
                                                     bind,
                                                     session]).

stream_compression_after_SASL(Config) ->
    Alice = escalus_users:get_options(Config, alice),
    {ok, _, _, _} = escalus_connection:start(Alice, [start_stream,
                                                     stream_features,
                                                     maybe_use_ssl,
                                                     authenticate,

                                                     maybe_use_compression,

                                                     bind,
                                                     session]).
