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
-module(adhoc_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, adhoc}].

groups() ->
    [{adhoc, [sequence], [ping]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Adhoc tests
%%--------------------------------------------------------------------
ping(Config) ->
    escalus:story(Config, [{alice, 1}],
                  fun(Alice) ->
                          %% Alice pings the server using adhoc command
                          escalus_client:send(Alice, escalus_stanza:to(escalus_stanza:adhoc_request(<<"ping">>),
                                                                       ct:get_config(ejabberd_domain))),

                          %% Server replies to Alice with pong
                          AdHocResp = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_adhoc_response, [<<"ping">>, <<"completed">>],
                                         AdHocResp)
                  end).
