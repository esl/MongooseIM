%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

%%
%% @doc of core xmpp documentation (https://xmpp.org/rfcs/rfc3921.html#session)
%% step 2, scenario when a resource is not allowed to create a session, we,
%% following recommendation, implement case #1:
%%
%% %% ============================================================================
%% %% If there is already an active resource of the same name, the server MUST either (1)
%% %% terminate the active resource and allow the newly-requested session, or (2) disallow
%% %% the newly-requested session and maintain the active resource. Which of these the
%% %% server does is up to the implementation, although it is RECOMMENDED to implement
%% %% case #1. In case #1, the server SHOULD send a <conflict/> stream error to the
%% %% active resource, terminate the XML stream and underlying TCP connection for the
%% %% active resource, and return a IQ stanza of type "result" (indicating success)
%% %% to the newly-requested session.
%% %% ============================================================================
%%

-module(resource_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SLEEP_TIME, 50).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, conflict}
    ].

groups() ->
    [{conflict, [sequence], conflict_test_cases()}
    ].

conflict_test_cases() ->
    [aaa
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    [{escalus_no_stanzas_after_story, true} |
     escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

same_resource_conflict(Config) ->
    EUs = proplists:get_value(escalus_users, Config),
    UserSpec = proplists:get_value(alice, EUs),
    Resource = <<"res1">>,
    {ok, Client} = escalus_client:start(Config, UserSpec, Resource),
    {ok, _Client2} = escalus_client:start(Config, UserSpec, Resource),
    E = escalus:wait_for_stanza(Client),
    escalus:assert(is_stream_error, [<<"conflict">>, <<"Replaced by new connection">>], E).

