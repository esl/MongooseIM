%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
-module(cassandra_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, cassandra_group}].

groups() ->
     [{cassandra_group, [sequence], test_cases()}].

test_cases() -> [check_connection].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Last tests
%%--------------------------------------------------------------------
check_connection(Config) ->
    case gen_tcp:connect("127.0.0.1", 9042, []) of
        {ok, _} ->
            ct:pal("Cassandra is listening on port 9042", []),
            check_connection2(Config);
        _ ->
            ct:pal("Cassandra is not listening on port 9042, skip the test", []),
            ok
    end.

check_connection2(Config) ->
    ejabberd_helper:start_ejabberd_with_config(Config, "ejabberd.cfg"),
    try
        check_connection3(Config)
    after
        ejabberd_helper:stop_ejabberd()
    end.

check_connection3(Config) ->
    %% Wait for cassandra workers to connect
    wait_for_cassandra(20),
    case mongoose_cassandra:status() of
        disabled ->
            %% Mongoose thinks that Cassandra is not configured but it is
            ct:comment("Cassandra is not configured");
        ok ->
            ct:comment("Cassandra is connected");
        failure ->
            ct:fail("Cassandra is configured but not connected", [])
    end.

wait_for_cassandra(0) -> ok;
wait_for_cassandra(N) when N > 0 ->
    case mongoose_cassandra:status() of
        failure -> timer:sleep(100), wait_for_cassandra(N-1);
        Other -> Other
    end.
