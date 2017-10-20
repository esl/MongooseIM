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

-module(acc_e2e_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(NS_BLOCKING,     <<"urn:xmpp:blocking">>).

-define(SLEEP_TIME, 50).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, message}
    ].

groups() ->
    [
        {message, [], message_test_cases()}
    ].

message_test_cases() ->
    [
        one_message,
        message_altered_by_filter_local_packet_hook
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {Mod, Code} = escalus_ejabberd:rpc(dynamic_compile, from_string,
                                       [acc_test_helper_code(Config)]),
    escalus_ejabberd:rpc(code, load_binary, [Mod, "acc_test_helper.erl", Code]),
    recreate_table(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(message, Config) ->
    % saves ref, timestamp and some attrs of acc in ets
    add_handler(c2s_preprocessing_hook, test_save_acc, 50),
    % checks it is the same acc
    add_handler(filter_local_packet, test_check_acc, 50),
    % checks it is the same acc and it has been stripped but keeps persistent props
    add_handler(user_receive_packet, test_check_final_acc, 50),
    escalus:create_users(Config, escalus:get_users([alice, bob]));
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(message, Config) ->
    remove_handler(c2s_preprocessing_hook, test_save_acc, 50),
    remove_handler(filter_local_packet, test_check_acc, 50),
    remove_handler(user_receive_packet, test_check_final_acc, 50),
    escalus:delete_users(Config, escalus:get_users([alice, bob]));
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(message_altered_by_filter_local_packet_hook = CN, Config) ->
    add_handler(filter_local_packet, alter_message, 60),
    escalus:init_per_testcase(CN, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(message_altered_by_filter_local_packet_hook = CN, Config) ->
    remove_handler(filter_local_packet, alter_message, 60),
    escalus:end_per_testcase(CN, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

one_message(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            M = escalus_stanza:chat_to(Bob, <<"hej">>),
            escalus:send(Alice, M),
            R = escalus_client:wait_for_stanza(Bob),
            % hook handlers break something if test does not pass so it should fail somewhere here
            escalus:assert(is_chat_message, [<<"hej">>], R),
            ok
        end).

message_altered_by_filter_local_packet_hook(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            M = escalus_stanza:chat_to(escalus_client:short_jid(Bob), <<"hi">>),
            escalus:send(Alice, M),
            R = escalus_client:wait_for_stanza(Bob),
            % filter_local_packet alters packet to be delivered
            % and mongoose_acc:strip must respect it
            escalus:assert(is_chat_message, [<<"bye">>], R),
            ok
        end).

acc_test_helper_code(Config) ->
    Dir = proplists:get_value(data_dir, Config),
    F = filename:join(Dir, "acc_test_helper.erl"),
    {ok, Code} = file:read_file(F),
    binary_to_list(Code).

add_handler(Hook, F, Seq) ->
    escalus_ejabberd:rpc(ejabberd_hooks, add, [Hook, host(), acc_test_helper, F, Seq]).

remove_handler(Hook, F, Seq) ->
    escalus_ejabberd:rpc(ejabberd_hooks, delete, [Hook, host(), acc_test_helper, F, Seq]).

host() -> <<"localhost">>.

%% creates a temporary ets table keeping refs and some attrs of accumulators created in c2s
recreate_table() ->
    escalus_ejabberd:rpc(acc_test_helper, recreate_table, []).
