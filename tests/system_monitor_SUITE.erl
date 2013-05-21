%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
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

-module(system_monitor_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     test_notification,
     test_kill
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {Mod, Bin, File} = code:get_object_code(?MODULE),
    rpc(code, load_binary, [Mod, File, Bin]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:create_users(Config),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:delete_users(Config),
    escalus:end_per_testcase(CaseName, Config),
    reset_watchdog_admins(),
    stop_alarms().

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-define(WATCHDOG_JID, <<"localhost/watchdog">>).

%% Trigger a fake large_heap for the mnesia_tm process
%% without using the alarms application.
%% Alice - the watchdog admin - should receive an xmpp  notification.
test_notification(Config) ->
    escalus:story(Config, [1], fun test_notification_story/1).

test_notification_story(Alice) ->
    add_watchdog_admin(Alice),

    Pid = rpc(erlang, whereis, [mnesia_tm]),
    ok = rpc(alarm_handler, set_alarm, [{large_heap, {Pid, dummy_info}}]),
    PidStr = rpc(erlang, pid_to_list, [Pid]),

    Stanza = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_chat_message, Stanza),
    Msg = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    ExpectedMsg = io_lib:format("(ejabberd@localhost) The process ~s"
                                " is consuming too much memory:\ndummy_info",
                                [PidStr]),
    [{0, _}] = binary:matches(Msg, iolist_to_binary(ExpectedMsg)),
    true.

%% Spawns a process that causes large_heap alarm.
%% Alice - the watchdog admin - kills the process using xmpp.
test_kill(Config) ->
    escalus:story(Config, [1], fun test_kill_story/1).

test_kill_story(Alice) ->
    start_alarms([], 100000),
    add_watchdog_admin(Alice),

    Pid = rpc(system_monitor_SUITE, use_memory, [150000, 100000]),
    PidStr = rpc(erlang, pid_to_list, [Pid]),
    Stanza = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_stanza_from, [?WATCHDOG_JID], Stanza),
    test_kill_consume_large_heap_message(PidStr, Stanza),
    {status, _} = rpc(erlang, process_info, [Pid, status]),

    % Alice decides to kill the process
    Cmd = iolist_to_binary(["kill ejabberd@localhost ", PidStr]),
    escalus:send(Alice, escalus_stanza:chat_to(?WATCHDOG_JID, Cmd)),
    test_kill_wait_for_ok(Alice, Pid, PidStr).

test_kill_wait_for_ok(Alice, Pid, PidStr) ->
    Stanza = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_stanza_from, [?WATCHDOG_JID], Stanza),
    case escalus_pred:is_chat_message(<<"ok">>, Stanza) of
        true ->
            undefined = rpc(erlang, process_info, [Pid, status]),
            true;
        false ->
            % There might be more large heap messages
            test_kill_consume_large_heap_message(PidStr, Stanza),
            test_kill_wait_for_ok(Alice, Pid, PidStr)
    end.

test_kill_consume_large_heap_message(PidStr, Stanza) ->
    escalus:assert(is_chat_message, Stanza),
    Msg = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    ct:log("consume ~p~n",[Stanza]),
    ExpectedMsg = io_lib:format("(ejabberd@localhost) The process ~s"
                                " is consuming too much memory:",
                                [PidStr]),
    [{0, _}] = binary:matches(Msg, iolist_to_binary(ExpectedMsg)).

%%--------------------------------------------------------------------
%% Internals
%%--------------------------------------------------------------------

add_watchdog_admin(Client) ->
    Admins = case rpc(ejabberd_config, get_local_option, [watchdog_admins]) of
                 undefined             -> [];
                 Adm when is_list(Adm) -> Adm
             end,
    Jid = escalus_client:short_jid(Client),
    {atomic, ok} = rpc(ejabberd_config, add_local_option,
                       [watchdog_admins, [binary_to_list(Jid)|Admins]]).

reset_watchdog_admins() ->
    {atomic, ok} = rpc(ejabberd_config, add_local_option,
                       [watchdog_admins, undefined]).

set_alarm(Alarm) ->
    ok = rpc(alarm_handler, set_alarm, [Alarm]).

start_alarms(Handlers, LargeHeap) ->
    ok = rpc(application, set_env, [alarms, handlers, Handlers]),
    ok = rpc(application, set_env, [alarms, large_heap, LargeHeap]),
    ok = rpc(alarms, start, []).

stop_alarms() ->
    rpc(alarms, stop, []).

use_memory(HowMuch, HowLong) ->
    spawn(fun() ->
                  L = lists:duplicate(HowMuch div 2, 1),
                  timer:sleep(HowLong),
                  L
          end).

rpc(M, F, A) ->
    Node = ct:get_config(ejabberd_node),
    rpc:call(Node, M, F, A).
