-module(tr_util_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [rpc/4, mim/0]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [c2s_hooks, c2s_elements].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    rpc(mim(), tr, start, []),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config),
    rpc(mim(), tr, stop, []).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(_CaseName, _Config) ->
    rpc(mim(), tr, stop_tracing, []),
    rpc(mim(), tr, clean, []).

%% Test Cases

c2s_hooks(Config) ->
    rpc(mim(), tr, trace, [[mongoose_c2s_hooks, gen_hook]]),
    [] = rpc(mim(), tr_util, c2s_hooks, []), % nothing collected yet
    escalus:fresh_story(Config, [{alice, 1}], fun c2s_hooks_story/1).

c2s_hooks_story(Alice) ->
    C2SHooks = rpc(mim(), tr_util, c2s_hooks, []),
    AliceJid = escalus_utils:get_jid(Alice),

    %% Get c2s hooks, and check the first few
    ?assertMatch([{AliceJid, user_send_packet, #{mongoose_acc := true}},
                  {AliceJid, user_send_iq, #{mongoose_acc := true}},
                  {AliceJid, user_open_session, #{mongoose_acc := true}} | _], C2SHooks),

    %% Get generic hook statistics, and check one hook
    HookStat = rpc(mim(), tr, call_stat, [fun tr_util:tr_to_hook_name_and_tag/1]),
    HT = domain_helper:host_type(),
    ?assertMatch(#{{user_open_session, HT} := {1, _, _}}, HookStat).

c2s_elements(Config) ->
    rpc(mim(), tr, trace, [[mongoose_c2s_hooks]]),
    [] = rpc(mim(), tr_util, c2s_elements, []), % nothing collected yet
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun c2s_elements_story/2).

c2s_elements_story(Alice, Bob) ->
    escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hello">>)),
    escalus:wait_for_stanza(Bob),
    AliceBareJid = escalus_utils:get_short_jid(Alice),
    BobBareJid = escalus_utils:get_short_jid(Bob),
    AliceJid = escalus_utils:get_jid(Alice),
    BobJid = escalus_utils:get_jid(Bob),

    %% Get elements exchanged between bare JIDs
    %% There can be more than 2 elements if the hook traces are interleaved
    %% between the sender and the receiver
    [Sent, Recv | _] = Events =
        rpc(mim(), tr_util, c2s_elements_between_jids, [[AliceBareJid, BobBareJid]]),
    ?assertMatch(#{name := <<"message">>, type := <<"chat">>,
                   jid := AliceJid, from_jid := AliceJid, to_jid := BobJid}, Sent),
    ?assertMatch(#{name := <<"message">>, type := <<"chat">>,
                   jid := BobJid, from_jid := AliceJid, to_jid := BobJid}, Recv),

    %% Get elements exchanged between full JIDs
    ?assertEqual(Events, rpc(mim(), tr_util, c2s_elements_between_jids, [[AliceJid, BobJid]])),

    %% Get all elements
    AllElements = rpc(mim(), tr_util, c2s_elements, []),
    ?assert(lists:member(Sent, AllElements)),
    ?assert(lists:member(Recv, AllElements)).
