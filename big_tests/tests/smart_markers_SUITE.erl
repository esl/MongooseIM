-module(smart_markers_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4, subhost_pattern/1]).
-import(domain_helper, [host_type/0]).

%%% Suite configuration
all() ->
    case (not ct_helper:is_ct_running())
          orelse mongoose_helper:is_rdbms_enabled(host_type()) of
        true -> all_cases();
        false -> {skip, require_rdbms}
    end.

all_cases() ->
    [
     {group, one2one}
    ].

groups() ->
    [
     {one2one, [],
      [
       marker_is_stored,
       remove_markers_when_removed_user
      ]}
    ].

suite() ->
    escalus:suite().

init_per_suite(Config0) ->
    HostType = host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, [{mod_smart_markers, [{backend, rdbms}]}]),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).
end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

%%% tests
marker_is_stored(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        send_message_respond_marker(Alice, Bob),
        AliceJid = jid:from_binary(escalus_client:full_jid(Alice)),
        BobJid = jid:from_binary(escalus_client:full_jid(Bob)),
        mongoose_helper:wait_until(
          fun() -> length(fetch_markers_for_users(BobJid, AliceJid)) > 0 end, true)
    end).

remove_markers_when_removed_user(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Body = <<"Hello Bob!">>,
        MsgId = escalus_stanza:id(),
        Msg = escalus_stanza:set_id(escalus_stanza:chat_to(Bob, Body), MsgId),
        escalus:send(Alice, Msg),
        escalus:wait_for_stanza(Bob),
        ChatMarker = escalus_stanza:chat_marker(Alice, <<"displayed">>, MsgId),
        escalus:send(Bob, ChatMarker),
        escalus:wait_for_stanza(Alice),
        AliceJid = jid:from_binary(escalus_client:full_jid(Alice)),
        BobJid = jid:from_binary(escalus_client:full_jid(Bob)),
        mongoose_helper:wait_until(fun() -> length(fetch_markers_for_users(BobJid, AliceJid)) > 0 end, true),
        unregister_user(Bob),
        mongoose_helper:wait_until(fun() -> length(fetch_markers_for_users(BobJid, AliceJid)) end, 0)
    end).

%%% helpers
fetch_markers_for_users(From, To) ->
    MRs = rpc(mim(), mod_smart_markers_backend, get_chat_markers,
              [host_type(), To, undefined, 0]),
    [MR || #{from := FR} = MR <- MRs, jid:are_bare_equal(From, FR)].

send_message_respond_marker(MsgWriter, MarkerAnswerer) ->
    Body = <<"Hello">>,
    MsgId = escalus_stanza:id(),
    Msg = escalus_stanza:set_id(escalus_stanza:chat_to(MarkerAnswerer, Body), MsgId),
    escalus:send(MsgWriter, Msg),
    escalus:wait_for_stanza(MarkerAnswerer),
    ChatMarker = escalus_stanza:chat_marker(MsgWriter, <<"displayed">>, MsgId),
    escalus:send(MarkerAnswerer, ChatMarker),
    escalus:wait_for_stanza(MsgWriter).

unregister_user(Client) ->
    Jid = jid:from_binary(escalus_client:short_jid(Client)),
    rpc(mim(), ejabberd_auth, remove_user, [Jid]).
