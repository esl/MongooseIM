-module(smart_markers_SUITE).
-compile([export_all, nowarn_export_all]).

-include("muc_light.hrl").

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
     {group, one2one},
     {group, muclight}
    ].

groups() ->
    [
     {one2one, [],
      [
       marker_is_stored,
       remove_markers_when_removed_user
      ]},
     {muclight, [],
      [
       marker_is_stored_for_room,
       markers_are_removed_when_room_is_removed
      ]}
    ].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    ok = dynamic_modules:ensure_modules(host_type(), group_to_module(GroupName)),
    Config.

group_to_module(one2one) ->
    [{mod_smart_markers, [{backend, rdbms}]}];
group_to_module(muclight) ->
    [{mod_smart_markers, [{backend, rdbms}]},
     {mod_muc_light,
      [{host, subhost_pattern(muc_light_helper:muc_host_pattern())},
       {backend, rdbms}]}].

end_per_group(muclight, Config) ->
    muc_light_helper:clear_db(host_type()),
    end_per_group(generic, Config);
end_per_group(_, Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
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

marker_is_stored_for_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                        fun(Alice, Bob, Kate) ->
        Users = [Alice, Bob, Kate],
        RoomId = create_room(Alice, [Bob, Kate], Config),
        RoomBinJid = muc_light_helper:room_bin_jid(RoomId),
        one_marker_in_room(Users, RoomBinJid, Alice, Bob),
        BobJid = jid:from_binary(escalus_client:full_jid(Bob)),
        mongoose_helper:wait_until(
          fun() -> length(fetch_markers_for_users(BobJid, jid:from_binary(RoomBinJid))) > 0 end, true)
    end).

markers_are_removed_when_room_is_removed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Users = [Alice, Bob],
        RoomId = create_room(Alice, [Bob], Config),
        RoomBinJid = muc_light_helper:room_bin_jid(RoomId),
        RoomJid = jid:from_binary(RoomBinJid),
        one_marker_in_room(Users, RoomBinJid, Alice, Bob),
        BobJid = jid:from_binary(escalus_client:full_jid(Bob)),
        mongoose_helper:wait_until(
          fun() -> length(fetch_markers_for_users(BobJid, RoomJid)) > 0 end, true),
        %% The room is then deleted
        delete_room(Alice, Users, RoomBinJid),
        [ begin
              Jid = jid:from_binary(escalus_client:full_jid(User)),
              mongoose_helper:wait_until(
                fun() -> length(fetch_markers_for_users(Jid, RoomJid)) end, 0)
          end || User <- Users]
    end).

%%% helpers
fetch_markers_for_users(From, To) ->
    MRs = rpc(mim(), mod_smart_markers_backend, get_chat_markers,
              [host_type(), To, undefined, 0]),
    [MR || #{from := FR} = MR <- MRs, jid:are_bare_equal(From, FR)].

create_room(Owner, Members, Config) ->
    RoomId = muc_helper:fresh_room_name(),
    MucHost = muc_light_helper:muc_host(),
    muc_light_helper:create_room(RoomId, MucHost, Owner, Members, Config, muc_light_helper:ver(1)),
    RoomId.

delete_room(Owner, Users, RoomBinJid) ->
    Destroy = escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_DESTROY, []), RoomBinJid),
    escalus:send(Owner, Destroy),
    AffUsersChanges = [{User, none} || User <- Users ],
    muc_light_helper:verify_aff_bcast([], AffUsersChanges, [?NS_MUC_LIGHT_DESTROY]),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Owner)).

one_marker_in_room(Users, RoomBinJid, Writer, Marker) ->
        MsgId = escalus_stanza:id(),
        Msg = escalus_stanza:set_id(
                escalus_stanza:groupchat_to(RoomBinJid, <<"Hello">>), MsgId),
        escalus:send(Writer, Msg),
        [ escalus:wait_for_stanza(User) || User <- Users],
        ChatMarker = escalus_stanza:setattr(
                       escalus_stanza:chat_marker(RoomBinJid, <<"displayed">>, MsgId),
                       <<"type">>, <<"groupchat">>),
        escalus:send(Marker, ChatMarker),
        [ escalus:wait_for_stanza(User) || User <- Users],
        MsgId.

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
