-module(smart_markers_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include("muc_light.hrl").
-define(NS_ESL_SMART_MARKERS, <<"esl:xmpp:smart-markers:0">>).
-define(NS_STANZAID, <<"urn:xmpp:sid:0">>).

-import(distributed_helper, [mim/0, rpc/4, subhost_pattern/1]).
-import(domain_helper, [host_type/0]).
-import(config_parser_helper, [mod_config/2]).

%%% Suite configuration
all() ->
    case (not ct_helper:is_ct_running())
         orelse mongoose_helper:is_rdbms_enabled(host_type()) of
        true -> all_cases();
        false -> {skip, require_rdbms}
    end.

all_cases() ->
    [
     {group, regular},
     {group, async_pools}
    ].

groups() ->
    inbox_helper:maybe_run_in_parallel(groups1()) ++ groups2().

groups1() ->
    [
     {one2one, [],
      [
       error_set_iq,
       error_bad_peer,
       error_no_peer_given,
       error_bad_timestamp,
       marker_is_stored,
       marker_can_be_fetched,
       marker_for_thread_can_be_fetched,
       marker_after_timestamp_can_be_fetched,
       marker_after_timestamp_for_threadid_can_be_fetched,
       remove_markers_when_removed_user,
       repeated_markers_produce_no_warnings
      ]},
     {muclight, [],
      [
       marker_is_stored_for_room,
       marker_can_be_fetched_for_room,
       marker_is_removed_when_user_leaves_room,
       markers_are_removed_when_room_is_removed
      ]},
     {keep_private, [],
      [
       marker_is_not_routed_nor_fetchable,
       fetching_room_answers_only_own_marker
      ]}
    ].

groups2() ->
    [
     {regular, [],
      [
       {group, one2one},
       {group, muclight},
       {group, keep_private}
      ]},
     {async_pools, [],
      [
       {group, one2one},
       {group, muclight},
       {group, keep_private}
      ]}
    ].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(regular, Config) ->
    [{merge_opts, #{backend => rdbms}} | Config];
init_per_group(async_pools, Config) ->
    [{merge_opts, #{backend => rdbms_async,
                    async_writer => #{pool_size => 2}}} | Config];
init_per_group(GroupName, Config) ->
    AsyncType = ?config(merge_opts, Config),
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    ok = dynamic_modules:ensure_modules(HostType, group_to_module(GroupName, AsyncType)),
    Config1.

group_to_module(one2one, MergeOpts) ->
    [{mod_smart_markers, mod_config(mod_smart_markers, MergeOpts)}];
group_to_module(keep_private, MergeOpts) ->
    [{mod_smart_markers, mod_config(mod_smart_markers, MergeOpts#{keep_private => true})},
     {mod_muc_light, mod_config(mod_muc_light, #{backend => rdbms})}];
group_to_module(muclight, MergeOpts) ->
    [{mod_smart_markers, mod_config(mod_smart_markers, MergeOpts)},
     {mod_muc_light, mod_config(mod_muc_light, #{backend => rdbms})}].

end_per_group(muclight, Config) ->
    muc_light_helper:clear_db(host_type()),
    end_per_group(generic, Config);
end_per_group(_, Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    Config.

init_per_testcase(repeated_markers_produce_no_warnings = TC, Config) ->
    logger_ct_backend:start(),
    escalus:init_per_testcase(TC, Config);
init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(repeated_markers_produce_no_warnings = TC, Config) ->
    logger_ct_backend:stop(),
    escalus:end_per_testcase(TC, Config);
end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

%%% tests
error_set_iq(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Query = escalus_stanza:query_el(?NS_ESL_SMART_MARKERS, []),
        Iq = escalus_stanza:iq(<<"set">>, [Query]),
        escalus:send(Alice, Iq),
        Response = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Response)
    end).

error_bad_peer(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Iq = iq_fetch_marker([{<<"peer">>, <<"/@">>}]),
        escalus:send(Alice, Iq),
        Response = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Response)
    end).

error_no_peer_given(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Iq = iq_fetch_marker([]),
        escalus:send(Alice, Iq),
        Response = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Response)
    end).

error_bad_timestamp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        PeerJid = <<"peer@localhost">>,
        Iq = iq_fetch_marker([{<<"peer">>, PeerJid}, {<<"after">>, <<"baddate">>}]),
        escalus:send(Alice, Iq),
        Response = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, [Iq], Response)
    end).

marker_is_stored(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        send_message_respond_marker(Alice, Bob),
        AliceJid = jid:from_binary(escalus_client:full_jid(Alice)),
        BobJid = jid:from_binary(escalus_client:full_jid(Bob)),
        mongoose_helper:wait_until(
          fun() -> length(fetch_markers_for_users(BobJid, AliceJid)) > 0 end, true)
    end).

marker_can_be_fetched(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        send_message_respond_marker(Alice, Bob),
        send_message_respond_marker(Bob, Alice),
        verify_marker_fetch(Bob, Alice),
        verify_marker_fetch(Alice, Bob)
    end).

marker_is_not_routed_nor_fetchable(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        MsgId = escalus_stanza:id(),
        Msg = escalus_stanza:set_id(escalus_stanza:chat_to(Bob, <<"Hello!">>), MsgId),
        escalus:send(Alice, Msg),
        escalus:wait_for_stanza(Bob),
        ChatMarker = escalus_stanza:chat_marker(Alice, <<"displayed">>, MsgId),
        escalus:send(Bob, ChatMarker),
        escalus_assert:has_no_stanzas(Alice), %% Marker is filtered, Alice won't receive the marker
        verify_marker_fetch_is_empty(Alice, Bob), %% Alice won't see Bob's marker
        verify_marker_fetch(Bob, Alice) %% Bob will see his own marker
    end).

fetching_room_answers_only_own_marker(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Users = [Alice, Bob, Kate],
        RoomId = create_room(Alice, [Bob, Kate], Config),
        RoomBinJid = muc_light_helper:room_bin_jid(RoomId),
        send_msg_to_room(Users, RoomBinJid, Alice, escalus_stanza:id()),
        send_msg_to_room(Users, RoomBinJid, Bob, escalus_stanza:id()),
        MsgId = send_msg_to_room(Users, RoomBinJid, Kate, escalus_stanza:id()),
        ChatMarker = escalus_stanza:setattr(
                       escalus_stanza:chat_marker(RoomBinJid, <<"displayed">>, MsgId),
                       <<"type">>, <<"groupchat">>),
        [ begin
              escalus:send(User, ChatMarker),
              {ok, MarkersThatUserHasInRoom} = verify_marker_fetch(User, RoomBinJid),
              ?assertEqual(1, length(MarkersThatUserHasInRoom))
          end || User <- [Alice, Bob] ]
    end).

marker_for_thread_can_be_fetched(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        ThreadId = <<"some-thread-id">>,
        send_message_respond_marker(Alice, Bob),
        send_message_respond_marker(Alice, Bob, ThreadId),
        verify_marker_fetch(Bob, Alice, ThreadId, undefined)
    end).

marker_after_timestamp_can_be_fetched(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        TS = rpc(mim(), erlang, system_time, [microsecond]),
        BinTS = list_to_binary(calendar:system_time_to_rfc3339(TS, [{offset, "Z"}, {unit, microsecond}])),
        send_message_respond_marker(Alice, Bob),
        send_message_respond_marker(Alice, Bob),
        verify_marker_fetch(Bob, Alice, undefined, BinTS)
    end).

marker_after_timestamp_for_threadid_can_be_fetched(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        ThreadId = <<"some-thread-id">>,
        TS = rpc(mim(), erlang, system_time, [microsecond]),
        BinTS = list_to_binary(calendar:system_time_to_rfc3339(TS, [{offset, "Z"}, {unit, microsecond}])),
        send_message_respond_marker(Alice, Bob),
        send_message_respond_marker(Alice, Bob, ThreadId),
        verify_marker_fetch(Bob, Alice, ThreadId, BinTS)
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

repeated_markers_produce_no_warnings(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        MsgId = escalus_stanza:id(),
        Msg = escalus_stanza:set_id(escalus_stanza:chat_to(Bob, <<"Hello!">>), MsgId),
        escalus:send(Alice, Msg),
        escalus:wait_for_stanza(Bob),
        ChatMarker = escalus_stanza:chat_marker(Alice, <<"displayed">>, MsgId),
        [MarkerEl] = ChatMarker#xmlel.children,
        RepeatedChatMarker = ChatMarker#xmlel{children =  [MarkerEl, MarkerEl]},
        logger_ct_backend:capture(warning),
        escalus:send(Bob, RepeatedChatMarker),
        escalus:wait_for_stanza(Alice),
        logger_ct_backend:stop_capture(),
        FilterFun = fun(_, WMsg) -> re:run(WMsg, "{bad_result,{updated,0}}") /= nomatch end,
        [] = logger_ct_backend:recv(FilterFun)
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

marker_can_be_fetched_for_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                        fun(Alice, Bob, Kate) ->
        Users = [Alice, Bob, Kate],
        RoomId = create_room(Alice, [Bob, Kate], Config),
        RoomBinJid = muc_light_helper:room_bin_jid(RoomId),
        one_marker_in_room(Users, RoomBinJid, Alice, Bob),
        verify_marker_fetch(Bob, RoomBinJid)
    end).

marker_is_removed_when_user_leaves_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun(Alice, Bob) ->
        Users = [Alice, Bob],
        RoomId = create_room(Alice, [Bob], Config),
        RoomBinJid = muc_light_helper:room_bin_jid(RoomId),
        RoomJid = jid:from_binary(RoomBinJid),
        one_marker_in_room(Users, RoomBinJid, Alice, Bob),
        BobJid = jid:from_binary(escalus_client:full_jid(Bob)),
        mongoose_helper:wait_until(
          fun() -> length(fetch_markers_for_users(BobJid, RoomJid)) > 0 end, true),
        % Remove Bob from the room
        muc_light_helper:user_leave(RoomId, Bob, [Alice]),
        mongoose_helper:wait_until(
          fun() -> length(fetch_markers_for_users(BobJid, RoomJid)) end, 0)
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
          end || User <- Users ]
    end).

%%% helpers
fetch_markers_for_users(From, To) ->
    MRs = rpc(mim(), mod_smart_markers_backend, get_chat_markers,
              [host_type(), To, undefined, 0]),
    [MR || #{from := FR} = MR <- MRs, jid:are_bare_equal(From, FR)].

iq_fetch_marker(Attrs) ->
    Query = escalus_stanza:query_el(?NS_ESL_SMART_MARKERS, Attrs, []),
    escalus_stanza:iq(<<"get">>, [Query]).

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
    StanzaId = send_msg_to_room(Users, RoomBinJid, Writer, MsgId),
    mark_msg(Users, RoomBinJid, Marker, StanzaId).

send_msg_to_room(Users, RoomBinJid, Writer, MsgId) ->
    Msg = escalus_stanza:set_id(escalus_stanza:groupchat_to(RoomBinJid, <<"Hello">>), MsgId),
    escalus:send(Writer, Msg),
    Msgs = [ escalus:wait_for_stanza(User) || User <- Users ],
    get_id(hd(Msgs), MsgId).

mark_msg(Users, RoomBinJid, Marker, StanzaId) ->
    ChatMarker = escalus_stanza:setattr(
                   escalus_stanza:chat_marker(RoomBinJid, <<"displayed">>, StanzaId),
                   <<"type">>, <<"groupchat">>),
    escalus:send(Marker, ChatMarker),
    [ escalus:wait_for_stanza(User) || User <- Users ],
    StanzaId.

send_message_respond_marker(MsgWriter, MarkerAnswerer) ->
    send_message_respond_marker(MsgWriter, MarkerAnswerer, undefined).

send_message_respond_marker(MsgWriter, MarkerAnswerer, MaybeThread) ->
    MsgId = escalus_stanza:id(),
    Msg = add_thread_id(escalus_stanza:set_id(
                          escalus_stanza:chat_to(MarkerAnswerer, <<"Hello!">>),
                          MsgId),
                        MaybeThread),
    escalus:send(MsgWriter, Msg),
    escalus:wait_for_stanza(MarkerAnswerer),
    ChatMarker = add_thread_id(escalus_stanza:chat_marker(
                                 MsgWriter, <<"displayed">>, MsgId),
                               MaybeThread),
    escalus:send(MarkerAnswerer, ChatMarker),
    escalus:wait_for_stanza(MsgWriter).

verify_marker_fetch(MarkingUser, MarkedUser) ->
    verify_marker_fetch(MarkingUser, MarkedUser, undefined, undefined).

verify_marker_fetch(MarkingUser, MarkedUser, Thread, After) ->
        MarkedUserBJid = case is_binary(MarkedUser) of
                             true -> [{<<"peer">>, MarkedUser}];
                             false -> [{<<"peer">>, escalus_utils:jid_to_lower(escalus_client:short_jid(MarkedUser))}]
                         end,
        MaybeThread = case Thread of
                          undefined -> [];
                          _ -> [{<<"thread">>, Thread}]
                      end,
        MaybeAfter = case After of
                          undefined -> [];
                          _ -> [{<<"after">>, After}]
                      end,
        Iq = iq_fetch_marker(MarkedUserBJid ++ MaybeThread ++ MaybeAfter),
        %% using wait_until to ensure that assertions are eventually passing
        mongoose_helper:wait_until(
          fun() ->
                  escalus:send(MarkingUser, Iq),
                  Response = escalus:wait_for_stanza(MarkingUser),
                  escalus:assert(is_iq_result, [Iq], Response),
                  Markers = [Marker | _] = exml_query:paths(
                                             Response, [{element_with_ns, <<"query">>, ?NS_ESL_SMART_MARKERS},
                                                        {element, <<"marker">>}]),
                  ?assertNotEqual(undefined, Marker),
                  ?assertNotEqual(undefined, exml_query:attr(Marker, <<"timestamp">>)),
                  ?assertEqual(<<"displayed">>, exml_query:attr(Marker, <<"type">>)),
                  ?assertEqual(Thread, exml_query:attr(Marker, <<"thread">>)),
                  ?assertNotEqual(undefined, exml_query:attr(Marker, <<"id">>)),
                  lists:sort(Markers)
          end,
          fun(_) -> true end, %% always positively validate the return value.
          #{name => fetch_marker}).

verify_marker_fetch_is_empty(MarkingUser, MarkedUser) ->
        MarkedUserBJid = escalus_utils:jid_to_lower(escalus_client:short_jid(MarkedUser)),
        Iq = iq_fetch_marker([{<<"peer">>, MarkedUserBJid}]),
        escalus:send(MarkingUser, Iq),
        Response = escalus:wait_for_stanza(MarkingUser),
        escalus:assert(is_iq_result, [Iq], Response),
        Markers = exml_query:paths(Response, [{element_with_ns, <<"query">>, ?NS_ESL_SMART_MARKERS},
                                              {element, <<"marker">>}]),
        ?assertEqual([], Markers).

get_id(Packet, Def) ->
    exml_query:path(
      Packet, [{element_with_ns, <<"stanza-id">>, ?NS_STANZAID}, {attr, <<"id">>}], Def).

add_thread_id(Msg, undefined) ->
    Msg;
add_thread_id(#xmlel{children = Children} = Msg, ThreadID) ->
    ThreadEl = #xmlel{name = <<"thread">>,
                      children = [#xmlcdata{content = ThreadID}]},
    Msg#xmlel{children = [ThreadEl | Children]}.

unregister_user(Client) ->
    Jid = jid:from_binary(escalus_client:short_jid(Client)),
    rpc(mim(), ejabberd_auth, remove_user, [Jid]).
