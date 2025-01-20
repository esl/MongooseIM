-module(push_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(muc_light_helper,
    [
        room_bin_jid/1,
        create_room/6
    ]).
-import(escalus_ejabberd, [rpc/3]).
-import(distributed_helper, [subhost_pattern/1]).
-import(domain_helper, [host_type/0]).
-import(config_parser_helper, [mod_config/2, config/2]).
-import(push_helper, [
    enable_stanza/2, enable_stanza/3, enable_stanza/4,
    disable_stanza/1, disable_stanza/2, become_unavailable/1
]).

-define(RPC_SPEC, distributed_helper:mim()).
-define(SESSION_KEY, publish_service).

-define(VIRTUAL_PUBSUB_DOMAIN, <<"virtual.domain">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, toggling},
        {group, pubsub_ful},
        {group, pubsub_less}
    ].

groups() ->
    [
         {toggling, [parallel], [
                                 enable_should_fail_with_missing_attributes,
                                 enable_should_fail_with_invalid_attributes,
                                 enable_should_succeed_without_form,
                                 enable_with_form_should_fail_with_incorrect_from,
                                 enable_should_accept_correct_from,
                                 disable_should_fail_with_missing_attributes,
                                 disable_should_fail_with_invalid_attributes,
                                 disable_all,
                                 disable_node,
                                 disable_node_enabled_in_session_removes_it_from_session_info,
                                 disable_all_nodes_removes_it_from_all_user_session_infos,
                                 disable_node_enabled_in_other_session_leaves_current_info_unchanged
                                ]},
         {pubsub_ful, [], notification_groups()},
         {pubsub_less, [], notification_groups()},
         {pm_msg_notifications, [parallel], [
                                             pm_no_msg_notifications_if_not_enabled,
                                             pm_no_msg_notifications_if_user_online,
                                             pm_msg_notify_if_user_offline,
                                             pm_msg_notify_if_user_offline_with_publish_options,
                                             pm_msg_notify_stops_after_disabling,
                                             pm_msg_notify_stops_after_removal
                                            ]},
         {muclight_msg_notifications, [parallel], [
                                                   muclight_no_msg_notifications_if_not_enabled,
                                                   muclight_no_msg_notifications_if_user_online,
                                                   muclight_msg_notify_if_user_offline,
                                                   muclight_msg_notify_if_user_offline_with_publish_options,
                                                   muclight_msg_notify_stops_after_disabling
                                                  ]}
    ].

notification_groups() ->
    [
     {group, pm_msg_notifications},
     {group, muclight_msg_notifications}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

%% --------------------- Callbacks ------------------------

init_per_suite(Config) ->
    %% For mocking with unnamed functions
    mongoose_helper:inject_module(?MODULE),
    escalus:init_per_suite(Config).
end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(pubsub_ful, Config) ->
    [{pubsub_host, real} | Config];
init_per_group(pubsub_less, Config) ->
    [{pubsub_host, virtual} | Config];
init_per_group(muclight_msg_notifications = GroupName, Config0) ->
    HostType = host_type(),
    Config = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, required_modules(GroupName)),
    muc_light_helper:clear_db(HostType),
    Config;
init_per_group(GroupName, Config0) ->
    HostType = host_type(),
    Config = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, required_modules(GroupName)),
    Config.

end_per_group(ComplexGroup, Config) when ComplexGroup == pubsub_ful;
                                         ComplexGroup == pubsub_less ->
    Config;
end_per_group(_, Config) ->
    dynamic_modules:restore_modules(Config).

init_per_testcase(CaseName, Config0) ->
    Config1 = escalus_fresh:create_users(Config0, [{bob, 1}, {alice, 1}, {kate, 1}]),
    Config2 = add_pubsub_jid([{case_name, CaseName} | Config1]),

    Config = case ?config(pubsub_host, Config0) of
                 virtual ->
                     start_hook_listener(Config2);
                 _ ->
                     start_route_listener(Config2)
             end,

    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    case ?config(pubsub_host, Config) of
        virtual ->
            stop_hook_listener(Config);
        _ ->
            stop_route_listener(Config)
    end,
    escalus:end_per_testcase(CaseName, Config).

%% --------------------- Helpers ------------------------

required_modules(muclight_msg_notifications) ->
    [pusher_module(), muc_light_module()];
required_modules(_) ->
    [pusher_module()].

pusher_module() ->
    PushOpts = #{backend => mongoose_helper:mnesia_or_rdbms_backend(),
                 virtual_pubsub_hosts => [subhost_pattern(?VIRTUAL_PUBSUB_DOMAIN)]},
    {mod_event_pusher, #{push => config([modules, mod_event_pusher, push], PushOpts)}}.

muc_light_module() ->
    {mod_muc_light,
     mod_config(mod_muc_light, #{backend => mongoose_helper:mnesia_or_rdbms_backend(),
                                 rooms_in_rosters => true})}.

%%--------------------------------------------------------------------
%% GROUP toggling
%%--------------------------------------------------------------------

enable_should_fail_with_missing_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            BobJID = escalus_utils:get_jid(Bob),

            escalus:send(Bob, escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>}])),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            CorrectAttrs = [{<<"xmlns">>, <<"urn:xmpp:push:0">>},
                            {<<"jid">>, BobJID},
                            {<<"node">>, <<"NodeKey">>}],

            %% Sending only one attribute should fail
            lists:foreach(
                fun({K, V}) ->
                    escalus:send(Bob, escalus_stanza:iq(<<"set">>,
                                                        [#xmlel{name = <<"enable">>,
                                                                attrs = #{K => V}}])),
                    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                                   escalus:wait_for_stanza(Bob))
                end, CorrectAttrs),

            %% Sending all but one attribute should fail
            lists:foreach(
                fun(Attr) ->
                    Attrs = #{K => V || {K, V} <- CorrectAttrs -- [Attr]},
                    escalus:send(Bob, escalus_stanza:iq(<<"set">>,
                                                        [#xmlel{name = <<"enable">>,
                                                                attrs = Attrs}])),
                    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                                   escalus:wait_for_stanza(Bob))
                end, CorrectAttrs),

            ok
        end).

enable_should_fail_with_invalid_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            %% Empty JID
            escalus:send(Bob, enable_stanza(<<>>, <<"nodeId">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            %% Empty node
            escalus:send(Bob, enable_stanza(PubsubJID, <<>>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            %% Missing value
            escalus:send(Bob, enable_stanza(PubsubJID, <<"nodeId">>,
                                            [{<<"secret1">>, undefined}])),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).


enable_should_succeed_without_form(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

enable_with_form_should_fail_with_incorrect_from(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [], <<"wrong">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).

enable_should_accept_correct_from(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [
                {<<"secret1">>, <<"token1">>},
                {<<"secret2">>, <<"token2">>}
            ])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

disable_should_fail_with_missing_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            BobJID = escalus_utils:get_jid(Bob),

            escalus:send(Bob, escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"disable">>}])),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            CorrectAttrs = [{<<"xmlns">>, <<"urn:xmpp:push:0">>}, {<<"jid">>, BobJID}],

            %% Sending only one attribute should fail
            lists:foreach(
                fun({K,V}) ->
                    escalus:send(Bob, escalus_stanza:iq(<<"set">>,
                                                        [#xmlel{name = <<"disable">>,
                                                                attrs = #{K => V}}])),
                    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                                   escalus:wait_for_stanza(Bob))
                end, CorrectAttrs),
            ok
        end).

disable_should_fail_with_invalid_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            %% Empty JID
            escalus:send(Bob, disable_stanza(<<>>, <<"nodeId">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, disable_stanza(<<>>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).

disable_all(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, disable_stanza(PubsubJID)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

disable_node(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

disable_node_enabled_in_session_removes_it_from_session_info(Config) ->
    escalus:fresh_story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),
            NodeId = pubsub_tools:pubsub_node_name(),

            escalus:send(Bob, enable_stanza(PubsubJID, NodeId, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            Info = mongoose_helper:get_session_info(?RPC_SPEC, Bob),
            {_JID, NodeId, _} = maps:get(?SESSION_KEY, Info),

            escalus:send(Bob, disable_stanza(PubsubJID, NodeId)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            Info2 = mongoose_helper:get_session_info(?RPC_SPEC, Bob),
            false = maps:get(?SESSION_KEY, Info2, false)
        end).

disable_all_nodes_removes_it_from_all_user_session_infos(Config) ->
    escalus:fresh_story(
        Config, [{bob, 2}],
        fun(Bob1, Bob2) ->
            PubsubJID = pubsub_jid(Config),
            NodeId = pubsub_tools:pubsub_node_name(),
            NodeId2 = pubsub_tools:pubsub_node_name(),

            escalus:send(Bob1, enable_stanza(PubsubJID, NodeId, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob1)),

            escalus:send(Bob2, enable_stanza(PubsubJID, NodeId2, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob2)),

            Info = mongoose_helper:get_session_info(?RPC_SPEC, Bob1),
            {JID, NodeId, _} = maps:get(?SESSION_KEY, Info),

            Info2 = mongoose_helper:get_session_info(?RPC_SPEC, Bob2),
            {JID, NodeId2, _} = maps:get(?SESSION_KEY, Info2),

            %% Now Bob1 disables all nodes
            escalus:send(Bob1, disable_stanza(PubsubJID)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob1)),

            %% And we check if Bob1 and Bob2 have push notifications cleared from session info
            Info3 = mongoose_helper:get_session_info(?RPC_SPEC, Bob1),
            false = maps:get(?SESSION_KEY, Info3, false),

            Info4 = mongoose_helper:get_session_info(?RPC_SPEC, Bob2),
            false = maps:get(?SESSION_KEY, Info4, false)
        end).

disable_node_enabled_in_other_session_leaves_current_info_unchanged(Config) ->
    escalus:fresh_story(
        Config, [{bob, 2}],
        fun(Bob1, Bob2) ->
            PubsubJID = pubsub_jid(Config),
            NodeId = pubsub_tools:pubsub_node_name(),
            NodeId2 = pubsub_tools:pubsub_node_name(),

            escalus:send(Bob1, enable_stanza(PubsubJID, NodeId, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob1)),

            escalus:send(Bob2, enable_stanza(PubsubJID, NodeId2, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob2)),

            Info = mongoose_helper:get_session_info(?RPC_SPEC, Bob1),
            {JID, NodeId, _} = maps:get(?SESSION_KEY, Info),

            Info2 = mongoose_helper:get_session_info(?RPC_SPEC, Bob2),
            {JID, NodeId2, _} = maps:get(?SESSION_KEY, Info2),

            %% Now Bob1 disables the node registered by Bob2
            escalus:send(Bob1, disable_stanza(PubsubJID, NodeId)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob1)),

            %% And we check if Bob1 still has its own Node in the session info
            Info3 = mongoose_helper:get_session_info(?RPC_SPEC, Bob1),
            false = maps:get(?SESSION_KEY, Info3, false)
        end).

%%--------------------------------------------------------------------
%% GROUP pm_msg_notifications
%%--------------------------------------------------------------------

pm_no_msg_notifications_if_not_enabled(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            become_unavailable(Bob),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not truly(received_push())),
            ok
        end).

pm_no_msg_notifications_if_user_online(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            escalus:assert(is_message, escalus:wait_for_stanza(Bob)),

            ?assert(not truly(received_push())),
            ok
        end).

pm_msg_notify_if_user_offline(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            AliceJID = bare_jid(Alice),
            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            #{ payload := Payload } = received_push(),
            ?assertMatch(<<"OH, HAI!">>, proplists:get_value(<<"last-message-body">>, Payload)),
            ?assertMatch(AliceJID,
                         proplists:get_value(<<"last-message-sender">>, Payload)),

            ok
        end).

pm_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>,
                                            [{<<"field1">>, <<"value1">>},
                                             {<<"field2">>, <<"value2">>}])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            #{ publish_options := PublishOptions } = received_push(),

            ?assertMatch(<<"value1">>, proplists:get_value(<<"field1">>, PublishOptions)),
            ?assertMatch(<<"value2">>, proplists:get_value(<<"field2">>, PublishOptions)),
            ok
        end).

pm_msg_notify_stops_after_disabling(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            %% Enable
            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            %% Disable
            escalus:send(Bob, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not received_push()),

            ok
        end).

pm_msg_notify_stops_after_removal(Config) ->
    PubsubJID = pubsub_jid(Config),
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            %% Enable
            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            %% Remove account - this should disable the notifications
            Pid = mongoose_helper:get_session_pid(Bob, distributed_helper:mim()),
            escalus_connection:send(Bob, escalus_stanza:remove_account()),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            mongoose_helper:wait_for_pid_to_die(Pid)
        end),
    BobUser = lists:keyfind(bob, 1, escalus_config:get_config(escalus_users, Config)),
    escalus_users:create_user(Config, BobUser),
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            become_unavailable(Bob),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            ?assert(not truly(received_push()))
        end).

%%--------------------------------------------------------------------
%% GROUP muclight_msg_notifications
%%--------------------------------------------------------------------

muclight_no_msg_notifications_if_not_enabled(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = room_name(Config),
            create_room(Room, [bob, alice, kate], Config),
            become_unavailable(Alice),
            become_unavailable(Kate),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),

            escalus:send(Bob, Stanza),

            ?assert(not truly(received_push())),

            ok
        end).

muclight_no_msg_notifications_if_user_online(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = room_name(Config),
            PubsubJID = pubsub_jid(Config),

            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Kate),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            ?assert(not truly(received_push())),
            ok
        end).

muclight_msg_notify_if_user_offline(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = pubsub_jid(Config),
            Room = room_name(Config),
            BobJID = bare_jid(Bob),

            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            #{ payload := Payload } = received_push(),

            ?assertMatch(Msg, proplists:get_value(<<"last-message-body">>, Payload)),
            SenderId = <<(room_bin_jid(Room))/binary, "/" ,BobJID/binary>>,
            ?assertMatch(SenderId,
                         proplists:get_value(<<"last-message-sender">>, Payload)),
            ok
        end).

muclight_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = pubsub_jid(Config),
            Room = room_name(Config),

            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>,
                                            [{<<"field1">>, <<"value1">>},
                                             {<<"field2">>, <<"value2">>}])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            #{ publish_options := PublishOptions } = received_push(),

            ?assertMatch(<<"value1">>, proplists:get_value(<<"field1">>, PublishOptions)),
            ?assertMatch(<<"value2">>, proplists:get_value(<<"field2">>, PublishOptions)),
            ok
        end).

muclight_msg_notify_stops_after_disabling(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            Room = room_name(Config),
            PubsubJID = pubsub_jid(Config),
            create_room(Room, [bob, alice, kate], Config),

            %% Enable
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

            %% Disable
            escalus:send(Alice, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            ?assert(not truly(received_push())),
            ok
        end).

%%--------------------------------------------------------------------
%% Remote code
%% Functions that will be executed in MongooseIM context + helpers that set them up
%%--------------------------------------------------------------------

start_route_listener(Config) ->
    %% We put namespaces in the state to avoid injecting push_helper module to MIM as well
    State = #{ pid => self(),
               pub_options_ns => push_helper:ns_pubsub_pub_options(),
               push_form_ns => push_helper:push_form_type() },
    Handler = rpc(mongoose_packet_handler, new, [?MODULE, #{state => State}]),
    Domain = pubsub_domain(Config),
    rpc(mongoose_router, register_route, [Domain, Handler]),
    Config.

stop_route_listener(Config) ->
    Domain = pubsub_domain(Config),
    rpc(mongoose_router, unregister_route, [Domain]).

process_packet(Acc, _From, To, El, #{state := State}) ->
    #{ pid := TestCasePid, pub_options_ns := PubOptionsNS, push_form_ns := PushFormNS } = State,
    PublishXML = exml_query:path(El, [{element, <<"pubsub">>},
                                      {element, <<"publish-options">>},
                                      {element, <<"x">>}]),
    PublishOptions = parse_form(PublishXML),

    PayloadXML = exml_query:path(El, [{element, <<"pubsub">>},
                                      {element, <<"publish">>},
                                      {element, <<"item">>},
                                      {element, <<"notification">>},
                                      {element, <<"x">>}]),
    Payload = parse_form(PayloadXML),

    case valid_ns_if_defined(PubOptionsNS, PublishOptions) andalso
         valid_ns_if_defined(PushFormNS, Payload) of
        true ->
            TestCasePid ! push_notification(jid:to_binary(To), Payload, PublishOptions);
        false ->
            %% We use publish_options0 and payload0 to avoid accidental match in received_push
            %% even after some tests updates and refactors
            TestCasePid ! #{ error => invalid_namespace,
                             publish_options0 => PublishOptions,
                             payload0 => Payload }
    end,
    Acc.

parse_form(undefined) ->
    undefined;
parse_form(#xmlel{name = <<"x">>} = Form) ->
    parse_form(exml_query:subelements(Form, <<"field">>));
parse_form(Fields) when is_list(Fields) ->
    lists:map(
        fun(Field) ->
            {exml_query:attr(Field, <<"var">>),
             exml_query:path(Field, [{element, <<"value">>}, cdata])}
        end, Fields).

valid_ns_if_defined(_, undefined) ->
    true;
valid_ns_if_defined(NS, FormProplist) ->
    NS =:= proplists:get_value(<<"FORM_TYPE">>, FormProplist).

start_hook_listener(Config) ->
    TestCasePid = self(),
    PubSubJID = pubsub_jid(Config),
    rpc(?MODULE, rpc_start_hook_handler, [TestCasePid, PubSubJID]),
    [{pid, TestCasePid}, {jid, PubSubJID} | Config].

stop_hook_listener(Config) ->
    TestCasePid = proplists:get_value(pid, Config),
    PubSubJID = proplists:get_value(jid, Config),
    rpc(?MODULE, rpc_stop_hook_handler, [TestCasePid, PubSubJID]).

rpc_start_hook_handler(TestCasePid, PubSubJID) ->
    gen_hook:add_handler(push_notifications,  <<"localhost">>,
                         fun ?MODULE:hook_handler_fn/3,
                         #{pid => TestCasePid, jid => PubSubJID}, 50).

hook_handler_fn(Acc,
                #{notification_forms := [PayloadMap], options := OptionMap} = _Params,
                #{pid := TestCasePid, jid := PubSubJID} = _Extra) ->
    try jid:to_binary(mongoose_acc:get(push_notifications, pubsub_jid, Acc)) of
        PubSubJIDBin when PubSubJIDBin =:= PubSubJID ->
            TestCasePid ! push_notification(PubSubJIDBin,
                                            maps:to_list(PayloadMap),
                                            maps:to_list(OptionMap));
        _ -> ok
    catch
        C:R:S ->
            TestCasePid ! #{event => handler_error,
                            class => C,
                            reason => R,
                            stacktrace => S}
    end,
    {ok, Acc}.

rpc_stop_hook_handler(TestCasePid, PubSubJID) ->
    gen_hook:delete_handler(push_notifications, <<"localhost">>,
                            fun ?MODULE:hook_handler_fn/3,
                            #{pid => TestCasePid, jid => PubSubJID}, 50).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

create_room(Room, [Owner | Members], Config) ->
    Domain = domain_helper:domain(),
    create_room(Room, <<"muclight.", Domain/binary>>, Owner, Members,
                                Config, <<"v1">>).

received_push() ->
    receive
        #{ push_notification := true } = Push -> Push
    after
        timer:seconds(5) ->
            ct:pal("~p", [#{ result => nomatch, msg_inbox => process_info(self(), messages) }]),
            false
    end.

truly(false) ->
    false;
truly(#{ push_notification := true }) ->
    true.

push_notification(PubsubJID, Payload, PublishOpts) ->
    #{push_notification => true, pubsub_jid_bin => PubsubJID,
      publish_options => PublishOpts, payload => Payload}.

bare_jid(JIDOrClient) ->
    ShortJID = escalus_client:short_jid(JIDOrClient),
    string:lowercase(ShortJID).

add_pubsub_jid(Config) ->
    CaseName = proplists:get_value(case_name, Config),
    CaseNameBin = atom_to_binary(CaseName, utf8),
    NameSuffix = uniq_name_suffix(),
    UniqID = <<CaseNameBin/binary, "_", NameSuffix/binary>>,
    {PubSubNodeName, PubSubDomain} =
        case ?config(pubsub_host, Config) of
            virtual ->
                %% unique node name, but preconfigured domain name
                {UniqID, ?VIRTUAL_PUBSUB_DOMAIN};
            _ ->
                %% any node name, but unique domain. unique domain
                %% is required to intercept safely message routing
                {<<"pub-sub">>, UniqID}
        end,
    PubSubJID = <<PubSubNodeName/binary, "@", PubSubDomain/binary>>,
    [{pubsub_jid, PubSubJID}, {pubsub_domain, PubSubDomain} | Config].

uniq_name_suffix() ->
    {_, S, US} = erlang:timestamp(),
    L = lists:flatten([integer_to_list(S rem 100), ".", integer_to_list(US)]),
    list_to_binary(L).

pubsub_domain(Config) ->
    proplists:get_value(pubsub_domain, Config).

pubsub_jid(Config) ->
    proplists:get_value(pubsub_jid, Config).

room_name(Config) ->
    CaseName = proplists:get_value(case_name, Config),
    <<"room_", (atom_to_binary(CaseName, utf8))/binary>>.
