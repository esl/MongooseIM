-module(muc_legacy_log_option_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         legacy_logging_option_does_not_break_room/1]).

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [host_type/0]).
-import(muc_helper, [muc_host/0,
                     room_address/1,
                     stanza_muc_enter_room/2,
                     stanza_to_room/2,
                     stanza_to_room/3,
                     fresh_room_name/0]).

-define(WAIT_TIMEOUT, 10000).
-define(MSG_TEXT, <<"legacy option message">>).
-define(NS_MUC_ROOMCONFIG, <<"http://jabber.org/protocol/muc#roomconfig">>).

all() ->
    [legacy_logging_option_does_not_break_room].

suite() ->
    distributed_helper:require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    Config2 = dynamic_modules:save_modules(host_type(), Config1),
    muc_helper:load_muc(),
    mongoose_helper:ensure_muc_clean(),
    ct:log("MUC backend: ~p", [mongoose_helper:mnesia_or_rdbms_backend()]),
    Config2.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    mongoose_helper:ensure_muc_clean(),
    muc_helper:unload_muc(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

legacy_logging_option_does_not_break_room(Config) ->
    RoomName = fresh_room_name(),
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        create_persistent_room(Alice, RoomName),
        join_room(Bob, RoomName, <<"bob">>, 3),
        escalus:wait_for_stanza(Alice, ?WAIT_TIMEOUT),
        leave_room(RoomName, Alice),
        escalus:wait_for_stanza(Bob, ?WAIT_TIMEOUT),
        leave_room(RoomName, Bob),
        stop_room(RoomName),
        inject_legacy_logging_option(RoomName),
        rejoin_and_chat(Alice, Bob, RoomName)
    end),
    muc_helper:destroy_room(muc_host(), RoomName),
    ok = rpc(mim(), mod_muc, forget_room, [host_type(), muc_host(), RoomName]).

create_persistent_room(Alice, RoomName) ->
    Nick = escalus_utils:get_username(Alice),
    escalus:send(Alice, stanza_muc_enter_room(RoomName, Nick)),
    escalus:wait_for_stanzas(Alice, 2),
    Fields = [#{var => <<"muc#roomconfig_persistentroom">>,
                values => [<<"1">>],
                type => <<"boolean">>}],
    Form = stanza_configuration_form(RoomName, Fields),
    Result = escalus:send_iq_and_wait_for_result(Alice, Form),
    escalus:assert(is_stanza_from, [room_address(RoomName)], Result).

rejoin_and_chat(Alice, Bob, RoomName) ->
    join_room(Alice, RoomName, escalus_utils:get_username(Alice), 2),
    join_room(Bob, RoomName, <<"bob">>, 3),
    escalus:wait_for_stanza(Alice, ?WAIT_TIMEOUT),
    Message = escalus_stanza:groupchat_to(room_address(RoomName), ?MSG_TEXT),
    escalus:send(Alice, Message),
    escalus:assert(is_groupchat_message, [?MSG_TEXT], escalus:wait_for_stanza(Bob)),
    escalus:assert(is_groupchat_message, [?MSG_TEXT], escalus:wait_for_stanza(Alice)),
    leave_room(RoomName, Alice),
    escalus:wait_for_stanza(Bob, ?WAIT_TIMEOUT),
    leave_room(RoomName, Bob).

join_room(User, RoomName, Nick, ExpectedStanzas) ->
    escalus:send(User, stanza_muc_enter_room(RoomName, Nick)),
    escalus:wait_for_stanzas(User, ExpectedStanzas).

leave_room(RoomName, User) ->
    Nick = escalus_utils:get_username(User),
    Stanza = stanza_to_room(escalus_stanza:presence(<<"unavailable">>), RoomName, Nick),
    escalus:send(User, Stanza),
    escalus:wait_for_stanza(User, ?WAIT_TIMEOUT).

stop_room(RoomName) ->
    HostType = host_type(),
    MucHost = muc_host(),
    case rpc(mim(), mod_muc_online_backend, find_room_pid, [HostType, MucHost, RoomName]) of
        {ok, Pid} ->
            Pid ! stop_persistent_room_process,
            wait_helper:wait_until(
              fun() ->
                  rpc(mim(), mod_muc_online_backend, find_room_pid, [HostType, MucHost, RoomName])
              end,
              {error, not_found}),
            ok;
        {error, not_found} ->
            ok
    end.

inject_legacy_logging_option(RoomName) ->
    HostType = host_type(),
    MucHost = muc_host(),
    case rpc(mim(), mod_muc, restore_room, [HostType, MucHost, RoomName]) of
        {ok, Opts} ->
            Affs = proplists:get_value(affiliations, Opts, undefined),
            BaseOpts = proplists:delete(affiliations, Opts),
            LegacyOpts = [{logging, true} | BaseOpts],
            FullOpts = case Affs of
                           undefined -> LegacyOpts;
                           _ -> [{affiliations, Affs} | LegacyOpts]
                       end,
            ok = rpc(mim(), mod_muc, store_room, [HostType, MucHost, RoomName, FullOpts]);
        {error, Reason} ->
            ct:fail({restore_room_failed, Reason})
    end.

stanza_configuration_form(Room, Fields) ->
    Form = form_helper:form(#{fields => Fields, ns => ?NS_MUC_ROOMCONFIG}),
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_OWNER, [Form]), Room).
