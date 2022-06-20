-module(mam_send_message_SUITE).

%% API
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([mam_muc_send_message/1,
         mam_pm_send_message/1]).

-import(mam_helper,
        [stanza_archive_request/2,
         wait_archive_respond/1,
         assert_respond_size/2,
         respond_messages/1,
         parse_forwarded_message/1]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             subhost_pattern/1,
                             rpc/4]).
-import(domain_helper, [host_type/0]).
-import(config_parser_helper, [mod_config/2]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, send_message}].

groups() ->
    [
     {send_message, [], [mam_pm_send_message,
                         mam_muc_send_message]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
init_per_group(Group, Config) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        true ->
            load_custom_module(),
            Config2 = dynamic_modules:save_modules(host_type(), Config),
            dynamic_modules:ensure_modules(host_type(), group_to_modules(Group)),
            [{props, mam_helper:mam06_props()}|Config2];
        false ->
            {skip, require_rdbms}
    end.

end_per_group(_Groupname, Config) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        true ->
            dynamic_modules:restore_modules(Config);
        false ->
            ok
    end,
    ok.

group_to_modules(send_message) ->
    MH = subhost_pattern(muc_light_helper:muc_host_pattern()),
    [{mod_mam, mam_helper:config_opts(#{pm => #{},
                                        muc => #{host => MH},
                                        send_message => mam_send_message_example})},
     {mod_muc_light, mod_config(mod_muc_light,
                                #{backend => mongoose_helper:mnesia_or_rdbms_backend()})},
     {mam_send_message_example, []}].

load_custom_module() ->
    mam_send_message_example:module_info(),
    {Mod, Code, File} = code:get_object_code(mam_send_message_example),
    rpc(mim(), code, load_binary, [Mod, File, Code]).

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================

init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    escalus:end_per_testcase(TestCase, Config).

%%%===================================================================
%%% Test Cases
%%%===================================================================

mam_pm_send_message(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:wait_for_stanza(Bob),
        mam_helper:wait_for_archive_size(Alice, 1),
        mam_helper:wait_for_archive_size(Bob, 1),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>)),
        Res = wait_archive_respond(Alice),
        assert_respond_size(1, Res),
        [Msg] = respond_messages(Res),
        verify_has_some_hash(Msg)
        end,
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], F).

mam_muc_send_message(Config0) ->
    F = fun(Config, Alice) ->
        P = ?config(props, Config),
        Room = muc_helper:fresh_room_name(),
        MucHost = muc_light_helper:muc_host(),
        muc_light_helper:create_room(Room, MucHost, alice,
                                     [], Config, muc_light_helper:ver(1)),
        escalus_assert:has_no_stanzas(Alice),
        RoomAddr = <<Room/binary, "@", MucHost/binary>>,
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, <<"text">>)),
        M = escalus:wait_for_stanza(Alice),
        escalus:assert(is_message, M),
        escalus_assert:has_no_stanzas(Alice),
        mam_helper:wait_for_room_archive_size(MucHost, Room, 1),
        escalus:send(Alice, escalus_stanza:to(stanza_archive_request(P, <<"q1">>), RoomAddr)),
        [Msg] = respond_messages(assert_respond_size(1, wait_archive_respond(Alice))),
        verify_has_some_hash(Msg)
        end,
    escalus:fresh_story_with_config(Config0, [{alice, 1}], F).

verify_has_some_hash(Msg) ->
    Hash = exml_query:path(Msg, [{element, <<"result">>},
                                 {element, <<"some_hash">>},
                                 {attr, <<"value">>}]),
    binary_to_integer(Hash). %% is integer
