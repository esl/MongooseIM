%%==============================================================================
%% Copyright 2026 Erlang Solutions Ltd.
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

-module(mod_stanzaid_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(NS_STANZAID, <<"urn:xmpp:sid:0">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, pm},
        {group, muc},
        {group, muclight}
    ].

groups() ->
    [
        {pm, [], [stanza_id_pm, stanza_id_pm_mam]},
        {muc, [], [
            stanza_id_muc,
            stanza_id_muc_mam
        ]},
        {muclight, [], [
            stanza_id_muclight,
            stanza_id_muclight_mam]}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    mongoose_helper:inject_module(?MODULE),
    Config1 = dynamic_modules:save_modules(domain_helper:host_type(), Config0),
    dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_stanzaid, []}]),
    case mam_helper:backend() of
        disabled ->
            {skip, "no mam backend"};
        Backend ->
            escalus:init_per_suite([{mam_backend, Backend} | Config1])
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    ok.

init_per_group(muc, Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    muc_helper:load_muc(),
    init_per_group(generic, Config1);
init_per_group(muclight, Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    dynamic_modules:ensure_modules(HostType, [{mod_muc_light, common_muc_light_opts()}]),
    Config1;
init_per_group(_GroupName, Config) ->
    Config1 = dynamic_modules:save_modules(domain_helper:host_type(), Config),
    escalus_fresh:create_users(Config1, escalus:get_users([alice, bob])).

end_per_group(muc, Config) ->
    muc_helper:unload_muc(),
    end_per_group(generic, Config);
end_per_group(_, Config) ->
    dynamic_modules:restore_modules(Config),
    Config.

init_per_testcase(stanza_id_pm_mam, Config) ->
    Config1 = init_per_testcase(generic, Config),
    MamOpts = mam_helper:config_opts(#{pm => #{}, backend => backend(Config)}),
    ok = dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_mam, MamOpts}]),
    Config1;
init_per_testcase(stanza_id_muc_mam, Config) ->
    Config1 = init_per_testcase(generic, Config),
    MamOpts = mam_helper:config_opts(#{muc => #{host => distributed_helper:subhost_pattern(muc_helper:muc_host_pattern())},
                                       async_writer => #{enabled => false},
                                       backend => backend(Config)}),
    ok = dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_mam, MamOpts}]),
    Config1;
init_per_testcase(stanza_id_muclight_mam, Config) ->
    Config1 = init_per_testcase(generic, Config),
    MamOpts = mam_helper:config_opts(#{muc => #{host => {prefix, <<"muclight.">>}},
                                       async_writer => #{enabled => false},
                                       backend => backend(Config)
                                     }),
    ok = dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_mam, MamOpts}]),
    Config1;
init_per_testcase(CaseName, Config) ->
    Config1 = dynamic_modules:save_modules(domain_helper:host_type(), Config),
    inject_handler(),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    dynamic_modules:restore_modules(Config),
    remove_handler(),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

stanza_id_pm(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            message(User1, User2, <<"Hi!">>),
            M = escalus:wait_for_stanza(User2),
            ?assert(has_stanza_id(M))
        end).

stanza_id_pm_mam(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            message(Alice, Bob, <<"Hi!">>),
            Message = escalus:wait_for_stanza(Bob),
            % get our stable stanza id
            OurBinStanzaID = receive_stanza_id(),
            % message arrived with the same id
            ?assertEqual(OurBinStanzaID, get_stanza_id(Message)),
            % and it has the same id in both archives
            mam_helper:wait_for_archive_size(Bob, 1),
            escalus:send(Bob, mam_helper:stanza_archive_request(mam_helper:mam06_props(), <<"q1">>)),
            Barch = escalus:wait_for_stanza(Bob),
            ?assertEqual(OurBinStanzaID, get_stanza_id(Barch)),
            escalus:send(Alice, mam_helper:stanza_archive_request(mam_helper:mam06_props(), <<"q1">>)),
            Aarch = escalus:wait_for_stanza(Alice),
            ?assertEqual(OurBinStanzaID, get_stanza_id(Aarch)),
            ok
        end).

stanza_id_muc(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun(Config, Alice, Bob) ->
        escalus:send(Bob, muc_helper:stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus:wait_for_stanzas(Bob, 2),
        escalus:send(Alice, muc_helper:stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Alice))),
        escalus:wait_for_stanza(Bob),
        escalus:wait_for_stanzas(Alice, 3),
%%
        Msg = <<"chat message">>,
        Id = <<"MyID">>,
        Stanza = escalus_stanza:set_id(
            escalus_stanza:groupchat_to(
                muc_helper:room_address(
                    ?config(room, Config)), Msg), Id),
        escalus:send(Alice, Stanza),
        AliceStanza = escalus:wait_for_stanza(Alice),
        BobStanza = escalus:wait_for_stanza(Bob),
        OurBinStanzaID = receive_stanza_id(),
        ?assertEqual(OurBinStanzaID, get_stanza_id(AliceStanza)),
        ?assertEqual(OurBinStanzaID, get_stanza_id(BobStanza)),
        ok
    end).

stanza_id_muc_mam(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun(Config, Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = muc_helper:room_address(Room),
        escalus:send(Bob, muc_helper:stanza_muc_enter_room(Room, escalus_utils:get_username(Bob))),
        escalus:wait_for_stanzas(Bob, 2),
        escalus:send(Alice, muc_helper:stanza_muc_enter_room(Room, escalus_utils:get_username(Alice))),
        escalus:wait_for_stanza(Bob),
        escalus:wait_for_stanzas(Alice, 3),
%%
        Msg = <<"chat message">>,
        Id = <<"MyID">>,
        Stanza = escalus_stanza:set_id(
            escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Alice, Stanza),
        AliceStanza = escalus:wait_for_stanza(Alice),
        BobStanza = escalus:wait_for_stanza(Bob),
        OurBinStanzaID = receive_stanza_id(),
        ?assertEqual(OurBinStanzaID, get_stanza_id(AliceStanza)),
        ?assertEqual(OurBinStanzaID, get_stanza_id(BobStanza)),
        ArchReq = escalus_stanza:to(mam_helper:stanza_archive_request(mam_helper:mam06_props(), <<"q1">>),
                                    RoomAddr),
        escalus:send(Alice, ArchReq),
        [AliceArchStanza, _Fin] = escalus:wait_for_stanzas(Alice, 2, 100),
        ?assertEqual(OurBinStanzaID, get_stanza_id(AliceArchStanza)),
        ok
    end).

stanza_id_muclight(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomName = <<"just-a-testroom">>,
        muc_light_helper:create_room(RoomName, muc_light_helper:muc_host(),
                                     Alice, [Bob], Config, muc_light_helper:ver(1)),
%%
        Stanza = escalus_stanza:groupchat_to(muc_light_helper:room_bin_jid(RoomName), <<"Hello">>),
        escalus:send(Bob, Stanza),
        AliceStanza = escalus:wait_for_stanza(Alice, 100),
        BobStanza = escalus:wait_for_stanza(Bob, 100),
        OurBinStanzaID = receive_stanza_id(),
        ?assertEqual(OurBinStanzaID, get_stanza_id(AliceStanza)),
        ?assertEqual(OurBinStanzaID, get_stanza_id(BobStanza)),
        ok
    end).

stanza_id_muclight_mam(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomName = <<"yet-another-testroom">>,
        RoomAddr = muc_light_helper:room_bin_jid(RoomName),
        muc_light_helper:create_room(RoomName, muc_light_helper:muc_host(),
                                     Alice, [Bob], Config, muc_light_helper:ver(1)),
%%
        Stanza = escalus_stanza:groupchat_to(muc_light_helper:room_bin_jid(RoomName), <<"Hello">>),
        escalus:send(Bob, Stanza),
        AliceStanza = escalus:wait_for_stanza(Alice, 100),
        BobStanza = escalus:wait_for_stanza(Bob, 100),
        OurBinStanzaID = receive_stanza_id(),
        ?assertEqual(OurBinStanzaID, get_stanza_id(AliceStanza)),
        ?assertEqual(OurBinStanzaID, get_stanza_id(BobStanza)),
        ArchReq = escalus_stanza:to(mam_helper:stanza_archive_request(mam_helper:mam06_props(), <<"q1">>),
                                    RoomAddr),
        escalus:send(Alice, ArchReq),
        [AliceArchStanza, _] = escalus:wait_for_stanzas(Alice, 2, 100),
        ?assertEqual(OurBinStanzaID, get_stanza_id(AliceArchStanza)),
        escalus:send(Bob, ArchReq),
        [BobArchStanza, _] = escalus:wait_for_stanzas(Bob, 2, 100),
        ?assertEqual(OurBinStanzaID, get_stanza_id(BobArchStanza)),
        ok
    end).

%%
%% helpers
%%

message(From, To, MsgTxt) ->
    escalus_client:send(From, escalus_stanza:chat_to(To, MsgTxt)).

has_stanza_id(#xmlel{children = Ch}) ->
    has_stanza_id(Ch);
has_stanza_id([]) ->
    false;
has_stanza_id([#xmlel{name = <<"stanza-id">>, attrs = #{<<"id">> := _,
                                                        <<"xmlns">> := ?NS_STANZAID}} | _]) ->
    true;
has_stanza_id([_ | Tail]) ->
    has_stanza_id(Tail).

get_stanza_id(#xmlel{children = Ch}) ->
    get_stanza_id(Ch);
get_stanza_id([]) ->
    undefined;
get_stanza_id([#xmlel{name = <<"stanza-id">>, attrs = #{<<"id">> := Id,
                                                        <<"xmlns">> := ?NS_STANZAID}} | _]) ->
    Id;
get_stanza_id([#xmlel{name = <<"result">>, attrs = #{<<"id">> := Id}} | _]) ->
    Id;
get_stanza_id([_ | Tail]) ->
    get_stanza_id(Tail).

inject_handler() ->
    Tpid = self(),
    distributed_helper:rpc(distributed_helper:mim(), ?MODULE, start_handler, [Tpid, domain_helper:host_type()]).

remove_handler() ->
    Tpid = self(),
    distributed_helper:rpc(distributed_helper:mim(), ?MODULE, stop_handler, [Tpid, domain_helper:host_type()]).

start_handler(Pid, HostType) ->
    gen_hook:add_handler(user_send_message, HostType,
                         fun ?MODULE:send_stanza_id/3,
                         #{pid => Pid}, 50),
    ok.

stop_handler(Pid, HostType) ->
    gen_hook:delete_handler(user_send_message, HostType,
                            fun ?MODULE:send_stanza_id/3,
                            #{pid => Pid}, 50),
    ok.

send_stanza_id(Acc, _, #{pid := Pid}) ->
    Pid ! {stanza_id, mongoose_acc:get(stable_stanza_id, value, 0, Acc)},
    {ok, Acc}.

receive_stanza_id() ->
    OurStanzaID = receive
                      {stanza_id, Id} -> Id
                  after 100 ->
            ct:fail("did not receive stanza id")
                  end,
    integer_to_binary(OurStanzaID, 32).

common_muc_light_opts() ->
    Opts = #{backend => mongoose_helper:mnesia_or_rdbms_backend(),
      cache_affs => config_parser_helper:default_config([modules, mod_muc_light, cache_affs])},
    maps:merge(config_parser_helper:default_mod_config(mod_muc_light), Opts).

backend(Config) ->
    ?config(mam_backend, Config).