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

-module(metrics_c2s_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").

-import(domain_helper, [host_type/0]).
-import(distributed_helper, [mim/0, rpc/4]).

-dialyzer({nowarn_function, tls_cert_remaining_days/1}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, events},
     {group, invalid_cert},
     {group, not_yet_valid_cert},
     {group, expired_cert}].

groups() ->
    [{events, [parallel], [login,
                           message,
                           message_error,
                           presence,
                           presence_error,
                           iq,
                           iq_error,
                           message_bounced,
                           tls_cert_remaining_days]},
      {invalid_cert, [], [tls_cert_remaining_days]},
      {not_yet_valid_cert, [], [tls_cert_remaining_days]},
      {expired_cert, [], [tls_cert_remaining_days]}].

suite() ->
    [{require, ejabberd_node} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    HostType = host_type(),
    Listener = c2s_listener(),
    Config1 = [{c2s_listener_backup, Listener} | Config],
    instrument_helper:start([{xmpp_element_in, labels()},
                             {xmpp_element_out, labels()},
                             {sm_message_bounced, #{host_type => HostType}},
                             {tls_cert_remaining_days, #{listener_id => listener_id(Listener)}}]),
    Config2 = instrument_helper:ensure_frequent_probes(Config1),
    restart_listener(Listener),
    Config3 = dynamic_modules:save_modules(HostType, Config2),
    dynamic_modules:ensure_stopped(HostType, [mod_offline]),
    escalus:init_per_suite(Config3).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config),
    dynamic_modules:restore_modules(Config),
    mongoose_helper:restore_config(Config),
    Listener = proplists:get_value(c2s_listener_backup, Config),
    restart_listener(Listener),
    instrument_helper:stop().

init_per_group(invalid_cert, Config) ->
    #{tls := Tls} = Listener = proplists:get_value(c2s_listener_backup, Config),
    restart_listener(Listener#{tls => Tls#{certfile => <<"priv/ssl/fake_key.pem">>}}),
    Config;
init_per_group(Group, Config) when Group == not_yet_valid_cert; Group == expired_cert ->
    CertSpec = #{cn => atom_to_list(Group)},
    #{key := Keyfile, cert := Certfile} = ca_certificate_helper:generate_cert(Config, CertSpec, #{}),
    #{tls := Tls} = Listener = proplists:get_value(c2s_listener_backup, Config),
    restart_listener(Listener#{tls => Tls#{certfile => Certfile, keyfile => Keyfile}}),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Listener = proplists:get_value(c2s_listener_backup, Config),
    restart_listener(Listener).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

c2s_listener() ->
    Listeners = rpc(mim(), mongoose_config, get_opt, [[listen]]),
    hd([Opts || #{access := c2s, tls := #{}} = Opts <- Listeners]).

listener_id(#{port := Port, ip_tuple := IPTuple, proto := Proto}) ->
    iolist_to_binary(io_lib:format("~s@~s/~p", [inet:ntoa(IPTuple), Proto, Port])).

restart_listener(Listener) ->
    [_ | Rest] = rpc(mim(), mongoose_config, get_opt, [[listen]]),
    rpc(mim(), mongoose_config, set_opt, [[listen], [Listener | Rest]]),
    % Instrumentation is set up again only when all listeners are restarted
    rpc(mim(), mongoose_listener, stop, []),
    rpc(mim(), mongoose_listener, start, []).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

login(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun login_story/1).

login_story(Alice) ->
    AliceBareJid = escalus_client:short_jid(Alice),

    %% Note: The first two events might originate from other tests because of unknown JID.
    %% It is acceptable, because the goal is to check that they are emitted when users log in.
    assert_events(out, fun(#xmlel{name = Name}) -> Name =:= <<"stream:features">> end),
    assert_events(in, fun(#xmlel{name = Name}) -> Name =:= <<"auth">> end),

    assert_event(out, AliceBareJid, #{},
                 fun(#xmlel{name = Name}) -> Name =:= <<"success">> end),
    assert_event(out, AliceBareJid, #{},
                 fun(#xmlel{name = Name}) -> Name =:= <<"stream:features">> end),
    assert_event(in, AliceBareJid, #{stanza_count => 1, iq_count => 1},
                 fun(El) -> escalus_pred:is_iq_set(El) andalso has_child(<<"bind">>, El) end),
    assert_event(out, Alice, #{stanza_count => 1, iq_count => 1},
                 fun(El) -> escalus_pred:is_iq_result(El) andalso has_child(<<"bind">>, El) end),
    assert_event(in, Alice, #{stanza_count => 1, iq_count => 1},
                 fun(El) -> escalus_pred:is_iq_set(El) andalso has_child(<<"session">>, El) end),
    assert_event(out, Alice, #{stanza_count => 1, iq_count => 1},
                 fun(El) -> escalus_pred:is_iq_result(El) andalso has_child(<<"session">>, El) end),
    assert_event(in, Alice, #{stanza_count => 1, presence_count => 1},
                 fun escalus_pred:is_presence/1),
    assert_event(out, Alice, #{stanza_count => 1, presence_count => 1},
                 fun escalus_pred:is_presence/1).

message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun message_story/2).

message_story(Alice, Bob) ->
    Msg = escalus_stanza:chat_to(Bob, <<"Hi!">>),
    escalus_client:send(Alice, Msg),
    MsgToBob = escalus_client:wait_for_stanza(Bob),
    escalus:assert(is_chat_message, MsgToBob),
    assert_event(in, Alice, #{stanza_count => 1, message_count => 1}, Msg),
    assert_event(out, Bob, #{stanza_count => 1, message_count => 1}, MsgToBob).

message_error(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun message_error_story/1).

message_error_story(Alice) ->
    StrangerJid = <<"stranger@", (escalus_client:server(Alice))/binary>>,
    Msg = escalus_stanza:chat_to(StrangerJid, <<"Hi!">>),
    escalus_client:send(Alice, Msg),
    Error = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Error),
    assert_event(in, Alice, #{stanza_count => 1, message_count => 1}, Msg),
    assert_event(out, Alice, #{stanza_count => 1, error_count => 1, message_error_count => 1},
                 Error).

presence(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun presence_story/2).

presence_story(Alice, Bob) ->
    Presence = escalus_stanza:presence_direct(Bob, <<"unavailable">>),
    escalus:send(Alice, Presence),
    PresenceToBob = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence, PresenceToBob),
    assert_event(in, Alice, #{stanza_count => 1, presence_count => 1}, Presence),
    assert_event(out, Bob, #{stanza_count => 1, presence_count => 1}, PresenceToBob).

presence_error(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun presence_error_story/1).

presence_error_story(Alice) ->
    Presence = escalus_stanza:presence(<<"unbelievable">>),
    escalus:send(Alice, Presence),
    Error = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], Error),
    assert_event(in, Alice, #{stanza_count => 1, presence_count => 1}, Presence),
    assert_event(out, Alice, #{stanza_count => 1, error_count => 1, presence_error_count => 1},
                               Error).

iq(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun iq_story/1).

iq_story(Alice) ->
    Request = escalus_stanza:roster_get(),
    escalus_client:send(Alice, Request),
    Response = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_iq_result, [Request], Response),
    assert_event(in, Alice, #{stanza_count => 1, iq_count => 1}, Request),
    assert_event(out, Alice, #{stanza_count => 1, iq_count => 1}, Response).

iq_error(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun iq_error_story/1).

iq_error_story(Alice) ->
    Request = escalus_stanza:iq_get(<<"bad-ns">>, []),
    escalus:send(Alice, Request),
    Error = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Error),
    assert_event(in, Alice, #{stanza_count => 1, iq_count => 1}, Request),
    assert_event(out, Alice, #{stanza_count => 1, error_count => 1, iq_error_count => 1}, Error).

message_bounced(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun message_bounced_story/3).

message_bounced_story(Config, Alice, Bob) ->
    mongoose_helper:logout_user(Config, Bob),
    escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
    Error = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Error),
    assert_event(out, Alice, #{stanza_count => 1, error_count => 1, message_error_count => 1},
                 Error),
    assert_message_bounced_event(Alice, Bob).

tls_cert_remaining_days(Config) ->
    #{tls := #{certfile := Certfile}} = Listener = c2s_listener(),
    Event = tls_cert_remaining_days,
    Labels = #{listener_id => listener_id(Listener)},

    Expected = case group_name(Config) of
                   invalid_cert ->
                       -32768;
                   not_yet_valid_cert ->
                       -32767;
                    _ ->
                        Cmd = io_lib:format("openssl x509 -in ~s -noout -enddate | cut -d= -f2", [Certfile]),
                        CmdOutput = list_to_binary(string:trim(rpc(mim(), os, cmd, [Cmd]))),
                        % Timezone is ignored since all datetimes are in UTC
                        {ok, EndDate} = tempo:parse(<<"%b %d %H:%M:%S %Y">>, {unix, CmdOutput}),
                        ct:pal("~p", [Certfile]),
                        ct:pal("~p -> ~p", [CmdOutput, EndDate]),
                        (EndDate - erlang:system_time(second)) div (24 * 60 * 60)
               end,
    ct:pal("~p", [Expected]),

    F = fun(#{count := NewCount}) -> NewCount == Expected end,
    instrument_helper:wait_and_assert_new(Event, Labels, F).

%% Helpers

%% C2S instrumentation events

has_child(SubElName, El) ->
    exml_query:subelement(El, SubElName) =/= undefined.

%% Assert exactly one XML stanza event with specific measurements and a JID
assert_event(Dir, ClientOrJid, Measurements, Element = #xmlel{}) ->
    assert_event(Dir, ClientOrJid, Measurements, fun(El) -> El =:= Element end);
assert_event(Dir, ClientOrJid, Measurements, CheckElFun) when is_function(CheckElFun, 1)->
    Jid = jid:from_binary(escalus_utils:get_jid(ClientOrJid)),
    Filter = fun(M = #{element := El}) ->
                     maps:remove(element, M) =:= Measurements#{jid => Jid, count => 1,
                                                               byte_size => exml:xml_size(El)}
                         andalso CheckElFun(El)
             end,
    instrument_helper:assert_one(event_name(Dir), labels(), Filter).

%% Assert one or more generic XML stanza events without a JID
assert_events(Dir, CheckElFun) ->
    Filter = fun(M = #{element := El}) ->
                     maps:remove(element, M) =:= #{jid => undefined, count => 1,
                                                   byte_size => exml:xml_size(El)}
                         andalso CheckElFun(El)
             end,
    instrument_helper:assert(event_name(Dir), labels(), Filter).

event_name(out) -> xmpp_element_out;
event_name(in) -> xmpp_element_in.

group_name(Config) ->
    proplists:get_value(name, proplists:get_value(tc_group_properties, Config)).

labels() -> #{connection_type => c2s, host_type => host_type()}.

%% SM instrumentation events

assert_message_bounced_event(Sender, Recipient) ->
    FromJid = jid:from_binary(escalus_utils:get_jid(Sender)),
    ToJid = jid:from_binary(escalus_utils:get_jid(Recipient)),
    instrument_helper:assert_one(
      sm_message_bounced, #{host_type => host_type()},
      fun(#{count := 1, from_jid := From, to_jid := To}) ->
               From =:= FromJid andalso To =:= ToJid
      end).
