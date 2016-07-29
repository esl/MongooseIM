%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
%%
%% Author: Joseph Yiasemides <joseph.yiasemides@erlang-solutions.com>
%% Description: Test HTTP Administration API for Mult-user Chat (MUC)
%%==============================================================================

-module(muc_http_api_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, positive}].

groups() ->
    [{positive, [shuffle], success_response()}].

success_response() ->
    [
     create_room,
     invite_online_user_to_room,
     %% invite_offline_user_to_room, %% TO DO.
     send_message_to_room
    ].


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    muc_helper:load_muc(muc_helper:muc_host()),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    muc_helper:unload_muc(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

create_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Domain = <<"localhost">>,
        Path = <<"/muc/domain", $/, Domain/binary>>,
        Name = <<"wonderland">>,
        Body = #{name => Name,
                 owner => escalus_utils:get_jid(Alice),
                 nick => <<"ali">>},
        {{<<"201">>, _}, <<"">>} = rest_helper:post(Path, Body),
        escalus:send(Alice, stanza_get_rooms()),
        Stanza = escalus:wait_for_stanza(Alice),
        has_room(muc_helper:room_address(Name), Stanza),
        escalus:assert(is_stanza_from, [muc_helper:muc_host()], Stanza)
    end).

invite_online_user_to_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Path = <<"/muc/domain/localhost/name/wonderland">>,
        Reason = <<"I think you'll like this room!">>,
        Body = #{sender => escalus_utils:get_jid(Alice),
                 recipient => escalus_utils:get_jid(Bob),
                 reason => Reason},
        {{<<"200">>, _}, <<"">>} = rest_helper:putt(Path, Body),
        Stanza = escalus:wait_for_stanza(Bob),
        is_direct_invitation(Stanza),
        direct_invite_has_reason(Stanza, Reason)
    end).

send_message_to_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Alice creates a MUC room.
        muc_helper:start_room([], escalus_users:get_user_by_name(alice),
                              <<"wonderland">>, <<"ali">>, []),
        %% Bob enters the room.
        escalus:send(Bob,
                     muc_helper:stanza_muc_enter_room(<<"wonderland">>,
                                                      <<"bobcat">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Parameters for this test.
        Domain = <<"localhost">>,
        Name = <<"wonderland">>,
        Path = <<"/muc/domain", $/, Domain/binary, $/, "name", $/, Name/binary>>,
        Message = <<"Greetings!">>,
        Body = #{sender => escalus_utils:get_jid(Bob),
                 message => Message},
        %% The HTTP call in question.
        {{<<"200">>, _}, <<"">>} = rest_helper:putt(Path, Body),
        Got = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Got),
        Message = exml_query:path(Got, [{element, <<"body">>}, cdata])
    end).

%%--------------------------------------------------------------------
%% Ancillary (adapted from the MUC suite)
%%--------------------------------------------------------------------

stanza_get_rooms() ->
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        muc_helper:muc_host()).

has_room(JID, #xmlel{children = [ #xmlel{children = Rooms} ]}) ->
    RoomPred = fun(Item) ->
        exml_query:attr(Item, <<"jid">>) == JID
    end,
    true = lists:any(RoomPred, Rooms).

is_direct_invitation(Stanza) ->
    escalus:assert(is_message, Stanza),
    ?NS_JABBER_X_CONF = exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"xmlns">>}]).

direct_invite_has_reason(Stanza, Reason) ->
    Reason = exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"reason">>}]).
