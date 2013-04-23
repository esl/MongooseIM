%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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
-module(mam_SUITE).
%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Tests
-export([simple_archive_request/1,
         muc_archive_request/1,
         range_archive_request/1,
         limit_archive_request/1,
         pagination/1]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml_stream.hrl").
-define(MUC_HOST, <<"muc.localhost">>).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, mam}].

groups() ->
    [{mam, [sequence], [simple_archive_request,
                        range_archive_request,
                        limit_archive_request,
                        muc_archive_request,
                        pagination]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(muc_archive_request=CaseName, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    Config2 = escalus:init_per_testcase(CaseName, Config1),
    [Alice | _] = ?config(escalus_users, Config2),
    start_room(Config2, Alice, RoomName, RoomNick, [{persistent, true}]);

init_per_testcase(CaseName, Config) ->
    Config1 = escalus:create_users(Config),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(muc_archive_request=CaseName, Config) ->
    destroy_room(Config),
    Config1 = escalus:delete_users(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName, Config) ->
    Config1 = escalus:delete_users(Config),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Adhoc tests
%%--------------------------------------------------------------------

%% Querying the archive for messages
simple_archive_request(Config) ->
    F = fun(Alice, Bob) ->
        %% Alice sends "OH, HAI!" to Bob
        %% {xmlelement,<<"message">>,
        %%  [{<<"from">>,<<"alice@localhost/res1">>},
        %%   {<<"to">>,<<"bob@localhost/res1">>},
        %%   {<<"xml:lang">>,<<"en">>},
        %%   {<<"type">>,<<"chat">>}],
        %%   [{xmlelement,<<"body">>,[],[{xmlcdata,<<"OH, HAI!">>}]}]}
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:send(Alice, stanza_archive_request(<<"q1">>)),
        Reply = escalus:wait_for_stanza(Alice),
        ct:pal("Reply ~p.", [Reply]),
        ok
        end,
    escalus:story(Config, [1, 1], F).


muc_archive_request(Config) ->
    F = fun(Alice, Bob) ->
        Room = ?config(room, Config),
        RoomAddr = room_address(Room),
        Msg = <<"Hi, Bob!">>,
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),
           
        %% Alice sends to the chat room.
		escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Msg)),
        %% Alice requests archive.
        escalus:send(Alice, stanza_to_room(stanza_archive_request(<<"q1">>), Room)),
        Reply = escalus:wait_for_stanza(Alice),
        ct:pal("Reply ~p.", [Reply]),
        ok
        end,
    escalus:story(Config, [1, 1], F).


%% @doc Querying the archive for all messages in a certain timespan.
range_archive_request(Config) ->
    F = fun(Alice) ->
        %% Send
        %% <iq type='get'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%     <start>2010-06-07T00:00:00Z</start>
        %%     <end>2010-07-07T13:23:54Z</end>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_date_range_archive_request()),
        Reply = escalus:wait_for_stanza(Alice),
        ct:pal("Reply ~p.", [Reply]),
        ok
        end,
    escalus:story(Config, [1], F).

%% @doc A query using Result Set Management.
limit_archive_request(Config) ->
    F = fun(Alice) ->
        %% Send
        %% <iq type='get' id='q29302'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%       <start>2010-08-07T00:00:00Z</start>
        %%       <set xmlns='http://jabber.org/protocol/rsm'>
        %%          <limit>10</limit>
        %%       </set>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_limit_archive_request()),
        Reply = escalus:wait_for_stanza(Alice),
        ct:pal("Reply ~p.", [Reply]),
        ok
        end,
    escalus:story(Config, [1], F).

pagination(Config) ->
    F = fun(Alice, Bob) ->
        [escalus:send(Alice,
                      escalus_stanza:chat_to(Bob, generate_message_text(N)))
         || N <- lists:seq(1, 15)],
        %% Wait 100 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 15, 5000),
        escalus:send(Alice, stanza_page_archive_request(5, <<"page_q">>)),
        Reply = escalus:wait_for_stanzas(Alice, 6, 5000),
        ct:pal("Reply ~p.", [Reply]),
        ok
        end,
    escalus:story(Config, [1, 1], F).

generate_message_text(N) when is_integer(N) ->
    <<"Message #", (list_to_binary(integer_to_list(N)))/binary>>.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

nick(User) -> escalus_utils:get_username(User).

mam_ns_string() -> "urn:xmpp:mam:tmp".
mam_ns_binary() -> <<"urn:xmpp:mam:tmp">>.
muc_ns_binary() -> <<"http://jabber.org/protocol/muc">>.


%% An optional 'queryid' attribute allows the client to match results to
%% a certain query.
stanza_archive_request(QueryId) ->
    escalus_stanza:iq(<<"get">>, [#xmlelement{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}, {<<"queryid">>, QueryId}]
    }]).

stanza_date_range_archive_request() ->
    Start = #xmlelement{name = <<"start">>,
                        children = #xmlcdata{content = "2010-06-07T00:00:00Z"}},
    End   = #xmlelement{name = <<"end">>,
                        children = #xmlcdata{content = "2010-07-07T13:23:54Z"}},
    escalus_stanza:iq(<<"get">>, [#xmlelement{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}],
       children = [Start, End]
    }]).

stanza_limit_archive_request() ->
    Start = #xmlelement{name = <<"start">>,
                        children = #xmlcdata{content = "2010-08-07T00:00:00Z"}},
    Limit = #xmlelement{name = <<"limit">>,
                        children = #xmlcdata{content = "10"}},
    Set   = #xmlelement{name = <<"set">>,
                        children = [Limit]},
    escalus_stanza:iq(<<"get">>, [#xmlelement{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}],
       children = [Start, Set]
    }]).

stanza_page_archive_request(Count, QueryId) ->
    Limit = #xmlelement{name = <<"limit">>,
                        children = #xmlcdata{content = integer_to_list(Count)}},
    Set   = #xmlelement{name = <<"set">>,
                        children = [Limit]},
    escalus_stanza:iq(<<"get">>, [#xmlelement{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}, {<<"queryid">>, QueryId}],
       children = [Set]
    }]).

%%--------------------------------------------------------------------
%% Helpers (muc)
%%--------------------------------------------------------------------

generate_rpc_jid({_,User}) ->
    {username, Username} = lists:keyfind(username, 1, User),
    {server, Server} = lists:keyfind(server, 1, User),
    %% esl-ejabberd uses different record to store jids
     %JID = <<Username/binary, "@", Server/binary, "/rpc">>,
     %{jid, JID, Username, Server, <<"rpc">>}.
    {jid, Username, Server, <<"rpc">>, Username, Server, <<"rpc">>}.

start_room(Config, User, Room, Nick, Opts) ->
    From = generate_rpc_jid(User),
    escalus_ejabberd:rpc(mod_muc, create_instant_room,
        [<<"localhost">>, Room, From, Nick,
            Opts]),
    [{nick, Nick}, {room, Room} | Config].

destroy_room(Config) ->
    case escalus_ejabberd:rpc(ets, lookup, [muc_online_room,
        {?config(room, Config), <<"muc.localhost">>}]) of
        [{_,_,Pid}|_] -> gen_fsm:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

stanza_muc_enter_room(Room, Nick) ->
    Elem = #xmlelement{ name = <<"x">>,
                        attrs=[{<<"xmlns">>, muc_ns_binary()}]},
    stanza_to_room(escalus_stanza:presence(<<"available">>, [Elem]),
                   Room, Nick).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

room_address(Room) when is_binary(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) when is_binary(Room), is_binary(Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.
