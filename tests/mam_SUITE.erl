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
         pagination_first5/1,
         pagination_last5/1,
         pagination_before11/1,
         pagination_after11/1]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml_stream.hrl").
-define(MUC_HOST, <<"muc.localhost">>).
-record(rsm_in, {
        max :: non_neg_integer() | undefined,
        direction :: before | 'after' | undefined,
        id :: binary() | undefined,
        index ::non_neg_integer() | undefined
        }).

-record(forwarded_message, {
    from           :: binary() | undefined,
    to             :: binary() | undefined,
    result_queryid :: binary() | undefined,
    result_id      :: binary() | undefined,
    delay_from     :: binary() | undefined,
    delay_stamp    :: binary() | undefined,
    message_to     :: binary() | undefined,
    message_type   :: binary() | undefined,
    message_body   :: binary() | undefined
}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, mam}, {group, muc}, {group, rsm}].

groups() ->
    [{mam, [], [simple_archive_request,
                range_archive_request,
                limit_archive_request]},
     {muc, [], [muc_archive_request]},
     {rsm, [], [pagination_first5,
                pagination_last5,
                pagination_before11]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(muc, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, RoomName, RoomNick, [{persistent, true}]);
init_per_group(rsm, Config) ->
    Config1 = escalus:create_users(Config),
    ct:pal("Config1: ~p", [Config1]),
    F = fun(Alice, Bob) ->
        %% Alice sends messages to Bob.
        [escalus:send(Alice,
                      escalus_stanza:chat_to(Bob, generate_message_text(N)))
         || N <- lists:seq(1, 15)],
        %% Wait 15 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 15, 5000),
        ok
        end,
    Config2 = escalus:init_per_testcase(pre_rsm, Config1),
    escalus:story(Config2, [1, 1], F),
    escalus:end_per_testcase(pre_rsm, Config2),
    Config1; %% it is right.
init_per_group(mam, Config) ->
    escalus:create_users(Config).

end_per_group(muc, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);
end_per_group(_, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
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

pagination_first5(Config) ->
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        escalus:send(Alice, stanza_page_archive_request(<<"first5">>, RSM)),
        Messages = escalus:wait_for_stanzas(Alice, 5, 5000),
        IQ = escalus:wait_for_stanzas(Alice, 1, 5000),
        ParsedMessages = [parse_forwarded_message(M) || M <- Messages],
        ct:pal("IQ: ~p~nMessages: ~p~nParsed messages: ~p~n",
               [IQ, Messages, ParsedMessages]),
        %% Compare body of the messages.
        ?assertEqual([generate_message_text(N) || N <- lists:seq(1, 5)],
                     [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
        end,
    escalus:story(Config, [1], F).

pagination_last5(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before},
        escalus:send(Alice, stanza_page_archive_request(<<"last5">>, RSM)),
        Messages = escalus:wait_for_stanzas(Alice, 5, 5000),
        IQ = escalus:wait_for_stanzas(Alice, 1, 5000),
        ParsedMessages = [parse_forwarded_message(M) || M <- Messages],
        ct:pal("IQ: ~p~nMessages: ~p~nParsed messages: ~p~n",
               [IQ, Messages, ParsedMessages]),
        %% Compare body of the messages.
        ?assertEqual([generate_message_text(N) || N <- lists:seq(10, 15)],
                     [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
        end,
    escalus:story(Config, [1], F).

pagination_before11(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, id=generate_message_text(11)},
        escalus:send(Alice, stanza_page_archive_request(<<"before11">>, RSM)),
        Messages = escalus:wait_for_stanzas(Alice, 5, 5000),
        IQ = escalus:wait_for_stanzas(Alice, 1, 5000),
        ParsedMessages = [parse_forwarded_message(M) || M <- Messages],
        ct:pal("IQ: ~p~nMessages: ~p~nParsed messages: ~p~n",
               [IQ, Messages, ParsedMessages]),
        %% Compare body of the messages.
        ?assertEqual([generate_message_text(N) || N <- lists:seq(5, 10)],
                     [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
        end,
    escalus:story(Config, [1], F).

pagination_after11(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction='after', id=generate_message_text(11)},
        escalus:send(Alice, stanza_page_archive_request(<<"after11">>, RSM)),
        Messages = escalus:wait_for_stanzas(Alice, 3, 5000),
        IQ = escalus:wait_for_stanzas(Alice, 1, 5000),
        ParsedMessages = [parse_forwarded_message(M) || M <- Messages],
        ct:pal("IQ: ~p~nMessages: ~p~nParsed messages: ~p~n",
               [IQ, Messages, ParsedMessages]),
        %% Compare body of the messages.
        ?assertEqual([generate_message_text(N) || N <- lists:seq(12, 15)],
                     [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
        end,
    escalus:story(Config, [1], F).

generate_message_text(N) when is_integer(N) ->
    <<"Message #", (list_to_binary(integer_to_list(N)))/binary>>.


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

nick(User) -> escalus_utils:get_username(User).

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


stanza_page_archive_request(QueryId,
                            #rsm_in{max=Max, direction=Direction, id=Id, index=Index}) ->
    LimitEl = [#xmlelement{name = <<"max">>,
                           children = #xmlcdata{content = integer_to_list(Max)}}
               || is_integer(Max)],
    IndexEl = [#xmlelement{name = <<"index">>,
                           children = #xmlcdata{content = integer_to_list(Index)}}
               || is_integer(Index)],
    BorderEl = [#xmlelement{name = atom_to_binary(Direction, latin1),
                            children = case Id of
                                        undefined -> [];
                                         _ -> #xmlcdata{content = Id}
                                       end}
                || Direction =/= undefined],
    SetEl = #xmlelement{name = <<"set">>,
                        children = lists:merge([LimitEl, IndexEl, BorderEl])},
    escalus_stanza:iq(<<"get">>, [#xmlelement{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}, {<<"queryid">>, QueryId}],
       children = [SetEl]
    }]).

%% Build a record from the term:
%%  {xmlelement,<<"message">>,
%%     [{<<"from">>,<<"alice@localhost">>},
%%      {<<"to">>,<<"alice@localhost/res1">>}],
%%     [{xmlelement,<<"result">>,
%%          [{<<"xmlns">>,<<"urn:xmpp:mam:tmp">>},
%%           {<<"queryid">>,<<"first5">>},
%%           {<<"id">>,<<"103689">>}],
%%          []},
%%      {xmlelement,<<"forwarded">>,
%%          [{<<"xmlns">>,<<"urn:xmpp:forward:0">>}],
%%          [{xmlelement,<<"delay">>,
%%               [{<<"xmlns">>,<<"urn:xmpp:delay">>},
%%                {<<"from">>,<<"alice@localhost/res1">>},
%%                {<<"stamp">>,<<"2013-04-25T16:34:55Z">>}],
%%               []},
%%           {xmlelement,<<"message">>,
%%               [{<<"xml:lang">>,<<"en">>},
%%                {<<"to">>,<<"bob@localhost/res1">>},
%%                {<<"type">>,<<"chat">>}],
%%               [{xmlelement,<<"body">>,[],
%%                    [{xmlcdata,<<"Message #1">>}]}]}]}]}
parse_forwarded_message(#xmlelement{name = <<"message">>,
                                    attrs = Attrs, children = Children}) ->
    M = #forwarded_message{
        from = proplists:get_value(<<"from">>, Attrs),
        to   = proplists:get_value(<<"to">>, Attrs)},
    lists:foldl(fun 'parse_children[message]'/2, M, Children).

'parse_children[message]'(#xmlelement{name = <<"result">>,
                                      attrs = Attrs}, M) ->
    M#forwarded_message{
        result_queryid = proplists:get_value(<<"queryid">>, Attrs),
        result_id      = proplists:get_value(<<"id">>, Attrs)};
'parse_children[message]'(#xmlelement{name = <<"forwarded">>,
                                      children = Children}, M) ->
    lists:foldl(fun 'parse_children[message/forwarded]'/2, M, Children).
    

'parse_children[message/forwarded]'(#xmlelement{name = <<"delay">>,
                                                attrs = Attrs}, M) ->
    M#forwarded_message{
        delay_from  = proplists:get_value(<<"from">>, Attrs),
        delay_stamp = proplists:get_value(<<"stamp">>, Attrs)};
'parse_children[message/forwarded]'(#xmlelement{name = <<"message">>,
                                                attrs = Attrs,
                                                children = Children}, M) ->
    M1 = M#forwarded_message{
        message_to   = proplists:get_value(<<"to">>, Attrs),
        message_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun 'parse_children[message/forwarded/message]'/2, M1, Children).

'parse_children[message/forwarded/message]'(#xmlelement{name = <<"body">>,
                                            children = [{xmlcdata, Body}]}, M) ->
    M#forwarded_message{message_body = Body}.
    
        
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

