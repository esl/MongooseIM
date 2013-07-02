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
-export([mam_service_discovery/1,
         muc_service_discovery/1,
         simple_archive_request/1,
         muc_archive_request/1,
         range_archive_request/1,
         limit_archive_request/1,
         prefs_set_request/1,
         pagination_first5/1,
         pagination_last5/1,
         pagination_before10/1,
         pagination_after10/1,
         pagination_empty_rset/1,
         archived/1,
         strip_archived/1,
         policy_violation/1]).

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

-record(result_iq, {
    from            :: binary(),
    to              :: binary(),
    id              :: binary(),
    first           :: binary() | undefined,
    first_index     :: non_neg_integer() | undefined,
    last            :: binary() | undefined,
    count           :: non_neg_integer()
}).

-record(error_iq, {
    id              :: binary(),
    type            :: binary(),
    error_type      :: binary(),
    condition       :: binary(),
    text            :: binary()
}).

-record(prefs_result_iq, {
    default_mode    :: binary() | undefined,
    always_jids = [] :: [binary()],
    never_jids  = [] :: [binary()]
}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, mam}, {group, muc}, {group, rsm}, {group, archived},
     {group, policy_violation}].

groups() ->
    [
     {mam, [], [mam_service_discovery,
                simple_archive_request,
                range_archive_request,
                limit_archive_request,
                prefs_set_request]},
     {archived, [], [archived, strip_archived]},
     {policy_violation, [], [policy_violation]},
     {muc, [], [muc_service_discovery,
                muc_archive_request]},
     {rsm, [], [pagination_first5,
                pagination_last5,
                pagination_before10,
                pagination_after10,
                pagination_empty_rset]}].

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
    Pid = self(),
    F = fun(Alice, Bob) ->
        %% Alice sends messages to Bob.
        [escalus:send(Alice,
                      escalus_stanza:chat_to(Bob, generate_message_text(N)))
         || N <- lists:seq(1, 15)],
        %% Bob is waiting for 15 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 15, 5000),
        %% Get whole history.
        escalus:send(Alice, stanza_archive_request(<<"all_messages">>)),
        AllMessages = escalus:wait_for_stanzas(Alice, 15, 5000),
        ParsedMessages = [parse_forwarded_message(M) || M <- AllMessages],
        Pid ! {parsed_messages, ParsedMessages},
        ok
        end,
    Config2 = escalus:init_per_testcase(pre_rsm, Config1),
    ok = escalus:story(Config2, [1, 1], F),
    ParsedMessages = receive {parsed_messages, PM} -> PM
                     after 5000 -> error(receive_timeout) end,

    escalus:end_per_testcase(pre_rsm, Config2),
    [{all_messages, ParsedMessages}|Config1]; %% it is right.
init_per_group(policy_violation, Config) ->
    escalus:create_users(Config);
init_per_group(mam, Config) ->
    escalus:create_users(Config);
init_per_group(archived, Config) ->
    escalus:create_users(Config).

end_per_group(muc, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);
end_per_group(_, Config) ->
    escalus:delete_users(Config).

init_per_testcase(archived, Config) ->
    escalus:init_per_testcase(archived, clean_archive(Config));
init_per_testcase(strip_archived, Config) ->
    escalus:init_per_testcase(strip_archived, clean_archive(Config));
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
        %% {xmlel,<<"message">>,
        %%  [{<<"from">>,<<"alice@localhost/res1">>},
        %%   {<<"to">>,<<"bob@localhost/res1">>},
        %%   {<<"xml:lang">>,<<"en">>},
        %%   {<<"type">>,<<"chat">>}],
        %%   [{xmlel,<<"body">>,[],[{xmlcdata,<<"OH, HAI!">>}]}]}
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:send(Alice, stanza_archive_request(<<"q1">>)),
        Reply = escalus:wait_for_stanza(Alice),
        ct:pal("Reply ~p.", [Reply]),
        ok
        end,
    escalus:story(Config, [1, 1], F).


archived(Config) ->
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob receives a message.
        Msg = escalus:wait_for_stanza(Bob),
        Arc = exml_query:subelement(Msg, <<"archived">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),

        ?assertEqual(By, escalus_client:short_jid(Bob)),

        %% Bob calls archive.
        escalus:send(Bob, stanza_archive_request(<<"q1">>)),
        [ArcMsg, _ArcIQ] = escalus:wait_for_stanzas(Bob, 2, 5000),
        #forwarded_message{result_id=ArcId} = parse_forwarded_message(ArcMsg),
        ?assertEqual(Id, ArcId),
        ok
        end,
    escalus:story(Config, [1, 1], F).

strip_archived(Config) ->
    F = fun(Alice, Bob) ->
        %% Archive must be empty.
        %% Alice sends "OH, HAI!" to Bob.
        escalus:send(Alice,
                     append_subelem(escalus_stanza:chat_to(Bob, <<"OH, HAI!">>),
                                    archived_elem(escalus_client:short_jid(Bob),
                                                  <<"fake-id">>))),

        %% Bob receives a message.
        Msg = escalus:wait_for_stanza(Bob),
        Arc = exml_query:subelement(Msg, <<"archived">>),
        %% JID of the archive (i.e. where the client would send queries to)
        By  = exml_query:attr(Arc, <<"by">>),
        %% Attribute giving the message's UID within the archive.
        Id  = exml_query:attr(Arc, <<"id">>),

        ?assertEqual(escalus_client:short_jid(Bob), By),

        %% Bob calls archive.
        escalus:send(Bob, stanza_archive_request(<<"q1">>)),
        [ArcMsg, _ArcIQ] = escalus:wait_for_stanzas(Bob, 2, 5000),
        #forwarded_message{result_id=ArcId} = parse_forwarded_message(ArcMsg),
        ?assertEqual(ArcId, Id),
        ok
        end,
    escalus:story(Config, [1, 1], F).

%% To conserve resources, a server MAY place a reasonable limit on how many
%% stanzas may be pushed to a client in one request.
%% If a query returns a number of stanzas greater than this limit and
%% the client did not specify a limit using RSM then the server should
%% return a policy-violation error to the client. 
policy_violation(Config) ->
    F = fun(Alice, Bob) ->
        %% Alice sends messages to Bob.
        %% WARNING: are we sending too fast?
        [escalus:send(Alice,
                      escalus_stanza:chat_to(Bob, generate_message_text(N)))
         || N <- lists:seq(1, 51)],
        %% Bob is waiting for 51 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 51, 5000),
        %% Get whole history (queryid is "will_fail", id is random).
        escalus:send(Alice, stanza_archive_request(<<"will_fail">>)),
        ErrorIQ = escalus:wait_for_stanza(Alice),
        ct:pal("ErrorIQ ~p", [ErrorIQ]),
        #error_iq{condition = Condition} = parse_error_iq(ErrorIQ),
        ?assertEqual(<<"policy-violation">>, Condition),
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

        %% Bob received Alice's presence.
        escalus:wait_for_stanza(Bob),

        %% Bob received his presence.
        escalus:wait_for_stanza(Bob),

        %% Bob received empty message with body and subject.
        %% This message will be archived (by bob@localhost).
        escalus:wait_for_stanza(Bob),

        %% Bob received the message "Hi, Bob!".
        %% This message will be archived (by bob@localhost).
        BobMsg = escalus:wait_for_stanza(Bob),
        ct:pal("Msg ~p.", [BobMsg]),
        %% TODO: check, that the message was logged into the room.
%       Arc = exml_query:subelement(Msg, <<"archived">>),

        %% Bob requests the room's archive.
        escalus:send(Bob, stanza_to_room(stanza_archive_request(<<"q1">>), Room)),
        ArcMsg = escalus:wait_for_stanza(Bob),
        ArcRes = escalus:wait_for_stanza(Bob),
        ct:pal("ArcMsg ~p.", [ArcMsg]),
        ct:pal("ArcRes ~p.", [ArcRes]),
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

pagination_empty_rset(Config) ->
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=0},
        escalus:send(Alice, stanza_page_archive_request(<<"empty_rset">>, RSM)),
        IQ = escalus:wait_for_stanza(Alice, 5000),
        ParsedIQ = parse_result_iq(IQ),
        ct:pal("IQ: ~p~nParsedIQ: ~p~n", [IQ, ParsedIQ]),
        ?assertEqual(ParsedIQ#result_iq.count, 15),
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
        ?assertEqual([generate_message_text(N) || N <- lists:seq(11, 15)],
                     [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
        end,
    escalus:story(Config, [1], F).

pagination_before10(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config)},
        escalus:send(Alice, stanza_page_archive_request(<<"before10">>, RSM)),
        Messages = escalus:wait_for_stanzas(Alice, 5, 5000),
        IQ = escalus:wait_for_stanzas(Alice, 1, 5000),
        ParsedMessages = [parse_forwarded_message(M) || M <- Messages],
        ct:pal("IQ: ~p~nMessages: ~p~nParsed messages: ~p~n",
               [IQ, Messages, ParsedMessages]),
        %% Compare body of the messages.
        ?assertEqual([generate_message_text(N) || N <- lists:seq(5, 9)],
                     [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
        end,
    escalus:story(Config, [1], F).

pagination_after10(Config) ->
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction='after', id=message_id(10, Config)},
        escalus:send(Alice, stanza_page_archive_request(<<"after10">>, RSM)),
        Messages = escalus:wait_for_stanzas(Alice, 5, 5000),
        IQ = escalus:wait_for_stanzas(Alice, 1, 5000),
        ct:pal("IQ: ~p~nMessages: ~p~n", [IQ, Messages]),
        ParsedMessages = [parse_forwarded_message(M) || M <- Messages],
        ct:pal("Parsed messages: ~p~n", [ParsedMessages]),
        %% Compare body of the messages.
        ?assertEqual([generate_message_text(N) || N <- lists:seq(11, 15)],
                     [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
        end,
    escalus:story(Config, [1], F).

generate_message_text(N) when is_integer(N) ->
    <<"Message #", (list_to_binary(integer_to_list(N)))/binary>>.



prefs_set_request(Config) ->
    F = fun(Alice) ->
        %% Send
        %% 
        %% <iq type='set' id='juliet2'>
        %%   <prefs xmlns='urn:xmpp:mam:tmp' default="roster">
        %%     <always>
        %%       <jid>romeo@montague.net</jid>
        %%     </always>
        %%     <never>
        %%       <jid>montague@montague.net</jid>
        %%     </never>
        %%   </prefs>
        %% </iq>
        escalus:send(Alice, stanza_prefs_set_request(<<"roster">>,
                                                     [<<"romeo@montague.net">>],
                                                     [<<"montague@montague.net">>])),
        ReplySet = escalus:wait_for_stanza(Alice),
        ct:pal("ReplySet ~p.", [ReplySet]),

        escalus:send(Alice, stanza_prefs_get_request()),
        ReplyGet = escalus:wait_for_stanza(Alice),
        ct:pal("ReplyGet ~p.", [ReplyGet]),
        ResultIQ1 = parse_prefs_result_iq(ReplySet),
        ResultIQ2 = parse_prefs_result_iq(ReplyGet),
        ?assertEqual(ResultIQ1, ResultIQ2),
        ok
        end,
    escalus:story(Config, [1], F).

mam_service_discovery(Config) ->
    F = fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:disco_info(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        ct:pal("Stanza ~p.", [Stanza]),
        escalus:assert(is_iq_result, Stanza),
        escalus:assert(has_feature, [mam_ns_binary()], Stanza),
        ok
        end,
    escalus:story(Config, [1], F).

%% Check, that MUC is supported.
muc_service_discovery(Config) ->
    F = fun(Alice) ->
        Domain = escalus_config:get_config(ejabberd_domain, Config),
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        %% If this fails, than check your config.
        %% `{mod_disco, [{extra_domains, [<<"muc.localhost">>]}]}' must be there.
        escalus:assert(has_service, [?MUC_HOST], Stanza),
        escalus:assert(is_stanza_from, [Domain], Stanza),
        ok
        end,
    escalus:story(Config, [1], F).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

nick(User) -> escalus_utils:get_username(User).

mam_ns_binary() -> <<"urn:xmpp:mam:tmp">>.
muc_ns_binary() -> <<"http://jabber.org/protocol/muc">>.


%% An optional 'queryid' attribute allows the client to match results to
%% a certain query.
stanza_archive_request(QueryId) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}, {<<"queryid">>, QueryId}]
    }]).

stanza_date_range_archive_request() ->
    Start = #xmlel{name = <<"start">>,
                   children = #xmlcdata{content = "2010-06-07T00:00:00Z"}},
    End   = #xmlel{name = <<"end">>,
                   children = #xmlcdata{content = "2010-07-07T13:23:54Z"}},
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}],
       children = [Start, End]
    }]).

stanza_limit_archive_request() ->
    Start = #xmlel{name = <<"start">>,
                   children = #xmlcdata{content = "2010-08-07T00:00:00Z"}},
    Limit = #xmlel{name = <<"limit">>, %% according XEP-0313, not XEP-0059
                   children = #xmlcdata{content = "10"}},
    Set   = #xmlel{name = <<"set">>,
                   children = [Limit]},
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}],
       children = [Start, Set]
    }]).


stanza_page_archive_request(QueryId,
                            #rsm_in{max=Max, direction=Direction, id=Id, index=Index}) ->
    LimitEl = [#xmlel{name = <<"max">>,
                      children = #xmlcdata{content = integer_to_list(Max)}}
               || is_integer(Max)],
    IndexEl = [#xmlel{name = <<"index">>,
                      children = #xmlcdata{content = integer_to_list(Index)}}
               || is_integer(Index)],
    BorderEl = [#xmlel{name = atom_to_binary(Direction, latin1),
                       children = case Id of
                                   undefined -> [];
                                    _ -> #xmlcdata{content = Id}
                                  end}
                || Direction =/= undefined],
    SetEl = #xmlel{name = <<"set">>,
                        children = lists:merge([LimitEl, IndexEl, BorderEl])},
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"query">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}, {<<"queryid">>, QueryId}],
       children = [SetEl]
    }]).

%% ----------------------------------------------------------------------
%% PREFERENCE QUERIES

stanza_prefs_set_request(DefaultMode, AlwaysJIDs, NewerJIDs) ->
    AlwaysEl = #xmlel{name = <<"always">>,
                      children = encode_jids(AlwaysJIDs)},
    NewerEl  = #xmlel{name = <<"never">>,
                      children = encode_jids(NewerJIDs)},
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"prefs">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}]
               ++ [{<<"default">>, DefaultMode} || is_def(DefaultMode)],
       children = [AlwaysEl, NewerEl]
    }]).

stanza_prefs_get_request() ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"prefs">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()}]
    }]).

encode_jids(JIDs) ->
    [#xmlel{name = <<"jid">>,
            children = [#xmlcdata{content = JID}]}
     || JID <- JIDs].

%% ----------------------------------------------------------------------
%% PARSING RESPONDS

parse_forwarded_message(#xmlel{name = <<"message">>,
                               attrs = Attrs, children = Children}) ->
    M = #forwarded_message{
        from = proplists:get_value(<<"from">>, Attrs),
        to   = proplists:get_value(<<"to">>, Attrs)},
    lists:foldl(fun 'parse_children[message]'/2, M, Children).

'parse_children[message]'(#xmlel{name = <<"result">>,
                                 attrs = Attrs,
                                 children = Children}, M) ->
    M1 = M#forwarded_message{
        result_queryid = proplists:get_value(<<"queryid">>, Attrs),
        result_id      = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun 'parse_children[message/result]'/2, M1, Children).

'parse_children[message/result]'(#xmlel{name = <<"forwarded">>,
                                        children = Children}, M) ->
    lists:foldl(fun 'parse_children[message/result/forwarded]'/2, M, Children).
    

'parse_children[message/result/forwarded]'(#xmlel{name = <<"delay">>,
                                                  attrs = Attrs}, M) ->
    M#forwarded_message{
        delay_from  = proplists:get_value(<<"from">>, Attrs),
        delay_stamp = proplists:get_value(<<"stamp">>, Attrs)};
'parse_children[message/result/forwarded]'(#xmlel{name = <<"message">>,
                                                  attrs = Attrs,
                                                  children = Children}, M) ->
    M1 = M#forwarded_message{
        message_to   = proplists:get_value(<<"to">>, Attrs),
        message_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun 'parse_children[message/result/forwarded/message]'/2,
                M1, Children).

'parse_children[message/result/forwarded/message]'(#xmlel{name = <<"body">>,
        children = [{xmlcdata, Body}]}, M) ->
    M#forwarded_message{message_body = Body};
%% Parse `<archived />' here.
'parse_children[message/result/forwarded/message]'(_, M) ->
    M.

%% Num is 1-based.
message_id(Num, Config) ->
    AllMessages = proplists:get_value(all_messages, Config),
    #forwarded_message{result_id=Id} = lists:nth(Num, AllMessages),
    Id.


%% @doc Result query iq.
%%
%% [{xmlel,<<"iq">>,
%%     [{<<"from">>,<<"alice@localhost">>},
%%      {<<"to">>,<<"alice@localhost/res1">>},
%%      {<<"id">>,<<"387862024ce65379b049e19751e4309e">>},
%%      {<<"type">>,<<"result">>}],
%%     []}]
%%
%%
%%  [{xmlel,<<"iq">>,
%%       [{<<"from">>,<<"alice@localhost">>},
%%        {<<"to">>,<<"alice@localhost/res1">>},
%%        {<<"id">>,<<"c256a18c4b720465e215a81362d41eb7">>},
%%        {<<"type">>,<<"result">>}],
%%       [{xmlel,<<"query">>,
%%            [{<<"xmlns">>,<<"urn:xmpp:mam:tmp">>}],
%%            [{xmlel,<<"set">>,
%%                 [{<<"xmlns">>,<<"http://jabber.org/protocol/rsm">>}],
%%                 [{xmlel,<<"first">>,
%%                      [{<<"index">>,<<"10">>}],
%%                      [{xmlcdata,<<"103439">>}]},
%%                  {xmlel,<<"last">>,[],[{xmlcdata,<<"103447">>}]},
%%                  {xmlel,<<"count">>,[],[{xmlcdata,<<"15">>}]}]}]}]}]
parse_result_iq(#xmlel{name = <<"iq">>,
                       attrs = Attrs, children = Children}) ->
    IQ = #result_iq{
        from = proplists:get_value(<<"from">>, Attrs),
        to   = proplists:get_value(<<"to">>, Attrs),
        id   = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun 'parse_children[iq]'/2, IQ, Children).

'parse_children[iq]'(#xmlel{name = <<"query">>, children = Children},
                     IQ) ->
    lists:foldl(fun 'parse_children[iq/query]'/2, IQ, Children).


'parse_children[iq/query]'(#xmlel{name = <<"set">>,
                                  children = Children},
                           IQ) ->
    lists:foldl(fun 'parse_children[iq/query/set]'/2, IQ, Children).

'parse_children[iq/query/set]'(#xmlel{name = <<"first">>,
                                      attrs = Attrs,
                                      children = [{xmlcdata, First}]},
                               IQ) ->
    Index = case proplists:get_value(<<"index">>, Attrs) of
                undefined -> undefined;
                X -> list_to_integer(binary_to_list(X))
            end,
    IQ#result_iq{first_index = Index, first = First};
'parse_children[iq/query/set]'(#xmlel{name = <<"last">>,
                                      children = [{xmlcdata, Last}]},
                               IQ) ->
    IQ#result_iq{last = Last};
'parse_children[iq/query/set]'(#xmlel{name = <<"count">>,
                                      children = [{xmlcdata, Count}]},
                               IQ) ->
    IQ#result_iq{count = list_to_integer(binary_to_list(Count))};
'parse_children[iq/query/set]'(_, IQ) -> IQ.

is_def(X) -> X =/= undefined.



parse_prefs_result_iq(#xmlel{name = <<"iq">>, children = Children}) ->
    IQ = #prefs_result_iq{},
    lists:foldl(fun 'parse_children[prefs_iq]'/2, IQ, Children).

'parse_children[prefs_iq]'(#xmlel{name = <<"prefs">>,
                                  attrs = Attrs, children = Children},
                           IQ) ->
    DefaultMode = proplists:get_value(<<"default">>, Attrs),
    IQ1 = IQ#prefs_result_iq{default_mode = DefaultMode},
    lists:foldl(fun 'parse_children[prefs_iq/prefs]'/2, IQ1, Children).


'parse_children[prefs_iq/prefs]'(#xmlel{name = <<"always">>,
                                        children = Children},
                                 IQ) ->
    IQ#prefs_result_iq{always_jids = lists:sort(parse_jids(Children))};
'parse_children[prefs_iq/prefs]'(#xmlel{name = <<"never">>,
                                        children = Children},
                                 IQ) ->
    IQ#prefs_result_iq{never_jids = lists:sort(parse_jids(Children))}.


parse_jids(Els) ->
    [JID || #xmlel{name = <<"jid">>, children = [{xmlcdata, JID}]} <- Els].

%% <iq type='error' id='q29302'>
%%   <error type='modify'>
%%     <policy-violation xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
%%     <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Too many results</text>
%%   </error>
%% </iq>
parse_error_iq(#xmlel{name = <<"iq">>,
                      attrs = Attrs, children = Children}) ->
    
    IQ = #error_iq{
        type = proplists:get_value(<<"type">>, Attrs),
        id   = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun 'parse_children[error_iq]'/2, IQ, Children).


'parse_children[error_iq]'(#xmlel{name = <<"error">>,
                                  attrs = Attrs, children = Children}, IQ) ->
    IQ1 = IQ#error_iq{
        error_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun 'parse_children[error_iq/error]'/2, IQ1, Children);
'parse_children[error_iq]'(_, IQ) ->
    IQ.

'parse_children[error_iq/error]'(#xmlel{name = <<"text">>,
                                 children = [{xmlcdata, Text}]}, IQ) ->
    IQ#error_iq{text = Text};
'parse_children[error_iq/error]'(#xmlel{name = Condition}, IQ) ->
    IQ#error_iq{condition = Condition};
'parse_children[error_iq/error]'(_, IQ) ->
    IQ.

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
        [<<"localhost">>, Room, From, Nick, Opts]),
    [{nick, Nick}, {room, Room} | Config].

destroy_room(Config) ->
    RoomName = ?config(room, Config),
    escalus_ejabberd:rpc(mod_mam_muc, delete_archive, 
        [RoomName, ?MUC_HOST]),
    case escalus_ejabberd:rpc(ets, lookup, [muc_online_room,
        {RoomName, ?MUC_HOST}]) of
        [{_,_,Pid}|_] -> gen_fsm:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

stanza_muc_enter_room(Room, Nick) ->
    Elem = #xmlel{ name = <<"x">>,
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


append_subelem(Elem=#xmlel{children=Cs}, SubElem) ->
    Elem#xmlel{children=Cs ++ [SubElem]}.

archived_elem(By, Id) ->
    #xmlel{name = <<"archived">>,
           attrs = [{<<"by">>, By}, {<<"id">>, Id}]}.

clean_archive(Config) ->
    [begin
     [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
     escalus_ejabberd:rpc(mod_mam, delete_archive, [Username, Server])
     end
     || {_, UserSpec} <- escalus_users:get_users(all)],
    Config.
