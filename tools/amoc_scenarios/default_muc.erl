%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(muc).

-define(HOST, <<"localhost">>).

-behaviour(amoc_scenario).
-compile([{parse_transform, lager_transform}]).
-include_lib("exml/include/exml.hrl").
-export([start/1]).
-export([init/0]).
-export([send_forever/3]).

-include_lib("exml/include/exml.hrl").

-define(MUCHOST, <<"muc.localhost">>).
-define(NS_MUC, <<"http://jabber.org/protocol/muc">>).
-define(NS_MUC_ADMIN, <<"http://jabber.org/protocol/muc#admin">>).
-define(NS_MUC_OWNER, <<"http://jabber.org/protocol/muc#owner">>).
-define(NS_MUC_UNIQUE, <<"http://jabber.org/protocol/muc#unique">>).
-define(NS_MUC_USER, <<"http://jabber.org/protocol/muc#user">>).
-define(NS_DATA_FORMS, <<"jabber:x:data">>).
-define(USERS, 1000).
-define(ROOMS, 100).
-define(PER_ROOM, 10).
-define(SENDER_INDICATOR, 3).
-define(MESSAGES_SENT_AT_ONCE_COUNT, 3).

-define(MESSAGES_CT, [amoc, counters, messages_sent]).
-define(MESSAGES_REC, [amoc, counters, messages_received]).
-define(MESSAGE_TTD_CT, [amoc, times, message_ttd]).
-define(CREATE_ROOM_TIME, [amoc, times, create_room_time]).

-type bin_jid() :: binary().
-type bin_room_jid() :: binary().

-spec init() -> ok.
init() ->
    lager:info("init the scenario"),
    exometer:new(?MESSAGES_CT, spiral),
    exometer:new(?MESSAGES_REC, spiral),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGES_CT, [one, count], 10000),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGES_REC, [one, count], 10000),
    exometer:new(?MESSAGE_TTD_CT, histogram),
    exometer:new(?CREATE_ROOM_TIME, histogram),
    exometer_report:subscribe(exometer_report_graphite, ?CREATE_ROOM_TIME, [mean, min, max, median, 95, 99, 999], 10000),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGE_TTD_CT, [mean, min, max, median, 95, 99, 999], 10000),
    ok.

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, XMPPToken, Res) ->
    Server = pick_server(),
    [ {username, ProfileId},
      {server, ?HOST},
      {password, XMPPToken},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ] ++ Server.

-spec username(integer()) -> binary().
username(Id) ->
    BinId = integer_to_binary(Id),
    <<"user_", BinId/binary>>.

-spec jid(amoc_scenario:user_id()) -> bin_jid().
jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

-spec get_my_room(amoc_scenario:user_id()) -> bin_room_jid().
get_my_room(Id) ->
    RoomId = (Id - 1 ) div ?PER_ROOM + 1,
    Bin = integer_to_binary(RoomId),
    <<"room_", Bin/binary>>.

-spec make_user_cfg(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user_cfg(GeriId, R) ->
    BinId = integer_to_binary(GeriId),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec maybe_create_room(escalus:client(), amoc_scenario:user_id()) -> ok.
maybe_create_room(Client, Id) when ((Id rem ?PER_ROOM) == 1) ->
    create_room(Client, Id);
maybe_create_room(_Client, _Id) ->
    ok.

-spec create_room(escalus:client(), amoc_scenario:user_id()) -> ok.
create_room(Client, Id) ->
    Username = jid(Id),
    Name = get_my_room(Id),
    Stanza = stanza_muc_enter_room(Name, Username),
    escalus_connection:send(Client, Stanza),
    Start = os:timestamp(),
    Res = escalus_connection:get_stanza(Client, create_room, 2000),
    _Res2 = escalus_connection:get_stanza(Client, message, infinity),
    true = check_room_created(Name, Res),
    Diff = timer:now_diff(os:timestamp(), Start),
    exometer:update(?CREATE_ROOM_TIME, Diff),
    St2 = stanza_instant_room(room_address(Name)),
    escalus:send(Client, St2),
    escalus:wait_for_stanza(Client). %topic

-spec check_room_created(binary(), exml:element()) -> boolean().
check_room_created(Name, Res) ->
    IsPresent = escalus_pred:is_presence(Res),
    case IsPresent of
        true ->
            lager:info("Room ~p created", [Name]);
        false ->
            lager:info("Room ~p was not created with response", [Res])
    end,
    IsPresent.

-spec join_room(escalus:client(), amoc_scenario:user_id()) -> ok.
join_room(Client, Id) ->
    RoomName = get_my_room(Id),
    Username = username(Id),
    Stanza = stanza_muc_enter_room(RoomName, Username),
    %%    lager:info("User ~p want to join room ~p with stanza ~p", [Username, RoomName, Stanza]),
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_message/1),
    escalus_connection:send(Client, Stanza).

-spec maybe_invite_neighbours(escalus:client(), amoc_scenario:user_id()) -> ok.
maybe_invite_neighbours(Client, Id) when ((Id rem ?PER_ROOM) == 1) ->
    RoomName = get_my_room(Id),
    FriendList = [{jid(N), <<"member">>} || N <- lists:seq(Id + 1 , Id + ?PER_ROOM - 1)],
    lager:info("I am ~p, I invite ~p", [Id, FriendList]),
    invite_many(Client, RoomName, FriendList);
maybe_invite_neighbours(_, _) ->
    ok.

-spec invite_many(bin_room_jid(), binary(), [{bin_jid(), binary()}]) -> ok.
invite_many(Client, RoomName, Friends) ->
    InviteStanza = stanza_set_affiliations(RoomName, Friends),
    %%    lager:info("Invite stanza is ~p", [InviteStanza]),
    escalus_connection:send(Client, InviteStanza),
    _Res1 = escalus_client:wait_for_stanza(Client).
%%    lager:info("Resp from invite is ~p", [Res1]).

-spec socket_opts() -> [gen_tcp:option()].
socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Cfg1 = make_user_cfg(MyId, <<"res1">>),
    Cfg2 = [{socket_opts, socket_opts()} | Cfg1],
    {ConnectionTime, ConnectionResult} = timer:tc(escalus_connection, start, [Cfg2]),
    Client = case ConnectionResult of
                 {ok, ConnectedClient, _, _} ->
                     exometer:update([amoc, counters, connections], 1),
                     exometer:update([amoc, times, connection], ConnectionTime),
                     ConnectedClient;
                 Error ->
                     exometer:update([amoc, counters, connection_failures], 1),
                     lager:error("Could not connect user=~p, reason=~p", [Cfg2, Error]),
                     exit(connection_failed)
             end,
    send_presence_available(Client),
    escalus_client:wait_for_stanza(Client),
    timer:sleep(2000),
    maybe_create_room(Client, MyId),
    maybe_invite_neighbours(Client, MyId),
    join_room(Client, MyId),
    timer:sleep(3000),
    IsSender = (MyId rem ?SENDER_INDICATOR) =/= 0,
    MyRoom = room_address(get_my_room(MyId)),
    be_sender(IsSender, MyId, MyRoom, Client).

-spec be_sender(boolean(), amoc_scenario:user_id(),
                escalus:client(), bin_room_jid()) -> no_return().
be_sender(true, MyId, MyRoom, Client) ->
    spawn_link(?MODULE, send_forever, [MyId, MyRoom, Client]),
    be_sender(false, MyId, MyRoom, Client);
%%    send_presence_unavailable(Client),
%%    escalus_connection:stop(Client);

be_sender(_, _MyId, _MyRoom, Client) ->
    receive_and_check(Client),
    be_sender(false, [], [], Client).

send_forever(MyId, MyRoom, Client) ->
    send_messages_many_times(MyId, Client, MyRoom, between_sleep_time()),
    timer:sleep(between_sleep_time()),
    send_forever(MyId, MyRoom, Client).

-spec receive_and_check(escalus:client()) -> ok.
receive_and_check(Client) ->
    Stanza = escalus_connection:get_stanza(Client, message, infinity),
    Now = usec:from_now(os:timestamp()),
    case exml_query:path(Stanza, [{element, <<"timestamp">>}, cdata]) of
        undefined ->
            ok;
        %%            lager:error("Timestamp not found in stanza ~p", [Stanza]);
        Sent ->
            TTD = (Now - binary_to_integer(Sent)),
            exometer:update(?MESSAGE_TTD_CT, TTD),
            exometer:update(?MESSAGES_REC, 1)
    end.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

-spec send_messages_many_times(amoc_scenario:user_id(), escalus:client(),
                               bin_room_jid(), integer()) -> [ok].
send_messages_many_times(Id, Client, Room, MessageInterval) ->
    [ send_message(Id, Client, Room, MessageInterval) ||
      _ <- lists:seq(1, ?MESSAGES_SENT_AT_ONCE_COUNT) ].

-spec send_message(amoc_scenario:user_id(), escalus:client(),
                   bin_room_jid(), integer()) -> ok.
send_message(_Id, Client, ToId, SleepTime) ->
    timer:sleep(SleepTime),
    Msg = make_message(ToId),
    Result = escalus_connection:send(Client, Msg),
    ok = exometer:update(?MESSAGES_CT, 1),
    Result.

-spec make_message(bin_jid()) -> exml:element().
make_message(ToId) ->
    Body = choose_body(),
    Id = escalus_stanza:id(),
    TimeStamp = integer_to_binary(usec:from_now(os:timestamp())),
    Msg = escalus_stanza:set_id(escalus_stanza:groupchat_to(ToId, Body), Id),
    OldChild = Msg#xmlel.children,
    NewChild = OldChild ++ [#xmlel{name = <<"timestamp">>, children = [#xmlcdata{content = TimeStamp}]}],
    Msg#xmlel{children = NewChild}.

-spec choose_body() -> binary().
choose_body() ->
    Bodies = bodies(),
    Index = crypto:rand_uniform(1, length(Bodies)),
    (lists:nth(Index, Bodies)).

-spec bodies() -> [binary()].
bodies() ->
    [<<"hello">>, <<"what?">>, <<"you silly...">>, <<"hi">>, <<"nope">>, <<"what's the deal">>,
     <<"cya">>, <<"have a nice day">>, <<"damm">>, <<"it not work">>, <<"should I stay or should go">>,
     <<"this is madness">>, <<"haha">>, <<"here I'm">>, <<"still not">>, <<":D">>, <<":)">>,
     <<"ok">>, <<"where are u?">>, <<"im on it">>, <<"nice! :)">>, <<"i fixed it">>, <<"wait one minute">>,
     <<"lol">>, <<"what excatly not work?">>, <<"chat">>, <<"can you fix that?">>].

-spec between_sleep_time() -> integer().
between_sleep_time() ->
    R = crypto:rand_uniform(1, 6),
    R * 1000.

-spec stanza_muc_enter_room(bin_room_jid(), bin_jid()) -> any().
stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
      escalus_stanza:presence(<<"available">>,
                              [#xmlel{name = <<"x">>,
                                      attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
      Room, Nick).

-spec stanza_set_affiliations(bin_room_jid(), [any()]) -> [exml:element()].
stanza_set_affiliations(Room, List) ->
    Fun = fun({JID, Affiliation}) ->
                  #xmlel{name = <<"item">>,
                         attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}]};
             ({JID, Affiliation, Reason}) ->
                  #xmlel{name = <<"item">>,
                         attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}],
                         children = [#xmlel{name = <<"reason">>,
                                            children = [#xmlcdata{content = Reason}]}
                                    ]
                        }
          end,
    Payload = lists:map(Fun, List),
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

-spec stanza_instant_room(bin_room_jid()) -> exml:element().
stanza_instant_room(Room) ->
    X = #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_DATA_FORMS},
                                        {<<"type">>, <<"submit">>}]},
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_OWNER, [X]), Room).


-spec stanza_to_room(exml:element(), bin_room_jid()) ->
    exml:element().
stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

-spec stanza_to_room(exml:element(), bin_room_jid(), bin_jid()) ->
    exml:element().
stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

-spec room_address(binary()) -> bin_room_jid().
room_address(Room) ->
    <<Room/binary, "@", ?MUCHOST/binary>>.

-spec room_address(binary(), binary()) -> binary().
room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUCHOST/binary, "/", Nick/binary>>.

-spec pick_server() -> [proplists:property()].
pick_server() ->
    Servers = amoc_config:get(xmpp_servers),
    verify(Servers),
    S = length(Servers),
    N = erlang:phash2(self(), S) + 1,
    lists:nth(N, Servers).

verify(Servers) ->
    lists:foreach(
      fun(Proplist) ->
              true = proplists:is_defined(host, Proplist)
      end,
      Servers
     ).
