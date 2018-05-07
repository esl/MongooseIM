%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(muc_light).

-define(HOST, <<"localhost">>).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-compile([{parse_transform, lager_transform}]).
-export([start/1]).
-export([init/0]).
-export([send_forever/3]).

-type bin_jid() :: binary().
-type bin_room_jid() :: binary().

-define(MUCHOST, <<"muclight.localhost">>).
-define(NS_MUC_LIGHT, <<"urn:xmpp:muclight:0">>).
-define(NS_MUC_LIGHT_CONFIGURATION, <<"urn:xmpp:muclight:0#configuration">>).
-define(NS_MUC_LIGHT_AFFILIATIONS, <<"urn:xmpp:muclight:0#affiliations">>).
-define(NS_MUC_LIGHT_INFO, <<"urn:xmpp:muclight:0#info">>).
-define(NS_MUC_LIGHT_BLOCKING, <<"urn:xmpp:muclight:0#blocking">>).
-define(NS_MUC_LIGHT_CREATE, <<"urn:xmpp:muclight:0#create">>).
-define(NS_MUC_LIGHT_DESTROY, <<"urn:xmpp:muclight:0#destroy">>).
-define(USERS, 1000).
-define(ROOMS, 100).
-define(PER_ROOM, 10).
-define(SENDER_INDICATOR, 3).
-define(MESSAGES_SENT_AT_ONCE_COUNT, 3).

-define(MESSAGES_CT, [amoc, counters, messages_sent]).
-define(MESSAGES_REC, [amoc, counters, messages_received]).
-define(MESSAGE_TTD_CT, [amoc, times, message_ttd]).
-define(CREATE_ROOM_TIME, [amoc, times, create_room_time]).

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

-spec jid(amoc_scenario:user_id()) -> bin_jid().
jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

-spec get_my_room(amoc_scenario:user_id()) -> bin_room_jid().
get_my_room(Id) ->
    RoomId = ( Id -1 ) div ?PER_ROOM + 1,
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

-spec create_room(escalus:client(), amoc_scenario:user_id()) -> boolean().
create_room(Client, Id) ->
    Name = get_my_room(Id),
    Stanza = stanza_create_room(Name, [{<<"roomname">>, Name}], []),
    Start = os:timestamp(),
    escalus_connection:send(Client, Stanza),
    Res = escalus_connection:get_stanza(Client, create_room, 5000),
    Diff = timer:now_diff(os:timestamp(), Start),
    exometer:update(?CREATE_ROOM_TIME, Diff),
    check_room_created(Name, Res).

-spec check_room_created(bin_room_jid(), exml:element()) -> boolean().
check_room_created(Name, Res) ->
    IsPresent = escalus_pred:is_message(Res),
    case IsPresent of
        true ->
            lager:info("Room ~p created", [Name]);
        false ->
            lager:info("Room ~p was not created with response", [Res])
    end,
    IsPresent.

-spec check_friends_invited(bin_room_jid(), exml:element(), exml:element()) ->
    boolean().
check_friends_invited(Room, Res1, Res2) ->
    AreInvited = escalus_pred:is_message(Res1) and escalus_pred:is_iq_result(Res2),
    case AreInvited of
        true ->
            lager:info("Invited friends to ~p room", [Room]);
        false ->
            lager:info("Invitation to room ~p failed with response ~p~n and n~p", [Room, Res1, Res2])
    end,
    AreInvited.

-spec maybe_invite_neighbours(escalus:client(), amoc_scenario:user_id()) ->
    boolean().
maybe_invite_neighbours(Client, Id) when ((Id rem ?PER_ROOM) == 1) ->
    IdBin = integer_to_binary(Id),
    RoomName = <<"room_", IdBin/binary>>,
    FriendList = [{jid(N), member} || N <- lists:seq(Id + 1 , Id + ?PER_ROOM - 1)],
    invite_many(Client, RoomName, FriendList);
maybe_invite_neighbours(_, _) ->
    true.

-spec invite_many(bin_room_jid(), binary(), [{bin_jid(), binary()}]) -> boolean().
invite_many(Client, RoomName, Friends) ->
    InviteStanza = stanza_aff_set(RoomName, Friends),
    escalus_connection:send(Client, InviteStanza),
    Res1 = escalus_connection:get_stanza(Client, message, infinity),
    Res2 = escalus_connection:get_stanza(Client, iq_result, infinity),
    check_friends_invited(RoomName, Res1, Res2).

-spec socket_opts() -> [gen_tcp:option()].
socket_opts() ->
    [
     binary,
     {reuseaddr, false},
     {nodelay, true}
    ].

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
    timer:sleep(3000),
    IsSender = (MyId rem ?SENDER_INDICATOR) =/= 0,
    MyRoom = room_address(get_my_room(MyId)),
    be_sender(IsSender, MyId, MyRoom, Client).


-spec be_sender(boolean(), amoc_scenario:user_id(),
                bin_room_jid(), escalus:client()) -> no_return().
be_sender(true, MyId, MyRoom, Client) ->
    spawn_link(?MODULE, send_forever, [MyId, MyRoom, Client]),
    be_sender(false, MyId, MyRoom, Client);

be_sender(false, _MyId, _MyRoom, Client) ->
    receive_and_check(Client),
    be_sender(false, [], [], Client).

-spec send_forever(amoc_scenario:user_id(), bin_room_jid(), escalus:client()) ->
    no_return().
send_forever(MyId, MyRoom, Client) ->
    send_messages_many_times(MyId, Client, between_sleep_time(), MyRoom),
    timer:sleep(between_sleep_time()),
    send_forever(MyId, MyRoom, Client).

-spec receive_and_check(escalus:client()) -> ok.
receive_and_check(Client) ->
    Stanza = escalus_connection:get_stanza(Client, message, infinity),
    Now = usec:from_now(os:timestamp()),
    case exml_query:path(Stanza, [{element, <<"body">>}, cdata]) of
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

send_messages_many_times(Id, Client, MessageInterval, Room) ->
    [ send_message(Id, Client, Room, MessageInterval) ||
      _ <- lists:seq(1, ?MESSAGES_SENT_AT_ONCE_COUNT) ].

send_message(_Id, Client, ToId, SleepTime) ->
    timer:sleep(SleepTime),
    Msg = make_message(ToId),
    Result = escalus_connection:send(Client, Msg),
    ok = exometer:update(?MESSAGES_CT, 1),
    Result.

-spec make_message(bin_jid()) -> exml:element().
make_message(ToId) ->
    Id = escalus_stanza:id(),
    Body = integer_to_binary(usec:from_now(os:timestamp())),
    escalus_stanza:set_id(escalus_stanza:groupchat_to(ToId, Body), Id).

-spec between_sleep_time() -> integer().
between_sleep_time() ->
    R = crypto:rand_uniform(1, 6),
    R * 1000.

-spec stanza_create_room(undefined | bin_room_jid(), any(), any()) ->
    exml:element().
stanza_create_room(RoomNode, InitConfig, InitOccupants) ->
    ToBinJID = case RoomNode of
                   undefined -> ?MUCHOST;
                   _ -> <<RoomNode/binary, $@, (?MUCHOST)/binary>>
               end,
    ConfigItem = #xmlel{ name = <<"configuration">>,
                         children = [ kv_el(K, V) || {K, V} <- InitConfig ] },
    OccupantsItems = [ #xmlel{name = <<"user">>,
                              attrs = [{<<"affiliation">>, BinAff}],
                              children = [#xmlcdata{ content = BinJID }] }
                       || {BinJID, BinAff} <- bin_aff_users(InitOccupants) ],
    OccupantsItem = #xmlel{ name = <<"occupants">>, children = OccupantsItems },
    escalus_stanza:to(escalus_stanza:iq_set(
                        ?NS_MUC_LIGHT_CREATE, [ConfigItem, OccupantsItem]), ToBinJID).

stanza_aff_set(Room, AffUsers) ->
    Items = [#xmlel{ name = <<"user">>, attrs = [{<<"affiliation">>, AffBin}],
        children = [#xmlcdata{ content = UserBin }] }
        || {UserBin, AffBin} <- bin_aff_users(AffUsers)],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_AFFILIATIONS, Items), room_address(Room)).

%% Helpers of helpers
kv_el(K, V) ->
    #xmlel{ name = K, children = [ #xmlcdata{ content = V } ] }.

bin_aff_users(AffUsers) ->
    [ {User, list_to_binary(atom_to_list(Aff))}
        || {User, Aff} <- AffUsers ].

-spec lbin(Bin :: binary()) -> binary().
lbin(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

-spec room_address(binary()) -> bin_room_jid().
room_address(Room) ->
    <<Room/binary, $@, (?MUCHOST)/binary>>.

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
