%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenarion users are sending message to its neighbours
%% (users wiht lower and grater idea defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be send NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE ms
%% Every CHECKER_SESSIONS_INDICATOR is a checker session which just measures message TTD
%%
%%==============================================================================
-module(pubsub_load_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(SERVER_IPS, {<<"127.0.0.1">>}).

-behaviour(amoc_scenario).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          pubsub_load_test
         ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

-define(NUM_USERS, 2).

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(?HOST, Config),
    dynamic_modules:ensure_modules(?HOST, required_modules()),
    Config2 = [{escalus_user_db, {module, escalus_ejabberd, []}} | Config1],
    escalus:init_per_suite(Config2),
    escalus_users:create_users(Config2, user_specs(?NUM_USERS)),
    application:ensure_all_started(amoc),
    Config2.

end_per_suite(Config) ->
    escalus_users:delete_users(Config, user_specs(?NUM_USERS)),
    escalus:end_per_suite(Config),
    dynamic_modules:restore_modules(?HOST, Config).

init_per_testcase(TC, Config) ->
    register(tc_master, self()),
    escalus:init_per_testcase(TC, Config).

end_per_testcase(TC, Config) ->
    escalus:end_per_testcase(TC, Config),
    unregister(tc_master).

pubsub_load_test(_Config) ->
    Ids = [1, 2],
    amoc_local:do(?MODULE, hd(Ids), lists:last(Ids)),
    Monitors = [monitor_user(Id) || Id <- Ids],
    wait_until_finished(Monitors),
    ok.

wait_until_finished([]) ->
    receive
        Msg ->
            error({unexpected_message, Msg})
    after 0 -> ok
    end;
wait_until_finished(Monitors) ->
    receive
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            normal = Reason,
            {value, {MonitorRef, Pid, Id}, RemainingMonitors} =
                lists:keytake(MonitorRef, 1, Monitors),
            done = get_notification(Id),
            wait_until_finished(RemainingMonitors)
    end.

monitor_user(Id) ->
    Pid = wait_for_notification(Id),
    {monitor(process, Pid), Pid, Id}.

%% AMOC Callbacks

-spec init() -> ok.
init() ->
    application:set_env(amoc, repeat_count, 1),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    notify(MyId, tc_master, self()),
    Client = start_client(MyId),
    send_presence_available(Client),
    receive_presence(Client, Client),
    work(MyId, Client),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client),
    notify(MyId, tc_master, done).

%% Helpers

required_modules() ->
    [{mod_pubsub, [
                   {plugins,[<<"dag">>]},
                   {nodetree,<<"dag">>}
                  ]}].

user_specs(N) ->
    [{user, make_user(Id, <<"res1">>)} || Id <- lists:seq(1, N)].

-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    [ {username, ProfileId},
      {server, ?HOST},
      {host, pick_server(?SERVER_IPS)},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ].

start_client(MyId) ->
    Cfg = make_user(MyId, <<"res1">>),
    {ok, Client, _, _} = escalus_connection:start(Cfg),
    Jid = make_jid(MyId),
    Client#client{jid = Jid}.

-define(PUBLISHER_ID, 1).
-define(SUBSCRIBER_ID, 2).
-define(DELAY_BETWEEN_MESSAGES, 1000).

work(MyId = ?PUBLISHER_ID, Client) ->
    pubsub_tools:create_node(Client, node_id(MyId)),
    notify(MyId, ?SUBSCRIBER_ID, node_created),
    subscribed = wait_for_notification(?SUBSCRIBER_ID),
    [publish_and_wait(Client, MyId, ItemId, ?DELAY_BETWEEN_MESSAGES) || ItemId <- item_ids()],
    unsubscribed = wait_for_notification(?SUBSCRIBER_ID),
    pubsub_tools:delete_node(Client, node_id(MyId));
work(MyId = ?SUBSCRIBER_ID, Client) ->
    node_created = wait_for_notification(?PUBLISHER_ID),
    pubsub_tools:subscribe(Client, node_id(?PUBLISHER_ID)),
    notify(MyId, ?PUBLISHER_ID, subscribed),
    [receive_item_notification_and_wait(Client, ?PUBLISHER_ID, ItemId,
                                        ?DELAY_BETWEEN_MESSAGES)
     || ItemId <- item_ids()],
    pubsub_tools:unsubscribe(Client, node_id(?PUBLISHER_ID)),
    notify(MyId, ?PUBLISHER_ID, unsubscribed).

node_id(PublisherId) ->
    NodeId = integer_to_binary(PublisherId),
    {<<"pubsub.localhost">>, <<"node", NodeId/binary>>}.

publish_and_wait(Client, PublisherId, ItemId, Delay) ->
    pubsub_tools:publish(Client, ItemId, node_id(PublisherId)),
    timer:sleep(Delay).

receive_item_notification_and_wait(Client, PublisherId, ItemId, Delay) ->
    pubsub_tools:receive_item_notification(Client, ItemId, node_id(PublisherId)),
    timer:sleep(Delay).

item_ids() ->
    [integer_to_binary(I) || I <- lists:seq(1, 5)].

user_pid(Id) ->
    %% Todo extend amoc (also with distributed mode)
    [{Id, Pid}] = ets:lookup(amoc_users, Id),
    Pid.

get_notification(Id) ->
    wait_for_notification(Id, 0).

wait_for_notification(Id) ->
    wait_for_notification(Id, 10000).

wait_for_notification(Id, Timeout) ->
    receive
        {notification, Id, Payload} ->
            Payload
    after Timeout ->
            error({missing_notification, Id})
    end.

notify(MyId, tc_master, Payload) ->
    tc_master ! {notification, MyId, Payload};
notify(MyId, Id, Payload) ->
    user_pid(Id) ! {notification, MyId, Payload}.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

receive_presence(Client1, Client2) ->
    PresenceNotification = escalus:wait_for_stanza(Client1),
    escalus:assert(is_presence, PresenceNotification),
    escalus:assert(is_stanza_from, [Client2], PresenceNotification).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

-spec make_jid(amoc_scenario:user_id()) -> any().
make_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

-spec pick_server({binary()}) -> binary().
pick_server(Servers) ->
    S = size(Servers),
    N = erlang:phash2(self(), S) + 1,
    element(N, Servers).
