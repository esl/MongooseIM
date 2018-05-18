%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%%==============================================================================
%%
%% Basic README
%%
%% There are two kind of users:
%%  - xmpp
%%  - amqp
%%
%% Function get_my_role(MyId) returns role for each user, basing on IS_AMQP and
%% IS_XMPP macros. To modify ratio of users type, please modify macros.
%%
%% XMPP users works as follow:
%% - connect to XMPP server
%% - schedule going online
%% - schedule sending message
%% - wait for synchronization with all other users
%% - if event go_online arrives they go online, and schedule go_offline after
%%   ?XMPP_GO_OFFLINE_AFTER
%% - if event go_offline arrives they go offline, and schedule go_online after
%%   ?XMPP_GO_ONLINE_AFTER
%% - if event stanza arrives, they handle it (currently ignores it)
%% - if event send_message arrives, they send message to ?NUMBER_OF_PREV_NEIGHBOURS
%%   and ?NUMBER_OF_NEXT_NEIGHBOURS and schedule send_message after ?XMPP_SEND_MESSAGE_AFTER
%%
%% AMQP users works as follow:
%% - open channel within existing connection
%% - creates its own queue
%% - binds own queue with presence exchanges of ?NUMBER_OF_PREV_NEIGHBOURS and ?NUMBER_OF_NEXT_NEIGHBOURS
%%   XMPP users are chosen.
%% - subscribes to own queue
%% - wait for synchronization with all other users
%% - They wait for #'basic_deliver'{} events. Currently it is ignored, just debug
%%   output may be printed.
%%
%%
%%==============================================================================
-module(xmpp_amqp).

-include_lib("exml/include/exml.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% General settings
-define(NUMBER_OF_PREV_NEIGHBOURS, 4).
-define(NUMBER_OF_NEXT_NEIGHBOURS, 4).

%% XMPP stuff
-define(XMPP_GO_OFFLINE_AFTER, 1*1000).
-define(XMPP_GO_ONLINE_AFTER, 1*1000).
-define(XMPP_SEND_MESSAGE_AFTER, 30*1000).

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(XMPP_RESOURCE, <<"resource">>).


%% rabbit stuff
-define(RABBIT_CONNECTIONS, 10).
-define(RABBIT_XMPP_EVENTS_TO_BIND, [login, logout, chat]).


%% Metrics should be defined here
-define(MESSAGES_CT, [amoc, counters, messages_sent]).
-define(MESSAGE_TTD_CT, [amoc, times, message_ttd]).
-define(MESSAGE_SERVER_TO_CLIENT_TIME, [amoc, times, server_to_client]).
-define(CONNECTION_TIME(UserType), [amoc, times, connection, UserType]).
-define(CONNECTION_COUNT(UserType), [amoc, counters, connections, UserType]).
-define(CONNECTION_FAILURE(UserType), [amoc, counters, connection_failures, UserType]).
-define(OPEN_CHANNEL_TIME, [amoc, times, open_channel]).
-define(OPEN_CHANNEL_FAILURE, [amoc, counters, open_channel_failures]).
-define(OPEN_CHANNELS, [amoc, times, open_channels]).

-define(METRICS,
        [
         {?MESSAGES_CT, spiral},
         {?MESSAGE_TTD_CT, histogram},
         {?MESSAGE_SERVER_TO_CLIENT_TIME, histogram},
         {?CONNECTION_TIME(amqp), histogram},
         {?CONNECTION_TIME(xmpp), histogram},
         {?CONNECTION_COUNT(amqp), spiral},
         {?CONNECTION_COUNT(xmpp), spiral},
         {?CONNECTION_FAILURE(amqp), spiral},
         {?CONNECTION_FAILURE(xmpp), spiral},
         {?OPEN_CHANNEL_TIME, histogram},
         {?OPEN_CHANNEL_FAILURE, spiral},
         {?OPEN_CHANNELS, spiral}
        ]).

-behaviour(amoc_scenario).

-export([start/1]).
-export([init/0]).


-type binjid() :: binary().
-type role() :: amqp
              | xmpp.

%================================================
% Global initalization
%================================================

required_vars() ->
    [
     xmpp_servers,
     amoc_users,
     rabbit_host,
     rabbit_port,
     rabbit_username,
     rabbit_password
    ].

% @doc This is simple function, which checks, if all required variables
% are available. Throws an arror otherwise.
-spec check_vars() -> ok | no_return().
check_vars() ->
    NonDefinedVars = lists:foldl(fun (Var, Acc) ->
                                         case amoc_config:get(Var) of
                                             undefined -> [Var|Acc];
                                             _ -> Acc
                                         end
                                 end,
                                 [],
                                 required_vars()),
    case NonDefinedVars of
        [] -> ok;
        _ -> error({undefined_variables, NonDefinedVars})
    end.

-spec init() -> ok.
init() ->
    check_vars(),
    init_metrics(),
    ensure_amqp_connection_pool(),
    % Setting up barrier, which stops all amoc users after they connect
    % to either MongooseIM or RabbitMQ.
    barrier_init(after_connection, amoc_config:get(amoc_users)),
    ok.

-spec start(amoc_scenario:user_id()) -> no_return().
start(MyID) ->
    case get_my_role(MyID) of
        xmpp -> xmpp_start(MyID);
        amqp -> amqp_start(MyID)
    end.

%================================================
% Metrics functions
%================================================

init_metrics() ->
    [begin
         exometer:ensure(Name, Type, []),
         exometer_report:subscribe(exometer_report_graphite,  Name, datapoints(Type), 10000)
     end
     ||
     {Name, Type} <- ?METRICS].

-spec datapoints(exometer_report:metric()) -> exometer_report:datapoints().
datapoints(spiral) -> [one, count];
datapoints(histogram) -> [mean, min, max, median, 95, 99, 999].

%================================================
% Assigning fuctions
%================================================

-define(IS_AMQP(ID), ID rem 3 == 0).
-define(IS_XMPP(ID), ID rem 3 =/= 0).

%% @doc This function returns role for specific user.
-spec get_my_role(amoc_scenario:user_id()) -> role().
get_my_role(ID) when ?IS_AMQP(ID) -> amqp;
get_my_role(ID) when ?IS_XMPP(ID) -> xmpp.

%% @doc This function returns a list of neighbours which connects via xmpp
-spec xmpp_neighbors(amoc_scenario:user_id()) -> [amoc_scenario:user_id()].
xmpp_neighbors(MyID) ->
    Filter = fun (ID) -> ?IS_XMPP(ID) end,

    PrevAmount = ?NUMBER_OF_PREV_NEIGHBOURS,
    PrevLimit = 1,
    PrevGenerator = fun (PrevValue) -> PrevValue - 1 end,

    NextAmount = ?NUMBER_OF_NEXT_NEIGHBOURS,
    NextLimit = amoc_config:get(amoc_users),
    NextGenerator = fun (PrevValue) -> PrevValue + 1 end,

    get_sequence([], PrevGenerator, Filter, PrevAmount, MyID - 1, PrevLimit)
    ++
    get_sequence([], NextGenerator, Filter, NextAmount, MyID + 1, NextLimit).

%================================================
% XMPP client behaviour
%================================================

-spec xmpp_start(amoc_scenario:user_id()) -> no_return().
xmpp_start(MyId) ->
    Cfg1 = make_user(MyId, ?XMPP_RESOURCE),
    Cfg2 = [{socket_opts, socket_opts()} | Cfg1],
    Client = connect_xmpp(Cfg2),
    escalus_connection:set_filter_predicate(Client, none),
    schedule(go_online, 0),
    schedule(send_message, 0),
    State = #{neighborIDs => xmpp_neighbors(MyId),
              client => Client,
              go_offline_after => ?XMPP_GO_OFFLINE_AFTER,
              go_online_after => ?XMPP_GO_ONLINE_AFTER,
              send_message_after => ?XMPP_SEND_MESSAGE_AFTER},
    barrier_wait(after_connection),
    xmpp_do(State).

-spec xmpp_do(State :: #{
                neighborIDs        => [amoc_scenario:user_id()],
                client             => escalus:client(),
                go_offline_after   => timer:time(),
                go_online_after    => timer:time(),
                send_message_after => timer:time()
                        }) -> no_return().
xmpp_do(#{neighborIDs := NeighborIDs,
          client := Client,
          go_offline_after := GoOfflineAfter,
          go_online_after := GoOnlineAfter,
          send_message_after := SendMessageAfter} = S) ->
    receive
        {stanza, Stanza} ->
            process_stanza(Stanza);
        {scheduled, go_offline} ->
            schedule(go_online, GoOnlineAfter),
            send_presence_unavailable(Client);
        {scheduled, go_online} ->
            schedule(go_offline, GoOfflineAfter),
            send_presence_available(Client);
        {scheduled, send_message} ->
            schedule(send_message, SendMessageAfter),
            send_messages_to_neighbors(Client, NeighborIDs)
    end,
    xmpp_do(S).

-spec process_stanza(exml:element()) -> ok.
process_stanza(_Stanza) -> ok.

-spec send_messages_to_neighbors(escalus:client(),
                                 [amoc_scenario:user_id()]) -> ok.
send_messages_to_neighbors(Client, TargetIds) ->
    lists:foreach(
      fun(TargetID) ->
              JID = make_full_jid(TargetID),
              send_message(Client, JID)
      end,
      TargetIds).

%================================================
% AMQP client behaviour
%================================================

-spec amqp_start(amoc_scenario:user_id()) -> no_return().
amqp_start(MyId) ->
    MyQueueName = make_queue_name(MyId),
    Channel = open_channel(),
    ok = create_queue(Channel, MyQueueName),
    N = xmpp_neighbors(MyId),
    ok = bind_queue_to_presence_exchange(Channel, MyQueueName, N),
    ok = subscribe_to_queue(Channel, MyQueueName),
    State = #{amqp_channel => Channel},
    barrier_wait(after_connection),
    amqp_do(State).

%% @doc Main function for AMQP client behaviour. It is fully event-based.
-spec amqp_do(State :: #{}) -> no_return().
amqp_do(State) ->
    NewState =
    receive
        {#'basic.deliver'{},
         #'amqp_msg'{props = P, payload = Payload}} ->
            Ts = P#'P_basic'.timestamp,
            lager:debug("Got msg ~p with timestamp ~p", [Payload, Ts]),
            State;
        #'basic.consume_ok'{} ->
            State;
        Msg ->
            lager:info("Got unhandled msg: ~p", [Msg]),
            State
    end,
    amqp_do(NewState).

%================================================
% XMPP actions
%================================================

-spec connect_xmpp(proplists:proplist()) -> escalus:client().
connect_xmpp(Cfg) ->
    case catch timer:tc(escalus_connection, start, [Cfg]) of
        {ConnectionTime, {ok, Client, _EscalusSessionFeatures}} ->
            exometer:update(?CONNECTION_COUNT(xmpp), 1),
            exometer:update(?CONNECTION_TIME(xmpp), ConnectionTime),
            Client;
        Error ->
            exometer:update(?CONNECTION_FAILURE(xmpp), 1),
            lager:error("Could not connect user=~p, reason=~p", [Cfg, Error]),
            timer:sleep(5000),
            connect_xmpp(Cfg)
    end.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

-spec send_message(escalus:client(), binjid()) -> ok.
send_message(Client, ToId) ->
    MsgIn = make_message(ToId),
    TimeStamp = integer_to_binary(usec:from_now(os:timestamp())),
    escalus_connection:send(Client, escalus_stanza:setattr(MsgIn, <<"timestamp">>, TimeStamp)),
    exometer:update([amoc, counters, messages_sent], 1).

%================================================
% AMQP actions
%================================================

% @doc This helper function wraps connecting to RabbitMQ servers.
%
% It provides retry mechanism and reports following metrics:
%  - connection failures count
%  - connection time
%  - connections count
-spec connect_amqp(#amqp_params_network{}) -> ok | no_return().
connect_amqp(Params) ->
    connect_amqp(Params, 5).

connect_amqp(_Params, 0) -> error("Could not connect to Rabbit, check logs");
connect_amqp(Params, Retries) ->
    case catch timer:tc(amqp_connection, start, [Params]) of
        {ConnectionTime, {ok, _Connection}} ->
            exometer:update(?CONNECTION_COUNT(amqp), 1),
            exometer:update(?CONNECTION_TIME(amqp), ConnectionTime),
            ok;
        Error ->
            exometer:update(?CONNECTION_FAILURE(amqp), 1),
            lager:error("Could not connect to Rabbit, reason=~p", [Error]),
            connect_amqp(Params, Retries - 1)
    end.

% @doc This helper function wraps opening channel to RabbitMQ servers.
%
% It provides retry mechanism and reports following metrics:
%  - open channel failures count
%  - open channel time
%  - opened channels count
-spec open_channel() -> ChannelPid :: pid() | no_return().
open_channel() -> open_channel(5).

open_channel(0) -> error("Could not open channel, see logs");
open_channel(Retries) ->
    Connection = get_amqp_connection(),
    case catch timer:tc(amqp_connection, open_channel, [Connection]) of
        {ConnectionTime, {ok, Channel}} ->
            exometer:update(?OPEN_CHANNELS, 1),
            exometer:update(?OPEN_CHANNEL_TIME, ConnectionTime),
            Channel;
        Error ->
            exometer:update(?OPEN_CHANNEL_FAILURE, 1),
            lager:error("Could not open channel to Rabbit, reason=~p", [Error]),
            open_channel(Retries - 1)
    end.

-spec create_queue(ChannelPid :: pid(), QueueName :: binary()) -> ok | error.
create_queue(Channel, QueueName) ->
    Declare = #'queue.declare'{queue = QueueName},
    case amqp_channel:call(Channel, Declare) of
        #'queue.declare_ok'{} ->
            lager:info("Declared queue for ~p", [QueueName]),
            ok;
        Err ->
            lager:error("Couldnt declare queue ~p", [Err]),
            errror
    end.

-spec subscribe_to_queue(ChannelPid :: pid, QueueName ::binary()) ->
    ok | error.
subscribe_to_queue(Channel, QueueName) ->
    Consume = #'basic.consume'{queue = QueueName,
                               consumer_tag = <<"amoc">>,
                               no_ack = true},
    case amqp_channel:subscribe(Channel, Consume, self()) of
        #'basic.consume_ok'{} ->
            lager:info("Successfully subscribed to queue ~p ", [QueueName]),
            ok;
        Err ->
            lagger:error("Couldnt subscribe to queue ~p, reason: ~p", [QueueName, Err]),
            error
    end.

%% @doc Function binds given queue to exchanges where neighborIDs are pushing
%% presence events.
%%
%% As a result, all messages presence messages published to neighbor exchanges
%% will be routed to the given queue. This does not mean user will receive
%% them. To receive them, user need to be subscribed to Queue. Returns
%% ok on success, error on error.
-spec bind_queue_to_presence_exchange(ChannelPid, Queue, NeighborIDs) ->
    ok | error when
      ChannelPid  :: pid(),
      Queue       :: binary(),
      NeighborIDs :: [amoc_scenario:user_id()].
bind_queue_to_presence_exchange(Channel, Queue, NeighborIDs) ->
    Exchange = amoc_config:get(rabbit_presence_exchange, <<"presence">>),
    RoutingKeys =
      [routing_keys_for_user(presence, ID, all_topics) || ID <- NeighborIDs],
    Results =
      [bind_queue_to_exchange(Channel, Queue, Exchange, RoutingKey)
       || RoutingKey <- RoutingKeys],
    return_ok_if_all_ok_else_error(Results).

% @doc Generic function for binding queue to exchange with given routing key.
-spec bind_queue_to_exchange(ChannelPid  :: pid(),
                             Queue       :: binary(),
                             Exchange    :: binary(),
                             RoutingKey  :: binary()) -> ok | error.
bind_queue_to_exchange(Channel, Queue, Exchange, RoutingKey) ->
    Binding = #'queue.bind'{queue = Queue,
                            exchange = Exchange,
                            routing_key = RoutingKey},
    case amqp_channel:call(Channel, Binding) of
        #'queue.bind_ok'{} ->
            ok;
        Err ->
            lager:error("Could not bind queue ~p to exchange ~p error: ~p", [Queue, Exchange, Err]),
            error
    end.

%================================================
% XMPP helpers
%================================================

-spec make_message(binjid()) -> exml:element().
make_message(ToId) ->
    Body = <<"hello sir, you are a gentelman and a scholar.">>,
    Id = escalus_stanza:id(),
    escalus_stanza:set_id(escalus_stanza:chat_to(ToId, Body), Id).

-spec make_full_jid(amoc_scenario:user_id()) -> binjid().
make_full_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary, "/", ?XMPP_RESOURCE/binary >>.

-spec make_bare_jid(amoc_scenario:user_id()) -> binjid().
make_bare_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

%================================================
% RabbitMQ helpers
%================================================

% @doc Functions starts ?RABBIT_CONNECTIONS connections
ensure_amqp_connection_pool() ->
    Host = amoc_config:get(rabbit_host),
    Port = amoc_config:get(rabbit_port),
    Username = amoc_config:get(rabbit_username),
    Password = amoc_config:get(rabbit_password),
    Params = #amqp_params_network{host = Host,
                                  port = Port,
                                  username = Username,
                                  password = Password},
    [connect_amqp(Params) || _ <- lists:seq(1, ?RABBIT_CONNECTIONS)].

% @doc This helper function utilizes internal amqp_client supervisor as
% a pool manager. Returns pid which identifies amqp connection handler.
-spec get_amqp_connection() -> ConnectionPid :: pid() | no_return().
get_amqp_connection() ->
    ConnectionSups = supervisor:which_children(amqp_sup),
    Length = length(ConnectionSups),
    {_, Pid, _, _} = lists:nth(rand:uniform(Length), ConnectionSups),
    Workers = supervisor:which_children(Pid),
    case lists:keyfind(connection, 1, Workers) of
        {connection, Pid2, worker, [amqp_gen_connection]} -> Pid2;
        _ -> error(no_connection)
    end.

-spec make_queue_name(amoc_scenario:user_id()) -> binary().
make_queue_name(Id) ->
    BinInt = integer_to_binary(Id),
    <<"amoc_amqp_", BinInt/binary>>.

%% @doc Returns routing keys, which should be used to bind to given Exchange,
%% for given user, for given topic (or 'all_topic').
%%
%% Presence exchange
%% ---
%%
%% It publishes all event with user's JID topic. So, this is possible to bind
%% only for all events (user online or user offline) for given user.
-spec routing_keys_for_user(ExchangeType :: presence,
                            UserID       :: amoc_scenario:user_id(),
                            Topics       :: all_topics) ->
    RoutingKeys :: [binary()].
routing_keys_for_user(presence, ID, all_topics) ->
    make_bare_jid(ID).

%================================================
% Helpers
%================================================

-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    Server = pick_server(),
    [ {username, ProfileId},
      {server, ?HOST},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ] ++ Server.

socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].

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
      Servers).

schedule(Msg, 0) ->
    self() ! {scheduled, Msg};

schedule(Msg, Timeout) ->
    erlang:send_after(Timeout, self(), {scheduled, Msg}).

%% @doc This function generates a sequence of values.
%%
%% This is helper function for lazy creation of lists.
%%
%% Things to be supplied:
%% - generator - function which returns next element, when there is
%%               old one given.
%% - filter - function which determines if element should be included in a result.
%% - Amount - how many elements should be generated
%% - element - initial element for generator
%% - limit - If emitted element is equal to this value stop.
-spec get_sequence(Acc       :: [X],
                   Generator :: fun((X) -> X),
                   Filter    :: fun((X) -> boolean()),
                   Amount    :: integer(),
                   Elem      :: X,
                   Limit     :: X) -> [X].
get_sequence(Acc, _Generator, _Filter, 0, _Elem, _Limit) ->
    Acc;
get_sequence(Acc, _Generator, Filter, _Amount, Elem, Limit)
  when Elem =:= Limit ->
    case Filter(Elem) of
        true -> [Elem|Acc];
        false -> Acc
    end;
get_sequence(Acc, Generator, Filter, Amount, Elem, Limit) ->
    NextElem = Generator(Elem),
    case Filter(Elem) of
        true ->
            get_sequence([Elem|Acc], Generator, Filter, Amount - 1, NextElem, Limit);
        false ->
            get_sequence(Acc, Generator, Filter, Amount, NextElem, Limit)
    end.

%% @doc Functions checks if list of results contains only 'ok' atoms. Returns
%% 'ok' or 'error'.
-spec return_ok_if_all_ok_else_error([ok | error]) -> ok | error.
return_ok_if_all_ok_else_error(Results) ->
    case lists:all(fun (Res) -> Res == ok end, Results) of
        true -> ok;
        false -> error
    end.

%================================================
% Simple barrier helpers allowing users to start at the same time.
%================================================

barrier_init(BarrierName, ExpectedUsers) ->
    Pid = spawn(fun() -> barrier_fun(ExpectedUsers, 0) end),
    register(BarrierName, Pid).

barrier_fun(TotalUsers, TotalUsers) -> ok;
barrier_fun(ExpectedUsers, TotalUsers) ->
    receive
        hi -> barrier_fun(ExpectedUsers, TotalUsers+1)
    end.

barrier_wait(BarrierName) ->
    Ref = monitor(process, BarrierName),
    BarrierName ! hi,
    receive
        {'DOWN', Ref, _, _, _} -> []
    end.
