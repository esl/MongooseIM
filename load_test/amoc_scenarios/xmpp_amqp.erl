%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%% Bartosz Szafran <bartosz.szafran@erlang-solutions.com>
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
%% Before starting users are started: (init/0 function)
%% - there is a check for required variables for scenario. It will crash
%%   if they are not set.
%% - Metrics are initialized.
%% - Connection pool to AMQP is created.
%%
%% XMPP users work as follow:
%% - connect to XMPP server
%% - schedule going online
%% - schedule sending message
%% - wait for synchronization with all other users
%% - if event go_online arrives they go online, and schedule go_offline after
%%   ?XMPP_GO_OFFLINE_AFTER
%% - if event go_offline arrives they go offline, and schedule go_online after
%%   ?XMPP_GO_ONLINE_AFTER
%% - if event stanza arrives, they handle it (measure TTD and report it)
%% - if event send_message arrives, they send message to ?NUMBER_OF_PREV_NEIGHBOURS
%%   and ?NUMBER_OF_NEXT_NEIGHBOURS and schedule send_message after ?XMPP_SEND_MESSAGE_AFTER
%%
%% AMQP users works as follow:
%% - open channel within existing connection
%% - creates its own queue
%% - binds own queue with ?AMQP_PRESENCE_EXCHANGE exchange for
%%   ?NUMBER_OF_PREV_NEIGHBOURS and ?NUMBER_OF_NEXT_NEIGHBOURS
%%   XMPP users.
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
-define(MAX_RETRIES, 5).
-define(SLEEP_TIME_BEFORE_RETRY, 5000). % milliseconds
% MAX_RETRIES and SLEEP_TIME_BEFORE_RETRY and used for
%  - connectiong to XMPP
%  - connection to AMQP
%  - opening channel via AMQP connection

%% XMPP stuff
-define(XMPP_GO_OFFLINE_AFTER, 10*60*1000). % milliseconds
-define(XMPP_GO_ONLINE_AFTER, 1*1000).  % milliseconds
-define(XMPP_SEND_MESSAGE_AFTER, 5*60*1000). % milliseconds

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(XMPP_RESOURCE, <<"resource">>).

%% rabbit stuff
-define(RABBIT_CONNECTIONS, 10).
-define(AMOC_AMQP_CONNECTIONS_TABLE_NAME, amoc_amqp_connections).
-define(AMQP_PRESENCE_EXCHANGE, <<"presence">>).
-define(AMQP_MESSAGE_EXCHANGE, <<"chat_msg">>).


%% Metrics should be defined here
-define(MESSAGES_SENT_COUNT(UserType), [amoc, UserType, counters, messages_sent]).
-define(MESSAGE_TTD(UserType), [amoc, UserType, times, message_ttd]).
-define(CONNECTION_TIME(UserType), [amoc, UserType, times, connection]).
-define(CONNECTION_COUNT(UserType), [amoc, UserType, counters, connections]).
-define(CONNECTION_FAILURE(UserType), [amoc, UserType, counters, connection_failures]).
-define(OPEN_CHANNEL_TIME, [amoc, amqp, times, open_channel]).
-define(OPEN_CHANNEL_FAILURE, [amoc, amqp, counters, open_channel_failures]).
-define(OPEN_CHANNELS, [amoc, amqp, counters, open_channels]).

-define(METRICS,
        [
         {?MESSAGES_SENT_COUNT(xmpp), spiral},

         {?MESSAGE_TTD(amqp), histogram},
         {?MESSAGE_TTD(xmpp), histogram},

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
    NonDefinedVars = lists:filter(fun (Var) ->
                                          amoc_config:get(Var) == undefined
                                  end,
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

-define(IS_AMQP(ID), ID rem 10 == 0).
-define(IS_XMPP(ID), ID rem 10 =/= 0).

%% @doc This function returns role for specific user.
-spec get_my_role(amoc_scenario:user_id()) -> role().
get_my_role(ID) when ?IS_AMQP(ID) -> amqp;
get_my_role(ID) when ?IS_XMPP(ID) -> xmpp.

%% @doc This function returns a list of neighbours which connects via xmpp
-spec xmpp_neighbors(amoc_scenario:user_id()) -> [amoc_scenario:user_id()].
xmpp_neighbors(MyID) ->
    AmocUsers = amoc_config:get(amoc_users),

    PrevNeighbors = [ ID || ID <- lists:seq(1, MyID-1), ?IS_XMPP(ID) ],
    LimitedPrevNeighbors = lists:sublist(lists:reverse(PrevNeighbors), ?NUMBER_OF_PREV_NEIGHBOURS),

    NextNeighbors = [ ID || ID <- lists:seq(MyID+1, AmocUsers), ?IS_XMPP(ID) ],
    LimitedNextNeighbors = lists:sublist(NextNeighbors, ?NUMBER_OF_PREV_NEIGHBOURS),

    LimitedPrevNeighbors ++ LimitedNextNeighbors.

%% @doc This function returns a list of neighbours which connects via xmpp
-spec xmpp_neighbors_for_amqp(amoc_scenario:user_id()) -> [amoc_scenario:user_id()].
xmpp_neighbors_for_amqp(MyID) ->
    AmocUsers = amoc_config:get(amoc_users),

    PrevNeighbors = [ ID || ID <- lists:seq(1, MyID-1), ?IS_XMPP(ID) ],
    LimitedPrevNeighbors = lists:sublist(lists:reverse(PrevNeighbors), 4),

    NextNeighbors = [ ID || ID <- lists:seq(MyID+1, AmocUsers), ?IS_XMPP(ID) ],
    LimitedNextNeighbors = lists:sublist(NextNeighbors, 4),

    LimitedPrevNeighbors ++ LimitedNextNeighbors.

%================================================
% XMPP client behaviour
%================================================

-spec xmpp_start(amoc_scenario:user_id()) -> no_return().
xmpp_start(MyId) ->
    Cfg1 = make_user(MyId, ?XMPP_RESOURCE),
    Cfg2 = [{socket_opts, socket_opts()} | Cfg1],
    Client = connect_xmpp(Cfg2),
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_chat_message/1),
    schedule(go_online, 0),
    State = #{neighborIDs => xmpp_neighbors(MyId),
              client => Client,
              go_offline_after => ?XMPP_GO_OFFLINE_AFTER,
              go_online_after => ?XMPP_GO_ONLINE_AFTER,
              send_message_after => ?XMPP_SEND_MESSAGE_AFTER},
    FirstMessageAfter = rand:uniform(?XMPP_SEND_MESSAGE_AFTER),
    % We did not start test for real, so it is good to GC
    garbage_collect(),
    barrier_wait(after_connection),
    schedule(send_message, FirstMessageAfter),
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
        {stanza, _ClientPid, Stanza} ->
            process_stanza(Stanza);
        {scheduled, go_offline} ->
            schedule(go_online, GoOnlineAfter),
            send_presence_unavailable(Client);
        {scheduled, go_online} ->
            %This can impact the performance of the whole test substantially.
            %schedule(go_offline, GoOfflineAfter),
            send_presence_available(Client);
        {scheduled, send_message} ->
            schedule(send_message, SendMessageAfter),
            send_messages_to_neighbors(Client, NeighborIDs);
        {system, {From, Mref}, get_state} -> % allows to do sys:get_state(Pid)
            From ! {Mref, S}
    end,
    xmpp_do(S).

-spec process_stanza(exml:element()) -> ok.
process_stanza(#xmlel{name = <<"message">>} = Stanza) ->
    case exml_query:path(Stanza, [{element, <<"body">>}, cdata]) of
        undefined -> ok;
        BinaryTimestamp -> report_message_ttd(BinaryTimestamp, xmpp)
    end;

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
    N = xmpp_neighbors_for_amqp(MyId),
    ok = bind_queue_to_presence_exchange(Channel, MyQueueName, N),
    ok = bind_queue_to_message_exchange(Channel, MyQueueName, N),
    ok = subscribe_to_queue(Channel, MyQueueName),
    State = #{amqp_channel => Channel},
    % We did not start test for real, so it is good to GC
    garbage_collect(),
    barrier_wait(after_connection),
    amqp_do(State).

%% @doc Main function for AMQP client behaviour. It is fully event-based.
-spec amqp_do(State :: #{}) -> no_return().
amqp_do(State) ->
    NewState =
    receive
        {
         #'basic.deliver'{exchange = ?AMQP_PRESENCE_EXCHANGE},
         #'amqp_msg'{} = M
        } ->
            process_presence_message(M),
            State;
        {
         #'basic.deliver'{exchange = ?AMQP_MESSAGE_EXCHANGE},
         #'amqp_msg'{} = M
        } ->
            process_chat_message(M),
            State;
        #'basic.consume_ok'{} ->
            State;
        Msg ->
            lager:info("Got unhandled msg: ~p", [Msg]),
            State
    end,
    amqp_do(NewState).

-spec process_presence_message(#'amqp_msg'{}) -> ok.
process_presence_message(#'amqp_msg'{payload = P}) ->
    lager:debug("Got presence message ~p", [P]),
    ok.

-spec process_chat_message(#'amqp_msg'{}) -> ok.
process_chat_message(#'amqp_msg'{payload = P}) ->
    #{<<"message">> := BinaryTimestamp} = jiffy:decode(P, [return_maps]),
    report_message_ttd(BinaryTimestamp, amqp),
    lager:debug("Got chat message ~p", [P]),
    ok.

%================================================
% XMPP actions
%================================================

% @doc This helper function wraps connecting to RabbitMQ servers.
% It uses generic_connect/2.
-spec connect_xmpp(proplists:proplist()) -> escalus:client() | no_return().
connect_xmpp(Cfg) ->
    ConnectFun =
      fun() ->
              {ok, Client, _Features} = escalus_connection:start(Cfg),
              {ok, Client}
      end,
    generic_connect(ConnectFun, xmpp).

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
    Stanza = make_message(ToId),
    escalus_connection:send(Client, Stanza),
    exometer:update(?MESSAGES_SENT_COUNT(xmpp), 1).

-spec report_message_ttd(Timestamp :: binary(), role()) -> ok.
report_message_ttd(Timestamp, Role) ->
    IntegerTimestamp = binary_to_integer(Timestamp),
    Diff = os:system_time(microsecond) - IntegerTimestamp,
    exometer:update(?MESSAGE_TTD(Role), Diff).

%================================================
% AMQP actions
%================================================

% @doc This helper function wraps connecting to RabbitMQ servers.
% It uses generic_connect/2.
-spec connect_amqp(#amqp_params_network{}) -> ok | no_return().
connect_amqp(Params) ->
    ConnectFun = fun() -> amqp_connection:start(Params) end,
    generic_connect(ConnectFun, amqp).

% @doc This helper function wraps opening channel to RabbitMQ servers.
%
% It provides retry mechanism and reports following metrics:
%  - open channel failures count
%  - open channel time
%  - opened channels count
-spec open_channel() -> ChannelPid :: pid() | no_return().
open_channel() -> open_channel(?MAX_RETRIES).

open_channel(0) -> error("Could not open channel, see logs");
open_channel(Retries) ->
    Connection = get_amqp_connection(),
    try timer:tc(amqp_connection, open_channel, [Connection]) of
        {ConnectionTime, {ok, Channel}} ->
            exometer:update(?OPEN_CHANNELS, 1),
            exometer:update(?OPEN_CHANNEL_TIME, ConnectionTime),
            Channel
    catch
        Error ->
            exometer:update(?OPEN_CHANNEL_FAILURE, 1),
            lager:error("Could not open channel to Rabbit, reason=~p", [Error]),
            timer:sleep(?SLEEP_TIME_BEFORE_RETRY),
            open_channel(Retries - 1)
    end.

-spec create_queue(ChannelPid :: pid(), QueueName :: binary()) -> ok | error.
create_queue(ChannelPid, QueueName) ->
    Declare = #'queue.declare'{queue = QueueName},
    try amqp_channel:call(ChannelPid, Declare) of
        #'queue.declare_ok'{} ->
            lager:info("Declared queue for ~p", [QueueName]),
            ok
    catch
        Err ->
            lager:error("Couldnt declare queue ~p", [Err]),
            error
    end.

-spec subscribe_to_queue(ChannelPid :: pid(), QueueName ::binary()) ->
    ok | error.
subscribe_to_queue(ChannelPid, QueueName) ->
    Consume = #'basic.consume'{queue = QueueName,
                               consumer_tag = <<"amoc">>,
                               no_ack = true},
    try amqp_channel:subscribe(ChannelPid, Consume, self()) of
        #'basic.consume_ok'{} ->
            lager:info("Successfully subscribed to queue ~p ", [QueueName]),
            ok
    catch
        Err ->
            lagger:error("Couldnt subscribe to queue ~p, reason: ~p", [QueueName, Err]),
            error
    end.

%% @doc Function binds given queue to exchange, where XMPP users are pushing
%% presence events, for given NeighborIDs' presences.
%%
%% As a result, all messages presence messages published to exchange
%% will be routed to the given queue. This does not mean user will receive
%% them. To receive them, user need to be subscribed to Queue. Returns
%% ok on success, error on error.
-spec bind_queue_to_presence_exchange(ChannelPid, Queue, NeighborIDs) ->
    ok | error when
      ChannelPid  :: pid(),
      Queue       :: binary(),
      NeighborIDs :: [amoc_scenario:user_id()].
bind_queue_to_presence_exchange(ChannelPid, Queue, NeighborIDs) ->
    Exchange = amoc_config:get(rabbit_presence_exchange, ?AMQP_PRESENCE_EXCHANGE),
    RoutingKeys =
      [routing_key_for_user(presence, ID, all_topics) || ID <- NeighborIDs],
    Results =
      [bind_queue_to_exchange(ChannelPid, Queue, Exchange, RoutingKey)
       || RoutingKey <- RoutingKeys],
    return_ok_if_all_ok_else_error(Results).


%% @doc
-spec bind_queue_to_message_exchange(ChannelPid, Queue, NeighborIDs) ->
    ok | error when
      ChannelPid  :: pid(),
      Queue       :: binary(),
      NeighborIDs :: [amoc_scenario:user_id()].
bind_queue_to_message_exchange(ChannelPid, Queue, NeighborIDs) ->
    Exchange = amoc_config:get(rabbit_presence_exchange, ?AMQP_MESSAGE_EXCHANGE),
    RoutingKeys =
      [routing_key_for_user(message, ID, all_topics) || ID <- NeighborIDs],
    Results =
      [bind_queue_to_exchange(ChannelPid, Queue, Exchange, RoutingKey)
       || RoutingKey <- RoutingKeys],
    return_ok_if_all_ok_else_error(Results).

% @doc Generic function for binding queue to exchange with given routing key.
-spec bind_queue_to_exchange(ChannelPid  :: pid(),
                             Queue       :: binary(),
                             Exchange    :: binary(),
                             RoutingKey  :: binary()) -> ok | error.
bind_queue_to_exchange(ChannelPid, Queue, Exchange, RoutingKey) ->
    Binding = #'queue.bind'{queue = Queue,
                            exchange = Exchange,
                            routing_key = RoutingKey},
    try amqp_channel:call(ChannelPid, Binding) of
        #'queue.bind_ok'{} ->
            ok
    catch
        Err ->
            lager:error("Could not bind queue ~p to exchange ~p error: ~p", [Queue, Exchange, Err]),
            error
    end.

%================================================
% XMPP helpers
%================================================

-spec make_message(binjid()) -> exml:element().
make_message(ToId) ->
    Timestamp = integer_to_binary(os:system_time(microsecond)),
    Id = escalus_stanza:id(),
    escalus_stanza:set_id(escalus_stanza:chat_to(ToId, Timestamp), Id).

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
    Connections =
      [{Num, connect_amqp(Params)} || Num <- lists:seq(1, ?RABBIT_CONNECTIONS)],
    ets:new(?AMOC_AMQP_CONNECTIONS_TABLE_NAME, [set,
                                                protected,
                                                named_table,
                                                {read_concurrency, true}]),
    lists:foreach(fun ({_Num, _ConnectPid} = Elem) ->
                          ets:insert(?AMOC_AMQP_CONNECTIONS_TABLE_NAME, Elem)
                  end,
                  Connections).

% @doc This helper function returns AMQP connections, which are stored in
% ?AMOC_AMQP_CONNECTIONS_TABLE
-spec get_amqp_connection() -> ConnectionPid :: pid() | no_return().
get_amqp_connection() ->
    Position = rand:uniform(?RABBIT_CONNECTIONS),
    [{_, ConnectionPid}] = ets:lookup(?AMOC_AMQP_CONNECTIONS_TABLE_NAME, Position),
    ConnectionPid.

-spec make_queue_name(amoc_scenario:user_id()) -> binary().
make_queue_name(Id) ->
    BinInt = integer_to_binary(Id),
    <<"amoc_amqp_", BinInt/binary>>.

%% @doc Returns routing key, which should be used to bind to given Exchange,
%% for given user, for given topic (or 'all_topic').
%%

-spec routing_key_for_user(ExchangeType :: presence,
                            UserID      :: amoc_scenario:user_id(),
                            Topics      :: all_topics) ->
    RoutingKey :: binary();
                          (ExchangeType :: message,
                           UserID       :: amoc_scenario:user_id(),
                           Topics       :: all_topics | sent | received) ->
    RoutingKey :: binary().

%% @doc Presence exchange
%%
%% It publishes all event with user's bare JID as a routing key. So, it is
%% possible to bind only for all events (user online or user offline) for
%% given user.
routing_key_for_user(presence, ID, all_topics) ->
    make_bare_jid(ID);

%% @doc Message exchange
%%
%% It publishes all event with user's bare JID as a routing key. So, it is
%% possible to bind only for all events (user online or user offline) for
%% given user.
routing_key_for_user(message, ID, all_topics) ->
    JID = make_bare_jid(ID),
    <<JID/binary, ".*">>;
routing_key_for_user(message, ID, sent) ->
    JID = make_bare_jid(ID),
    <<JID/binary, ".chat_msg_sent">>;
routing_key_for_user(message, ID, received) ->
    JID = make_bare_jid(ID),
    <<JID/binary, ".chat_msg_recv">>.



%================================================
% Helpers
%================================================

-spec generic_connect(ConnectFun, Role) -> Connection | no_return()
      when ConnectFun    :: fun(() -> ConnectionRet),
           ConnectionRet :: {ok, Connection} | {error, Connection} | no_return(),
           Role          :: role(),
           Connection    :: escalus_connection:escalus_connection()
                          | pid().
generic_connect(ConnectFun, Role) ->
    generic_connect(ConnectFun, Role, ?MAX_RETRIES).

% @doc Function which calls ConnectFun. It measures connection time, provides
% retry mechanism and reports metrics.
generic_connect(_, Role, 0) ->
    Msg = io_lib:format("Could not connect in role ~p, check logs", Role),
    error(Msg);

generic_connect(ConnectFun, Role, Retries) ->
    try timer:tc(ConnectFun) of
        {ConnectionTime, {ok, Connection}} ->
            exometer:update(?CONNECTION_COUNT(Role), 1),
            exometer:update(?CONNECTION_TIME(Role), ConnectionTime),
            Connection
    catch
        Error ->
            exometer:update(?CONNECTION_FAILURE(Role), 1),
            lager:error("Could not connect to ~p, reason=~p", [Role, Error]),
            timer:sleep(?SLEEP_TIME_BEFORE_RETRY),
            generic_connect(ConnectFun, Role, Retries - 1)
    end.


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
    verify_host_addr_defined(Servers),
    S = length(Servers),
    N = erlang:phash2(self(), S) + 1,
    lists:nth(N, Servers).

verify_host_addr_defined(Servers) ->
    lists:foreach(
      fun(Proplist) ->
              true = proplists:is_defined(host, Proplist)
      end,
      Servers).

schedule(Msg, 0) ->
    self() ! {scheduled, Msg};

schedule(Msg, Timeout) ->
    erlang:send_after(Timeout, self(), {scheduled, Msg}).

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
    global:register_name(BarrierName, Pid).

barrier_fun(TotalUsers, TotalUsers) -> ok;
barrier_fun(ExpectedUsers, TotalUsers) ->
    receive
        hi -> barrier_fun(ExpectedUsers, TotalUsers+1)
    end.

barrier_wait(BarrierName) ->
    Pid = global:whereis_name(BarrierName),
    Ref = monitor(process, Pid),
    Pid ! hi,
    receive
        {'DOWN', Ref, _, _, _} -> []
    end.
