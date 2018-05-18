%%%-------------------------------------------------------------------
%%% @author Kacper Mentel <kacper.mentel@erlang-solutions.com>
%%% @copyright (C) 2018, Kacper Mentel
%%% @doc
%%% Backend for `mod_event_pusher` that supports RabbitMQ integeration.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_rabbit).

-behaviour(gen_mod).
-behaviour(mod_event_pusher).

-include_lib("mongooseim/include/mod_event_pusher_events.hrl").
-include_lib("mongooseim/include/mongoose.hrl").
-include_lib("mongooseim/include/jlib.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% Types and definitions
%%%===================================================================

-define(DEFAULT_PRESENCE_TOPIC, <<"presence">>).

%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1]).

%% API
-export([push_event/3]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> ok.
start(Host, _Opts) ->
    application:ensure_all_started(amqp_client),
    application:ensure_all_started(worker_pool),
    WorkerNum = opt(Host, pool_size, 100),
    wpool:start_sup_pool(pool_name(Host),
                         [{worker, {mongoose_rabbit_worker,
                                    [{amqp_client_opts, amqp_client_opts(Host)}]
                                   }},
                          {workers, WorkerNum}]),
    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    wpool:stop_sup_pool(pool_name(Host)),
    ok.

push_event(Acc, _, #user_status_event{jid = UserJID, status = Status}) ->
    publish_user_presence_change(UserJID, Status),
    Acc;
push_event(Acc, _, _) ->
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec publish_user_presence_change(JID :: jid:jid(), Status :: atom()) -> ok.
publish_user_presence_change(JID, Status) ->
    {User, Host, _} = jid:to_lower(JID),
    Topic = opt(Host, presence_updates_topic, ?DEFAULT_PRESENCE_TOPIC),
    wpool:cast(pool_name(Host), {user_presence_changed,
                                 #{user_jid => jid:to_binary({User, Host}),
                                   status => Status,
                                   topic => Topic}}, available_worker).


-spec pool_name(Host :: jid:lserver()) -> atom().
pool_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

-spec amqp_client_opts(Host :: jid:lserver()) ->
    amqp_client:amqp_params_network().
amqp_client_opts(Host) ->
    DefaultParams = #amqp_params_network{},
    #amqp_params_network{
       host = opt(Host, amqp_host, DefaultParams#amqp_params_network.host),
       port = opt(Host, amqp_port, DefaultParams#amqp_params_network.port),
       username = opt(Host, amqp_username,
                      DefaultParams#amqp_params_network.username),
       password = opt(Host, amqp_password,
                      DefaultParams#amqp_params_network.password)}.

%% Getter for module options
-spec opt(Host :: jid:lserver(), Option :: atom()) -> Value :: term().
opt(Host, Option) ->
    opt(Host, Option, undefined).

%% Getter for module options with default value
-spec opt(Host :: jid:lserver(), Option :: atom(), Default :: term()) ->
     Value :: term().
opt(Host, Option, Default) ->
    gen_mod:get_module_opt(Host, ?MODULE, Option, Default).
