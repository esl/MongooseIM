%%%-------------------------------------------------------------------
%%% @copyright (C) 2019 Erlang Solutions Ltd.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% RDBMS backend for mod_event_pusher_push.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_mnesia).
-behavior(mod_event_pusher_push).

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

-export([init/2]).
-export([enable/4,
         disable/3,
         get_publish_services/1]).


%%--------------------------------------------------------------------
%% Backend callbacks
%%--------------------------------------------------------------------

-spec init(Host :: jid:server(), Opts :: list()) -> ok.
init(_Host, _Opts) ->
    ok.


-spec enable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
             Node :: mod_event_pusher_push:pubsub_node(), Form :: mod_event_pusher_push:form()) ->
                    ok | {error, Reason :: term()}.
enable(User, PubSub, Node, Forms) ->
    disable(User, PubSub, Node),
    {error, not_implemented}.

-spec disable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
              Node :: mod_event_pusher_push:pubsub_node()) -> ok | {error, Reason :: term()}.
disable(User, PubsubJID, Node) ->
    {error, not_implemented}.

-spec get_publish_services(User :: jid:jid()) ->
                                  {ok, [{PubSub :: jid:jid(),
                                         Node :: mod_event_pusher_push:pubsub_node(),
                                         Form :: mod_event_pusher_push:form()}]} |
                                                 {error, Reason :: term()}.
get_publish_services(User) ->
    {ok, []}.

