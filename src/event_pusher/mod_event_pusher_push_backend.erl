%%%-------------------------------------------------------------------
%%% @copyright 2021, Erlang Solutions Ltd.
%%% @doc A proxy interface module between the mod_event_pusher_push
%%% module and the backend modules.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_backend).

-export([init/2,
         enable/5,
         disable/2,
         disable/4,
         get_publish_services/2]).

-define(MAIN_MODULE, mod_event_pusher_push).

%%--------------------------------------------------------------------
%% DB backend behaviour definition
%%--------------------------------------------------------------------

-callback init(Host :: jid:server(), Opts :: list()) -> ok.

-callback enable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                 Node :: mod_event_pusher_push:pubsub_node(),
                 Form :: mod_event_pusher_push:form()) ->
                    ok | {error, Reason :: term()}.

-callback disable(UserJID :: jid:jid()) ->
    ok | {error, Reason :: term()}.

-callback disable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                  Node :: mod_event_pusher_push:pubsub_node() | undefined) ->
                     ok | {error, Reason :: term()}.

-callback get_publish_services(User :: jid:jid()) ->
    {ok, [mod_event_pusher_push:publish_service()]} | {error, Reason :: term()}.


-spec init(Host :: jid:server(), Opts :: list()) -> ok.
init(Host, Opts) ->
    TrackedFuns = [enable, disable, get_publish_services],
    mongoose_backend:init(Host, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [Host, Opts],
    mongoose_backend:call(Host, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec enable(Host :: jid:server(),
             UserJID :: jid:jid(), PubsubJID :: jid:jid(),
             Node :: mod_event_pusher_push:pubsub_node(),
             Form :: mod_event_pusher_push:form()) ->
                ok | {error, Reason :: term()}.
enable(Host, UserJID, PubsubJID, Node, Form) ->
    Args = [UserJID, PubsubJID, Node, Form],
    mongoose_backend:call_tracked(Host, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec disable(Host :: jid:server(), UserJID :: jid:jid()) ->
    ok | {error, Reason :: term()}.
disable(Host, UserJID) ->
    Args = [UserJID],
    mongoose_backend:call_tracked(Host, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec disable(Host :: jid:server(), UserJID :: jid:jid(), PubsubJID :: jid:jid(),
              Node :: mod_event_pusher_push:pubsub_node() | undefined) ->
                 ok | {error, Reason :: term()}.
disable(Host, UserJID, PubsubJID, Node) ->
    Args = [UserJID, PubsubJID, Node],
    mongoose_backend:call_tracked(Host, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_publish_services(Host :: jid:server(), User :: jid:jid()) ->
    {ok, [mod_event_pusher_push:publish_service()]} | {error, Reason :: term()}.
get_publish_services(Host, User) ->
    Args = [User],
    mongoose_backend:call_tracked(Host, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
