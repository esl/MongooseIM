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

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback enable(mongooseim:host_type(), UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                 Node :: mod_event_pusher_push:pubsub_node(),
                 Form :: mod_event_pusher_push:form()) ->
                    ok | {error, Reason :: term()}.

-callback disable(mongooseim:host_type(), UserJID :: jid:jid()) ->
    ok | {error, Reason :: term()}.

-callback disable(mongooseim:host_type(), UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                  Node :: mod_event_pusher_push:pubsub_node() | undefined) ->
                     ok | {error, Reason :: term()}.

-callback get_publish_services(mongooseim:host_type(), UserJID :: jid:jid()) ->
    {ok, [mod_event_pusher_push:publish_service()]} | {error, Reason :: term()}.

%% API

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [enable, disable, get_publish_services],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec enable(mongooseim:host_type(),
             UserJID :: jid:jid(), PubsubJID :: jid:jid(),
             Node :: mod_event_pusher_push:pubsub_node(),
             Form :: mod_event_pusher_push:form()) ->
                ok | {error, Reason :: term()}.
enable(HostType, UserJID, PubsubJID, Node, Form) ->
    Args = [HostType, UserJID, PubsubJID, Node, Form],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec disable(mongooseim:host_type(), UserJID :: jid:jid()) ->
    ok | {error, Reason :: term()}.
disable(HostType, UserJID) ->
    Args = [HostType, UserJID],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec disable(mongooseim:host_type(), UserJID :: jid:jid(), PubsubJID :: jid:jid(),
              Node :: mod_event_pusher_push:pubsub_node() | undefined) ->
                 ok | {error, Reason :: term()}.
disable(HostType, UserJID, PubsubJID, Node) ->
    Args = [HostType, UserJID, PubsubJID, Node],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_publish_services(mongooseim:host_type(), User :: jid:jid()) ->
    {ok, [mod_event_pusher_push:publish_service()]} | {error, Reason :: term()}.
get_publish_services(HostType, User) ->
    Args = [HostType, User],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
