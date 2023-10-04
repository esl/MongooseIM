%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Mnesia backend for mod_event_pusher_push.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_mnesia).
-author("Rafal Slota").
-behavior(mod_event_pusher_push_backend).

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

-export([init/2]).
-export([enable/5,
         disable/2,
         disable/4,
         get_publish_services/2]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-record(push_subscription, {
          user_jid    :: key() | undefined,
          pubsub_jid  :: jid:jid(),
          pubsub_node :: mod_event_pusher_push:pubsub_node(),
          form        :: mod_event_pusher_push:form()
         }).

-type key()     :: jid:simple_bare_jid().
-type sub_record()  :: #push_subscription{}.

%%--------------------------------------------------------------------
%% Backend callbacks
%%--------------------------------------------------------------------

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, _Opts) ->
    mongoose_mnesia:create_table(push_subscription,
        [{disc_copies, [node()]}, {type, bag},
         {attributes, record_info(fields, push_subscription)}]),
    ok.

-spec enable(mongooseim:host_type(), UserJID :: jid:jid(), PubsubJID :: jid:jid(),
             Node :: mod_event_pusher_push:pubsub_node(), Form :: mod_event_pusher_push:form()) ->
                    ok | {error, Reason :: term()}.
enable(HostType, User, PubSub, Node, Forms) ->
    disable(HostType, User, PubSub, Node),
    write(make_record(User, PubSub, Node, Forms)).


-spec disable(mongooseim:host_type(), UserJID :: jid:jid()) -> ok | {error, Reason :: term()}.
disable(_HostType, User) ->
    delete(key(User)).

-spec disable(mongooseim:host_type(), UserJID :: jid:jid(), PubsubJID :: jid:jid(),
              Node :: mod_event_pusher_push:pubsub_node() | undefined) ->
          ok | {error, Reason :: term()}.
disable(_HostType, User, PubsubJID, Node) ->
    Result =
    exec(
          fun() ->
                  Filtered =
                      [Record ||
                          #push_subscription{pubsub_jid = RecPubsubJID,
                                             pubsub_node = RecNode} = Record <- read(key(User)),
                          RecPubsubJID == PubsubJID,
                          Node == undefined orelse RecNode == Node],

                  [mnesia:delete_object(Record) || Record <- Filtered]
          end),

    case Result of
        {error, _} = E ->
            E;
        _ ->
            ok
    end.


-spec get_publish_services(mongooseim:host_type(), User :: jid:jid()) ->
                                  {ok, [{PubSub :: jid:jid(),
                                         Node :: mod_event_pusher_push:pubsub_node(),
                                         Form :: mod_event_pusher_push:form()}]} |
                                                 {error, Reason :: term()}.
get_publish_services(_HostType, User) ->
    case safe_read(key(User)) of
        {ok, Records} ->
            {ok, [{PubsubJID, Node, Forms} ||
                #push_subscription{pubsub_jid = PubsubJID,
                                   pubsub_node = Node,
                                   form = Forms} <- Records]};
        {error, _} = E ->
            E
    end.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

-spec read(key()) -> [sub_record()] | no_return().
read(Key) ->
    F = fun() -> mnesia:read({push_subscription, Key}) end,
    mnesia:async_dirty(F).

-spec safe_read(key()) -> {ok, [sub_record()]} | {error, Reason :: term()}.
safe_read(Key) ->
    try read(Key) of
        Records ->
            {ok, Records}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec write(sub_record()) -> ok | {error, Reason :: term()}.
write(Record) ->
    F = fun() -> mnesia:write(Record) end,
    exec(F).

-spec delete(key()) -> ok | {error, Reason :: term()}.
delete(Key) ->
    F = fun() -> mnesia:delete({push_subscription, Key}) end,
    exec(F).

-spec exec(fun(() -> any())) -> Result :: any().
exec(F) ->
    mnesia:sync_dirty(F).

-spec make_record(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                  Node :: mod_event_pusher_push:pubsub_node(),
                  Form :: mod_event_pusher_push:form()) -> sub_record().
make_record(UserJID, PubsubJID, Node, Form) ->
    #push_subscription{
       user_jid = key(UserJID),
       pubsub_jid = PubsubJID,
       pubsub_node = Node,
       form = Form
      }.

-spec key(jid:jid()) -> key().
key(JID) ->
    jid:to_lus(JID).
