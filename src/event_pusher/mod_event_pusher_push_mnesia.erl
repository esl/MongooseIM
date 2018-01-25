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
-behavior(mod_event_pusher_push).

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

-export([init/2]).
-export([enable/4, disable/3, get_publish_services/1]).

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

-spec init(Host :: jid:server(), Opts :: list()) -> ok.
init(_Host, _Opts) ->
    mnesia:create_table(push_subscription,
                        [{disc_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, push_subscription)}]),
    mnesia:add_table_copy(push_subscription, node(), disc_copies),
    ok.


-spec enable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
             Node :: mod_event_pusher_push:pubsub_node(), Form :: mod_event_pusher_push:form()) ->
                    ok | {error, Reason :: term()}.
enable(User, PubSub, Node, Forms) ->
    disable(User, PubSub, Node),
    write(make_record(User, PubSub, Node, Forms)).


-spec disable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
              Node :: mod_event_pusher_push:pubsub_node()) -> ok | {error, Reason :: term()}.
disable(User, undefined, undefined) ->
    delete(key(User));
disable(User, PubsubJID, Node) ->
    Result =
    exec(
          fun() ->
                  PubsubFiltered =
                      [Record ||
                          #push_subscription{pubsub_jid = RecPubsubJID} = Record <- read(key(User)),
                          PubsubJID == undefined orelse RecPubsubJID == PubsubJID],

                  NodeFiltered =
                      [Record ||
                          #push_subscription{pubsub_node = RecNode} = Record <- PubsubFiltered,
                          Node == undefined orelse RecNode == Node],

                  [mnesia:delete_object(Record) || Record <- NodeFiltered]
          end),

    case Result of
        {error, _} = E ->
            E;
        _ ->
            ok
    end.


-spec get_publish_services(User :: jid:jid()) ->
                                  {ok, [{PubSub :: jid:jid(),
                                         Node :: mod_event_pusher_push:pubsub_node(),
                                         Form :: mod_event_pusher_push:form()}]} |
                                                 {error, Reason :: term()}.
get_publish_services(User) ->
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
