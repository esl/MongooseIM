%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @todo: write me!
%%% @end
%%%-------------------------------------------------------------------
-module(mod_push_mnesia).
-author("Rafal Slota").
-behavior(mod_push).

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
    pubsub_jid  :: ejabberd:jid(),
    pubsub_node :: mod_push:pubsub_node(),
    form        :: mod_push:form()
}).

-type key()     :: ejabberd:simple_bare_jid().
-type record()  :: #push_subscription{}.

%%--------------------------------------------------------------------
%% Backend callbacks
%%--------------------------------------------------------------------

-spec init(Host :: ejabberd:server(), Opts :: list()) -> ok.
init(_Host, _Opts) ->
    mnesia:create_table(push_subscription,
                        [{disc_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, push_subscription)}]),
    mnesia:add_table_copy(push_subscription, node(), disc_copies),
    ok.


-spec enable(UserJID :: ejabberd:jid(), PubsubJID :: ejabberd:jid(),
                 Node :: mod_push:pubsub_node(), Form :: mod_push:form()) ->
    ok | {error, Reason :: term()}.
enable(User, PubSub, Node, Forms) ->
    write(make_record(User, PubSub, Node, Forms)).


-spec disable(UserJID :: ejabberd:jid(), PubsubJID :: ejabberd:jid(),
                  Node :: mod_push:pubsub_node()) -> ok | {error, Reason :: term()}.
disable(User, undefined, undefined) ->
    delete(key(User));
disable(User, undefined, undefined) ->
    delete(key(User));
disable(User, PubsubJID, Node) ->
    Result =
        transaction(
            fun() ->
                PubsubFiltered =
                    [Record ||
                        #push_subscription{pubsub_jid = RecPubsubJID} = Record <- read(key(User)),
                     PubsubJID == undefined orelse RecPubsubJID == PubsubJID],

                NodeFiltered =
                    [Record || #push_subscription{pubsub_node = RecNode} = Record <- PubsubFiltered,
                     Node == undefined orelse RecNode == Node],

                [mnesia:delete_object(Record) || Record <- NodeFiltered]
            end),

    case Result of
        {error, _} = E ->
            E;
        _ ->
            ok
    end.


-spec get_publish_services(User :: ejabberd:jid()) ->
    {ok, [{PubSub :: ejabberd:jid(), Node :: mod_push:node(), Form :: mod_push:form()}]} |
    {error, Reason :: term()}.
get_publish_services(User) ->
    case safe_read(key(User)) of
        {ok, Records} ->
            [{PubsubJID, Node, Forms} ||
                #push_subscription{pubsub_jid = PubsubJID,
                                   pubsub_node = Node,
                                   form = Forms} <- Records];
        {error, _} = E ->
            E
    end.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

-spec read(key()) -> [record()] | no_return().
read(Key) ->
    F = fun() -> mnesia:read({push_subscription, Key}) end,
    mnesia:async_dirty(F).

-spec safe_read(key()) -> {ok, [record()]} | {error, Reason :: term()}.
safe_read(Key) ->
    try read(Key)
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec write(record()) -> ok | {error, Reason :: term()}.
write(Record) ->
    F = fun() -> mnesia:write(Record) end,
    transaction(F).

-spec delete(key()) -> ok | {error, Reason :: term()}.
delete(Key) ->
    F = fun() -> mnesia:delete({push_subscription, Key}) end,
    transaction(F).

-spec transaction(fun(() -> any())) -> Result :: any().
transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

-spec make_record(UserJID :: ejabberd:jid(), PubsubJID :: ejabberd:jid(),
                  Node :: mod_push:pubsub_node(), Form :: mod_push:form()) -> record().
make_record(UserJID, PubsubJID, Node, Form) ->
    #push_subscription{
        user_jid = key(UserJID),
        pubsub_jid = PubsubJID,
        pubsub_node = Node,
        form = Form
    }.

-spec key(ejabberd:jid()) -> key() | no_return().
key(JID) ->
    case jid:to_lus(JID) of
        error ->
            throw({invalid_jid, JID});
        SimpleBareJID ->
            SimpleBareJID
    end.