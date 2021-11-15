%%%-------------------------------------------------------------------
%%% @copyright (C) 2019 Erlang Solutions Ltd.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% RDBMS backend for mod_event_pusher_push.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_rdbms).
-behavior(mod_event_pusher_push_backend).

-include("mongoose.hrl").
-include_lib("jid/include/jid.hrl").
%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

-export([init/2]).
-export([enable/4,
         disable/1,
         disable/3,
         get_publish_services/1]).

%%--------------------------------------------------------------------
%% Backend callbacks
%%--------------------------------------------------------------------

-spec init(Host :: jid:server(), Opts :: list()) -> ok.
init(_Host, _Opts) ->
    prepare_queries().

-spec enable(User, PubSub, Node, Form) -> Result when
      User :: jid:jid(),
      PubSub :: jid:jid(),
      Node :: mod_event_pusher_push:pubsub_node(),
      Form :: mod_event_pusher_push:form(),
      Result :: ok | {error, term()}.
enable(#jid{lserver = LServer} = User, PubSub, Node, Forms) ->
    ExtUser = jid:to_binary(jid:to_lus(User)),
    ExtPubSub = jid:to_binary(PubSub),
    ExtForms = encode_form(Forms),
    execute_delete(LServer, ExtUser, Node, ExtPubSub),
    CreatedAt = os:system_time(microsecond),
    case execute_insert(LServer, ExtUser, Node, ExtPubSub, ExtForms, CreatedAt) of
        {updated, 1} -> ok;
        Other -> Other
    end.

-spec disable(User :: jid:jid()) -> ok.
disable(#jid{lserver = LServer} = User) ->
    ExtUser = jid:to_binary(jid:to_lus(User)),
    execute_delete(LServer, ExtUser),
    ok.

-spec disable(User :: jid:jid(), PubSub :: jid:jid(),
              Node :: mod_event_pusher_push:pubsub_node() | undefined) -> ok.
disable(#jid{lserver = LServer} = User, PubSub, undefined) ->
    ExtUser = jid:to_binary(jid:to_lus(User)),
    ExtPubSub = jid:to_binary(PubSub),
    execute_delete(LServer, ExtUser, ExtPubSub),
    ok;
disable(#jid{lserver = LServer} = User, PubSub, Node) ->
    ExtUser = jid:to_binary(jid:to_lus(User)),
    ExtPubSub = jid:to_binary(PubSub),
    execute_delete(LServer, ExtUser, Node, ExtPubSub),
    ok.

-spec get_publish_services(User :: jid:jid()) ->
    {ok, [{PubSub :: jid:jid(),
           Node :: mod_event_pusher_push:pubsub_node(),
           Form :: mod_event_pusher_push:form()}]}.
get_publish_services(#jid{lserver = LServer} = User) ->
    ExtUser = jid:to_binary(jid:to_lus(User)),
    {selected, Rows} = execute_select(LServer, ExtUser),
    {ok, decode_rows(Rows)}.

decode_rows(Rows) ->
    [decode_row(Row) || Row <- Rows].

decode_row({NodeID, PubSubBin, FormJSON}) ->
    {jid:from_binary(PubSubBin),
     NodeID,
     decode_form(FormJSON)}.

encode_form(Forms) ->
    jiffy:encode({Forms}).

decode_form(FormJSON) ->
    {Items} = jiffy:decode(FormJSON),
    Items.

%% Prepared queries

-spec prepare_queries() -> ok.
prepare_queries() ->
    mongoose_rdbms:prepare(event_pusher_push_insert, event_pusher_push_subscription,
                           [owner_jid, node, pubsub_jid, form, created_at],
                           <<"INSERT INTO event_pusher_push_subscription VALUES (?, ?, ?, ?, ?)">>),
    mongoose_rdbms:prepare(event_pusher_push_select, event_pusher_push_subscription,
                           [owner_jid],
                           <<"SELECT node, pubsub_jid, form FROM event_pusher_push_subscription "
                             "WHERE owner_jid = ?">>),
    mongoose_rdbms:prepare(event_pusher_push_delete, event_pusher_push_subscription,
                           [owner_jid],
                           <<"DELETE FROM event_pusher_push_subscription "
                             "WHERE owner_jid = ?">>),
    mongoose_rdbms:prepare(event_pusher_push_delete_pubsub_jid, event_pusher_push_subscription,
                           [owner_jid, pubsub_jid],
                           <<"DELETE FROM event_pusher_push_subscription "
                             "WHERE owner_jid = ? AND pubsub_jid = ?">>),
    mongoose_rdbms:prepare(event_pusher_push_delete_node, event_pusher_push_subscription,
                           [owner_jid, node, pubsub_jid],
                           <<"DELETE FROM event_pusher_push_subscription "
                             "WHERE owner_jid = ? AND node = ? AND pubsub_jid = ?">>),
    ok.

-spec execute_insert(jid:lserver(), jid:literal_jid(), mod_event_pusher_push:pubsub_node(),
                     jid:literal_jid(), iodata(), non_neg_integer()) ->
          mongoose_rdbms:query_result().
execute_insert(LServer, OwnerJid, Node, PubSubJid, Form, CreatedAt) ->
    mongoose_rdbms:execute_successfully(LServer, event_pusher_push_insert,
                                        [OwnerJid, Node, PubSubJid, Form, CreatedAt]).

-spec execute_select(jid:lserver(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_select(LServer, OwnerJid) ->
    mongoose_rdbms:execute_successfully(LServer, event_pusher_push_select, [OwnerJid]).

-spec execute_delete(jid:lserver(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_delete(LServer, OwnerJid) ->
    mongoose_rdbms:execute_successfully(LServer, event_pusher_push_delete, [OwnerJid]).

-spec execute_delete(jid:lserver(), jid:literal_jid(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_delete(LServer, OwnerJid, PubSubJid) ->
    mongoose_rdbms:execute_successfully(LServer, event_pusher_push_delete_pubsub_jid,
                                        [OwnerJid, PubSubJid]).

-spec execute_delete(jid:lserver(), jid:literal_jid(), mod_event_pusher_push:pubsub_node(),
                     jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_delete(LServer, OwnerJid, Node, PubSubJid) ->
    mongoose_rdbms:execute_successfully(LServer, event_pusher_push_delete_node,
                                        [OwnerJid, Node, PubSubJid]).
