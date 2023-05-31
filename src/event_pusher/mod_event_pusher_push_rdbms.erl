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

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

-export([init/2]).
-export([enable/5,
         disable/2,
         disable/4,
         get_publish_services/2]).

%%--------------------------------------------------------------------
%% Backend callbacks
%%--------------------------------------------------------------------

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, _Opts) ->
    prepare_queries().

-spec enable(HostType, User, PubSub, Node, Form) -> Result when
      HostType :: mongooseim:host_type(),
      User :: jid:jid(),
      PubSub :: jid:jid(),
      Node :: mod_event_pusher_push:pubsub_node(),
      Form :: mod_event_pusher_push:form(),
      Result :: ok | {error, term()}.
enable(HostType, User, PubSub, Node, Form) ->
    ExtUser = jid:to_bare_binary(User),
    ExtPubSub = jid:to_binary(PubSub),
    ExtForms = encode_form(Form),
    execute_delete(HostType, ExtUser, Node, ExtPubSub),
    CreatedAt = os:system_time(microsecond),
    case execute_insert(HostType, ExtUser, Node, ExtPubSub, ExtForms, CreatedAt) of
        {updated, 1} -> ok;
        Other -> Other
    end.

-spec disable(mongooseim:host_type(), User :: jid:jid()) -> ok.
disable(HostType, User) ->
    ExtUser = jid:to_bare_binary(User),
    execute_delete(HostType, ExtUser),
    ok.

-spec disable(mongooseim:host_type(), User :: jid:jid(), PubSub :: jid:jid(),
              Node :: mod_event_pusher_push:pubsub_node() | undefined) -> ok.
disable(HostType, User, PubSub, undefined) ->
    ExtUser = jid:to_bare_binary(User),
    ExtPubSub = jid:to_binary(PubSub),
    execute_delete(HostType, ExtUser, ExtPubSub),
    ok;
disable(HostType, User, PubSub, Node) ->
    ExtUser = jid:to_bare_binary(User),
    ExtPubSub = jid:to_binary(PubSub),
    execute_delete(HostType, ExtUser, Node, ExtPubSub),
    ok.

-spec get_publish_services(mongooseim:host_type(), User :: jid:jid()) ->
    {ok, [{PubSub :: jid:jid(),
           Node :: mod_event_pusher_push:pubsub_node(),
           Form :: mod_event_pusher_push:form()}]}.
get_publish_services(HostType, User) ->
    ExtUser = jid:to_bare_binary(User),
    {selected, Rows} = execute_select(HostType, ExtUser),
    {ok, decode_rows(Rows)}.

decode_rows(Rows) ->
    [decode_row(Row) || Row <- Rows].

decode_row({NodeID, PubSubBin, FormJSON}) ->
    {jid:from_binary(PubSubBin),
     NodeID,
     decode_form(FormJSON)}.

encode_form(Form) ->
    jiffy:encode(Form).

decode_form(FormJSON) ->
    jiffy:decode(FormJSON, [return_maps]).

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

-spec execute_insert(mongooseim:host_type(), jid:literal_jid(), mod_event_pusher_push:pubsub_node(),
                     jid:literal_jid(), iodata(), non_neg_integer()) ->
          mongoose_rdbms:query_result().
execute_insert(HostType, OwnerJid, Node, PubSubJid, Form, CreatedAt) ->
    mongoose_rdbms:execute_successfully(HostType, event_pusher_push_insert,
                                        [OwnerJid, Node, PubSubJid, Form, CreatedAt]).

-spec execute_select(mongooseim:host_type(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_select(HostType, OwnerJid) ->
    mongoose_rdbms:execute_successfully(HostType, event_pusher_push_select, [OwnerJid]).

-spec execute_delete(mongooseim:host_type(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_delete(HostType, OwnerJid) ->
    mongoose_rdbms:execute_successfully(HostType, event_pusher_push_delete, [OwnerJid]).

-spec execute_delete(mongooseim:host_type(), jid:literal_jid(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_delete(HostType, OwnerJid, PubSubJid) ->
    mongoose_rdbms:execute_successfully(HostType, event_pusher_push_delete_pubsub_jid,
                                        [OwnerJid, PubSubJid]).

-spec execute_delete(mongooseim:host_type(), jid:literal_jid(), mod_event_pusher_push:pubsub_node(),
                     jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_delete(HostType, OwnerJid, Node, PubSubJid) ->
    mongoose_rdbms:execute_successfully(HostType, event_pusher_push_delete_node,
                                        [OwnerJid, Node, PubSubJid]).
