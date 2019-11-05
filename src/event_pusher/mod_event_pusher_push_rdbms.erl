%%%-------------------------------------------------------------------
%%% @copyright (C) 2019 Erlang Solutions Ltd.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% RDBMS backend for mod_event_pusher_push.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_rdbms).
-behavior(mod_event_pusher_push).

-include("mongoose.hrl").
-include("jid.hrl").
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

-spec enable(User, PubSub, Node, Form) -> Result when
      User :: jid:jid(),
      PubSub :: jid:jid(),
      Node :: mod_event_pusher_push:pubsub_node(),
      Form :: mod_event_pusher_push:form(),
      Result :: ok | {error, term()}.
enable(#jid{lserver = LServer} = User, PubSub, Node, Forms) ->
    UserEscaped = mongoose_rdbms:escape_string(jid:to_binary(jid:to_lus(User))),
    PubSubEscaped = mongoose_rdbms:escape_string(jid:to_binary(PubSub)),
    FormsJSONEscaped = mongoose_rdbms:escape_string(encode_form(Forms)),
    NodeEscaped = mongoose_rdbms:escape_string(Node),

    DeleteQuery = delete_query(UserEscaped, PubSubEscaped, NodeEscaped),
    mongoose_rdbms:sql_query(LServer, DeleteQuery),

    CreatedAtEscaped = mongoose_rdbms:escape_integer(os:system_time(microsecond)),
    InsertQuery = insert_query(UserEscaped, PubSubEscaped, NodeEscaped,
                               FormsJSONEscaped, CreatedAtEscaped),
    case mongoose_rdbms:sql_query(LServer, InsertQuery) of
        {updated, 1} -> ok;
        Other -> Other
    end.

-spec disable(User :: jid:jid(), PubSub :: jid:jid() | undefined,
              Node :: mod_event_pusher_push:pubsub_node() | undefined) ->
    ok | {error, Reason :: term()}.
disable(#jid{lserver = LServer} = User, PubSub, Node) ->
    UserEscaped = mongoose_rdbms:escape_string(jid:to_binary(jid:to_lus(User))),
    PubSubBin = maybe_to_binary(PubSub),
    PubSubEscaped = escape_if_defined(PubSubBin),
    NodeEscaped = escape_if_defined(Node),
    DeleteQuery = delete_query(UserEscaped, PubSubEscaped, NodeEscaped),
    case mongoose_rdbms:sql_query(LServer, DeleteQuery) of
        {updated, _} -> ok;
        Other -> Other
    end.

maybe_to_binary(undefined) -> undefined;
maybe_to_binary(#jid{} = JID) -> jid:to_binary(JID).

escape_if_defined(undefined) -> undefined;
escape_if_defined(Value) -> mongoose_rdbms:escape_string(Value).

-spec get_publish_services(User :: jid:jid()) ->
    {ok, [{PubSub :: jid:jid(),
           Node :: mod_event_pusher_push:pubsub_node(),
           Form :: mod_event_pusher_push:form()}]} |
    {error, Reason :: term()}.
get_publish_services(#jid{lserver = LServer} = User) ->
    UserEscaped = mongoose_rdbms:escape_string(jid:to_binary(jid:to_lus(User))),
    SelectQuery = select_query(UserEscaped),
    Result = mongoose_rdbms:sql_query(LServer, SelectQuery),
    case Result of
        {selected, Rows} ->
            {ok, decode_rows(Rows)};
        Other ->
            Other
    end.

decode_rows(Rows) ->
    [decode_row(Row) || Row <- Rows].

decode_row({NodeID, PubSubBin, FormJSON}) ->
    {jid:from_binary(PubSubBin),
     NodeID,
     decode_form(FormJSON)}.

insert_query(User, PubSub, Node, FormJSON, CreatedAt) ->
    [<<"INSERT INTO ">>, table_name(), <<" (owner_jid, node, pubsub_jid, form, created_at) VALUES (">>,
        mongoose_rdbms:use_escaped_string(User), "," ,
        mongoose_rdbms:use_escaped_string(Node), ",",
        mongoose_rdbms:use_escaped_string(PubSub), "," ,
        mongoose_rdbms:use_escaped_string(FormJSON), "," ,
        mongoose_rdbms:use_escaped_integer(CreatedAt),
     ")"].

delete_query(User, PubSub, Node) ->
    [<<"DELETE FROM ">>, table_name(),
     <<" WHERE owner_jid = ">>, mongoose_rdbms:use_escaped_string(User),
     maybe_add_col_filter("node", Node),
     maybe_add_col_filter("pubsub_jid", PubSub)].

maybe_add_col_filter(_, undefined) -> [];
maybe_add_col_filter(ColName, Value) ->
    [" AND ", ColName, " = ", mongoose_rdbms:use_escaped_string(Value)].

select_query(User) ->
    [<<"SELECT node, pubsub_jid, form FROM ">>, table_name(),
     <<" WHERE owner_jid = ">>, mongoose_rdbms:use_escaped_string(User)].

encode_form(Forms) ->
    jiffy:encode({Forms}).

decode_form(FormJSON) ->
    {Items} = jiffy:decode(FormJSON),
    Items.

table_name() ->
    <<"event_pusher_push_subscription">>.
