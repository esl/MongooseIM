%%%-------------------------------------------------------------------
%%% @author piotr.nosek@erlang-solutions.com
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 5. Jul 2018
%%%-------------------------------------------------------------------

-module(mod_inbox_rdbms_mssql).
-author("piotr.nosek@erlang-solutions.com").

-include("mod_inbox.hrl").

-export([set_inbox/7, set_inbox_incr_unread/6]).

-import(mod_inbox_rdbms, [esc_string/1, esc_int/1]).

%% -----------------------------------------------------------
%% API
%% -----------------------------------------------------------

-spec set_inbox(Username :: jid:luser(),
                Server :: jid:lserver(),
                ToBareJid :: binary(),
                Content :: binary(),
                Count :: binary(),
                MsgId :: binary(),
                Timestamp :: non_neg_integer()) -> query_result().
set_inbox(Username, Server, ToBareJid, Content, Count, MsgId, Timestamp) ->
    Query = build_query(Username, Server, ToBareJid, Content, Count, MsgId, Timestamp),
    mongoose_rdbms:sql_query(Server, Query).


-spec set_inbox_incr_unread(Username :: jid:luser(),
                            Server :: jid:lserver(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary(),
                            Timestamp :: non_neg_integer()) -> query_result().
set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId, Timestamp) ->
    Query = build_query(Username, Server, ToBareJid, Content, increment, MsgId, Timestamp),
    mongoose_rdbms:sql_query(Server, Query).

%% -----------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------

build_query(Username, Server, ToBareJid, Content, increment, MsgId, Timestamp) ->
    CountUpdate = "target.unread_count + 1",
    build_query(Username, Server, ToBareJid, Content, MsgId,
                Timestamp, esc_string("1"), CountUpdate);
build_query(Username, Server, ToBareJid, Content, CountValue, MsgId, Timestamp) ->
    ECount = esc_string(CountValue),
    build_query(Username, Server, ToBareJid, Content, MsgId, Timestamp, ECount, ECount).

build_query(Username, Server, ToBareJid, Content, MsgId, Timestamp, CountInsert, CountUpdate) ->
    ELUser = esc_string(Username),
    ELServer = esc_string(Server),
    EToBareJid = esc_string(ToBareJid),
    EContent = mongoose_rdbms:use_escaped_binary(mongoose_rdbms:escape_binary(Server, Content)),
    EMsgId = esc_string(MsgId),
    ETimestamp = esc_int(Timestamp),

    ["MERGE INTO inbox WITH (SERIALIZABLE) AS target"
      " USING (SELECT ",
                    ELUser, " as luser, ",
                    ELServer, " as lserver, ",
                    EToBareJid, " as remote_bare_jid, ",
                    EContent, " as content, ",
                    CountInsert, " as unread_count, ",
                    EMsgId, " as msg_id, ",
                    ETimestamp, " as timestamp)"
      " AS source (luser, lserver, remote_bare_jid, content, unread_count, msg_id, timestamp)"
          " ON (target.luser = source.luser"
          " AND target.lserver = source.lserver"
          " AND target.remote_bare_jid = source.remote_bare_jid)"
      " WHEN MATCHED THEN UPDATE"
          " SET content = ", EContent, ","
              " unread_count = ", CountUpdate, ","
              " msg_id = ", EMsgId, ",",
              " timestamp = ", ETimestamp,
      " WHEN NOT MATCHED THEN INSERT"
          " (luser, lserver, remote_bare_jid, content, unread_count, msg_id, timestamp)"
      " VALUES (", ELUser, ", ", ELServer, ", ", EToBareJid, ", ", EContent, ", ",
                CountInsert, ", ", EMsgId, ", ", ETimestamp, ");"].

