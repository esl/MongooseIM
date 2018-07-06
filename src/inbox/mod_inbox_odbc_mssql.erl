%%%-------------------------------------------------------------------
%%% @author piotr.nosek@erlang-solutions.com
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 5. Jul 2018
%%%-------------------------------------------------------------------

-module(mod_inbox_odbc_mssql).
-author("piotr.nosek@erlang-solutions.com").

-include("mod_inbox.hrl").

-export([set_inbox/6, set_inbox_incr_unread/5]).

-import(mod_inbox_odbc, [esc_string/1]).

%% -----------------------------------------------------------
%% API
%% -----------------------------------------------------------

-spec set_inbox(Username :: jid:luser(),
                Server :: jid:lserver(),
                ToBareJid :: binary(),
                Content :: binary(),
                Count :: binary(),
                MsgId :: binary()) -> query_result().
set_inbox(Username, Server, ToBareJid, Content, Count, MsgId) ->
    Query = build_query(Username, Server, ToBareJid, Content, Count, MsgId),
    mongoose_rdbms:sql_query(Server, Query).


-spec set_inbox_incr_unread(Username :: jid:luser(),
                            Server :: jid:lserver(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary()) -> query_result().
set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId) ->
    Query = build_query(Username, Server, ToBareJid, Content, increment, MsgId),
    mongoose_rdbms:sql_query(Server, Query).

%% -----------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------

build_query(Username, Server, ToBareJid, Content, increment, MsgId) ->
    CountUpdate = "target.unread_count + 1",
    build_query(Username, Server, ToBareJid, Content, MsgId, esc_string("1"), CountUpdate);
build_query(Username, Server, ToBareJid, Content, CountValue, MsgId) ->
    ECount = esc_string(CountValue),
    build_query(Username, Server, ToBareJid, Content, MsgId, ECount, ECount).

build_query(Username, Server, ToBareJid, Content, MsgId, CountInsert, CountUpdate) ->
    ELUser = esc_string(Username),
    ELServer = esc_string(Server),
    EToBareJid = esc_string(ToBareJid),
    EContent = mongoose_rdbms:use_escaped_binary(mongoose_rdbms:escape_binary(Server, Content)),
    EMsgId = esc_string(MsgId),

    ["MERGE INTO inbox WITH (SERIALIZABLE) AS target"
      " USING (SELECT ",
                    ELUser, " as luser, ",
                    ELServer, " as lserver, ",
                    EToBareJid, " as remote_bare_jid, ",
                    EContent, " as content, ",
                    CountInsert, " as unread_count, ",
                    EMsgId, " as msg_id)"
      " AS source (luser, lserver, remote_bare_jid, content, unread_count, msg_id)"
          " ON (target.luser = source.luser"
          " AND target.lserver = source.lserver"
          " AND target.remote_bare_jid = source.remote_bare_jid)"
      " WHEN MATCHED THEN UPDATE"
          " SET content = ", EContent, ","
              " unread_count = ", CountUpdate, ","
              " msg_id = ", EMsgId,
      " WHEN NOT MATCHED THEN INSERT"
          " (luser, lserver, remote_bare_jid, content, unread_count, msg_id)"
      " VALUES (", ELUser, ", ", ELServer, ", ", EToBareJid, ", ", EContent, ", ",
                CountInsert, ", ", EMsgId, ");"].
