%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2018 12:51
%%%-------------------------------------------------------------------
-module(mod_inbox_odbc_psql).
-author("ludwikbukowski").

%% API
-export([get_inbox/2, get_inbox_unread/3, set_inbox/6,
         remove_inbox/3, set_inbox_incr_unread/5,
         reset_inbox_unread/4, clear_inbox/2]).

-define(ESC(T), mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(T))).

get_inbox(LUser, Server) ->
  mongoose_rdbms:sql_query(
    Server,
    ["select remote_bare_jid, content, unread_count from inbox "
    "where luser=", ?ESC(LUser), " and lserver=",?ESC(Server),";"]).

get_inbox_unread(LUser, Server, ToBareJid) ->
  mongoose_rdbms:sql_query(
    Server,
    ["select unread_count from inbox where luser=", ?ESC(LUser)," and lserver=",?ESC(Server),
      " and remote_bare_jid=", ?ESC(ToBareJid), ";"]).


set_inbox(Username, Server, ToBareJid, Content, C, MsgId) ->
    mongoose_rdbms:sql_query(Server, ["insert into inbox(luser, lserver, remote_bare_jid, content,
    unread_count, msg_id) values (", ?ESC(Username), ",",?ESC(Server),",", ?ESC(ToBareJid), ",",
      ?ESC(Content), ",", ?ESC(C), ",", ?ESC(MsgId),
      ") on conflict(luser, lserver, remote_bare_jid)do update set content=",
    ?ESC(Content), ", unread_count=", ?ESC(C),",msg_id=", ?ESC(MsgId), ";"]).

remove_inbox(Username, Server, ToBareJid) ->
  mongoose_rdbms:sql_query(Server, ["delete from inbox where luser=",
    ?ESC(Username)," and lserver=",?ESC(Server),
    " and remote_bare_jid=",
    ?ESC(ToBareJid),";"]).

set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId) ->
    mongoose_rdbms:sql_query(Server, ["insert into inbox(luser, lserver, remote_bare_jid,
    content, unread_count, msg_id) values (", ?ESC(Username), ", ", ?ESC(Server),",",
    ?ESC(ToBareJid), ", ", ?ESC(Content), ", ", "1", ", ",
    ?ESC(MsgId), ") on conflict(luser, lserver, remote_bare_jid) do update set content=",
    ?ESC(Content), ", unread_count=inbox.unread_count + 1, msg_id=",
    ?ESC(MsgId),";"]).


reset_inbox_unread(Username, Server, ToBareJid, MsgId) ->
    mongoose_rdbms:sql_query(Server, ["update inbox set unread_count=0 where luser=",
    ?ESC(Username)," and lserver=",?ESC(Server), " and remote_bare_jid=",
    ?ESC(ToBareJid)," and msg_id=", ?ESC(MsgId), ";"]).

clear_inbox(Username, Server) ->
  mongoose_rdbms:sql_query(Server, ["delete from inbox where luser=",
                           ?ESC(Username), " and lserver=",?ESC(Server), ";"]).

