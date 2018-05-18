%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2018 12:51
%%%-------------------------------------------------------------------
-module(mod_inbox_odbc_mysql).
-author("ludwikbukowski").
-include("mod_inbox.hrl").
%% API
-export([set_inbox/6, set_inbox_incr_unread/5]).
-define(ESC(T), mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(T))).


-spec set_inbox(Username :: jid:luser(),
                Server :: jid:lserver(),
                ToBareJid :: binary(),
                Content :: binary(),
                Count :: binary(),
                MsgId :: binary()) -> query_result().
set_inbox(Username, Server, ToBareJid, Content, Count, MsgId) ->
    mongoose_rdbms:sql_query(Server, ["insert into inbox(luser, lserver, remote_bare_jid, content,
    unread_count, msg_id) values (", ?ESC(Username), ",", ?ESC(Server), ",", ?ESC(ToBareJid), ",",
        ?ESC(Content), ",", ?ESC(Count), ",", ?ESC(MsgId),
        ") on duplicate key update content=",
        ?ESC(Content), ", unread_count=", ?ESC(Count), ",msg_id=", ?ESC(MsgId), ";"]).


-spec set_inbox_incr_unread(Username :: jid:luser(),
                            Server :: jid:lserver(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary()) -> query_result().
set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId) ->
    mongoose_rdbms:sql_query(Server, ["insert into inbox(luser, lserver, remote_bare_jid,
    content, unread_count, msg_id) values (", ?ESC(Username), ", ", ?ESC(Server), ",",
        ?ESC(ToBareJid), ", ", ?ESC(Content), ", ", "1", ", ",
        ?ESC(MsgId), ") on duplicate key update content=",
        ?ESC(Content), ", unread_count=inbox.unread_count + 1, msg_id=",
        ?ESC(MsgId), ";"]).



