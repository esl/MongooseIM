%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2018 12:51
%%%-------------------------------------------------------------------
-module(mod_inbox_odbc_pgsql).

-author("ludwikbukowski").

-include("mod_inbox.hrl").

-export([set_inbox/6, set_inbox_incr_unread/5]).

-import(mod_inbox_odbc, [esc_string/1]).

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------

-spec set_inbox(Username :: jid:luser(),
                Server :: jid:lserver(),
                ToBareJid :: binary(),
                Content :: binary(),
                Count :: binary(),
                MsgId :: binary()) -> query_result().
set_inbox(Username, Server, ToBareJid, Content, Count, MsgId) ->
    mongoose_rdbms:sql_query(Server,
        ["insert into inbox(luser, lserver, remote_bare_jid, content, unread_count, msg_id) "
                "values (", esc_string(Username), ",", esc_string(Server), ",",
                            esc_string(ToBareJid), ",", esc_string(Content), ",",
                            esc_string(Count), ",", esc_string(MsgId),
                       ") on conflict (luser, lserver, remote_bare_jid) do "
            "update set content=", esc_string(Content),
                     ", unread_count=", esc_string(Count),
                     ", msg_id=", esc_string(MsgId), ";"]).

-spec set_inbox_incr_unread(Username :: jid:luser(),
                            Server :: jid:lserver(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary()) -> query_result().
set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId) ->
    mongoose_rdbms:sql_query(Server,
        ["insert into inbox(luser, lserver, remote_bare_jid, content, unread_count, msg_id) "
                "values (", esc_string(Username), ", ", esc_string(Server), ",",
                            esc_string(ToBareJid), ", ", esc_string(Content), ", ", "1", ", ",
                            esc_string(MsgId), ") on conflict (luser, lserver, remote_bare_jid) do "
            "update set content=", esc_string(Content), ", "
                       "unread_count=inbox.unread_count + 1, "
                       "msg_id=", esc_string(MsgId), ";"]).

