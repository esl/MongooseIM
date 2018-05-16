%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 16:59
%%%-------------------------------------------------------------------
-module(mod_inbox_odbc).
-author("ludwikbukowski").
-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_inbox.hrl").

%% API
-export([get_inbox/2,
         init/2,
         set_inbox/6,
         set_inbox_incr_unread/5,
         reset_unread/4,
         remove_inbox/3,
         clear_inbox/2]).

-import(mongoose_rdbms, [escape_string/1]).

init(_VHost, _Options) ->
  ok.

-spec get_inbox(LUser, LServer) -> ok when
                LUser  :: binary(),
                LServer :: binary().
get_inbox(Username, Server) ->
  LUsername = jid:nodeprep(Username),
  LServer = jid:nameprep(Server),
  case mod_inbox_odbc_psql:get_inbox(LUsername, LServer) of
    {selected, []} ->
      [];
    {selected, Res} ->
      [decode_row(LServer, R) || R <- Res]
  end.

set_inbox(Username, Server, ToBareJid, Content, C, MsgId) ->
  LUsername = jid:nodeprep(Username),
  LServer = jid:nameprep(Server),
  LToBareJid = jid:nameprep(ToBareJid),
  Res = mod_inbox_odbc_psql:set_inbox(LUsername, LServer, LToBareJid, Content, C, MsgId),
  ok = check_result(Res, 1).

-spec remove_inbox(User :: binary(),
                   Server :: binary(),
                   ToBareJid :: binary()) -> ok.
remove_inbox(Username, Server, ToBareJid) ->
  LUsername = jid:nodeprep(Username),
  LServer = jid:nameprep(Server),
  LToBareJid = jid:nameprep(ToBareJid),
  Res = mod_inbox_odbc_psql:remove_inbox(LUsername, LServer, LToBareJid),
  check_result(Res).

-spec set_inbox_incr_unread(Username :: binary(),
                            Server :: binary(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary()) -> ok.
set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId) ->
  LUsername = jid:nodeprep(Username),
  LServer = jid:nameprep(Server),
  LToBareJid = jid:nameprep(ToBareJid),
  Res = mod_inbox_odbc_psql:set_inbox_incr_unread(LUsername, LServer, LToBareJid, Content, MsgId),
  check_result(Res, 1).

-spec reset_unread(User :: binary(),
                   Server :: binary(),
                   BareJid :: binary(),
                   MsgId :: binary()) -> ok.
reset_unread(Username, Server, ToBareJid, MsgId) ->
  LUsername = jid:nodeprep(Username),
  LServer = jid:nameprep(Server),
  LToBareJid = jid:nameprep(ToBareJid),
  Res = mod_inbox_odbc_psql:reset_inbox_unread(LUsername, LServer, LToBareJid, MsgId),
  check_result(Res).

-spec clear_inbox(Username :: binary(),  Server :: binary()) -> ok.
clear_inbox(Username, Server) ->
  LUsername = jid:nodeprep(Username),
  LServer = jid:nameprep(Server),
  Res = mod_inbox_odbc_psql:clear_inbox(LUsername, LServer),
  check_result(Res).

-spec decode_row(host(), {username(), sender(), binary(), count()}) -> inbox_res().
decode_row(LServer, {Username, Content, Count}) ->
  Pool = mongoose_rdbms_sup:pool(LServer),
  Data = mongoose_rdbms:unescape_binary(Pool, Content),
  {Username, Data, Count}.

check_result({updated, Val}, Val) ->
  ok;
check_result({updated, Res}, Exp) ->
  {error, {expected_does_not_match, Exp, Res}};
check_result(Result, _) ->
  {error, {bad_result, Result}}.

check_result({updated, _}) ->
  ok;
check_result(Result) ->
  {error, {bad_result, Result}}.