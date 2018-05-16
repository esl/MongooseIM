%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, <COMPANY>
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
         set_inbox/7,
         set_inbox_incr_unread/6,
         reset_unread/4,
         remove_inbox/3,
         clear_inbox/2]).

-import(mongoose_rdbms, [escape_string/1]).


init(_VHost, _Options) ->
  ok.
-spec get_inbox(LUser, LServer) -> ok when
                LUser  :: binary(),
                LServer :: binary().
get_inbox(User, Server) ->
  LUser = mongoose_rdbms:escape_string(jid:nameprep(User)),
  LServer = jid:nameprep(Server),
  case rdbms_queries:get_inbox(LUser, LServer) of
    {selected, []} ->
      [];
    {selected, Res} ->
      [decode_row(LServer, R) || R <- Res]
  end.

-spec set_inbox(User :: binary(),
               Server :: binary(),
               ToBareJid :: binary(),
               ToResource :: binary(),
               Content :: binary(),
               Count :: binary(),
               MsgId :: binary()) -> ok.
set_inbox(User, Server, ToBareJid, Sender, Content, C, MsgId) ->
  Username = mongoose_rdbms:escape_string(jid:nameprep(User)),
  LServer = jid:nameprep(Server),
  EscToBareJid = mongoose_rdbms:escape_string(jid:nameprep(ToBareJid)),
  EscSender = mongoose_rdbms:escape_string(jid:nameprep(Sender)),
  EscContent = mongoose_rdbms:escape_string(Content),
  EscCount = mongoose_rdbms:escape_string(C),
  EscMsgId = mongoose_rdbms:escape_string(MsgId),
  Res = rdbms_queries:set_inbox(Username, LServer, EscToBareJid, EscSender, EscContent, EscCount, EscMsgId),
  check_result(Res, 1).

-spec remove_inbox(User :: binary(),
                   Server :: binary(),
                   ToBareJid :: binary()) -> ok.
remove_inbox(User, Server, ToBareJid) ->
  Username = mongoose_rdbms:escape_string(jid:nameprep(User)),
  LServer = jid:nameprep(Server),
  EscToBareJid = mongoose_rdbms:escape_string(jid:nameprep(ToBareJid)),
  Res = rdbms_queries:remove_inbox(Username, LServer, EscToBareJid),
  check_result(Res).

-spec set_inbox_incr_unread(User :: binary(),
                            Server :: binary(),
                            ToBareJid :: binary(),
                            ToResource :: binary(),
                            Content :: binary(),
                            MsgId :: binary()) -> ok.
set_inbox_incr_unread(User, Server, ToBareJid, Sender, Content, MsgId) ->
  Username = mongoose_rdbms:escape_string(jid:nameprep(User)),
  LServer = jid:nameprep(Server),
  EscToBareJid = mongoose_rdbms:escape_string(jid:nameprep(ToBareJid)),
  EscSender = mongoose_rdbms:escape_string(jid:nameprep(Sender)),
  EscContent = mongoose_rdbms:escape_string(Content),
  EscMsgId =  mongoose_rdbms:escape_string(MsgId),
  Res = rdbms_queries:set_inbox_incr_unread(Username, LServer, EscToBareJid, EscSender, EscContent, EscMsgId),
  check_result(Res, 1).

-spec reset_unread(User :: binary(),
                   Server :: binary(),
                   BareJid :: binary(),
                   MsgId :: binary()) -> ok.
reset_unread(User, Server, ToBareJid, Id) ->
  Username = mongoose_rdbms:escape_string(jid:nameprep(User)),
  LServer = jid:nameprep(Server),
  EscToBareJid = mongoose_rdbms:escape_string(jid:nameprep(ToBareJid)),
  EscId = mongoose_rdbms:escape_string(Id),
  Res = rdbms_queries:reset_inbox_unread(Username, LServer, EscToBareJid, EscId),
  check_result(Res).

-spec clear_inbox(User :: binary(),  Server :: binary()) -> ok.
clear_inbox(Username, Server) ->
  EscUsername = mongoose_rdbms:escape_string(jid:nameprep(Username)),
  LServer = jid:nameprep(Server),
  Res = rdbms_queries:clear_inbox(EscUsername, LServer),
  check_result(Res).

-spec decode_row(host(), {username(), sender(), binary(), count()}) -> inbox_res().
decode_row(LServer, {Username, Sender, Content, Count}) ->
  Pool = mongoose_rdbms_sup:pool(LServer),
  Data = mongoose_rdbms:unescape_binary(Pool, Content),
  {Username, Sender, Data, Count}.

check_result({updated, Val}, Val) ->
  ok.

check_result({updated, _}) ->
  ok.