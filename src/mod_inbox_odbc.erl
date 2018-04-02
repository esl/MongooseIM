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
-include("mod_inbox.hrl").

%% API
-export([get_inbox/2,
         init/2,
         set_inbox/7,
         set_inbox_incr_unread/6,
         reset_unread/4,
         remove_inbox/3,
         clear_inbox/2]).


init(_VHost, _Options) ->
  ok.
-spec get_inbox(LUser, LServer) -> ok when
                LUser  :: binary(),
                LServer :: binary().
get_inbox(LUser, LServer) ->
  U = jid:nameprep(mongoose_rdbms:escape(LUser)),
  S = jid:nameprep(mongoose_rdbms:escape(LServer)),
  case rdbms_queries:get_inbox(U, S) of
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
  Username = jid:nameprep(mongoose_rdbms:escape(User)),
  EscServer = jid:nameprep(mongoose_rdbms:escape(Server)),
  EscToBareJid = jid:nameprep(mongoose_rdbms:escape(ToBareJid)),
  EscSender = jid:nameprep(mongoose_rdbms:escape(Sender)),
  EscContent = mongoose_rdbms:escape(Content),
  EscCount = mongoose_rdbms:escape(C),
  EscMsgId = mongoose_rdbms:escape(MsgId),
  rdbms_queries:set_inbox(Username, EscServer, EscToBareJid, EscSender, EscContent, EscCount, EscMsgId),
  ok.

-spec remove_inbox(User :: binary(),
                   Server :: binary(),
                   ToBareJid :: binary()) -> ok.
remove_inbox(User, Server, ToBareJid) ->
  Username = jid:nameprep(mongoose_rdbms:escape(User)),
  EscServer = jid:nameprep(mongoose_rdbms:escape(Server)),
  EscToBareJid = jid:nameprep(mongoose_rdbms:escape(ToBareJid)),
  rdbms_queries:remove_inbox(Username, EscServer, EscToBareJid),
  ok.

-spec set_inbox_incr_unread(User :: binary(),
                            Server :: binary(),
                            ToBareJid :: binary(),
                            ToResource :: binary(),
                            Content :: binary(),
                            MsgId :: binary()) -> ok.
set_inbox_incr_unread(User, Server, ToBareJid, Sender, Content, MsgId) ->
  Username = jid:nameprep(mongoose_rdbms:escape(User)),
  EscServer = jid:nameprep(mongoose_rdbms:escape(Server)),
  EscToBareJid = jid:nameprep(mongoose_rdbms:escape(ToBareJid)),
  EscSender = jid:nameprep(mongoose_rdbms:escape(Sender)),
  EscContent = mongoose_rdbms:escape(Content),
  EscMsgId =  mongoose_rdbms:escape(MsgId),
  rdbms_queries:set_inbox_incr_unread(Username, EscServer, EscToBareJid, EscSender, EscContent, EscMsgId),
  ok.

-spec reset_unread(User :: binary(),
                   Server :: binary(),
                   BareJid :: binary(),
                   MsgId :: binary()) -> ok.
reset_unread(User, Server, ToBareJid, Id) ->
  Username = jid:nameprep(mongoose_rdbms:escape(User)),
  EscServer = jid:nameprep(mongoose_rdbms:escape(Server)),
  EscToBareJid = jid:nameprep(mongoose_rdbms:escape(ToBareJid)),
  EscId = mongoose_rdbms:escape(Id),
  rdbms_queries:reset_inbox_unread(Username, EscServer, EscToBareJid, EscId),
  ok.

-spec clear_inbox(User :: binary(),  Server :: binary()) -> ok.
clear_inbox(Username, Server) ->
  EscUsername = mongoose_rdbms:escape(jid:nameprep(Username)),
  EscServer = mongoose_rdbms:escape(jid:nameprep(Server)),
  rdbms_queries:clear_inbox(EscUsername, EscServer),
  ok.

-spec decode_row(host(), {username(), sender(), binary(), count()}) -> inbox_res().
decode_row(LServer, {Username, Sender, Content, Count}) ->
  EscFormat = mongoose_rdbms:escape(LServer),
  DbEngine = mongoose_rdbms:db_engine(LServer),
  SData = mongoose_rdbms:unescape_binary(DbEngine, Content),
  Data = mongoose_rdbms:unescape_binary(EscFormat, SData),
  {Username, Sender, Data, Count}.
