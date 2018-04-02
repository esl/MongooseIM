-type inbox_res() :: {username(), sender(), content(), count()}.
-type username() :: binary().
-type sender() :: binary().
-type content() :: binary().
-type count() :: binary().
-type id() :: binary().
-type host() :: binary().

-callback init(Host, Opts) -> ok when
  Host :: binary(),
  Opts :: list().

-callback get_inbox(LUser, LServer) -> any() when
  LUser :: binary(),
  LServer :: binary().

-callback set_inbox(User, Server, ToBareJid, ToResource, Content, Count, MsgId) -> any() when
  User :: binary(),
  Server :: binary(),
  ToBareJid :: binary(),
  ToResource :: binary(),
  Content :: binary(),
  Count :: binary(),
  MsgId :: binary().

-callback remove_inbox(User, Server, ToBareJid) -> any() when
  User :: binary(),
  Server :: binary(),
  ToBareJid :: binary().

-callback set_inbox_incr_unread(User, Server, ToBareJid, ToResource, Content, MsgId) -> any() when
  User :: binary(),
  Server :: binary(),
  ToBareJid :: binary(),
  ToResource :: binary(),
  Content :: binary(),
  MsgId :: binary().

-callback reset_unread(User, Server, BareJid, MsgId) -> any() when
  User :: binary(),
  Server :: binary(),
  BareJid :: binary(),
  MsgId :: binary().

-callback clear_inbox(User, Server) -> any() when
  User :: binary(),
  Server :: binary().
