%% Just a proxy interface module between the main mod_inbox module and
%% the backend modules (i.e. mod_inbox_rdbms).
-module(mod_inbox_backend).

-export([init/2,
         get_inbox/4,
         clear_inbox/3,
         remove_domain/2,
         set_inbox/6,
         remove_inbox_row/2,
         set_inbox_incr_unread/5,
         get_inbox_unread/2,
         get_entry_properties/2,
         set_entry_properties/3,
         reset_unread/3]).

-define(MAIN_MODULE, mod_inbox).

-callback init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().

-callback get_inbox(HostType, LUser, LServer, Params) -> mod_inbox:get_inbox_res() when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Params :: mod_inbox:get_inbox_params().

-callback clear_inbox(HostType, LUser, LServer) -> mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver().

-callback remove_domain(HostType, LServer) -> ok when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver().

-callback set_inbox(HostType, InboxEntryKey, Content, Count, MsgId, Timestamp) ->
    mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Content :: binary(),
    Count :: integer(),
    MsgId :: binary(),
    Timestamp :: integer().

-callback remove_inbox_row(HostType, InboxEntryKey) -> mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key().

-callback set_inbox_incr_unread(HostType, InboxEntryKey, Content, MsgId, Timestamp) ->
    mod_inbox:count_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Content :: binary(),
    MsgId :: binary(),
    Timestamp :: integer().

-callback reset_unread(HostType, InboxEntryKey, MsgId) -> mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    MsgId :: binary().

-callback get_inbox_unread(HostType, InboxEntryKey) -> {ok, integer()} when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key().

-callback get_entry_properties(HostType, InboxEntryKey) -> Ret when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Ret :: mod_inbox:entry_properties() | nil().

-callback set_entry_properties(HostType, InboxEntryKey, Params) -> Ret when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Params :: mod_inbox:entry_properties(),
    Ret :: mod_inbox:entry_properties() | {error, binary()}.

-spec init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, callback_funs(), Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_inbox(HostType, LUser, LServer, Params) -> mod_inbox:get_inbox_res() when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Params :: mod_inbox:get_inbox_params().
get_inbox(HostType, LUser, LServer, Params) ->
    Args = [HostType, LUser, LServer, Params],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec clear_inbox(HostType, LUser, LServer) -> mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver().
clear_inbox(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(HostType, LServer) -> ok when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver().
remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_inbox(HostType, InboxEntryKey, Content, Count, MsgId, Timestamp) ->
    mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Content :: binary(),
    Count :: integer(),
    MsgId :: binary(),
    Timestamp :: integer().
set_inbox(HostType, InboxEntryKey, Content, Count, MsgId, Timestamp) ->
    Args = [HostType, InboxEntryKey, Content, Count, MsgId, Timestamp],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_inbox_row(HostType, InboxEntryKey) -> mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key().
remove_inbox_row(HostType, InboxEntryKey) ->
    Args = [HostType, InboxEntryKey],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_inbox_incr_unread(HostType, InboxEntryKey, Content, MsgId, Timestamp) ->
    mod_inbox:count_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Content :: binary(),
    MsgId :: binary(),
    Timestamp :: integer().
set_inbox_incr_unread(HostType, InboxEntryKey, Content, MsgId, Timestamp) ->
    Args = [HostType, InboxEntryKey, Content, MsgId, Timestamp],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec reset_unread(HostType, InboxEntryKey, MsgId) -> mod_inbox:write_res() when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    MsgId :: binary() | undefined.
reset_unread(HostType, InboxEntryKey, MsgId) ->
    Args = [HostType, InboxEntryKey, MsgId],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_inbox_unread(HostType, InboxEntryKey) -> {ok, integer()} when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key().
get_inbox_unread(HostType, InboxEntryKey) ->
    Args = [HostType, InboxEntryKey],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_entry_properties(HostType, InboxEntryKey) -> Ret when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Ret :: mod_inbox:entry_properties() | nil().
get_entry_properties(HostType, InboxEntryKey) ->
    Args = [HostType, InboxEntryKey],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_entry_properties(HostType, InboxEntryKey, Params) -> Ret when
    HostType :: mongooseim:host_type(),
    InboxEntryKey :: mod_inbox:entry_key(),
    Params :: mod_inbox:entry_properties(),
    Ret :: mod_inbox:entry_properties() | {error, binary()}.
set_entry_properties(HostType, InboxEntryKey, Params) ->
    Args = [HostType, InboxEntryKey, Params],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

callback_funs() ->
  [get_inbox, set_inbox, set_inbox_incr_unread,
   reset_unread, remove_inbox_row, clear_inbox, get_inbox_unread,
   get_entry_properties, set_entry_properties, remove_domain].
