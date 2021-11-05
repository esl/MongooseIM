-module(mod_privacy_backend).

-define(MAIN_MODULE, mod_privacy).

-export([init/2,
         get_default_list/3,
         get_list_names/3,
         get_privacy_list/4,
         set_default_list/4,
         forget_default_list/3,
         remove_privacy_list/4,
         replace_privacy_list/5,
         remove_user/3,
         remove_domain/2]).

%% ------------------------------------------------------------------
%% Backend callbacks
%% ------------------------------------------------------------------

-callback init(HostType, Opts) -> ok when
      HostType :: mongooseim:host_type(),
      Opts     :: gen_mod:module_opts().

-callback remove_user(HostType, LUser, LServer) -> any() when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver().

-callback remove_domain(HostType, LServer) -> any() when
      HostType :: mongooseim:host_type(),
      LServer :: jid:lserver().

-callback get_list_names(HostType, LUser, LServer) ->
    {ok, {Default, Names}} | {error, Reason} when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver(),
      Default :: mod_privacy:list_name(),
      Names   :: [mod_privacy:list_name()],
      Reason  :: not_found | term().

-callback get_privacy_list(HostType, LUser, LServer, Name) ->
    {ok, Items} | {error, Reason} when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver(),
      Name    :: mod_privacy:list_name(),
      Items   :: [mod_privacy:list_item()],
      Reason  :: not_found | term().

-callback set_default_list(HostType, LUser, LServer, Name) ->
    ok | {error, Reason} when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver(),
      Name    :: mod_privacy:list_name(),
      Reason  :: not_found | term().

-callback forget_default_list(HostType, LUser, LServer) ->
    ok | {error, Reason} when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver(),
      Reason  :: not_found | term().

-callback remove_privacy_list(HostType, LUser, LServer, Name) ->
    ok | {error, Reason} when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver(),
      Name    :: mod_privacy:list_name(),
      Reason  :: conflict | term().

-callback replace_privacy_list(HostType, LUser, LServer, Name, Items) ->
    ok | {error, Reason} when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver(),
      Name    :: mod_privacy:list_name(),
      Items   :: [mod_privacy:list_item()],
      Reason  :: conflict | term().

-callback get_default_list(HostType, LUser, LServer) ->
    {ok, {Default, Items}} | {error, Reason} when
      HostType :: mongooseim:host_type(),
      LUser   :: jid:luser(),
      LServer :: jid:lserver(),
      Default :: mod_privacy:list_name(),
      Items   :: [mod_privacy:list_item()],
      Reason  :: not_found | term().

%% ------------------------------------------------------------------
%% Backend implementation
%% ------------------------------------------------------------------

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [get_privacy_list, get_list_names, set_default_list, forget_default_list,
                   remove_privacy_list, replace_privacy_list, get_default_list],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> any().
remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> any().
remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_list_names(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    {ok, {Default, Names}} | {error, Reason} when
      Default :: mod_privacy:list_name(),
      Names   :: [mod_privacy:list_name()],
      Reason  :: not_found | term().
get_list_names(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_privacy_list(mongooseim:host_type(), jid:luser(), jid:lserver(), Name) ->
    {ok, Items} | {error, Reason} when
      Name    :: mod_privacy:list_name(),
      Items   :: [mod_privacy:list_item()],
      Reason  :: not_found | term().
get_privacy_list(HostType, LUser, LServer, Name) ->
    Args = [HostType, LUser, LServer, Name],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_default_list(mongooseim:host_type(), jid:luser(), jid:lserver(), Name) ->
    ok | {error, Reason} when
      Name    :: mod_privacy:list_name(),
      Reason  :: not_found | term().
set_default_list(HostType, LUser, LServer, Name) ->
    Args = [HostType, LUser, LServer, Name],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec forget_default_list(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    ok | {error, Reason} when
      Reason  :: not_found | term().
forget_default_list(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_privacy_list(mongooseim:host_type(), jid:luser(), jid:lserver(), Name) ->
    ok | {error, Reason} when
      Name    :: mod_privacy:list_name(),
      Reason  :: conflict | term().
remove_privacy_list(HostType, LUser, LServer, Name) ->
    Args = [HostType, LUser, LServer, Name],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec replace_privacy_list(mongooseim:host_type(), jid:luser(), jid:lserver(), Name, Items) ->
    ok | {error, Reason} when
      Name    :: mod_privacy:list_name(),
      Items   :: [mod_privacy:list_item()],
      Reason  :: conflict | term().
replace_privacy_list(HostType, LUser, LServer, Name, List) ->
    Args = [HostType, LUser, LServer, Name, List],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_default_list(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    {ok, {Default, Items}} | {error, Reason} when
      Default :: mod_privacy:list_name(),
      Items   :: [mod_privacy:list_item()],
      Reason  :: not_found | term().
get_default_list(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
