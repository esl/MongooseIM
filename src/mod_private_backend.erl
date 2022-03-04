%% Just a proxy interface module between the main mod_private module and
%% the backend modules (i.e. mod_private_rdbms, mod_private_mnesia...).
-module(mod_private_backend).
-export([init/2,
         multi_set_data/4,
         multi_get_data/4,
         get_all_nss/3,
         remove_user/3,
         remove_domain/2]).

-define(MAIN_MODULE, mod_private).

-type ns() :: binary().
-type ns_xml() :: {ns(), exml:element()}.

%% ----------------------------------------------------------------------
%% Callbacks
%% (exactly the same as specs in this module)

-callback init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().

-callback multi_set_data(HostType, LUser, LServer, NS2XML) -> Result when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    NS2XML :: [ns_xml()],
    Reason :: term(),
    Result :: ok | {aborted, Reason} | {error, Reason}.

-callback multi_get_data(HostType, LUser, LServer, NS2Def) -> [XML | Default] when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    NS2Def :: [{NS, Default}],
    NS :: ns(),
    Default :: term(),
    XML :: exml:element().

-callback remove_user(HostType, LUser, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver().

-callback remove_domain(HostType, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver().

-callback get_all_nss(HostType, LUser, LServer) -> NSs when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    NSs :: [ns()].

%% ----------------------------------------------------------------------
%% API Functions

-spec init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().
init(HostType, Opts) ->
    TrackedFuns = [multi_get_data, multi_set_data],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec multi_set_data(HostType, LUser, LServer, NS2XML) -> Result when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    NS2XML :: [ns_xml()],
    Reason :: term(),
    Result :: ok | {aborted, Reason} | {error, Reason}.
multi_set_data(HostType, LUser, LServer, NS2XML) ->
    Args = [HostType, LUser, LServer, NS2XML],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec multi_get_data(HostType, LUser, LServer, NS2Def) -> [XML | Default] when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    NS2Def :: [{NS, Default}],
    NS :: ns(),
    Default :: term(),
    XML :: exml:element().
multi_get_data(HostType, LUser, LServer, NS2Def) ->
    Args = [HostType, LUser, LServer, NS2Def],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(HostType, LUser, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver().
remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(HostType, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver().
remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_all_nss(HostType, LUser, LServer) -> NSs when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    NSs :: [ns()].
get_all_nss(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
