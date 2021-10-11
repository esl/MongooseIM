-module(mod_private_backend).
-export([init/2,
         multi_set_data/4,
         multi_get_data/4,
         get_all_nss/3,
         remove_user/3,
         remove_domain/2]).

-define(MAIN_MODULE, mod_private).
-include("backend_module.hrl").

-type ns() :: binary().

%% ----------------------------------------------------------------------
%% Callbacks

-callback init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts     :: list().

-callback multi_set_data(HostType, LUser, LServer, NS2XML) -> Result when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    NS2XML  :: [{NS, XML}],
    NS      :: ns(),
    XML     :: exml:element(),
    Reason  :: term(),
    Result  :: ok | {aborted, Reason} | {error, Reason}.

-callback multi_get_data(HostType, LUser, LServer, NS2Def) -> [XML | Default] when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    NS2Def  :: [{NS, Default}],
    NS      :: ns(),
    Default :: term(),
    XML     :: exml:element().

-callback remove_user(HostType, LUser, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver().

-callback remove_domain(HostType, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver().

-callback get_all_nss(HostType, LUser, LServer) -> NSs when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    NSs     :: [ns()].

%% ----------------------------------------------------------------------
%% specs

-spec init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts     :: list().

-spec multi_set_data(HostType, LUser, LServer, NS2XML) -> Result when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    NS2XML  :: [{NS, XML}],
    NS      :: ns(),
    XML     :: exml:element(),
    Reason  :: term(),
    Result  :: ok | {aborted, Reason} | {error, Reason}.

-spec multi_get_data(HostType, LUser, LServer, NS2Def) -> [XML | Default] when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    NS2Def  :: [{NS, Default}],
    NS      :: ns(),
    Default :: term(),
    XML     :: exml:element().

-spec remove_user(HostType, LUser, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver().

-spec remove_domain(HostType, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver().

-spec get_all_nss(HostType, LUser, LServer) -> NSs when
    HostType :: mongooseim:host_type(),
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    NSs     :: [ns()].

%% ----------------------------------------------------------------------
%% Functions

tracked_funs() ->
    [multi_get_data, multi_set_data].

init(HostType, Opts) ->
    backend_module:ensure_backend_metrics(?MAIN_MODULE, tracked_funs()),
    ?CALL(HostType, (HostType, Opts)).

multi_set_data(HostType, LUser, LServer, NS2XML) ->
    ?CALL_TRACKED(HostType, (HostType, LUser, LServer, NS2XML)).

multi_get_data(HostType, LUser, LServer, NS2Def) ->
    ?CALL_TRACKED(HostType, (HostType, LUser, LServer, NS2Def)).

get_all_nss(HostType, LUser, LServer) ->
    ?CALL(HostType, (HostType, LUser, LServer)).

remove_user(HostType, LUser, LServer) ->
    ?CALL(HostType, (HostType, LUser, LServer)).

remove_domain(HostType, LServer) ->
    ?CALL(HostType, (HostType, LServer)).
