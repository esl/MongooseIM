-module(mod_stream_management_backend).
-export([init/2,
         register_smid/3,
         unregister_smid/2,
         get_sid/2]).

-define(MAIN_MODULE, mod_stream_management).

%% ----------------------------------------------------------------------
%% Callbacks
%% (exactly the same as specs in this module)

-callback init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().

-callback register_smid(HostType, SMID, SID) ->
    ok | {error, term()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    SID :: ejabberd_sm:sid().

-callback unregister_smid(mongooseim:host_type(), ejabberd_sm:sid()) ->
    {ok, SMID :: mod_stream_management:smid()} | {error, smid_not_found}.

-callback get_sid(mongooseim:host_type(), mod_stream_management:smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.

%% ----------------------------------------------------------------------
%% API Functions

-spec init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().
init(HostType, Opts) ->
    TrackedFuns = [],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec register_smid(HostType, SMID, SID) ->
    ok | {error, term()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    SID :: ejabberd_sm:sid().
register_smid(HostType, SMID, SID) ->
    Args = [HostType, SMID, SID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec unregister_smid(mongooseim:host_type(), ejabberd_sm:sid()) ->
    {ok, SMID :: mod_stream_management:smid()} | {error, smid_not_found}.
unregister_smid(HostType, SID) ->
    Args = [HostType, SID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sid(mongooseim:host_type(), mod_stream_management:smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.
get_sid(HostType, SMID) ->
    Args = [HostType, SMID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
