-module(mod_vcard_backend).

-define(MAIN_MODULE, mod_vcard).

-export([init/2]).
-export([remove_user/3]).
-export([get_vcard/3]).
-export([set_vcard/5]).
-export([search/3]).
-export([search_fields/2]).
-export([search_reported_fields/3]).
-export([remove_domain/2]).
-export([tear_down/1]).

%%--------------------------------------------------------------------
%% callbacks
%%--------------------------------------------------------------------
-callback init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().

-callback remove_user(HostType, LUser, LServer) -> any() when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver().

-callback remove_domain(HostType, Domain) -> ok when
    HostType :: mongooseim:host_type(),
    Domain :: jid:lserver().

-callback set_vcard(HostType, LUser, LServer, VCard, VCardSearch) ->
    ok | {error, Reason :: term()} when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    VCard :: term(),
    VCardSearch :: term().

-callback get_vcard(HostType, LUser, LServer) ->
    {ok, Vcard :: term()} | {error, Reason :: term()} when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver().

-callback search(HostType, LServer, Data) ->
    Res :: term() when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    Data :: term().

-callback search_fields(HostType, LServer) ->
    Res :: list() when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver().

-callback search_reported_fields(HostType, LServer, Lang) ->
    Res :: term() when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    Lang :: binary().

-callback tear_down(HostType) -> ok when
    HostType :: mongooseim:host_type().

-optional_callbacks([tear_down/1, remove_domain/2]).


%%--------------------------------------------------------------------
%% specs
%%--------------------------------------------------------------------
-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [set_vcard, get_vcard, search],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> any().
remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_vcard(mongooseim:host_type(), jid:luser(), jid:lserver(), VCard, VCardSearch) ->
    ok | {error, Reason :: term()} when
    VCard :: term(),
    VCardSearch :: term().
set_vcard(HostType, LUser, LServer, VCard, VCardSearch) ->
    Args = [HostType, LUser, LServer, VCard, VCardSearch],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_vcard(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    {ok, Vcard :: term()} | {error, Reason :: term()}.
get_vcard(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec search(mongooseim:host_type(), jid:lserver(), Data) ->
    Res :: [[mongoose_data_forms:field()]] when
    Data :: term().
search(HostType, LServer, Data) ->
    Args = [HostType, LServer, Data],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec search_fields(mongooseim:host_type(), jid:lserver()) -> list().
search_fields(HostType, LServer) ->
    Args = [HostType, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec search_reported_fields(mongooseim:host_type(), jid:lserver(), Lang) ->
    Res :: [mongoose_data_forms:field()] when
    Lang :: binary().
search_reported_fields(HostType, LServer, Lang) ->
    Args = [HostType, LServer, Lang],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    case mongoose_backend:is_exported(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, 2) of
        true ->
            Args = [HostType, LServer],
            mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args);
        false ->
            ok
    end.

-spec tear_down(mongooseim:host_type()) -> ok.
tear_down(HostType) ->
    case mongoose_backend:is_exported(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, 1) of
        true ->
            mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]);
        false ->
            ok
    end.
