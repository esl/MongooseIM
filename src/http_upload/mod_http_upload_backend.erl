%% Just a proxy interface module between the main mod_http_upload module and
%% the backend modules (i.e. mod_http_upload_s3).
-module(mod_http_upload_backend).

-export([init/2, create_slot/7]).

-define(MAIN_MODULE, mod_http_upload).
%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback create_slot(UTCDateTime :: calendar:datetime(), UUID :: binary(),
                      Filename :: unicode:unicode_binary(), ContentType :: binary() | undefined,
                      Size :: pos_integer(), gen_mod:module_opts()) ->
    {PUTURL :: binary(), GETURL :: binary(), Headers :: #{binary() => binary()}}.

-spec init(HostType :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [create_slot], Opts).

-spec create_slot(HostType::mongooseim:host_type(),
                  UTCDateTime :: calendar:datetime(),
                  UUID :: binary(),
                  Filename :: unicode:unicode_binary(),
                  ContentType :: binary() | undefined,
                  Size :: pos_integer(),
                  Opts :: gen_mod:module_opts()) ->
    {PUTURL :: binary(), GETURL :: binary(), Headers :: #{binary() => binary()}}.
create_slot(HostType, UTCDateTime, UUID, Filename, ContentType, Size, Opts) ->
    Args  = [UTCDateTime, UUID, Filename, ContentType, Size, Opts],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
