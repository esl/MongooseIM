-module(mongoose_s2s_backend).

-callback init(map()) -> any().
-callback get_s2s_out_pids(ejabberd_s2s:fromto()) ->
    {ok, [pid()]} | {error, Reason :: term()}.
-callback try_register(Pid :: pid(),
                       ShouldWriteF :: fun(),
                       FromTo :: ejabberd_s2s:fromto()) -> boolean().
-callback remove_connection(FromTo :: ejabberd_s2s:fromto(), Pid :: pid()) -> ok.
-callback node_cleanup(Node :: node()) -> term().
-callback register_secret(HostType :: mongooseim:host_type(),
                          Source :: ejabberd_s2s:secret_source(),
                          Secret :: ejabberd_s2s:base16_secret()) -> ok.
-callback get_shared_secret(mongooseim:host_type()) ->
    {ok, {ejabberd_s2s:secret_source(), ejabberd_s2s:base16_secret()}} | {error, not_found}.

-export([init/1,
         get_s2s_out_pids/1,
         try_register/3,
         remove_connection/2,
         node_cleanup/1]).

-export([register_secret/3,
         get_shared_secret/1]).

-ignore_xref([behaviour_info/1]).

-define(MAIN_MODULE, mongoose_s2s).

-spec init(map()) -> any().
init(Opts) ->
    Args = [Opts],
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_s2s_out_pids(ejabberd_s2s:fromto()) ->
    {ok, [pid()]} | {error, Reason :: term()}.
get_s2s_out_pids(FromTo) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [FromTo]).

%% Register ejabberd_s2s_out connection
-spec try_register(Pid :: pid(),
                   ShouldWriteF :: fun(),
                   FromTo :: ejabberd_s2s:fromto()) -> boolean().
try_register(Pid, ShouldWriteF, FromTo) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Pid, ShouldWriteF, FromTo]).

-spec remove_connection(FromTo :: ejabberd_s2s:fromto(), Pid :: pid()) -> ok.
remove_connection(FromTo, Pid) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [FromTo, Pid]).

-spec node_cleanup(Node :: node()) -> term().
node_cleanup(Node) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Node]).

-spec register_secret(HostType :: mongooseim:host_type(),
                      Source :: ejabberd_s2s:secret_source(),
                      Secret :: ejabberd_s2s:base16_secret()) -> ok.
register_secret(HostType, Source, Secret) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Source, Secret]).

-spec get_shared_secret(mongooseim:host_type()) ->
    {ok, {ejabberd_s2s:secret_source(), ejabberd_s2s:base16_secret()}} | {error, not_found}.
get_shared_secret(HostType) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]).
