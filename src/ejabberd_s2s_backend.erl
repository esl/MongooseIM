-module(ejabberd_s2s_backend).

-callback init(map()) ->
    any().
-callback dirty_read_s2s_list_pids(ejabberd_s2s:fromto()) ->
    {ok, [pid()]} | {error, Reason :: term()}.

-export([init/1,
         dirty_read_s2s_list_pids/1,
         try_register/3,
         remove_connection/2,
         node_cleanup/1]).

-export([register_secret/3,
         get_shared_secret/1]).

-ignore_xref([behaviour_info/1]).

-define(MAIN_MODULE, ejabberd_s2s).

-spec init(map()) -> any().
init(Opts) ->
    Args = [Opts],
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec dirty_read_s2s_list_pids(ejabberd_s2s:fromto()) ->
    {ok, [pid()]} | {error, Reason :: term()}.
dirty_read_s2s_list_pids(FromTo) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [FromTo]).

try_register(Pid, ShouldWriteF, FromTo) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Pid, ShouldWriteF, FromTo]).

remove_connection(FromTo, Pid) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [FromTo, Pid]).

node_cleanup(Node) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Node]).

register_secret(HostType, Source, Secret) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType, Source, Secret]).

get_shared_secret(HostType) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]).
