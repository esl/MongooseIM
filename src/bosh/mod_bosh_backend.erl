%%% @doc A proxy interface module between the main mod_bosh module and the backend modules.
%%% There is only one backend implementation (mnesia), so the backend module is global.
-module(mod_bosh_backend).

-export([start/2,
         create_session/2,
         delete_session/2,
         get_session/2,
         get_sessions/1,
         node_cleanup/2]).

-define(MAIN_MODULE, mod_bosh).

%% Callbacks

-callback start(mongooseim:host_type()) -> any().

-callback create_session(mongooseim:host_type(), mod_bosh:session()) -> any().

-callback delete_session(mongooseim:host_type(), mod_bosh:sid()) -> any().

-callback get_session(mongooseim:host_type(), mod_bosh:sid()) -> [mod_bosh:session()].

-callback get_sessions(mongooseim:host_type()) -> [mod_bosh:session()].

-callback node_cleanup(mongooseim:host_type(), Node :: atom()) -> any().

%% API Functions

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec create_session(mongooseim:host_type(), mod_bosh:session()) -> any().
create_session(HostType, Session) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [Session]).

-spec delete_session(mongooseim:host_type(), mod_bosh:sid()) -> any().
delete_session(HostType, Sid) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [Sid]).

-spec get_session(mongooseim:host_type(), mod_bosh:sid()) -> [mod_bosh:session()].
get_session(HostType, Sid) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [Sid]).

-spec get_sessions(mongooseim:host_type()) -> [mod_bosh:session()].
get_sessions(HostType) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec node_cleanup(mongooseim:host_type(), Node :: atom()) -> any().
node_cleanup(HostType, Node) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [Node]).
