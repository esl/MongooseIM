%%% @doc A proxy interface module between the main mod_bosh module and the backend modules.
%%% There is only one backend implementation (mnesia), so the backend module is global.
-module(mod_bosh_backend).

-export([start/1,
         create_session/1,
         delete_session/1,
         get_session/1,
         get_sessions/0,
         node_cleanup/1]).

-define(MAIN_MODULE, mod_bosh).

%% Callbacks

-callback start() -> any().

-callback create_session(mod_bosh:session()) -> any().

-callback delete_session(mod_bosh:sid()) -> any().

-callback get_session(mod_bosh:sid()) -> [mod_bosh:session()].

-callback get_sessions() -> [mod_bosh:session()].

-callback node_cleanup(Node :: atom()) -> any().

%% API Functions

-spec start(gen_mod:module_opts()) -> any().
start(Opts) ->
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec create_session(mod_bosh:session()) -> any().
create_session(Session) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Session]).

-spec delete_session(mod_bosh:sid()) -> any().
delete_session(Sid) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Sid]).

-spec get_session(mod_bosh:sid()) -> [mod_bosh:session()].
get_session(Sid) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Sid]).

-spec get_sessions() -> [mod_bosh:session()].
get_sessions() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec node_cleanup(Node :: atom()) -> any().
node_cleanup(Node) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Node]).
