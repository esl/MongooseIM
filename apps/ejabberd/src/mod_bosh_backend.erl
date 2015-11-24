-module(mod_bosh_backend).

%% -export([behaviour_info/1]).
%% -spec behaviour_info(atom()) -> undefined | [{atom(), arity()}].
%% behaviour_info(callbacks) ->
%%     [{start, 1},
%%      {create_session, 1},
%%      {delete_session, 1},
%%      {get_session, 1},
%%      {get_sessions, 0}];
%% behaviour_info(_Other) ->
%%     undefined.

-callback start(list()) -> any().
-callback create_session(mod_bosh:session()) -> any().
-callback delete_session(mod_bosh:sid()) -> any().
-callback get_session(mod_bosh:sid()) -> [mod_bosh:session()].
-callback get_sessions() -> [mod_bosh:session()].
-callback node_cleanup(Node :: atom()) -> any().

