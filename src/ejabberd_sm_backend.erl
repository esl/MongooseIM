%% Generic module to access SM backend modules
-module(ejabberd_sm_backend).

-callback init(map()) ->
    any().
-callback get_sessions() ->
    [ejabberd_sm:session()].
-callback get_sessions(jid:server()) ->
    [ejabberd_sm:session()].
-callback get_sessions(jid:user(), jid:server()) ->
    [ejabberd_sm:session()].
-callback get_sessions(jid:user(), jid:server(), jid:resource()) ->
    [ejabberd_sm:session()].
-callback set_session(jid:luser(),
                      jid:lserver(),
                      jid:lresource(),
                      ejabberd_sm:session()) ->
    ok | {error, term()}.
-callback delete_session(ejabberd_sm:sid(),
                         jid:user(),
                         jid:server(),
                         jid:resource()) -> ok.
-callback cleanup(Node :: atom()) -> any().
-callback total_count() -> integer().
-callback unique_count() -> integer().

-export([init/1,
         get_sessions/0, get_sessions/1, get_sessions/2, get_sessions/3,
         set_session/4, delete_session/4, cleanup/1,
         total_count/0, unique_count/0]).

-ignore_xref([cleanup/1, behaviour_info/1]).

-define(MAIN_MODULE, ejabberd_sm).

-spec init(map()) -> any().
init(Opts) ->
    Args = [Opts],
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sessions() -> [ejabberd_sm:session()].
get_sessions() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec get_sessions(jid:server()) -> [ejabberd_sm:session()].
get_sessions(Server) ->
    Args = [Server],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sessions(jid:user(), jid:server()) ->
    [ejabberd_sm:session()].
get_sessions(User, Server) ->
    Args = [User, Server],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sessions(jid:user(),
                   jid:server(),
                   jid:resource()) ->
    [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    Args = [User, Server, Resource],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_session(User :: jid:user(),
                  Server :: jid:server(),
                  Resource :: jid:resource(),
                  Session :: ejabberd_sm:session()) -> ok | {error, term()}.
set_session(User, Server, Resource, Session) ->
    Args = [User, Server, Resource, Session],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec delete_session(Sid :: ejabberd_sm:sid(),
                     User :: jid:user(),
                     Server :: jid:server(),
                     Resource :: jid:resource()) -> ok.
delete_session(Sid, User, Server, Resource) ->
    Args = [Sid, User, Server, Resource],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec cleanup(Node :: atom()) -> any().
cleanup(Node) ->
    Args = [Node],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec total_count() -> integer().
total_count() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec unique_count() -> integer().
unique_count() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).
