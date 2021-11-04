%% Generic module to access SM backend modules
%% Pass HostType to the functions as an argument, if you can
%% Pass global, when you cannot
-module(ejabberd_gen_sm).

-callback start(list()) ->
    any().
-callback get_sessions() ->
    [ejabberd_sm:session()].
-callback get_sessions(jid:server()) ->
    [ejabberd_sm:session()].
-callback get_sessions(jid:user(), jid:server()) ->
    [ejabberd_sm:session()].
-callback get_sessions(jid:user(), jid:server(), jid:resource()) ->
    [ejabberd_sm:session()].
-callback create_session(jid:luser(),
                         jid:lserver(),
                         jid:lresource(),
                         ejabberd_sm:session()) ->
    ok | {error, term()}.
-callback update_session(jid:luser(),
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

-export([start/2, get_sessions/1, get_sessions/2, get_sessions/3,
         get_sessions/4, create_session/5, update_session/5, delete_session/5, cleanup/2,
         total_count/1, unique_count/1]).

-ignore_xref([cleanup/2, behaviour_info/1]).

-define(MAIN_MODULE, ejabberd_sm).
-type host_type() :: mongooseim:host_type() | global.

-spec start(host_type(), list()) -> any().
start(_HostType, Opts) ->
    Args = [Opts],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sessions(host_type()) -> [ejabberd_sm:sessions()].
get_sessions(_HostType) ->
    Args = [],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sessions(host_type(), jid:server()) -> [ejabberd_sm:sessions()].
get_sessions(_HostType, Server) ->
    Args = [Server],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sessions(host_type(), jid:user(), jid:server()) ->
    [ejabberd_sm:session()].
get_sessions(_HostType, User, Server) ->
    Args = [User, Server],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_sessions(host_type(),
                   jid:user(),
                   jid:server(),
                   jid:resource()) ->
    [ejabberd_sm:session()].
get_sessions(_HostType, User, Server, Resource) ->
    Args = [User, Server, Resource],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec create_session(HostType :: host_type(), User :: jid:user(),
                     Server :: jid:server(),
                     Resource :: jid:resource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
create_session(_HostType, User, Server, Resource, Session) ->
    Args = [User, Server, Resource, Session],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec update_session(HostType :: host_type(), User :: jid:luser(),
                     Server :: jid:lserver(),
                     Resource :: jid:lresource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
update_session(_HostType, User, Server, Resource, Session) ->
    Args = [User, Server, Resource, Session],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec delete_session(HostType :: host_type(), Sid :: ejabberd_sm:sid(),
                     User :: jid:user(),
                     Server :: jid:server(),
                     Resource :: jid:resource()) -> ok.
delete_session(_HostType, Sid, User, Server, Resource) ->
    Args = [Sid, User, Server, Resource],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec cleanup(HostType :: host_type(), Node :: atom()) -> any().
cleanup(_HostType, Node) ->
    Args = [Node],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec total_count(host_type()) -> integer().
total_count(_HostType) ->
    Args = [],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec unique_count(host_type()) -> integer().
unique_count(_HostType) ->
    Args = [],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
