-module(ejabberd_gen_sm).

-callback start(list()) -> any().
-callback get_sessions() -> [ejabberd_sm:ses_tuple()].
-callback get_sessions(jid:server()) -> [ejabberd_sm:ses_tuple()].
-callback get_sessions(jid:user(), jid:server()) ->
    [ejabberd_sm:session()].
-callback get_sessions(jid:user(), jid:server(), jid:resource()
                      ) -> [ejabberd_sm:session()].
-callback create_session(_User :: jid:user(),
                         _Server :: jid:server(),
                         _Resource :: jid:resource(),
                         Session :: ejabberd_sm:session()) ->
    ok | {error, term()}.
-callback delete_session(ejabberd_sm:sid(),
                         _User :: jid:user(),
                         _Server :: jid:server(),
                         _Resource :: jid:resource()) -> ok.
-callback cleanup(Node :: atom()) -> any().
-callback total_count() -> integer().
-callback unique_count() -> integer().

-export([start/2, get_sessions/1, get_sessions/2, get_sessions/3,
         get_sessions/4, create_session/5, delete_session/5, cleanup/2,
         total_count/1, unique_count/1]).

-spec start(module(), list()) -> any().
start(Mod, Opts) ->
    Mod:start(Opts).

-spec get_sessions(module()) -> [ejabberd_sm:ses_tuple()].
get_sessions(Mod) ->
    Mod:get_sessions().

-spec get_sessions(module(), jid:server()) -> [ejabberd_sm:ses_tuple()].
get_sessions(Mod, Server) ->
    Mod:get_sessions(Server).

-spec get_sessions(module(), jid:user(), jid:server()) ->
    [ejabberd_sm:session()].
get_sessions(Mod, User, Server) ->
    Mod:get_sessions(User, Server).

-spec get_sessions(module(),
                   jid:user(),
                   jid:server(),
                   jid:resource()) ->
    [ejabberd_sm:session()].
get_sessions(Mod, User, Server, Resource) ->
    Mod:get_sessions(User, Server, Resource).

-spec create_session(Mod :: module(), User :: jid:user(),
                     Server :: jid:server(),
                     Resource :: jid:resource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
create_session(Mod, User, Server, Resource, Session) ->
    Mod:create_session(User, Server, Resource, Session).

-spec delete_session(Mod :: module(), Sid :: ejabberd_sm:sid(),
                     User :: jid:user(),
                     Server :: jid:server(),
                     Resource :: jid:resource()) -> ok.
delete_session(Mod, Sid, User, Server, Resource) ->
    Mod:delete_session(Sid, User, Server, Resource).

-spec cleanup(Mod :: module(), Node :: atom()) -> any().
cleanup(Mod, Node) ->
    Mod:cleanup(Node).

-spec total_count(module()) -> integer().
total_count(Mod) ->
    Mod:total_count().

-spec unique_count(module()) -> integer().
unique_count(Mod) ->
    Mod:unique_count().

%% -export([behaviour_info/1]).
%% -spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
%% behaviour_info(callbacks) ->
%%     [{start, 1},
%%      {get_sessions, 0},
%%      {get_sessions, 1},
%%      {get_sessions, 2},
%%      {get_sessions, 3},
%%      {create_session, 4},
%%      {delete_session, 4},
%%      {cleanup, 1},
%%      {total_count, 0},
%%      {unique_count, 0}];
%% behaviour_info(_Other) ->
%%     undefined.
