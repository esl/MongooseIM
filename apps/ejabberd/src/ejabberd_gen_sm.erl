-module(ejabberd_gen_sm).

-callback start(list()) -> any().
-callback get_sessions() -> [ejabberd_sm:ses_tuple()].
-callback get_sessions(ejabberd:server()) -> [ejabberd_sm:ses_tuple()].
-callback get_sessions(ejabberd:user(), ejabberd:server()) -> [ejabberd_sm:session()].
-callback get_sessions(ejabberd:user(), ejabberd:server(), ejabberd:resource()
                      ) -> [ejabberd_sm:session()].
-callback create_session(_User :: ejabberd:user(),
                         _Server :: ejabberd:server(),
                         _Resource :: ejabberd:resource(),
                         Session :: ejabberd_sm:session()) -> ok | {error, term()}.
-callback delete_session(ejabberd_sm:sid(),
                         _User :: ejabberd:user(),
                         _Server :: ejabberd:server(),
                         _Resource :: ejabberd:resource()) -> ok.
-callback cleanup(atom()) -> any().
-callback total_count() -> integer().
-callback unique_count() -> integer().

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
