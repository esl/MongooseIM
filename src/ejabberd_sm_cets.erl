-module(ejabberd_sm_cets).

-behavior(ejabberd_sm_backend).

-include("mongoose.hrl").
-include("session.hrl").

-export([init/1,
         get_sessions/0,
         get_sessions/1,
         get_sessions/2,
         get_sessions/3,
         set_session/4,
         delete_session/4,
         cleanup/1,
         total_count/0,
         unique_count/0]).

-define(TABLE, cets_session).

-spec init(map()) -> any().
init(_Opts) ->
    cets:start(?TABLE, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE).

-spec get_sessions() -> [ejabberd_sm:session()].
get_sessions() ->
    tuples_to_sessions(ets:tab2list(?TABLE)).

-spec get_sessions(jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(Server) ->
    %% This is not a full table scan. From the ETS docs:
    %% For ordered_set a partially bound key will limit the traversal to only
    %% scan a subset of the table based on term order.
    %% A partially bound key is either a list or a tuple with
    %% a prefix that is fully bound.
    R = {{Server, '_', '_', '_'}, '_', '_'},
    Xs = ets:match_object(?TABLE, R),
    tuples_to_sessions(Xs).

-spec get_sessions(jid:luser(), jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(User, Server) ->
    R = {{Server, User, '_', '_'}, '_', '_'},
    Xs = ets:match_object(?TABLE, R),
    tuples_to_sessions(Xs).

-spec get_sessions(jid:luser(), jid:lserver(), jid:lresource()) ->
    [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    R = {{Server, User, Resource, '_'}, '_', '_'},
    Xs = ets:match_object(?TABLE, R),
    %% TODO these sessions should be deduplicated.
    %% It is possible, that after merging two cets tables we could end up
    %% with sessions from two nodes for the same full jid.
    %% One of the sessions must be killed.
    %% We can detect duplicates on the merging step or on reading (or both).
    tuples_to_sessions(Xs).

-spec set_session(User :: jid:luser(),
                  Server :: jid:lserver(),
                  Resource :: jid:lresource(),
                  Session :: ejabberd_sm:session()) -> ok | {error, term()}.
set_session(_User, _Server, _Resource, Session) ->
    cets:insert(?TABLE, session_to_tuple(Session)).

-spec delete_session(SID :: ejabberd_sm:sid(),
                     User :: jid:luser(),
                     Server :: jid:lserver(),
                     Resource :: jid:lresource()) -> ok.
delete_session(SID, User, Server, Resource) ->
    cets:delete(?TABLE, make_key(User, Server, Resource, SID)).

%% cleanup is called on each node in the cluster, when Node is down
-spec cleanup(atom()) -> any().
cleanup(Node) ->
    KeyPattern = {'_', '_', '_', {'_', '$1'}},
    Guard = {'==', {node, '$1'}, Node},
    R = {KeyPattern, '_', '_'},
    cets:ping_all(?TABLE),
    %% This is a full table scan, but cleanup is rare.
    Tuples = ets:select(?TABLE, [{R, [Guard], ['$_']}]),
    cets:delete_many(?TABLE, [Key || {Key, _, _} <- Tuples]),
    lists:foreach(fun(Tuple) ->
                          Session = tuple_to_session(Tuple),
                          ejabberd_sm:run_session_cleanup_hook(Session)
                  end, Tuples).

-spec total_count() -> integer().
total_count() ->
    ets:info(?TABLE, size).

%% Counts merged by US
-spec unique_count() -> integer().
unique_count() ->
    compute_unique(ets:first(?TABLE), 0).

compute_unique('$end_of_table', Sum) ->
    Sum;
compute_unique({S, U, _, _} = Key, Sum) ->
    Key2 = ets:next(?TABLE, Key),
    case Key2 of
        {S, U, _, _} ->
            compute_unique(Key2, Sum);
        _ ->
            compute_unique(Key2, Sum + 1)
    end.

session_to_tuple(#session{sid = SID, usr = {U, S, R}, priority = Prio, info = Info}) ->
    {make_key(U, S, R, SID), Prio, Info}.

make_key(User, Server, Resource, SID) ->
    {Server, User, Resource, SID}.

tuple_to_session({{S, U, R, SID}, Prio, Info}) ->
    #session{sid = SID, usr = {U, S, R}, us = {U, S}, priority = Prio, info = Info}.

tuples_to_sessions(Xs) ->
    [tuple_to_session(X) || X <- Xs].
