-module(ejabberd_sm_cets).

-behavior(ejabberd_sm_backend).

-include("mongoose.hrl").
-include("session.hrl").

-export([init/1,
         get_sessions/0,
         get_sessions/1,
         get_sessions/2,
         get_sessions/3,
         create_session/4,
         update_session/4,
         delete_session/4,
         cleanup/1,
         total_count/0,
         unique_count/0]).

-define(TABLE, cets_session).

-spec init(list()) -> any().
init(_Opts) ->
    cets:start(?TABLE, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE).

-spec get_sessions() -> [ejabberd_sm:session()].
get_sessions() ->
    tuples_to_sessions(ets:tab2list(?TABLE)).

-spec get_sessions(jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(Server) ->
    R = {{Server, '_', '_', '_'}, '_', '_'},
    Xs = ets:select(?TABLE, [{R, [], ['$_']}]),
    tuples_to_sessions(Xs).

-spec get_sessions(jid:luser(), jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(User, Server) ->
    R = {{Server, User, '_', '_'}, '_', '_'},
    Xs = ets:select(?TABLE, [{R, [], ['$_']}]),
    tuples_to_sessions(Xs).

-spec get_sessions(jid:luser(), jid:lserver(), jid:lresource()) ->
    [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    R = {{Server, User, Resource, '_'}, '_', '_'},
    Xs = ets:select(?TABLE, [{R, [], ['$_']}]),
    %% TODO these sessions should be deduplicated.
    %% It is possible, that after merging two cets tables we could end up
    %% with sessions from two nodes for the same full jid.
    %% One of the sessions must be killed.
    %% We can detect duplicates on the merging step or on reading (or both).
    tuples_to_sessions(Xs).

-spec create_session(User :: jid:luser(),
                     Server :: jid:lserver(),
                     Resource :: jid:lresource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
create_session(User, Server, Resource, Session) ->
    case get_sessions(User, Server, Resource) of
        [] ->
            cets:insert(?TABLE, session_to_tuple(Session));
        Sessions when is_list(Sessions) ->
            %% Fix potential race condition during XMPP bind, where
            %% multiple calls (> 2) to ejabberd_sm:open_session
            %% have been made, resulting in >1 sessions for this resource
            %% XXX Why do we need that exactly?
            %%     Sessions are open from c2s and that specific process is updating
            %%     its session info. Adding info from other processes would cause some
            %%     strange bugs. On another hand, there is very limited usage
            %%     of that info field, so nothing would probably break if
            %%     we keep calling merge_info (and it would make ejabberd_sm_SUITE happy).
            MergedSession = mongoose_session:merge_info
                              (Session, hd(lists:sort(Sessions))),
            cets:insert(?TABLE, session_to_tuple(MergedSession))
    end.

-spec update_session(User :: jid:luser(),
                     Server :: jid:lserver(),
                     Resource :: jid:lresource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
update_session(_User, _Server, _Resource, Session) ->
    cets:insert(?TABLE, session_to_tuple(Session)).

-spec delete_session(ejabberd_sm:sid(),
                     User :: jid:luser(),
                     Server :: jid:lserver(),
                     Resource :: jid:lresource()) -> ok.
delete_session(SID, User, Server, Resource) ->
    cets:delete(?TABLE, make_key(User, Server, Resource, SID)).

-spec cleanup(atom()) -> any().
cleanup(Node) ->
    %% TODO this could be optimized, we don't need to replicate deletes,
    %% we could just call cleanup on each node (but calling the hook only
    %% on one of the nodes)
    KeyPattern = {'_', '_', '_', {'_', '$1'}},
    Guard = {'==', {node, '$1'}, Node},
    R = {KeyPattern, '_', '_'},
    cets:sync(?TABLE),
    Tuples = ets:select(?TABLE, [{R, [Guard], ['$_']}]),
    Keys = lists:map(fun({Key, _, _} = Tuple) ->
                          Session = tuple_to_session(Tuple),
                          ejabberd_sm:run_session_cleanup_hook(Session),
                          Key
                  end, Tuples),
    cets:delete_many(?TABLE, Keys).

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
