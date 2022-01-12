%% Mnesia SM backend with fragments support.
%% Does not use indexes.
%% Uses fragments:
%% - 20% slower reads and writes
%% - smaller table sizes - this means the table could be synced even under
%%   heavy load (less number of locks, because locks are per segment.
%%   Less time for each fragment to be locked during the sync).
%% - If a node crashes, each other node would clean up locally
%%   (reduces traffic in the cluster, giving a chance that the cluster
%%    would not overload itself and crash).
-module(ejabberd_sm_mnesia_frag).

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

%% Called from mongoose_frag_hash
-export([simple_key/1]).

-record(mnesia_session, {sur_sid, priority, info}).

simple_key({Server, User, _Resource, _Sid}) ->
    {Server, User}.

-spec init(list()) -> any().
init(_Opts) ->
    FragOpts = [{n_fragments, 32},
                {hash_module, mongoose_frag_hash},
                {hash_state, [{simple_hash_module, ?MODULE}]}],
    mnesia:create_table(mnesia_session,
                        [{ram_copies, [node()]},
                         %% Ordered for each fragment
                         {type, ordered_set},
                         {frag_properties, FragOpts},
                         {attributes, record_info(fields, mnesia_session)}]),
    mnesia:add_table_copy(mnesia_session, node(), ram_copies).

-spec get_sessions() -> [ejabberd_sm:session()].
get_sessions() ->
    match_sessions(#mnesia_session{_ = '_'}).

-spec get_sessions(jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(Server) ->
    match_sessions(#mnesia_session{_ = '_', sur_sid = {Server, '_', '_', '_'}}).

-spec get_sessions(jid:luser(), jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(User, Server) ->
    match_sessions(#mnesia_session{_ = '_', sur_sid = {Server, User, '_', '_'}}).

-spec get_sessions(jid:luser(), jid:lserver(), jid:lresource()
                  ) -> [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    match_sessions(#mnesia_session{_ = '_', sur_sid = {Server, User, Resource, '_'}}).

-spec create_session(_User :: jid:luser(),
                     _Server :: jid:lserver(),
                     _Resource :: jid:lresource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
create_session(User, Server, Resource, Session) ->
    case get_sessions(User, Server, Resource) of
        [] ->
            write_session(Session);
        Sessions when is_list(Sessions) ->
            %% Fix potential race condition during XMPP bind, where
            %% multiple calls (> 2) to ejabberd_sm:open_session
            %% have been made, resulting in >1 sessions for this resource
            MergedSession = mongoose_session:merge_info
                              (Session, hd(lists:sort(Sessions))),
            write_session(MergedSession)
    end.

-spec update_session(_User :: jid:luser(),
                     _Server :: jid:lserver(),
                     _Resource :: jid:lresource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
update_session(_User, _Server, _Resource, Session) ->
    write_session(Session).

write_session(Session) ->
    F = fun(_) -> mnesia:write(from_session(Session)) end,
    mnesia:activity(sync_dirty, F, [frag_dist], mnesia_frag).

-spec delete_session(ejabberd_sm:sid(),
                     User :: jid:luser(),
                     Server :: jid:lserver(),
                     Resource :: jid:lresource()) -> ok.
delete_session(SID, User, Server, Resource) ->
    F = fun(_) -> mnesia:delete({mnesia_session, {Server, User, Resource, SID}}) end,
    mnesia:activity(sync_dirty, F, [frag_dist], mnesia_frag).

-spec cleanup(atom()) -> any().
cleanup(Node) ->
    Pattern = #mnesia_session{_ = '_', sur_sid = {'_', '_', '_', {'_', Node}}},
    Sessions = match_sessions(Pattern),
    %% TODO Ideally, we don't want to run that hook
    lists:map(fun ejabberd_sm:run_session_cleanup_hook/1, Sessions),
    [ets:match_delete(Tab, Pattern) || Tab <- all_frag_tables(mnesia_session)].

all_frag_tables(Tab) ->
    Props = mnesia:table_info(mnesia_session, frag_properties),
    {n_fragments, Count} = lists:keyfind(n_fragments, 1, Props),
    Rest = [list_to_atom(atom_to_list(Tab) ++ "frag" ++ integer_to_list(N))
            || N <- lists:seq(2, Count)],
    [Tab|Rest].

-spec total_count() -> integer().
total_count() ->
    mnesia:table_info(mnesia_session, size).

-spec unique_count() -> integer().
unique_count() ->
    compute_unique(mnesia:dirty_first(mnesia_session), 0).

-spec compute_unique(term(), non_neg_integer()) -> non_neg_integer().
compute_unique('$end_of_table', Sum) ->
    Sum;
compute_unique({S, U, _R, _Sid} = Key, Sum) ->
    F = fun(_) -> mnesia:dirty_next(session, Key) end,
    Key2 = mnesia:activity(sync_dirty, F, [frag_dist], mnesia_frag),
    case Key2 of
        {S, U, _, _} ->
            compute_unique(Key2, Sum);
        _ ->
            compute_unique(Key2, Sum + 1)
    end.

match_sessions(Pattern) ->
    F = fun(_) -> mnesia:match_object(Pattern) end,
    Recs = mnesia:activity(sync_dirty, F, [frag_dist], mnesia_frag),
    lists:map(fun to_session/1, Recs).

to_session(#mnesia_session{sur_sid = {S, U, R, Sid}, priority = Priority, info = Info}) ->
    #session{usr = {U, S, R}, us = {U, S}, priority = Priority, info = Info, sid = Sid}.

from_session(#session{usr = {U, S, R}, priority = Priority, sid = Sid, info = Info}) ->
    #mnesia_session{sur_sid = {S, U, R, Sid}, priority = Priority, info = Info}.
