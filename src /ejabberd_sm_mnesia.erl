%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Implementation of Mnesia-based session manager
%%%
%%% @end
%%% Created : 17 Nov 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_mnesia).

-behavior(ejabberd_gen_sm).

-include("mongoose.hrl").
-include("session.hrl").

-export([start/1,
         get_sessions/0,
         get_sessions/1,
         get_sessions/2,
         get_sessions/3,
         create_session/4,
         delete_session/4,
         cleanup/1,
         total_count/0,
         unique_count/0]).

-spec start(list()) -> any().
start(_Opts) ->
    mnesia:create_table(session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies).


-spec get_sessions() -> [ejabberd_sm:ses_tuple()].
get_sessions() ->
    mnesia:activity(transaction,
        fun() ->
            mnesia:foldl(fun(#session{ usr = Usr, sid = Sid, priority = Pri, info = Inf}, AccIn) ->
                           [{Usr, Sid, Pri, Inf}|AccIn]
                         end,
                [],
                session)
        end).


-spec get_sessions(jid:server()) -> [ejabberd_sm:ses_tuple()].
get_sessions(Server) ->
    Sessions = mnesia:dirty_select(
        session,
          [{#session{usr = '$1', sid='$2', priority='$3', info='$4', _ = '_' },
          [{'==', {element, 2, '$1'}, Server}],
          ['$$']}]),
    [ {USR, SID, Pri, Info} || [USR, SID, Pri, Info] <- Sessions ].


-spec get_sessions(jid:user(), jid:server()) -> [ejabberd_sm:session()].
get_sessions(User, Server) ->
    mnesia:dirty_index_read(session, {User, Server}, #session.us).


-spec get_sessions(jid:user(), jid:server(), jid:resource()
                  ) -> [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    mnesia:dirty_index_read(session, {User, Server, Resource}, #session.usr).


-spec create_session(_User :: jid:user(),
                     _Server :: jid:server(),
                     _Resource :: jid:resource(),
                     Session :: ejabberd_sm:session()) -> ok | {error, term()}.
create_session(User, Server, Resource, Session) ->
    case get_sessions(User, Server, Resource) of
        [] -> mnesia:sync_dirty(fun() -> mnesia:write(Session) end);
        Sessions when is_list(Sessions) ->
            %% Fix potential race condition during XMPP bind, where
            %% multiple calls (> 2) to ejabberd_sm:open_session
            %% have been made, resulting in >1 sessions for this resource
            MergedSession = mongoose_session:merge_info
                              (Session, hd(lists:sort(Sessions))),
            mnesia:sync_dirty(fun() -> mnesia:write(MergedSession) end)
    end.

-spec delete_session(ejabberd_sm:sid(),
                     _User :: jid:user(),
                     _Server :: jid:server(),
                     _Resource :: jid:resource()) -> ok.
delete_session(SID, _User, _Server, _Resource) ->
    mnesia:sync_dirty(fun() ->
                              mnesia:delete({session, SID})
                      end).


-spec cleanup(atom()) -> any().
cleanup(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       session,
                       [{#session{sid = {'_', '$1'}, _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(#session{ usr = {U, S, R}, sid = SID }) ->
                                      mnesia:delete({session, SID}),
                                      ejabberd_hooks:run(session_cleanup, S, [U, S, R, SID])
                              end, Es)

        end,
    mnesia:async_dirty(F).


-spec total_count() -> integer().
total_count() ->
    mnesia:table_info(session, size).


-spec unique_count() -> integer().
unique_count() ->
    compute_unique(mnesia:dirty_first(session),
                   sets:new()).

-spec compute_unique(term(), sets:set()) -> integer().
compute_unique('$end_of_table', Set) ->
    sets:size(Set);
compute_unique(Key, Set) ->
    NewSet = case mnesia:dirty_read(session, Key) of
                 [Session] ->
                     sets:add_element(Session#session.us, Set);
                 _ ->
                     Set
             end,
    compute_unique(mnesia:dirty_next(session, Key), NewSet).
