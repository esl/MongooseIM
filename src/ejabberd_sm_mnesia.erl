%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Implementation of Mnesia-based session manager
%%%
%%% @end
%%% Created : 17 Nov 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_mnesia).

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

-spec init(map()) -> any().
init(_Opts) ->
    mongoose_mnesia:create_table(session,
        [{ram_copies, [node()]},
         {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us).

-spec get_sessions() -> [ejabberd_sm:session()].
get_sessions() ->
    mnesia:activity(transaction,
        fun() ->
            mnesia:foldl(fun(Session, AccIn) -> [Session | AccIn] end,
                [], session)
        end).

-spec get_sessions(jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(Server) ->
    mnesia:dirty_select(
        session,
          [{#session{usr = '$1', sid='$2', priority='$3', info='$4', _ = '_' },
          [{'==', {element, 2, '$1'}, Server}],
          ['$_']}]).

-spec get_sessions(jid:luser(), jid:lserver()) -> [ejabberd_sm:session()].
get_sessions(User, Server) ->
    mnesia:dirty_index_read(session, {User, Server}, #session.us).

-spec get_sessions(jid:luser(), jid:lserver(), jid:lresource()
                  ) -> [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    mnesia:dirty_index_read(session, {User, Server, Resource}, #session.usr).

-spec set_session(_User :: jid:luser(),
                  _Server :: jid:lserver(),
                  _Resource :: jid:lresource(),
                  Session :: ejabberd_sm:session()) -> ok | {error, term()}.
set_session(_User, _Server, _Resource, Session) ->
    mnesia:sync_dirty(fun() -> mnesia:write(Session) end).

-spec delete_session(ejabberd_sm:sid(),
                     _User :: jid:luser(),
                     _Server :: jid:lserver(),
                     _Resource :: jid:lresource()) -> ok.
delete_session(SID, _User, _Server, _Resource) ->
    mnesia:sync_dirty(fun() -> mnesia:delete({session, SID}) end).

-spec cleanup(atom()) -> any().
cleanup(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       session,
                       [{#session{sid = {'_', '$1'}, _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(#session{sid = SID} = Session) ->
                                      mnesia:delete({session, SID}),
                                      ejabberd_sm:run_session_cleanup_hook(Session)
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
