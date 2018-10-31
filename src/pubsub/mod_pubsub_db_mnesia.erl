%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_mnesia.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub Mnesia backend
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_mnesia).
-author('piotr.nosek@erlang-solutions.com').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/0, stop/0]).
-export([transaction/1, dirty/1]).
-export([set_state/1, del_state/2, get_state/2,
         get_states/1, get_states_by_lus/1, get_states_by_bare/1,
         get_states_by_bare_and_full/1, get_own_nodes_states/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() ->
    mnesia:create_table(pubsub_state,
                        [{disc_copies, [node()]},
                         {type, ordered_set},
                         {attributes, record_info(fields, pubsub_state)}]),
    mnesia:add_table_copy(pubsub_state, node(), disc_copies),
    ok.

-spec stop() -> ok.
stop() ->
    ok.

%% ------------------------ Fun execution ------------------------

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

dirty(Fun) ->
    try mnesia:sync_dirty(Fun, []) of
        Result ->
            Result
    catch
        C:R ->
            {error, {C, R, erlang:get_stacktrace()}}
    end.

%% ------------------------ Node state ------------------------

-spec set_state(mod_pubsub:pubsubState()) -> ok.
set_state(State) ->
    mnesia:write(State).

-spec del_state(Nidx :: mod_pubsub:nodeIdx(),
                UserLJID :: jid:ljid()) -> ok.
del_state(Nidx, UserLJID) ->
    mnesia:delete({pubsub_state, {UserLJID, Nidx}}).

-spec get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states(Nidx) ->
    States = case catch mnesia:match_object(
                          #pubsub_state{stateid = {'_', Nidx}, _ = '_'}) of
                 List when is_list(List) -> List;
                 _ -> []
             end,
    {ok, States}.

-spec get_states_by_lus(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_lus(#jid{ luser = LUser, lserver = LServer }) ->
    {ok, mnesia:match_object(#pubsub_state{stateid = {{LUser, LServer, '_'}, '_'}, _ = '_'})}.

-spec get_states_by_bare(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare(JID) ->
    LBare = jid:to_bare(jid:to_lower(JID)),
    {ok, mnesia:match_object(#pubsub_state{stateid = {LBare, '_'}, _ = '_'})}.

-spec get_states_by_bare_and_full(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states_by_bare_and_full(JID) ->
    LJID = jid:to_lower(JID),
    LBare = jid:to_bare(LJID),
    {ok, mnesia:match_object(#pubsub_state{stateid = {LJID, '_'}, _ = '_'})
         ++ mnesia:match_object(#pubsub_state{stateid = {LBare, '_'}, _ = '_'})}.

-spec get_own_nodes_states(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_own_nodes_states(JID) ->
    LBare = jid:to_bare(jid:to_lower(JID)),
    MyStates = mnesia:match_object(#pubsub_state{stateid = {LBare, '_'},
                                                 affiliation = owner, _ = '_'}),
    NodeIdxs = [Nidx || #pubsub_state{stateid = {_, Nidx}} <- MyStates],
    OwnNodesStates =
    mnesia:foldl(fun (#pubsub_state{stateid = {_, Nidx}} = PubSubState, Acc) ->
                         case lists:member(Nidx, NodeIdxs) of
                             true -> [PubSubState | Acc];
                             false -> Acc
                         end
                 end,
                 [], pubsub_state),
    {ok, OwnNodesStates}.

-spec get_state(Nidx :: mod_pubsub:nodeIdx(),
                UserLJID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(Nidx, UserLJID) ->
    StateId = {UserLJID, Nidx},
    case catch mnesia:read({pubsub_state, StateId}) of
        [#pubsub_state{} = State] -> {ok, State};
        _ -> {ok, #pubsub_state{stateid = StateId}}
    end.

