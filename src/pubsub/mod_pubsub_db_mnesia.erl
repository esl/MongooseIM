%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db_mnesia.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub Mnesia backend
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db_mnesia).
-author('piotr.nosek@erlang-solutions.com').

-include("pubsub.hrl").

-export([start/2, stop/2]).
-export([transaction/2, dirty/2]).
-export([set_state/2, del_state/3, get_states/2, get_state/3]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-spec start(Host :: jid:lserver(), PubSubHost :: jid:lserver()) -> ok.
start(_, _) ->
    mnesia:create_table(pubsub_state,
                        [{disc_copies, [node()]},
                         {type, ordered_set},
                         {attributes, record_info(fields, pubsub_state)}]),
    mnesia:add_table_copy(pubsub_state, node(), disc_copies),
    ok.

-spec stop(Host :: jid:lserver(), PubSubHost :: jid:lserver()) -> ok.
stop(_, _) ->
    ok.

%% ------------------------ Fun execution ------------------------

transaction(_PubSubHost, Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

dirty(_PubSubHost, Fun) ->
    try mnesia:sync_dirty(Fun) of
        Result ->
            Result
    catch
        C:R ->
            {error, {C, R, erlang:get_stacktrace()}}
    end.

%% ------------------------ Node state ------------------------

-spec set_state(PubSubHost :: jid:lserver(),
                mod_pubsub:pubsubState()) -> ok.
set_state(_PubSubHost, State) ->
    mnesia:write(State).

-spec del_state(PubSubHost :: jid:lserver(),
                Nidx :: mod_pubsub:nodeIdx(),
                UserLJID :: jid:ljid()) -> ok.
del_state(_PubSubHost, Nidx, UserLJID) ->
    mnesia:delete({pubsub_state, {UserLJID, Nidx}}).

-spec get_states(PubSubHost :: jid:lserver(),
                 Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.
get_states(_PubSubHost, Nidx) ->
    States = case catch mnesia:match_object(
                          #pubsub_state{stateid = {'_', Nidx}, _ = '_'}) of
                 List when is_list(List) -> List;
                 _ -> []
             end,
    {ok, States}.

-spec get_state(PubSubHost :: jid:lserver(),
                Nidx :: mod_pubsub:nodeIdx(),
                UserLJID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.
get_state(_PubSubHost, Nidx, UserLJID) ->
    StateId = {UserLJID, Nidx},
    case catch mnesia:read({pubsub_state, StateId}) of
        [#pubsub_state{} = State] -> {ok, State};
        _ -> {ok, #pubsub_state{stateid = StateId}}
    end.

