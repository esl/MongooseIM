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
-export([set_state/2]).

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

