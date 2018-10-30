%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub DB behaviour
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose_logger.hrl").

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-callback start(Host :: jid:lserver(), PubSubHost :: jid:lserver()) -> ok.

-callback stop(Host :: jid:lserver(), PubSubHost :: jid:lserver()) -> ok.

-callback transaction(PubSubHost :: jid:lserver(),
                      Fun :: fun(() -> {result | error, any()})) ->
    {result | error, any()}.

%% Synchronous
-callback dirty(PubSubHost :: jid:lserver(),
                Fun :: fun(() -> {result | error, any()})) ->
    {result | error, any()}.

-callback set_state(State :: mod_pubsub:pubsubState()) -> ok.

-callback del_state(Nidx :: mod_pubsub:nodeIdx(),
                    UserLJID :: jid:ljid()) -> ok.

%% When a state is not found, returns empty state.
-callback get_state(Nidx :: mod_pubsub:nodeIdx(),
                    UserLJID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.

-callback get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_lus(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_bare(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_full(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_own_nodes_states(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

%%====================================================================
%% API
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

