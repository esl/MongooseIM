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

-callback start() -> ok.

-callback stop() -> ok.

%% ----------------------- Fun execution ------------------------

-callback transaction(Fun :: fun(() -> {result | error, any()})) ->
    {result | error, any()}.

%% Synchronous
-callback dirty(Fun :: fun(() -> {result | error, any()})) ->
    {result | error, any()}.

%% ----------------------- Direct #pubsub_state access ------------------------

%% TODO: Replace with del_node when it is fully possible from backend
%%       i.e. when pubsub_item is migrated to RDBMS as well
-callback del_state(Nidx :: mod_pubsub:nodeIdx(),
                    LJID :: jid:ljid()) -> ok.

%% When a state is not found, returns empty state.
%% Maybe can be removed completely later?
-callback get_state(Nidx :: mod_pubsub:nodeIdx(),
                    JID :: jid:jid()) ->
    {ok, mod_pubsub:pubsubState()}.

%% Maybe can be removed completely later?
-callback get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_lus(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_bare(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_bare_and_full(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_own_nodes_states(JID :: jid:jid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

%% ----------------------- Affiliations ------------------------

-callback set_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                          JID :: jid:jid(),
                          Affiliation :: mod_pubsub:affiliation()) ->
    ok.

-callback get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                          JID :: jid:jid()) ->
    {ok, mod_pubsub:affiliation()}.

%% ----------------------- Subscriptions ------------------------

-callback add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                           JID :: jid:jid(),
                           Sub :: mod_pubsub:subscription(),
                           SubId :: mod_pubsub:subId()) ->
    ok.

-callback update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                              JID :: jid:jid(),
                              Subscription :: mod_pubsub:subscription(),
                              SubId :: mod_pubsub:subId()) ->
    ok.

-callback get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:jid(), Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.

-callback get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                        JID :: jid:jid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.

-callback delete_subscription(
            Nidx :: mod_pubsub:nodeIdx(),
            JID :: jid:jid(),
            SubId :: mod_pubsub:subId()) ->
    ok.

-callback delete_all_subscriptions(
            Nidx :: mod_pubsub:nodeIdx(),
            JID :: jid:jid()) ->
    ok.

%% ----------------------- Items ------------------------

%% TODO: Refactor to use MaxItems value, so separate remove_items in publishing
%% won't be necessary and the whole operation may be optimised in DB layer.
-callback add_item(Nidx :: mod_pubsub:nodeIdx(),
                   JID :: jid:jid(),
                   ItemId :: mod_pubsub:itemId()) ->
    ok.

-callback remove_items(Nidx :: mod_pubsub:nodeIdx(),
                       JID :: jid:jid(),
                       ItemIds :: [mod_pubsub:itemId()]) ->
    ok.

-callback remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.

%%====================================================================
%% API
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

