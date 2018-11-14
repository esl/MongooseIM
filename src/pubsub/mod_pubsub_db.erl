%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub DB behaviour
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose_logger.hrl").

-export([transaction_error/2, dirty_error/4]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%% ------------------------ Backend start/stop ------------------------

-callback start() -> ok.

-callback stop() -> ok.

%% ----------------------- Fun execution ------------------------

%% `ErrorDebug` are the maps of extra data that will be added to error tuple

-callback transaction(Fun :: fun(() -> {result | error, any()}),
                      ErrorDebug :: map()) ->
    {result | error, any()}.

%% Synchronous
-callback dirty(Fun :: fun(() -> {result | error, any()}),
                ErrorDebug :: map()) ->
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
                   ItemId :: mod_pubsub:pubsubItem()) ->
    ok.

-callback remove_items(Nidx :: mod_pubsub:nodeIdx(),
                       JID :: jid:jid(),
                       ItemIds :: [mod_pubsub:itemId()]) ->
    ok.

-callback remove_all_items(Nidx :: mod_pubsub:nodeIdx()) ->
    ok.

-callback get_items(Nidx :: mod_pubsub:nodeIdx(), gen_pubsub_node:get_item_options()) ->
    {ok, {[mod_pubsub:pubsubItem()], jlib:rsm_out()}}.

-callback get_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) ->
    {ok, mod_pubsub:pubsubItem()} | {error, item_not_found}.

-callback set_item(Item :: mod_pubsub:pubsubItem()) -> ok.

-callback del_item(Nidx :: mod_pubsub:nodeIdx(), ItemId :: mod_pubsub:itemId()) -> ok.

-callback del_items(Nidx :: mod_pubsub:nodeIdx(), [ItemId :: mod_pubsub:itemId()]) -> ok.

%%====================================================================

%% API
%%====================================================================

%% These are made as separate functions to make tracing easier, just in case.

-spec transaction_error(Reason :: any(), ErrorDebug :: map()) ->
    {error, Details :: map()}.
transaction_error(Reason, ErrorDebug) ->
    {error, ErrorDebug#{ event => transaction_failure,
                         reason => Reason }}.

-spec dirty_error(Class :: atom(), Reason :: any(), StackTrace :: list(), ErrorDebug :: map()) ->
    {error, Details :: map()}.
dirty_error(Class, Reason, StackTrace, ErrorDebug) ->
    {error, ErrorDebug#{ event => dirty_failure,
                         class => Class,
                         reason => Reason,
                         stacktrace => StackTrace}}.

%%====================================================================
%% Internal functions
%%====================================================================

