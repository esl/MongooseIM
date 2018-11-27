%%%----------------------------------------------------------------------
%%% File    : mod_pubsub_db.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : PubSub DB behaviour
%%% Created : 26 Oct 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_pubsub_db).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose_logger.hrl").

-export([db_error/3]).

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

%% When a state is not found, returns empty state.
%% Maybe can be removed completely later?
-callback get_state(Nidx :: mod_pubsub:nodeIdx(),
                    JID :: jid:ljid()) ->
    {ok, mod_pubsub:pubsubState()}.

%% Maybe can be removed completely later?
-callback get_states(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_lus(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_bare(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_states_by_bare_and_full(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:pubsubState()]}.

-callback get_idxs_of_own_nodes_with_pending_subs(JID :: jid:ljid()) ->
    {ok, [mod_pubsub:nodeIdx()]}.

%% ----------------------- Node management ------------------------

-callback create_node(Nidx :: mod_pubsub:nodeIdx(),
                      Owner :: jid:ljid()) ->
    ok.

-callback del_node(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [mod_pubsub:pubsubState()]}.

%% ----------------------- Affiliations ------------------------

-callback set_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                          JID :: jid:ljid(),
                          Affiliation :: mod_pubsub:affiliation()) ->
    ok.

-callback get_affiliation(Nidx :: mod_pubsub:nodeIdx(),
                          JID :: jid:ljid()) ->
    {ok, mod_pubsub:affiliation()}.

%% ----------------------- Subscriptions ------------------------

-callback add_subscription(Nidx :: mod_pubsub:nodeIdx(),
                           JID :: jid:ljid(),
                           Sub :: mod_pubsub:subscription(),
                           SubId :: mod_pubsub:subId()) ->
    ok.

-callback update_subscription(Nidx :: mod_pubsub:nodeIdx(),
                              JID :: jid:ljid(),
                              Subscription :: mod_pubsub:subscription(),
                              SubId :: mod_pubsub:subId()) ->
    ok.

-callback get_node_subscriptions(Nidx :: mod_pubsub:nodeIdx()) ->
    {ok, [{Entity :: jid:ljid(), Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.

-callback get_node_entity_subscriptions(Nidx :: mod_pubsub:nodeIdx(),
                                        JID :: jid:ljid()) ->
    {ok, [{Sub :: mod_pubsub:subscription(), SubId :: mod_pubsub:subId()}]}.

-callback delete_subscription(
            Nidx :: mod_pubsub:nodeIdx(),
            JID :: jid:ljid(),
            SubId :: mod_pubsub:subId()) ->
    ok.

-callback delete_all_subscriptions(
            Nidx :: mod_pubsub:nodeIdx(),
            JID :: jid:ljid()) ->
    ok.

%% ----------------------- Items ------------------------

%% TODO: Refactor to use MaxItems value, so separate remove_items in publishing
%% won't be necessary and the whole operation may be optimised in DB layer.
-callback add_item(Nidx :: mod_pubsub:nodeIdx(),
                   JID :: jid:ljid(),
                   PubSubItem :: mod_pubsub:pubsubItem()) ->
    ok.

-callback remove_items(Nidx :: mod_pubsub:nodeIdx(),
                       JID :: jid:ljid(),
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

-spec db_error(ReasonData :: map(), ErrorDebug :: map(), Event :: any()) ->
    {error, Details :: map()}.
db_error(ReasonData, ErrorDebug, Event) ->
    {error, maps:merge(ErrorDebug#{ event => Event }, ReasonData)}.

%%====================================================================
%% Internal functions
%%====================================================================

